#!/usr/bin/python
print('# loading libraries')

### import ROS packages necessary to deal with the bag files and other common packages
### ROS must be already installed in the system (ideally ROS kinetic, under Ubuntu 16.04)
import os, rosbag, re, math, numpy, shutil, tf, sys, time
from sensor_msgs.msg import Imu
import sensor_msgs.point_cloud2 as pc2

reload(sys)  
sys.setdefaultencoding('utf8')

### function for converting the data from our system's IMU (sbg ellipse) to a common ROS sensor message
### this is necessary to provide a readable input for the LOAM algorithm
def convertImu(sbgImu, sbgQuat):
    imuMsg = Imu()

    imuMsg.orientation.x = sbgQuat.quaternion.x
    imuMsg.orientation.y = sbgQuat.quaternion.y
    imuMsg.orientation.z = sbgQuat.quaternion.z
    imuMsg.orientation.w = sbgQuat.quaternion.w

    imuMsg.angular_velocity.x = sbgImu.gyro.x
    imuMsg.angular_velocity.y = sbgImu.gyro.y
    imuMsg.angular_velocity.z = sbgImu.gyro.z

    imuMsg.linear_acceleration.x = sbgImu.accel.x
    imuMsg.linear_acceleration.y = sbgImu.accel.y
    imuMsg.linear_acceleration.z = sbgImu.accel.z

    imuMsg.header = sbgImu.header

    return imuMsg


### function for (optional) manipulation of the input point cloud frames (from the VLP16)
### in which the axes can be shifted and data range filtered before processing with SLAM 
def filterPointCloud2(msg, radius = None, swap = 'xyz'):

    swap = swap.lower()
    order = {'x':0, 'y':1, 'z':2}
    
    for i in range(len(swap)):
        ival = swap[i]
        order[ival] = i

    outData = []
    for p in pc2.read_points(msg, skip_nans=True):

        x = p[0]
        y = p[1]
        z = p[2]

        p = list(p)

        p[ order['x'] ] = x
        p[ order['y'] ] = y
        p[ order['z'] ] = z
        
        p = tuple(p)
        
        dst = math.sqrt( x**2 + y**2 + z**2 )

        if(radius is not None and dst > radius):
            continue
        
        outData.append(p)

    cld = pc2.create_cloud(msg.header, msg.fields, outData)

    return cld

#########################################
print('# defining global variables')

### define path to directory with all bag files to be processed
### the bag files must contain the /velodyne_points topic and data from the IMU (optional)
### such bag files can also be generated from a raw velodyne PCAP through the velodyne's ROS drivers
os.chdir(r'/media/tiago/DATA/PRJ_Plantar_10072019/BAGS/')

### variables used to convert imu_data and parse pointCloud2
### list of ROS topics with raw data from the sensors
tops = [r'/velodyne_points', r'/ekf_quat', r'/imu_data']

### distance limit to consider for the LiDAR data
radius = None

### axes' reordering to apply on the LiDAR's point cloud frames
swap = 'xyz'

### variables definig SLAM parameters
### ### use hector slam method?
hectorSlam = False
### ### use IMU data?
useImu = True
### ### time ratio to apply when processing the bag file - lower ratios produce better point clouds
playRatio = 0.25
### ### topics to be recorded while running the SLAM - results of the corregistration
recTopics = [r'/imu/data', r'/integrated_to_init', r'/velodyne_cloud_registered']
### ### path to the workspace where the LOAM package is built
sourcePath = r'~/catkin_loam'

### variables used for generating the LAZ point cloud
### ### path to the pcd2laz executable
pcd2laz = '/home/tiago/pcd2laz/bin/Release/pcd2laz' 
### ### path to the directory that will be created to host temporary point cloud files (.pcd)
pcdDir = 'pcd_temp'

### command to kill all ROS processes after the SLAM finishes
rosKill = r'rosnode kill -a && killall -9 rosmaster'

### list all bag files
bagFiles = []
for i in os.listdir('.'):
    if re.match(r'.+\.bag$', i) is not None:
        bagFiles.append(i)

### process bag files, one by one
for rBag in bagFiles:

    print('### processing: ' + rBag)

    ### define laz file name (final output)
    oLaz = re.sub(r'\.bag$', r'.laz', rBag)

    #########################################
    print('# getting sensorMsg topics')

    ### read bag file with raw data and convert it to bag of common ROS messages 
    wBag = re.sub(r'\.bag$', '_sensorMsg.bag', rBag)
    bag = rosbag.Bag(rBag)

    writeBag = rosbag.Bag(wBag, 'w')

    imu = None
    for topic, msg, t in bag.read_messages(topics=tops):
        if( topic == tops[2] ):
            imu = msg

        if(topic == tops[1] and imu is not None):
            if(imu.time_stamp == msg.time_stamp):
                imuMsg = convertImu(imu, msg)
                writeBag.write('/imu/data', imuMsg, t)
            
        if(topic == tops[0]):
            writeBag.write(topic, filterPointCloud2(msg, radius, swap), t)

    bag.close()
    writeBag.close()

    #########################################
    print('# performing SLAM')

    ### define some command line parameters
    launchPref = r'hector_' if hectorSlam else ''
    oBag = re.sub(r'(\.bag$)', r'_' + launchPref + r'slam.bag', wBag)

    ### get input bag file's duration
    bag = rosbag.Bag(wBag)
    loadTime = 10
    bagTime = math.ceil(loadTime + (bag.get_end_time() - bag.get_start_time()) / playRatio)
    bag.close()

    ### call parallel ROS processes for running the SLAM 
    cmdTimeout = r'timeout ' + str(int(bagTime)) + r' '
    cmdSleep = r'sleep 2 && '
    cmdStart = r'xterm -e "source ' + sourcePath + r'/devel/setup.bash && '
    cmdImu = r'' if useImu else r' --topics /velodyne_points'

    roslaunch = cmdStart + r' roslaunch loam_velodyne ' + launchPref + r'loam_velodyne.launch" &'
    os.system(roslaunch)

    time.sleep(loadTime)

    bagRecord = cmdSleep + cmdStart + r'rosbag record ' + r' '.join(recTopics) + r' -O ' + oBag + r'" &'
    os.system(bagRecord)

    bagPlay = cmdSleep + cmdStart + r'rosbag play ' + wBag + r' -r ' + str(playRatio) + r' ' + cmdImu + r'" &'
    os.system(bagPlay)

    time.sleep(bagTime+2)
    os.system(rosKill)

    #########################################
    # oBag = rBag
    print('# writing LAZ point cloud')

    ### redefine the temporary directory for the pcds
    if os.path.exists(pcdDir): 
        shutil.rmtree(pcdDir)

    os.makedirs(pcdDir)

    ### call ROS processes for exporting pcds from a bag file
    os.system('roscore &')

    time.sleep(loadTime)

    pclCmd = 'rosrun pcl_ros bag_to_pcd ' + oBag + ' /velodyne_cloud_registered ' + pcdDir

    os.system(pclCmd)
    os.system(rosKill)

    ### convert all pcd files to a single laz file with time stamps
    lazCmd = pcd2laz + ' -f ' + pcdDir + ' -o ' + oLaz

    os.system(lazCmd)

    shutil.rmtree(pcdDir)

    #########################################
    print('# writing IMU and SLAM path`s information')

    ### convert the path information (calculated by the SLAM) from a bag file to text files
    bag = rosbag.Bag(oBag)

    rad2deg = 180/math.pi
    angs = []
    slamPath = []
    for topic, msg, t in bag.read_messages(topics=recTopics):

        time = float(msg.header.stamp.secs) + float(msg.header.stamp.nsecs) / 10**9

        if topic == recTopics[0]:
            quat = (
                msg.orientation.x,
                msg.orientation.y,
                msg.orientation.z,
                msg.orientation.w
            )

            euler = tf.transformations.euler_from_quaternion(quat)

            # time, roll, pitch, yaw
            info = [time, euler[0] * rad2deg, euler[1] * rad2deg, euler[2] * rad2deg]
            angs.append(info)

        if topic == recTopics[1]:
            pose = msg.pose.pose

            quat = (
                pose.orientation.x,
                pose.orientation.y,
                pose.orientation.z,
                pose.orientation.w
            )

            euler = tf.transformations.euler_from_quaternion(quat)
            
            # time, x, y, z, roll, pitch, yaw
            info = [time, pose.position.x, pose.position.y, pose.position.z, euler[0] * rad2deg, euler[1] * rad2deg, euler[2] * rad2deg]
            slamPath.append(info)

    bag.close()

    ### write the text files
    if len(angs) > 0:
        angs = numpy.array(angs)
        oTxt = re.sub(r'\.bag$', r'_imu.txt', rBag)
        numpy.savetxt(oTxt, angs, fmt="%f")

    if len(slamPath) > 0:
        slamPath = numpy.array(slamPath)
        oTxt = re.sub(r'\.bag$', r'_slam_path.txt', rBag)
        numpy.savetxt(oTxt, slamPath, fmt="%f")

    ### finished process, go to next point cloud
    print('# done')