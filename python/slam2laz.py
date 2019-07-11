#!/usr/bin/python
print('# loading libraries')

### import ROS packages necessary to deal with the bag files and other common packages
### ROS must be already installed in the system (ideally ROS kinetic, under Ubuntu 16.04)
import os, rosbag, re, math, numpy, shutil, tf, sys, time
reload(sys)  
sys.setdefaultencoding('utf8')

#########################################
print('# defining global variables')

### define path to directory with all bag files to be processed
### the bag files must contain the /velodyne_points topic and data from the IMU (optional)
### such bag files can also be generated from a raw velodyne PCAP through the velodyne's ROS drivers
os.chdir(r'/media/tiago/9F3D-9F0B/slam_bags/')

### variables definig SLAM parameters
### ### time ratio to apply when processing the bag file - lower ratios produce better point clouds
playRatio = 0.25
### ### topics to be recorded while running the SLAM - results of the corregistration
recTopics = [r'/imu/data', r'/integrated_to_init', r'/velodyne_cloud_registered']

### variables used for generating the LAZ point cloud
### ### path to the pcd2laz executable
pcd2laz = '/home/tiago/pcd2laz/bin/Release/pcd2laz' 
### ### path to the directory that will be created to host temporary point cloud files (.pcd)
pcdDir = 'pcd_temp'

### command to kill all ROS processes after the SLAM finishes
rosKill = r'sleep 3 && rosnode kill -a && killall -9 rosmaster'

### list all bag files
bagFiles = []
for i in os.listdir('.'):
    if re.match(r'.+\.bag$', i) is not None:
        bagFiles.append(i)

lazFiles = []
for i in os.listdir('../results'):
    if re.match(r'.+\.laz$', i) is not None:
        lazFiles.append(i)

### process bag files, one by one
for rBag in bagFiles:
      
    ### define laz file name (final output)
    oLaz = re.sub(r'_sensorMsg_slam\.bag$', r'.laz', rBag)

    if oLaz in lazFiles:
        continue
    
    oLaz = '../results/' + oLaz

    print('### processing: ' + rBag + ' to ' + oLaz)

    ### redefine the temporary directory for the pcds
    if os.path.exists(pcdDir): 
        shutil.rmtree(pcdDir)

    os.makedirs(pcdDir)
    
    ### call ROS processes for exporting pcds from a bag file
    os.system('roscore &')

    pclCmd = 'rosrun pcl_ros bag_to_pcd ' + rBag + ' /velodyne_cloud_registered ' + pcdDir

    os.system(pclCmd)
    os.system(rosKill)

    ### convert all pcd files to a single laz file with time stamps
    lazCmd = pcd2laz + ' -f ' + pcdDir + ' -o ' + oLaz

    os.system(lazCmd)

    shutil.rmtree(pcdDir)

    #########################################
    print('# writing IMU and SLAM path`s information')

    ### convert the path information (calculated by the SLAM) from a bag file to text files
    bag = rosbag.Bag(rBag)

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
        oTxt = '../results/' + re.sub(r'\.bag$', r'_imu.txt', rBag)
        numpy.savetxt(oTxt, angs, fmt="%f")

    if len(slamPath) > 0:
        slamPath = numpy.array(slamPath)
        oTxt = '../results/' + re.sub(r'\.bag$', r'_path.txt', rBag)
        numpy.savetxt(oTxt, slamPath, fmt="%f")

    ### finished process, go to next point cloud
    print('# done')