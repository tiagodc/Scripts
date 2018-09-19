#!/usr/bin/python
print('# loading libraries')

import os, rosbag, re, math, numpy, shutil, tf, sys
from sensor_msgs.msg import Imu
import sensor_msgs.point_cloud2 as pc2

reload(sys)  
sys.setdefaultencoding('utf8')

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


# bag files path
os.chdir(r'/home/tiago/Desktop/slam_tests')

# variables used to convert imu_data and parse pointCloud2
rBag = r'20180627_euc1_x_hor.bag'
tops = [r'/velodyne_points', r'/ekf_quat', r'/imu_data']
radius = None
swap = 'xyz'

# variables definig SLAM parameters
hectorSlam = False
useImu = True
playRatio = 3
recTopics = [r'/imu/data', r'/integrated_to_init', r'/velodyne_cloud_registered']
sourcePath = r'~/catkin_loam'

# variables used for generating the LAZ point cloud
pcd2laz = '~/pcd2laz/bin/Release/pcd2laz' 
pcdDir = 'pcd_temp'
oLaz = re.sub(r'\.bag$', r'.laz', rBag)

print('### processing: ' + rBag)


#########################################
print('# getting sensorMsg topics')

wBag = re.sub(r'\.bag$', '_sensorMsg.bag', rBag)
bag = rosbag.Bag(rBag)

writeBag = rosbag.Bag(wBag, 'w')

for topic, msg, t in bag.read_messages(topics=tops):
    if( topic == tops[2] ):
        imu = msg

    if(topic == tops[1]):
        if(imu.time_stamp == msg.time_stamp):
            imuMsg = convertImu(imu, msg)
            writeBag.write('/imu/data', imuMsg, t)
    
    if(topic == tops[0]):
        writeBag.write(topic, filterPointCloud2(msg, radius, swap), t)

bag.close()
writeBag.close()

#########################################
print('# performing SLAM')

launchPref = r'hector_' if hectorSlam else ''
oBag = re.sub(r'(\.bag$)', launchPref + r'_slam.bag', wBag)
rosKill = r'rosnode kill -a && killall -9 rosmaster'

bag = rosbag.Bag(wBag)
bagTime = math.ceil(5 + (bag.get_end_time() - bag.get_start_time()) / playRatio)
bag.close()

cmdTimeout = r'timeout ' + str(int(bagTime)) + r' '
cmdSleep = r'sleep 2 && '
cmdStart = r'xterm -e "source ' + sourcePath + r'/devel/setup.bash && '
cmdImu = r'' if useImu else r' --topics /velodyne_points'

roslaunch = cmdTimeout + cmdStart + r' roslaunch loam_velodyne ' + launchPref + r'loam_velodyne.launch" &'
os.system(roslaunch)

bagRecord = cmdSleep + cmdStart + r'rosbag record ' + r' '.join(recTopics) + r' -O ' + oBag + r'" &'
os.system(bagRecord)

bagPlay = cmdSleep + cmdStart + r'rosbag play ' + wBag + r' -r ' + str(playRatio) + r' ' + cmdImu + r'"'
os.system(bagPlay)

os.system(rosKill)

#########################################
print('# writing LAZ point cloud')

if os.path.exists(pcdDir): 
    shutil.rmtree(pcdDir)

os.makedirs(pcdDir)

os.system('roscore &')

pclCmd = 'cd ' + pcdDir + ' && rosrun pcl_ros pointcloud_to_pcd input:=/velodyne_cloud_registered &'

os.system(pclCmd)

playCmd = 'rosbag play ' + oBag

os.system(playCmd)
os.system(rosKill)

lazCmd = pcd2laz + ' -f ' + pcdDir + ' -o ' + oLaz

os.system(lazCmd)

shutil.rmtree(pcdDir)

#########################################
print('# writing IMU and SLAM path`s information')

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

if len(angs) > 0:
    angs = numpy.array(angs)
    oTxt = re.sub(r'\.bag$', r'_imu.txt', rBag)
    numpy.savetxt(oTxt, angs, fmt="%f")

if len(slamPath) > 0:
    slamPath = numpy.array(slamPath)
    oTxt = re.sub(r'\.bag$', r'_slam_path.txt', rBag)
    numpy.savetxt(oTxt, slamPath, fmt="%f")

print('# done')