#!usr/bin/python3

import rosbag, os, sys, re, numpy, math
from sensor_msgs.msg import Imu
import sensor_msgs.point_cloud2 as pc2

os.chdir(r'/home/tiago/Desktop/slam_tests')

def convertImu(sbgImu, sbgQuat):
    imuMsg = Imu()

    # covMat = [0.0 for i in range(9)]

    imuMsg.orientation.x = sbgQuat.quaternion.x
    imuMsg.orientation.y = sbgQuat.quaternion.y
    imuMsg.orientation.z = sbgQuat.quaternion.z
    imuMsg.orientation.w = sbgQuat.quaternion.w

    # imuMsg.orientation_covariance = covMat

    imuMsg.angular_velocity.x = sbgImu.gyro.x
    imuMsg.angular_velocity.y = sbgImu.gyro.y
    imuMsg.angular_velocity.z = sbgImu.gyro.z

    # imuMsg.angular_velocity_covariance = covMat

    imuMsg.linear_acceleration.x = sbgImu.accel.x
    imuMsg.linear_acceleration.y = sbgImu.accel.y
    imuMsg.linear_acceleration.z = sbgImu.accel.z

    # imuMsg.linear_acceleration_covariance = covMat

    # imuMsg.header.seq = sbgImu.header.seq
    # imuMsg.header.frame_id = "/imu/data"
    # imuMsg.header.stamp.secs  = sbgImu.header.stamp.secs
    # imuMsg.header.stamp.nsecs = sbgImu.header.stamp.nsecs

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

def writePoints(msg, file = r'points.txt'):

    points = []
    for p in pc2.read_points(msg, skip_nans=True):
        points.append(p)
    
    points = numpy.array(points)
    numpy.savetxt(file, points, fmt="%f")

rBag = r'20180627_euc1_x_hor.bag'
bag = rosbag.Bag(rBag)

tops = [r'/velodyne_points', r'/ekf_quat', r'/imu_data']

wBag = re.sub(r'\.bag$', '_sensorMsg.bag', rBag)
wBag = rosbag.Bag(wBag, 'w')

for topic, msg, t in bag.read_messages(topics=tops):

    if( topic == tops[2] ):
        imu = msg

    if(topic == tops[1]):
        if(imu.time_stamp == msg.time_stamp):
            imuMsg = convertImu(imu, msg)
            wBag.write('/imu/data', imuMsg, t)
    
    if(topic == tops[0]):
        wBag.write(topic, filterPointCloud2(msg), t)

bag.close()
wBag.close()

