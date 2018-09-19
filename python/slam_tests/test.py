import rosbag, math, os, numpy, re, laspy, pcl_msgs, sys
import sensor_msgs.point_cloud2 as pc2
import tf

def transformationMatrix(msg, pcStamp):
    pose = msg.pose.pose.position
    orit = msg.pose.pose.orientation
    
    quaternion = [orit.x, orit.y, orit.z, orit.w]
    euler = tf.transformations.euler_from_quaternion(quaternion)
    
    mat = tf.transformations.euler_matrix( euler[0], euler[1], euler[2] )
    mat[3,0:3] = [pose.x, pose.y, pose.z]

    return {
        'matrix': mat,
        'secs': pcStamp.secs,
        'nsecs':pcStamp.nsecs
    }

def cloudArray(msg):
    
    outData = []
    for p in pc2.read_points(msg, skip_nans=True):
        outData.append(p)
    
    return numpy.matrix(outData)

trans = transformationMatrix(msg)

cld = cloudArray(msg)


os.chdir(r'/home/tiago/Desktop/slam_tests')

def filterPointCloud2(msg, radius=5):

    outData = []
    for p in pc2.read_points(msg, skip_nans=True):
        dst = math.sqrt( p[0]**2 + p[1]**2 + p[2]**2 )
        if(dst <= radius):
            outData.append(p)

    cld = pc2.create_cloud(msg.header, msg.fields, outData)

    return cld

def convertImu(sbgImu, sbgQuat):
    imuMsg = Imu()

    covMat = [0.0 for i in range(9)]

    imuMsg.orientation.x = sbgQuat.quaternion.x
    imuMsg.orientation.y = sbgQuat.quaternion.y
    imuMsg.orientation.z = sbgQuat.quaternion.z
    imuMsg.orientation.w = sbgQuat.quaternion.w

    imuMsg.orientation_covariance = covMat

    imuMsg.angular_velocity.x = sbgImu.gyro.x
    imuMsg.angular_velocity.y = sbgImu.gyro.y
    imuMsg.angular_velocity.z = sbgImu.gyro.z

    imuMsg.angular_velocity_covariance = covMat

    imuMsg.linear_acceleration.x = sbgImu.accel.x
    imuMsg.linear_acceleration.y = sbgImu.accel.y
    imuMsg.linear_acceleration.z = sbgImu.accel.z

    imuMsg.linear_acceleration_covariance = covMat

    imuMsg.header.seq = sbgImu.header.seq
    imuMsg.header.frame_id = "/imu/data"
    imuMsg.header.stamp.secs  = sbgImu.header.stamp.secs
    imuMsg.header.stamp.nsecs = sbgImu.header.stamp.nsecs

    return imuMsg

bag = r'20180627_euc1_x_hor_sensorMsg_slam.bag'
bag = rosbag.Bag(bag)

bag2 = r'20180627_euc1_x_hor_sensorMsg.bag'
bag2 = rosbag.Bag(bag2)


tops = [r'/aft_mapped_to_init', r'/integrated_to_init', r'/velodyne_cloud_registered', r'/velodyne_points', r'/ekf_quat', r'/imu_data', r'/laser_odom_to_init']

ct = 0
for topic, msg, t in bag.read_messages():
    if(topic == '/tf'): continue
    print( 'time: {}, secs: {}, nsecs: {}, topic: {}'.format(t, msg.header.stamp.secs, msg.header.stamp.nsecs, topic) )
    ct += 1
    if(ct > 5):
        break

ct = 0
for topic, msg, t in bag2.read_messages(topics=tops[2]):
    print( 'secs: {}, nsecs: {}, topic: {}'.format( msg.header.stamp.secs, msg.header.stamp.nsecs, topic) )

    ct += 1
    if(ct > 100):
        break


bag.close()

    # if(topic == tops[0]):
    #     pos = msg.pose.pose.position
    #     ori = msg.pose.pose.orientation
    #     pt = [tm, pos.x, pos.y, pos.z, ori.x, ori.y, ori.z, ori.w ]
    #     arr.append( pt )

    # if(topic == tops[2]):
    #     for p in pc2.read_points(msg, skip_nans=True):
    #         p2 = [tm, p[0], p[1], p[2]]
    #         pcd.append(p2)
        
    #     centerCloud = filterPointCloud2(msg)
    #     wBag.write(tops[2], centerCloud, t)

    #     for p in pc2.read_points(centerCloud, skip_nans=True):
    #         tm = centerCloud.header.stamp
    #         tm = tm.secs + tm.nsecs / 10**9
    #         p2 = [tm, p[0], p[1], p[2]]
    #         pcc.append(p2)
            


        # break

wBag.close()
arr = numpy.array(arr)
pcd = numpy.array(pcd)
pcc = numpy.array(pcc)

# wBag = rosbag.Bag('bagsy.bag')

for topic, msg, t in wBag.read_messages(topics=tops):
    if(topic == tops[2]):
        for p in pc2.read_points(msg, skip_nans=True):
            print(p)



numpy.savetxt('all1.txt', arr, fmt="%f")
numpy.savetxt('vel2.txt', pcd, fmt="%f")
numpy.savetxt('vel3.txt', pcc, fmt="%f")

pts = numpy.loadtxt(r'scan000.3d')