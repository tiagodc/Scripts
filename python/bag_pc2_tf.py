import rosbag, re, math
from sensor_msgs.msg import Imu
import sensor_msgs.point_cloud2 as pc2
from copy import deepcopy

TOPICS = [r'/velodyne_points', r'/ekf_quat', r'/imu_data']

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
    imuMsg.header.frame_id = "imu_link"
    return imuMsg


def filterPointCloud2(msg):

    outData = []
    n = 0
    for p in pc2.read_points(msg, skip_nans=True):
        p = list(p)
        p.append(0.1 / (msg.width - n))      
        outData.append(tuple(p))    
        n+=1

    msg.header.frame_id = "base_link"
    pt = deepcopy(msg.fields[0])
    pt.name = 'time'
    pt.offset = 24
    pt.datatype = 7
    pt.count = 1
    msg.fields.append(pt)

    cld = pc2.create_cloud(msg.header, msg.fields, outData)
    cld.is_dense = msg.is_dense
    # cld.is_bigendian = msg.is_bigendian
    # cld.point_step = msg.point_step
    cld.row_step = 0
    # cld.width = msg.width
    cld.height = 1
    return cld


rBag = './IP_G_T13U_P49_20200416_T_20200416.bag'
wBag = re.sub(r'\.bag$', '_sensorMsg.bag', rBag)    

bag = rosbag.Bag(rBag)
writeBag = rosbag.Bag(wBag, 'w')

imu = None
n = 0
for topic, msg, t in bag.read_messages():
    if( topic == TOPICS[2] ):
        imu = msg

    elif(topic == TOPICS[1] and imu is not None):
        if(imu.time_stamp == msg.time_stamp):
            imuMsg = convertImu(imu, msg)
            writeBag.write('/imu_raw', imuMsg, t)
        
    elif(topic == TOPICS[0]):
        writeBag.write('/points_raw', filterPointCloud2(msg), t)
    
    # else:
    #     writeBag.write(topic, msg, t)

bag.close()
writeBag.close()
