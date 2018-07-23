import rosbag, os, sys, re
from sensor_msgs.msg import Imu

def convertImu(sbgImu):
    imuMsg = Imu()

    imuMsg.orientation.x = 0
    imuMsg.orientation.y = 0
    imuMsg.orientation.z = 0
    imuMsg.orientation.w = 0

    imuMsg.orientation_covariance = [-1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

    imuMsg.angular_velocity.x = sbgImu.gyro.x
    imuMsg.angular_velocity.y = sbgImu.gyro.y
    imuMsg.angular_velocity.z = sbgImu.gyro.z

    imuMsg.angular_velocity_covariance = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

    imuMsg.linear_acceleration.x = sbgImu.accel.x
    imuMsg.linear_acceleration.y = sbgImu.accel.y
    imuMsg.linear_acceleration.z = sbgImu.accel.z

    imuMsg.linear_acceleration_covariance = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]

    imuMsg.header.seq = sbgImu.header.seq
    imuMsg.header.frame_id = "imu_link"
    imuMsg.header.stamp.secs  = sbgImu.header.stamp.secs
    imuMsg.header.stamp.nsecs = sbgImu.header.stamp.nsecs

    return imuMsg

path = sys.argv[1]
os.chdir(path)
files = sorted(os.listdir('./'))

if(len(sys.argv) > 2):
    outPath = sys.argv[2]
    if(outPath[-1] != '/'):
        outPath += '/'
else:
    outPath = './'

for file in files:
    bagType = re.match(r'.+\.bag$', file)
    
    if(bagType == None):
        continue
    
    print('converting ' + file)

    bag = rosbag.Bag(file)
    wBag = rosbag.Bag(outPath + file, 'w')

    for topic, msg, t in bag.read_messages():
        
        # if(topic == 'horizontal_laser_3d'):
        #     print(msg.header)
        #     break

        if(topic == '/imu_data'):
            # print(msg)
            imuMsg = convertImu(msg)
            wBag.write('imu', imuMsg, t)
        elif(topic == '/velodyne_points'):
            # print(msg.header)
            msg.header.frame_id = 'horizontal_vlp16_link'
            wBag.write('horizontal_laser_3d', msg, t)

            # msg.header.frame_id = 'vertical_vlp16_link'
            # wBag.write('vertical_laser_3d', msg, t)

        # if(counter >= 30):
        #     break

    bag.close()
    wBag.close()