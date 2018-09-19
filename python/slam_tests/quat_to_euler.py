#!/usr/bin/python

import rosbag, os, sys, tf, math, numpy, matplotlib.pyplot as plt

os.chdir(r'/home/tiago/Desktop/slam_tests')

rBag = r'20180627_euc1_x_hor_sensorMsg.bag'
bag = rosbag.Bag(rBag)
top = ['/imu/data']

angs = []
for topic, msg, t in bag.read_messages(topics=top):

    quat = (
        msg.orientation.x,
        msg.orientation.y,
        msg.orientation.z,
        msg.orientation.w
    )
    
    # roll, pitch, yaw - respectively
    euler = tf.transformations.euler_from_quaternion(quat)
    time = float(msg.header.stamp.secs) + float(msg.header.stamp.nsecs) / 10**9
    info = [time, euler[0] * 180/math.pi, euler[1] * 180/math.pi, euler[2] * 180/math.pi]
    angs.append(info)

bag.close()

angs = numpy.array(angs).transpose()

numpy.savetxt('angs.txt', angs.transpose(), fmt="%f")

plt.plot(angs[0], angs[3])
plt.show()



