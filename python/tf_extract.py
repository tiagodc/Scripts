import os, rosbag, re, math, numpy, shutil, tf, sys, time

rBag = "/home/tiago/Desktop/tests/Ento_q_sensorMsg_slam.bag"
bag = rosbag.Bag(rBag)

store = []
uq = set()
for topic, msg, t in bag.read_messages(topics=['/tf']):
    child = msg.transforms[0].child_frame_id
    uq.add(child)
    if child == '/aft_mapped':
        tfmsg = msg.transforms[0].transform
        quat = (tfmsg.rotation.x, tfmsg.rotation.y, tfmsg.rotation.z, tfmsg.rotation.w)
        euler = tf.transformations.euler_from_quaternion(quat)
        #time, x, y, z, roll, pitch, yaw
        temp = [msg.transforms[0].header.stamp.secs + msg.transforms[0].header.stamp.nsecs * 1e-9, tfmsg.translation.x, tfmsg.translation.y, tfmsg.translation.z, euler[0], euler[1], euler[2]]
        store.append(temp)        
        # print temp

bag.close()

slamPath = numpy.array(store)
oTxt = re.sub(r'\.bag$', r'_path.txt', rBag)
numpy.savetxt(oTxt, slamPath, fmt="%f")