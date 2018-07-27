import rosbag, math, os, numpy, re, laspy
import sensor_msgs.point_cloud2 as pc2

def exportLas(lasName=r'points.laz'):

    regex = re.compile(r'.+\.frames$')
    frameFiles = filter(regex.search, os.listdir())
    frameFiles = [re.sub(r'\.frames$', r'', i) for i in frameFiles]

    x = numpy.array([])
    y = numpy.array([])
    z = numpy.array([])
    gps = numpy.array([])
    tStp = 0
    for i in sorted(frameFiles):
        print(r'loading ' + i)
        pts = numpy.loadtxt(i + r'.3d')
        mat = numpy.loadtxt(i + r'.frames')[-1,:]
        mat = numpy.array([ mat[0:4], mat[4:8], mat[8:12], mat[12:16] ])

        oneCol = numpy.array([[1]] * len(pts))
        pts = numpy.append(pts, oneCol, axis=1)
        pts = numpy.dot(pts, mat)

        x = numpy.append(x, pts[:,0])
        y = numpy.append(y, pts[:,1])
        z = numpy.append(z, pts[:,2])
        gps = numpy.append(gps, [tStp] * len(pts))

        tStp += 1

    # numpy.savetxt('temp.txt', numpy.column_stack((x,y,z)), fmt='%f')

    lasHead = laspy.header.Header(point_format=1)
    xmin = numpy.floor(numpy.min(x))
    ymin = numpy.floor(numpy.min(y))
    zmin = numpy.floor(numpy.min(z))

    scale = 1000000

    lasOut = laspy.file.File(lasName, mode="w", header = lasHead)
    lasOut.header.offset = [xmin,ymin,zmin]
    lasOut.header.scale = [1/scale] * 3

    lasOut.set_x(x*scale)
    lasOut.set_y(y*scale)
    lasOut.set_z(z*scale)
    lasOut.set_gps_time(gps)

    lasOut.close()

def getTime(msg):

    secs  = msg.header.stamp.secs
    nsecs = msg.header.stamp.nsecs
    time = (secs, nsecs)
    return time

def getAngles(msg = None):

    if msg == None:
        return [0,0,0]

    pitch = msg.angle.x * 180 / math.pi
    yaw = msg.angle.y * 180 / math.pi
    roll = msg.angle.z * 180 / math.pi

    imu = [pitch,yaw,roll]
    return imu

def getPos():
    return [0,0,0]

def getCloud(msg):

    cloud = []
    for p in pc2.read_points(msg, skip_nans=True):     
        cloud.append(p)
    
    return cloud


goDir = r'/home/tiago/Desktop/bag/'
slamDir = r'/home/tiago/SLAM/slam6d-code/bin/'
bagFiles = os.listdir(goDir)

for goBag in bagFiles:
    # goBag = r'20180626_ser1_x_45.bag'
    
    if re.match(r'.+\.bag$', goBag) == None:
        continue

    os.chdir(goDir)
    bagPref = re.sub(r'\.bag', '', goBag)
    bag = rosbag.Bag(goBag)

    os.mkdir(bagPref)
    os.chdir(bagPref)

    ang = True
    tempAng = numpy.array([getPos(), getAngles()])
    cld = False
    skip = 0
    skipMax = 1
    counter = 0
    for topic, msg, t in bag.read_messages():
        
        # if(not cld and not ang and topic == r'/ekf_euler'):
        #     tempAng = getAngles(msg)
        #     tempAng = numpy.array([getPos(), tempAng])
        #     ang = True

        if(ang and not cld and topic == r'/velodyne_points'):
            tempCloud = getCloud(msg)
            tempCloud = numpy.array(tempCloud)
            cld = True

        if ang and cld:
            if skip == 0:
                print('converting cloud ' + str(counter))
                ctr = '00' + str(counter) if counter < 10 else ('0' + str(counter) if counter < 100 else str(counter))
                numpy.savetxt('scan' + ctr + '.pose', tempAng, fmt="%f")
                numpy.savetxt('scan' + ctr + '.3d', tempCloud[:,0:3], fmt="%f")
                counter += 1

            skip = 0 if skip >= skipMax else skip+1
            # ang = False
            cld = False
        
        if(counter > 999):
            break

    bag.close()

    cwd = os.getcwd()
    os.chdir(slamDir)

    cmd = r'./slam6D -i 600 -I 15 --metascan --epsICP=0.00000001 --epsSLAM=0.1 -d 1.25 -D 1 -G 1 -r 0.2 -a 2 ' + cwd
    print(cmd)
    os.system(cmd)

    cmd = r'./exportPoints ' + cwd #+ r' && ./txt2las -i points.pts -o points.laz -v'
    print(cmd)
    os.system(cmd)

    if(cwd[-1] != r'/'): 
        cwd += r'/'

    os.rename(r'points.pts', cwd + r'points.pts')
    os.rename(r'positions.txt', cwd + r'positions.txt')
    os.rename(r'poses.txt', cwd + r'poses.txt')
    # os.rename(r'points.laz', cwd + r'points.laz')
    os.rename(r'loopclose.pts', cwd + r'loopclose.pts')

    os.chdir(cwd)
    exportLas()
