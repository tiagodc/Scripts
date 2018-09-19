import rosbag, math, os, numpy, re, sys
import sensor_msgs.point_cloud2 as pc2

def filterPointCloud2(pc2msg, radius=5):
    '''
    crop 3d data from a PointCloud2 message (@msg) according to a @radius from the [0,0,0] point (sensor position) 
    '''
    outData = []
    for p in pc2.read_points(pc2msg, skip_nans=True):
        dst = math.sqrt( p[0]**2 + p[1]**2 + p[2]**2 )
        if(dst <= radius):
            outData.append(p)

    return pc2.create_cloud(pc2msg.header, pc2msg.fields, outData)

