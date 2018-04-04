#!/bin/bash

#give permission to run this script
## chmod u+x slam_bash

#full path to the directory containing the .pcap files
path="/home/tiago/Desktop/esalq/"

#time (in seconds) to leave the ROS terminals open per file
tempo=180

mkdir -p $path'pcd_temp'

#loop over the *.pcap files in the input directory 
for file in  $path*.pcap
do

	sleep 2
	echo processing: $file

	#open Rviz
	timeout $tempo xterm -e "source ~/catkin_loam/devel/setup.bash && roslaunch loam_velodyne loam_velodyne.launch" &

	#save .pcd files for all slam frames 
	timeout $tempo xterm -e "cd "$path"pcd_temp && source ~/catkin_loam/devel/setup.bash && rosrun pcl_ros pointcloud_to_pcd input:=/velodyne_cloud_registered" &

	#process the .pcap file
	timeout $tempo xterm -e 'source ~/catkin_loam/devel/setup.bash && sleep 1 && roslaunch velodyne_pointcloud VLP16_points.launch pcap:="'$file'" read_once:="true" max_range:="80" min_range:="1"' &

	#convert all pcd frames to a single .laz file
	sleep $tempo && xterm -e 'cd '$path' && ./pcd2laz -f pcd_temp -o '"${file%.pcap}.laz"' && cd pcd_temp && rm *.pcd'

done
