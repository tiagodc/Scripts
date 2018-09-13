#!/bin/bash

#give permission to run this script
## chmod u+x slam_bash

#full path to the directory containing the .pcap files
path="/media/tiago/TDC/Foridar/TLS_2018/"

#time (in seconds) to leave the ROS terminals open per file
tempo=480

mkdir -p $path'pcd_temp'

#loop over the *.pcap files in the input directory 
for file in  $path'clouds/'*.bag
do

	sleep 2
	echo processing: $file

	#open Rviz
	timeout $tempo xterm -e "source ~/catkin_loam/devel/setup.bash && roslaunch loam_velodyne loam_velodyne.launch" &

	# save .pcd files for all slam frames 
	timeout $tempo xterm -e "cd "$path"pcd_temp && source ~/catkin_loam/devel/setup.bash && rosrun pcl_ros pointcloud_to_pcd input:=/velodyne_cloud_registered" &

	# #process the .pcap file	
	timeout $tempo xterm -e 'source ~/catkin_loam/devel/setup.bash && sleep 2 && rosbag play -r 0.5 '$file &

	#convert all pcd frames to a single .laz file
	sleep $tempo && xterm -e 'cd '$path' && ./pcd2laz -f pcd_temp -o '"${file%.bag}.laz"

	rm $path'pcd_temp/'*.pcd

done

rm -r $path'pcd_temp/'
cd $path'clouds/' && mv *.laz ../reprocessed/
