#!/bin/bash

#ifconfig eth0 192.168.3.100
### browse 192.168.1.201

# In the "displays" panel, click "Add", then select "Point Cloud2", then press "OK".
# In the "Topic" field of the new "Point Cloud2" tab, enter "/velodyne_points" 

xterm -e "source /home/tiago/catkin_loam/devel/setup.bash && roslaunch velodyne_pointcloud VLP16_points.launch" &

# xterm -e "rosnode list" &

# xterm -e "rostopic echo /velodyne_points" &

sleep 5 && xterm -e "source /home/tiago/catkin_loam/devel/setup.bash && rosrun rviz rviz -f velodyne"