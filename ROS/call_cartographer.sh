#!/bin/bash

file=$1

source ${HOME}/catkin_cartographer/install_isolated/setup.bash

cd ${HOME}/catkin_cartographer && catkin_make_isolated --install --use-ninja

roslaunch cartographer_ros vlp16_offline.launch bag_filenames:=$file