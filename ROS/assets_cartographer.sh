#!/bin/bash

file=$1

source ${HOME}/catkin_cartographer/install_isolated/setup.bash

roslaunch cartographer_ros vlp16_assets.launch bag_filenames:=$file pose_graph_filename:=$file".pbstream"