if(!require(devtools)){
  install.packages('devtools')
  require(devtools)
}

# install lidR (developing) version 2.0
devtools::install_github("Jean-Romain/rlas", dependencies=TRUE, ref="devel")
devtools::install_github("Jean-Romain/RCSF")
devtools::install_github("Jean-Romain/lidR", dependencies=TRUE, ref="devel")

# load TLS processing functions
source('https://raw.githubusercontent.com/tiagodc/Scripts/master/R/TLSprocess.R')

# set cloud directory
setwd('d:/')

# choose files to work on
file = 'Caba04_04_07_u45_rot.laz'

slamFile = sub('_rot.laz', '_slam_path_rot.txt', file, fixed = T)

# read point cloud and slam file
cld = readLAS('Caba04_04_07_u45_rot.laz')
slam = read.table('Caba04_04_07_u45_slam_path_rot.txt')

# check point cloud visually
plot(cld) ; axes3d(col='white')

# correct point cloud axes if necessary
# ...set "upside_down = T" if the trees are facing downwards
# ...set "mirrored = T" if placement of the trees in the plot is reversed in relation to the field
# ...set "shift_z_axis = T" if the height is not represented by the Z axis

cld %<>% correctCloud(upside_down = T)
slam %<>% correctCloud(upside_down = T)

# rotate cloud to get horisontal ground
cld %<>% rotateCloudInternal()

# correct slam file to fit the rotated point cloud
slam[,2:4] = as.matrix(slam[,2:4]) %*% cld$matrix
cld = cld$cloud

# normalize the point cloud and remove the ground points (optional) for a clear view of the trees
cld %<>% lowResNormalize(keepGround = F)

# check if all went well
plot(cld) ; axes3d(col='#FFFFFF')

# name files to be written
outLas = sub('.laz','_norm.laz',file,fixed = T)
outSlam = sub('.txt', '_norm.txt', slamFile, fixed = T)

# write normalized files
writeLAS(cld, outLas)
write.table(slam, outSlam, row.names = F, col.names = F)
