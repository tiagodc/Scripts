if(!require(devtools)){
  install.packages('devtools')
  require(devtools)
}

# install lidR (developing) version 2.0
devtools::install_github("Jean-Romain/rlas", dependencies=TRUE, ref="devel")
devtools::install_github("Jean-Romain/RCSF")
devtools::install_github("Jean-Romain/lidR", dependencies=TRUE, ref="devel")

# load TLS processing functions
source('C:/Work/Scripts/R/TLSprocess.R')

# set cloud directory
setwd('D:/Foridar/Gerdau/outubro/')

# choose files to work on
files = dir(pattern = '\\.laz$')

outDir = 'normalized'
# dir.create(outDir)

## 00 = shift_z
## 45 = NA
## 90 = NA

# i=11
for(i in 1:length(files)){
  file = files[i]
  
  paste(i, '-', file) %>% print
  
  slamFile = sub('\\.laz$', '_slam_path.txt', file)
  
  # read point cloud and slam file
  cld = readLAS(file)
  slam = read.table(slamFile)
  
  # check point cloud visually
  # plot(cld) ; axes3d(col='white')
  # apply(cld@data[,1:3], 2,range)
  
  # cld %<>% gpsTimeFilter(.5)
  
  # correct point cloud axes if necessary
  # ...set "upside_down = T" if the trees are facing downwards
  # ...set "mirrored = T" if placement of the trees in the plot is reversed in relation to the field
  # ...set "shift_z_axis = T" if the height is not represented by the Z axis
  
  if(grepl('00\\.laz$', file)){
    cld %<>% correctCloud(shift_z_axis = T)
    slam %<>% correctCloud(shift_z_axis = T)
  }
  
  # rotate cloud to get horisontal ground
  cld %<>% rotateCloudInternal()
  
  # correct slam file to fit the rotated point cloud
  slam[,2:4] = as.matrix(slam[,2:4]) %*% cld$matrix
  cld = cld$cloud
  
  # normalize the point cloud and remove the ground points (optional) for a clear view of the trees
  cld %<>% dtmNormalize(keepGround = F, res = 1)
  
  # check if all went well
  # plot(cld) ; axes3d(col='#FFFFFF')
  
  # name files to be written
  outLas = paste(outDir, sub('.laz','_norm.laz',file,fixed = T), sep='/')
  outSlam = paste(outDir, sub('.txt', '_norm.txt', slamFile, fixed = T), sep='/')
  
  # write normalized files
  cld@header@PHB$`X scale factor` = .0001
  cld@header@PHB$`Y scale factor` = .0001
  cld@header@PHB$`Z scale factor` = .0001
  
  writeLAS(cld, outLas)
  write.table(slam, outSlam, row.names = F, col.names = F)
}
