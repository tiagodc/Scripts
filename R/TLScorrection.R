require(lidR)
require(rgl)
require(magrittr)
source('TLSprocess.R')

# setwd('C:/Work/gerdau/')

swapAxes = function(las, swap='xyz'){
  
  if(class(las) == 'LAS'){
    df = data.frame(
      x = las@data$X,
      y = las@data$Y,
      z = las@data$Z
    )
  } else {
    df = data.frame(
      x = las[,2],
      y = las[,3],
      z = las[,4]
    )
  }
  
  for(i in 1:3){
    char = substr(swap, i, i) %>% tolower
    
    if(class(las) == 'LAS'){
      las@data[,i] = df[,char]
    } else{
      las[,i+1] = df[,char]
    }
  }
  
  return(las)
  
}

cloudSample = function(las, size=500000){
  n = nrow(las@data)
  keep = sample(1:n, size, F)
  las = LAS(las@data[keep,])
  return(las)
}

lasFiles = dir(pattern = '\\.laz$')

i = 7
file = lasFiles[i] %T>% print
slamFile = sub('\\.laz', '_slam_path.txt', file) %T>% print

las = readLAS(file)
slam = read.table(slamFile)

if(grepl('00\\.laz$',file)){
  # 00
  swap = 'xzy'
  las %<>% swapAxes(swap) ; slam %<>% swapAxes(swap)
  
} else if(grepl('45\\.laz$',file)){
  # 45
  las@data$Z = -las@data$Z ; slam[,4] = -slam[,4]
}

apply(las@data[,1:3], 2, range)

shortLas = cloudSample(las)
plot(shortLas) ; axes3d(col='#FFFFFF')
spheres3d(slam[,2], slam[,3], slam[,4], 1, col='white')

tempFile = sub('\\.laz$', '_temp.laz', file)
writeLAS(las, tempFile)

rotationData = rotateCloud(tempFile)
lasRot = rotationData$cloud  
slam[,2:4] = as.matrix(slam[,2:4]) %*% rotationData$matrix

apply(lasRot@data[,1:3], 2, range)
shortLas = cloudSample(lasRot)
plot(shortLas) ; axes3d(col='#FFFFFF')
spheres3d(slam[,2], slam[,3], slam[,4], 1, col='white')

unlink(tempFile, force = T)

rotFile = sub('\\.laz$', '_rot.laz', file)
writeLAS(lasRot, paste0('rotated/',rotFile))

slamFile = sub('\\.txt$', '_rot.txt', slamFile)
write.table(slam, paste0('rotated/',slamFile), row.names = F, col.names = F)

