require(rgl)

plotLAS = function(cloud, size=1, new=F, col=NULL){
  if(new) open3d()
  if(is.null(col)) col = TreeLS::cloud.col(cloud, n = 20)
  bg3d('black')
  rgl.points(cloud, size=size, col=col)
  axes3d()
}

slamDir = '~/SLAM/slam6d-code/bin/'
dataDir = '~/Desktop/scripts/datasets/hannover1/'
setwd(slamDir)

cmd = './slam6D -s 1 -e 65 -r 10 -i 100 -d 75 --epsICP=0.00001 -D 250 -I 50 --cldist=750 -G 1'
cmd = paste(cmd, dataDir)

system(cmd)
setwd(dataDir)

frames = dir(pattern = '\\.frames$')
frames = sub('\\.frames$', '', frames)

clouds = paste(frames, '.3d', sep='')
coregs = paste(frames, '.frames', sep='')
cols = rainbow(length(frames))

{
track = data.frame()
for(i in 1:length(frames)
    ){
  
  nuv = read.table(clouds[i], nrows = 10)
  nuv = as.matrix(read.table(clouds[i], colClasses = apply(nuv,2,class)))
  slam = read.table(coregs[i])
  slam = as.double(slam[nrow(slam),])
  
  rotation = matrix(slam[c(1:3,5:7,9:11)], nrow = 3, byrow = T)
  translation = slam[13:15]
  track = rbind(track, translation)
  
  head(nuv)

  nuv = crossprod(t(nuv), rotation)
  nuv[,1] = nuv[,1] + translation[1]
  nuv[,2] = nuv[,2] + translation[2]
  nuv[,3] = nuv[,3] + translation[3]
  #nuv = nuv %*% rotation
  
  head(nuv)
  
  plotLAS(nuv, size = .5, col = cols[i])
}
plot(track[,c(3,1)], cex=1.5, pch=20, col='red')
}
