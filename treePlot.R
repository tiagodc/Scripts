require(TreeLS)
require(lidR)

lazMatrix = function(lazFile){
  return( data.frame(X = lazFile@data$X, Y = lazFile@data$Y, Z = lazFile@data$Z, class = lazFile@data$Classification) )  
}

eucDist = function(px, py, vx, vy, dmax = 0.1){
  
  px = as.double(px)
  py = as.double(py)
  
  ed = sqrt((vx-px)^2+(vy-py)^2)
  
  return( which(ed <= dmax) )
  
}

setwd('~/Desktop/silvilaser/')
files = dir('./plot_clouds/', '.*\\.laz')

for(file in files){

# lazPlot = lidR::readLAS('duratex_p1464_transecto.laz')
# summary(lazPlot)

lgd = paste('lasground -i', file, '-odix _g -olaz -replace_z -no_bulge -wilderness')

system(lgd)

lazNorm = readLAS(sub('.laz', '_g.laz', file))


clouds_dir = 'plot_clouds/'
for(f in 2:length(files)){
file = paste(clouds_dir, files[f] ,sep='')
lazNorm = readLAS(file)

cmd = paste('./las2rings -i', file, '-d 0.05 -v 3 -l LOW -u UP')

results = data.frame()

for(i in c(.5,1,1.5,2,2.5,3)){
  
  cmdTemp = sub('LOW', i, cmd)  
  cmdTemp = sub('UP', i+1, cmdTemp)  
  
  system(cmdTemp)
  
  report_file = sub('\\.laz', '_result.txt', file)
  
  treeLocations = read.table(report_file, header = T)
  
  results = rbind(results, treeLocations)
  
}

keep = data.frame()
nms = names(results)
while(nrow(results) > 0){
  
  point = results[1,]
  
  sameBole = eucDist(px = point['x_main'], py = point['y_main'], vx = results$x_main, vy = results$y_main)
  
  if(length(sameBole) >= 3){
    keep = rbind(keep, apply(results[sameBole,],2,mean))
  }
  
  results = results[-sameBole,]
  print(nrow(results))
}
names(keep) = nms

onePerTree = data.frame()

while(nrow(keep) > 0){
  
  x = keep[1,'x_main']
  y = keep[1,'y_main']
  
  dst = eucDist(x,y,keep$x_main, keep$y_main, .5)
  
  if(length(dst) > 1){
    temp = apply(keep[dst,], 2, mean)
  }else{
    temp = keep[dst,]
  }
  
  onePerTree = rbind(onePerTree, temp)
  names(onePerTree) = nms
  keep = keep[-dst,]
  
}

clear3d()
bg3d('black')
rgl.points(lazMatrix(lazNorm), size=.5, col='white')
with(onePerTree, spheres3d(x_main, y_main, (z_min+z_max)/2, r_main, col='green'))
axes3d()


clear3d()
bg3d('black')
rad = 1
for(row in 1:nrow(onePerTree)){
# row = 1
xy  = as.double(onePerTree[row,1:2])
arv = clip.XY(lazMatrix(lazNorm), rad = rad, center = xy)
arvlaz = LAS(arv)
lidR::writeLAS(arvlaz,'tree.laz')

system('./las2rings -i tree.laz -t')

t = readLAS('tree_cloud.las')

if(is.null(t)) next

height = diff(range(t@data$Z))

if(height < 3) next

if(!exists('stems')){
  stems = t
}else{
  stems@data = rbind(stems@data, t@data)
}

rgl.points(t@data, col=rainbow(nrow(onePerTree))[row])
rgl.points(arv, size=.5)
}
rgl.points(lazMatrix(lazNorm), size=.5, col='darkgrey')

writeLAS(stems, sub('.laz', '_stems.laz', file))
write.table(onePerTree, sub('.laz', '_trees.txt', file), col.names = T, row.names = F)

rm(list = c('stems', 'onePerTree', 'results', 'keep', 'nms') )
}
