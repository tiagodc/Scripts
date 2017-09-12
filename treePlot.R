require(TreeLS)
require(lidR)

lazMatrix = function(lazFile){
  return( data.frame(X = lazFile@data$X, Y = lazFile@data$Y, Z = lazFile@data$Z) )  
}

eucDist = function(px, py, vx, vy, dmax = 0.1){
  
  px = as.double(px)
  py = as.double(py)
  
  ed = sqrt((vx-px)^2+(vy-py)^2)
  
  return( which(ed <= dmax) )
  
}

lazPlot = lidR::readLAS('duratex_p1464_transecto.laz')
summary(lazPlot)

system('lasground -i duratex_p1464_transecto.laz -odix _g -olaz -replace_z -no_bulge -wilderness')

lazNorm = readLAS('duratex_p1464_transecto_g.laz')

rgl.points(lazMatrix(lazNorm), size=.5)
axes3d()

{
  
clear3d()
bg3d('black')
rgl.points(lazMatrix(lazNorm), size=.1, col='white')
axes3d()

cmd = 'las2rings -i duratex_p1464_transecto_g.laz -d 0.02 -v 2 -l LOW -u UP -p 0.02'

results = data.frame()

for(i in c(.5,1,1.5,2,2.5)){
  
  cmdTemp = sub('LOW', i, cmd)  
  cmdTemp = sub('UP', i+1, cmdTemp)  
  
  system(cmdTemp)
  
  treeLocations = read.table('duratex_p1464_transecto_g_result.txt', header = T)
  
  results = rbind(results, treeLocations)
  
  with(treeLocations, spheres3d(x_main, y_main, (z_min+z_max)/2, r_main, col='green'))
  
}

keep = data.frame()
nms = names(results)
while(nrow(results) > 0){
  
  point = results[1,]
  
  sameBole = eucDist(px = point['x_main'], py = point['y_main'], vx = results$x_main, vy = results$y_main)
  
  if(length(sameBole) >= 3){
    keep = rbind(keep, apply(results[sameBole,],2,mean))
    # keep = rbind(keep, point)
  }
  
  results = results[-sameBole,]
  print(nrow(results))
}
names(keep) = nms

clear3d()
bg3d('black')
rgl.points(lazMatrix(lazNorm), size=.5, col='white')
with(keep, spheres3d(x_main, y_main, (z_min+z_max)/2, r_main, col='green'))
axes3d()

nrow(keep)



clear3d()
bg3d('black')
for(row in 1:nrow(keep)){
# row = 1
xy  = as.double(keep[row,1:2])
rad = 1
arv = clip.XY(lazMatrix(lazNorm), rad = rad, center = xy)
arvlaz = LAS(arv)
lidR::writeLAS(arvlaz,'tree.laz')

# rgl.points(arv)

system('las2rings -i tree.laz -t')

t = readLAS('tree_cloud.las')

# clear3d()
# bg3d('black')
rgl.points(t@data, col=rainbow(nrow(keep))[row])
rgl.points(arv, size=.5)
}
rgl.points(lazMatrix(lazNorm), size=.5, col='darkgrey')

}
