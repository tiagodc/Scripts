require(TreeLS)
require(lidR)

lazMatrix = function(lazFile){
  return( data.frame(X = lazFile@data$X, Y = lazFile@data$Y, Z = lazFile@data$Z, 
                     class = lazFile@data$Classification) )  
}

eucDist = function(px, py, vx, vy, dmax = 0.1){
  
  px = as.double(px)
  py = as.double(py)
  
  ed = sqrt((vx-px)^2+(vy-py)^2)
  
  return( which(ed <= dmax) )
  
}

files = dir('LAZ_norm/', pattern = '\\.laz')
files = paste('LAZ_norm/', files, sep='')

for(fileName in files){

print(paste('## nuvem', which(files == fileName), 'de', length(files)))
  
cmd = paste('las2rings -i',  fileName, '-d 0.01 -v 2 -l LOW -u UP')

results = data.frame()
for(i in c(.5,1,1.5,2,2.5,3)){
  
  cmdTemp = sub('LOW', i, cmd)  
  cmdTemp = sub('UP', i+1, cmdTemp)  
  
  system(cmdTemp)
  
  treeLocations = read.table(sub('\\.laz', '_result.txt' ,fileName), header = T)
  
  results = rbind(results, treeLocations)
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
  # print(nrow(results))
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

lazNorm = readLAS(fileName)

clear3d()
bg3d('black')
rad = 1
minHeight = 3
for(row in 1:nrow(onePerTree)){
  xy  = as.double(onePerTree[row,1:2])
  arv = clip.XY(lazMatrix(lazNorm), rad = rad, center = xy)
  arvlaz = LAS(arv)
  lidR::writeLAS(arvlaz,'tree.laz')
  
  system('las2rings -i tree.laz -t -z 1')
  
  t = readLAS('tree_cloud.las')
  
  if(is.null(t)) next
  
  height = diff(range(t@data$Z))
  
  if(height < minHeight) next
  
  if(!exists('stems')){
    stems = t
  }else{
    stems@data = rbind(stems@data, t@data)
  }
  
  rgl.points(t@data, col=rainbow(nrow(onePerTree))[row])
  rgl.points(arv, size=.5)
  
  print(round(row/nrow(onePerTree), digits = 3)*100)
}
rgl.points(lazMatrix(lazNorm), size=.5, col='darkgrey')

writeLAS(stems, sub('.laz', '_stems.laz', fileName))
write.table(onePerTree, sub('.laz', '_trees.txt', fileName), col.names = T, row.names = F)

}

