require(TreeLS)
require(lidR)

lazMatrix = function(lazFile){
  return( data.frame(x = lazFile@data$X, y = lazFile@data$Y, z = lazFile@data$Z) )  
}

laz = readLAS('000981.laz')

system('lasground -i 000981.laz -odix _g -olaz -replace_z -wilderness')

lazNorm = readLAS('000981_g.laz')

nuv  = lazMatrix(lazNorm)
gMax = max(nuv[lazNorm@data$Classification ==2, 'z']) 
nuv  = nuv[nuv[,3] < 6 & nuv[,3] > 0.1,]

yMax = range(nuv$y)
xMax = range(nuv$x)

step = 20
classX = cut(nuv$x, seq(xMax[1], xMax[2]+step, step))
classY = cut(nuv$y, seq(yMax[1], yMax[2]+step, step))

counts   = as.data.frame(table(classX, classY))
minCount = quantile(counts$Freq, .95)

keepClasses = counts[counts$Freq >= minCount,]
keepMerged = apply(keepClasses[,1:2], 1, paste, collapse="")

keepCloud = data.frame()
for(i in 1:nrow(nuv)){
  
  cls = paste(classX[i], classY[i], sep="")
  
  charX = as.character(classX[i])
  charX = substr(charX, 2, nchar(charX)-1)
  charX = strsplit(charX, ',', T)[[1]]
  charX = as.double(charX)
  
  charY = as.character(classY[i])
  charY = substr(charY, 2, nchar(charY)-1)
  charY = strsplit(charY, ',', T)[[1]]
  charY = as.double(charY)
  
  
  if(cls %in% keepMerged){
    
    keepCloud = rbind(keepCloud, c(nuv[i,], charX, charY ))
    names(keepCloud) = NULL
  }
  
  print(round(i/nrow(nuv)*100, 0))
  
}

names(keepCloud) = c('x','y', 'z', 'x1', 'x2', 'y1', 'y2')

rgl.points(keepCloud, size=1, col='red')
rgl.points(nuv, size=.5)
axes3d()


