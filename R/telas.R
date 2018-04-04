require(TreeLS)
require(lidR)

lazMatrix = function(lazFile){
  return( data.frame(x = lazFile@data$X, y = lazFile@data$Y, z = lazFile@data$Z, 
                     int = lazFile@data$Intensity, class = lazFile@data$Classification) )  
}

laz = readLAS('000981.laz')

system('lasground -i 000981.laz -odix _g -olaz -replace_z -wilderness')

{
lazNorm = readLAS('000981_g.laz')

nuv  = lazMatrix(lazNorm)
gMax = max(nuv[lazNorm@data$Classification ==2, 'z']) 
nuv  = nuv[nuv[,3] < 6 & nuv[,3] > 0.1,]

yMax = range(nuv$y)
xMax = range(nuv$x)

step = 20
classX = cut(nuv$x, seq(xMax[1], xMax[2]+step, step))
classY = cut(nuv$y, seq(yMax[1], yMax[2]+step, step))
classXY = paste(classX, classY, sep='')

counts   = as.data.frame(table(classX, classY))
countsXY = paste(counts$classX, counts$classY, sep='')
minCount = quantile(counts$Freq, .95)

keepClasses = counts[counts$Freq >= minCount,]
keepMerged  = countsXY[counts$Freq >= minCount]


keepCloud = data.frame()
cells = data.frame()
for(i in 1:length(keepMerged)){
  xySqr = keepMerged[i]
  obs = which(classXY == xySqr)
  
  charX = as.character(keepClasses[i,1])
  charX = substr(charX, 2, nchar(charX)-1)
  charX = strsplit(charX, ',', T)[[1]]
  charX = as.double(charX)
  
  charY = as.character(keepClasses[i,2])
  charY = substr(charY, 2, nchar(charY)-1)
  charY = strsplit(charY, ',', T)[[1]]
  charY = as.double(charY)

  keepCloud = rbind(keepCloud, cbind(nuv[obs,], x1 = charX[1], x2 = charX[2], y1 = charY[1], y2 = charY[2]))
  cells     = rbind(cells, c(charX, charY))
  
  row.names(keepCloud) = NULL
  row.names(cells)     = NULL
  
}

names(cells) = c('x1', 'x2', 'y1', 'y2')

minIntensity = quantile(keepCloud$int, probs = .9)

keepIntensity = keepCloud[keepCloud$int > minIntensity,]

nbhd = c()
for(i in 1:nrow(keepIntensity)){
  
  xy = as.double(keepIntensity[i,1:2])
  
  dist = sqrt( (keepIntensity$x - xy[1])^2 + 
               (keepIntensity$y - xy[2])^2 )
  
  close = which(dist <= 3)
  nbhd = c(nbhd, length(close))
  
}

keepDense = keepIntensity[nbhd >= 100,]

clear3d()
bg3d('black')

rgl.points(keepDense, size=2, col='green')
rgl.points(keepIntensity, size=1.5, col='blue')
rgl.points(keepCloud, size=1, col='red')
rgl.points(lazMatrix(lazNorm), size=.5)
axes3d()
}

