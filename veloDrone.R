#############################################################################
# 
# Important info:
# ATT = flight attitute
# TimeUS = time since the board started running, in micro seconds (US)
# pitch, roll, yaw = degrees
# 
#############################################################################

# setwd('../Desktop/eucflux/')

require(stringr)
require(sp)
require(rgdal)
require(lidR)
require(TreeLS)

### funtions
predictTimeStamps = function(gpsVelodyne, gmsList){
  charDate = as.character(gpsVelodyne[1,13])
  year = 2000 + as.double(substr(charDate, 5,6))
  month = as.double(substr(charDate, 3,4))
  day = as.double(substr(charDate, 1,2))
  weekDay = paste(year, month, day, sep='-')
  weekDay = as.double(strftime(weekDay,'%u'))
  weekDay = ifelse(weekDay==7,0,weekDay)
  
  charTime = as.character(gpsVelodyne[,5])
  hour = as.double(substr(charTime, 1,2))
  min  = as.double(substr(charTime, 3,4))
  sec  = as.double(substr(charTime, 5,6))
  
  gms  = ((hour*60*60 + min*60 + sec) + weekDay*24*60*60)*1000
  gmsToClock = lm(gpsVelodyne[,3] ~ gms)
  
  plot(gpsVelodyne[,3] ~ gms, xlab='GMS', ylab='TimeUS (Velodyne)', pch=20)
  abline(gmsToClock, col='red')
  
  estimated = gmsToClock$coefficients[1] + gmsToClock$coefficients[2] * gmsList
  names(estimated) = NULL
  
  return(estimated)
}

plotRotationMatrix = function(xyz , gps = c(0,0,0), inflate = c(1,1,1), cols = c('red','green','blue'), ...){
  
  xyz[1,] = inflate[1]*xyz[1,] + gps[1]
  xyz[2,] = inflate[2]*xyz[2,] + gps[2]
  xyz[3,] = inflate[3]*xyz[3,] + gps[3]
  
  rgl.lines(c(gps[1],xyz[1,1]), c(gps[2],xyz[2,1]), c(gps[3],xyz[3,1]), col=cols[1], ...)
  rgl.lines(c(gps[1],xyz[1,2]), c(gps[2],xyz[2,2]), c(gps[3],xyz[3,2]), col=cols[2], ...)
  rgl.lines(c(gps[1],xyz[1,3]), c(gps[2],xyz[2,3]), c(gps[3],xyz[3,3]), col=cols[3], ...)
}

clipSphere = function(cloud, rad = 1.5, center = c(0,0,0)) {
  if (center[1] == 0 & center[2] == 0) {
    dists = sqrt(cloud[,1]^2+cloud[,2]^2+cloud[,3]^2)
    out = cloud[dists <= rad, ]
  } else {
    dists = sqrt(( (cloud[,1]-center[1])^2 + (cloud[,2]-center[2])^2 + (cloud[,3]-center[3])^2 ))
    out=cloud[dists<=rad, ]
  }  
  return (out)
}

rotateYAxis = function(pointCloud, angleDegrees=0, axes=c('x','y','z')){
  
  # axVec = if(axis == 'x') c(1,0,0) else if (axis == 'y') c(0,1,0) else c(0,0,1)
  
  angRad = angleDegrees*pi/180
  
  rotMat = matrix(c(
    cos(angRad), 0, -sin(angRad),
    0,1,0,
    sin(angRad), 0, cos(angRad)
  ), nrow = 3, byrow = T)
  
  newCloud = as.matrix(pointCloud[,axes]) %*% rotMat
  pointCloud[,axes] = newCloud
  
  return(pointCloud)
}

flightRotationMatrix = function(yaw, pitch, roll){
  
  alpha = yaw   * pi/180
  beta  = pitch * pi/180
  gama  = roll  * pi/180
  
  Rz = matrix(c(
    cos(alpha) , -sin(alpha), 0,
    sin(alpha) , cos(alpha) , 0,
    0          , 0          , 1
  ), ncol=3, byrow = T)
  
  Ry = matrix(c(
    cos(beta) , 0, sin(beta),
    0         , 1, 0        ,
    -sin(beta), 0, cos(beta)
  ), ncol=3, byrow = T)
  
  Rx = matrix(c(
    1,         0, 0         ,
    0, cos(gama), -sin(gama),
    0, sin(gama), cos(gama)
  ), ncol=3, byrow = T)
  
  R3d = Rz %*% Ry %*% Rx
  
  return(R3d)
  
}


# read flight log
logVooFile = '70m_6m.s/2017-12-15 11-46-36.log'
cloudsDir = 'voo1/'
gpsFiles = dir(cloudsDir, pattern = '_gps.txt', full.names = T)

logVoo = read.table(logVooFile, head=F, as.is = T, sep=';')[,1]
gpsVelodyne = lapply(gpsFiles, read.table, header = F, as.is = T, sep=',')
gpsVelodyne = do.call(rbind, gpsVelodyne)

# isolate position and orientation rows
gps = logVoo[grepl('^GPS', logVoo)] 
# imu = logVoo[grepl('^IMU,', logVoo)] 
att = logVoo[grepl('^ATT', logVoo)]

# parse gps data
gps = as.data.frame(do.call(rbind, strsplit(gps, ', ', fixed = T)))
for(i in 2:ncol(gps)) gps[,i] = as.double(as.character(gps[,i]))
names(gps) = c('Sensor','TimeUS','Status','GMS','GWk','NSats','HDop','Lat','Lng','Alt','Spd','GCrs','VZ','U')
ord = order(gps$TimeUS)
gps = gps[ord,]


# project and convert gps to UTM
latLong = data.frame(lat = gps$Lat, lon = gps$Lng)

coordinates(latLong) = c('lon', 'lat')
proj4string(latLong) <- CRS("+init=epsg:4326")
res = spTransform(latLong, CRS("+init=epsg:22522"))

gps$utmLat = res@coords[,2]
gps$utmLng = res@coords[,1]

# parse attitude (orientation) data
att = as.data.frame(do.call(rbind, strsplit(att, ', ', fixed = T)))
for(i in 2:ncol(att)) att[,i] = as.double(as.character(att[,i]))
names(att) = c('Sensor', 'TimeUS','DesRoll','Roll','DesPitch','Pitch','DesYaw','Yaw','ErrRP','ErrYaw')

# calculate coordinates for each orientation 
att = cbind(att, GMS = NA, xPos = NA, yPos = NA, zPos = NA, utmLat = NA, utmLng = NA)
for(i in 2:nrow(gps)){
  
  # i = 2 
  pos1 = gps[i-1,]
  pos2 = gps[i,]
  
  timeDistance = pos2$TimeUS - pos1$TimeUS
  
  # ATT
  iRot = which(att$TimeUS < pos2$TimeUS & att$TimeUS >= pos1$TimeUS)
  rotations = att[iRot,]
  
  posRotation = (rotations$TimeUS - pos1$TimeUS)/timeDistance
  
  GMS  = pos1$GMS + posRotation * (pos2$GMS - pos1$GMS)
  xPos = pos1$Lng + posRotation * (pos2$Lng - pos1$Lng)
  yPos = pos1$Lat + posRotation * (pos2$Lat - pos1$Lat)
  zPos = pos1$Alt + posRotation * (pos2$Alt - pos1$Alt)
  utmLat  = pos1$utmLat + posRotation * (pos2$utmLat - pos1$utmLat)
  utmLng  = pos1$utmLng + posRotation * (pos2$utmLng - pos1$utmLng)
  
  att[iRot,c('GMS', 'xPos', 'yPos', 'zPos','utmLat', 'utmLng')] = cbind(GMS, xPos, yPos, zPos, utmLat, utmLng)
  
}
att = att[!is.na(att$xPos),]
att$velodyneTimeStamp = predictTimeStamps(gpsVelodyne, att$GMS)

# windows()
# par(mfrow=c(1,3))
plot(att$Roll ~ att$velodyneTimeStamp, pch=20, cex=.5)
spl = smooth.spline(att$velodyneTimeStamp, att$Roll, df=100)
lines(spl$x, spl$y, col='red')
att$Roll = spl$y

plot(att$Pitch ~ att$velodyneTimeStamp, pch=20, cex=.5)
spl = smooth.spline(att$velodyneTimeStamp, att$Pitch, df = 100)
lines(spl$x, spl$y, col='red')
att$Pitch = spl$y

roughCh = with(att, which(Yaw[-1] - Yaw[-length(Yaw)] > 10))
roughYaw = att$velodyneTimeStamp[roughCh]

att$Yaw = ifelse(att$velodyneTimeStamp <= roughYaw, att$Yaw+360, att$Yaw)
plot(att$Yaw ~ att$velodyneTimeStamp, pch=20, cex=.5)
spl = smooth.spline(att$velodyneTimeStamp, att$Yaw, df=100)
lines(spl$x, spl$y, col='red')
att$Yaw = spl$y

plot(att$zPos ~ att$velodyneTimeStamp, pch=20, cex=.5)
spl = smooth.spline(att$velodyneTimeStamp, att$zPos, df=50)
lines(spl$x, spl$y, col='red')
att$zPos = spl$y

rm(gpsVelodyne, gps, latLong, res, logVoo)

# als = read.table('MDT_IPEF_2PTS.xyz')

# clear3d()
# rgl::plot3d(att[,c('utmLng', 'utmLat', 'zPos')], size=3, col='black', aspect = F)
# rgl.points(als, size=.5, col=cloud.col(als, topo.colors, 30))

# read point clouds
cloudFiles = sub(pattern = '_gps', '', gpsFiles)
pointCloud = lapply(cloudFiles, read.table, header = T, colClasses='numeric')
pointCloud = do.call(rbind, pointCloud)

# cross time and gps position
qt = quantile(1:nrow(pointCloud), probs = c(.1,.101))
firstCloud = pointCloud[qt[1]:qt[2],]
azim = firstCloud$azimuth
azim = mean(azim[ azim < 360])
azim = 45

# clear3d()
# rgl.points(firstCloud[,c('x','y','z')])
# axes3d()
# 
# rotMat = flightRotationMatrix(0,azim,0)
# rotCloud = as.matrix(firstCloud[,c('x','y','z')]) %*% rotMat
# 
# rgl.points(rotCloud, col='red')
# apply(rotCloud, 2, range)

yAngle = azim
rotMat = flightRotationMatrix(0,yAngle,0)
pointCloud[,c('x','y','z')] = as.matrix(pointCloud[,c('x','y','z')]) %*% rotMat

pointCloud$z = -pointCloud$z
singleStamps = unique(pointCloud$timeStamp)


inflation = c(30,30,30)
skip = 50
j = 12000
i = singleStamps[j]
bottom = which(att$velodyneTimeStamp > i)[1]
temp = apply(att[c(bottom-1, bottom),-1], 2, mean)
pointSelect = which(pointCloud$timeStamp >= i & pointCloud$timeStamp < singleStamps[j+skip])
tempPos = as.double(temp[c('utmLng', 'utmLat', 'zPos')])

rotMat1 = flightRotationMatrix(temp['Yaw'], -temp['Pitch'], -temp['Roll'])

rotMat2 = flightRotationMatrix(-temp['Yaw'], temp['Pitch'], temp['Roll'])

tempCloud = pointCloud[pointSelect, c('x', 'y', 'z')]

laz = as.matrix(tempCloud) %*% rotMat1
tempMat = as.data.frame(laz)
names(tempMat) = c('x', 'y', 'z')

# tempMat$x = (pointCloud[pointSelect, c('x')] + tempPos[1])
# tempMat$y = (pointCloud[pointSelect, c('y')] + tempPos[2])
# tempMat$z = (tempPos[3] - pointCloud[pointSelect, c('z')])

clear3d()
bg3d('black')
rgl.points(tempCloud, col='white')
axes3d()
rgl.points(tempMat, col='red')
plotRotationMatrix(rotMat2, c(0,0,-80), inflation, lwd=2)
plotRotationMatrix(diag(rep(1,4)), c(0,0,-80), inflation, cols = c('darkred', 'darkgreen', 'darkblue'), lwd=5)




inflation = c(10,10,10)
clear3d()
rgl::plot3d(att[,c('utmLng', 'utmLat', 'zPos')], size=3, col='black', aspect = F)
# rgl.points(als, size=.5, col=cloud.col(als, topo.colors, 30))

skip = 200
j = 1
altThr = min(att$zPos) + 65
while(j < length(singleStamps) ){
  
  i = singleStamps[j]
  # i=1000000
  bottom = which(att$velodyneTimeStamp > i)[1]
  
  if(bottom < 1){
    j = j+skip
    next
  }
  
  temp = apply(att[c(bottom-1, bottom),-1], 2, mean)
  
  if(temp['zPos'] < altThr){
    j = j+skip
    next
  }
  
  pointSelect = which(pointCloud$timeStamp >= i & pointCloud$timeStamp < singleStamps[j+skip])
  
  tempPos = as.double(temp[c('utmLng', 'utmLat', 'zPos')])

  rotMat = flightRotationMatrix(-temp['Yaw'], temp['Pitch'], temp['Roll'])

  plotRotationMatrix(rotMat, tempPos, inflation)

  rotMat = flightRotationMatrix(temp['Yaw'], -temp['Pitch'], -temp['Roll'])
  
  laz = as.matrix(pointCloud[pointSelect, c('x', 'y', 'z')]) %*% rotMat
  pointCloud[pointSelect, c('x', 'y', 'z')] = laz;

  pointCloud[pointSelect, c('x')] = (pointCloud[pointSelect, c('x')] + tempPos[1])
  pointCloud[pointSelect, c('y')] = (pointCloud[pointSelect, c('y')] + tempPos[2])
  pointCloud[pointSelect, c('z')] = (tempPos[3] + pointCloud[pointSelect, c('z')])

  rgl.points(pointCloud[pointSelect, c('x', 'y', 'z')], col='black')
    
  print(paste(which(singleStamps == i), 'of', length(singleStamps)))
  
  # rgl.snapshot(str_c('pics/',j,'.png'))
  j = j+skip

}

pointCloud = pointCloud[ pointCloud$z > 200,]
dim(pointCloud)

names(pointCloud)[6:9] = c('X', 'Y', 'Z', 'Intensity')


writeLAS(LAS(pointCloud[6:9]), 'eucflux1.laz')
# # plot all data combined
# clear3d()
# rgl::plot3d(att[,c('utmLng', 'utmLat', 'zPos')], size=3, col='black', aspect = F)
# rgl.points(pointCloud[, c('X', 'Y', 'Z')], size = 1, col='black')
# ,col=cloud.col(pointCloud[, c('X', 'Y', 'Z')],topo.colors ,n = 50), size=1)
# 
# als = read.table('MDT_IPEF_2PTS.xyz')
# 
# grounded = readLAS('eucflux1_g.laz')
# ground = grounded@data[ grounded@data$Classification == 2 , ]
# 
# bound = apply(ground[,c('X','Y')], 2, range)
# 
# clipAls = als[ with(als, V1 > bound[1,1] & V1 < bound[2,1] & V2 > bound[1,2] & V2 < bound[2,2]) ,]
# 
# clear3d()
# bg3d('black')
# rgl.points(ground, col=cloud.col(ground, topo.colors, 30))
# rgl.points(clipAls, col='white', size=.5)
# 
# names(clipAls) = c('X', 'Y', 'Z', 'Intensity')
# 
# writeLAS(LAS(clipAls), 'chao7.laz')
# 
# rgl.points(grounded@data, col='grey', size=.5)
# 
# 
# apply(als, 2, range)
# apply(att[,-1], 2, range)
# 
# inflation = apply(att[,c('utmLng', 'utmLat', 'zPos')], 2, function(x) diff(range(x, na.rm = T)))/30
# inflation = c(10,10,10)
# 
# skip = 1
# i=1
# cloudAcum = data.frame()
# while(i <= nrow(cloudCoordinates)){
#   
#   temp = cloudCoordinates[i,]
#   
#   if(is.na(temp$xPos) | i < 10 | i > 70){
#     i = i + skip
#     next
#   }
#   
#   tempPos = as.double(temp[,c('utmLng', 'utmLat', 'zPos')])
#   rotMat = xyz.rotation.matrix(temp$Roll, temp$Pitch, temp$Yaw) 
#   
#   # clear3d(); bg3d('#000000')
#   plotRotationMatrix(rotMat, tempPos, inflation)
#   # plotRotationMatrix(diag(rep(1,3)), tempPos, inflation, cols = c('orange', 'cyan', 'magenta'))
#   
#   laz = readLAS( str_c(folder,temp$file), XYZonly = T )
#   laz = as.data.frame(rotateYAxis(laz@data, yAngle))
#   
#   rotMat = xyz.rotation.matrix(-temp$Roll, -temp$Pitch, -temp$Yaw) 
#   
#   laz = as.matrix(laz) %*% rotMat
#   laz[,1] = (laz[,1] + tempPos[1])
#   laz[,2] = (laz[,2] + tempPos[2])
#   laz[,3] = (-laz[,3] + tempPos[3])
#   
#   cloudAcum = rbind(cloudAcum, laz)
#   
#   rgl.points(laz, size=1.5, col='black')
#   
#   print(paste(100*round(i/nrow(cloudCoordinates), digits = 4), '%'))
#   
#   i = i + skip
# }
# 
# names(cloudAcum) = c('X', 'Y', 'Z')
# exportCloud = LAS(cloudAcum)
# writeLAS(exportCloud, 'fibria.laz')
# 
# rm(list = ls())
# 
# require(rgl)
# 
# exportCloud = lidR::readLAS('areiao.laz', XYZonly=T)
# plot(exportCloud, size=.5)
# axes3d(color='white')

# imgCloud = lidR::readLAS('Nuvem_pontos.las', XYZonly = T)
# nPts = sample(1:nrow(imgCloud@data), 1000000, replace = F)
# redCloud = imgCloud@data[nPts,]

# deslocateCLoud = exportCloud@data
# deslocateCLoud[,3] = deslocateCLoud[,3]+13.4
# 
# clear3d()
# bg3d('black')
# rgl.points(redCloud, col='white', size=1)
# rgl.points(deslocateCLoud, 
#            col=cloud.col(exportCloud@data, n = 100), 
#            size=1.5)
# axes3d(col='red')
