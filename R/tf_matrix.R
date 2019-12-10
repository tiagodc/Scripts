reg = readLAS('~/Desktop/tests/Ento_q.laz')
raw = readLAS('~/Desktop/tests/ento_q_raw.laz')

path = read.table('~/Desktop/tests/Ento_q_slam_path_aft_map.txt')
names(path) = c('time', 'x', 'y', 'z', 'roll', 'pitch', 'yaw')

unique(sort(raw@data$gpstime))[1:50] %>% print(digits=20)
sort(path$time)[1:10] %>% print(digits=20)
unique(sort(reg@data$gpstime))[1:10] %>% print(digits=20)

ts = sort(path$time)[201] %T>% print(digits=20)

pos = path[path$time == ts,]
pos$time %>% print(digits=20)
rawFrame = lasfilter(raw, gpstime == ts) %>% tlsCrop(0,0,25)
regFrame = lasfilter(reg, gpstime == ts)

# corregFrame = regFrame
# corregFrame@data$X = regFrame@data$Z
# corregFrame@data$Y = regFrame@data$X
# corregFrame@data$Z = regFrame@data$Y

newrawFrame = rawFrame
newrawFrame@data$X = rawFrame@data$Y
newrawFrame@data$Y = rawFrame@data$Z
newrawFrame@data$Z = rawFrame@data$X

clear3d() ; bg3d('black') ; pan3d(2)
rgl.points(rawFrame@data, size=2, col='white')
rgl.points(newrawFrame@data, size=2, col='green')
rgl.points(regFrame@data, size=2, col='red')

tfFrame = newrawFrame@data[,1:3]
torad = pi/180
tfMat = tfMatrix(pos$yaw*torad, pos$pitch*torad, pos$roll*torad, pos$x, pos$y, pos$z)
tfFrame = (tfMat %*% t(as.matrix(cbind(tfFrame, 1)))) %>% t %>% as.data.frame

rgl.points(tfFrame, size=4, col='blue')


pos
