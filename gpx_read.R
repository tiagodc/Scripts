# setwd('../Google Drive/GET - LIDAR/Log de Voo 19.09.2017/')

require(maptools)
gpx = readGPS(i = 'gpx', f = 'logvoo.gpx', type = 't')
gpx = gpx[ !is.na(gpx$V4) ,]

x = gpx$V4
y = gpx$V3
z = as.double(sub('M', '', gpx$V14))
t = as.double(as.character(gpx$V15)) *60*60*24

pos = data.frame(x,y,z,t)

logFile = read.table('log.log', sep = ';')
lgf = as.character(logFile$V1)

require(stringr)
sum(str_count(lgf, 'IMU,'))
