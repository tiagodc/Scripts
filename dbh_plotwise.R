require(TreeLS)
require(lidR)
require(stringr)

# new RANSAC circle estimation
RANSAC.olofsson = function(stem.sec, n=3, p=.8, P=.99, timesN=50, inner_circle = 0.015){
  
  slc = stem.sec = dbhSegment
  
  if(nrow(stem.sec) < n) n = nrow(stem.sec)
  
  N = log(1 - P) / log(1 - p^n)
  
  data = matrix(ncol=5, nrow=0)
  for(j in 1:(timesN*N)){
    a = sample(1:nrow(slc), size = n)
    
    b = tryCatch(circlefit(slc[a,1], slc[a,2]),
                 error = function(con){ return('next') },
                 warning = function(con) return('next'))
    
    if(b == 'next') next
    
    b_euc = sqrt( (slc[,1] - b[1])^2 + (slc[,2] - b[2])^2 )
    b_log = b_euc <= b[3]+inner_circle & b_euc >= b[3]-inner_circle 
    
    b_inliers  = length(b_euc[b_log])
    b_outliers = length(b_euc[!b_log])
    b_total    = length(b_euc)
    
    b = c(b, b_inliers/b_total)
    
    data = rbind(data, b)
  }
  
  if(nrow(data) == 0){ dt = NULL }else{
    data = data[ data[,5] > 0.25 ,]
    
    if(length(data) == 0){ 
      dt = NULL 
    }else if(is.vector(data)) {
      dt = data
      names(dt) = c('x', 'y', 'rad', 'err', 'in')
    }else{
      ord = order(data[,5], data[,4], decreasing = T)
      dt = data[ord[1],]
      names(dt) = c('x', 'y', 'rad', 'err', 'in')
    }
  }
  
  return(dt)
  
}

setwd('~/ProLiDAR/Pilotos/TLS/LAZ/')
# dir.create('normalized')
# dir.create('stems')
files = dir(pattern = '*.\\.laz')

# files = c(
#   "klabin_p1180_parado_90.laz", #done 
#   "klabin_p224_central_rambo.laz", #done
#   "klabin_p228_transecto_45.laz", #done
#   "klabin_p228_transecto.laz", #done
#   "klabin_p5830_girando.laz", #done
#   "klabin_p5830_girando_rotacionado.laz",
#   "klabin_p232_transecto_45.laz",
#   "klabin_p228_aleatorio2.laz",
#   "klabin_p224_u.laz",
#   "klabin_p224_transecto_45_rotacionado.laz",
#   "klabin_p224_transecto_45.laz",
#   "klabin_p224_central_rambo45.laz",
#   "duratex_p1480_transecto_giro.laz",
#   "duratex_p1464_transecto_giro.laz",
#   "duratex_p1462_transecto_45.laz"
# )


# clip and save plot clouds
for(i in 1:length(files)){
# i=1
cat(paste('\n\n################\n\n\n', files[i], '\n\n\n################\n\n'))

# cloud = readLAS(files[i], XYZonly = T)
# plot(cloud)
normName = normName = str_c('normalized/', sub('\\.laz', '_n.laz', files[i]))

lasground = 
# str_c('docker run -v ', getwd(), ':/LAZ pdal/pdal:1.6 pdal ground -v 8 -i LAZ/', files[i],' -o LAZ/', normName)
paste('wine lasground -i', files[i] ,'-no_bulge -no_stddev -wilderness -odix _n -odir normalized -olaz -replace_z -v')

system(lasground)

# rm(cloud)
normCloud = readLAS(normName, XYZonly = T)
 
# plot(normCloud)
# axes3d(col='red')

# forPlot = clip.XY(cloud@data, 15, c(1,12))
# cloud = LAS(forPlot)
# file = sub('_norm.laz', '_plot.laz', file)
# writeLAS(cloud, paste('plot_clouds/', file,sep=''))

# process DBHs from all clouds

las2rings = paste('./las2rings -i', normName ,'-d 0.01 -l LOW -u UP -p 0.03 -r 0.2 -v 2 -o normalized/res_temp.txt -O normalized/laz_temp.laz')

centers = data.frame()
tempCloud = 'normalized/laz_temp.laz'
tempResult = 'normalized/res_temp.txt'
for(h in c(1,1.5,2,2.5,3)){
  
  las2temp = sub('LOW', h,las2rings)
  las2temp = sub('UP', h+1,las2temp)
  
  system(las2temp)

  tempTab = read.table(tempResult, header = T, as.is=T)
  
  centers = rbind(centers, tempTab)
  names(centers) = names(tempTab)
  
  unlink(c(tempCloud, tempResult))
}

inclusionRadius = .2
minSegs = 3
keepCenters = data.frame()

nTree = 1
while(nrow(centers) > 0){
  temp = centers[1,]
  
  xyDist = sqrt( (centers$x_average - temp$x_average)^2 + (centers$y_average - temp$y_average)^2 )
  
  tempCloud = as.data.frame(centers[ xyDist <= inclusionRadius ,])
  
  if(nrow(tempCloud) > minSegs){
    tempCloud$n_tree = nTree
    keepCenters = rbind(keepCenters, apply(tempCloud, 2, mean))
    names(keepCenters) = names(tempCloud)
    nTree = nTree + 1
  }
  
  centers = centers[ xyDist > inclusionRadius,]
}


minPoints = 300
minHeight = 4
neighborRadius = .75
treePositions = data.frame()
stemCloud = data.frame()
counter = 1
# for(i in 1:nrow(keepCenters)){
while(nrow(keepCenters) > 0){
  temp = keepCenters[1,]
  
  xyDist = sqrt( (temp$x_average - keepCenters$x_average)^2 + (temp$y_average - keepCenters$y_average)^2 )
  
  temp = keepCenters[ xyDist <= neighborRadius, ]
  temp = as.data.frame(t(apply(temp, 2, mean)))
  
  tempTree =  clip.XY(normCloud@data, rad = neighborRadius, center = c(temp$x_average, temp$y_average))
  writeLAS(LAS(tempTree), 'stems/tree.laz')

  system('./las2rings -i stems/tree.laz -o stems/table.txt -O stems/stem.laz -t -z 1 -p 0.025')
  
  stem = readLAS('stems/stem.laz', XYZonly = T)
  dTable = read.table('stems/table.txt', header = T)
  
  keepCenters = keepCenters[xyDist > neighborRadius,]
  unlink(c('stems/tree.laz , stems/table.txt , stems/stem.laz'))

  if(is.null(stem)) next
  
  if( (nrow(stem@data) > minPoints & diff(range(dTable$z_max)) > minHeight) ){
    # treePositions = rbind(treePositions, temp)
    stemCloud = rbind(stemCloud, stem@data)
    
    dTable$n = counter
    treePositions = rbind(treePositions, dTable)
    
    counter = counter+1
  }
    
  print(paste(nrow(keepCenters), 'and down'))
}

stemsCloudName = str_c('stems/', sub('\\.laz', '_stems.laz',files[i]))
stemsStatsName = str_c('stems/', sub('\\.laz', '_stems.txt',files[i]))

if(nrow(stemCloud) == 0) next

writeLAS(LAS(stemCloud), stemsCloudName)
write.table(treePositions, stemsStatsName)

}

test = dir('stems/', pattern = '\\.laz')
test = sub('_stems','',test)

cloud = readLAS('klabin_p5830_girando.laz', XYZonly = T)

# cloud2 = cloud@data
# cloud2[,2] = cloud@data[,2]

rgl.points(cloud@data, size=.5) ; axes3d(col='red')

# clear3d()
# rgl.points(stemCloud, col='green')
# rgl.points(normCloud@data, size=.5, col='white')
# with( treePositions, spheres3d(x_center, y_center, z_min, radius = radius, col='red') )

plotsDir = 'plot_clouds/'
stemPlots = dir(plotsDir, '_stems\\.laz')
stemCoordinates = dir(plotsDir, '_trees\\.txt')

hDbh  = c(1.1,1.5)
pixel = 0.025
minPoints = 5

par(mfrow=c(2,3))
for(i in 1:length(stemCoordinates)){

cloud  = readLAS(paste(plotsDir, stemPlots[i], sep=''))
coords = read.table(paste(plotsDir, stemCoordinates[i], sep=''), header = T)

dbhs = data.frame()
for(j in 1:nrow(coords)){
  
  print(paste('tree:', j, '; %:', j/nrow(coords) * 100))
  
  row = as.double(coords[j,])
  
  stem = as.data.frame( clip.XY( cloud@data, .75 , row[3:4] ) )[,1:3]
  
  if(nrow(stem) < minPoints) next
  
  dbhSegment = stem[ stem[,3] >= hDbh[1] & stem[,3] <= hDbh[2]  ,]

  if(nrow(dbhSegment) < minPoints) next
  
  dbhRaster = makeRaster(dbhSegment, pixel, image = F)
  dbhHough  = hough(dbhRaster, c(pixel, .2), pixel, .025)
  
  descOrd = order(dbhHough$centers[,'votes'], dbhHough$centers[,'radii'], decreasing = T)
  
  parHough = as.double(c(dbhHough$centers[descOrd[1],], 1))
  
  parRANSAC = lapply(1:5, function(x) RANSAC.olofsson(dbhSegment))
  parRANSAC = do.call(rbind, parRANSAC)
  if(!is.double(parRANSAC[[1]])) next
  parRANSAC = as.double(parRANSAC[ parRANSAC[,4] == min(parRANSAC[,4]) ,])
  
 if(parRANSAC[3] > parHough[3]+pixel){
   truePars = c(parHough, 0)
 }else{
   truePars = c(parRANSAC, 1)
 }
  
  dbhs = rbind(dbhs, truePars)
  names(dbhs) = c('x', 'y', 'r', 'opt', 'in', 'RANSAC')
  
}

threshold = qnorm(.99, mean(dbhs$r), sd(dbhs$r))

dbhs = dbhs[ dbhs$r < threshold & dbhs$r > 0,]

hist(dbhs$r)

write.table(dbhs, paste(plotsDir, sub('_trees\\.txt', '_dbh2.txt',stemCoordinates[i]), sep=''), col.names = T, row.names = F)

}


# compare to field data
field = read.csv('RemediçãoMonteAlegre.csv', sep=';', dec=',')
plotNms = unique(field$nm_parcela)
{
xmax = 35
maxden = 0
results = list()
for(pn in plotNms){
  
temp = list(name = pn)

fieldPlot = field[ field$nm_parcela == pn ,]

print( paste('n fustes:', length(which(fieldPlot$nm_fuste > 1)))  )

cloudFile = sub('__NP__', pn, 'plot_clouds/duratex_p__NP___transecto_plot_stems.laz')
cloud = readLAS(cloudFile)
# cloud@data = cloud@data[ with(cloud@data, X > -8 & X < 8 & Y > 0 & Y < 16) ,]
# plot(cloud, size=.5)

dbhFile = sub('__NP__', pn, 'plot_clouds/duratex_p__NP___transecto_plot_dbh2.txt')
dbhs = read.table(dbhFile, head=T)
dbhs = dbhs[ with(dbhs, x > -8 & x < 8 & y > 1 & y < 17) ,]
dbhs = dbhs[ dbhs$r < .1,]
# with(dbhs, spheres3d(x, y, 1.3, r, col='red'))

temp$nCloud  = nrow(dbhs)
temp$nManual = nrow(fieldPlot[ !is.na(fieldPlot$nm_d_c) ,])

dmax = ceiling(max(dbhs$r*200))
xmax = ifelse(xmax < dmax, dmax, xmax)

par(mfrow=c(1,2))
p1 = hist(fieldPlot$nm_d_c, breaks = seq(0,xmax,3)+3, freq = F)
p2 = hist(I(dbhs$r*200), breaks = seq(0,xmax,3)+3, freq = F)

temp$pManual = p1
temp$pCloud  = p2

c1 = with(fieldPlot, dnorm(0:(xmax*100)/100, mean(nm_d_c, na.rm = T), sd(nm_d_c, na.rm = T)))
c2 = with(dbhs, dnorm(0:(xmax*100)/100, mean(r*200), sd(r*200)))

temp$cManual = c1
temp$cCloud  = c2

tempden = max(c(p1$density, p2$density, c1, c2))
maxden = ifelse(maxden < tempden, tempden, maxden)

par(mfrow=c(1,1))
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,xmax), ylim = c(0,maxden), freq=F)  # first histogram
lines(y=c1 , x=0:(xmax*100)/100, col='blue')
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,xmax), freq=F, add=T)  # second
lines(y=c2 , x=0:(xmax*100)/100, col='red')

temp$mManual  = mean(fieldPlot$nm_d_c, na.rm = T)
temp$mCloud  = mean(dbhs$r*200, na.rm = T)

temp$sdManual = sd(fieldPlot$nm_d_c, na.rm = T)
temp$sdCloud = sd(dbhs$r*200, na.rm = T)

temp$cfManual = confint(lm(fieldPlot$nm_d_c~1))
temp$cfCloud = confint(lm(I(dbhs$r*200)~1))

temp$normManual = shapiro.test(fieldPlot$nm_d_c)
temp$normCloud  = shapiro.test(I(dbhs$r*200))

temp$tTest = t.test(fieldPlot$nm_d_c, I(dbhs$r*200), paired = F)
temp$uTest = wilcox.test(fieldPlot$nm_d_c, I(dbhs$r*200), paired = F, conf.level = .95, conf.int = T)

results[[as.character(pn)]] = temp
}


png(filename = 'densities2.png', width = 24, height = 16, units = 'cm', res = 300)
par(mfrow = c(2,3))
sapply(results, function(temp){
  
  plot( temp$pManual, col=rgb(0,0,1,1/4), xlim=c(0,xmax), ylim = c(0,maxden), freq=F, main=temp$name, ylab='', xlab='')  # first histogram
  lines(y=temp$cManual , x=seq(0, xmax, length.out = length(temp$cManual)), col='blue')
  
  plot( temp$pCloud, col=rgb(1,0,0,1/4), xlim=c(0,xmax), freq=F, add=T)  # second
  lines(y=temp$cCloud , x=seq(0, xmax, length.out = length(temp$cCloud)), col='red')
  
  text(x=17,y=.18, labels = paste('Field:', 
                                  round(temp$mManual, digits = 2),
                                  "±",
                                  round( diff(temp$cfManual[1,]) ,digits=2),
                                  "cm"
  ), pos = 4)
  
  text(x=17,y=.15, labels = paste('Cloud:', 
                                  round(temp$mCloud, digits = 2),
                                  "±",
                                  round( diff(temp$cfCloud[1,]) ,digits=2),
                                  "cm"
  ), pos = 4)
  
  text(x=17,y=.12, labels = paste('U test p-value:', 
                                  round(temp$uTest$p.value, digits = 4)
  ), pos = 4)
  
  
})

plot(0,cex=0, xlab='',ylab='', axes=F)
legend('center', c('Field Measured', 'Point Cloud Measured'), fill = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), cex=1.5)
title(ylab = 'Density', xlab = 'Diameter (cm)', outer = T, line = -1.7, cex.lab=2)
dev.off()
}
sapply(results, function(temp) temp$nManual)
