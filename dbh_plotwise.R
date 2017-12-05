require(TreeLS)
require(lidR)
require(stringr)

setwd('~/ProLiDAR/Pilotos/TLS/LAZ/')

# new RANSAC circle estimation
RANSAC.olofsson = function(stem.sec, n=3, p=.8, P=.99, timesN=50, inner_circle = 0.015){
  
  slc = stem.sec #= dbhSegment
  
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

files = dir('eldorado', pattern = '*.\\.laz', full.names = T)

normalize = F
# clip and save plot clouds
for(i in 1:length(files)){
# i=1
cat(paste('\n\n################\n\n\n', files[i], '\n\n\n################\n\n'))

  if(normalize){
    # cloud = readLAS(files[i], XYZonly = T)
    # plot(cloud)
    normName = normName = str_c('normalized/', sub('\\.laz', '_n.laz', files[i]))
    
    lasground = 
    # str_c('docker run -v ', getwd(), ':/LAZ pdal/pdal:1.6 pdal ground -v 8 -i LAZ/', files[i],' -o LAZ/', normName)
    paste('wine lasground -i', files[i] ,'-no_bulge -no_stddev -wilderness -odix _n -odir normalized -olaz -replace_z -v')
    
    system(lasground)
  }else{
    normName = files[i] 
  }
# rm(cloud)
normCloud = readLAS(normName, XYZonly = T)
 
# process DBHs from all clouds

las2rings = paste('./las2rings -i', normName ,'-d 0.01 -l LOW -u UP -p 0.03 -r 0.2 -v 2 -o normalized/res_temp.txt -O normalized/laz_temp.laz')

centers = data.frame()
tempCloud = 'normalized/laz_temp.laz'
tempResult = 'normalized/res_temp.txt'
for(h in c(1,1.5,2)){
  
  las2temp = sub('LOW', h,las2rings)
  las2temp = sub('UP', h+1,las2temp)
  
  system(las2temp)

  tempTab = read.table(tempResult, header = T, as.is=T)
  
  centers = rbind(centers, tempTab)
  names(centers) = names(tempTab)
  
  unlink(c(tempCloud, tempResult))
}

inclusionRadius = .2
minSegs = 2
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


minPoints = 30
minHeight = 2
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

#######################################################################

files = dir('stems', pattern = '\\.laz', full.names = T)
txtFiles = sub('\\.laz', '.txt', files)

# dbh measurements
for(i in 1:length(files)){
# i = 1
cloud = readLAS(files[i], XYZonly = T)
info  = read.table(txtFiles[i], header = T) 

X = as.double(by(info$x_center, as.factor(info$n), mean))
Y = as.double(by(info$y_center, as.factor(info$n), mean))

hDbh  = min(cloud@data$Z) + c(1.1,1.5)
pixel = 0.025
minPoints = 5
dbhs = data.frame()

for(nt in 1:length(X)){
  print( paste('plot:', i, 'of', length(files),'; tree:', nt, 'of', length(X)) )
  
  stem = as.data.frame(clip.XY(cloud@data, 1, c(X[nt], Y[nt])))

  if(nrow(stem) < minPoints) next
  
  dbhSegment = stem[ stem[,3] >= hDbh[1] & stem[,3] <= hDbh[2]  ,]

  if(nrow(dbhSegment) < minPoints) next
  
  dbhRaster = makeRaster(dbhSegment, pixel, image = F)
  dbhHough  = hough(dbhRaster, c(pixel, .2), pixel, .025)
  
  colnames(dbhHough$centers) = c('x', 'y', 'votes', 'radii')
  
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
# hist(dbhs$r)

nomFile = sub('\\.laz', '.txt', files[i])
nomFile = sub('stems/', 'dbh/', nomFile)
write.table(dbhs, nomFile, col.names = T, row.names = F)

}

# stem model measurements
for(i in 43:length(files)){
  # i = 1
  cloud = readLAS(files[i], XYZonly = T)
  info  = read.table(txtFiles[i], header = T) 
  
  X = as.double(by(info$x_center, as.factor(info$n), mean))
  Y = as.double(by(info$y_center, as.factor(info$n), mean))
  treeNumbers = unique(info$n)
  
  minPoints = 5
  measures = data.frame()
  
  for(j in treeNumbers){
    
    print( paste('plot:', i, 'of', length(files),'; tree:', j, 'of', length(X)) )
    
    stem = as.data.frame(clip.XY(cloud@data, 1, c(X[j], Y[j])))
    stemHough = info[info$n == j,]
    
    if(nrow(stem) < minPoints) next
    
    parRANSAC = apply(stemHough, 1, function(x){
      sgmt = stem[ stem$Z > x['z_min'] & stem$Z < x['z_max'] ,]
      cPars = RANSAC.olofsson(sgmt, timesN = 100)
      if(!is.null(cPars)) cPars = c(cPars, x['z_min'], x['z_max'])
      return(cPars)
    })
    
    if(is.null(parRANSAC)) next
    
    parRANSAC = if(class(parRANSAC) == 'list') do.call(rbind, parRANSAC) else as.data.frame(t(parRANSAC))
    merged = merge(stemHough, parRANSAC, by = c('z_min', 'z_max'), all = T)
  
    measures = rbind(measures, merged)
  }
  
  nomFile = sub('\\.laz', '.txt', files[i])
  nomFile = sub('stems/', 'stem_model/', nomFile)
  write.table(measures, nomFile, col.names = T, row.names = F)
}

#######################################################################

radiiFilter = function(table, radiiCol = 'r', maxRad = 0.1, conf=.975){

  table = table[ table[,radiiCol] < maxRad , ]
    
  if(!is.null(conf)){
    # CI = confint(lm(table[,radiiCol]~1), level=conf)
    # CI = quantile(table[,radiiCol], c(1-conf, conf))
    CI = qnorm(c(1-conf, conf), mean(table[,radiiCol]), sd(table[,radiiCol]))
    
    table = table[ table[,radiiCol] > CI[1] & table[,radiiCol] < CI[2] ,]
  }

  return(table)
}
dbhMap = function(X, Y, radii, ...){
  angs = pi * 1:360 / 180
  
  circles = lapply(1:length(radii), function(i){
    r = radii[i]
    perX = cos(angs) * r
    perY = sin(angs) * r
    
    coords = data.frame(X = perX + X[i], Y = perY + Y[i])
    return(coords)
  })
  
  boundBox = apply(do.call(rbind, circles), 2, range)
  
  plot(Y~X, data=boundBox, cex=0, asp=1, ...)
  
  plt = lapply(circles, function(i){
    lines(i$X, i$Y, lwd=1, lty=1)
  })
  
}
dbhCurve = function(cloudDiam, fieldDiam, ...){
  
  cloudDiam = cloudDiam[!is.na(cloudDiam)]
  fieldDiam = fieldDiam[!is.na(fieldDiam)]
  
  # cm = radii*200
  maxD  = max(cloudDiam)
  # minD  = min(cloudDiam)
  
  maxFD = max(fieldDiam)
  # minFD = min(fieldDiam)
  # cm = cm[cm < maxD]
  
  c1 = dnorm(0:(maxD*100)/100, mean(cloudDiam, na.rm = T), sd(cloudDiam, na.rm = T))
  c2 = dnorm(0:(maxFD*100)/100, mean(fieldDiam, na.rm = T), sd(fieldDiam, na.rm = T))
  
  a = hist(cloudDiam, breaks = seq(0,maxD+1,1), freq = F, 
       col=rgb(0,0,1,1/4), 
       ylim = c(0, 1.2*max(c(c1,c2))), xlim=c(0, 1.2*max(c(cloudDiam, fieldDiam))),
       ylab="densidade", xlab="DAP (cm)"
       , ...
       )
  lines(y=c1 , x=0:(maxD*100)/100, col='blue')
  
  b = hist(fieldDiam, breaks = seq(0,maxFD+1,1), freq = F, 
           col=rgb(1,0,0,1/4), add=T)
  lines(y=c2, x=0:(maxFD*100)/100, col='red')
  
  avg  = round(mean(cloudDiam), 2)
  CI   = round(avg - confint(lm(cloudDiam~1))[1], 2)
  
  fAvg = round(mean(fieldDiam), 2)
  fCI  =round(fAvg - confint(lm(fieldDiam~1))[1], 2)
    
  texto  = paste('n =', length(cloudDiam), 'DAP =', avg , '±', CI, 'cm')
  texto2 = paste('n =', length(fieldDiam), 'DAP =', fAvg , '±', fCI, 'cm')
  
  # par(xpd=T)
  # text(1, quantile(a$density, .9), pos = 4, labels = texto)

  legend('topleft', 
         fill = c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)),
         border = c('blue', 'red'),
         legend = c(paste('nuvem:', texto), paste('campo:', texto2))
        )
  
}

# dbh plot
files = dir('dbh', pattern = '\\.txt', full.names = T)
lazFiles = sub('dbh/', 'normalized/', files)
lazFiles = sub('stems.txt', 'n.laz', lazFiles)
parcelas = unique(as.double(sub('.*_p([0-9]+)_.*', '\\1', files)))
campo = read.csv('campo/RemediçãoMonteAlegre.csv', sep=';', dec=',')
dMax = 20
for(i in 1:length(files)){
  file = read.table(files[i], header = T)
  
  mainName = sub('dbh/(.+)_stems\\.txt', '\\1', files[i])
  file = radiiFilter(file, maxRad = dMax/200, conf = .95)
  
  pn = as.double(sub('.*_p([0-9]+)_.*', '\\1', files[i]))
  tempCampo = campo[ campo$nm_parcela == pn,]
  
  if(nrow(tempCampo) == 0) next
  
  png(str_c('pics/dbh/',mainName,'.png'), 20,15, units = 'cm', res = 200)
  # dbhMap(file$x, file$y, file$r, main=mainName)
  dbhCurve(file$r * 200, tempCampo$nm_d_c, main=mainName)
  dev.off()
  
  # cloud = readLAS(lazFiles[i], XYZonly = T)
  # clear3d()
  # bg3d('black')
  # rgl.points(cloud@data, size=.5)
  # spheres3d(file$x, file$y, 1.3, file$r, col='green')
  # spheres3d(file$x, file$y, 1.3, file$r*5, col='red', alpha=.6)
}

# stem model
files = dir('stem_model', pattern = '\\.txt', full.names = T)
lazFiles = sub('stem_model/', 'normalized/', files)
lazFiles = sub('stems.txt', 'n.laz', lazFiles)

px = .025
rMax = .125
for(i in 1:length(files)){
file = read.table(files[i], header = T)

keepRansac = file$rad > (file$radius + px)
rad = ifelse(keepRansac, file$rad, file$radius)
x   = ifelse(keepRansac, file$x, file$x_center)
y   = ifelse(keepRansac, file$y, file$y_center)
h   = (file$z_min + file$z_max) / 2
taper = solid(20, h, rMax, 3)
rad[rad > taper] =  NA

df = data.frame(x,y,rad,h,taper, file$n)
df = df[!is.na(rad),]

df$class = cut(df$h, seq(0,max(df$h)+3, 3))
df = split(df, df$file.n)

firstHeight = 3
lastHeight = 20
df = lapply(df, function(x){
  # nr = nrow(x)
  fh = min(x$h)
  if(fh <= firstHeight) return(x[x$h < lastHeight,])
})

df = do.call(rbind, df)
nTrees = length(unique(df$file.n))

mainName = sub('stem_model/(.+)_stems\\.txt', '\\1', files[i])
png(str_c('pics/model/',mainName,'.png'), 30,15, units = 'cm', res = 200)
par(mfrow=c(1,2), mar=rep(3,4), oma=rep(1,4), font.lab=2)
plot(I(df$rad*200) ~ df$class, xlab='', ylab='')
barplot(table(df$class))
title(ylab = str_c('n (', nTrees, ' trees)'), line=2, font=2, xpd=T)
title(main = mainName, xlab = 'H (m)', ylab = 'D (cm)', outer = T, line = -.5, font=2)
dev.off()

# cloud = readLAS(lazFiles[i], XYZonly = T)
# clear3d()
# bg3d('black')
# rgl.points(cloud@data, size=.5)
# spheres3d(df$x, df$y, df$h, df$rad, col='green')
# spheres3d(df$x, df$y, 1.3, df$rad*5, col='red', alpha=.6)
}

#######################################################################

files = dir('stem_model', pattern = '\\.txt', full.names = T)


# compare to field data

{
xmax = 35
maxden = 0
results = list()
for(f in files){
  pn = as.double(sub('.*_p([0-9]+)_.*', '\\1', f))
  temp = campo[ campo$nm_parcela == pn ,]

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
