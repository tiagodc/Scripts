if(!require(lidR)){
  install.packages('lidR')
  require(lidR)
}
if(!require(magrittr)){
  install.packages('magrittr')
  require(magrittr)
}
if(!require(rgl)){
  install.packages('rgl')
  require(rgl)
}

anguloX = function(XYZplane, eixo='z', stat_mat=cov){
  e = eigen(stat_mat(XYZplane))
  #if(e$vectors[3,3] < 0) e$vectors = -e$vectors
  if(eixo != 'z') e$vectors[3,3] = 0

  vetor_eixo = if(eixo=='z') c(0,0,1) else if(eixo=='x') c(1,0,0) else c(0,1,0)
  ang = ( e$vectors[,3] %*% vetor_eixo ) / ( sqrt(sum(e$vectors[,3]^2)) * sqrt(sum(vetor_eixo^2)) )
  ang = ang[,,drop=T]
  degs = acos(ang)*180/pi
  return(degs)
}
rotationMatrix = function (ax, az, ax2){
  ax = ax * pi/180
  Rx = matrix(c(1, 0, 0, 0, cos(ax), sin(ax), 0, -sin(ax),
                cos(ax)), ncol = 3, byrow = T)
  az = az * pi/180
  Rz = matrix(c(cos(az), 0, -sin(az), 0, 1, 0, sin(az), 0,
                cos(az)), ncol = 3, byrow = T)
  ax2 = ax2 * pi/180
  Rx2 = matrix(c(cos(ax2), sin(ax2), 0, -sin(ax2), cos(ax2),
                 0, 0, 0, 1), ncol = 3, byrow = T)
  ro.mat = Rx2 %*% Rz %*% Rx
  return(ro.mat)
}
rotateCloud = function(file, lasDir=''){

  systring = ifelse(Sys.info()[['sysname']] == 'Linux', 'wine ', '')

  cmd = paste0(systring, lasDir, 'lasground.exe -i ', file,
               ' -odix _temp -olaz -no_bulge -no_stddev -wilderness')
  system(cmd)

  outCloud = readLAS(file)

  file = sub('\\.laz','_temp.laz',file)
  cloud = readLAS(file)
  unlink(file, force = T)

  chao = lasfilter(cloud, Classification == 2)
  cen = apply(chao@data[,1:2], 2, mean) %>% as.double
  chao_clip = lasclipCircle(chao, cen[1], cen[2], 10)

  az = anguloX(chao_clip@data[,1:3], 'z', cov)
  ax = anguloX(chao_clip@data[,1:3], 'x', cov)
  ay = anguloX(chao_clip@data[,1:3], 'y', cov)

  rz = ifelse(az > 90, 180-az, -az)
  rx = ifelse(ay < 90, -ax, ax)

  rot = rotationMatrix(0, rz, rx)

  cloud_norm = ( as.matrix(outCloud@data[,1:3]) %*% as.matrix(rot) ) %>% as.data.frame

  outCloud@data[,1:3] = cloud_norm
  outCloud = LAS(outCloud@data)

  return(
    list(
      cloud = lasfilter(outCloud, Classification != 2),
      matrix = as.matrix(rot)
    )
  )
}
correctCloud = function(cloud, mirrored_x=F, mirrored_y=F, upside_down=F, shift_z_axis=F){

  if(class(cloud) == 'LAS'){

    if(shift_z_axis){

      y = cloud@data$Y
      z = cloud@data$Z

      cloud@data$Z = y
      cloud@data$Y = z

    }

    if(upside_down){

      cloud@data$Z = -cloud@data$Z

    }

    if(mirrored_y){

      cloud@data$Y = -cloud@data$Y

    }
    
    if(mirrored_x){
      
      cloud@data$X = -cloud@data$X
      
    }
    
    cloud = LAS(cloud@data)

  }else{

    if(shift_z_axis){

      y = cloud[,3]
      z = cloud[,4]

      cloud[,4] = y
      cloud[,3] = z

    }

    if(upside_down){

      cloud[,4] = -cloud[,4]

    }

    if(mirrored_y){

      cloud[,3] = -cloud[,3]

    }
    
    if(mirrored_x){
      
      cloud[,2] = -cloud[,2]
      
    }

  }

  return(cloud)

}
rotateCloudInternal = function(cloud, keepGround = T){

  cloud = LAS(cloud@data[,1:5]) %>% lasground(csf(class_threshold = .2), F)

  chao = lasfilter(cloud, Classification == 2)
  cen = apply(chao@data[,1:2], 2, mean) %>% as.double
  chao_clip = lasclipCircle(chao, cen[1], cen[2], 10)

  az = anguloX(chao_clip@data[,1:3], 'z', cov)
  ax = anguloX(chao_clip@data[,1:3], 'x', cov)
  ay = anguloX(chao_clip@data[,1:3], 'y', cov)

  rz = ifelse(az > 90, 180-az, -az)
  rx = ifelse(ay < 90, -ax, ax)

  rot = rotationMatrix(0, rz, rx)

  cloud@data[,1:3] = ( as.matrix(cloud@data[,1:3]) %*% as.matrix(rot) ) %>% as.data.frame

  if(!keepGround) cloud %<>% lasfilter(Classification != 2)

  return(
    list(
      cloud = cloud,
      matrix = as.matrix(rot)
    )
  )
}
dtmNormalize = function(cloud, res=.5, keepGround=T, bufferFactor = 5){

  # make a raster that encompass the point cloud
  grid = cloud@data[,1:2] %>% apply(2,range) %>% as.double

  grid[c(1,3)] = grid[c(1,3)]-bufferFactor
  grid[c(2,4)] = grid[c(2,4)]+bufferFactor

  grid %<>% extent %>% raster
  res(grid) = res

  # Force to interpolate in these pixels
  dtm = grid_terrain(cloud, res = grid, algorithm = knnidw())

  cloud %<>% lasnormalize(dtm)

  if(!keepGround) cloud %<>% lasfilter(Classification != 2)

  return(cloud)

}
angle = function (a, b){
  prod = a %*% b
  lprod = sqrt(sum(a^2)) * sqrt(sum(b^2))
  ang = prod/lprod
  cang = acos(ang) * 180/pi
  return(cang[, , drop = T])
}
solid = function (height, zs, maxRad, b = 2){
  sol = sqrt(((height - zs)/height)^b)
  rads = sol * maxRad
  return(rads)
}
angleFilter = function(diams, maxAng=25){
  # maxAng = 30
  diams = diams[ order(diams$tree, diams$z_min) ,]
  diams$angles = NA
  for(i in unique(diams$tree)){
    temp = diams[diams$tree == i,]
    temp %<>% with(data.frame(x=x_ransac, y=y_ransac, z=(z_min + z_max)/2))
    zAngs = c()
    for(j in 2:nrow(temp)){
      vec = (temp[j,] - temp[j-1,]) %>% as.double
      ang = angle(vec, c(0,0,1))
      zAngs %<>% c(ang)
    }
    aAngs = c(zAngs[1], zAngs)
    dAngs = c(zAngs, rev(zAngs)[1])
    zAngs = (aAngs + dAngs)/2
    diams[diams$tree == i,'angles'] = zAngs
  }

  diams = diams[ diams$angles < maxAng ,]
  return(diams)
}
solidFilter = function(diams, maxRad=.25, tHeight=25, shapeExp=2){
  keepDiams = data.frame()
  for(i in unique(diams$tree)){
    temp = diams[diams$tree == i,]
    coneRads = solid(tHeight, (temp$z_min + temp$z_max)/2, maxRad, shapeExp)
    keep = (coneRads - temp$rad_ransac) > 0
    # diams = diams[ diams$tree != i ,]
    if(keep %>% which %>% length > 2){
      keepDiams %<>% rbind( temp[keep,] )
    }
  }
  return(keepDiams)
}
quantileFilter = function(diams, inf=.01, sup=.95){
  qts = quantile(diams$rad_ransac, c(inf,sup))
  diams = diams[ diams$rad_ransac > qts[1] & diams$rad_ransac < qts[2] ,]
  return(diams)
}
filterSegments = function(diams, stems, maxRad=.25, px=.025, dMin=1, hMin=3, nLim=1){

  diams %<>% solidFilter(maxRad)

  # remove segments not encopassing stem points
  # px = .025
  dsts = c()
  for(i in 1:nrow(diams)){
    # print(i)
    x = diams[i,'x_ransac',drop=T]
    y = diams[i,'y_ransac',drop=T]
    z = (diams$z_min[i] + diams$z_max[i])/2
    rad = diams$rad_ransac[i] + px
    dists = sqrt( (x-stems@data$X)^2 + (y-stems@data$Y)^2 + (z-stems@data$Z)^2 )
    npts = which(dists <= rad) %>% length
    dsts %<>% c(npts)
  }
  diams = diams[dsts > 3,]

  # remove isolated segments
  # dMin = 1
  coords = with(diams, data.frame(x_ransac, y_ransac, (z_max + z_min)/2))
  distMat = dist(coords) %>% as.matrix
  diag(distMat) = NA
  minVals = apply(distMat, 2, min, na.rm=T )
  isolated = which(minVals > dMin)
  diams = diams[-isolated,]

  # remove short objects
  # hMin = 3
  hIntervals = by(diams$z_max, diams$tree, function(x) diff(range(x)) )
  stumps = which(hIntervals < hMin)
  lowTrees = names(hIntervals)[stumps] %>% as.double
  diams = diams[!(diams$tree %in% lowTrees) ,]

  # remove baseless objects
  # nLim = 1
  nLow = by(diams$z_max, diams$tree, function(x) length(which(x < hMin)) )
  fakeTrees = which(nLow <= nLim)
  fakeTrees = names(nLow)[fakeTrees] %>% as.double
  diams = diams[!(diams$tree %in% fakeTrees),]

  diams %<>% angleFilter

  return(diams)
}
removeClones = function(diams, cloneRange=1){
  xTrees = by(diams$x_ransac, diams$tree, mean)
  yTrees = by(diams$y_ransac, diams$tree, mean)

  # cloneRange = 1
  treeIds = xTrees %>% names %>% as.double
  dists = dist(data.frame(xTrees %>% as.double, yTrees %>% as.double)) %>% as.matrix
  repTrees = apply(dists, 2, function(x){
    rows = which(x < cloneRange) %>% as.double
    return(treeIds[rows])
  }) %>% unique
  repTrees = repTrees[sapply(repTrees, length) > 1]

  stay = sapply(repTrees, function(i){
    temp = diams[diams$tree %in% i,]
    segs = by(temp$rad_ransac, temp$tree, length)
    keep = names(segs)[segs == max(segs)] %>% as.double
    keep = ifelse(length(keep) > 1, keep[1], keep)
    return(keep)
  })

  repTrees %<>% do.call(what = c)
  leave = repTrees[!(repTrees %in% stay)]

  diams = diams[!(diams$tree %in% leave),]

  return(diams)
}
mergeClones = function(diams, cloneRange=1){
  xTrees = by(diams$x_ransac, diams$tree, mean)
  yTrees = by(diams$y_ransac, diams$tree, mean)

  # cloneRange = 1
  treeIds = xTrees %>% names %>% as.double
  dists = dist(data.frame(xTrees %>% as.double, yTrees %>% as.double)) %>% as.matrix
  repTrees = apply(dists, 2, function(x){
    rows = which(x < cloneRange) %>% as.double
    return(treeIds[rows])
  }) %>% unique
  repTrees = repTrees[sapply(repTrees, length) > 1]

  for(i in repTrees){
    diams[ diams$tree %in% i , 'tree'] = i[1]
  }

  return(diams)
}
modelDiameters = function(diams){
  modDiams = data.frame()
  for(i in diams$tree %>% unique){
    temp = diams[ diams$tree == i ,]

    if(nrow(temp) < 3) next

    hs   = (temp$z_max + temp$z_min)/2
    hMax = max(hs)
    mod = lm(temp$rad_ransac ~ hs)

    if(mod$coefficients[2] > 0) next

    predHs = seq(.1, hMax, .1)
    newDiams = predict(mod, list(hs = predHs))
    x = temp$x_ransac %>% mean
    y = temp$y_ransac %>% mean
    df = data.frame(tree = i,x, y, h = predHs, d = newDiams*200, ang=mod$coefficients[2])
    modDiams %<>% rbind(df)
  }
  return(modDiams)
}
interpolateDiameters = function(diams, radMax = .25){
  splDiams = data.frame()
  for(i in diams$tree %>% unique){
    temp = diams[ diams$tree == i ,]

    if(nrow(temp) < 3) next

    hs = (temp$z_max + temp$z_min)/2
    hMax = round(max(hs), 1)
    n  = 1 + (hMax - .1) / .1

    spl = spline(hs, temp$rad_ransac, n, xmin = .1, xmax = hMax)
    x = spline(hs, temp$x_ransac, n, xmax=hMax)$y
    y = spline(hs, temp$y_ransac, n, xmax=hMax)$y

    keep = spl$y > 0 & spl$y < radMax

    d = spl$y[keep]
    h = spl$x[keep]
    x = x[keep]
    y = y[keep]

    df = data.frame(tree = i,x, y, h = h, d = d*200)

    splDiams %<>% rbind(df)
  }
  return(splDiams)
}
gridMax = function(z){
  return(list(ht = max(z)))
}
treeHeight = function(cloud,x,y,minZ,rad = 2.5){
  clip = lasclipCircle(cloud, x %>% as.double,y %>% as.double,rad)
  h = max(clip@data$Z) - minZ
  return(h)
}
cloudMeasures = function(diams, cloud, plotRad = 12.7, baseHeight=.3, hInt = .25, refD = 1.3){
  modDiams = modelDiameters(diams)
  splDiams = interpolateDiameters(diams)

  modDiams = modDiams[modDiams$h > baseHeight,]
  splDiams = splDiams[splDiams$h > baseHeight,]

  minZ  = min(cloud@data$Z)
  xy = with(diams, data.frame(
    tree = unique(tree),
    x = by(x_ransac, tree, mean) %>% as.double,
    y = by(y_ransac, tree, mean) %>% as.double
  ))

  xy$h = apply(xy, 1, function(x) treeHeight(cloud, x[2], x[3], minZ))

  modInfo = with(modDiams, list(
    tree = unique(tree),
    x = by(x, tree, mean),
    y = by(y, tree, mean),
    mind = by(d, tree, min),
    maxd = by(d, tree, max),
    minh = by(h, tree, min),
    maxh = by(h, tree, max),
    v = (.1 * pi*(d / 200)^2) %>% by(tree, sum),
    dbh  = by(d[h > refD-hInt & h < refD+hInt], tree[h > refD-hInt & h < refD+hInt], mean)
  )) %>% do.call(what = cbind) %>% as.data.frame
  modInfo  %<>% merge(xy[,c(1,4)], by = 'tree', all.x = T)

  splInfo = with(splDiams, list(
    tree = unique(tree),
    x = by(x, tree, mean),
    y = by(y, tree, mean),
    mind = by(d, tree, min),
    maxd = by(d, tree, max),
    minh = by(h, tree, min),
    maxh = by(h, tree, max),
    v = (.1 * pi*(d / 200)^2) %>% by(tree, sum)
  )) %>% do.call(what = cbind)
  dbh = with(splDiams, by(d[h > refD-hInt & h < refD+hInt], tree[h > refD-hInt & h < refD+hInt], mean))
  nms = dbh %>% names %>% as.double
  temp = data.frame(nms, dbh = dbh %>% as.double)
  splInfo %<>% merge(temp, by.x = 'tree', by.y = 'nms') %>%
    merge(xy[,c(1,4)], by = 'tree', all.x = T)

  z = (diams$z_max + diams$z_min)/2
  df = diams[ z > refD-hInt & z < refD+hInt , ]
  df = by(df$rad_ransac, df$tree, mean) * 200

  dEsts = list(df,splInfo$dbh,modInfo$dbh)

  means = sapply(dEsts, mean)
  sds   = sapply(dEsts, sd)
  ba    = sapply(dEsts, function(x) 10000 * sum(pi*(x/200)^2) / (pi*plotRad^2) )
  hd   = grid_metrics(cloud, gridMax(Z), 1)$ht %>% sort(decreasing = T) %>% head(5) %>% mean - minZ

  res   = data.frame(ba, means, sds, hd)

  rownames(res) = c('measured', 'interpolated', 'modelled')
  colnames(res) = c('ba', 'dbh', 'dbh_sd', 'hd')

  return(list(plot = res, interpolated = splInfo, modelled = modInfo))
}
gpsTimeFilter = function(cloud, to=NULL, from=NULL){

  # qt0 = min(cloud@data$gpstime)
  # qt1 = max(cloud@data$gpstime)

  if(!is.null(from)){
    qt0 = quantile(cloud@data$gpstime, from)
    cloud@data = cloud@data[ cloud@data$gpstime > qt0 ,]
  }

  if(!is.null(to)){
    qt1 = quantile(cloud@data$gpstime, to)
    cloud@data = cloud@data[ cloud@data$gpstime < qt1 ,]
  }

  return(LAS(cloud@data))

}
makeCircle = function(x, y, rad){
  angs = seq(0, 2*pi, length.out = 12)
  xcos = x + rad * cos(angs)
  ysin = y + rad * sin(angs)
  return(data.frame(x=xcos, y=ysin))
}
writePlotLayers = function(cloud, report, prefix='temp', reduce=F, treeId=NULL, timeCols = c('green', 'orange'), treeCol = 'brown', circleCol='blue', labelCol='black'){

  if(!is.null(treeId)){
    cloud %<>% lasfilter(UserData == treeId)
    report = report[report$tree == treeId,]
  }

  hvals = report$h_min %>% unique %>% sort
  hint = hvals[2] - hvals[1]

  xlim = (cloud@data$X %>% range)*1.1
  ylim = (cloud@data$Y %>% range)*1.1

  if(reduce) cloud %<>% lasfilterdecimate(random(500))

  for(i in 2:length(hvals)){
    # i = 5
    paste('layer', i-1, 'of', length(hvals)-1) %>% print

    layer = lasfilter(cloud, Z >= hvals[i-1] & Z < hvals[i])
    stemPts = lasfilter(layer, Classification == 30)
    otherPts = lasfilter(layer, Classification != 30)

    diams = report[report$h_max == hvals[i],]

    colRamp = colorRampPalette(timeCols)
    unTimes = cloud@data$gpstime %>% unique %>% sort %>% as.character
    unCols = colRamp(unTimes %>% length)
    names(unCols) = unTimes

    fileName = paste(prefix, hvals[i-1], hvals[i], sep='_') %>% paste0('.svg')

    {
      # png('temp.png', 100, 100, 'cm', res=500)
      # pdf('temp.pdf', width = 50, height = 40)
      svg(fileName, width = 50, height = 40)

      layout(matrix(c(rep(1,9),2,3,4), nrow = 3, byrow = F))
      par(mar=c(25,20,10,10))

      plot(otherPts@data[,1:2], xlim=xlim, ylim=ylim, cex=.02, pch=20, asp=1, cex.lab=10, main=paste(hvals[i-1], 'a', hvals[i], 'm acima do solo'), cex.main=10, cex.axis=8,
           col=ifelse(otherPts@data$Classification == 20, treeCol, 'black'), xlab='x (m)', ylab='y (m)', mgp=c(12,5,0) )

      points(stemPts@data$X, stemPts@data$Y, cex=.05, pch=20,
             col=unCols[ stemPts@data$gpstime %>% as.character ])

      apply(diams[,c('x','y', 'rad')], 1, function(x){
        makeCircle(x[1], x[2], x[3]) %>% lines(col=circleCol, lwd=1.5)
      })

      lineSpace = .05
      xlines = seq(xlim[1], xlim[2]+lineSpace, lineSpace)
      ylines = seq(ylim[1], ylim[2]+lineSpace, lineSpace)

      abline(v=xlines, lty=1, lwd=.25, col='lightgrey')
      abline(h=ylines, lty=1, lwd=.25, col='lightgrey')

      lineSpace = .1
      xlines = seq(xlim[1], xlim[2]+lineSpace, lineSpace)
      ylines = seq(ylim[1], ylim[2]+lineSpace, lineSpace)

      abline(v=xlines, lty=3, lwd=.5, col='red')
      abline(h=ylines, lty=3, lwd=.5, col='red')

      labs = paste0(diams$tree, '\n', round(diams$rad*200, 2))
      graphics::text(x=diams$x, y=diams$y, labels=labs, cex=.66, col=labelCol)

      main = paste('Dm =', round(mean(diams$rad*200), 2), 'cm')
      hist(diams$rad*200, col=rgb(.2,.2,1,.6), main=main, freq=T, xlab='D (cm)', ylab='n', cex.main=7, cex.axis=5, cex.lab=7, mgp=c(12,5,0))
      abline(v = mean(diams$rad*200), col='black', lty=2, lwd=10)

      plot(0,cex=0,axes=F, xlab='', ylab='')
      plot(0,cex=0,axes=F, xlab='', ylab='')
      legend('center', pch = c(rep(20,4), 8, 21, rep(NA,2)),
             lty=c(rep(NA, 6),3,1), lwd=5, cex=5, bty='n',
             col = c(timeCols, treeCol, 'black', labelCol, circleCol, 'red', 'lightgray'),
             # col = c('green', 'orange', 'brown', 'black', 'black', 'blue', 'red', 'lightgray'),
             legend = c('pontos de tronco - in�cio',
                        'pontos de tronco - fim',
                        'pontos da �rvore',
                        'outros',
                        'ID da �rvore / di�metro (cm)',
                        'se��o de tronco',
                        'marcadores de 10 cm',
                        'marcadores de 5 cm'))
      box()

      dev.off()
    }

  }
}
checkLayer3d = function(cloud, report, h_min=0, newPlot=F, reduce=F, treeId=NULL, timeCols = c('green', 'orange'), treeCol='brown', circleCol='blue', labelCol='white'){

  if(reduce) cloud %<>% lasfilterdecimate(random(500))
  if(!is.null(treeId)) cloud %<>% lasfilter(UserData == treeId)

  diams = report[report$h_min == h_min,]
  if(!is.null(treeId)) diams = diams[ diams$tree == treeId, ]
  h_max = diams$h_max[1]
  layer = lasfilter(cloud, Z >= h_min & Z < h_max)

  colRamp = colorRampPalette(timeCols)
  unTimes = cloud@data$gpstime %>% unique %>% sort %>% as.character
  unCols = colRamp(unTimes %>% length)
  names(unCols) = unTimes

  stemPts = lasfilter(layer, Classification == 30)
  treePts = lasfilter(layer, Classification == 20)
  otherPts = lasfilter(layer, Classification != 30 & Classification != 20)

  if(newPlot) rgl.open()
  clear3d() ; bg3d('black')

  rgl.points(stemPts@data[,1:3], color=unCols[ stemPts@data$gpstime %>% as.character ], size=1.5)
  rgl.points(treePts@data[,1:3], color=treeCol, size=1.5)
  rgl.points(otherPts@data[,1:3], color='white', size=.5)

  spheres3d(diams$x, diams$y, (diams$h_min + diams$h_max)/2, diams$rad, col=circleCol)

  txt = paste('id', diams$tree, '-', round(diams$rad*200, 2), 'cm')
  text3d(diams$x, diams$y, diams$h_max, txt, color=labelCol, size=.5)

  axes3d(col='white')

}
checkTree3d = function(cloud, report, treeId, newPlot=F, trunkOnly=F, timeCols = c('green', 'orange'), treeCol='brown', circleCol='blue', labelCol='white'){

  cloud %<>% lasfilter(UserData %in% treeId)
  report = report[ report$tree %in% treeId ,]

  if(newPlot) rgl.open()

  bg3d('black')

  stemPts = lasfilter(cloud, Classification == 30 )
  treePts = lasfilter(cloud, Classification == 20 )
  otherPts = lasfilter(cloud, Classification != 30 & Classification != 20 )

  colRamp = colorRampPalette(timeCols)
  unTimes = cloud@data$gpstime %>% unique %>% sort %>% as.character
  unCols = colRamp(unTimes %>% length)
  names(unCols) = unTimes

  rgl.points(stemPts@data[,1:3], size=1.5, color=unCols[ stemPts@data$gpstime %>% as.character ])

  if(!trunkOnly){
    rgl.points(treePts@data[,1:3], size=1.5, color=treeCol)
    rgl.points(otherPts@data[,1:3], size=.5, color='white')
  }


  spheres3d(report$x, report$y, (report$h_min + report$h_max)/2, report$rad, color=circleCol)
  txt = paste(round(report$rad*200,2), 'cm')
  text3d(report$x, report$y, report$h_min, txt, color=labelCol)

  axes3d(col='white')
}
treePlot3d = function(cloud, report){
  rgl.open(); bg3d('black') ; clear3d()
  spheres3d(report$x, report$y, (report$h_min + report$h_max)/2, report$rad, color='orange')
  rgl.points(cloud@data[cloud@data$Classification == 30,], color = "darkred", size=1)
  rgl.points(cloud@data[cloud@data$Classification == 20,], color = "darkgreen", size=.5)
  rgl.points(cloud@data[cloud@data$Classification == 2,], color = "brown", size=.5)
  rgl.points(cloud@data[cloud@data$Classification == 1,], color = "darkgray", size=.5)
}
redirectCloud = function(cloud, slam){
  startLim = floor(nrow(slam) / 3)
  pathStart = slam[1:startLim,]

  ax = anguloX(pathStart[,2:4], 'x')
  ay = anguloX(pathStart[,2:4], 'y')

  rx = ifelse(ay < 90, -ax, ax)

  rot = rotationMatrix(0, 0, rx)

  xyz  = ( as.matrix(cloud@data[,1:3]) %*% as.matrix(rot) ) %>% as.data.frame
  slam[,2:4] = ( as.matrix(slam[,2:4]) %*% as.matrix(rot) ) %>% as.data.frame

  cloud@data[,1:3] = xyz
  cloud = LAS(cloud@data)

  return(
    list(
      cloud = cloud,
      slam = slam
    )
  )
}

importResults = function(lasName, maxRad=.15, minPts=20){
  repName = sub('\\.laz$', '_results.txt',lasName)

  las = readLAS(lasName)
  rep = read.table(repName, header = T)

  hMin = min(rep$h_min)
  rep$h_min = rep$h_min - hMin
  rep$h_max = rep$h_max - hMin

  las@data$Z = las@data$Z - hMin

  las = LAS(las@data)
  rep = rep[ rep$rad < maxRad & rep$h_max > 0 & rep$n > minPts ,]

  return(list(las=las, report=rep))

}

getHeigths = function(las, treeRadius=1.5, graph=T){

  ids = las@data$UserData %>% unique
  ids = ids[ids != 0]

  hs = sapply(ids, function(i){
    tree = lasfilter(las, UserData == i)
    x = mean(tree@data$X)
    y = mean(tree@data$Y)
    tree = lasclipCircle(las, x, y, treeRadius)
    return(max(tree@data$Z))
  })

  if(graph){
    hist(hs, main='Tree Height', xlab='h (m)', ylab='n')
    abline(v=mean(hs), lty=2, lwd=3)
  }

  df = data.frame(id=ids, h=hs)

  return(df)

}

getDbhs = function(rep, hRange=c(1,1.6), graph=T){
  layer = rep[ rep$h_min >= hRange[1] & rep$h_max <= hRange[2] ,]
  rads = tapply(layer$rad, layer$tree, mean)
  df = data.frame(id = rads %>% names %>% as.double, dbh = rads*200)
  row.names(df) = NULL

  if(graph){
    hist(df$dbh, main='Tree DBH', xlab='DBH (cm)', ylab='n')
    abline(v=mean(df$dbh), lwd=3, lty=2)
  }

  return(df)
}

plotDiams = function(las, rep, hRange=c(1,1.6), timeCols=c('green','orange'), gridRes=.025, export=T, pref='temp'){
  ids = las@data$UserData %>% unique
  ids = ids[ids != 0]
  colRamp = colorRampPalette(timeCols)
  angs = seq(0,pi*2, length.out = 12)

  for(i in ids){

    paste('tree', i) %>% print

    # i=ids[1]
    cld = lasfilter(las, UserData == i & Z >= hRange[1] & Z <= hRange[2])
    seg = rep[ rep$h_min >= hRange[1] & rep$h_max <= hRange[2] & rep$tree == i ,]

    fileName = paste0(pref, '_', i, '.png')

    if(export) png(fileName, 15, 15, units = 'cm', res = 300)

    plot(cld@data[ cld@data$Classification != 30 ,1:2], pch=20, cex=.75, asp=1,
         main=paste0('id ', i, '\n',hRange[1], ' - ', hRange[2], ' m'))

    trk = cld@data[ cld@data$Classification == 30 , ]
    unTimes = trk$gpstime %>% unique %>% sort %>% as.character
    unCols = colRamp(unTimes %>% length)
    names(unCols) = unTimes

    points(trk$X,
           trk$Y,
           col=unCols[trk$gpstime %>% as.character],
           pch=20, cex=.75
    )

    mx = seg$x %>% mean
    my = seg$y %>% mean
    mr = mean(trk$PointSourceID) / 1000
    cx = cos(angs) * mr + mx
    cy = sin(angs) * mr + my
    lines(cx, cy, lwd=2, col='red')

    apply(seg, 1, function(x){
      points(x['x'], x['y'], col='blue', pch=3, cex=2)

      cx = cos(angs) * x['rad'] + x['x']
      cy = sin(angs) * x['rad'] + x['y']

      lines(cx, cy, lwd=2, col='blue')

    })

    vrg = range(cld@data$X) %>% round(1)
    vrg = seq(vrg[1], vrg[2], gridRes)
    abline(v=vrg, lty=2, lwd=.5, col='red')

    hrg = range(cld@data$Y) %>% round(1)
    hrg = seq(hrg[1], hrg[2], gridRes)
    abline(h=hrg, lty=2, lwd=.5, col='red')

    if(export) dev.off()
  }
}

clipPoints = function(las, x=NULL, y=NULL, rad=.15, keepInner=T, ...){

  if(is.null(x)){
    dst = rep(T, nrow(las@data))
  }  else{
    dst = sqrt( (las@data$X-x)^2 + (las@data$Y-y)^2 ) < rad
    if(!keepInner) dst = !dst
  }

  las@data = las@data[dst,]
  las = LAS(las@data)

  plot(las@data[,1:2], asp=1, ...)

  pars = TreeLS::RANSAC.circle(las@data[,1:3])

  angs = seq(0,pi*2, length.out = 12)
  cx = cos(angs) * pars[3] + pars[1]
  cy = sin(angs) * pars[3] + pars[2]

  points(pars[1], pars[2], pch=3, cex=2, col='red')
  lines(cx, cy, lwd=2, col='red')

  names(pars) = c('x', 'y', 'rad', 'err')

  return(list(las = las, pars=pars))
}

