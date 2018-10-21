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
  
  return(
    list(
      cloud = lasfilter(outCloud, Classification != 2),
      matrix = as.matrix(rot)
    )
  )
}
correctCloud = function(cloud, mirrored=F, upside_down=F, shift_z_axis=F){
  
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
    
    if(mirrored){
      
      cloud@data$Y = -cloud@data$Y
      
    }
    
  }else{
    
    if(shift_z_axis){
      
      y = cloud[,2]
      z = cloud[,3]
      
      cloud[,3] = y
      cloud[,2] = z
      
    }  
    
    if(upside_down){
      
      cloud[,3] = -cloud[,3]
      
    }
    
    if(mirrored){
      
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
dtmNormalize = function(cloud, res=.5, keepGround=T){
  
  # make a raster that encompass the point cloud
  grid = cloud@data[,1:2] %>% apply(2,range) %>% as.double %>% extent %>% raster
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