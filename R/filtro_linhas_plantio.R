# Detecção automática de linhas de plantio
# Desenvolvedor: Tiago de Conto
# Empresa: Forlidar Consultoria Agroflorestal LTDA
# Disclaimer: o algoritmo e código fonte neste script são propriedade intelectual do desenvolvedor, NÃO sendo autorizada sua utilização ou distribução para fins comerciais sem autorização do mesmo.

##Vector processing=group
##Pontos_das_plantas=vector
##Linha_guia=vector
##Contornos_dos_talhoes=vector
##Espaco_entre_linhas=number
##Espaco_entre_plantas=number
##Tamanho_do_tile=number 50
##Distancia_maxima_da_linha_de_plantio=number 0.3
##Linhas=output vector
##Plantas=output vector

# setwd('C://Work/Spectrum/')

{
  if(!require(raster)){
    install.packages('raster')
    require(raster)
  }
  if(!require(sp)){
    install.packages('sp')
    require(sp)
  }
  if(!require(sf)){
    install.packages('sf')
    require(sf)
  }
  if(!require(magrittr)){
    install.packages('magrittr')
    require(magrittr)
  }
  if(!require(doParallel)){
    install.packages('doParallel')
    require(doParallel)
  }
  if(!require(foreach)){
    install.packages('foreach')
    require(foreach)
  }
  if(!require(parallel)){
    install.packages('parallel')
    require(parallel)
  }
}

nCores = parallel::detectCores() - 1
# if(Nucleos > 0 && Nucleos <= nCores) nCores = Nucleos

pts = st_as_sf(Pontos_das_plantas)
refLine = st_as_sf(Linha_guia)
stands = st_as_sf(Contornos_dos_talhoes)
tileSize = Tamanho_do_tile
spcLin = Espaco_entre_linhas
spcPla = Espaco_entre_plantas
tol = spcLin/2
tol2 = Distancia_maxima_da_linha_de_plantio

# pts = read_sf('teste/plantas_t2.shp')
# refLine = read_sf('teste/ref_t2.shp')
# stands = read_sf('teste/Faz_Pereira.shp')
# tileSize = 50
# spcLin = 3
# spcPla = 2
# tol = spcLin/2
# tol2 = .2

{
  classPts   = (pts %>% st_geometry %>% class)[1]
  classLine  = (refLine %>% st_geometry %>% class)[1]
  classStand = (stands %>% st_geometry %>% class)[1]

  if(classPts != 'sfc_POINT'){
    msg = '"Pontos" precisa ser camada vetorial do tipo ponto.'
    winDialog('ok', msg)
    stop(msg)
  }
  if(classLine != 'sfc_LINESTRING'){
    msg = '"Linha_mestre" precisa ser camada vetorial do tipo linha.'
    winDialog('ok', msg)
    stop(msg)
  }
  if(classStand != 'sfc_MULTIPOLYGON' && classStand != 'sfc_POLYGON'){
    msg = '"Contornos" precisa ser camada vetorial do tipo polígono.'
    winDialog('ok', msg)
    stop(msg)
  }
  if(!(st_crs(pts) == st_crs(refLine) && st_crs(refLine) == st_crs(stands))){
    msg = 'Sistema de coordenadas difere entre as camadas de entrada.'
    winDialog('ok', msg)
    stop(msg)
  }
}

pointPerStand = st_intersects(pts, stands, sparse = F)

xy = refLine %>% st_coordinates
xy = xy[,1:2]

reta = lm(xy[,2] ~ xy[,1])

ang = reta$coefficients[2] %>% as.double %>% atan

rMat = matrix(c(cos(ang) , sin(ang), -sin(ang), cos(ang)), byrow = T, ncol=2)
horizontalPts = (rMat %*% ((pts %>% st_coordinates)[,1:2] %>% t)) %>% t

{
  # cl = makePSOCKcluster(nCores)
  # registerDoParallel(cl)

  lineCols = foreach(i = 1:ncol(pointPerStand), .combine = 'cbind', .packages = c('magrittr')) %dopar% {

    multiLine = rep(NA, nrow(horizontalPts))
    lineNumber = 0
    
    inStand = pointPerStand[,i]
    
    if(!all(inStand)) return(NA)
    
    lims = apply(horizontalPts[ inStand ,], 2, range)
    xBreaks = seq(lims[1,1] - tileSize, lims[2,1] + tileSize, tileSize)
    yBreaks = seq(lims[1,2] - tileSize, lims[2,2] + tileSize, tileSize)
    pairBreaks = expand.grid(xBreaks, yBreaks)
    pairBreaks = pairBreaks[ order(pairBreaks[,2], pairBreaks[,1]) ,]

    for(j in 1:nrow(pairBreaks)){

     print(paste('stand',i, 'of', ncol(pointPerStand), '; tile', j, 'of', nrow(pairBreaks)))

     pair = pairBreaks[j,] %>% as.double
     x1 = pair[1]
     x2 = x1 + tileSize
     y1 = pair[2]
     y2 = y1 + tileSize
     inTile = inStand &
       horizontalPts[,1] > x1 & horizontalPts[,1] <= x2 &
       horizontalPts[,2] > y1 & horizontalPts[,2] <= y2

     donePts = c()
     includePts = c()

      while((inTile %>% which %>% length) > (donePts %>% length)){

        unsurveyed = !(1:nrow(horizontalPts) %in% donePts)
        temp = horizontalPts[unsurveyed & inTile,]
        if(!(temp %>% is.matrix)) temp %<>% t
        ptOrd = temp[,1] %>% order(decreasing = F)
        startPt = temp[ptOrd[1],] %>% as.double

        lowerLim = startPt - tol
        upperLim = startPt + tol

        bounded = inTile & unsurveyed &
          (horizontalPts[,2] > lowerLim[2] & horizontalPts[,2] < upperLim[2])
        sampleLine = horizontalPts[ bounded ,]

        if(is.matrix(sampleLine)){
          modLine = lm(sampleLine[,2] ~ sampleLine[,1])

          closeToLine = bounded %>% which
          closeToLine = closeToLine[ abs(modLine$residuals) < tol2 ]
          onLine = horizontalPts[ closeToLine ,]

          if(onLine %>% is.matrix && nrow(onLine) > 0){
            includePts %<>% c(closeToLine)
            lineNumber = lineNumber + 1
            multiLine[ closeToLine ] = lineNumber
          }
        }
        donePts %<>% c(which(bounded))
      }
    }
    return(multiLine)
  } %>% as.matrix

  filteredPts = foreach(j = 1:ncol(lineCols), .combine = 'cbind', .packages = c('magrittr')) %dopar% {

    multiLine = lineCols[,j]
    
    if(all(is.na(multiLine))) return(NA)
    
    lineVals = unique(multiLine)
    lineVals = lineVals[!is.na(lineVals)]

    lineRanges = by(horizontalPts %>% as.data.frame, multiLine, function(x) range(x[,1]) )
    lineLengths = sapply(lineRanges, diff)
    lineYs = by(horizontalPts %>% as.data.frame, multiLine, function(y) mean(y[,2]) )
    linesChecked = c()

    cleanLines = NULL
    while(length(linesChecked) < length(lineVals)){
      i = lineVals[ !(lineVals %in% linesChecked) ][1] %>% as.character

      print(paste( (lineVals %>% length) - (linesChecked %>% length), 'remaining lines' ))

      tempY = lineYs[[i]] - lineYs
      tempRange = lineRanges[[i]]
      tempRange = sapply(lineRanges, function(x) ( x[[1]] >= tempRange[[1]] & x[[1]] <= tempRange[[2]] ) |
                                                 ( x[[2]] >= tempRange[[1]] & x[[2]] <= tempRange[[2]] ) )
      tempClose = abs(tempY) < tol2
      tempNoise = !tempClose & abs(tempY) < spcLin * 2/3

      add  = which(tempRange & tempClose)
      take = which(tempRange & tempNoise)

      multiLine[multiLine %in% add]  = i %>% as.double

      if(length(take) > 0){
        tempLen = c(lineLengths[take], lineLengths[i]) %>% sort(decreasing = T)
        tempRem = names(tempLen)[-1] %>% as.double
        multiLine[multiLine %in% tempRem] = NA
        i = names(tempLen)[1]
      }

      # i %<>% as.double
      # temp = multiLine == i & !is.na(multiLine)
      # ptOrd = order( horizontalPts[ temp , 1] )
      # tempLine = st_coordinates( pts[temp,] )[ptOrd,] %>% as.matrix %>% st_linestring
      # if(cleanLines %>% is.null) cleanLines = tempLine else cleanLines %<>% c(tempLine)

      linesChecked %<>% c(add, take)
    }
    return(multiLine)
  } %>% as.matrix
  
  stichLines = foreach(i = 1:ncol(filteredPts), .combine = 'c', .packages = c('magrittr')) %dopar% {
    
    multiLine = filteredPts[,i]
    
    if(all(is.na(multiLine))) return(NULL)
    
    lineAngs = by(horizontalPts %>% as.data.frame, multiLine, function(y) lm(y[,2]~y[,1])$coefficients[2] %>% as.double %>% atan) * 180/pi
    crooked = names(lineAngs)[abs(lineAngs) > 2] %>% as.double
    
    angAdjust = lineAngs[lineAngs < 2] %>% mean(na.rm=T) * pi/180
    
    rMat = matrix(c(cos(angAdjust) , sin(angAdjust), -sin(angAdjust), cos(angAdjust)), byrow = T, ncol=2)
    trueHorizontal = (rMat %*% (horizontalPts %>% t)) %>% t
    
    multiLine[multiLine %in% crooked] = NA   
    
    yMeans = by(trueHorizontal %>% as.data.frame, multiLine, function(y) median(y[,2]) )
    yDists = dist(yMeans) %>% as.matrix

    coLines = which(yDists < tol, arr.ind = T)
    coLines = coLines[order(coLines[,1]),] %>% apply(1, sort) %>% t %>% unique
    coLines = split(coLines, coLines[,1]) %>% lapply(unique) %>% lapply(function(x) colnames(yDists)[x] %>% as.double )
    
    for(j in coLines){
      temp = trueHorizontal[multiLine %in% j, ]
      if(nrow(temp) < 2 || !is.matrix(temp)) next
      mod = lm(temp[,2] ~ temp[,1])
      keep = which(multiLine %in% j)[mod$residuals < tol]
      multiLine[ keep ] = j[1]
    }
    
    temp = data.frame(trueHorizontal, multiLine)
    temp = temp[ order(temp[,3], temp[,1]) ,]
    temp = temp[!is.na(temp[,3]),]
    temp = by(temp, temp[,3], function(x){
      dst = x[-1,1] - x[-nrow(x),1]
      longs = which(dst > spcPla+tol2)
      names(x) = c('x', 'y', 'id')
      coords = lapply(longs, function(y){
        run = seq(0, dst[y]-tol2, spcPla)[-1]
        xCoords = x[y,1]+run
        yCoords = rep(x[y,2], length(xCoords))
        lineId = rep(x[y,3], length(xCoords))
        return(data.frame(x=xCoords, y=yCoords, id=lineId))
      }) %>% do.call(what = rbind)
      return(rbind(x, coords))
    }) %>% do.call(what = rbind)
    
    temp = temp[order(temp[,3], temp[,1]),]
    
    rMat = matrix(c(cos(-angAdjust) , sin(-angAdjust), -sin(-angAdjust), cos(-angAdjust)), byrow = T, ncol=2)
    temp[,1:2] = (rMat %*% (temp[,1:2] %>% t)) %>% t
    
    return(list(lines = temp, points = multiLine))
  }

  # stopImplicitCluster()
  # stopCluster(cl)
  # registerDoSEQ()
}

pointLog = stichLines[seq(2,length(stichLines),2)] %>% do.call(what = cbind)
# pointLog = multiLine %>% as.matrix
pts2 = pts[apply(pointLog,1,function(x) !all(is.na(x))),]

rMat = matrix(c(cos(-ang) , sin(-ang), -sin(-ang), cos(-ang)), byrow = T, ncol=2)
geoLines = NULL
for(i in seq(1,length(stichLines), 2)){
  temp = stichLines[[i]]
  temp[,-3] = (rMat %*% (temp[,-3] %>% t)) %>% t
  temp = by(temp, temp[,3], function(x){
    x[,-3] %>% as.matrix %>% st_linestring
  }) %>% do.call(what = c)
  if(is.null(geoLines)) geoLines = temp else geoLines %<>% c(temp)
}
geoLines %<>% st_geometry() %>% st_cast('LINESTRING')
st_crs(geoLines) = st_crs(pts)
geoLines = geoLines[ sapply(geoLines, function(x) length(as.double(x))) > 2 ]
geoLines = st_intersection(geoLines, stands)

Plantas = as_Spatial(pts2)
Linhas  = as_Spatial(geoLines) %>% SpatialLinesDataFrame(data.frame(ID = 1:length(geoLines)), match.ID = F)

>print(paste('geradas', nrow(Linhas), 'linhas de plantio'))

# pLines = apply(pointLog, 2, function(x){
#   ord = order(horizontalPts[,1])
#   by(st_coordinates(pts)[ord,], x[ord], function(y){
#     y %>% as.matrix %>% st_linestring
#   }) %>% do.call(what = c) %>%  return()
# }) %>% do.call(what = c) %>% st_geometry %>% st_cast('LINESTRING')
# st_crs(pLines) = st_crs(pts)
# pLines = pLines[ sapply(pLines, function(x) length(as.double(x))) > 2 ]
# pLines = st_intersection(pLines, stands)

# st_write(pts2, 'pts2.shp', delete_layer = T)
# st_write(pLines, 'lines2.shp', delete_layer=T)
# st_write(geoLines, 'lines3.shp', delete_layer=T)
