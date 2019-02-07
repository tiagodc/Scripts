# load packages
{
  require(magrittr)
  require(raster)
  require(rgeos)
  require(sp)
  require(lidR)
  require(doParallel)
  require(foreach)
  require(caret)
  require(leaps)
}

# set working directory
setwd('C:/Users/tiago/Desktop/BID_ALS/')

# set number of cores to use on parallel processing
nCores = parallel::detectCores() - 1


### import global data

{
  # get clouds info
  ctg = catalog('CLOUDS/', recursive=T)
  # plot(ctg)
  
  # read and reproject total area shapefile
  areaTotal = shapefile('SIG/SHAPES/uso_solo_CBO(projetos_BID).shp', 
                        encoding="UTF-8", use_iconv=T) %>% spTransform(ctg@crs)
  # rgdal::writeOGR(areaTotal, 'SIG/SHAPES/uso_solo_CBO_reproj.shp', 'SIG/SHAPES/uso_solo_CBO_reproj', 
  #                 'ESRI Shapefile', overwrite_layer = T, encoding = 'UTF-8')
  
  # area of interest
  interestStands = c('Talhões')
  talhoes = areaTotal[areaTotal$DCR_CARAC_ %in% interestStands , ]
  
  # read and reproject plots point feature layer
  parcelas = shapefile('SIG/SHAPES/Parcelas_Centroids.shp',
                       encoding="UTF-8", use_iconv=T) %>% spTransform(ctg@crs)
  
  # calculate radii for circular plots
  parcelas$RAIO = sqrt( parcelas$AREA / pi )
  
  # set age classes for all plots
  parcelas$CLASS_IDADE = parcelas$PC_IDADE %>% cut(0:10) %>% as.character %>% factor
  
  # generate circular plots polygon layer with exact area
  # polyParcelas = gBuffer(parcelas[1,], width = parcelas$RAIO[1], quadsegs = 360)
  # for(i in 2:nrow(parcelas)){
  #   polyParcelas %<>% union(gBuffer(parcelas[i,], width = parcelas$RAIO[i], quadsegs = 360))
  # }
  # polyParcelas %<>% SpatialPolygonsDataFrame(data= parcelas@data)
  
}

### ALS data extraction

# set lidR background processing parameters
buffer(ctg) = 20
cores(ctg) = nCores
by_file(ctg) = F
tiling_size(ctg) = 1000

# define tree level metrics function
treeProps = function(x, y, z, i, hCut = 2, covCut = 7){
  output = list(
    area   = lidR::area(x,y),
    height = max(z),
    p90    = quantile(z[z>hCut], .9),
    dns    = length(z[z>covCut]) / length(z),
    relief = mean(z[z>hCut]) / max(z),
    x      = mean(x),
    y      = mean(y)
  )
  return(output)
}

# define plot/pixel level metrics functions
plotProps = function(x, y, z, i, radius, center, isCircle=T, hCut = 2, covCut = 7, 
                     subPixel = .5, qts = c(.1, .25, .5, .75, .9, .95, .99), 
                     hs=seq(hCut,45,5)){

  if(z[z > hCut] %>% length < 100) return(NULL)
    
  subCloud = LAS(data.frame(X=x, Y=y, Z=z, Intensity=i))
  chm = grid_canopy(subCloud, subPixel, subcircle = .2)
  # tops = tree_detection(subCloud, hmin = covCut, ws = 1.5)
  # lastrees_silva(subCloud, as.raster(chm) %>% focal(matrix(1,3,3), mean), tops)
  # lastrees_dalponte(subCloud, as.raster(chm) %>% focal(matrix(1,3,3), mean), tops, covCut)
  lastrees_li2(subCloud, hmin = covCut, speed_up = 2.25)
  singleTrees = tree_metrics(subCloud, treeProps(X,Y,Z,Intensity))
  # nrow(singleTrees) / (pi * (radius+5)^2) * 10000
  
  if(isCircle){
    buffer = sqrt((x-center[1])^2 + (y-center[2])^2) > radius
    chmBuffer = sqrt((chm$X-center[1])^2 + (chm$Y-center[2])^2) > radius
    singleTreeBuffer = sqrt((singleTrees$x-center[1])^2 + (singleTrees$y-center[2])^2 ) > radius
  }else{
    buffer = abs(x - center[1]) > radius | abs(y - center[2]) > radius
    chmBuffer = abs(chm$X - center[1]) > radius | abs(chm$Y - center[2]) > radius
    singleTreeBuffer = abs(singleTrees$x - center[1]) > radius | abs(singleTrees$y - center[2]) > radius
  }
  
  singleTrees = singleTrees[!singleTreeBuffer,]
  # nrow(singleTrees) / (pi * (radius)^2) * 10000
  
  keep  = z > hCut & !buffer
  zKeep = z[keep]
  
  output = list(
    hMean  = mean(zKeep),
    hSd    = sd(zKeep),
    hSkew  = moments::skewness(zKeep),
    hKurt  = moments::kurtosis(zKeep),
    iMean  = mean(i[keep]),
    iSd    = sd(i[keep]),
    dns    = length(z[z>covCut & !buffer]) / length(z[!buffer]),
    relief = mean(zKeep) / max(z[!buffer])
  )
  
  output[ paste0('hQ',qts*100) ] = quantile(zKeep, qts)
  output[ paste0('iQ',qts*100) ] = quantile(i[keep], qts)
  
  hDensities = table( cut(zKeep, hs) ) / length(zKeep)
  output[ paste0('hD', names(hDensities)) ] = hDensities
  
  weibPars = tryCatch(MASS::fitdistr(zKeep, 'weibull', lower=0)$estimate, 
                      error = function(e) c(0,0))

  output$hWeibShape = weibPars[1]
  output$hWeibScale = weibPars[2]
  
  output$avgMaxH = mean(chm$Z[!chmBuffer])
  output$sdMaxH  = sd(chm$Z[!chmBuffer])
  
  output$nTrees         = nrow(singleTrees)
  output$avgCrownArea   = mean(singleTrees$area)
  output$sdCrownArea    = sd(singleTrees$area)
  output$avgCrownHeight = mean(singleTrees$height)
  output$sdCrownHeight  = sd(singleTrees$height)
  output$avgCrownP90    = mean(singleTrees$p90)
  output$sdCrownP90     = sd(singleTrees$p90)
  output$avgCrownDns    = mean(singleTrees$dns)
  output$sdCrownDns     = sd(singleTrees$dns)
  output$avgCrownRelief = mean(singleTrees$relief)
  output$sdCrownRelief  = sd(singleTrees$relief)
    
  return(output)
}

pixelProps = function(x, y, z, i, hCut = 2, covCut = 7, 
                      subPixel = .5, qts = c(.1, .25, .5, .75, .9, .95, .99), 
                      hs=seq(hCut,45,5)){
  
  if(z[z > hCut] %>% length < 100) return(NULL)
    
  subCloud = LAS(data.frame(X=x, Y=y, Z=z, Intensity=i))
  chm = grid_canopy(subCloud, subPixel, subcircle = .2)
  # tops = tree_detection(subCloud, hmin = covCut, ws = 1.5)
  # lastrees_silva(subCloud, as.raster(chm) %>% focal(matrix(1,3,3), mean), tops)
  # lastrees_dalponte(subCloud, as.raster(chm) %>% focal(matrix(1,3,3), mean), tops, covCut)
  lastrees_li2(subCloud, hmin = covCut, speed_up = 2.25)
  singleTrees = tree_metrics(subCloud, treeProps(X,Y,Z,Intensity))
  
  keep  = z > hCut
  zKeep = z[keep]
  
  output = list(
    hMean  = mean(zKeep),
    hSd    = sd(zKeep),
    hSkew  = moments::skewness(zKeep),
    hKurt  = moments::kurtosis(zKeep),
    iMean  = mean(i[keep]),
    iSd    = sd(i[keep]),
    dns    = length(z[z>covCut]) / length(z),
    relief = mean(zKeep) / max(z)
  )
  
  output[ paste0('hQ',qts*100) ] = quantile(zKeep, qts)
  output[ paste0('iQ',qts*100) ] = quantile(i[keep], qts)
  
  hDensities = table( cut(zKeep, hs) ) / length(zKeep)
  output[ paste0('hD', names(hDensities)) ] = hDensities
  
  weibPars = tryCatch(MASS::fitdistr(zKeep, 'weibull', lower=0)$estimate, 
                      error = function(e) c(0,0))
  
  output$hWeibShape = weibPars[1]
  output$hWeibScale = weibPars[2]
  
  output$avgMaxH = mean(chm$Z)
  output$sdMaxH  = sd(chm$Z)
  
  output$nTrees         = nrow(singleTrees)
  output$avgCrownArea   = mean(singleTrees$area)
  output$sdCrownArea    = sd(singleTrees$area)
  output$avgCrownHeight = mean(singleTrees$height)
  output$sdCrownHeight  = sd(singleTrees$height)
  output$avgCrownP90    = mean(singleTrees$p90)
  output$sdCrownP90     = sd(singleTrees$p90)
  output$avgCrownDns    = mean(singleTrees$dns)
  output$sdCrownDns     = sd(singleTrees$dns)
  output$avgCrownRelief = mean(singleTrees$relief)
  output$sdCrownRelief  = sd(singleTrees$relief)
  
  return(output)
}

# define metrics extraction workflow for whole area
getStandMetrics = function(las, spatialStands){

  lasclassify(las, spatialStands, field = "stand")
  
  las %<>% lasfilter(stand == T)
  
  if(is.null(las)) return(NULL)
  
  metrics = grid_metrics(las, pixelProps(X, Y, Z, Intensity), 20)
  
  return(metrics)
}

{
  # clip plots from all clouds
  # plotBuffer = 5
  lasPlots = lasclipCircle(ctg, parcelas@coords[,1], parcelas@coords[,2], parcelas$RAIO)
  
  # join lidar metrics with field data for all plots
  plotMetrics = foreach(pcd = lasPlots, .combine = 'rbind') %do% {
    center = c( extent(pcd)[1:2] %>% mean,  extent(pcd)[3:4] %>% mean)
    dists = sqrt( (parcelas@coords[,1] - center[1])^2 + (parcelas@coords[,2] - center[2])^2 )
    
    index = which(dists == min(dists))
    trueCenter = parcelas@coords[index,] %>% as.double
    
    field = parcelas@data[index,] %>% as.list
    lidar = lasmetrics(pcd, pixelProps(X,Y,Z,Intensity))
    
    if(is.null(lidar)) return(NULL)
    
    combo = c(field, lidar) %>% as.data.frame
    combo$index = index

    return(combo)
  }
  
  # write.csv(plotMetrics, 'plot_metrics.csv')
  # rm(lasPlots) ; gc()
  
  # calculate metrics for an entire catalog
  metrics = catalog_apply(ctg, getStandMetrics, list(spatialStands = talhoes), 
                          filter='-drop_z_below 0 -keep_classification 2 -keep_classification 3 -keep_classification 4 -keep_classification 5')

  metrics = metrics[!sapply(metrics, is.null)]
  metrics = metrics[sapply(metrics, nrow) > 0]
  metrics %<>% do.call(what = rbind)
  
  write.csv(metrics, 'all_metrics.csv')
    
}


### stratification
# load and filter metrics
metrics = read.csv('all_metrics.csv', row.names = NULL)[,-1]
metrics = metrics[!duplicated(metrics[,1:2]),]
naDetect = apply(metrics, 1, function(x) any(is.na(x)))
metrics = metrics[!naDetect,] %>% as.data.frame

{
  # establish training data
  lims = which(names(metrics) %in% c('hMean', 'sdCrownRelief'))
  trainData = metrics[,lims[1]:lims[2]]
  
  # standardize variables to avoid weighting on clustering
  stdTrainData = apply(trainData, 2, function(x) (x - mean(x))/sd(x) )
  
  # extract sample for lighter graphics
  sampleData = sample(1:nrow(trainData), size = 10000, replace = F)
  
  # variable cluster
  metricsDist = dist(stdTrainData %>% t)
  hClust = hclust(metricsDist, method = 'complete')
  coph = cor(cophenetic(hClust), dist(stdTrainData %>% t))
  
  # visual assessment to define group number
  # plot(hClust)
  # abline(h = rev(hClust$height)[4:5] %>% mean, col='red')
  varGroups = cutree(hClust, 3)
  
  # perform PCA to summarize samples/variables spread 
  pcaMetrics = princomp(trainData , cor = T, scores = T)
  
  # plot(pcaMetrics$scores[sampleData,1:2], cex=1, pch=20, xlim=c(-30, 30), ylim=c(-30, 30))
  # cols = varGroups %>% unique %>% length %>% rainbow
  # inflate=100
  # for(i in unique(varGroups)){
  #   gp = which(varGroups == i)
  #   
  #   arrows(0,0,pcaMetrics$loadings[gp,1] * inflate, pcaMetrics$loadings[gp,2] * inflate, 
  #          length = .05, angle = 20, lwd=2, col=cols[i])
  #   
  #   graphics::text(x=pcaMetrics$loadings[gp,1] * inflate, 
  #                  y=pcaMetrics$loadings[gp,2] * inflate, 
  #                  labels=names(varGroups[gp]), col=cols[i])
  #   
  # }
  
  # pick proxy variables
  handPick = c('hQ90', 'sdMaxH', 'iMean', 'relief', 'dns', 'iSd', 'sdCrownRelief')
  nClusts = sapply(2:10, function(x) kmeans(stdTrainData[,handPick], x)$withinss %>% mean )
  # plot(nClusts)
  kClust = kmeans(stdTrainData[,handPick], 3)
  
  # biplot of grouped samples against proxies
  cols = kClust$cluster %>% unique %>% length %>% rainbow
  # plot(pcaMetrics$scores[sampleData,1:2], cex=1, pch=20, xlim=c(-30, 30), ylim=c(-30, 30), col=cols[kClust$cluster[sampleData]])
  
  # inflate=100
  # arrows(0,0,pcaMetrics$loadings[handPick,1] * inflate, pcaMetrics$loadings[handPick,2] * inflate, length = .05, angle = 20, lwd=3)
  # text(x=pcaMetrics$loadings[handPick,1] * inflate, y=pcaMetrics$loadings[handPick,2] * inflate, labels=handPick, cex=1.5)
  # 
  # organize stratification variables and merge to metrics data frame
  stratInfo = cbind(kClust$cluster, pcaMetrics$scores[,1:3]) %>% as.data.frame
  names(stratInfo) = c('kMeans', 'PC1', 'PC2', 'PC3')
  
  metrics %<>% cbind(stratInfo)
}

### data analysis

forceTraining = function(x, y, trPrcnt, method='lm', metric='RMSE', maxIter = 10){
  
  trc = 
    if(trPrcnt == 1){
      trainControl(method = 'LOOCV')
    }else{
      trainControl(method = 'LGOCV', p=trPrcnt, number=10)
    }
  
  fitted = NULL
  counter = 1
  
  while(is.null(fitted) && counter <= maxIter){
    fitted = tryCatch(train(x=x, y=y, method=method, metric=metric, trControl = trc),
                      error = function(e) return(NULL))
    counter %<>% sum(1)
  }
  
  return(fitted)
  
}

estimateFromModel = function(modelInfo, fittedModels, metricsTable, CF=F){
  
  model = fittedModels[[ modelInfo$var %>% as.character ]][[ modelInfo$group %>% as.character ]][[ modelInfo$metrics %>% as.character ]]
  
  if(modelInfo$group == 'ml'){
    fit = metricsTable
    fit = extractPrediction(list(model), unkX = fit, unkOnly = T)$pred %>% as.data.frame
  }else{
  
    fit = modelInfo$metrics %>% 
      as.character %>% 
      strsplit(split = '|', fixed = T)
    
    fit = fit[[1]] %>%
      sapply(function(x){
        if(grepl('^log_', x)){
          cn = sub('^log_(.+)', '\\1', x)
          temp = metricsTable[,cn] %>% log
        }else{
          temp = metricsTable[,x]
        }
        return(temp)
      })
    
    fit = extractPrediction(list(model), unkX = fit, unkOnly = T)$pred %>% as.data.frame
    
    if(CF) fit = 1
    
    if( modelInfo$group == 'log' ){
      bias = 
        exp( sqrt( sum(residuals(model)^2) / (length(residuals(model)) - model$finalModel$rank) )^2 / 2)
      # residuals(model) %>% exp %>% mean
      if(CF){
        fit = bias
      }else{
        fit = exp(fit) * bias
      }
    }
  }

  cn = paste(modelInfo$var %>% as.character, modelInfo$group %>% as.character, modelInfo$metrics %>% as.character, sep='|')
  names(fit) = cn
  
  return(fit)
}

## modelling
{

  # lidar variables 
  lims  = which(names(plotMetrics) %in% c('hMean', 'sdCrownRelief'))
  lidar = plotMetrics[,handPick] 
  # lidar = plotMetrics[,lims[1]:lims[2]]
  # lidarFilter = apply(lidar, 2, function(x) any(is.na(x)) | sd(x) == 0 )
  # lidar = lidar[!lidarFilter]
  
  # log metrics
  logLidar = log(lidar)
  names(logLidar) = paste0('log_', names(lidar))
  logLidar = logLidar[, !apply(logLidar, 2, function(x) any(is.na(x)) | any(is.infinite(x)) | any(is.nan(x)) ) ]
  allLidar = cbind(lidar, logLidar)

  # fit models
  {
    cl = makePSOCKcluster(nCores)
    registerDoParallel(cl)

    predVars = c('PC_VOL_TOT', 'PC_DAP', 'PC_HT', 'PC_AB', 'PC_DG', 'PC_H100', 'NFUSTES')
    fitModels = foreach(i = predVars, .combine = 'c', .packages = c('magrittr', 'leaps', 'caret', 'foreach')) %dopar% {

      regSubs = regsubsets(x=allLidar, y=plotMetrics[,i], nvmax = 3, nbest = 3, really.big = T) %>%
        summary %$% apply(which, 1, function(x) names(x[x])[-1] )
      
      regSubsLog = regsubsets(x=logLidar, y=log(plotMetrics[,i]), nvmax = 4, nbest = 3) %>%
        summary %$% apply(which, 1, function(x) names(x[x])[-1] )
      
      modelsList = foreach(perc = seq(1,1,-.1), .combine='c') %do% {
        
        lmModels = lapply(regSubs, function(x){
          forceTraining(x= allLidar[,x,drop=F], y=plotMetrics[,i], trPrcnt = perc)
        })
        names(lmModels) = sapply(regSubs, paste, collapse='|')

        logModels = lapply(regSubsLog, function(x){
          forceTraining(x= logLidar[,x,drop=F], y=log(plotMetrics[,i]), trPrcnt = perc)
        })
        names(logModels) = sapply(regSubsLog, paste, collapse='|')

        mlMethods = c('rf', 'svmLinear', 'svmPoly')#, 'nnet', 'avNNet', 'DENFIS')
        mlModels = foreach(mth = mlMethods, .combine = 'c') %do% {
          list(
            forceTraining(x= lidar, y=plotMetrics[,i], method = mth, trPrcnt = perc)
          )
        }
        names(mlModels) = mlMethods
        
        combModels = list( lmModels, logModels, mlModels )
        names(combModels) = c('lm', 'log', 'ml')
        
        out = list(combModels)
        names(out) = paste(i, perc, sep='|')
        
        return(out)
      }
      return(modelsList)
    }
    
    stopImplicitCluster()
    stopCluster(cl)
    registerDoSEQ()
  }
  
  # get diagnostics from every model
  infoModels = foreach(varPerc = fitModels %>% names, .combine = 'c') %:% 
    foreach(grp = names( fitModels[[varPerc]] ), .combine = 'c') %:%
      foreach(mod = names( fitModels[[varPerc]][[grp]] )) %do% {
        
        temp = fitModels[[varPerc]][[grp]][[mod]]
        if(is.null(temp)) return(NULL)
    
        bestFit = temp$bestTune %>% row.names %>% as.integer
    
        info = data.frame(var = varPerc, group = grp, metrics = mod, type = temp$modelInfo$label)
        result  = temp$results[ bestFit ,,drop=F]
        result$CF = 1
        
        if(grp == 'log'){
          pred = predict(temp)
          obs  = temp$trainingData$.outcome
          
          result$RMSE = (exp(pred) - exp(obs))^2 %>% mean %>% sqrt
          result$MAE  = abs(exp(pred) - exp(obs)) %>% mean
          result$CF   = exp( sqrt( sum(residuals(temp)^2) / (length(residuals(temp)) - temp$finalModel$rank) )^2 / 2)
        }
        
        return(cbind(info, result))
  }
  
  # combine main diagnostics in data frame
  commonVars = c("var", "group", 'metrics', "type", "RMSE", "Rsquared", "MAE", 'CF')
  commonVars = foreach(i = infoModels, .combine = 'rbind') %do% { i[,commonVars] }
  
  # split data frame per predicted variable
  varPerSample = split(commonVars, commonVars$var) %>% lapply(function(x){
    ord = with(x, order(RMSE, -Rsquared, MAE))
    return(x[ord,])
  })
  
  # select models 
  selectModels = lapply(predVars, function(x){
    temp   = varPerSample[[ paste0(x,'|1') ]]
    lens   = sapply(temp$metrics %>% as.character, function(y) strsplit(y, split='|', fixed = T)[[1]] %>% length)
    
    simple = which(lens == 1 & temp$group == 'lm') %>% head(1)
    double = which(lens == 2) %>% head(1)
    other  = which( !(1:nrow(temp) %in% c(simple,double)) ) %>% head(1)
    
    selec  = c(simple, double, other) 
    return( temp[selec,] )
  })
  names(selectModels) = predVars

  # fit all models over all pixels
  predicted = foreach(var = selectModels, .combine = 'cbind') %:% foreach(i = 1:nrow(var), .combine = 'cbind') %do% {
    
    tempMts = metrics
    if(var[i,'group'] == 'ml'){
      tempMts = metrics[,names(lidar)]
    }
    
    preds = estimateFromModel(var[i,], fitModels, tempMts)
    qt = quantile(preds[,1], c(.01,.99))
    preds[ preds[,1] < qt[1] | preds[,1] > qt[2] | is.infinite(preds[,1]) | preds[,1] < 0, 1] = NA
    return(preds)
  }
  
  # merge fitted variables to metrics grid
  metrics %<>% cbind(predicted)
  
}

## direct tree count
{
  treeHa = 10000 * plotMetrics$nTrees / (pi * plotMetrics$RAIO^2)
  treeCountError = treeHa - plotMetrics$NFUSTES
  nTreeStats = c(
    RMSE = mean(treeCountError^2) %>% sqrt,
    MAE  = abs(treeCountError) %>% mean 
  )

  # estimate trees per ha for whole area
  metrics$treesPerHaCount = 10000 * metrics$nTrees / 400
  
  # plot(treeCountError ~ plotMetrics$NFUSTES, pch=20)
  # abline(h=0, col='red')
  
  write.csv(metrics, 'full_info.csv')
}


### build maps

{
  # convert points to pixels to polygons
  metrics = read.csv('full_info.csv')[,-1]
  metrics = sp::SpatialPixelsDataFrame(metrics[,1:2], metrics[,-c(1:2)], proj4string = ctg@crs)
  metrics %<>% as("SpatialPolygonsDataFrame")
  
  # multicore processing for cropping pixel features
  {
    cl = makePSOCKcluster(nCores)
    registerDoParallel(cl)
    
    cropMetrics = foreach(i = talhoes$ID_PROJETO %>% unique, .combine = 'c', .packages = c('raster')) %dopar% {
      tal  = talhoes[ talhoes$ID_PROJETO == i ,]
      temp = intersect(metrics, tal)
      return(temp)
    }
    
    stopImplicitCluster()
    stopCluster(cl)
    registerDoSEQ()
  }
  
  # merge all projects into single feature layer
  cropMetrics = do.call(rbind, cropMetrics)
  rm(metrics) ; gc()
  
  # get weighted means per stand
  cropMetrics$pixArea = raster::area(cropMetrics)
  projtal = paste0(cropMetrics$ID_PROJETO, cropMetrics$CD_TALHAO)
  lim = ( (names(cropMetrics) == 'OBJECTID') %>% which ) - 1
  standWise = by(cropMetrics@data, projtal, function(x){
    # if(sum(x$pixArea) < 2500*mean(x$VLR_AREA)) return(0)
    weights = x$pixArea / sum(x$pixArea)
    res = apply(x[,1:lim], 2, function(y) sum(y*weights, na.rm=T))
    res$pixArea = sum(x$pixArea) / 10000
    return(res)
  }) %>% do.call(what = rbind)

  # merge stand info to shapefile
  standWise %<>% as.data.frame %>% cbind(PROJTAL = row.names(standWise))
  talhoes$PROJTAL = paste0(talhoes$ID_PROJETO, talhoes$CD_TALHAO)
  talhoes %<>% merge(y = standWise, by='PROJTAL', all.x = T)
  
  # get age class for pixels and stands 
  cropMetrics$CLASS_IDADE = cut(cropMetrics$IDADE_PLAN, seq(0,30, 1)) %>% as.character %>% factor
  talhoes$CLASS_IDADE = cut(talhoes$IDADE_PLAN, seq(0,30,1)) %>% as.character %>% factor
  talhoes$kMeans %<>% as.double %>% round(0)
  
  # write shapefile of cropped pixels
  rgdal::writeOGR(cropMetrics, 'cropped_pixels.shp', 'cropped_pixels', 'ESRI Shapefile', overwrite_layer = T, encoding = 'UTF-8')
  
  # write shapefile of stand-wise info
  for(i in names(talhoes@data)){
    temp = talhoes@data[,i]
    if(is.list(temp)){
      talhoes@data[,i] = temp %>% as.double
    }
  }
  rgdal::writeOGR(talhoes, 'talhoes_full.shp', 'talhoes_full', 'ESRI Shapefile', overwrite_layer = T, encoding = 'UTF-8')
    
}


### forest inventory

# confidence width calculation
calcCI = function(err, n, alpha=.05){
  return(
    qt(1 - alpha/2, n-1) * err #/ sqrt(n)
  )
}

# calculation of sample parameters
calcPars = function(df, N, alpha=.05){
  
  means = apply(df, 2, mean)
  vars   = apply(df, 2, function(y){
    (var(y) / length(y)) * ((N - length(y)) / N)
  })
  err = sqrt(vars)
  cis = calcCI(err, nrow(df), alpha)
  
  err_pc = 100*cis/means
  
  out = rbind(means, vars, err, cis, err_pc, nrow(df)) %>% as.data.frame
  row.names(out) = c('mean', 'mean_var', 'std_err', 'ci', 'err_pc', 'n')
  names(out) = names(df)
  
  return(out)
}

# ideal number of plots for simple casual sampling
idealPlotNumber = function(y, N, errDesired=.05, alpha=.05){
  B = errDesired * mean(y)
  qt = qt(1 - alpha/2, length(y)-1)
  
  n = N*var(y)*qt^2 / (N * B^2 + qt^2 * var(y))
  
  return(n)
}

# ideal plots for stratified sample
stratPlotNumber = function(y, g, Nh, errDesired=.05, alpha=.05){
  vars = by(y, g, stats::var)
  Wh   = by(y, g, length) / length(y)
  
  Nh = Nh[ names(Nh) %in% g ]
  
  B = errDesired * mean(y)
  
  n = sum( Nh^2 * vars / Wh ) / ( (sum(Nh)^2 * B^2)/4 + sum(Nh * vars) )
    
}

# ideal number of plots for double sampling
dubleSamplePlotNumber = function(y, x, xLarge, Cpg = 300, errDesired = .05, alpha = .05){

  rho = cor(x,y)
  a = var(y) * (1 - rho^2)
  b = var(y) * rho^2
  
  B = errDesired * mean(y)
  qt = qt(1 - alpha/2, length(y)-1)

  nG = (sqrt( a*b*Cpg ) + b) / (B^2 / qt^2)
  nP = (sqrt( a*b/Cpg ) + a) / (B^2 / qt^2)
  
  return(nP)
}

# double sampling regression estimations
doubleSampleRegPars = function(y, x, xLarge, alpha=.05){
  n = length(y)
  beta = ( sum(y*x, na.rm=T) - ( sum(x, na.rm=T)*sum(y, na.rm=T) / n ) ) / ( sum(x^2, na.rm=T) - (sum(x, na.rm=T)^2 / n) )
  
  rho = cor(y,x)
  N = length(xLarge)
  
  ydsr = mean(y, na.rm=T) + beta * ( mean(xLarge, na.rm=T) - mean(x, na.rm=T) )
  vardsr = (var(y, na.rm=T)/n)*(1 - (rho^2)*(N-n)/N)
  stderr = sqrt(vardsr)
  ci     = calcCI(stderr, n, alpha)
  
  out = c(mean = ydsr, mean_var = vardsr, std_err = stderr, ci = ci, err_pc = 100*ci/ydsr, n=n, rho=rho)
  
  return(out)
}

# double sampling ratio estimations
doubleSampleRatioPars = function(y, x, xLarge, popSize, alpha=.05){
  
  n = length(y)
  N = length(xLarge)
  
  Rhat = mean(y) / mean(x)
  varLarge = var(xLarge) 
  covXY = cov(x,y)
  k = var(y) + Rhat^2 * varLarge - 2*Rhat*covXY
  l = 2*Rhat*covXY - Rhat^2*varLarge
  m = -(var(y)/popSize)
  
  ydsr   = Rhat * mean(xLarge)
  vardsr = k/n + l/N + m #abs()
  stderr = sqrt(vardsr)
  ci     = calcCI(stderr, n, alpha)
  
  out = c(mean = ydsr, mean_var = vardsr, std_err = stderr, ci = ci, err_pc = 100*ci/ydsr, n=n)
  
  return(out)
}

# population estimation from strata
popFromStrata = function(factorStrataList){

  popEstimates = foreach(i = factorStrataList, .combine = 'c') %do% {
    gpMeans = lapply(i, function(x) x[1,,drop=F]) %>% do.call(what = rbind)
    gpVars  = lapply(i, function(x) x[2,,drop=F]) %>% do.call(what = rbind)
    
    cols = 1:(ncol(gpMeans)-4)
    popMean   = apply(gpMeans[,cols], 2, function(x) sum(x*gpMeans$N) ) / populationSize
    popVar    = apply(gpVars[,cols], 2, function(x) sum( x * (gpVars$N/populationSize)^2 ) )
    popStdErr = sqrt(popVar)
    popCI     = calcCI(popStdErr, sum(gpMeans$n))
    
    popPars = data.frame(
      mean     = popMean,
      mean_var = popVar,
      std_err  = popStdErr,
      ci       = popCI,
      err_pc   = 100 * popCI / popMean,
      n        = sum( sapply(i, function(x) mean(x$n)) )
    )
    
    return(list(popPars))
  }
  
  names(popEstimates) = names(factorStrataList)
  return(popEstimates)
}

{
  # get kmeans stratum for each sample plot 
  temp = shapefile('SIG/SHAPES/Parcelas_Centroids.shp')
  spatialPlotMetrics = SpatialPointsDataFrame(plotMetrics[,c('X','Y')], plotMetrics, proj4string = temp@proj4string) %>% spTransform(ctg@crs)
  rm(temp); gc()
  
  metrics = read.csv('full_info.csv')[,-1]
  
  spatialPlotMetrics$kMeans = spatialPlotMetrics@coords %>% apply(1, function(x){
    minDist = sqrt( (x[1] - metrics$X)^2 + (x[2] - metrics$Y)^2 ) %>% order(decreasing = F) %>% head(1)
    return(metrics$kMeans[minDist])
  })

  # extract all information of plots' location  
  groups = apply(spatialPlotMetrics@data, 2, function(x) x[!is.na(x)] %>% unique %>% length )
  groups = groups[groups <= 10 & groups > 1] %>% names

  # stratification groups
  groups = c('DCR_MATERI', 'CLASS_IDADE', 'DCR_ESPACA', 'kMeans') #, CD_REGIME)
  
  # set variables of interest
  interestVars = c('PC_VOL_TOT', 'PC_DAP', 'PC_HT', 'PC_AB', 'PC_DG', 'PC_H100', 'NFUSTES')

  # check stratification classes
  # par(mfrow=c( length(groups), length(interestVars) ))
  # attach(spatialPlotMetrics@data)
  # for(i in groups){ 
  #   for(j in interestVars){
  #     plot( get(i) %>% factor , get(j), main = j, sub=i)
  #   }
  # }
  # detach(spatialPlotMetrics@data)
  
  # get population size
  populationSize = raster::area(talhoes[talhoes$IDADE_PLAN < 6 & talhoes$IDADE_PLAN > 2,]) %>% sum / mean(spatialPlotMetrics$AREA)
  populationSize = raster::area(talhoes) %>% sum / mean(spatialPlotMetrics$AREA)

    # get casual sampling info
  simplePars = spatialPlotMetrics@data[,interestVars] %>% calcPars(populationSize)

  # get stratified sampling info
  rightAges = talhoes$IDADE_PLAN < 6 & talhoes$IDADE_PLAN > 2
  stratPars = foreach(fac = groups, .combine = 'rbind') %:% foreach(g = spatialPlotMetrics@data[,fac] %>% unique %>% as.character %>% sort, .combine = 'rbind') %do% {
    inGroup = talhoes@data[,fac] == g
    popSize = sum( raster::area(talhoes[rightAges & !is.na(inGroup) & inGroup,] )) / mean(spatialPlotMetrics$AREA)
    
    inGroup = spatialPlotMetrics@data[,fac] == g
    tempPars = spatialPlotMetrics@data[inGroup,interestVars,drop=F]
    
    inventory = calcPars(tempPars, popSize)
    inventory$factor = fac
    inventory$group  = g
    inventory$n      = nrow(tempPars)
    inventory$N      = popSize
    
    return(inventory)
  }
  
  stratPars %<>% base::split(f = stratPars$factor) %>% lapply(function(x) split(x, x$group))
  globalStratPars = popFromStrata(stratPars)

  # get double sampling parameters
  lims = spatialPlotMetrics@data %>% names %in% c('hMean', 'sdCrownRelief') %>% which
  lidarOnly = spatialPlotMetrics@data[,lims[1]:lims[2]]
  
  corrMat = cor(spatialPlotMetrics@data[,interestVars], lidarOnly)
  corrMat = corrMat[,apply(corrMat, 2, function(x) !any(is.na(x)))]

  doubleSamplePars = foreach(i = interestVars, .combine = 'rbind') %do% {
    aux = corrMat[i,]
    pick = which( aux == max(aux) ) %>% names
    XL = cropMetrics@data[cropMetrics@data$IDADE_PLAN > 2 & cropMetrics@data$IDADE_PLAN < 6,pick]
    
    est = doubleSampleRegPars(spatialPlotMetrics@data[,i], spatialPlotMetrics@data[,pick], XL) %>% data.frame()
    # est = doubleSampleRatioPars(spatialPlotMetrics@data[,i], spatialPlotMetrics@data[,pick], XL, populationSize) %>% data.frame()
    names(est) = i
    
    est %<>% t %>% as.data.frame
    est$aux = pick
    
    return(est)
  }

  # get stratified double sampling info
  rightAges = talhoes$IDADE_PLAN < 6 & talhoes$IDADE_PLAN > 2
  rightAgesPixels = cropMetrics@data$IDADE_PLAN < 6 & cropMetrics@data$IDADE_PLAN > 2
  stratDoublePars = foreach(fac = groups, .combine = 'rbind') %:% foreach(g = spatialPlotMetrics@data[,fac] %>% unique %>% as.character %>% sort, .combine = 'rbind') %do% {
    inGroup = talhoes@data[,fac] == g
    popSize = sum( raster::area(talhoes[rightAges & !is.na(inGroup) & inGroup,] )) / mean(spatialPlotMetrics$AREA)
    
    inGroup  = spatialPlotMetrics@data[,fac] == g
    tempPars = spatialPlotMetrics@data[inGroup,interestVars,drop=F]

    pixelPars = cropMetrics@data[,fac] == g & rightAgesPixels
    
    inventory = sapply(names(tempPars), function(x){
      covar = corrMat[x,] %>% sort %>% tail(1) %>% names
      covPlot = spatialPlotMetrics@data[inGroup,covar]
      dPars = doubleSampleRegPars(tempPars[,x], covPlot, cropMetrics@data[pixelPars,covar])
      return(dPars)
    }) %>% as.data.frame
    
    inventory$factor = fac
    inventory$group  = g
    inventory$n      = nrow(tempPars)
    inventory$N      = popSize
    
    return(inventory)
  }
  
  stratDoublePars %<>% base::split(f = stratDoublePars$factor) %>% lapply(function(x) split(x, x$group))
  globalDoubleStratPars = popFromStrata(stratDoublePars)

  # combine all results
  inventoryTables = list(
    simpleCasualSample     = simplePars %>% t,
    stratifiedSample       = globalStratPars,
    doubleSample           = doubleSamplePars,
    stratifiedDoubleSample = globalDoubleStratPars,
    perStratum             = stratPars,
    perStratumDouble       = stratDoublePars
  )
  
  # ideal plot numbers
  errs = 1:20
  scsPlotn = idealPlotNumber(plotMetrics$PC_VOL_TOT, populationSize, errDesired = errs/100)
  
  stratAreas = by(area(talhoes), talhoes$CLASS_IDADE, sum) / mean(parcelas$AREA)
  cssPlotn = stratPlotNumber(plotMetrics$PC_VOL_TOT, plotMetrics$CLASS_IDADE, stratAreas, errs/100)

  dsPlotn = dubleSamplePlotNumber(plotMetrics$PC_VOL_TOT, plotMetrics$avgCrownHeight, cropMetrics$avgCrownHeight, 300, errs/100)
  
}

# png('nParcelas.png', 15, 10, 'cm', res = 200)
# plot( scsPlotn ~ errs, type='l', col='red', lwd=2, ylim=c(0,300), ylab='Número de parcelas', xlab='Erro amostral (%)',
#       axes=F)
# 
# axis(1)
# axis(2, at = c(35, 50, 85, 150, 200, 250, 300))
# axis(2, at = 50, cex.axis=.8, line=-.7)
# box()
# 
# lines(errs, cssPlotn, col='green', lwd=2)
# lines(errs, dsPlotn, col='blue', lwd=2)
# 
# abline(v=5, col='black', lwd=2, lty=2)
# lines(c(0,5), rep(dsPlotn[5], 2), col='blue', lty=2, lwd=2)
# lines(c(0,5), rep(cssPlotn[5], 2), col='green', lty=2, lwd=2)
# lines(c(0,5), rep(50, 2), col='black', lty=2, lwd=2)
# 
# legend('topright', col=rainbow(3), lwd=2,
#        legend = c('ACS', 'ACE', 'AD'))
# 
# dev.off()

# save.image(file='BID_inventario.RData')
# load('BID_inventario.RData')
