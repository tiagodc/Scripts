# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge a bunch of ASC raster files into one single ASC file
#
# Autor: Luiz Carlos Estraviz Rodriguez, Tiago de Conto
# Projeto: UPM
# Date: 11/Jul/2017
#
# Fonte: http://r-video-tutorial.blogspot.com.br/2014/04/merge-asc-grids-with-r.html
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls(all=TRUE))                                  # Clean memory

# Install packages~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# library("raster")
# library("sp")
# library("rasterVis")
# library("rgeos")
# library("rgdal")
# library("latticeExtra")
# library("maptools")
# library("data.table")
# library("ggplot2")
# library("stringr")
require(raster)
require(sp)
require(rasterVis)
require(rgeos)
require(rgdal)
require(latticeExtra)
require(maptools)
require(data.table)
require(ggplot2)
require(stringr)

#LAStools batch calling
lasBinDir = "C:\\LAStools\\bin\\"
projectDir = "C:/Users/Tiago/Desktop/RLAS/"
cores = 3
setwd(projectDir)

makeCommand = function(tool, inputFile, ...){
  return(
    paste(paste(lasBinDir, tool, sep=''), '-i', inputFile , ...)
  )
}

filesFolder = 'CLOUDS/ATLCW/lazin/'
format = 'laz'
files = dir(filesFolder, pattern = str_c('.',format))

filesPath = paste(projectDir,filesFolder,sep='')
filesVector = paste(filesPath,files,sep='')


#check integrity
checkDir = str_c(filesPath, '0.check')
dir.create(checkDir, showWarnings = F)
filesFlag = str_c(filesPath,'*.',format)

checkCmd = makeCommand('lasvalidate', filesFlag, 
            '-no_CRS_fail -o', 
            str_c(checkDir, '/Validation.xml'))

print(checkCmd)
system(checkCmd)


#check strips
stripCmd = makeCommand('lasoverlap', filesFlag, 
                       #'-utm 22M',
                       '-step 1.0 -max_diff 1.0',
                       '-odir', checkDir)

print(stripCmd)
system(stripCmd)


#create infos
infoDir = str_c(filesPath, 'info')
dir.create(infoDir, showWarnings = F)

infoCmd = makeCommand('lasinfo', filesFlag,
                      '-cores', cores,
                      '-odir', infoDir,
                      '-odix _info',
                      '-otxt',
                      '-cd',
                      '-histo scan_angle 3')

print(infoCmd)
system(infoCmd)


#classify ground points
groundDir = str_c(filesPath, '1.ground')
dir.create(groundDir, showWarnings = F)

groundCmd = makeCommand('lasground', filesFlag,
                        '-cores', cores,
                        '-wilderness',
                        '-compute_height',
                        '-odir', groundDir,
                        '-odix', '_h',
                        '-olaz')

print(groundCmd)
system(groundCmd)


#classify non-ground points
classDir = str_c(filesPath, '2.lazc')
dir.create(classDir, showWarnings = F)

nGroungCmd = makeCommand('lasclassify', str_c(groundDir, '/*.laz'),
                         '-cores', cores,
                         #'-step 3',
                         #'-ground_offset 0.5',
                         #'-planar 0.2',
                         #'-rugged 0.4',
                         '-odir', classDir,
                         '-odix', '_c',
                         '-olaz')

print(nGroungCmd)
system(nGroungCmd)


#normalize tiles
normDir = str_c(filesPath, '3.lazn')
dir.create(normDir, showWarnings = F)
normCmd = makeCommand('lasheight', str_c(classDir, '/*.laz'),
                      '-replace_z',
                      '-set_user_data 0',
                      '-odir', normDir,
                      '-odix', '_n',
                      '-olaz')

print(normCmd)
system(normCmd)

nInfoDir = str_c(normDir, '/info')
dir.create(nInfoDir)
nInfoCmd = makeCommand('lasinfo', str_c(normDir,'/*.laz'),
                       '-cores', cores,
                       '-odir', nInfoDir,
                       '-otxt',
                       '-cd',
                       '-histo scan_angle 3')

print(nInfoCmd)
system(nInfoCmd)


#denoise tiles
noiseDir = str_c(filesPath, '4.laznn')
dir.create(noiseDir, showWarnings = F)
noiseCmd = makeCommand('lasnoise', str_c(normDir, '/*.laz'),
                      '-ignore_class 2',
                      '-cores', cores,
                      '-step 2',
                      '-isolated 3',
                      '-classify_as 1',
                      '-odir', noiseDir,
                      '-olaz')

print(noiseCmd)
system(noiseCmd)

dnInfoDir = str_c(noiseDir, '/info')
dir.create(dnInfoDir)
dnInfoCmd = makeCommand('lasinfo', str_c(noiseDir,'/*.laz'),
                       '-cores', cores,
                       '-odir', dnInfoDir,
                       '-otxt',
                       '-cd',
                       '-histo scan_angle 3')

print(dnInfoCmd)
system(dnInfoCmd)


#calculate metrics
metricsDir = str_c(filesPath, '5.metrics')
dir.create(metricsDir, showWarnings = F)

metricsCmd = makeCommand('lascanopy', str_c(noiseDir, '/*.laz'),
                         '-cores', cores,
                         #'-merged',
                         '-keep_z 0.0 35.0',
                         '-drop_class 1',
                         '-step 20',
                         #'-use_tile_bb',
                         '-centroids',
                         '-grid_ll 10 10',
                         '-height_cutoff 0.5', #0.0
                         '-p 90 -max -avg -std',
                         #'-c 0.0 0.5 40.0',
                         #'-d 0.0 0.5 40.0',
                         '-cover_cutoff 0.5',
                         '-cov -dns',
                         '-odir', metricsDir,
                         '-oasc')

print(metricsCmd)
system(metricsCmd)


#clean las

# Set paths, cell size number of areas of interest ~~~~~~~~~~~~~~~~~~~~ ----

projectDir <-"C:/Users/Tiago/Desktop/RLAS"
projectShp = paste(projectDir, '/SHAPES', sep='')
setwd(str_c(projectDir, "/CLOUDS/2016/STRIPS/7.metrics"))
cell_size <- 400
nbr_Areas <- 6
nbr_Gaps  <- 5
nbr_AOIs  <- nbr_Areas + nbr_Gaps
results   <- data.frame(AOI=NA, TotalArea=NA, TotalVolume=NA,
                        VOLmin=NA, VOL1stQ=NA, VOLMdn=NA, VOLMean=NA, VOL3rdQ=NA, VOLmax=NA,
                        P90min=NA, P901stQ=NA, P90Mdn=NA, P90Mean=NA, P903rdQ=NA, P90max=NA,
                        DNSmin=NA, DNS1stQ=NA, DNSMdn=NA, DNSMean=NA, DNS3rdQ=NA, DNSmax=NA,
                        MAXmin=NA, MAX1stQ=NA, MAXMdn=NA, MAXMean=NA, MAX3rdQ=NA, MAXmax=NA,
                        AVGmin=NA, AVG1stQ=NA, AVGMdn=NA, AVGMean=NA, AVG3rdQ=NA, AVGmax=NA,
                        STDmin=NA, STD1stQ=NA, STDMdn=NA, STDMean=NA, STD3rdQ=NA, STDmax=NA,
                        COVmin=NA, COV1stQ=NA, COVMdn=NA, COVMean=NA, COV3rdQ=NA, COVmax=NA)

# Merge tiled raster files (ASC) into one single RasterLayer file ~~~~ ----
# 
# List file names to be merged:  
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
LP90 <- list.files(pattern = "p90.asc")
LDNS <- list.files(pattern = "dns.asc")
LMAX <- list.files(pattern = "max.asc")
LAVG <- list.files(pattern = "avg.asc")
LSTD <- list.files(pattern = "std.asc")
LCOV <- list.files(pattern = "cov.asc")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Turn these files into a list (RasterStack) of RasterLayer objects 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
RP90 <- lapply(LP90, raster)
RDNS <- lapply(LDNS, raster)
RMAX <- lapply(LMAX, raster)
RAVG <- lapply(LAVG, raster)
RSTD <- lapply(LSTD, raster)
RCOV <- lapply(LCOV, raster)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Merge the RasterStack (list of raster files) into one RasterLayer
# (one single raster file) with 'do.call', sets projection
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
P90 <- do.call("merge",RP90)
DNS <- do.call("merge",RDNS)
MAX <- do.call("merge",RMAX)
AVG <- do.call("merge",RAVG)
STD <- do.call("merge",RSTD)
COV <- do.call("merge",RCOV)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set projection system
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
crs(P90) <- "+init=epsg:31982"
crs(DNS) <- "+init=epsg:31982"
crs(MAX) <- "+init=epsg:31982"
crs(AVG) <- "+init=epsg:31982"
crs(STD) <- "+init=epsg:31982"
crs(COV) <- "+init=epsg:31982"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Delete temporary variables from memory 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(LDNS, LP90, LMAX, LAVG, LSTD, LCOV, RDNS, RP90, RMAX, RAVG, RSTD, RCOV)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save the newly merged raster files
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
writeRaster(P90,"Merged_P90.asc")
writeRaster(DNS,"Merged_DNS.asc")
writeRaster(MAX,"Merged_MAX.asc")
writeRaster(AVG,"Merged_AVG.asc")
writeRaster(STD,"Merged_STD.asc")
writeRaster(COV,"Merged_COV.asc")
# Main processing block ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
#
# The raster file Volume ([MgC]/ha)
#    is calculated as a function of DNS and P90:
#
#    VOL = 25* ( exp(-8.8656 + 0.7588*ln(DNS) + 2.2169*ln(P90)) )
#
# ** The function was originally adjusted for volume of 400 m2 plots.
#    Therefore, VOL must be multiplied by  25  to represent MgC / ha.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
VOL <- overlay(DNS, P90, fun=function(x, y) {
  25*(exp(-8.8656 + 0.7588*log(x) +2.2169*log(y)))
}, filename ="q", overwrite = TRUE )
crs(VOL) <- "+init=epsg:31982"
writeRaster(VOL,"Merged_VOL.asc")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Two polygons are needed for the clipping of each AOI,
# <...>SHP and <...>BUF
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loops thru all AOIs
for(i in 7:nbr_AOIs) {
  if (i <= nbr_Areas) {
    AreaSHP  <- readOGR(str_c(projectShp, "/weFor_Area0", i, ".shp"))
    crs(AreaSHP) <-  "+init=epsg:31982"
    AreaBUF <- gBuffer(spgeom = AreaSHP, width = cell_size, byid = FALSE)
    crs(AreaBUF) <-  "+init=epsg:31982"
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores AOI name in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,1] <- str_c("Area0", i)
    
    # Volume ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # crop rasters previously created with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    VOLc <- crop(x = VOL, y = AreaBUF)
    VOLp <- rasterToPolygons(VOLc)
    crs(VOLp) <- "+init=epsg:31982"
    rm(VOLc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    VOLi <- intersect(VOLp, AreaSHP)
    crs(VOLi) <- "+init=epsg:31982"
    rm(VOLp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # >>>>>>>>>>>>>>>>>> sets vetor with proportions of the original cell areas in the grid
    # >>>>>>>>>>>>>>>>>>>>>>>>>>> stores total area in the results data frame (in hectares)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    PixelProp <- c(as.numeric(lapply(VOLi@polygons, slot, "area")) / cell_size)
    results[i,2] <- sum(PixelProp * cell_size) / 10000
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # polygons in the border have area smaller than cell_size m2. For instance,
    #   compare  VOLi@polygons[[1]]@area  with  VOLi@polygons[[100]]@area
    #   But, as expected, most of the pixels are cell_size m2 !!
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # summary(as.numeric(lapply(VOLi@polygons, slot, "area")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data table to get volumes and vector of volumes for the fixed pixels
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpVOL <- data.table(VOLi@data)
    PixelVols <- c((tmpVOL[, 1]$layer / 25) * PixelProp)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # >>>>>> stores total volume in results (as the sum of volumes from each "fixed" pixel)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,3] <- sum(PixelVols)
    
    # hist(tmpVOL[, 1], main=str_c("Area ", i), col="springgreen", xlab="VOL (Mg/ha)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(PixelVols * 25))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores  basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,4] <- tmpDF[,1]; results[i,5] <- tmpDF[,2]; results[i,6] <- tmpDF[,3]
    results[i,7] <- tmpDF[,4]; results[i,8] <- tmpDF[,5]; results[i,9] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(VOLi, projectShp, str_c("Area0", i, "_VOL"), driver="ESRI Shapefile")
    rm(VOLi, tmpVOL, tmpDF)
    
    # Percentil 90 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P90c <- crop(x = P90, y = AreaBUF)
    P90p <- rasterToPolygons(P90c)
    crs(P90p) <- "+init=epsg:31982"
    rm(P90c)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P90i <- intersect(P90p, AreaSHP)
    crs(P90i) <- "+init=epsg:31982"
    rm(P90p)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpP90 <- data.table(P90i@data)
    colnames(tmpP90)[1] <- "P90"
    head(tmpP90[, 1])
    # hist(tmpP90$P90, main=str_c("Area ", i), col="springgreen", xlab="P90 (m)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpP90$P90))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,10] <- tmpDF[,1]; results[i,11] <- tmpDF[,2]; results[i,12] <- tmpDF[,3]
    results[i,13] <- tmpDF[,4]; results[i,14] <- tmpDF[,5]; results[i,15] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(P90i, projectShp, str_c("Area0", i, "_P90"), driver="ESRI Shapefile")
    rm(P90i, tmpP90, tmpDF)
    
    # DNS: ALL returns above cover cutoff divided by ALL returns as percentage ~~~~~~~~~~~~ ------
    # 
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    DNSc <- crop(x = DNS, y = AreaBUF)
    DNSp <- rasterToPolygons(DNSc)
    crs(DNSp) <- "+init=epsg:31982"
    rm(DNSc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    DNSi <- intersect(DNSp, AreaSHP)
    crs(DNSi) <- "+init=epsg:31982"
    rm(DNSp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpDNS <- data.table(DNSi@data)
    colnames(tmpDNS)[1] <- "DNS"
    head(tmpDNS[, 1])
    # hist(tmpDNS$DNS, main=str_c("Area ", i), col="springgreen", xlab="DNS (%)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpDNS$DNS))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,16] <- tmpDF[,1]; results[i,17] <- tmpDF[,2]; results[i,18] <- tmpDF[,3]
    results[i,19] <- tmpDF[,4]; results[i,20] <- tmpDF[,5]; results[i,21] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(DNSi, projectShp, str_c("Area0", i, "_DNS"), driver="ESRI Shapefile")
    rm(DNSi, tmpDNS, tmpDF)
    
    # Maximum height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    MAXc <- crop(x = MAX, y = AreaBUF)
    MAXp <- rasterToPolygons(MAXc)
    crs(MAXp) <- "+init=epsg:31982"
    rm(MAXc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    MAXi <- intersect(MAXp, AreaSHP)
    crs(MAXi) <- "+init=epsg:31982"
    rm(MAXp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpMAX <- data.table(MAXi@data)
    colnames(tmpMAX)[1] <- "MAX"
    head(tmpMAX[, 1])
    # hist(tmpMAX$MAX, main=str_c("Area ", i), col="springgreen", xlab="MAX (m)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpMAX$MAX))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,22] <- tmpDF[,1]; results[i,23] <- tmpDF[,2]; results[i,24] <- tmpDF[,3]
    results[i,25] <- tmpDF[,4]; results[i,26] <- tmpDF[,5]; results[i,27] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(MAXi, projectShp, str_c("Area0", i, "_MAX"), driver="ESRI Shapefile")
    rm(MAXi, tmpMAX, tmpDF)
    
    # Mean height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    AVGc <- crop(x = AVG, y = AreaBUF)
    AVGp <- rasterToPolygons(AVGc)
    crs(AVGp) <- "+init=epsg:31982"
    rm(AVGc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    AVGi <- intersect(AVGp, AreaSHP)
    crs(AVGi) <- "+init=epsg:31982"
    rm(AVGp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpAVG <- data.table(AVGi@data)
    colnames(tmpAVG)[1] <- "AVG"
    head(tmpAVG[, 1])
    # hist(tmpAVG$AVG, main=str_c("Area ", i), col="springgreen", xlab="AVG (m)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpAVG$AVG))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,28] <- tmpDF[,1]; results[i,29] <- tmpDF[,2]; results[i,30] <- tmpDF[,3]
    results[i,31] <- tmpDF[,4]; results[i,32] <- tmpDF[,5]; results[i,33] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(AVGi, projectShp, str_c("Area0", i, "_AVG"), driver="ESRI Shapefile")
    rm(AVGi, tmpAVG, tmpDF)
    
    # Standard deviation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    STDc <- crop(x = STD, y = AreaBUF)
    STDp <- rasterToPolygons(STDc)
    crs(STDp) <- "+init=epsg:31982"
    rm(STDc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    STDi <- intersect(STDp, AreaSHP)
    crs(STDi) <- "+init=epsg:31982"
    rm(STDp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpSTD <- data.table(STDi@data)
    colnames(tmpSTD)[1] <- "STD"
    head(tmpSTD[, 1])
    # hist(tmpSTD$STD, main=str_c("Area ", i), col="springgreen", xlab="STD (m)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpSTD$STD))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,34] <- tmpDF[,1]; results[i,35] <- tmpDF[,2]; results[i,36] <- tmpDF[,3]
    results[i,37] <- tmpDF[,4]; results[i,38] <- tmpDF[,5]; results[i,39] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(STDi, projectShp, str_c("Area0", i, "_STD"), driver="ESRI Shapefile")
    rm(STDi, tmpSTD, tmpDF)
    
    # COV: FIRST returns above cover cutoff divided by ALL FIRST returns as percentage ~~~~ ----
    # 
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    COVc <- crop(x = COV, y = AreaBUF)
    COVp <- rasterToPolygons(COVc)
    crs(COVp) <- "+init=epsg:31982"
    rm(COVc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    COVi <- intersect(COVp, AreaSHP)
    crs(COVi) <- "+init=epsg:31982"
    rm(COVp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpCOV <- data.table(COVi@data)
    colnames(tmpCOV)[1] <- "COV"
    head(tmpCOV[, 1])
    # hist(tmpCOV$COV, main=str_c("Area ", i), col="springgreen", xlab="COV (%)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpCOV$COV))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,40] <- tmpDF[,1]; results[i,41] <- tmpDF[,2]; results[i,42] <- tmpDF[,3]
    results[i,43] <- tmpDF[,4]; results[i,44] <- tmpDF[,5]; results[i,45] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(COVi, projectShp, str_c("Area0", i, "_COV"), driver="ESRI Shapefile")
    rm(COVi, tmpCOV, tmpDF)
    
  } else if (i <= nbr_AOIs) {
    GapSHP <- readOGR(str_c(projectShp, "/weFor_Gap0", i - nbr_Areas, ".shp"))
    crs(GapSHP)  <-  "+init=epsg:31982"
    GapBUF <- gBuffer(spgeom = GapSHP, width = cell_size, byid = FALSE)
    crs(GapBUF) <-  "+init=epsg:31982"
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores AOI name in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,1] <- str_c("Gap0", i - nbr_Areas)
    
    # Volume ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    VOLc <- crop(x = VOL, y = GapBUF)
    VOLp <- rasterToPolygons(VOLc)
    crs(VOLp) <- "+init=epsg:31982"
    rm(VOLc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    VOLi <- intersect(VOLp, GapSHP)
    crs(VOLi) <- "+init=epsg:31982"
    rm(VOLp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # >>>>>>>>>>>>>>>>>> sets vetor with proportions of the original cell areas in the grid
    # >>>>>>>>>>>>>>>>>>>>>>>>>>> stores total area in the results data frame (in hectares)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    PixelProp <- c(as.numeric(lapply(VOLi@polygons, slot, "area")) / cell_size)
    results[i,2] <- sum(PixelProp * cell_size) / 10000
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # polygons in the border have area smaller than cell_size m2. For instance,
    #   compare  VOLi@polygons[[1]]@area  with  VOLi@polygons[[100]]@area
    #   But, as expected, most of the pixels are cell_size m2 !!
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # summary(as.numeric(lapply(VOLi@polygons, slot, "area")))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data table to get volumes and vector of volumes for the fixed pixels
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpVOL <- data.table(VOLi@data)
    PixelVols <- c((tmpVOL[, 1]$layer / 25) * PixelProp)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # >>>>>> stores total volume in results (as the sum of volumes from each "fixed" pixel)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,3] <- sum(PixelVols)
    
    # hist(tmpVOL$VOL, main=str_c("Gap ", i), col="springgreen", xlab="VOL (Mg/ha)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(PixelVols * 25))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores  basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,4] <- tmpDF[,1]; results[i,5] <- tmpDF[,2]; results[i,6] <- tmpDF[,3]
    results[i,7] <- tmpDF[,4]; results[i,8] <- tmpDF[,5]; results[i,9] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(VOLi, projectShp, str_c("Gap0", i - nbr_Areas, "_VOL"), driver="ESRI Shapefile")
    rm(VOLi, tmpVOL, tmpDF)
    
    # Percentil 90 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P90c <- crop(x = P90, y = GapBUF)
    P90p <- rasterToPolygons(P90c)
    crs(P90p) <- "+init=epsg:31982"
    rm(P90c)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    P90i <- intersect(P90p, GapSHP)
    crs(P90i) <- "+init=epsg:31982"
    rm(P90p)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpP90 <- data.table(P90i@data)
    colnames(tmpP90)[1] <- "P90"
    head(tmpP90[, 1])
    # hist(tmpP90$P90, main=str_c("Gap ", i - nbrAreas), col="springgreen", xlab="P90 (m)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpP90$P90))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores  basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,10] <- tmpDF[,1]; results[i,11] <- tmpDF[,2]; results[i,12] <- tmpDF[,3]
    results[i,13] <- tmpDF[,4]; results[i,14] <- tmpDF[,5]; results[i,15] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(P90i, projectShp, str_c("Gap0", i - nbr_Areas, "_P90"), driver="ESRI Shapefile")
    rm(P90i, tmpP90, tmpDF)
    
    # DNS: ALL returns above cover cutoff divided by ALL returns as percentage ~~~~~~~~~~~~ ------
    # 
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    DNSc <- crop(x = DNS, y = GapBUF)
    DNSp <- rasterToPolygons(DNSc)
    crs(DNSp) <- "+init=epsg:31982"
    rm(DNSc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    DNSi <- intersect(DNSp, GapSHP)
    crs(DNSi) <- "+init=epsg:31982"
    rm(DNSp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpDNS <- data.table(DNSi@data)
    colnames(tmpDNS)[1] <- "DNS"
    head(tmpDNS[, 1])
    # hist(tmpDNS$DNS, main=str_c("Gap ", i - nbrAreas), col="springgreen", xlab="DNS (%)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpDNS$DNS))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores  basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,16] <- tmpDF[,1]; results[i,17] <- tmpDF[,2]; results[i,18] <- tmpDF[,3]
    results[i,19] <- tmpDF[,4]; results[i,20] <- tmpDF[,5]; results[i,21] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(DNSi, projectShp, str_c("Gap0", i - nbr_Areas, "_DNS"), driver="ESRI Shapefile")
    rm(DNSi, tmpDNS, tmpDF)
    
    # Maximum height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    MAXc <- crop(x = MAX, y = GapBUF)
    MAXp <- rasterToPolygons(MAXc)
    crs(MAXp) <- "+init=epsg:31982"
    rm(MAXc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    MAXi <- intersect(MAXp, GapSHP)
    crs(MAXi) <- "+init=epsg:31982"
    rm(MAXp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpMAX <- data.table(MAXi@data)
    colnames(tmpMAX)[1] <- "MAX"
    head(tmpMAX[, 1])
    # hist(tmpMAX$MAX, main=str_c("Gap ", i - nbrAreas), col="springgreen", xlab="MAX (m)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpMAX$MAX))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores  basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,22] <- tmpDF[,1]; results[i,23] <- tmpDF[,2]; results[i,24] <- tmpDF[,3]
    results[i,25] <- tmpDF[,4]; results[i,26] <- tmpDF[,5]; results[i,27] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(MAXi, projectShp, str_c("Gap0", i - nbr_Areas, "_MAX"), driver="ESRI Shapefile")
    rm(MAXi, tmpMAX, tmpDF)
    
    # Mean height ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    AVGc <- crop(x = AVG, y = GapBUF)
    AVGp <- rasterToPolygons(AVGc)
    crs(AVGp) <- "+init=epsg:31982"
    rm(AVGc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    AVGi <- intersect(AVGp, GapSHP)
    crs(AVGi) <- "+init=epsg:31982"
    rm(AVGp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpAVG <- data.table(AVGi@data)
    colnames(tmpAVG)[1] <- "AVG"
    head(tmpAVG[, 1])
    # hist(tmpAVG$AVG, main=str_c("Gap ", i - nbrAreas), col="springgreen", xlab="AVG (m)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpAVG$AVG))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores  basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,28] <- tmpDF[,1]; results[i,29] <- tmpDF[,2]; results[i,30] <- tmpDF[,3]
    results[i,31] <- tmpDF[,4]; results[i,32] <- tmpDF[,5]; results[i,33] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(AVGi, projectShp, str_c("Gap0", i - nbr_Areas, "_AVG"), driver="ESRI Shapefile")
    rm(AVGi, tmpAVG, tmpDF)
    
    # Standard deviation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
    # 
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    STDc <- crop(x = STD, y = GapBUF)
    STDp <- rasterToPolygons(STDc)
    crs(STDp) <- "+init=epsg:31982"
    rm(STDc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    STDi <- intersect(STDp, GapSHP)
    crs(STDi) <- "+init=epsg:31982"
    rm(STDp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpSTD <- data.table(STDi@data)
    colnames(tmpSTD)[1] <- "STD"
    head(tmpSTD[, 1])
    # hist(tmpSTD$STD, main=str_c("Gap ", i - nbrAreas), col="springgreen", xlab="STD (m)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpSTD$STD))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores  basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,34] <- tmpDF[,1]; results[i,35] <- tmpDF[,2]; results[i,36] <- tmpDF[,3]
    results[i,37] <- tmpDF[,4]; results[i,38] <- tmpDF[,5]; results[i,39] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(STDi, projectShp, str_c("Gap0", i - nbr_Areas, "_STD"), driver="ESRI Shapefile")
    rm(STDi, tmpSTD, tmpDF)
    
    # COV: FIRST returns above cover cutoff divided by ALL FIRST returns as percentage ~~~~ ----
    #  
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # crop rasters previously created for the metric with a buffered polygon around the AOI
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    COVc <- crop(x = COV, y = GapBUF)
    COVp <- rasterToPolygons(COVc)
    crs(COVp) <- "+init=epsg:31982"
    rm(COVc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # clips the buffered raster with the unbuffered AOI polygon
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    COVi <- intersect(COVp, GapSHP)
    crs(COVi) <- "+init=epsg:31982"
    rm(COVp)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create temporary data tables to work on the attributes
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    tmpCOV <- data.table(COVi@data)
    colnames(tmpCOV)[1] <- "COV"
    head(tmpCOV[, 1])
    # hist(tmpCOV$COV, main=str_c("Gap ", i - nbrAreas), col="springgreen", xlab="COV (%)", ylab="Pixels")
    tmpDF <- data.frame(rbind(as.table(summary(tmpCOV$COV))))
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # stores  basic statistics in the results data frame
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    results[i,40] <- tmpDF[,1]; results[i,41] <- tmpDF[,2]; results[i,42] <- tmpDF[,3]
    results[i,43] <- tmpDF[,4]; results[i,44] <- tmpDF[,5]; results[i,45] <- tmpDF[,6]
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # save map in shape format
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    writeOGR(COVi, projectShp, str_c("Gap0", i - nbr_Areas, "_COV"), driver="ESRI Shapefile")
    rm(COVi, tmpCOV, tmpDF)
    
  }
}

write.csv(results, file = str_c(projectDir, "/Assessment/weForestAssessment2.csv"), row.names = FALSE)
print(results)
rm(list=ls(all=TRUE))                                  # Clean memory
