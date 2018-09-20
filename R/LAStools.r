if(!require(stringr)){
  install.packages('stringr')
  require(stringr)
}

# Project folder
projectDir  = "C:/GoogleDriveUSP/classification/"
setwd(projectDir)
cores = 3

LAStool = function(tool, inputFile, ...){
  cmd = paste(paste(tool, sep=''), '-i', inputFile , ...) 
  cat(cmd)
  system(cmd)
  return(cmd)
}

replaceFiles = function(mainDir, tempDir, isWildCard = T){
  if(isWildCard){
    mainDir = sub('/\\*\\..+', '', mainDir)
  }
  
  oldFiles = dir(mainDir, full.names = T)
  unlink(oldFiles, recursive = F, force = T)
  
  oldFiles = dir(mainDir, full.names = T, recursive = T)
  newNames = sub(tempDir, mainDir, oldFiles)
  file.rename(oldFiles, newNames)
  
  unlink(tempDir, recursive = T, force = T)
}

# Sets cell size for calculation of LiDAR metrics and 
#     cell area for pixel size in raster maps
hectare = 10000
cell_size = 2
cell_area = cell_size * cell_size

# Sets parameters for graphs - font size
txt.size = 7
thm.size = (14/5) * txt.size
# Sets parameters for graphs - function that calculates mean
fun_mean <- function(x){
        return(round(data.frame(y=mean(x),label=mean(x,na.rm=T)),2))
  }

# Paths to LiDAR clouds and RESULT directory
cloudPath = ''
resultDir = 'processing'
dir.create(resultDir, showWarnings = F)

setwd(cloudPath)


# Check integrity
inDir  = '*.las'
outDir = '0.check'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasvalidate', inDir,
                 '-no_CRS_fail',
                 '-o', str_c(outDir, '/Validation.xml'))

# Check flightlines
LASrun = LAStool('lasoverlap', inDir,
                 '-merged',
                 '-epsg 31982',
                 '-target_epsg 31982',
                 '-step 1.0',
                 '-max_diff 1.0',
                 '-odir', outDir,
                 '-odix _flt',
                 '-opng -v')

# Create info
outDir = '0.info'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasinfo', inDir,
                 '-cores', cores,
                 '-cd',
                 '-histo scan_angle 3',
                 '-odir', outDir,
                 '-odix _info',
                 '-otxt -v')

# Reset clouds
outDir = '1.lazin'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('las2las', inDir,
                 '-cores', cores,
                 '-epsg 31982',
                 '-target_epsg 31982',
                 '-meter',
                 '-target_meter',
                 '-clip_to_bounding_box',
                 '-rescale 0.01 0.01 0.01',
                 '-repair_zero_returns',
                 '-set_classification 0',
                 '-odir', outDir,
                 '-olaz -v')

# Delete duplicate points
# Removes all xyz duplicates and stores only the xyz unique points
inDir  = '1.lazin/*.laz'
outDir = '1.lazin/temp'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasduplicate', inDir,
                 '-cores', cores,
                 '-unique_xyz',
                 '-odir', outDir,
                 '-olaz -v')

replaceFiles(inDir, outDir)

# Retile with buffers
#
# Buffers are necessary when a temporary TIN is internally created by 
# las2dem or blast2dem (that is then rastered at the user-specified 
# step size onto a grid); when classifying points with lasground or
# lasclassify; when calculating height above ground or 
# height-normalizing LiDAR tiles with lasheight; when removing noise
# with lasnoise; when creating contours with las2iso or blast2iso;
# or any other operation where an incomplete neighborhood of points
# can affect the results.
# Source: http://rapidlasso.com/2015/08/07/use-buffers-when-processing-lidar-in-tiles/
#
# All LAStools (such as lasground, lasground_new, lasnoise, lasduplicate, ... )
# also operate on all points flagged as 'withheld' unless they are explicitly
# instructed to drop those points (aka '-drop_withheld'). The reason to add
# the '-flag_as_withheld' option during the initial tiling step with lastile
# is so these buffer points can later easily be dropped when merging multiple
# tiles into one without needing an explicit step to '-remove_buffer' with
# lastile thereby avoiding yet another copy of the data.
# Source: https://groups.google.com/d/msg/lastools/BpiRJP62zZ8/U5EBp0WuBgAJ

inDir  = '1.lazin/*.laz'
outDir = '2.tiles'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lastile', inDir,
                 '-epsg 31982',
                 '-target_epsg 31982',
                 '-meter',
                 '-target_meter',
                 '-tile_size 1000',
                 '-buffer 30',
                 '-flag_as_withheld',
                 '-odix _tile',
                 '-odir', outDir,
                 '-olaz -v')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optimize >> Used before tiling: FAILS to alloc buffer for x points of size 34  <<
#
# (1) remove fluff in coordinate resolution (i.e. when all X, Y or Z coordinates
# are multiples of 10, 100, or 1000); (2) remove any additional padding in the
# LAS header or before the point block; (3) set nicely rounded offsets in the
# LAS header; (4) zero the contents of the user data field; (5) turn
# (sufficiently small) EVLRs into VLRs (LAS 1.4 only); (6) rearrange points
# for better compression and spatial indexing 
inDir  = '2.tiles/*.laz'
outDir = '2.tiles/temp'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasoptimize', inDir,
                 '-cores', cores,
                 '-odir', outDir,
                 '-olaz -v')

replaceFiles(inDir, outDir)

inDir  = '2.tiles/*.laz'
outDir = '2.tiles/info'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasinfo', inDir,
                 '-cores', cores,
                 '-odir', outDir,
                 '-odix _info',
                 '-otxt',
                 '-cd -v')

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# classify ground points
inDir  = '2.tiles/*.laz'
outDir = '3.ground'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasground', inDir,
                 '-cores', cores,
                 '-wilderness',       # using default step ... '-step 5'
                 '-compute_height',
                 '-odir', outDir,
                 '-odix _g',
                 '-olaz -v')

# Classify non-ground points
inDir  = '3.ground/*.laz'
outDir = '4.classf'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasclassify', inDir,
                 '-cores', cores,
                 '-step 1',
                 '-ground_offset 0.5',
                 '-odir', outDir,
                 '-odix _c',
                 '-olaz -v')

inDir  = '4.classf/*.laz'
outDir = '4.classf/info'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasinfo', inDir,
                 '-cores', cores,
                 '-odir', outDir,
                 '-odix _info',
                 '-otxt',
                 '-cd -v')

# Denoise tiles
inDir  = '4.classf/*.laz'
outDir = '5.bufnon'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasnoise', inDir,
                 '-cores', cores,
                 '-ignore_class 2',
                 '-step 4',
                 '-isolated 5',
                 '-odir', outDir,
                 '-odix _d',
                 '-olaz -v')

# Unbuffer tiles and creates one laz for DTM, DSM and DEM of Total Area
inDir  = '5.bufnon/*.laz'
outDir = '6.unbnon'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lastile',inDir,'-cores',cores,'-remove_buffer -odir',outDir,'-odix _u -olaz -v')

# inDir  = '6.unbnon/*.laz'
# inShp  = str_c(projectDir, 'SHAPES/TOTAREA/LiDAR2015_TOTAREA.shp')
# outDir = '6.unbnon/TOTAREA'
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('lasclip',inDir,'-merged -poly',inShp,'-odir',outDir,'-o Ipe2016_TOTALAREA.laz -olaz -v')

# Normalize tiles
inDir  = '5.bufnon/*.laz'
outDir = '7.bufnrm'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasheight', inDir,
                 '-cores', cores,
                 '-replace_z',
                 '-set_user_data 0',
                 '-odir', outDir,
                 '-odix _n',
                 '-olaz -v')

# Remove buffer from normalized tiles and >> MAINTAIN ONLY TOTAREA TILES <<
inDir  = '7.bufnrm/*.laz'  # Unbuffer normalized tiles
outDir = '8.unbnrm'
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lastile',inDir,'-cores',cores,'-remove_buffer -odir',outDir,'-odix _u -olaz -v')

# inDir  = str_c(cloudPath,  '8.unbnrm/*.laz') #  Maintain only TOTAREA tiles
# inShp  = str_c(projectDir, 'SHAPES/TOTAREA/LiDAR2015_TOTAREA.shp')
# outDir = str_c(cloudPath,  '8.unbnrm/TOTAREA')
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('lasclip',inDir,'-poly',inShp,'-odir',outDir,'-olaz -v')

# Merge normalized tiles and clips AOI for metrics calculation
# inDir  = str_c(cloudPath,  '8.unbnrm/*.laz')
# inShp  = str_c(projectDir, 'SHAPES/Estratos/Ipe.shp')
# outDir = str_c(cloudPath,  '9.clippd')
# dir.create(outDir, showWarnings = F)
# outDir = str_c(cloudPath,  '9.clippd/normalized')
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('lasclip', inDir,
#                  '-merged',
#                  '-poly', inShp,
#                  '-split NameShort',
#                  '-odir', outDir,
#                  '-olaz',
#                  '-v')

# Merge non normalized tiles and clips AOI for DTM, DSM and DEM
# inDir  = str_c(cloudPath,  '6.unbnon/*.laz')
# inShp  = str_c(projectDir, 'SHAPES/Estratos/Ipe.shp')
# outDir = str_c(cloudPath,  '9.clippd')
# dir.create(outDir, showWarnings = F)
# outDir = str_c(cloudPath,  '9.clippd/notnormlzd')
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('lasclip', inDir,
#                  '-merged',
#                  '-poly', inShp,
#                  '-split NameShort',
#                  '-odir', outDir,
#                  '-olaz',
#                  '-v')

# 03. Create DIGITAL MODELS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# Create a DTM image for the Total Area
inDir  = '6.unbnon/*.laz'
outDir = str_c(resultDir, '/DTM')
dir.create(outDir, showWarnings = F)
outDir = str_c(outDir, '/TOTAREA')
dir.create(outDir, showWarnings = F)
LASrun = LAStool('blast2dem', inDir,
                 '-keep_class 2',
                 '-step 1',
                 '-elevation',
                 '-hillshade',
                 '-odir', outDir,
                 '-odix _dtm',
                 '-opng -v')


# Create a DSM image for the Total Area
inDir  = '6.unbnon/*.laz'
outDir = str_c(resultDir, '/DSM')
dir.create(outDir, showWarnings = F)
outDir = str_c(outDir, '/TOTAREA')
dir.create(outDir, showWarnings = F)
LASrun = LAStool('blast2dem', inDir,
                 '-keep_class 2 5',
                 '-step 1',
                 '-elevation',
                 '-hillshade',
                 '-odir', outDir,
                 '-odix _dsm',
                 '-opng -v')

# Create a DEM image for tree heights (false coloring) for the Total Area
inDir  = '6.unbnon/*.laz'
outDir = str_c(resultDir, '/DEM')
dir.create(outDir, showWarnings = F)
outDir = str_c(outDir, '/TOTAREA')
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lasgrid', inDir,
                 '-user_data',
                 '-step 1',
                 '-false',
                 '-odir', outDir,
                 '-odix _dem',
                 '-opng -v')

# Create a DTM image for each polygon
# inDir  = str_c(cloudPath, '9.clippd/notnormlzd/*.laz')
# outDir = str_c(resultDir, '/DTM')
# dir.create(outDir, showWarnings = F)
# outDir = str_c(outDir, '/AOI')
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('blast2dem', inDir,
#                  '-keep_class 2',
#                  '-step 1',
#                  '-elevation',
#                  '-hillshade',
#                  '-odir', outDir,
#                  '-odix _dtm',
#                  '-opng -v')

# Create a DSM image for each polygon
# inDir  = str_c(cloudPath, '9.clippd/notnormlzd/*.laz')
# outDir = str_c(resultDir, '/DSM')
# dir.create(outDir, showWarnings = F)
# outDir = str_c(outDir, '/AOI')
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('blast2dem', inDir,
#                  '-keep_class 2 5',
#                  '-step 1',
#                  '-elevation',
#                  '-hillshade',
#                  '-odir', outDir,
#                  '-odix _dsm',
#                  '-opng -v')

# Create a DEM image for tree heights (false coloring) of each polygon
# inDir  = str_c(cloudPath, '9.clippd/notnormlzd/*.laz')
# outDir = str_c(resultDir, '/DEM')
# dir.create(outDir, showWarnings = F)
# outDir = str_c(outDir, '/AOI')
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('lasgrid', inDir,
#                  '-user_data',
#                  '-step 1',
#                  '-false',
#                  '-odir', outDir,
#                  '-odix _dem',
#                  '-opng -v')

# UNNECESSARY >>>> Converts and merges all polygon LAZ files into one 
#    shapefile using MultiPointZ records containing 2048 points each.
#    Check if polygon of LiDAR points corresponds to shapes of the AOI
#    2017 LiDAR data DOES NOT cover the whole area in all polygons
#    Seems that areas 02 and 06 are not fully covered with LiDAR data.
#    QGIS Convex Hull might help converting points to polygon (need field)
# inDir  = str_c(cloudPath, '9.unbclp/*.laz')
# outDir = str_c(cloudPath, '9.unbclp/shape')
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('las2shp', inDir,
#                  '-merged',
#                  '-record 2048',
#                  '-odir', outDir,
#                  '-o Ipe2017.shp -v')
# Calculate LiDAR metrics for the sample plots with LAStools ~~~~~~~~~~~

# Clip sample plots from unbuffered and normalized tiles in folder "unbnrm"
# inDir  = str_c(cloudPath, '9.clippd/normalized/*.laz')
# inShp  = str_c(projectDir, 'SHAPES/Parcelas/Plots_400m2.shp')
# outDir = str_c(cloudPath, 'A.plots')
# dir.create(outDir, showWarnings = F)
# runLAStool = LAStool('lasclip', inDir,
#                      '-merged',
#                      '-poly', inShp,
#                      '-split PlotName', # SHP field for naming LAZ files
#                      '-odir', outDir,
#                      '-olaz -v')					 					 

# 04. LiDAR: METRICS CALCULATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# Calculate metrics for the normalized cloud in the total area w/ 100m buffer
#
# IMPORTANT (consider changing the following reference heights)!!
# *********
#   Reference height for the calculation of
#   percentiles, bincentiles, min, max, avg, std, ske, kur, qav
#   ---> height_cutoff: 0.5 m
#   Reference height for canopy cover and canopy density metrics
#   ---> cover_cutoff:  0.5 m
# IMPORTANT (reconsider dropping unclassified returns and cell size)!!
# *********
#   drop_class 1 7
#   cell_size = 5
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# One single CSV file with all metrics by raster cell for TOTAREA LiDAR
inDir  = '8.unbnrm/*.laz'
LASrun = LAStool('lascanopy', inDir,
                 '-merged',
                 '-keep_z 0.0 35.0 -drop_class 1 7',
                 '-step ', cell_size,
                 '-centroids',
                 '-grid_ll 10 10',
                 '-height_cutoff 0.5',
                 '-cover_cutoff  0.5',
                 '-max -avg -std',
                 '-cov',  # canopy cover (%) first returns above the cover cutoff / all first returns
                 '-dns',  # (%) all returns above the cover cutoff   / all returns
                 '-b 25 50 75', # points in the lower 25% "voxel" / "points in the voxel" {voxel: column with height = maximum - cutoff}
                 '-c 0.5 3.5 15.0 35.0', # amount of points whose heights fall into the intervals
                 '-d 0.5 3.5 15.0 35.0', # (%) points with heights into the intervals / total
                 '-p 10 25 33 50 67 75 90 95',       # percentiles 25, 50, 75 and 90
                 '-odir', resultDir,
                 '-odix _mts',
                 '-ocsv -v')

# > Creation of the directory and raster files for the IMPORTANT RESULTS
inDir  = '8.unbnrm/*.laz'
outDir = str_c(resultDir, '/ASC')
dir.create(outDir, showWarnings = F)
LASrun = LAStool('lascanopy', inDir,
                 '-merged',
                 '-keep_z 0.0 35.0 -drop_class 1 7',
                 '-step ', cell_size,
                 '-grid_ll 10 10',
                 '-height_cutoff 0.5',
                 '-cover_cutoff  0.5',
                 '-max -avg -std',
                 '-cov',  # canopy cover (%) first returns above the cover cutoff / all first returns
                 '-dns',  # (%) all returns above the cover cutoff   / all returns
                 '-b 25 50 75', # points in the lower 25% "voxel" / "points in the voxel" {voxel: column with height = maximum - cutoff}
                 '-c 0.5 3.5 15.0 35.0', # amount of points whose heights fall into the intervals
                 '-d 0.5 3.5 15.0 35.0', # (%) points with heights into the intervals / total
                 '-p 10 25 33 50 67 75 90 95',       # percentiles 25, 50, 75 and 90
                 '-odir', outDir,
                 '-odix _mts',
                 '-oasc -v')

# Calculate LiDAR metrics for the clipped sample plots
#     Input files are plots, then use -files_are_plots and -names
#     options to have the file names added to the output CSV file.
# inDir  = str_c(cloudPath, 'A.plots/*.laz')
# outDir = resultDir
# dir.create(outDir, showWarnings = F)
# LASrun = LAStool('lascanopy', inDir,
#                  '-keep_z 0.0 35.0 -drop_class 1 7',
#                  '-files_are_plots',
#                  '-names',
#                  '-height_cutoff 0.5',
#                  '-cover_cutoff  0.5',
#                  '-max -avg -std',
#                  '-cov',  # canopy cover (%) first returns above the cover cutoff / all first returns
#                  '-dns',  # (%) all returns above the cover cutoff   / all returns
#                  '-b 25 50 75', # points in the lower 25% "voxel" / "points in the voxel" {voxel: column with height = maximum - cutoff}
#                  '-c 0.5 3.5 15.0 35.0', # amount of points whose heights fall into the intervals
#                  '-d 0.5 3.5 15.0 35.0', # (%) points with heights into the intervals / total
#                  '-p 25 50 75 90',       # percentiles 25, 50, 75 and 99
#                  '-odir', outDir,
#                  '-o Ipe2017_PLOT_metrics.csv',
#                  '-oasc -v')

rm(cloudPath, cores)

# 05. Initial settings for creation of maps with nice edges ~~~~~~~~~~~~ ----
if(!require(raster)){
        install.packages('raster')
        require(raster)
}
if(!require(sp)){
        install.packages('sp')
        require(sp)
}
# if(!require(rgeos)){
#         install.packages('rgeos')
#         require(rgeos)
# }
if(!require(rgdal)){
        install.packages('rgdal')
        require(rgdal)
}
# if(!require(data.table)){
#         install.packages('data.table')
#         require(data.table)
# }

# Lists of raster files (ASC) for each metric to be fetched ~~~~~~~~~~~~
# >>>>>> raster files are fetched in the directory for IMPORTANT RESULTS

ascDir = str_c(resultDir, "/ASC")
rasterFiles = dir(ascDir, pattern = '\\.asc$')

# Creates a RasterLayer for each metric ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# If more than one ASC file for each metric:
#  turns files into a list of RasterLayer objects (RasterStack) and
#  merges the RasterStack (list of raster files) into one RasterLayer
#  (one single raster file) with 'do.call' ... and sets projection.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd(ascDir)

metricsList = unique(sub('.*_(.*)\\.asc$' ,'\\1',rasterFiles))

mosaicList = sapply(metricsList, function(x){
  
  metricMosaic = grep(x, rasterFiles)
  metricMosaic = lapply(rasterFiles[metricMosaic], raster)
  
  if(length(metricMosaic) > 1){
    metricMosaic$fun = mean
    metricMosaic$na.rm = T
    
    metricMosaic = do.call(mosaic ,  metricMosaic)    
  }else{
    metricMosaic = metricMosaic[[1]]
  }

  fileName = str_c(x, '.asc')
  writeRaster(metricMosaic, fileName, overwrite=T)
  
  return(fileName)
  
})

layerStack = raster::stack(mosaicList)
crs(layerStack) = "+init=epsg:32723"

# Main processing block ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The raster file CVR is for the coeficient of variation in each cell
#    and depends on STD and AVG
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CVR = overlay(layerStack$std, layerStack$avg, fun=function(x, y) {
  return(round((x/y*100),digits=4))
})

# The raster file AGC (MgC/ha)
#   is calculated as a function of DNS and P90:
#   AGC = k * ( exp(-8.8656 + 0.7588*ln(DNS) + 2.2169*ln(P90)) )
#     k = 10000/cell_area         (= 25 for 400 m2 plots)
# ** The AGC function was adjusted for 400 m2 plots, then
#        AGC multiplied by k = 25 makes the pixel express MgC/ha.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
k = hectare / cell_area
AGC = overlay(layerStack$dns, layerStack$p90, fun=function(x, y) {
  return(round(k*(exp(-8.8656+0.7588*log(x)+2.2169*log(y))), digits=4))
})

layerStack = addLayer(layerStack, CVR, AGC)
names(layerStack)[ c( nlayers(layerStack)-1 , nlayers(layerStack) ) ] = c('cvr', 'agc')

layerFrame = as.data.frame(layerStack, xy=T)

rasPoly = SpatialPixelsDataFrame(layerFrame[,1:2], layerFrame)
rasPoly = as(rasPoly, "SpatialPolygonsDataFrame")
crs(rasPoly) = crs(layerStack)

# Path for the directory where the resulting maps will be stored
mapDir = '../MAP'
dir.create(mapDir, showWarnings = F)
setwd(mapDir)

# AreaSHP  = readOGR(shpFiles[i])
# rasPoly = crop(x = rasPoly, y = AreaSHP)
writeOGR(rasPoly, 'map.shp', 'map', overwrite_layer=TRUE, driver="ESRI Shapefile")

rm(ascDir)

# ----

# 07. Initial settings for data manipulation and graphS preparation ~~~~ ----
# install.packages("tidyverse") >> CALL TO THIS PACKAGE MUST REMAIN HERE 
if(!require(tidyverse)){
        install.packages('tidyverse')
        require(tidyverse)
}

# 08. TOTAL AREA: READ  LiDAR METRICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# Read 2017 CSV file with LiDAR metrics per pixel for total area ~~~~~~~
inDir = str_c(projectDir, 'RESULT/2017')
TAreMet2017 <- read_csv(str_c(inDir, '/Ipe2017_TOTAREA_metrics.csv'), na = c("-", "NA"))
TAreMet2017$Year <- 2017                 # Add field Year
# Read 2016 CSV file with LiDAR metrics per pixel for total area ~~~~~~~
inDir = str_c(projectDir, 'RESULT/2016')
TAreMet2016 <- read_csv(str_c(inDir, '/Ipe2016_TOTAREA_metrics.csv'), na = c("-", "NA"))
TAreMet2016$Year <- 2016                 # Add field Year
# Read 2015 CSV file with LiDAR metrics per pixel for total area ~~~~~~~
inDir = str_c(projectDir, 'RESULT/2015')
TAreMet2015 <- read_csv(str_c(inDir, '/Ipe2015_TOTAREA_metrics.csv'), na = c("-", "NA"))
TAreMet2015$Year <- 2015                 # Add field Year
# Merge multi temporal LiDAR metrics into one single dataframe ~~~~~~~~~
TAreMetrics <-as.tibble(
        bind_rows(TAreMet2015, TAreMet2016, TAreMet2017, .id = NULL)
)
rm(TAreMet2015, TAreMet2016, TAreMet2017)
TAreMetrics <- TAreMetrics %>%        # Sets Year as first column
        select(Year, everything())    # Year and Plot are the unique key
TAreMetrics$Year     <- as.factor(as.character(TAreMetrics$Year))

# 09. TOTAL AREA: GRAPH LiDAR METRICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---- 
# Histograms ... TAKES LONG TIME TO RENDER ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (i in c("2015", "2016", "2017")) {
        # max and avg
        tmpDF <- filter(TAreMetrics, Year == i)
        tmpDF <- select(tmpDF, Year, index, max, avg, std, p50, p90, dns, d02)
        StatMAX <- summarize(tmpDF, count = n(), mean = mean(max, na.rm = TRUE))
        StatAVG <- summarize(tmpDF, count = n(), mean = mean(avg, na.rm = TRUE))
        TitleString  = str_c("Area Total: MAX e AVG (", i,")")
        LabelString1 = round(StatAVG$mean,2)
        LabelString2 = round(StatMAX$mean,2)
        jpeg(str_c(projectDir, "IMAGES/JPG/Ipe",i,"_EA_MAXAVG.jpg"), width = 800, height = 400, units = "px", quality = 100)
        plot(
        ggplot(data = tmpDF) +
        geom_histogram(mapping = aes(x = max, fill="m"),
                       breaks=seq(0, 34, by=1),
                       col = "black",
                       alpha = 1) +
        geom_histogram(mapping = aes(x = avg, fill="a"),
                       breaks=seq(0, 34, by=1), 
                       col = "black",
                       alpha = .7) +
        scale_fill_manual(name=NULL, values=c("m" = "blue", "a"="red"),
                          labels=c("m"="maximum", "a"="average")) +
        labs(title=TitleString, x="Altura (m)", y="Count") +
        scale_x_continuous(breaks=seq(0,42,2)) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.x = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold"),
                panel.ontop = FALSE
                ) +
        geom_vline(mapping = aes(xintercept = StatAVG$mean), col='red2', size=1) +
        geom_text(mapping  = aes(label=LabelString1, y=12000, x=1.8+StatAVG$mean),
                  col = 'red2',
                  size=txt.size) +
        geom_vline(mapping = aes(xintercept = StatMAX$mean), col='blue2', size=1) +
        geom_text(mapping  = aes(label=LabelString2, y=14000, x=1.8+StatMAX$mean),
                  col = 'blue2',
                  size=txt.size))
        dev.off()
        # percentiles
        StatP50 <- summarize(tmpDF, count = n(), mean = mean(p50, na.rm = TRUE))
        StatP90 <- summarize(tmpDF, count = n(), mean = mean(p90, na.rm = TRUE))
        TitleString = str_c("Area Total: P50 e P90 (", i,")")
        LabelString1 = round(StatP50$mean,2)
        LabelString2 = round(StatP90$mean,2)
        jpeg(str_c(projectDir, "IMAGES/JPG/Ipe",i,"_EA_P50P90.jpg"), width = 800, height = 400, units = "px", quality = 100)
        plot(
        ggplot(data = tmpDF) +
        geom_histogram(mapping = aes(x = p50, fill="p1"),
                       breaks=seq(0, 28, by=1), 
                       col = "black",
                       alpha = 1) +
        geom_histogram(mapping = aes(x = p90, fill="p2"),
                       breaks=seq(0, 28, by=1),
                       col = "black",
                       alpha = .7) +
        scale_fill_manual(name=NULL, values=c("p1" = "blue", "p2"= "red"),
                          labels=c("p1" = "P50", "p2"="P90")) +
        labs(title=TitleString, x="Altura (m)", y="Count") +
        scale_x_continuous(breaks=seq(0,28,2))+
        scale_y_continuous(breaks=seq(0,28000,4000))+
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.x = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold"),
                panel.ontop = FALSE
        ) +
        geom_vline(mapping = aes(xintercept = StatP50$mean), col='blue2', size=1) +
        geom_text(mapping  = aes(label=LabelString1, y=10000, x=1.8+StatP50$mean),
                  col = 'blue2',
                  size=txt.size) +
        geom_vline(mapping = aes(xintercept = StatP90$mean), col='red2', size=1) +
        geom_text(mapping  = aes(label=LabelString2, y=12000, x=1.8+StatP90$mean),
                  col = 'red2',
                  size=txt.size))
        dev.off()
        # canopy density
        StatDNS <- summarize(tmpDF, count = n(), mean = mean(dns, na.rm = TRUE))
        TitleString = str_c("Area Total: DNS canopy density (", i,")")
        LabelString = str_c("Mean = ", round(StatDNS$mean,2))
        jpeg(str_c(projectDir, "IMAGES/JPG/Ipe",i,"_EA_DNS.jpg"), width = 800, height = 400, units = "px", quality = 100)
        plot(
        ggplot(data = tmpDF) +
        geom_histogram(mapping = aes(x = dns, fill="dn"),
                       breaks=seq(0, 100, by=5), 
                       col = "black",
                       alpha = 1) +
        scale_fill_manual(name=NULL, values=c("dn"="gray"),
                          labels=c("dn"="Pts>1.30 / All_pts")) +
        labs(title=TitleString, x="% of points", y="Count") +
        scale_x_continuous(breaks=seq(0,100,10)) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.x = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold"),
                panel.ontop = FALSE
        ) +
        geom_vline(mapping = aes(xintercept = StatDNS$mean), col='gray', size=1) +
        geom_text(mapping  = aes(label=LabelString, y=10000, x=5+StatDNS$mean),
                  col = 'gray',
                  size=txt.size))
        dev.off()
        # D02: % in [15-35m]
        StatD02 <- summarize(tmpDF, count = n(), mean = mean(d02, na.rm = TRUE))
        TitleString = str_c("Area Total: D02 15-30m (", i,")")
        LabelString = str_c("Mean = ", round(StatD02$mean,2))
        jpeg(str_c(projectDir, "IMAGES/JPG/Ipe",i,"_EA_D02.jpg"), width = 800, height = 400, units = "px", quality = 100)
        plot(
        ggplot(data = tmpDF) +
        geom_histogram(mapping = aes(x = d02, fill="d2"),
                       breaks=seq(0.01, 66.01, by=4), 
                       col = "black",
                       alpha = 1) +
        scale_fill_manual(name=NULL, values=c("d2"="salmon4"),
                          labels=c("d2"="[Pts 15-35m]/All_Pts")) +
        labs(title=TitleString, x="% of points", y="Count") +
        scale_x_continuous(breaks=seq(0,66,4)) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.x = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold"),
                panel.ontop = FALSE
        ) +
        geom_vline(mapping = aes(xintercept = StatD02$mean), col='black', size=1) +
        geom_text(mapping  = aes(label=LabelString, y=1000, x=5+StatD02$mean),
                  col = 'black',
                  size=txt.size))
        dev.off()
        # Scatter plot ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Deviation vs percentile 90
        StatSTD <- summarize(tmpDF, count = n(), mean = mean(std, na.rm = TRUE))
        TitleString = str_c("Area Total: P90 vs STD (", i,")")
        jpeg(str_c(projectDir, "IMAGES/JPG/Ipe",i,"_EA_P90vsSTD.jpg"), width = 800, height = 488, units = "px", quality = 100)
        plot(
        ggplot(data = tmpDF) +
        geom_point(mapping = aes(x = std, y = p90)) +
        labs(title=TitleString, x="Desvio padrão (m)", y="Percentil 90 (m)") +
        scale_x_continuous(breaks=seq(0,20,2)) +
        scale_y_continuous(breaks=seq(0,50,5)) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.x = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold"),
                panel.ontop = FALSE))
dev.off()
}
rm(tmpDF, StatAVG, StatMAX, StatP50, StatP90, StatDNS, StatD02, StatSTD,
   TitleString, LabelString, LabelString1, LabelString2)

# BoxPlots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# MAX and AVG
TitleString = "Area Total: MAX e AVG"
tmpDF <- select(TAreMetrics, Year, max, avg)
tmpDF <- gather(tmpDF, "metric", "value", -Year) %>% drop_na()
p = ggplot(data = tmpDF, aes(x = metric, y = value))
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_EA_BoxPlot_MAXAVG.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + geom_boxplot(aes(fill=metric)) +
             stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
             stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, size = txt.size) +
             facet_wrap( ~ Year) +
             labs(title=TitleString, x=NULL, y="Altura (m)") +
             theme(
                     panel.background = element_rect(fill = "white"),
                     panel.grid.major = element_line(colour = NULL),
                     legend.title = NULL,
                     legend.text  =  element_text(size = thm.size, colour="black"),
                     axis.title= element_text(size = thm.size, face="bold"),
                     axis.line = element_line(size = .5, colour = "black"),
                     axis.text = element_text(size = thm.size, colour="black"),
                     title = element_text(size = thm.size, colour="black", face="bold"),
                     strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                     axis.title.y = element_text(size = thm.size, colour="black", face="bold")
                     )
     )
dev.off()
# P50 and P90
TitleString = "Area Total: P50 e P90"
tmpDF <- select(TAreMetrics, Year, p50, p90)
tmpDF <- gather(tmpDF, "metric", "value", -Year) %>% drop_na()
p = ggplot(data = tmpDF, aes(x = metric, y = value))
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_EA_BoxPlot_P50P90.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + geom_boxplot(aes(fill=metric)) +
             stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
             stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, size = txt.size) +
             facet_wrap( ~ Year) +
             labs(title=TitleString, x=NULL, y="Altura (m)") +
             theme(
                     panel.background = element_rect(fill = "white"),
                     panel.grid.major = element_line(colour = NULL),
                     legend.title = NULL,
                     legend.text  =  element_text(size = thm.size, colour="black"),
                     axis.title= element_text(size = thm.size, face="bold"),
                     axis.line = element_line(size = .5, colour = "black"),
                     axis.text = element_text(size = thm.size, colour="black"),
                     title = element_text(size = thm.size, colour="black", face="bold"),
                     strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                     axis.title.y = element_text(size = thm.size, colour="black", face="bold")
             )
)
dev.off()
# DNS
TitleString = "Area Total: DNS = [Pts>1.30/All_Pts]%"
tmpDF <- select(TAreMetrics, Year, dns)
tmpDF <- gather(tmpDF, "metric", "value", -Year) %>% drop_na()
p = ggplot(data = tmpDF, aes(x = metric, y = value))
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_EA_BoxPlot_DNS.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + geom_boxplot(aes(fill=metric)) +
             stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
             stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, size = txt.size) +
             facet_wrap( ~ Year) +
             labs(title=TitleString, x=NULL, y="Altura (m)") +
             theme(
                     panel.background = element_rect(fill = "white"),
                     panel.grid.major = element_line(colour = NULL),
                     legend.title = NULL,
                     legend.text  =  element_text(size = thm.size, colour="black"),
                     axis.title= element_text(size = thm.size, face="bold"),
                     axis.line = element_line(size = .5, colour = "black"),
                     axis.text = element_text(size = thm.size, colour="black"),
                     title = element_text(size = thm.size, colour="black", face="bold"),
                     strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                     axis.title.y = element_text(size = thm.size, colour="black", face="bold")
             )
)
dev.off()
# D02
TitleString = "Area Total: D02 = [Pts 15-35m/All_Pts]%"
tmpDF <- select(TAreMetrics, Year, d02)
tmpDF <- gather(tmpDF, "metric", "value", -Year) %>% drop_na()
p = ggplot(data = tmpDF, aes(x = metric, y = value))
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_EA_BoxPlot_D02.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + geom_boxplot(aes(fill=metric)) +
             stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
             stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7, size = txt.size) +
             facet_wrap( ~ Year) +
             labs(title=TitleString, x=NULL, y="Altura (m)") +
             theme(
                     panel.background = element_rect(fill = "white"),
                     panel.grid.major = element_line(colour = NULL),
                     legend.title = NULL,
                     legend.text  =  element_text(size = thm.size, colour="black"),
                     axis.title= element_text(size = thm.size, face="bold"),
                     axis.line = element_line(size = .5, colour = "black"),
                     axis.text = element_text(size = thm.size, colour="black"),
                     title = element_text(size = thm.size, colour="black", face="bold"),
                     strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                     axis.title.y = element_text(size = thm.size, colour="black", face="bold")
             )
)
dev.off()
rm(p, tmpDF, TAreMetrics)

# ----

# 10. SAMPLE PLOTS: READ  LiDAR METRICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# Read 2017 CSV file with LiDAR metrics for each sample plot ~~~~~~~~~~~
inDir = str_c(projectDir, 'RESULT/2017')
PlotMet2017 <- read_csv(str_c(inDir, '/Ipe2017_PLOT_metrics.csv'), na = c("-", "NA"))
Plot <- gsub(pattern = "\\.laz$", "", basename(PlotMet2017$file_name))
PlotMet2017$Year <- 2017                 # Add field Year
PlotMet2017 <- cbind(Plot, PlotMet2017)  # Add field with plot names
PlotMet2017$file_name <- NULL            # Delete fld with full name
# Read 2016 CSV file with LiDAR metrics for each sample plot ~~~~~~~~~~~
inDir = str_c(projectDir, 'RESULT/2016')
PlotMet2016 <- read_csv(str_c(inDir, '/Ipe2016_PLOT_metrics.csv'), na = c("-", "NA"))
Plot <- gsub(pattern = "\\.laz$", "", basename(PlotMet2016$file_name))
PlotMet2016$Year <- 2016                 # Add field Year
PlotMet2016 <- cbind(Plot, PlotMet2016)  # Add field with plot names
PlotMet2016$file_name <- NULL            # Delete fld with full name
# Read 2015 CSV file with LiDAR metrics for each sample plot ~~~~~~~~~~~
inDir = str_c(projectDir, 'RESULT/2015')
PlotMet2015 <- read_csv(str_c(inDir, '/Ipe2015_PLOT_metrics.csv'), na = c("-", "NA"))
Plot <- gsub(pattern = "\\.laz$", "", basename(PlotMet2015$file_name))
PlotMet2015$Year <- 2015                 # Add field Year
PlotMet2015 <- cbind(Plot, PlotMet2015)  # Add field with plot names
PlotMet2015$file_name <- NULL            # Delete fld with full name
# Merge multi temporal LiDAR metrics into one single dataframe ~~~~~~~~~
PlotMetrics <-as.tibble(bind_rows(PlotMet2015, PlotMet2016, PlotMet2017, .id = NULL))
rm(Plot, PlotMet2015, PlotMet2016, PlotMet2017)
PlotMetrics$Year <- as.character(str_c(PlotMetrics$Year))
PlotMetrics <- PlotMetrics %>%        # Sets Year as first column
        select(Year, everything())    # Year and Plot are the unique key
PlotMetrics$Plot     <- as.factor(PlotMetrics$Plot)

# 11. SAMPLE PLOTS: READ  FIELDMEASUREMENTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# Read CSV file with field measurements from each sample plots
inDir = str_c(projectDir, 'FLDDAT')
PlotMsrmnts <- read_csv(str_c(inDir, '/Ipe_PlotMsrmnts_2015a2017.csv'), na = c("-", "NA"))
PlotMsrmnts$Year     <- as.factor(as.character(str_c(PlotMsrmnts$Year)))
PlotMsrmnts$Plot     <- as.factor(PlotMsrmnts$Plot)
PlotMsrmnts$Estrato  <- as.factor(as.character(str_c(PlotMsrmnts$Estrato)))
PlotMsrmnts$Plot_Ipe <- as.factor(PlotMsrmnts$Plot_Ipe)

# 12. SAMPLE PLOTS: MERGE LiDAR AND FIELD DATA ~~~~~~~~~~~~~~~~~~~~~~~~~ ----
PlotData <- inner_join(PlotMetrics, PlotMsrmnts, by = c("Year", "Plot"))
rm(PlotMetrics, PlotMsrmnts)

# 13. SAMPLE PLOTS: GRAPH LiDAR AND FIELD DATA ~~~~~~~~~~~~~~~~~~~~~~~~~ ----
# BoxPlots ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# count(tmpDF, Year, Estrato)
# MAX and AVG
TitleString  = str_c("Parcelas amostrais: MAX e AVG")
tmpDF <- select(PlotData, Year, Plot, Estrato, max, avg)
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -Plot, -Estrato))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) + 
        geom_boxplot(aes(fill=metric)) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.4, hjust=-0.1, size = txt.size) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_MAXAVGa.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="Altura (m)") + facet_wrap ( ~ Year))
dev.off()
TitleString  = str_c("Parcelas amostrais: MAX e AVG, por Área")
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_MAXAVGb.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="Altura (m)") + facet_grid (Estrato ~ Year))
dev.off()
# P50 and P90
TitleString  = str_c("Parcelas amostrais: P50 e P90")
tmpDF <- select(PlotData, Year, Plot, Estrato, p50, p90)
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -Plot, -Estrato))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) + 
        geom_boxplot(aes(fill=metric)) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.4, hjust=-0.1, size = txt.size) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_P50P90a.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="Altura (m)") + facet_wrap ( ~ Year))
dev.off()
TitleString  = str_c("Parcelas amostrais: P50 e P90, por Área")
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_P50P90b.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="Altura (m)") + facet_grid (Estrato ~ Year))
dev.off()
# DNS
TitleString = "Parcelas amostrais: DNS = [Pts>1.30/All_Pts]%"
tmpDF <- select(PlotData, Year, Plot, Estrato, dns)
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -Plot, -Estrato))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) + 
        geom_boxplot(aes(fill=metric)) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.4, hjust=-0.1, size = txt.size) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_DNSa.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="(%)") + facet_wrap ( ~ Year))
dev.off()
TitleString = "Parcelas amostrais: DNS = [Pts>1.30/All_Pts]%, por Área"
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_DNSb.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="(%)") + facet_grid (Estrato ~ Year))
dev.off()
# D02
TitleString = "Parcelas amostrais: D02 = [Pts 15-35m/All_Pts]%"
tmpDF <- select(PlotData, Year, Plot, Estrato, d02)
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -Plot, -Estrato))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) + 
        geom_boxplot(aes(fill=metric)) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.4, hjust=-0.1, size = txt.size) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_D02a.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="(%)") + facet_wrap ( ~ Year))
dev.off()
TitleString = "Parcelas amostrais: D02 = [Pts 15-35m/All_Pts]%, por Área"
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_D02b.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="(%)") + facet_grid (Estrato ~ Year))
dev.off()
# Measured height
TitleString = "Parcelas amostrais: alturas medidas"
tmpDF <- select(PlotData, Year, Plot, Estrato, Fustes, Sum_H)
tmpDF$MeanH <- tmpDF$Sum_H/tmpDF$Fustes
tmpDF$Fustes <- NULL
tmpDF$Sum_H <- NULL
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -Plot, -Estrato))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) + 
        geom_boxplot(aes(fill=metric)) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.4, hjust=-0.1, size = txt.size) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_Heights_a.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="Altura (m)") + facet_wrap ( ~ Year))
dev.off()
TitleString = "Parcelas amostrais: alturas medidas, por Área"
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_Heights_b.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="Altura (m)") + facet_grid (Estrato ~ Year))
dev.off()
# Basal Area
TitleString = "Parcelas amostrais: basal area e sectional area"
tmpDF <- select(PlotData, Year, Plot, Estrato, Sum_BA, Sum_SA)
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -Plot, -Estrato))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) + 
        geom_boxplot(aes(fill=metric)) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.4, hjust=-0.1, size = txt.size) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_BASA_a.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="(m2)") + facet_wrap ( ~ Year))
dev.off()
TitleString = "Parcelas amostrais: basal area e sectional area, por Área"
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_BASA_b.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="(m2)") + facet_grid (Estrato ~ Year))
dev.off()
# Biomass >> x Kg/(400 m2) to y MgC/ha >> y = x*25/1000]*0.5 = x*0.0125
TitleString = "Parcelas amostrais: AGC (Mgc/ha, Stem+Crown)"
tmpDF <- select(PlotData, Year, Plot, Estrato, Sum_BmStem, Sum_BmCrown, Sum_BmRoot)
tmpDF$TotBiomass  <- 0.0125*(tmpDF$Sum_BmStem+tmpDF$Sum_BmCrown+tmpDF$Sum_BmRoot)
names(tmpDF)[names(tmpDF) == "TotBiomass"] <- "AGC"
tmpDF$Sum_BmStem  <- NULL
tmpDF$Sum_BmCrown <- NULL
tmpDF$Sum_BmRoot  <- NULL
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -Plot, -Estrato))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) + 
        geom_boxplot(aes(fill=metric)) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=3) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.4, hjust=-0.1, size = txt.size) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_AGC_a.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="(Mg C)/ha") + facet_wrap ( ~ Year))
dev.off()
TitleString = "Parcelas amostrais: AGC (Mgc/ha, Stem+Crown), por Área"
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_SP_BoxPlot_AGC_b.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + labs(title=TitleString, x=NULL, y="(Mg C)/ha") + facet_grid (Estrato ~ Year))
dev.off()
rm(p, tmpDF)

# 14. AREAS OF INTEREST (AOI): READ  LiDAR METRICS ~~~~~~~~~~~~~~~~~~~~~ ----
# Read 2017 CSV file with LiDAR metrics for each area of interest AOI ~~
inDir = str_c(projectDir, 'RESULT/2017')
PolyMet2017 <- read_csv(str_c(inDir, '/Ipe2017_AOIPIXL_metrics.csv'), na = c("-", "NA"))
PolyMet2017$Year <- 2017                 # Add field Year
# Read 2016 CSV file with LiDAR metrics for each area of interest AOI ~~
inDir = str_c(projectDir, 'RESULT/2016')
PolyMet2016 <- read_csv(str_c(inDir, '/Ipe2016_AOIPIXL_metrics.csv'), na = c("-", "NA"))
PolyMet2016$Year <- 2016                 # Add field Year
# Read 2015 CSV file with LiDAR metrics for each area of interest AOI ~~
inDir = str_c(projectDir, 'RESULT/2015')
PolyMet2015 <- read_csv(str_c(inDir, '/Ipe2015_AOIPIXL_metrics.csv'), na = c("-", "NA"))
PolyMet2015$Year <- 2015                 # Add field Year
# Merge multi temporal LiDAR metrics into one single dataframe ~~~~~~~~~
PolyMetrics <-as.tibble(bind_rows(PolyMet2015, PolyMet2016, PolyMet2017, .id = NULL))
rm(PolyMet2015, PolyMet2016, PolyMet2017)
PolyMetrics$Year <- as.character(str_c(PolyMetrics$Year))
PolyMetrics <- PolyMetrics %>%        # Sets Year as first column
        select(Year, everything())    # Year and Plot are the unique key
PolyMetrics$AOI <- as.factor(PolyMetrics$AOI)


tmpDF <- PolyMetrics %>%
        select(Year, AOI, Pixel, MAX, AVG) %>%
        filter(str_detect(AOI, "Area*"))

# 15. AREAS OF INTEREST (AOI): GRAPH LiDAR METRICS ~~~~~~~~~~~~~~~~~~~~~ ----
# OBS_df <- as.tibble((count(PolyMetrics, Year, AOI)))
# OBS_n  <- as.integer(count(filter(OBS_df, Year==2016)))
# MAX and AVG
Title = str_c("AOI: MAX e AVG")
yLabel = "Altura (m)"
tmpDF <- PolyMetrics %>%
        select(Year, AOI, Pixel, MAX, AVG) %>%
        filter(str_detect(AOI, "Area*"))
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -AOI, -Pixel))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) +
        geom_boxplot(aes(fill=metric), outlier.alpha = 0.1) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.5, hjust=-0.3, size = txt.size) +
        scale_y_continuous(name = yLabel) +
        scale_x_discrete(name = NULL) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = .7*thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = .7*thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = .7*thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_MAXAVGa.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + ggtitle(Title) + facet_wrap ( ~ Year))
dev.off()
Title = str_c("AOI: MAX e AVG, por Área")
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_MAXAVGb.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + ggtitle(Title) + facet_grid (AOI ~ Year))
dev.off()
# P50 and P90
Title = str_c("AOI: P50 e P90")
yLabel = "Altura (m)"
tmpDF <- PolyMetrics %>%
        select(Year, AOI, Pixel, P50, P90) %>%
        filter(str_detect(AOI, "Area*"))
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -AOI, -Pixel))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) +
        geom_boxplot(aes(fill=metric), outlier.alpha = 0.1) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.5, hjust=-0.3, size = txt.size) +
        scale_y_continuous(name = yLabel) +
        scale_x_discrete(name = NULL) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = .7*thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = .7*thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = .7*thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_P50P90a.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + ggtitle(Title) + facet_wrap ( ~ Year))
dev.off()
Title = str_c("P50 e P90, por Área")
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_P50P90b.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + ggtitle(Title) + facet_grid (AOI ~ Year))
dev.off()
# DNS
Title = str_c("AOI: DNS = [Pts>1.30/All_Pts]%")
yLabel = "(%)"
tmpDF <- PolyMetrics %>%
        select(Year, AOI, Pixel, DNS) %>%
        filter(str_detect(AOI, "Area*"))
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -AOI, -Pixel))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) +
        geom_boxplot(aes(fill=metric), outlier.alpha = 0.1) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.5, hjust=-0.3, size = txt.size) +
        scale_y_continuous(name = yLabel) +
        scale_x_discrete(name = NULL) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = .7*thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = .7*thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = .7*thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_DNSa.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + ggtitle(Title) + facet_wrap ( ~ Year))
dev.off()
Title = str_c("AOI: DNS = [Pts>1.30/All_Pts]%, por Área")
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_DNSb.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + ggtitle(Title) + facet_grid (AOI ~ Year))
dev.off()
# CVR
Title = str_c("AOI: Coeficiente of Variation (CVR)")
yLabel = "(%)"
tmpDF <- PolyMetrics %>%
        select(Year, AOI, Pixel, CVR) %>%
        filter(str_detect(AOI, "Area*"))
tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -AOI, -Pixel))
p = ggplot(data = tmpDF, aes(x = metric, y = value)) +
        geom_boxplot(aes(fill=metric), outlier.alpha = 0.1) +
        stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
        stat_summary(fun.data = fun_mean, geom="text", vjust=-0.5, hjust=-0.3, size = txt.size) +
        scale_y_continuous(name = yLabel) +
        scale_x_discrete(name = NULL) +
        theme(
                panel.background = element_rect(fill = "white"),
                panel.grid.major = element_line(colour = NULL),
                legend.title = NULL,
                legend.text  =  element_text(size = thm.size, colour="black"),
                axis.title= element_text(size = thm.size, face="bold"),
                axis.line = element_line(size = .5, colour = "black"),
                axis.text = element_text(size = .7*thm.size, colour="black"),
                title = element_text(size = thm.size, colour="black", face="bold"),
                strip.text.x = element_text(size = .7*thm.size, colour="black", face="bold"),
                strip.text.y = element_text(size = .7*thm.size, colour="black", face="bold"),
                axis.title.y = element_text(size = thm.size, colour="black", face="bold")
        )
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_CVRa.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + ggtitle(Title) + facet_wrap ( ~ Year))
dev.off()
Title = str_c("AOI: Coeficiente of Variation (CVR), por Área")
jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_CVRb.jpg"), width = 800, height = 400, units = "px", quality = 100)
plot(p + ggtitle(Title) + facet_grid (AOI ~ Year))
dev.off()



# 16. AREAS OF INTEREST (AOI): GRAPH SUMMARY LiDAR METRICS ~~~~~~~~~~~~~ ----
# Read 2017 CSV file with SUMMARY LiDAR metrics for each AOI ~~~~~~~~~~~
inDir = str_c(projectDir, 'RESULT/2017')
PolySum2017 <- read_csv(str_c(inDir, '/Ipe2017_AOIPOLY_metrics.csv'), na = c("-", "NA"))
PolySum2017$Year <- 2017                 # Add field Year
# Read 2016 CSV file with SUMMARY LiDAR metrics for each AOI ~~~~~~~~~~~
inDir = str_c(projectDir, 'RESULT/2016')
PolySum2016 <- read_csv(str_c(inDir, '/Ipe2016_AOIPOLY_metrics.csv'), na = c("-", "NA"))
PolySum2016$Year <- 2016                 # Add field Year
# Read 2015 CSV file with SUMMARY LiDAR metrics for each AOI ~~~~~~~~~~~
inDir = str_c(projectDir, 'RESULT/2015')
PolySum2015 <- read_csv(str_c(inDir, '/Ipe2015_AOIPOLY_metrics.csv'), na = c("-", "NA"))
PolySum2015$Year <- 2015                 # Add field Year
# Merge multi temporal LiDAR summaries into one single dataframe ~~~~~~~
PolySumMtrc <-as.tibble(bind_rows(PolySum2015, PolySum2016, PolySum2017, .id = NULL))
rm(PolySum2015, PolySum2016, PolySum2017)
PolySumMtrc$Year <- as.character(str_c(PolySumMtrc$Year))
PolySumMtrc <- PolySumMtrc %>%        # Sets Year as first column
        select(Year, everything())    # Year and Plot are the unique key
PolySumMtrc$AOI <- as.factor(PolySumMtrc$AOI)

tmpDF <- PolySumMtrc %>%
        select(Year, AOI, TotalArea, TotalAGC) %>%
        filter(str_detect(AOI, "Area*"))

tmpDF$AGC_ha <- tmpDF$TotalAGC / tmpDF$TotalArea

ggplot(data=tmpDF, aes(x=Year, y=AGC_ha, group=AOI, colour=AOI)) + 
        geom_line(size=.5) + 
        geom_point(size=3, fill="white") +
        geom_text(aes(label = round(AGC_ha,2)), size = 3, colour="black", vjust=-0.2, hjust=-0.1)


# # # AGC  >>>>> AGC MgC/Pixel to AGCha MgC/ha >> AGCha = 10000 * AGC / Area
# # # PROBLEMA !!!  divisão por áreas mto pqnas gera pixels com SUPERESTIMATIVAS
# Title = str_c("AOI: Above Ground Carbon (Mg/ha)")
# yLabel = "MgC/ha"
# tmpDF <- PolyMetrics %>%
#         select(Year, AOI, Pixel, Area, AGC) %>%
#         filter(str_detect(AOI, "Area*"))
# tmpDF$AGC_HA <- hectare * tmpDF$AGC / tmpDF$Area
# tmpDF$AGC   <- NULL
# tmpDF$Area  <- NULL
# tmpDF <- gather(tmpDF, "metric", "value", c(-Year, -AOI, -Pixel))
# p = ggplot(data = tmpDF, aes(x = metric, y = value)) +
#         geom_boxplot(fill='grey80', color="black", outlier.alpha = 0.1) +
#         stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) +
#         stat_summary(fun.data = fun_mean, geom="text", vjust=-0.5, hjust=-0.4) +
#         scale_y_continuous(name = yLabel) +
#         scale_x_discrete(name = NULL) +
#         theme(
#                 panel.background = element_rect(fill = "white"),
#                 panel.grid.major = element_line(colour = NULL),
#                 legend.title = NULL,
#                 legend.text  =  element_text(size = thm.size, colour="black"),
#                 axis.title= element_text(size = thm.size, face="bold"),
#                 axis.line = element_line(size = .5, colour = "black"),
#                 axis.text = element_text(size = .7*thm.size, colour="black"),
#                 title = element_text(size = thm.size, colour="black", face="bold"),
#                 strip.text.x = element_text(size = .7*thm.size, colour="black", face="bold"),
#                 strip.text.y = element_text(size = .7*thm.size, colour="black", face="bold"),
#                 axis.title.y = element_text(size = thm.size, colour="black", face="bold")
#         )
# jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_AGCa.jpg"), width = 800, height = 400, units = "px", quality = 100)
# plot(p + ggtitle(Title) + facet_wrap ( ~ Year))
# dev.off()
# Title = str_c("AOI: Above Ground Carbon (Mg/ha), by Area")
# jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_BoxPlot_AGCb.jpg"), width = 800, height = 400, units = "px", quality = 100)
# plot(p + ggtitle(Title) + facet_grid (AOI ~ Year))
# dev.off()


# 
# # Scatter plots
# # PROBLEMA !!!  pixels com SUPERESTIMATIVAS
# Title = str_c("AOI: ACG(Mg/ha) vs P90(m)")
# yLabel = "ACG (Mg/ha)"
# xLabel = "P90 (m)"
# tmpDF <- PolyMetrics %>%
#         select(Year, AOI, Pixel, AGC, Area) %>%
#         filter(str_detect(AOI, "Area*"))
# tmpDF %>% 
#         group_by(Year, AOI) %>% 
#         summarise_all(sum) %>%
#         mutate(Area_ha = Area/hectare)
# 
# #summarize(tmpDF, count = n(), max = max(AGC, na.rm = TRUE))
# # tmpDF <- filter(tmpDF, Area > 10)
# # tmpDF <- filter(tmpDF, AGC < 5)
# tmpDF$AGC_HA <- hectare * tmpDF$AGC / tmpDF$Area
# tmpDF$AGC   <- NULL
# tmpDF$Area  <- NULL
# p <- ggplot(data = tmpDF, mapping = aes(x = P90, y = AGC_HA, colour = factor(AOI))) +
#         geom_point(size = 1, alpha = 1/10) +
#         scale_colour_hue(l=20) + # Use a slightly darker palette than normal
#         geom_smooth(method=lm,   # Add linear regression lines
#                     se=FALSE,    # Don't add shaded confidence region
#                     fullrange=FALSE) + # Extend regression lines
#         theme(
#                 panel.background = element_rect(fill = "grey80"),
#                 panel.grid.major = element_line(colour = NULL),
#                 axis.line = element_line(size = .5, colour = "black"),
#                 panel.ontop = FALSE
#                 )
# jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_Scatter_AGCvsP90a.jpg"), width = 800, height = 400, units = "px", quality = 100)
# plot(p + labs(title=Title, x=xLabel, y=yLabel))
# dev.off()
# p <- ggplot(data = tmpDF, mapping = aes(x = P90, y = AGC_HA, colour = factor(Year))) +
#         geom_point(size = 1, alpha = 1/10) +
#         scale_colour_hue(l=20) + # Use a slightly darker palette than normal
#         geom_smooth(method=lm,   # Add linear regression lines
#                     se=FALSE,    # Don't add shaded confidence region
#                     fullrange=FALSE) + # Extend regression lines
#         theme(
#                 panel.background = element_rect(fill = "grey80"),
#                 panel.grid.major = element_line(colour = NULL),
#                 axis.line = element_line(size = .5, colour = "black"),
#                 panel.ontop = FALSE
#         )
# jpeg(str_c(projectDir, "IMAGES/JPG/Ipe_AOI_Scatter_AGCvsP90b.jpg"), width = 800, height = 400, units = "px", quality = 100)
# plot(p + labs(title=Title, x=xLabel, y=yLabel))
# dev.off()

# # ---- ----
#  END ~~~~