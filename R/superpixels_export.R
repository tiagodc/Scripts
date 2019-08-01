library(OpenImageR)
library(raster)
library(magrittr)

im = readImage("img.png")

res_slic = superpixels(input_image = im, method = "slic", superpixel = 1000, compactness = 20, return_slic_data = TRUE, return_labels = TRUE, write_slic = "", verbose = TRUE)

pixel_spacing = 1
x_offset = 0
y_offset = 0

xs = x_offset + pixel_spacing * 1:ncol(res_slic$slic_data) - 1
ys = y_offset + pixel_spacing * 1:nrow(res_slic$slic_data) - 1

xy_grid = expand.grid(ys, xs)
xy_grid = data.frame(X = xy_grid[,2], Y = xy_grid[,1])

data_grid = sapply(1:3, function(i) res_slic$slic_data[,,i] %>% as.vector ) %>% as.data.frame
data_grid = sp::SpatialPixelsDataFrame(xy_grid, data_grid) %>% raster::stack()

### uncomment to export only binary map
# for(i in 1:3){
#   values(data_grid[[i]]) = ifelse(values(data_grid[[i]]) == 255, 1, 0)
# }

writeRaster(data_grid, 'path.tif')



