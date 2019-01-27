require(lidR)
require(magrittr)

files = c('d:/geoslam.laz', 'd:/Projects/TLStools/test_clouds/gerdau.laz', 'd:/Roadbridge.laz', 'd:/aline/LAZ/bloco1.laz')
vx_sizes = c(0.1, 0.05, 0.025, 0.01)

f = files[2]
v = vx_sizes[2]

props = data.frame()

for(f in files){
  for(v in vx_sizes){

    filter = '-thin_with_voxel' %>% paste(v)
    tmBeg = Sys.time()
    paste(f,v,tmBeg) %>% print
    Sys.sleep(10)
    cloud = readLAS(f, select='XYZ', filter=filter)
    Sys.sleep(10)
    tmEnd = Sys.time()

    temp = data.frame(file = f, voxel = v, n = nrow(cloud@data), start=tmBeg, end=tmEnd)
    props %<>% rbind(temp)

    rm(cloud)
    gc(T,T,T)
  }
}

write.csv(props, 'tls_test.csv', row.names = F)