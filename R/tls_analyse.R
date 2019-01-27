require(magrittr)

setwd('c:/Users/tiago/Desktop/')

info = read.csv('tls_test.csv')
info$start %<>% as.POSIXct
info$end %<>% as.POSIXct
bench = read.csv('bench.csv')
bench$time %<>% as.POSIXct

for(i in 1:nrow(info)){

  temp = info[i,] %>% as.data.frame
  times = bench[ bench$time >= temp$start & bench$time <= temp$end,]

  info$mem[i] = times$mem %>% range %>% diff
}

info$time = info$end - info$start
