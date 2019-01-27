require(magrittr)
bench = data.frame()

time = Sys.time() + 25*60

while(Sys.time() < time){
  temp = system('systeminfo', intern = T)
  n = grep('^Mem.+ria f.+sica dispon.+vel:.+', temp)
  temp = temp[n]

  temp = sub('.+\\s{2,}(\\d+\\.\\d*)\\sMB', '\\1', temp) %>% as.double

  df = data.frame(time = Sys.time(), mem = temp)

  bench %<>% rbind(df)
}

write.csv(bench, 'bench.csv', row.names = F)

