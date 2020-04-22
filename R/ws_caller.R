setwd('~/Desktop')
require(magrittr)

write_file = 'enquia.txt'
write(c('img', 'info', 'code', 'cite', 'likes', 'species_info', 'species_imgs'),
      write_file, ncolumns = 7, sep = '|', append = F)

log_file = 'log.txt'
write('status', log_file, append = F)

setwd('~/Desktop/Projects/Scripts/R')

n = 300000
starts = seq(0, 5000000, n)
for(i in starts){
  paste('xterm -e "Rscript web_scrapping.R', i, n, write_file, log_file, '" &') %>% system
}
