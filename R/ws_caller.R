setwd('~/Desktop')
require(magrittr)

write_file = 'enquia.txt'
# write(c('img', 'info', 'code', 'cite', 'likes', 'species_info', 'species_imgs'),
#       write_file, ncolumns = 7, sep = '|', append = F)

log_file = 'log.txt'
# write('status', log_file, append = F)

write_file = paste(getwd(), write_file, sep='/')
log_file = paste(getwd(), log_file, sep='/')

starts = 1:8
n = length(starts)
for(i in starts){
  paste('xterm -e "Rscript ~/Desktop/Projects/Scripts/R/web_scrapping.R', i, n, write_file, log_file, '" &') %>% system
}
