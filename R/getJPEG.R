setwd('~/Desktop/Sintecsys/db3/')
library(RSQLite)
library(magrittr)

db3files = dir(pattern = '\\.db3$')

for(filename in db3files){

  # filename <- "~/Desktop/Sintecsys/OneDrive_1_9-30-2019/TTG TUR.db3"
  gc()
  sqlite.driver = dbDriver("SQLite")
  db = dbConnect(sqlite.driver, dbname = filename)
  dirpath = sub('\\.db3$', '', filename) %T>% dir.create()

  ## Some operations
  # dbListTables(db)
  tab = dbReadTable(db, 'Deteccao')

  for(i in 1:nrow(tab)){
    paste(dirpath, '>>', i, 'of', nrow(tab), '\n') %>% cat
    row = tab[i,]
    hex = row$IMAGEM[[1]]

    if(is.null(hex)) next

    ff = which(hex == 'ff')
    d8 = which(hex == 'd8')

    if(length(ff) == 0) next

    for(j in ff){
      jj = which(d8 == j+1)
      if(length(jj) > 0){
        ff = j
        break
      }
    }

    hex = hex[-c(1:(ff-1))]

    img = magick::image_read(hex)
    dt = gsub(':', '-', row$DATA)
    magick::image_write(img, paste0(dirpath, '/', dt, '_', row$DETECCAO, '.jpeg'))
  }

  dbDisconnect(db)
}
