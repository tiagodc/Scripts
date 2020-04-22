setwd('~/Desktop/')
require(rvest)
# require(foreach)
# require(doParallel)

start_url = as.double(commandArgs(T)[1])
end_url   = start_url + as.double(commandArgs(T)[2])
start_url = start_url + 1

write_file = commandArgs(T)[3]
log_file = commandArgs(T)[4]

padZeros = function(number, digits=7){
    nstr = as.character(number)
    add_zeros = digits - stringr::str_length(nstr)
    rep(0, add_zeros) %>% paste0(collapse = '') %>% paste0(number) %>% return()
}

webMiner = function(page_index, base_url = "https://www.wikiaves.com.br/"){
  
  page_index = page_index %>% padZeros
  url = paste0(base_url, page_index)
  
  session = html_session(url)
  raw_txt = read_html(url) %>% as.character()

  is_empty = grepl('.*Mídia não encontrada.*', raw_txt)
  is_sound = grepl('id="imgSonograma"', raw_txt)
  
  if(is_empty){
    return(-1)
  }else if(is_sound){
    return(0)
  }
  
  info = sub('.+<!-- AddThis Button END -->(.+Assunto.+)</div>.*<div id=\"divDetalhesBotao\">.+', '\\1', raw_txt) %>%
    gsub(pattern = '\t|nbsp|<.*?>|&.*?;', replacement = '') %>%
    sub(pattern = '^\n', replacement = '') %>%
    gsub(pattern = '"', replacement = "'") %>% 
    gsub(pattern = '\n+|\\[.*\\]', replacement = ';')
  
  code = sub('.+<div class=\"wa-recordid font-poppins\">(.+?)</div>.+', '\\1', raw_txt) %>%
    gsub(pattern = '\t|\n', replacement = '')
  
  cite = sub('.+<b>Citação:.*</b?>(.+)<b>Atenção:.*</b?>.+', '\\1', raw_txt) %>%
    gsub(pattern = '\t|nbsp|<.*?>|&.*?;|\r\n', replacement = '')
  
  likes = sub('.+class=\"wa-likes\">(.+?)</a>.+', '\\1', raw_txt)
  
  img = sub('.+id=\"wa-foto\" class=\"img-responsive\" src=\"(.+?)">.+', '\\1', raw_txt)
  
  if(grepl('NÃO-IDENTIFICADA', raw_txt)){
    species_info_url = 'NA'
    species_imgs_url = 'NA'
  }else{
    species_info_url = base_url %>% paste0( html_node(session, 'a.wa-menu-especie') %>% html_attr('href') )
    species_imgs_url = base_url %>% paste0( sub('.+<a href="(.+)\" class.+Fotos da Espécie.+', '\\1', raw_txt) )
  }

  data = c(img = img, info = info, code = code, cite = cite, likes = likes, species_info = species_info_url, species_imgs = species_imgs_url)
  return(data)
}

empty_counter = 0
t0 = Sys.time()

# nCores = 4
# cl = makePSOCKcluster(nCores)
# registerDoParallel(cl)

for(i in start_url:end_url) {
# foreach(i = 0:9999999, .combine = 'c', .packages = 'rvest') %dopar% {

  paste('\nmining page:', padZeros(i), '\n') %>% cat
  print(Sys.time() - t0)
  
  data = tryCatch(webMiner(i), error=function() NULL)
  
  if(is.null(data)){
    paste('\n', Sys.time(), '- error trying to load page:', padZeros(i)) %>% write(log_file, append = T)
    paste('error trying to load page:', padZeros(i)) %>% message()
  }else if(data == -1 || data == 0){
    if(data[1] == -1) empty_counter = empty_counter + 1
    next
  }else{
    sanity = sapply(data, stringr::str_length) %>% max
    if(sanity > 500){
      paste('\n', Sys.time(), '- HTML warning:', padZeros(i)) %>% write(log_file, append = T)
    }else{
      paste('\n', Sys.time(), '- mining page:', padZeros(i)) %>% write(log_file, append = T)
    }
  }
  
  empty_counter = 0
  empty_counter = ifelse(data[1] == -1, empty_counter + 1, 0) %>% as.double
  
  if(empty_counter > 1000) break 
  
  paste0('\n', data) %>% write(write_file, 7, T, '|')    
}

# stopImplicitCluster()
# stopCluster(cl)
# registerDoSEQ()
