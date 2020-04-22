require(rvest)

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
  
  is_empty = grepl('<div class="m-alert__text">.*Mídia não encontrada.*?</div>', raw_txt)
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
  
  species_info_url = base_url %>% paste0( html_node(session, 'a.wa-menu-especie') %>% html_attr('href') )
  
  species_imgs_url =  base_url %>% paste0( sub('.+<a href="(.+)\" class.+Fotos da Espécie.+', '\\1', raw_txt) )
  
  data = c(img = img, info = info, code = code, cite = cite, likes = likes, species_info = species_info_url, species_imgs = species_imgs_url)
  return(data)
}

write_file = 'enquia.txt'
write(c('img', 'info', 'code', 'cite', 'likes', 'species_info', 'species_imgs'), 
      write_file, ncolumns = 7, sep = '|', append = F)

log_file = 'log.txt'
write('errors', log_file, append = F)

empty_counter = 0
t0 = Sys.time()

for(i in 0:9999999){
  paste('\nmining page:', padZeros(i), '\n') %>% cat
  print(Sys.time() - t0)
  
  data = tryCatch(webMiner(i), error=function() NULL)
  
  if(is.null(data)){
    write(padZeros(i) , log_file, append = T)
    paste('error trying to load page:', padZeros(i)) %>% message()
  }
  
  empty_counter = ifelse(data[1] == -1, empty_counter + 1, 0) %>% as.double
  
  if(empty_counter > 1000) break else if(data == -1 || data == 0) next
  
  write(data, write_file, 7, T, '|')    
}
