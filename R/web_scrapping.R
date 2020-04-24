require(rvest)

# pars = c(1,6,'~/Desktop/enquia.txt', '~/Desktop/log.txt')
pars = commandArgs(T)
url_codes = seq(as.double(pars[1]), 5000000, as.double(pars[2]))

write_file = pars[3]
log_file = pars[4]

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

repeat{
  used_codes = read.table(log_file, sep='|', as.is = T)[-1,1] %>% 
    as.character %>% sub(pattern = '.+:\\s(\\d+$)', replacement = '\\1') %>% as.double
  
  url_codes = url_codes[!(url_codes %in% used_codes)]
  
  if(length(url_codes) == 0) break
  
  empty_counter = 0
  t0 = Sys.time()
  
  counter = 0
  for(i in url_codes) {
  
    counter = counter + 1
    paste('\nterminal', pars[1], '- iteration', counter, '- mining page:', padZeros(i), '\n') %>% cat
    print(Sys.time() - t0)
    
    data = tryCatch(webMiner(i), error=function() NULL)
    
    msg = paste('\n', Sys.time(), '- terminal', pars[1], '- iteration', counter, '-')
    if(is.null(data)){
      paste(msg, 'error trying to load page:', padZeros(i)) %>% write(log_file, append = T)
      paste('error trying to load page:', padZeros(i)) %>% message()
    }else if(data[1] == -1){
      paste(msg, 'empty page:', padZeros(i)) %>% write(log_file, append = T)
      empty_counter = empty_counter + 1
      if(empty_counter > 1000) break
      next
    }else if(data[1] == 0){
      paste(msg, 'sound page:', padZeros(i)) %>% write(log_file, append = T)
      next
    }else{
      sanity = sapply(data, stringr::str_length) %>% max
      if(sanity > 500){
        paste(msg, 'HTML warning:', padZeros(i)) %>% write(log_file, append = T)
      }else{
        paste(msg, 'mining page:', padZeros(i)) %>% write(log_file, append = T)
      }
    }
    
    empty_counter = 0

    data[1] = paste0('\n', data[1])
    write(data, write_file, 7, T, '|')
  }
}
