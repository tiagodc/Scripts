require(rvest)

url <- "https://www.wikiaves.com.br/0000007"
session <- html_session(url)
raw_txt = read_html(url) %>% as.character(a)

info = sub('.+<!-- AddThis Button END -->(.+Assunto.+)</div>.*<div id=\"divDetalhesBotao\">.+', '\\1', raw_txt) %>%
  gsub(pattern = '\t|nbsp|<.*?>|&.*?;', replacement = '') %>%
  sub(pattern = '^\n', replacement = '') %>%
  gsub(pattern = '"', replacement = "'") %>% 
  gsub(pattern = '\n+|\\[.*\\]', replacement = '|')

# strsplit(info,'\\|')
