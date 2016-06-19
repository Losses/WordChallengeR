library(rjson)

cls <- function(){
  cat('\014')
}

yn <- function(hint){
  status <- readline(hint)
  if (!status %in% c(0, 1, 'q')) yn(hint)
  if (status == 'q') stop('You choose to quit.')
  as.numeric(status)
}

pasteLines <- function(x, y = '', html = F){
  if (x == '') return(y)
  if (html) paste(x, y, sep = '</br>')
  else paste(x, y, sep = '\n')
}

remove_blank <- function(x){
  x <- as.character(x)
  x[x!='' & !is.na(x) & !is.null(x)]
}

html_tag <- function(content, tag){
  sprintf('<%s>%s</%s>', tag, content, tag)
}

read.word <- function(word){
  file.loc <- sprintf('./dictionary/%s.json', word)
  if (!file.exists(file.loc)){
    url <- sprintf('http://fanyi.youdao.com/openapi.do?keyfrom=WordChallengeR&key=1512553241&type=data&doctype=json&only=dict&version=1.1&q=%s', word)
    download.file(url, destfile = file.loc)
  }
  
  entry <- fromJSON(file = file.loc)
  list(
    word = word,
    phonetic = entry$basic$`us-phonetic`,
    explains = entry$basic$explains
  )
}

read.list <- function(file.name){
  words <- read.csv(sprintf('./data/%s', file.name), header = T)
  words <- lapply(words, remove_blank)
  words
}

flatten_list <- function(list){
  words_character <- c()
  for(.i in names(list))
    words_character <- c(words_character, list[[.i]])
  words_character <- words_character[words_character != '']
  
  words_character
}

as.dictionary <- function(entry, lib_mark = T, html = F){
  words_dictionary <- ''
  words_dictionary <- pasteLines(
    words_dictionary,
    sprintf('%s%s',
            ifelse(need_italic(entry$word),
                   ifelse(lib_mark,
                          ifelse(html,
                                 html_tag(entry$word, 'i'),
                                 sprintf('△ %s', entry$word)),
                          entry$word),
                   ifelse(html & need_bold(entry$word),
                          html_tag(entry$word, 'b'),
                          entry$word)
                   ), 
            ifelse(is.null(entry$phonetic),
                   '',
                   sprintf(' /%s/', strsplit(entry$phonetic, ';')[[1]][1])
            )
    )
  , html = html
  )
  
  words_dictionary <- pasteLines(
    words_dictionary,
    paste(entry$explains[!1:length(entry$explains) %in% grep('人名',entry$explains)], 
          collapse = ifelse(html, '</br>', '\n'))
    , html = html
  )
  if (html) words_dictionary <- sprintf('<div class="word">%s</div>', words_dictionary)
  words_dictionary
}

is.pharse <- function(x){
  length(grep(' ', x)) > 0
}

word_in_cache <- function(word){
  nrow(WORD_CACHE[WORD_CACHE$word == word,])>0
}

need_bold <- function(word){
  word_in_cache(word) && WORD_CACHE[WORD_CACHE$word == word,'freq'] > 1
}

need_italic <- function(word){
  word_in_cache(word) && !WORD_CACHE[WORD_CACHE$word == word,'in_lib']
}

write.file <- function(x, file){
  con <- file(file, "a")
  tryCatch({
    cat(iconv(x, to="UTF-8"), file=con, sep="\n")
  },
  finally = {
    close(con)
  })
}

build_word_cache <- function(){
  word_cache <- list()
  all_words <- c()
  for(.file in list_data_file())
    all_words <- c(all_words, read.list(.file))
  
  word_cache <- as.data.frame(table(flatten_list(all_words)))
  colnames(word_cache) <- c('word', 'freq')
  word_cache$in_lib <- sapply(word_cache$word, is.pharse) | word_cache$word %in% standard_lib
  word_cache$word <- as.character(word_cache$word)
  
  word_cache
}

get_file_hint <- function(file = readline()){
  if(file.exists(sprintf('./data/%s', file))) file
  else get_file()
}

list_data_file <- function(){
  csv_list <- list.files('./data')
  csv_list <- csv_list[grep('\\s*.csv', csv_list)]
  csv_list[csv_list != 'temp.csv']
}

get_file <- function(){
  csv_list <- list_data_file()
  cat('Choose a word list from following files: \n')
  cat(paste(csv_list, collapse = '\t'))
  cat('\n')
  get_file_hint()
}

standard_lib <- readLines('./data/standardLib.txt')
WORD_CACHE <- build_word_cache()
