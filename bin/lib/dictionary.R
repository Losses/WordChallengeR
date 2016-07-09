#--------------------------------------------------------
#  FUNCTIONS FOR DICTIONARY GENERATING
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

EC_API <- '1512553241'
EE_API <- '84ca89b8-2ca9-4851-9ede-d93d7b866f39'

read.profile <- function(profile.file){
  sprintf('usr/profile/%s', profile.file) %>% read.csv
}

read.word <- function(word, lang = 'EC', generate_sentence = F){
  if(word == '') return(F)
  if(generate_sentence == T) return(read.word.ee(word, generate_sentence = T))
  if(lang == 'EE') return(read.word.ee(word))
  read.word.ec(word)
}

#-----------------------
# EC Dictionary
#-----------------------

read.word.ec <- function(word){
  file.loc <- sprintf('etc/dictionary/ec/%s.json', word)
  if (!file.exists(file.loc)) {
    url <- sprintf('http://fanyi.youdao.com/openapi.do?keyfrom=WordChallengeR&key=%s&type=data&doctype=json&only=dict&version=1.1&q=%s', EC_API, word)
    download.file(url, destfile = file.loc)
  }
  
  entry <- fromJSON(file = file.loc)
  list(
    word = word,
    phonetic = entry$basic$`us-phonetic`,
    explains = entry$basic$explains
  )
}

#-----------------------
# EE Dictionary
#-----------------------

value_node <- function(node, tag){
  set <- getNodeSet(node, tag)
  if (length(set) == 0) return(NA)
  xmlValue(set[[1]])
}

get_def <- function(node){
  text <- xmlChildren(node)$text
  if (!is.null(text)) return(xmlValue(text))
  xmlValue(xmlChildren(node)$un)
}

clear_def <- function(text){
  gsub('^:', '', text) %>%
    gsub(':$', '', .) %>%
    gsub(' :', ', ', ., perl = T) %>%
    gsub(' $', '', ., perl = T)
}

filter_def <- function(text){
  if(is.valid(text)) text
  else ''
}

read.entry_node.ee <- function(node){
  entry <- list()
  entry$fl <- value_node(node, 'fl')
  entry$gram <- value_node(node, 'def/gram')
  entry$phonetic <- value_node(node, 'pr')
  
  entry$def <- getNodeSet(node, 'def/dt') %>% sapply(get_def) %>%
    na.omit  %>% filter_def %>% clear_def %>% paste(., collapse = '; ')
  entry$sentence <- getNodeSet(node, 'def/dt/vi') %>%
    lapply(. ,xmlValue) %>% as.character %>% filter_def
  
  entry
}

get_entry_def.ee <- function(x){
  sprintf('%s. %s', x$fl, x$def)
}

get_entry_sentence.ee <- function(x, n = 1){
  na.omit(x$sentence[1:n])
}

read.word.ee <- function(word, generate_sentence = F){
  if(is.pharse(word)) return(read.word.ec(word))
  
  file.loc <- sprintf('etc/dictionary/ee/%s.xml', word)
  if (!file.exists(file.loc)) {
    url <- sprintf('http://www.dictionaryapi.com/api/v1/references/learners/xml/%s?key=%s', word, EE_API)
    download.file(url, destfile = file.loc)
  }
  
  data <- xmlParse(file.loc)
  if (getNodeSet(data, '//entry_list/suggestion') %>% length) return(read.word.ec(word))
  
  path <- sprintf('//entry_list/entry[contains(@id, "[") or @id="%s"]', word)
  entry_list <- getNodeSet(data, path)
  
  if (length(entry_list) == 0) {
    path <- '//entry_list/entry'
    entry_list <- getNodeSet(data, path)
    if (length(entry_list) == 0) return(read.word.ec(word))
  }
  
  query <- lapply(entry_list, read.entry_node.ee) %>% do.call(rbind, .)
  phonetic <- read.word.ec(word)$phonetic
  #phonetic <- na.omit.safe(query[,'phonetic']$phonetic)[1]
  sentence <- apply(query, 1, get_entry_sentence.ee)
  
  if (!generate_sentence) {
    list(
      word = word,
      phonetic = phonetic,
      explains = apply(query, 1, get_entry_def.ee),
      sentence = sentence
    )
  } else {
    list(
      word = word,
      phonetic = phonetic,
      explains = sentence
    )
  }
}

#####################################################
# Dictionary List Functions
#####################################################

read.list <- function(file.name){
  words <- read.csv(sprintf('usr/list/%s', file.name), header = T)
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
            ifelse(is.null(entry$phonetic) || is.na(entry$phonetic),
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

#####################################################
# Web Application Generating Functions
#####################################################

new_dictionary_list <- function(list.file = FILE_NAME){
  words <- read.list(list.file)
  unique <- unique(flatten_list(words))
  
  list(
    words = words,
    unique = unique,
    order = sort(unique),
    shuffle = sample(unique)
  )
}

fill_template <- function(x, template){
  sprintf(paste(readLines(template),collapse = ''), x)
}

unit_header <- function(x, html){
  if(html) return(html_tag(x, 'h2'))
  else return(paste(x, 
                    paste(rep('=', length(x)), collapse = ''),
                    sep = '\n')
  )
}

split_char <- function(x){
  strsplit(x, split = '')[[1]]
}

switch_list <- function(x, order){
  switch(order,
         order = x$order,
         shuffle = x$shuffle,
         x$unique)
}

generate_dictionary <- function(x, order, lang = 'EC', generate_sentence = F, html = T){
  summary_str <- '%s words in total, %s vocabulary not within the scope of examination, %s key words.'
  words_list <- switch_list(x, order)
  
  words_dictionary <- ''
  
  for (.i in words_list) {
    entry <- read.word(.i, lang = lang, generate_sentence = generate_sentence)
    
    if (html) {
      words_dictionary <- paste(
        words_dictionary,
        as.dictionary(entry, html = T),
        collapse = ''
      )
    } else {
      words_dictionary <- pasteLines(
        words_dictionary,
        as.dictionary(entry, html = T)
      )
    }
  }
  
  words_dictionary <- pasteLines(
    words_dictionary,
    sprintf(summary_str,
            length(x$unique),
            sum(sapply(x$unique,need_italic)),
            sum(sapply(x$unique,need_bold)))
  )
  
  words_dictionary
}

generate_list_by_unit <- function(x, order, html = T){
  result <- ''
  warp <- ifelse(html, '<div class="unit">%s</div>', '%s')
  if (order == 'original') {
    words <- x$words
  } else if (order == 'order') {
    words <- list()
    for (.i in x$order) {
      .group <- split_char(.i)[1] %>% toupper
      words[[.group]] <- c(words[[.group]], .i)
    }
  } else return(F)
  
  for (.class in names(x$words)) {
    .class_original <- ''
    for (.word in x$words[[.class]]) {
      .class_original <- pasteLines(
        .class_original,
        .word, html = html
      )
    }
    
    .class_original <- unit_header(.class, html = html) %>%
      paste(. , .class_original) %>%
      sprintf(warp, .)
    
    result <- paste(
      c(result, .class_original),
      collapse = ifelse(html, '', '\n')
    )
  }
  
  result
}

generate_simple_list <- function(x, order, html = T){
  words_list <- switch_list(x, order)
  
  result <- ''
  
  for(.i in words_list){
    result <- pasteLines(result, .i, html = html)
  }
  
  length(words_list) %>% sprintf('%s words in total', .) %>%
    pasteLines(result, ., html = html)
}

generate_list <- function(x, order, style, html = T){
  if (style == 'unit' && order != 'shuffle')
    return(generate_list_by_unit(x, order, html))
  else
    return(generate_simple_list(x, order, html))
}

content_warpper <- function(x, profile){
  profile <- as.list(profile)
  if (profile$type == 'list')
    content <- generate_list(x, order = profile$order, style = profile$style)
  else if (profile$type == 'sentence')
    content <- generate_dictionary(x, order = profile$order, generate_sentence = T)
  else if (profile$type == 'dictionary')
    content <- generate_dictionary(x, order = profile$order, lang = toupper(profile$lang))
  else content <- ''
  
  sprintf('<div data-list-id="%s" class="%s">%s</div>',
          profile$id, profile$type, content)
}

generate_content <- function(x, profile){
  apply(profile, 1, content_warpper, x = x) %>% paste(collapse = '')
}

generate_list_list_element <- function(profile, title_page = F){
  profile <- as.list(profile)
  if (!title_page)
    sprintf('<li data-to-list="%s">%s</li>', profile$id, profile$name)
  else
    sprintf('<li data-to-list="%s"><h1>%s</h1><p>%s %s</p></li>',
            profile$id, profile$name, profile$order, profile$type)
}

generate_list_list <- function(profile, title_page = F){
  apply(profile, 1, generate_list_list_element, title_page = title_page) %>% 
    paste(collapse = '')
}

generate_application <- function(list.file, profile.file = 'default.csv'){
  profile <- read.profile(profile.file)
  dict_list <- new_dictionary_list(list.file = list.file)
  
  dict_content <- generate_content(dict_list, profile)
  list_content <- generate_list_list(profile)
  title_list_content <- generate_list_list(profile, T)
  
  readLines('etc/template/webApplication.html') %>% paste(collapse = '\n') %>%
    sub_utf('\\{\\{DICT_CONTENT\\}\\}', dict_content, .) %>%
    sub_utf('\\{\\{LIST_CONTENT\\}\\}', list_content, .) %>%
    sub_utf('\\{\\{TITLE_LIST_CONTENT\\}\\}', title_list_content, .)
}

o_name <- function(x, dir.name = as.numeric(Sys.time())){
  dir_name <- sprintf('usr/output/%s', dir.name)
  if (!dir.exists(dir_name)) dir.create(dir_name)
  sprintf('usr/output/%s/%s', dir.name, x)
}

write.application <- function(list.file, 
                              profile.file = 'default.csv',
                              dir.name = as.numeric(Sys.time())){
  generate_application(list.file, profile.file) %>%
    writeLines(o_name('application.html', dir.name), useBytes = T)
}
