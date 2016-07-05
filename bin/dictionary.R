FILE_NAME <- get_file()
ORDERED_DICTIONARY <- yn('Generating order dictionary (1/0): ') == 1
SESSION <- as.numeric(Sys.time())

words <- read.list(FILE_NAME)
words_unique <- unique(flatten_list(words))
words_order <- sort(words_unique)

#words_shuffle <- list()
#for (.i in 1:5){
#  words_shuffle[[sprintf('list%s', .i)]] <- sample(words_unique)
#}

words_dictionary <- ''

for (.i in ifelse(ORDERED_DICTIONARY,  list(words_order), list(words_unique))[[1]]){
  entry <- read.word(.i)
  
#  words_dictionary <- pasteLines(
#    words_dictionary,
#    as.dictionary(entry, html = T)
#  )
  
  words_dictionary <- paste(
    words_dictionary,
    as.dictionary(entry, html = T),
    collapse = ''
  )
}

words_dictionary <- pasteLines(
  words_dictionary,
  sprintf('%s words in total, %s vocabulary not within the scope of examination, %s key words.',
          length(words_unique), sum(sapply(words_unique,need_italic)), sum(sapply(words_unique,need_bold)))
)

words_dictionary <- sprintf(paste(readLines('etc/template/dictionary.html'),collapse = ''), words_dictionary)

original_list <- ''

for(.class in names(words)){
  .class_original <- ''
  for(.word in words[[.class]]){
    .class_original <- pasteLines(
      .class_original,
      .word, html = T
    )
  }
  
  .class_original <- sprintf('<div class="unit">%s</div>',paste(html_tag(.class, 'h2'), .class_original))
  
  original_list <- paste(
    c(original_list, .class_original),
    collapse = ''
  )
}

original_list <- sprintf(paste(readLines('etc/template/original.html'),collapse = ''), original_list)

order_list <- ''
for(.i in words_order){
  order_list <- pasteLines(order_list, .i, html = T)
}
order_list <- pasteLines(order_list, sprintf('%s words in total', length(words_order)), html = T)
order_list <- sprintf(paste(readLines('etc/template/original.html'),collapse = ''), order_list)

o_name <- function(name){
  dir_name <- sprintf('usr/output/%s', SESSION)
  if(!dir.exists(dir_name)) dir.create(dir_name)
  sprintf('usr/output/%s/%s', SESSION, name)
}

writeLines(order_list, o_name('order.html'), useBytes = T)
writeLines(original_list, o_name('original.html'), useBytes = T)
writeLines(words_dictionary, o_name('dictionary.html'), useBytes = T)

#writeLines(paste(words_unique, collapse = '\n'), o_name('unique.txt'))
#write.csv(words, file = o_name('original'), row.names = F)
#write.csv(words_shuffle, file = o_name('shuffle.csv'), row.names = F)
