#--------------------------------------------------------
#  FUNCTIONS TO PRINT THE MEANING OF WORDS
#  By Losses Don @ 2016-08-21
#--------------------------------------------------------

print_dictionary <- function(x){
  EE <- read.word(x, lang = 'EE')
  EC <- read.word(x, lang = 'EC')
  sentence <- read.word(x, generate_sentence = T)
  
  title <- paste0(x, ' /', EC$phonetic, '/')
  title_len <- strlength(title)
  title_len <- title_len + 8
  border_top_bottom <- repstr('-', title_len)
  paste0('\n +', border_top_bottom, '+\n ', '|    ', title, '    |\n +', border_top_bottom, '+') %>% cat
  cat('\n\n')
  
  paste0(green(' English - Chinese Translation \n'), reset(), repstr('~', 31), '\n') %>% cat
  paste(EC$explains, collapse = '\n') %>% cat
  
  cat('\n\n')
  
  paste0(green(' English - English Translation \n'), reset(), repstr('~', 31), '\n') %>% cat
  paste(EE$explains, collapse = '\n') %>% cat
  
  cat('\n\n')
  
  paste0(green(' Example Sentences \n'), reset(), repstr('~', 19), '\n') %>% cat
  paste(sentence$explains, collapse = '\n') %>% cat
  
  cat('\n\n\n')
  
  paste0(silver('English - Chinese translation provided by Youdao, English - English translation and sentences provided by Merriam-Webster.')) %>% cat
}
