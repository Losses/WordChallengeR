#--------------------------------------------------------
#  DICTIONARY READER
#  By Losses Don @ 2016-07-10
#--------------------------------------------------------
#
# command:
#   get [word]
#

paste(LAST_COMMAND[-1], collapse = ' ') %>% read.word %>% as.dictionary %>% cat
cat('\n')