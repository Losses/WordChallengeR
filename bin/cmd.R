rm(list = ls())
source('bin/lib/function.R', encoding = 'utf8')

Sys.setlocale("LC_CTYPE","chinese")
Sys.setenv(LANG = "en")

standard_lib <- readLines('etc/standardLib.txt')
WORD_CACHE <- build_word_cache()

command_list <- list(
  dictionary = 'bin/dictionary.R',
  quiz = 'bin/quiz.R',
  summary = 'bin/summary.R',
  get = 'bin/get.R'
)

command_line <- function(){
  command <- readline('$WC > ')
  LAST_COMMAND <<- command
  program <- strsplit(command, ' ')[[1]][1]
  if(command == 'q') return(F)
  if(!program %in% names(command_list)){
    cat('Command not found!\n')
  } else {
    source(command_list[[program]], encoding = 'utf8')
  }
  command_line()
}

cls()
cat(' Word Challange System \n=======================\nVersion:1.0\nAuthor:Losses Don\n')
command_line()