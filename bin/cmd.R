rm(list = ls())
Sys.setlocale("LC_CTYPE","chinese")
Sys.setenv(LANG = "en")

source('bin/lib/function.R', encoding = 'utf8')

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
  LAST_COMMAND <<- strsplit(command, ' ')[[1]]
  program <- strsplit(command, ' ')[[1]][1]
  if (command == 'debug') return(F)
  if (command == 'q') q()
  if (!program %in% names(command_list)) {
    cat('Command not found!\n')
  } else {
    source(command_list[[program]], encoding = 'utf8')
  }
  command_line()
}

cls()
cat(' Word Challange System \n=======================\nVersion: 1.0\nAuthor: Losses Don\n')
cat(sprintf('R Version: %s\n', version$version.string))
cat('Dictionary API Provied By: Youdao Dict, Merriam-Webster\n\n')
command_line()