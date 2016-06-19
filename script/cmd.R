Sys.setlocale("LC_CTYPE","chinese")
Sys.setenv(LANG = "en")
source('./script/function.R', encoding = 'utf8')

command_list <- list(
  dictionary = './script/dictionary.R',
  quiz = './script/quiz.R',
  summary = './script/summary.R',
  get = './script/get.R'
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

source('./script/function.R', encoding = 'utf8')

cls()
cat(' Word Challange System \n=======================\nVersion:1.0\nAuthor:Losses Don\n')
command_line()