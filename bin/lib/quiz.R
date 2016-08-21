#--------------------------------------------------------
#  FUNCTIONS FOR QUIZ
#  By Losses Don @ 2016-07-07
#--------------------------------------------------------

show_question <- function(word){
  cls()
  cat(word)
  status <- yn('\nKnow the word or not(1/0): ')
  cls()
  cat(as.dictionary(read.word(word), lib_mark = F))
  if (status == 0) {
    readline('Press any key to continue...')
    return (-1)
  }
  status <- yn('\nRight or not(1/0): ')
  if(status == 0) return (-2)
  else return (1)
}

load_paper <- function(){
  word_list <- read.list(get_file())
  paper <<- data.frame(
    word = unique(sample(flatten_list(word_list))),
    status = 0
  )
  
  save(paper, file = '.quizPaper')
  start_quiz()
}

generate_report <- function(){
  if (nrow(paper[paper$status == 0, ]) != 0){
    return(F)
  } else {
    cls()
    total <- nrow(paper)
    left <- sum(paper$status %in% c(-1,-2))
    mark <- round(100 - left / total * 100, 1)
    cat(sprintf('%s words in on paper, %s words left.\nFinished %s percent.\n', total, left, mark))
    cat('Generating list...')
    dn <- as.character(paper[paper$status == -1, ]$word)
    wrong <- as.character(paper[paper$status == -2, ]$word)
    max_length <- max(c(length(dn), length(wrong)))
    report <- data.frame(
      dn = c(dn, character(max_length - length(dn))),
      wrong = c(wrong, character(max_length - length(wrong)))
    )
    time <- as.numeric(Sys.time())
    write.csv(x = report, file = sprintf('usr/output/quiz_%s.csv', time), row.names = F)
    if(file.exists('.quizPaper')) file.rename(from = '.quizPaper', to = sprintf('.paper_%s', time))
    cat(' done\n')
    return(T)
  }
}

start_quiz <- function(){
  for(.word in paper[paper$status == 0, ]$word){
    paper[paper$word == .word, 'status'] <<- show_question(.word)
    save(paper, file = '.quizPaper')
  }
  
  generate_report()
}
