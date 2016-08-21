if(!file.exists('.quizPaper')){
  load_paper()
} else {
  load('.quizPaper')
  if (generate_report()) load_paper()
  else start_quiz()
}
