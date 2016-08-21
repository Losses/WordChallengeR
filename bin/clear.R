#--------------------------------------------------------
#  OUTPUT DIR CLEANING TOOL
#  By Losses Don @ 2016-07-31
#--------------------------------------------------------

if(readline('This action will clear all the output content including dictionary and quiz, input OK to clear.') == 'OK'){
  clear_output()
} else {
  cat('You canceled this action.\n')
}
