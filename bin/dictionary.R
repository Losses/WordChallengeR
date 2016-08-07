#--------------------------------------------------------
#  DICTION GENERATOR
#  By Losses Don @ 2016-07-09
#--------------------------------------------------------
#
# command:
#   dictionary (listFileName) (profileFileName) (dirName)
#

(function(){
  list.file <- if_command(2, get_file(), T)
  dir.name <- sprintf('[%s]%s', format(Sys.time(), format='%m-%d~%H.%M.%S'), list.file)
  
  new_dictionary(
    list.file = list.file,
    profile.file = if_command(3, 'default.csv', T),
    dir.name = dir.name,
    force = T
  )
})()
