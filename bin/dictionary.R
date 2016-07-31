#--------------------------------------------------------
#  DICTION GENERATOR
#  By Losses Don @ 2016-07-09
#--------------------------------------------------------
#
# command:
#   dictionary (listFileName) (profileFileName) (dirName)
#

write.application(
  if_command(2, get_file(), T),
  if_command(3, 'default.csv', T),
  if_command(4, Sys.time() %>% as.numeric),
  force = T
)