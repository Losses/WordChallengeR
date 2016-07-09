#--------------------------------------------------------
#  LIST CREATOR
#  By Losses Don @ 2016-07-09
#--------------------------------------------------------
#
# command:
#   new (listFileName)
#

new_list(
  file.name = if_command(2, readline('List name: '))
)