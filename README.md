 WordChallangeR
================

Hate to recite the f**king vocabulary again, again, again, and again?

![I know that feel bro.](http://7fvi7e.com1.z0.glb.clouddn.com/notalone)

But nobody can install the dictionary.apk to your brain, so you still have to 
recite it again, again and again, just like a masochist :D.

This tool can't give you a better brain, but it can save your time on organizing
the vocabulary list.

wcR can:
* Generating PDF format vocabulary list.
* Generating web application for mobile device, so you do not need to install
  specific app for learning vocabulary.
* Making test paper for you to check the learning result.
* And some small toys :P.

 Installation and config
-------------------------
* Download the wcR from [github](https://github.com/Losses/WordChallengeR.git).
* Install [Prince](http://www.princexml.com/download/) and 
   [R](https://www.r-project.org/) on your system.
* Apply for API from [Youdao](http://fanyi.youdao.com/openapi?path=data-mode)
   and [Merriam-Webster](http://www.dictionaryapi.com/)
   **Notice: On Merriam-webster Developer Center, there are many type of APIs,
   but wcR ONLY support 'Merriam-Webster's Learner's Dictionary with Audio', 
   so you should require for this API.**
* Rename `usr/sample.config.R` to `usr/config.R`, and edit it: Fill the Youdao's
  API key and ‘key from’ like:

        EC_API <- '0000000000'
        EC_KEY_FROM <- 'Linda_wcR'

   and Fill the Merriam-Webster's API key like:

        EE_API <- '00000000-0000-0000-0000-000000000000'

* Let wcR know how to call the Prince, like:

        PDF_GENERATOR_CALL <- '../bin/prince/bin/prince'

* Now, start .Rprofile(it's a file in the wcR's folder) with R, like:

        R .Rprofile

 Get started
-------------
Now you can see `$wc>`on your screen, everything start here.

### Create a new vocabulary list
* Type `new [list name]` to create a new list, like:

        new myList

* `$new/? (0,0) >` was printed on the screen. `/?` is the name of current unit,
  we haven't set any unit now, so it leaves a question mark. `(0,0)` is the 
  position of the word, the first number means the # of unit, the second number
  means the # of the following word you'd input.
* We need a name for the first unit, so input `>[unit name]`, like:

        >UnitOne

  Make sure the unit name ONLY have alphabets, without number, space, Chinese
  characters or others. `>[unit name]` is the command for switching between 
  units or create a new unit.
 * Type a word normally to add a new word.
 * Type `:v` to see all words in the current list.
 * `:q` to save the list, and quit the new tool.
 * `:fq` to quit the new tool without saving the list.
 * The new tool will save the current workspace automatically, so if the program
   crashed or you leaved the new tool by using the command `:fq`, you can simply
   enter the new tool by the command `new`, and type `1` to recover the workspace.
   
### Generating a vocabulary list
 * The command `dictionary [list name]` is for creating a new vocabulary list.

If you want to know all the feature of wcR, please see `User guide.pdf`.

 License
---------
WordChallangeR - A tool which can generate vocabulary list

Copyright (C) 2016 Losses Don

This program is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software 
Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY 
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
PARTICULAR PURPOSE. See the GNU General Public License for more details.
You should have received a copy of the GNU General Public License along with this 
program. If not, see <http://www.gnu.org/licenses/>.
