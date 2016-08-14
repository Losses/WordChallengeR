 WordChallangeR
================

Hate to recite the f**king vocabulary again, again, again, and again?

![I know that feel bro.](http://7fvi7e.com1.z0.glb.clouddn.com/notalone)

But nobody can install the dictionary.apk to your brain, so you still have to 
recite it again, again and again, just like a masochist :D.

This tool can't give you a better brain, but it can save your time onorganizing
the vocabulary list.

wcR can:
* Generating PDF format vocabulary list.
* Generating web application for mobile device, so you do not need to install
  specific app for learning vocabulary.
* Making test paper for you to check the learning result.
* And some small toys :P.

 Installation and config
-------------------------
1. Download the wcR from [github](https://github.com/Losses/WordChallengeR.git)
2. Install [Prince]() and R on your system.
3. Apply for API from [Youdao](http://fanyi.youdao.com/openapi?path=data-mode)
   and [Merriam-Webster](http://www.dictionaryapi.com/)
   **Notice: On Merriam-webster Developer Center, there are many type of APIs,
   but wcR ONLY support 'Merriam-Webster's Learner's Dictionary with Audio', 
   so you should require for this API.**
4. Rename `usr/sample.config.R` to `usr/config.R`, and edit it: Fill the Youdao's
   API in the first line like: 

        EC_API <- '0000000000'

   and Fill the Merriam-Webster's API in the second like:

        EE_API <- '00000000-0000-0000-0000-000000000000'

5. Let wcR know how to call the Prince, like:

        PDF_GENERATOR_CALL <- '../bin/prince/bin/prince'

6. Now, start .Rprofile(it's a file in the wcR's folder) with R, like:

        R .Rprofile
