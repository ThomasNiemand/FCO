####ToDo####
#Ideen:
#Schreibe print fuer recommend-Funktionen

#Package root:
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Publikationen/Freiheitsgradebeitrag/Cutoffs/FCO/FCO")

#Checking:
devtools::install_deps()
devtools::test()
devtools::test_coverage()
devtools::run_examples()
devtools::check()
rhub::check_for_cran()
usethis::use_cran_comments()
devtools::check_rhub()
devtools::check_win_devel()
devtools::document()
devtools::spell_check()
devtools::check()
#https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package/
library(goodpractice)
goodpractice::gp()
inteRgrate::check_pkg()
inteRgrate::check_lintr()
usethis::use_tidy_description()
inteRgrate::check_r_filenames()
inteRgrate::check_gitignore()

#Release / Update:
devtools::release()

##Publish on GitHub:
#Copy the files in the package root "~/Library/Mobile Documents/com~apple~CloudDocs/Publikationen/Freiheitsgradebeitrag/Cutoffs/FCO/FCO"
#...to the folder Repo FCO/FCO/
#Stage the changes in the GitHub tool (e.g., GitKraken)
#Commit it, give it a name like FCO 0.8.0
#Push (commit) to publish it on GitHub
