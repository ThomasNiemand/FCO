####ToDo####
#sapply -> vapply done
#nrow, ncol -> seq_len, seq_along done
#T, F -> TRUE, FALSE done
#unit tests? add for helper functions constr_mod

#Package root:
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Publikationen/Freiheitsgradebeitrag/Cutoffs/FCO/FCO")

#To be done
devtools::install_deps()
devtools::test()
devtools::test_coverage()
devtools::run_examples()
devtools::check()
results <- rhub::check_for_cran()
results$cran_summary()
usethis::use_cran_comments()
devtools::check_rhub()
devtools::check_win_devel()
devtools::document()
devtools::spell_check()
devtools::check()

results <- rhub::check_for_cran("FCO")
results$cran_summary()
usethis::use_cran_comments()
#https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package/
#update cran_comments
library(goodpractice)
goodpractice::gp()
inteRgrate::check_pkg()
inteRgrate::check_lintr()
usethis::use_tidy_description()
inteRgrate::check_r_filenames()
inteRgrate::check_gitignore()

#Overall level not working so far
devtools::test_coverage("FCO")
covr::package_coverage("FCO")

##Unit tests working

#index guess
usethis::use_test("index_guess")
covr::file_coverage("R/index_guess.R", "tests/testthat/test-index_guess.R")

#gen_fit
usethis::use_test("gen_fit")
#because file-based coverage does not know the environment
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Publikationen/Freiheitsgradebeitrag/Cutoffs/FCO/FCO/R/")
sapply(list.files(), source)
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Publikationen/Freiheitsgradebeitrag/Cutoffs/FCO/FCO")
covr::file_coverage("R/gen_fit.R", "tests/testthat/test-gen_fit.R")

#pop_mod
usethis::use_test("pop_mod")
covr::file_coverage("R/pop_mod.R", "tests/testthat/test-pop_mod.R")

#flex_co
usethis::use_test("flex_co")
covr::file_coverage("R/flex_co.R", "tests/testthat/test-flex_co.R")

#recommend
usethis::use_test("recommend")
covr::file_coverage("R/recommend.R", "tests/testthat/test-recommend.R")

#recommend_dv
usethis::use_test("recommend_dv")
covr::file_coverage("R/recommend_dv.R", "tests/testthat/test-recommend_dv.R")

#pop_mod_dv
usethis::use_test("pop_mod_dv")
covr::file_coverage("R/pop_mod_dv.R", "tests/testthat/test-pop_mod_dv.R")

#check_std
usethis::use_test("check_std")
covr::file_coverage("R/check_std.R", "tests/testthat/test-check_std.R")

#get_free
usethis::use_test("get_free")
covr::file_coverage("R/get_free.R", "tests/testthat/test-get_free.R")

#constr_mod
usethis::use_test("constr_mod")
covr::file_coverage("R/constr_mod.R", "tests/testthat/test-constr_mod.R")

