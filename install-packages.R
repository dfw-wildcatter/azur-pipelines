## ensure user library exists, or else R will attempt to install to /usr/local/lib
print(Sys.getenv("R_LIBS_USER"))
print(getwd())

dir.create("library")

#if(!dir.exists(Sys.getenv("R_LIBS_USER"))) {
# dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE) }

# ensure /usr/local/lib/R/site-library is writeable first
#install.packages("xgboost", repos="https://cran.rstudio.com", lib="library")
install.packages("xgboost", repos='https://mran.microsoft.com/snapshot/2019-09-20/', lib="library")
install.packages("magrittr", repos="https://cran.rstudio.com", lib="library")
install.packages("tidyverse", repos="https://cran.rstudio.com", lib="library")
install.packages("RcppRoll", repos="https://cran.rstudio.com", lib="library")
install.packages("devtools", repos="https://cran.rstudio.com", lib="library")
devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.22/catboost-R-Linux-0.22.tgz',
                      INSTALL_opts = c('--no-multiarch'))
 