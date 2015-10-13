#' prostateCancerMain
#' 
#' This script installs all the necessary module used in this model
#' 
#' Author: Yeeleng Scott Vang (ysvang@uci.edu)

# standard R modules
install.packages('survival')
install.packages('pec')
install.packages('MASS')
install.packages('muhaz')
install.packages('caret')
install.packages('matrixStats')
install.packages('glmnet')
install.packages('devtools')
library('devtools')

# may have to create a folder called "gbmci" in the directory where this script is running from before running this line
with_lib(new="gbmci", install_github('uci-cbcl/GBMCI')) # installs the GBMCI module from github repository


