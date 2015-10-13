#' trainTablePreprocess
#' 
#' Performs various data cleanups and prepares training table for modeling
#' 
#' Author: Yeeleng Scott Vang (ysvang@uci.edu)
#================================================================================================================
# This script reads in the training table csv file and applies preprocessing to obtain a useable design matrix.
# Covariates are examined and processed in order.  
# When covariates are available in only a subset of tests, it will be deleted.
# For binary covariates, characters will be mapped to ZEROs and ONEs.
# For categorical covariates, training set and final set will be mapped accordingly.
# For categorical covariates, missing data are replaced with the most frequent category.
# For real-valued covariates, missing data are replaced with averages of that covariate.

rm(list=ls()) # clean enviornment variables

# read in data file 
trainingTable <- read.csv("data\\CoreTable_training.csv")
# create duplicate dataframe from trainingTable for modification
tDF <- trainingTable
# delete DOMAIN, STUDYID, RPT columns not relevant to design matrix
tDF$DOMAIN <- NULL
tDF$STUDYID <- NULL
tDF$RPT <- NULL

# replace DEATH with ZEROs and ONEs
tDF$DEATH <- sub("^$", 0, tDF$DEATH)
tDF$DEATH <- sub("YES", 1, tDF$DEATH)
tDF$DEATH <- strtoi(tDF$DEATH) # convert from char to int


# delete PER_REF/LKADT_REF
tDF$PER_REF <-NULL
tDF$LKADT_REF <-NULL
# delete LKADT_PER since all rows says "DAYS"
tDF$LKADT_PER <-NULL
# delete GLEAS_DX, TSTAG_DX since only available for one test (ASCENT2)
tDF$GLEAS_DX <- NULL
tDF$TSTAG_DX <- NULL
# delete AGEGRP
tDF$AGEGRP <- NULL


# delete columns as either data not available for all tests or is uninformative
tDF$REGION_C <- NULL
tDF$SMOKE <- NULL
tDF$SMOKFREQ <- NULL
tDF$SMOKSTAT <- NULL
tDF$TRT1_ID <- NULL
tDF$TRT2_ID <- NULL
tDF$TRT3_ID <- NULL
tDF$LDH <- NULL
tDF$TESTO <- NULL
tDF$CREACL <- NULL
tDF$NA. <- NULL
tDF$MG <- NULL
tDF$PHOS <- NULL
tDF$ALB <- NULL
tDF$TPRO <- NULL
tDF$RBC <- NULL
tDF$LYM <- NULL
tDF$BUN <- NULL
tDF$CCRC <- NULL
tDF$GLU <- NULL
tDF$CREACLCA <- NULL



# Substitue ZEROs and ONEs for binary features
tDF$NON_TARGET <- sub("^$", 0, tDF$NON_TARGET)
tDF$NON_TARGET <- sub("Y", 1, tDF$NON_TARGET)
tDF$NON_TARGET <- strtoi(tDF$NON_TARGET) 
tDF$TARGET <- sub("^$", 0, tDF$TARGET)
tDF$TARGET <- sub("Y", 1, tDF$TARGET)
tDF$TARGET <- strtoi(tDF$TARGET) 
tDF$BONE <- sub("^$", 0, tDF$BONE)
tDF$BONE <- sub("Y", 1, tDF$BONE)
tDF$BONE <- strtoi(tDF$BONE) 
tDF$RECTAL <- sub("^$", 0, tDF$RECTAL)
tDF$RECTAL <- sub("Y", 1, tDF$RECTAL)
tDF$RECTAL <- strtoi(tDF$RECTAL) 
tDF$LYMPH_NODES <- sub("^$", 0, tDF$LYMPH_NODES)
tDF$LYMPH_NODES <- sub("Y", 1, tDF$LYMPH_NODES)
tDF$LYMPH_NODES <- strtoi(tDF$LYMPH_NODES) 
tDF$KIDNEYS <- sub("^$", 0, tDF$KIDNEYS)
tDF$KIDNEYS <- sub("Y", 1, tDF$KIDNEYS)
tDF$KIDNEYS <- strtoi(tDF$KIDNEYS) 
tDF$LUNGS <- sub("^$", 0, tDF$LUNGS)
tDF$LUNGS <- sub("Y", 1, tDF$LUNGS)
tDF$LUNGS  <- strtoi(tDF$LUNGS) 
tDF$LIVER <- sub("^$", 0, tDF$LIVER)
tDF$LIVER <- sub("Y", 1, tDF$LIVER)
tDF$LIVER <- strtoi(tDF$LIVER) 
tDF$PLEURA <- sub("^$", 0, tDF$PLEURA)
tDF$PLEURA <- sub("Y", 1, tDF$PLEURA)
tDF$PLEURA <- strtoi(tDF$PLEURA) 
tDF$OTHER <- sub("^$", 0, tDF$OTHER)
tDF$OTHER <- sub("Y", 1, tDF$OTHER)
tDF$OTHER <- strtoi(tDF$OTHER) 
tDF$PROSTATE <- sub("^$", 0, tDF$PROSTATE)
tDF$PROSTATE <- sub("Y", 1, tDF$PROSTATE)
tDF$PROSTATE <- strtoi(tDF$PROSTATE) 
tDF$ADRENAL <- sub("^$", 0, tDF$ADRENAL)
tDF$ADRENAL <- sub("Y", 1, tDF$ADRENAL)
tDF$ADRENAL <- strtoi(tDF$ADRENAL) 
tDF$BLADDER <- sub("^$", 0, tDF$BLADDER)
tDF$BLADDER <- sub("Y", 1, tDF$BLADDER)
tDF$BLADDER <- strtoi(tDF$BLADDER) 
tDF$PERITONEUM <- sub("^$", 0, tDF$PERITONEUM)
tDF$PERITONEUM <- sub("Y", 1, tDF$PERITONEUM)
tDF$PERITONEUM <- strtoi(tDF$PERITONEUM) 
tDF$COLON <- sub("^$", 0, tDF$COLON)
tDF$COLON <- sub("Y", 1, tDF$COLON)
tDF$COLON <- strtoi(tDF$COLON) 
tDF$COLON <- sub("NA", 0, tDF$COLON)


tDF$HEAD_AND_NECK <- NULL

tDF$SOFT_TISSUE <- sub("^$", 0, tDF$SOFT_TISSUE)
tDF$SOFT_TISSUE <- sub("Y", 1, tDF$SOFT_TISSUE)
tDF$SOFT_TISSUE <- strtoi(tDF$SOFT_TISSUE) 

tDF$STOMACH <- NULL
tDF$PANCREAS <- NULL
tDF$THYROID <- NULL
tDF$ABDOMINAL <- NULL 

tDF$ORCHIDECTOMY <- sub("^$", 0, tDF$ORCHIDECTOMY)
tDF$ORCHIDECTOMY <- sub("Y", 1, tDF$ORCHIDECTOMY)
tDF$ORCHIDECTOMY <- strtoi(tDF$ORCHIDECTOMY) 
tDF$PROSTATECTOMY <- sub("^$", 0, tDF$PROSTATECTOMY)
tDF$PROSTATECTOMY <- sub("Y", 1, tDF$PROSTATECTOMY)
tDF$PROSTATECTOMY <- strtoi(tDF$PROSTATECTOMY) 
tDF$TURP <- sub("^$", 0, tDF$TURP)
tDF$TURP <- sub("Y", 1, tDF$TURP)
tDF$TURP <- strtoi(tDF$TURP) 
tDF$LYMPHADENECTOMY <- sub("^$", 0, tDF$LYMPHADENECTOMY)
tDF$LYMPHADENECTOMY <- sub("Y", 1, tDF$LYMPHADENECTOMY)
tDF$LYMPHADENECTOMY <- strtoi(tDF$LYMPHADENECTOMY) 
tDF$SPINAL_CORD_SURGERY <- sub("^$", 0, tDF$SPINAL_CORD_SURGERY)
tDF$SPINAL_CORD_SURGERY <- sub("Y", 1, tDF$SPINAL_CORD_SURGERY)
tDF$SPINAL_CORD_SURGERY <- strtoi(tDF$SPINAL_CORD_SURGERY) 
tDF$BILATERAL_ORCHIDECTOMY <- sub("^$", 0, tDF$BILATERAL_ORCHIDECTOMY)
tDF$BILATERAL_ORCHIDECTOMY <- sub("Y", 1, tDF$BILATERAL_ORCHIDECTOMY)
tDF$BILATERAL_ORCHIDECTOMY <- strtoi(tDF$BILATERAL_ORCHIDECTOMY) 
tDF$PRIOR_RADIOTHERAPY <- sub("^$", 0, tDF$PRIOR_RADIOTHERAPY)
tDF$PRIOR_RADIOTHERAPY <- sub("Y", 1, tDF$PRIOR_RADIOTHERAPY)
tDF$PRIOR_RADIOTHERAPY <- strtoi(tDF$PRIOR_RADIOTHERAPY) 

tDF$ANALGESICS <- sub("^$", 0, tDF$ANALGESICS)
tDF$ANALGESICS <- sub("YES", 1, tDF$ANALGESICS)
tDF$ANALGESICS <- strtoi(tDF$ANALGESICS) 
tDF$ANTI_ANDROGENS <- sub("^$", 0, tDF$ANTI_ANDROGENS)
tDF$ANTI_ANDROGENS <- sub("YES", 1, tDF$ANTI_ANDROGENS)
tDF$ANTI_ANDROGENS <- strtoi(tDF$ANTI_ANDROGENS) 
tDF$GLUCOCORTICOID <- sub("^$", 0, tDF$GLUCOCORTICOID)
tDF$GLUCOCORTICOID <- sub("YES", 1, tDF$GLUCOCORTICOID)
tDF$GLUCOCORTICOID <- strtoi(tDF$GLUCOCORTICOID) 
tDF$GONADOTROPIN <- sub("^$", 0, tDF$GONADOTROPIN)
tDF$GONADOTROPIN <- sub("YES", 1, tDF$GONADOTROPIN)
tDF$GONADOTROPIN <- strtoi(tDF$GONADOTROPIN) 
tDF$BISPHOSPHONATE <- sub("^$", 0, tDF$BISPHOSPHONATE)
tDF$BISPHOSPHONATE <- sub("YES", 1, tDF$BISPHOSPHONATE)
tDF$BISPHOSPHONATE <- strtoi(tDF$BISPHOSPHONATE) 
tDF$CORTICOSTEROID <- sub("^$", 0, tDF$CORTICOSTEROID)
tDF$CORTICOSTEROID <- sub("YES", 1, tDF$CORTICOSTEROID)
tDF$CORTICOSTEROID <- strtoi(tDF$CORTICOSTEROID) 
tDF$IMIDAZOLE <- sub("^$", 0, tDF$IMIDAZOLE)
tDF$IMIDAZOLE <- sub("YES", 1, tDF$IMIDAZOLE)
tDF$IMIDAZOLE <- strtoi(tDF$IMIDAZOLE) 
tDF$ACE_INHIBITORS <- sub("^$", 0, tDF$ACE_INHIBITORS)
tDF$ACE_INHIBITORS <- sub("YES", 1, tDF$ACE_INHIBITORS)
tDF$ACE_INHIBITORS <- strtoi(tDF$ACE_INHIBITORS) 
tDF$BETA_BLOCKING <- sub("^$", 0, tDF$BETA_BLOCKING)
tDF$BETA_BLOCKING <- sub("YES", 1, tDF$BETA_BLOCKING)
tDF$BETA_BLOCKING <- strtoi(tDF$BETA_BLOCKING) 
tDF$HMG_COA_REDUCT <- sub("^$", 0, tDF$HMG_COA_REDUCT)
tDF$HMG_COA_REDUCT <- sub("YES", 1, tDF$HMG_COA_REDUCT)
tDF$HMG_COA_REDUCT <- strtoi(tDF$HMG_COA_REDUCT) 
tDF$ESTROGENS <- sub("^$", 0, tDF$ESTROGENS)
tDF$ESTROGENS <- sub("YES", 1, tDF$ESTROGENS)
tDF$ESTROGENS <- strtoi(tDF$ESTROGENS) 
tDF$ANTI_ESTROGENS <- sub("^$", 0, tDF$ANTI_ESTROGENS)
tDF$ANTI_ESTROGENS <- sub("YES", 1, tDF$ANTI_ESTROGENS)
tDF$ANTI_ESTROGENS <- strtoi(tDF$ANTI_ESTROGENS) 

tDF$ARTTHROM <- NULL

tDF$CEREBACC <- sub("^$", 0, tDF$CEREBACC)
tDF$CEREBACC <- sub("Y", 1, tDF$CEREBACC)
tDF$CEREBACC <- strtoi(tDF$CEREBACC) 
tDF$CHF <- sub("^$", 0, tDF$CHF)
tDF$CHF <- sub("Y", 1, tDF$CHF)
tDF$CHF <- strtoi(tDF$CHF) 
tDF$DVT <- sub("^$", 0, tDF$DVT)
tDF$DVT <- sub("Y", 1, tDF$DVT)
tDF$DVT <- strtoi(tDF$DVT) 
tDF$DIAB <- sub("^$", 0, tDF$DIAB)
tDF$DIAB <- sub("Y", 1, tDF$DIAB)
tDF$DIAB <- strtoi(tDF$DIAB) 
tDF$GASTREFL <- sub("^$", 0, tDF$GASTREFL)
tDF$GASTREFL <- sub("Y", 1, tDF$GASTREFL)
tDF$GASTREFL <- strtoi(tDF$GASTREFL) 
tDF$GIBLEED <- sub("^$", 0, tDF$GIBLEED)
tDF$GIBLEED <- sub("Y", 1, tDF$GIBLEED)
tDF$GIBLEED <- strtoi(tDF$GIBLEED) 
tDF$MI <- sub("^$", 0, tDF$MI)
tDF$MI <- sub("Y", 1, tDF$MI)
tDF$MI <- strtoi(tDF$MI) 
tDF$PUD <- sub("^$", 0, tDF$PUD)
tDF$PUD <- sub("Y", 1, tDF$PUD)
tDF$PUD <- strtoi(tDF$PUD) 
tDF$PULMEMB <- sub("^$", 0, tDF$PULMEMB)
tDF$PULMEMB <- sub("Y", 1, tDF$PULMEMB)
tDF$PULMEMB <- strtoi(tDF$PULMEMB) 
tDF$PATHFRAC <- sub("^$", 0, tDF$PATHFRAC)
tDF$PATHFRAC <- sub("Y", 1, tDF$PATHFRAC)
tDF$PATHFRAC <- strtoi(tDF$PATHFRAC) 
tDF$SPINCOMP <- sub("^$", 0, tDF$SPINCOMP)
tDF$SPINCOMP <- sub("Y", 1, tDF$SPINCOMP)
tDF$SPINCOMP <- strtoi(tDF$SPINCOMP) 
tDF$COPD <- sub("^$", 0, tDF$COPD)
tDF$COPD <- sub("Y", 1, tDF$COPD)
tDF$COPD <- strtoi(tDF$COPD) 


tDF$MHBLOOD <- sub("^$", 0, tDF$MHBLOOD)
tDF$MHBLOOD <- sub("YES", 1, tDF$MHBLOOD)
tDF$MHBLOOD <- strtoi(tDF$MHBLOOD) 
tDF$MHCARD <- sub("^$", 0, tDF$MHCARD)
tDF$MHCARD <- sub("YES", 1, tDF$MHCARD)
tDF$MHCARD <- strtoi(tDF$MHCARD) 
tDF$MHCONGEN <- sub("^$", 0, tDF$MHCONGEN)
tDF$MHCONGEN <- sub("YES", 1, tDF$MHCONGEN)
tDF$MHCONGEN <- strtoi(tDF$MHCONGEN) 
tDF$MHEAR <- sub("^$", 0, tDF$MHEAR)
tDF$MHEAR <- sub("YES", 1, tDF$MHEAR)
tDF$MHEAR <- strtoi(tDF$MHEAR) 
tDF$MHENDO <- sub("^$", 0, tDF$MHENDO)
tDF$MHENDO <- sub("YES", 1, tDF$MHENDO)
tDF$MHENDO <- strtoi(tDF$MHENDO) 
tDF$MHEYE <- sub("^$", 0, tDF$MHEYE)
tDF$MHEYE <- sub("YES", 1, tDF$MHEYE)
tDF$MHEYE <- strtoi(tDF$MHEYE) 
tDF$MHGASTRO <- sub("^$", 0, tDF$MHGASTRO)
tDF$MHGASTRO <- sub("YES", 1, tDF$MHGASTRO)
tDF$MHGASTRO <- strtoi(tDF$MHGASTRO) 
tDF$MHGEN <- sub("^$", 0, tDF$MHGEN)
tDF$MHGEN <- sub("YES", 1, tDF$MHGEN)
tDF$MHGEN <- strtoi(tDF$MHGEN) 
tDF$MHHEPATO <- sub("^$", 0, tDF$MHHEPATO)
tDF$MHHEPATO <- sub("YES", 1, tDF$MHHEPATO)
tDF$MHHEPATO <- strtoi(tDF$MHHEPATO) 
tDF$MHIMMUNE <- sub("^$", 0, tDF$MHIMMUNE)
tDF$MHIMMUNE <- sub("YES", 1, tDF$MHIMMUNE)
tDF$MHIMMUNE <- strtoi(tDF$MHIMMUNE) 
tDF$MHINFECT <- sub("^$", 0, tDF$MHINFECT)
tDF$MHINFECT <- sub("YES", 1, tDF$MHINFECT)
tDF$MHINFECT <- strtoi(tDF$MHINFECT) 
tDF$MHINJURY <- sub("^$", 0, tDF$MHINJURY)
tDF$MHINJURY <- sub("YES", 1, tDF$MHINJURY)
tDF$MHINJURY <- strtoi(tDF$MHINJURY) 
tDF$MHINVEST <- sub("^$", 0, tDF$MHINVEST)
tDF$MHINVEST <- sub("YES", 1, tDF$MHINVEST)
tDF$MHINVEST <- strtoi(tDF$MHINVEST) 
tDF$MHMETAB <- sub("^$", 0, tDF$MHMETAB)
tDF$MHMETAB <- sub("YES", 1, tDF$MHMETAB)
tDF$MHMETAB <- strtoi(tDF$MHMETAB) 
tDF$MHMUSCLE <- sub("^$", 0, tDF$MHMUSCLE)
tDF$MHMUSCLE <- sub("YES", 1, tDF$MHMUSCLE)
tDF$MHMUSCLE <- strtoi(tDF$MHMUSCLE) 
tDF$MHNEOPLA <- sub("^$", 0, tDF$MHNEOPLA)
tDF$MHNEOPLA <- sub("YES", 1, tDF$MHNEOPLA)
tDF$MHNEOPLA <- strtoi(tDF$MHNEOPLA) 
tDF$MHNERV <- sub("^$", 0, tDF$MHNERV)
tDF$MHNERV <- sub("YES", 1, tDF$MHNERV)
tDF$MHNERV <- strtoi(tDF$MHNERV) 
tDF$MHPSYCH <- sub("^$", 0, tDF$MHPSYCH)
tDF$MHPSYCH <- sub("YES", 1, tDF$MHPSYCH)
tDF$MHPSYCH <- strtoi(tDF$MHPSYCH) 
tDF$MHRENAL <- sub("^$", 0, tDF$MHRENAL)
tDF$MHRENAL <- sub("YES", 1, tDF$MHRENAL)
tDF$MHRENAL <- strtoi(tDF$MHRENAL) 
tDF$MHRESP <- sub("^$", 0, tDF$MHRESP)
tDF$MHRESP <- sub("YES", 1, tDF$MHRESP)
tDF$MHRESP <- strtoi(tDF$MHRESP) 
tDF$MHSKIN <- sub("^$", 0, tDF$MHSKIN)
tDF$MHSKIN <- sub("YES", 1, tDF$MHSKIN)
tDF$MHSKIN <- strtoi(tDF$MHSKIN) 
tDF$MHSOCIAL <- sub("^$", 0, tDF$MHSOCIAL)
tDF$MHSOCIAL <- sub("YES", 1, tDF$MHSOCIAL)
tDF$MHSOCIAL <- strtoi(tDF$MHSOCIAL) 
tDF$MHSURG <- sub("^$", 0, tDF$MHSURG)
tDF$MHSURG <- sub("YES", 1, tDF$MHSURG)
tDF$MHSURG <- strtoi(tDF$MHSURG) 
tDF$MHVASC <- sub("^$", 0, tDF$MHVASC)
tDF$MHVASC <- sub("YES", 1, tDF$MHVASC)
tDF$MHVASC <- strtoi(tDF$MHVASC) 


# removes rows whom does not have value for DISCONT
tDF <- tDF[(tDF$DISCONT!='.'),]
tDF$DISCONT <- NULL # delete covariate DISCONT as not addressing Q2
tDF$ENTRT_PC <- NULL # delete covariate ENTRT_PC as not addressing Q2

tDF$BMI <- as.numeric(as.character(tDF$BMI)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_BMI <- mean(tDF$BMI, na.rm = TRUE) # find average of column
tDF$BMI <- ifelse(is.na(tDF$BMI), avg_BMI, tDF$BMI) # replace 'na' with average


tDF$ALP <- as.numeric(as.character(tDF$ALP)) # change datatype from factor to numeric. automatically replaces '.' with 'na''
avg_ALP <- mean(tDF$ALP, na.rm = TRUE) # find average of column
tDF$ALP <- ifelse(is.na(tDF$ALP), avg_ALP, tDF$ALP) # replace 'na' with average


tDF$ALT <- as.numeric(as.character(tDF$ALT)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_ALT <- mean(tDF$ALT, na.rm = TRUE) # find average of column
tDF$ALT <- ifelse(is.na(tDF$ALT), avg_ALT, tDF$ALT) # replace 'na' with average

tDF$AST <- as.numeric(as.character(tDF$AST)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_AST <- mean(tDF$AST, na.rm = TRUE) # find average of column
tDF$AST <- ifelse(is.na(tDF$AST), avg_AST, tDF$AST) # replace 'na' with average

tDF$CA <- as.numeric(as.character(tDF$CA)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_CA <- mean(tDF$CA, na.rm = TRUE) # find average of column
tDF$CA <- ifelse(is.na(tDF$CA), avg_CA, tDF$CA) # replace 'na' with average

tDF$CREAT <- as.numeric(as.character(tDF$CREAT)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_CREAT <- mean(tDF$CREAT, na.rm = TRUE) # find average of column
tDF$CREAT <- ifelse(is.na(tDF$CREAT), avg_CREAT, tDF$CREAT) # replace 'na' with average

tDF$HB <- as.numeric(as.character(tDF$HB)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_HB <- mean(tDF$HB, na.rm = TRUE) # find average of column
tDF$HB <- ifelse(is.na(tDF$HB), avg_HB, tDF$HB) # replace 'na' with average

tDF$NEU <- as.numeric(as.character(tDF$NEU)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_NEU <- mean(tDF$NEU, na.rm = TRUE) # find average of column
tDF$NEU <- ifelse(is.na(tDF$NEU), avg_NEU, tDF$NEU) # replace 'na' with average

tDF$PLT <- as.numeric(as.character(tDF$PLT)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_PLT <- mean(tDF$PLT, na.rm = TRUE) # find average of column
tDF$PLT <- ifelse(is.na(tDF$PLT), avg_PLT, tDF$PLT) # replace 'na' with average

tDF$PSA <- as.numeric(as.character(tDF$PSA)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_PSA <- mean(tDF$PSA, na.rm = TRUE) # find average of column
tDF$PSA <- ifelse(is.na(tDF$PSA), avg_PSA, tDF$PSA) # replace 'na' with average

tDF$TBILI <- as.numeric(as.character(tDF$TBILI)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_TBILI <- mean(tDF$TBILI, na.rm = TRUE) # find average of column
tDF$TBILI <- ifelse(is.na(tDF$TBILI), avg_TBILI, tDF$TBILI) # replace 'na' with average

tDF$WBC <- as.numeric(as.character(tDF$WBC)) # change datatype from factor to numeric. automatically replaces '.' with 'na'
avg_WBC <- mean(tDF$WBC, na.rm = TRUE) # find average of column
tDF$WBC <- ifelse(is.na(tDF$WBC), avg_WBC, tDF$WBC) # replace 'na' with average

tDF$ENDTRS_C <- NULL  # delete covariate ENDTRS_C as not addressing Q2

# AGEGRP2 is categorical.  Create a one-hot encoding.
AGEGRP2_DM <- model.matrix(~tDF$AGEGRP2+0)
tDF$AGEGRP2 <- NULL
tDF <- data.frame(tDF, AGEGRP2_DM)

# RACE_C, replace 'Missing' with most frequent value (White), then one-hot encode.
tDF$RACE_C <- sub("Missing", "White", tDF$RACE_C)
tDF$RACE_C <- sub("Hispanic", "Other", tDF$RACE_C) # replace 'Hispancic' with 'other' as validation set lacks category 'Hispanic'
tDF$RACE_C <- factor(tDF$RACE_C)
RACE_C_DM <- model.matrix(~tDF$RACE_C+0)
tDF$RACE_C <- NULL
tDF <- data.frame(tDF, RACE_C_DM)


# Validation set's ECOG_C covariate is binary and not categorical, therefore replace '.', '2', and '3' with most frequent value '1'.
tDF$ECOG_C[tDF$ECOG_C == '.'] <- '1'  # replace '.' with '1'
tDF$ECOG_C[tDF$ECOG_C == '2'] <- '1'  # replace '2' with '1'
tDF$ECOG_C[tDF$ECOG_C == '3'] <- '1'  # replace '3' with '1'
tDF$ECOG_C <- factor(tDF$ECOG_C)


# Training covariate HEIGHTBL need to process to categorical HGTBLCAT to match validation set (Cat: >=180-200, >=160-180, >=140-160)
tDF$HEIGHTBL <- as.numeric(as.character(tDF$HEIGHTBL))
avg_HEIGHTBL <- mean(tDF$HEIGHTBL, na.rm = TRUE) # find average of column
tDF$HEIGHTBL <- ifelse(is.na(tDF$HEIGHTBL), avg_HEIGHTBL, tDF$HEIGHTBL) # replace 'na' with average

HEIGHTBL_Category <- function(x) {
  if ((x >= 180)) {
    ">=180-200"
  } else if ((x >= 160) &&  (x < 180)){
    ">=160-180"
  } else {
    ">=140-160"
  }
}
tDF$HGTBLCAT <- sapply(tDF$HEIGHTBL, HEIGHTBL_Category)
tDF$HGTBLCAT <- factor(tDF$HGTBLCAT)
HGTBLCAT_DM <- model.matrix(~tDF$HGTBLCAT+0)  # create a one-hot encoded matrix for column HGTBLCAT 
tDF$HGTBLCAT <- NULL
tDF$HEIGHTBL <- NULL
tDF <- data.frame(tDF, HGTBLCAT_DM)


# WEIGHTBL need to process to categorical WGTBLCAT to match validation set (Cat: >=140-150, >=130-140, >=120-130,
#                                 >=110-120, >=100-110, >=90-100, >=80-90, >=70-80, >=60-70, >=50-60, >=40-50)
tDF$WEIGHTBL <- as.numeric(as.character(tDF$WEIGHTBL))
avg_WEIGHTBL <- mean(tDF$WEIGHTBL, na.rm = TRUE) # find average of column
tDF$WEIGHTBL <- ifelse(is.na(tDF$WEIGHTBL), avg_WEIGHTBL, tDF$WEIGHTBL) # replace 'na' with average

WEIGHTBL_Category <- function(x) {
  if (x >= 140) {
    ">=140-150"
  } else if ((x >= 130) &&  (x < 140)){
    ">=130-140"
  } else if ((x >= 120) &&  (x < 130)){
    ">=120-130"   
  } else if ((x >= 110) &&  (x < 120)){
    ">=110-120"
  } else if ((x >= 100) &&  (x < 110)){
    ">=100-110"
  } else if ((x >= 90) &&  (x < 100)){
    ">=90-100"
  } else if ((x >= 80) &&  (x < 90)){
    ">=80-90"   
  } else if ((x >= 70) &&  (x < 80)){
    ">=70-80"   
  } else if ((x >= 60) &&  (x < 70)){
    ">=60-70"
  } else if ((x >= 50) &&  (x < 60)){
    ">=50-60"
  } else {
    ">=40-50"
  }
}
tDF$WGTBLCAT <- sapply(tDF$WEIGHTBL, WEIGHTBL_Category)
tDF$WGTBLCAT <- factor(tDF$WGTBLCAT)
WGTBLCAT_DM <- model.matrix(~tDF$WGTBLCAT+0)  # create a one-hot encoded matrix for column WGTBLCAT
tDF$WGTBLCAT <- NULL
tDF$WEIGHTBL <- NULL
tDF <- data.frame(tDF, WGTBLCAT_DM)

# move ECOG_C to the right-most side of the design matrix to have real-valued covariates on the left and binary covariates on the right.
ECOG_C <- tDF$ECOG_C
tDF$ECOG_C <- NULL
tDF <- data.frame(tDF, ECOG_C)

row.names(tDF) <- NULL

# separate time and event covariate out of the design matrix and save all three matrix to separate files
event <- data.frame(tDF$DEATH)
time <- data.frame(tDF$LKADT_P)
tDF$DEATH <- NULL
tDF$LKADT_P <- NULL
trainingDF <- tDF
write.csv(trainingDF, file="trainDesignMatrix.csv", row.names=FALSE)
write.csv(event, file="trainEvent.csv", row.names=FALSE)
write.csv(time, file="trainTime.csv", row.names=FALSE)
