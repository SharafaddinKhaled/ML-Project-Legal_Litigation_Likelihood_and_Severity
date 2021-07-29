#################################### 
# Khaled Sharafaddin
# July 03.2021
# Regression and Classification Analysis Report  
# Mohawk Industries 
#################################### 

# 1. Load Libraries
library(dplyr)
library(aCRM)
library(mice)
library(plyr)
library(car)
library(lattice)
library(lubridate)
library(AggregateR)
library(caret)
library(MASS)
library(DMwR) # Loading DMwr to balance the unbalanced class
library(caTools)
library(Boruta)
library(neuralnet)
library(caTools)
library(dplyr)
library(Lahman)
library(caTools)
library(Hmisc)
library('pROC')
library("viridis")        
library(ISLR)
library(rsample)   # for creating our train-test splits
library(recipes)   # for minor feature engineering tasks
options(scipen=999) # remove scientific notation

###-----------------------------------------------
# 2. Load All Excel sheets:
Fundamentals <- read.csv("/Users/khaledsharafaddin/Documents/Merrimack\ College/Capstone/Data\ Files/Fundamentals_DS.csv")
Ratings <- read.csv("/Users/khaledsharafaddin/Documents/Merrimack\ College/Capstone/Data\ Files/Ratings_DS.csv")
Securities <- read.csv("/Users/khaledsharafaddin/Documents/Merrimack\ College/Capstone/Data\ Files/Securities_DS.csv")
Stocks <- read.csv("/Users/khaledsharafaddin/Documents/Merrimack\ College/Capstone/Data\ Files/Stocks_DS.csv")

# 3. Remove Your company and save it
# MOHAWK_INDUSTRIES  <- subset(Fundamentals, Fundamentals$conm =='MOHAWK INDUSTRIES INC')
# saveRDS(MOHAWK_INDUSTRIES, file = "MOHAWK_INDUSTRIES.Rds")
MOHAWK_INDUSTRIES <- readRDS(file = "MOHAWK_INDUSTRIES.Rds")

###-----------------------------------------------

# 3. Cleaning Each File Used Individually 

# --------------------- I. Fundamentals.csv  ---------------------

# These are the fields that logically might be good factors at determining likelihood of prediction: 
Chosen_Predictors_Fundamentals <- read.csv("/Users/khaledsharafaddin/Documents/Merrimack\ College/Capstone/Chosen_Predictors.csv")
# Convert to vector for Fields 
Chosen_Predictors_Fundamentals <- as.vector(Chosen_Predictors_Fundamentals$Predictor)

# Now this is the new Fundamental data set that contains subset fields from original Fundamentals dataset 
Fundamentals_new <- Fundamentals %>% dplyr::select(Chosen_Predictors_Fundamentals) # 17416 by 901 variables 
dim(Fundamentals_new)

# We will remove ALL rows where datafmt =='SUMM_STD' and keep only the STD format (this is where the corrected filing for SEC happened, so we don't need them as they will be repeatitive.)
Fundamentals_new <- subset(Fundamentals_new, datafmt =='STD')
table(Fundamentals_new$datafmt)  # we are left with 9078 rows out of the original 17416 rows
Fundamentals_new$gvkey <- as.factor(Fundamentals_new$gvkey)   # convert to a factor

# We need to only keep the companies that are related to target company's gsector
MHK <- subset(Fundamentals_new, conm =='MOHAWK INDUSTRIES INC')
MHK$gsector # 25 

Fundamentals_new2 <- subset(Fundamentals_new, gsector==25)
dim(Fundamentals_new2)  # 3893  901

Fundamentals_clean1 <- Fundamentals_new2

# Remove one-class categorical variables 
Fundamentals_clean1$consol <- NULL # C
Fundamentals_clean1$indfmt <- NULL # INDL
Fundamentals_clean1$popsrc <- NULL # D
Fundamentals_clean1$curcd  <- NULL # USD

dim(Fundamentals_clean1)  # 3893  898

# We add new category to the empty strings in many of the categorical predictors: 
Fundamentals_clean1$acqmeth <- mapvalues(Fundamentals_clean1$acqmeth, from = c(" ","AI", "AP", "RO", "RP", "RU"), to = c("Unspecified","AI", "AP", "RO", "RP", "RU"))
Fundamentals_clean1$acctstd <- mapvalues(Fundamentals_clean1$acctstd, from = c(" ",  "DI", "DS", "DU"), to = c("Unspecified",  "DI","DS", "DU"))
Fundamentals_clean1$compst <- mapvalues(Fundamentals_clean1$compst, from = c(" " , "AA", "AB", "AC", "AG", "AR", "AS", "AT", "AZ", "DB", "DZ"), to = c("Unspecified" , "AA", "AB", "AC", "AG", "AR", "AS", "AT", "AZ", "DB", "DZ"))
Fundamentals_clean1$ceoso <- mapvalues(Fundamentals_clean1$ceoso, from = c(" ", "E", "N", "Y"), to = c("Unspecified", "E", "N", "Y"))
Fundamentals_clean1$cfoso <- mapvalues(Fundamentals_clean1$cfoso, from = c(" ", "E", "N", "Y"), to = c("Unspecified", "E", "N", "Y"))
Fundamentals_clean1$incorp <- mapvalues(Fundamentals_clean1$incorp, from = c(" ","AZ", "CA", "CO", "CT", "DE", "FL", "GA" ,"IA" ,"IL" ,"IN", "KS" ,"KY" ,"LA" ,"MA", "MD", "MI", "MN" ,"MO" ,"MS", "NC", "NE", "NJ", "NM", "NV", "NY" ,"OH", "OK", "OR", "PA" ,"RI", "SC" ,"TN" ,"TX" ,"UT", "VA", "WA", "WI", "WY"), to = c("Unspecified"  ,"AZ", "CA", "CO", "CT", "DE", "FL", "GA" ,"IA" ,"IL" ,"IN", "KS" ,"KY" ,"LA" ,"MA", "MD", "MI", "MN" ,"MO" ,"MS", "NC", "NE", "NJ", "NM", "NV", "NY" ,"OH", "OK", "OR", "PA" ,"RI", "SC" ,"TN" ,"TX" ,"UT", "VA", "WA", "WI", "WY"))
Fundamentals_clean1$acctchg <- mapvalues(Fundamentals_clean1$acctchg, from = c(" ", "APB14-1",  "FE03-6-1", "FS157", "FS160"), to = c("Unspecified", "APB14-1",  "FE03-6-1", "FS157", "FS160"))

# Class Imbalance 
Fundamentals_clean1$bspr <- mapvalues(Fundamentals_clean1$bspr, from = c(" ", "GB"), to = c("Unspecified", "GB"))
# Class Imbalance 
Fundamentals_clean1$stalt <- mapvalues(Fundamentals_clean1$stalt, from = c(" ", "TL"), to = c("Unspecified", "TL"))
Fundamentals_clean1$ogm <- mapvalues(Fundamentals_clean1$ogm, from = c(" ", "N"), to = c("Unspecified", "N"))

Fundamentals_clean1$acchg <- NULL   # removed because it's eithe NA or 0 
Fundamentals_clean1$aocisecgl <- NULL   # removed because it's eithe NA or 0 
Fundamentals_clean1$esopr <- NULL   # removed because it's eithe NA or 0 
Fundamentals_clean1$esopt <- NULL   # Preferred ESOP Obligation - Total
Fundamentals_clean1$fatn <- NULL    # Property, Plant, and Equipment - Natural Resources at Cost 
Fundamentals_clean1$ictb <- NULL # removed because it's eithe NA or 0 
Fundamentals_clean1$tstkp <- NULL # removed because it's eithe NA or 0 
Fundamentals_clean1$xi <- NULL # removed because it's eithe NA or 0
Fundamentals_clean1$xintopt <- NULL # removed because it's eithe NA or 0
Fundamentals_clean1$xoptd <- NULL # removed because it's eithe NA or 0
Fundamentals_clean1$xopteps <- NULL  # removed because it's eithe NA or 0
Fundamentals_clean1$adrr <- NULL # removed because it's eithe NA or 0

# These need to be categorical data but they are not needed 
Fundamentals_clean1$ggroup <- NULL
Fundamentals_clean1$gind <-  NULL
Fundamentals_clean1$subind <-  NULL
Fundamentals_clean1$stko <-  NULL
Fundamentals_clean1$incorp <-  NULL   # Current State/Province of Incorporation Code
Fundamentals_clean1$fic <-  NULL      # Current ISO Country Code - Incorporation
Fundamentals_clean1$acctchg <-  NULL  # Adoption of Accounting Changes
Fundamentals_clean1$acctstd <-  NULL
Fundamentals_clean1$datafmt <-  NULL  # only one category STD 
Fundamentals_clean1$city <-  NULL  

Fundamentals_clean1 <- as.data.frame(Fundamentals_clean1)
dim(Fundamentals_clean1)  # 3893  878

# 2. Now removing all variables where missing NA values are >= 25% 
# 0% means 0 NAs, else, it's the percentage of missing values 
# Found all of the predictors with all NA values, put them into a dataframe which makes them indecies. Then extracted the rownames so we can eliminate them from the Fundamentals_clean1 dataframe
empty_fields <- colSums(is.na(Fundamentals_clean1))/nrow(Fundamentals_clean1) * 100
empty_fields <- sort(empty_fields, decreasing = FALSE)
Percent_empty <- empty_fields[empty_fields>=25]
Percent_empty <- as.data.frame(Percent_empty)
rownames_percent_empty <- rownames(Percent_empty)  # convert index into row names so we can remove them from Fundamentals_clean1

# Remove rownames_percent_empty variables from the Fundamentals dataframe
empty <- names(Fundamentals_clean1) %in% rownames_percent_empty
Fundamentals_clean2 <- Fundamentals_clean1[!empty]
Fundamentals_clean2 <- as.data.frame(Fundamentals_clean2)
dim(Fundamentals_clean2)   # 3893  281


# Note: Before using mice, make sure to remove any variables that don't contribute to the predictive power such as gvkey or IDs 
Imputed_Data_factors <- Fundamentals_clean2[,sapply(Fundamentals_clean2, is.factor)]
Imputed_Data_factors <- as.data.frame(Imputed_Data_factors)
dim(Imputed_Data_factors)
View(Imputed_Data_factors)

#  MISSING DATA IMPUTATION - MICE ANALYSIS:  
Imputed_Data_int <- as.data.frame(Fundamentals_clean2[,sapply(Fundamentals_clean2, is.numeric)])
Imputed_Data_int_mice <- mice(Imputed_Data_int,m=3,maxit=5,method='cart',seed=500)

summary(Imputed_Data_int_mice)

# This is the complete dataset after imputation. There are still some NA values. Let's see what's going on: 
complete_mice_int_1 <- complete(Imputed_Data_int_mice, 2)

# Saved
# saveRDS(Imputed_Data_int_mice, file = "Imputed_Data_int_mice.Rds")
Imputed_Data_int_mice.copy <- readRDS(file = "Imputed_Data_int_mice.Rds")

complete_mice_int_1 <- complete(Imputed_Data_int_mice.copy, 2)
complete_mice_int_1 <- as.data.frame(complete_mice_int_1)
dim(complete_mice_int_1)  

## KEEP CLEANING FUNDAMENTALS - FINAL REVIEW

# Checking the values that are still NA, we need to explore them one by one to understand their nature
empty_fields <- sort(colSums(is.na(complete_mice_int_1))/nrow(complete_mice_int_1) * 100, decreasing = FALSE)
rownames_percent_empty <- rownames(as.data.frame(empty_fields[empty_fields>0]))  # convert index into row names so we can remove them from Fundamentals_clean1
rownames_percent_complete <- rownames(as.data.frame(empty_fields[empty_fields<=0]))  # 0 NAs here. We seperate them for later 
rownames_percent_complete

# This is good. 
mice_imputed_complete <- complete_mice_int_1[rownames_percent_complete]
colSums(is.na(mice_imputed_complete))/nrow(mice_imputed_complete) * 100
dim(mice_imputed_complete)  # 9078  240

# Checking the new dataset and removing the NAs for the last time:
empty_fields <- sort(colSums(is.na(mice_imputed_complete))/nrow(mice_imputed_complete) * 100, decreasing = FALSE)
rownames_percent_empty <- rownames(as.data.frame(empty_fields[empty_fields>0])) # convert index into row names so we can remove them from Fundamentals_clean1
rownames_percent_empty

mice_imputed_complete <- as.data.frame(mice_imputed_complete)
full_data_Set <- cbind(Imputed_Data_factors, mice_imputed_complete)

# convert gvkey to numeric 
full_data_Set$gvkey <- as.vector(full_data_Set$gvkey)


full_data_Set$itcb <- NULL
full_data_Set$scf <- NULL
full_data_Set$ibcom <- NULL
full_data_Set$dilavx <- NULL
full_data_Set$epspi <- NULL
full_data_Set$epspx <- NULL
full_data_Set$conml <- NULL  # company legal name does not help with prediction
full_data_Set$conm <- NULL  # company legal name does not help with prediction
dim(full_data_Set)  # 9078  252

# No NAs anymore. 
sort(colSums(is.na(full_data_Set)), decreasing = TRUE)


# Now we convert all categorical fields into dummy variables: 
correlation_factors <- full_data_Set[,sapply(full_data_Set, is.factor)]
correlation_factors <- as.data.frame(correlation_factors)
colnames(correlation_factors)


# Remove further unneeded predictors 
full_data_Set$bspr <- NULL   # Balance Sheet Presentation
full_data_Set$ogm <- NULL   # Balance Sheet Presentation
full_data_Set$stalt <- NULL # Status Alert
full_data_Set$stalt <- NULL # Status Alert
full_data_Set$city <- NULL 
full_data_Set$conml <- NULL   # company legal name
full_data_Set$ggroup <- NULL   # company legal name
full_data_Set$gind <- NULL   # company legal name
full_data_Set$incorp <- NULL   # ISO country code
full_data_Set$stko <- NULL   # stock ownership code
full_data_Set$gind <- NULL   # GIC Industries
full_data_Set$gsubind <- NULL   # GIC sub Industries
full_data_Set$acqmeth <- NULL   # aquisition method
full_data_Set$compst <- NULL   #  Comparability Status

full_data_Set$gvkey <- as.factor(full_data_Set$gvkey)

dim(full_data_Set)   # 3893  244

# Saved
# saveRDS(full_data_Set, file = "full_data_Set.Rds")
full_data_Set <- readRDS(file = "full_data_Set.Rds")





# --------------------- II. Securities.csv  ---------------------

# 1. Select only the following predictors from Securities 
securities_include <- c('gvkey','datadate', 'primiss','prccm','prchm','prclm','trfm','trt1m','mkvalincl','gsector','conm')
Securities_temp <- as.data.frame(Securities[, securities_include])

# Finding only companies with gsector == 25 
Securities_temp <- subset(Securities_temp, gsector==25)
table(Securities_temp$gsector) # 25 
dim(Securities_temp)  # 92217    11


# 2. Check NAs 
sort(colSums(is.na(Securities_temp))/nrow(Securities_temp) * 100, decreasing = T)

# 3. Use mice to fill in the missing values: 
Securities_temp_mice <- mice(Securities_temp[, c('primiss','prccm','prchm','prclm','trfm','trt1m','mkvalincl')],
                             m=3,maxit=5,method='cart',seed=500)

Securities_complete <- as.data.frame(complete(Securities_temp_mice,2))
securities_partial <- Securities_temp[, c('gvkey','datadate')]  # putting gvkey and datadate back with the other predictors  
Securities_full <- cbind(securities_partial, Securities_complete)

# No NAs any longer 
sort(colSums(is.na(Securities_full))/nrow(Securities_full) * 100, decreasing = T)

Securities_full$primiss <- mapvalues(Securities_full$primiss, from = c(" ", "J", "P"), to = c("None", "J", "P"))
Securities_full$mkvalincl <- mapvalues(Securities_full$mkvalincl, from = c(" " ,"N", "Y"), to = c("None" ,"N", "Y"))
Securities_full$gvkey <- as.factor(Securities_full$gvkey)
dim(Securities_full) # 92217     9
# Saved
# saveRDS(Securities_full, file = "Securities_full.Rds")
Securities_full <- readRDS(file = "Securities_full.Rds")
dim(Securities_full)  # 202696      9


# --------------------- III. Ratings.csv  ---------------------
# 1. Select only the following predictors from Securities 
Ratings_include <- c('gvkey','datadate', 'splticrm','spsticrm','spcsrc','gsector')
ratings_temp <- as.data.frame(Ratings[, Ratings_include])
dim(ratings_temp)  # 123678      6
ratings_temp <- subset(ratings_temp, gsector==25)
ratings_temp$gsector <- NULL # remove it since it's no longer needed for prediction.

# 2. Check NAs 
sort(colSums(is.na(ratings_temp))/nrow(ratings_temp) * 100, decreasing = T)

# Fix categorical variable where it's " "
ratings_temp$splticrm <- mapvalues(ratings_temp$splticrm, from = c(" "  ,  "A"  ,  "A-"  , "A+"   ,"AA" ,  "AA-" , "AAA" , "B"  ,  "B-"   ,"B+"  , "BB" ,  "BB-" , "BB+" , "BBB"  ,"BBB-", "BBB+",
                                                                   "CC",   "CCC"  ,"CCC-", "CCC+" ,"D"  ,  "SD"), 
                                   to = c("None"  ,  "A"  ,  "A-"  , "A+"   ,"AA" ,  "AA-" , "AAA" , "B"  ,  "B-"   ,"B+"  , "BB" ,  "BB-" , "BB+" , "BBB"  ,"BBB-", "BBB+",
                                          "CC",   "CCC"  ,"CCC-", "CCC+" ,"D"  ,  "SD"))

ratings_temp$spsticrm <- mapvalues(ratings_temp$spsticrm, from = c(" " ,   "A-1" , "A-1+", "A-2",  "A-3" , "B" ,   "B-1"  ,"C" ,   "D"), 
                                   to = c("None" ,   "A-1" , "A-1+", "A-2",  "A-3" , "B" ,   "B-1"  ,"C" ,   "D"))


ratings_temp$spcsrc <- mapvalues(ratings_temp$spcsrc, from = c(" "  , "A"  , "A-" , "A+"  ,"B"  , "B-" , "B+" , "C"  , "D" ,  "LIQ"), 
                                 to = c("None"  , "A"  , "A-" , "A+"  ,"B"  , "B-" , "B+" , "C"  , "D" ,  "LIQ"))

ratings_temp$gvkey <- as.factor(ratings_temp$gvkey)

# Saved
# saveRDS(ratings_temp, file = "ratings_temp.Rds")
ratings_full <- readRDS(file = "ratings_temp.Rds")
dim(ratings_full)   #  57884     5
##-------------------------------------------------------

## AGGREGATION AND MERGING FUNCTIONS


# Creating aggregation function for all files:

# I. Function to find the most frequent factor variable per gvkey. The max number of that category will be used for that particular gvkey and the rest will be out
getmode <- function(v) {
  levels(v)[which.max(table(v))]}

# 2. For numeric variables - if there is skewness, median is used, else mean
# If the mean > than the median, the distribution is positively skewed.
# If the mean < than the median, the distribution is negatively skewed.
# if mean == median, mean should be used  

# To do: 1. find a way to add the standard deviation value if mean or MAD if median into each file then inner join merge 
# MAD (Median Abosolute Deviation) is a robust statistic, being more resilient to outliers in a data set than the standard deviation.
# https://en.wikipedia.org/wiki/Median_absolute_deviation
# 2. 

# Original: 
my_summary <- function(column, id, na.rm = FALSE){
  if (is.numeric(column)) {
    if(round(mean(column),2) > round(median(column),2) | round(mean(column),2) < round(median(column),2) ){
      return(tapply(column, id, median, na.rm = na.rm)) 
    }
    else{
      return(tapply(column, id, mean, na.rm = na.rm))
      
    }
  }  
  
  if (is.factor(column)) {
    return(tapply(column, id, getmode))
  }  
}  


# 2. This is for Median Abosolute Deviation and Standard Deviation functions
my_summary2 <- function(column, id, na.rm = FALSE){
  if (is.numeric(column)) {
    if(round(mean(column),2) > round(median(column),2) | round(mean(column),2) < round(median(column),2) ){
      return(tapply(column, id, mad, na.rm = na.rm)) 
    }
    else{
      return(tapply(column, id, sd, na.rm = na.rm))
      
    }
  }  
  if (is.factor(column)) {
    return(tapply(column, id, getmode))
  }  
  
} 

# Histograms to check skewness:
par(mfrow=c(2,2))
res_prchm <- hist(log(Securities_full$prchm),n=70, col = 'cornflowerblue',main="", xlab="Price - High - Monthly",las=1)
res_trfm <- hist(log(Securities_full$trfm),n=50, col = 'indianred',main="", xlab="Monthly Total Return Factor",las=1,ylim = c(0,10000))
res_trt1m <- hist(log(Securities_full$trt1m),n=50, col = 'yellow',main="", xlab="Monthly Total Return",las=1,ylim = c(0,10000))
res_prclm <- hist(log(Securities_full$prclm),n=50, col = 'black',main="", xlab="Price - Low - Monthly",las=1,ylim = c(0,10000))


# 3. Apply the two functions above by gvkey. This will produce only one gvkey record aggregated. 
# One for median and mean and mode, and the second for MAD, SD and Mode 
Securities_full_aggregated1 <- data.frame(lapply(Securities_full, my_summary, id = Securities_full$gvkey))
Securities_full_aggregated2 <- data.frame(lapply(Securities_full, my_summary2, id = Securities_full$gvkey))

# 4. Selecting only the numeric values for the second function so we can cbind them into the full dataset
Securities_full_aggregated1_numeric <- Securities_full_aggregated1[, 4:8]
Securities_full_aggregated2_numeric <- Securities_full_aggregated2[, 4:8]
# 5. Changing the column names so there will be no overlap 
colnames(Securities_full_aggregated1_numeric) <- paste(colnames(Securities_full_aggregated1_numeric), "Avg", sep = ".")
colnames(Securities_full_aggregated2_numeric) <- paste(colnames(Securities_full_aggregated2_numeric), "Deviation", sep = ".")
# 6. Combine both variables above into one full dataset that includes average and their deviations + the mode for factors:
Securities_Final <- cbind(Securities_full_aggregated1[, c('gvkey','datadate')], 
                          Securities_full_aggregated1[, c('primiss','mkvalincl')],
                          Securities_full_aggregated1_numeric, Securities_full_aggregated2_numeric)


# 6. Repeat for Ratings - all are factors: 
Ratings_full_aggregated1 <- data.frame(lapply(ratings_full, my_summary, id = ratings_full$gvkey))
Ratings_Final <- Ratings_full_aggregated1
dim(Ratings_Final) # 1548    5
# 7. Fundamentals - aggregate on average only: 
fundamentals_aggregated <- data.frame(lapply(full_data_Set, my_summary, id = full_data_Set$gvkey))
dim(fundamentals_aggregated) # 1073  244

# 8. Last check with dim and length of unique gvkeys:
length(unique(fundamentals_aggregated$gvkey)) #  1073  
dim(fundamentals_aggregated)  # 1073  244
length(unique(Securities_Final$gvkey))        #  2111   14 
dim(Securities_Final)              
length(unique(Ratings_Final$gvkey))           #  1548    5
dim(Ratings_Final)
# ------------------------------






# Combining The three files on gvkey: 
# all.x	- logical; if TRUE, then extra rows will be added to the output, one for each row in x  that has no matching row in y. These rows will have NAs in those columns that are usually 
# filled with values from y. The default is FALSE, so that only rows with data from both x and y are 
# included in the output.

Merged_sec_rating_Fund <- merge(merge(fundamentals_aggregated, Securities_Final, by='gvkey'), Ratings_Final, by='gvkey')
dim(Merged_sec_rating_Fund)   # 890 261

# remove repetitive columns and fix year :
Merged_sec_rating_Fund$datadate.x <- NULL 
Merged_sec_rating_Fund$datadate.y <- NULL 
Merged_sec_rating_Fund$fyear <- round(Merged_sec_rating_Fund$fyear)

# Reordering so factors go before all numerics 
Merged_sec_rating_Fund <- mutate_if(Merged_sec_rating_Fund, is.character, as.factor)
Merged_sec_rating_Fund_int <- Merged_sec_rating_Fund[,sapply(Merged_sec_rating_Fund, is.numeric)]
Merged_sec_rating_Fund_factor <- Merged_sec_rating_Fund[,sapply(Merged_sec_rating_Fund, is.factor)]
Fund_Secu_Rat_Full <- cbind(Merged_sec_rating_Fund_factor, Merged_sec_rating_Fund_int)

# Final Complete dataset: 
# Save:
# saveRDS(Fund_Secu_Rat_Full, file = "Fund_Secu_Rat_Full.Rds")
Fund_Secu_Rat_Full <- readRDS(file = "Fund_Secu_Rat_Full.Rds")
dim(Fund_Secu_Rat_Full)  # 890 259




#-----------------SCA Filing Settlment File ----------------

## COMBINE THE MERGED DATASET INTO THE SCA Filing and Settlement File 


# How to approach: use this file as a reference table. Use your assigned company as a target and ask: 
# for each company in your file, if they have been sued by the SCA or not. So if that company exists in the SCA filing, then ==’Yes’, else ‘No’. 
# Merge the two sets (your final dataset and this sca filings) by Ticker symbol

# 1. Load file, then convert dollar amount to a number 
SCA_Settlements <- read.csv("/Users/khaledsharafaddin/Documents/Merrimack\ College/Capstone/Data\ Files/SCAFilingSettlements.csv", header = TRUE)
SCA_Settlements$SettlementAmount <- as.numeric(gsub('[$,]', '', SCA_Settlements$SettlementAmount))

# 2. Create the first y_response variable sued. All companies in this file are sued. 
SCA_Settlements$sued <- 'Yes'

# 3. Convert char to Factors
SCA_Settlements <- mutate_if(SCA_Settlements, is.character, as.factor)
str(SCA_Settlements)

# 4. Aggregate all companies together by max amount of settlements for those companies that show up more than once.
# If factor, stays as is, if numeric, give me max value of settlement
max_settlements <- function(column, id, na.rm = FALSE){
  if (is.numeric(column)) {
    return(tapply(column, id, max, na.rm = na.rm))
  }
  
  if (is.factor(column)) {
    return(tapply(column, id, getmode))
  }  
}

# 4. Apply the max_settlement function 
# and Fix the indecies (they are not numbers, they are company names)
SCA_Settlements_aggregated <- data.frame(lapply(SCA_Settlements, max_settlements, id = SCA_Settlements$FilingName))
rownames(SCA_Settlements_aggregated) <- 1:nrow(SCA_Settlements_aggregated)
SCA_Settlements_aggregated <- mutate_if(SCA_Settlements_aggregated, is.character, as.factor)

# 1309 records NAs for SettlementAmount
colSums(is.na(SCA_Settlements_aggregated))  # 1734    5
dim(SCA_Settlements_aggregated)

# 5. Change column name in Fund_Secu_Rat_Full from tic to Ticker so the merge works 
colnames(Fund_Secu_Rat_Full)[which(names(Fund_Secu_Rat_Full) == "tic")] <- "Ticker"

# 8. Important: We want all records in Fund_Secu_Rat_Full that exist in SCA_Settlements_aggregated, while keeping all records on the first dataset
# All of Fund_Secu_Rat_Full companies, including the ones in SCA_Settlements_aggregated (left join merge)
dim(Fund_Secu_Rat_Full)  # 890 259
dim(SCA_Settlements_aggregated) # 1734    5

full_mrge <- merge(x=Fund_Secu_Rat_Full, y=SCA_Settlements_aggregated, by="Ticker", all.x = TRUE)
dim(full_mrge)  # 904 263


# 9. Create new if_sued variable. If sued ==yes, then if_sued==yes, else no
full_mrge$if_sued <- ifelse(is.na(full_mrge$sued), 0, 1)

table(full_mrge$sued) # 161 yes
table(full_mrge$if_sued) # 743 N 161 Y
na <- is.na(full_mrge$SettlementAmount) # NA: 867, $$=37
table(na)

# Removed sued so we don't get confused. if_sued is now the functioning response 
full_mrge$sued <- NULL
full_mrge$Exchange <- NULL
full_mrge$FilingName <- NULL


# Checking dims and nulls again:
sort(colSums(is.na(full_mrge))/nrow(full_mrge) * 100, decreasing = T)
dim(full_mrge) # 904 262

# We have sever class imbalance for the two response variables 
prop.table(table(full_mrge$if_sued)) *100 # 743 N(82%) 161 Y (18%)
na <- is.na(full_mrge$SettlementAmount) # NA: 867, $$=37
prop.table(table(na)) *100 # $ = 867 or (4%) NA = 161  NA (96%)

# Try to reduce more predictors?
full_mrge$datadate <- NULL
full_mrge$fyear <- NULL
full_mrge$txndbr <- NULL
full_mrge$txo <- NULL
full_mrge$gsector <- NULL
full_mrge$txw <- NULL
full_mrge$rea <- NULL
full_mrge$pstkn <- NULL
full_mrge$prcaeps <- NULL
full_mrge$prcad <- NULL
full_mrge$mib <- NULL
full_mrge$intc <- NULL
full_mrge$esopct <- NULL
full_mrge$dvpa <- NULL
full_mrge$dvp <- NULL
full_mrge$dudd <- NULL
full_mrge$diladj <- NULL
full_mrge$dcvsub <- NULL
full_mrge$dcom <- NULL
full_mrge$ciother <- NULL
full_mrge$aociother <- NULL
full_mrge$aldo <- NULL
full_mrge$acdo <- NULL
full_mrge$upd <- NULL
full_mrge$pddur <- NULL
full_mrge$ceoso <- NULL
full_mrge$cfoso <- NULL


# Fix this:
full_mrge$splticrm <- mapvalues(full_mrge$splticrm, from = c(" "  ,  "A"  ,  "A-"  , "A+"   ,"AA" ,  "AA-" , "AAA" , "B"  ,  "B-"   ,"B+"  , "BB" ,  "BB-" , "BB+" , "BBB"  ,"BBB-", "BBB+",
                                                             "CC",   "CCC"  ,"CCC-", "CCC+" ,"D"  ,  "SD"), 
                                to = c("None"  ,  "A"  ,  "A-"  , "A+"   ,"AA" ,  "AA-" , "AAA" , "B"  ,  "B-"   ,"B+"  , "BB" ,  "BB-" , "BB+" , "BBB"  ,"BBB-", "BBB+",
                                       "CC",   "CCC"  ,"CCC-", "CCC+" ,"D"  ,  "SD"))


dim(full_mrge)  # 904 234


# For yes/no if_sued, reconstitiute the dataset to have randomly selected from the biggest proportion and only use those 
barplot(prop.table(table(full_mrge$if_sued)) *100, 
        col = rainbow(2),main="Percentage of Class 'sued' Distribution") # N: % 82.19027 Y = % 017.80973 


## ------------------------------------------------------------------



# Problem1: Let's look and see if some categorical predictors can be condenced into fewer categories due to the severe class imbalance 
# splticrm - S&P Domestic Long Term Issuer Credit Rating can be combined into only three categories (splticrmOther, splticrmHigh, splticrmLow) for simplicity.
full_mrge_copy <- full_mrge

levels(full_mrge_copy$splticrm) <- list("1"=c("A","A-","A+","AA-","B","BB","BB+","B+","BBB","BBB+"), "2"=c("B-","BB-","BBB-","CCC+"), "3"=c("None"))
levels(full_mrge_copy$spsticrm) <- list("1"=c("A-1","A-1+","A-2"), "2"=c("A-3", "B"), "3"=c("None"))
levels(full_mrge_copy$spcsrc) <- list("1"=c("A",  "A-", "A+", "B"), "2"=c("B-" ,"B+", "C",  "D",  "LIQ" ), "3"=c("None"))
levels(full_mrge_copy$costat) <- list("1"=c("A"), "2"=c("I"))
levels(full_mrge_copy$idbflag) <- list("1"=c("B"), "2"=c("D"))
levels(full_mrge_copy$primiss) <- list("1"=c("B"), "2"=c("D"))
levels(full_mrge_copy$mkvalincl) <- list("1"=c("B"), "2"=c("D"))


# Convert the newly created collapsed high, low, none categories to numeric 
full_mrge_copy$splticrm <- as.numeric(full_mrge$splticrm)
full_mrge_copy$spsticrm <- as.numeric(full_mrge$spsticrm)
full_mrge_copy$spcsrc <- as.numeric(full_mrge$spcsrc)
full_mrge_copy$costat <- as.numeric(full_mrge$costat)
full_mrge_copy$idbflag <- as.numeric(full_mrge$idbflag)
full_mrge_copy$primiss <- as.numeric(full_mrge$primiss)
full_mrge_copy$mkvalincl <- as.numeric(full_mrge$mkvalincl)

# Remove these variables because of severe class imbalance and they might not be useful (ratio 899:5 records)
full_mrge_copy$mkvalincl <- NULL 
full_mrge_copy$primiss <- NULL 


# Save:
# saveRDS(full_mrge_copy, file = "full_mrge_copy.Rds")
Fund_SCA_Full <- readRDS(file = "full_mrge_copy.Rds")
dim(Fund_SCA_Full)  # 904 232

# ------------------------------------------------------------------------

# MORE CLEANING, MERGING, ANALYSIS

# Excellent source for class imbalance: https://www.r-bloggers.com/dealing-with-unbalanced-data-in-machine-learning/
# Response Variable if_sued class imbalance. An unbalanced dataset will bias the prediction model towards the more common class
# 82:17 NO:YES ratio of whether companies are sued or not 
prop.table(table(Fund_SCA_Full$if_sued)) *100
Fund_SCA_Full <- as.data.frame(Fund_SCA_Full)
Fund_SCA_Full$if_sued <- ifelse(Fund_SCA_Full$if_sued==0, "No","Yes")

# NOTE: REMOVE SettlementAmount first 
# Save a copy of settlment amount in a vector so we can cbind it eventually , and remove it from the full dataset since it's not a predictor

Fund_SCA_Full <- readRDS(file = "full_mrge_copy.Rds")
Full_data_with_SettlementAmount <- Fund_SCA_Full
Fund_SCA_Full$SettlementAmount <- NULL

# saveRDS(Full_data_with_SettlementAmount, file = "Full_data_with_SettlementAmount.Rds")
Full_data_with_SettlementAmount <- readRDS(file = "Full_data_with_SettlementAmount.Rds")
dim(Full_data_with_SettlementAmount)

# REMOVE YOUR COMPNAY FROM THE DATASET, because we don't want to build a model on the company we want to predict 
# Save a copy first, and remove settlement amount and if_sued predictors, since we will predict them 
MHK <- subset(Fund_SCA_Full, gvkey =='25119')
MHK$if_sued <- NULL

Fund_SCA_Full <- Fund_SCA_Full[Fund_SCA_Full$gvkey != "25119", ]  # only the MHK company 
Fund_SCA_Full$if_sued <- as.factor(Fund_SCA_Full$if_sued)


# Save:
# saveRDS(Fund_SCA_Full, file = "Fund_SCA_Full.Rds")
Fund_SCA_Full_new <- readRDS(file = "Fund_SCA_Full.Rds")
dim(Fund_SCA_Full_new)  # 904 231

# Remove ticker and gvkey since they're not useful for predictions:
Fund_SCA_Full_new$Ticker <- NULL
Fund_SCA_Full_new$gvkey <- NULL 

# Potential Solution: oversample or undersample the class with highest/lowest ratio and use that officially 
# The main disadvantage of under-sampling is that we loose potentially relevant information from the left-out samples.
# Oversampling: While we avoid loosing information with oversampling approach, we also run the risk of overfitting our model as we are more likely to get the same samples in the training and in the test data, i.e. the test data is no longer independent from training data. 
# This would lead to an overestimation of our model’s performance and generalizability.

# In reality , we should not simply perform over- or under-sampling on our training data and then run the model. 
# We need to account for cross-validation and perform over- or under-sampling on each fold independently to get an honest estimate of model performance!



# ------------------- Multicollinearity --------------------
# We need to figure out Multicollinearity so we can imporve the models for rf and ANN we created: 

multi_collinearity <- as.vector(finalvars)
multi_collinearity_vars <- balance.data.full[, multi_collinearity] # only vars in multicollinearity 
corr_vars <- as.data.frame(cor(multi_collinearity_vars))

# Function to find the highly correlated variables and suggests which ones to remove.
# "the function looks at the mean absolute correlation of each variable and removes the variable with the largest mean absolute correlation." So in other words, it chooses one of the two variables based on how correlated it is with all the other variables.
correlated_vars_final <- as.vector(findCorrelation(corr_vars, cutoff = .96, verbose = F, names = T))

# This dataset has all variables except the highly correlated variables with correlation higher than 98%
# This new dataset will be applied to the RF and ANN models to see if they would result in higher accuracy levels:
balance.data.full.corr_cleaned <- balance.data.full[, !names(balance.data.full) %in% c(correlated_vars_final)]

# Split them into train and test sets:
train_data_cor_clean <- balance.data.full.corr_cleaned[index, ]
test_data_cor_clean <- balance.data.full.corr_cleaned[-index, ]



# ------- II. IMPORTANT VARIABLES WITH BORUTA -----------------------------

# USE BORUTA CART ALGORITHM TO FIND IMPORTANT VARIABLES 

# NOTE: Collinearity - It is important to handle collinearity after getting important variables from boruta

Boruta_model <- Boruta(if_sued~., balance.data.full, doTrace = 0, maxRuns=500)
print(Boruta_model)

# Save:
# saveRDS(Boruta_model, file = "Boruta_model.Rds")
Boruta_model <- readRDS(file = "Boruta_model.Rds")

# Plot Variables
plot(Boruta_model, xlab = "Boruta_model", xaxt = "n")
k <-lapply(1:ncol(Boruta_model$ImpHistory),function(i)
  Boruta_model$ImpHistory[is.finite(Boruta_model$ImpHistory[,i]),i])
names(k) <- colnames(Boruta_model$ImpHistory)
Labels <- sort(sapply(k,median))
axis(side = 1,las=2,labels = names(Labels),at = 1:ncol(Boruta_model$ImpHistory), cex.axis = 0.7)

# All final important variables saved in a vector
finalvars = getSelectedAttributes(Boruta_model, withTentative = T)

# attStats(Boruta_model)

# only the confirmed variables 
confirmed_vars <- getConfirmedFormula(Boruta_model)

# Add confirmed + tentative variables to test on rf model 
confirmed_tentative_vars <- getNonRejectedFormula(Boruta_model)

# ------- MOHAWK_INDSTRIES ---------------- 
# Save:
# saveRDS(MOHAWK_INDSTRIES, file = "MOHAWK_INDSTRIES.Rds")
MOHAWK_INDSTRIES <- readRDS(file = "MOHAWK_INDSTRIES.Rds")
#----------------------------------------------------------



#------ BINARY CLASSIFICATION ANALYSIS OF LITIGATION LIKELIHOOD -----------------

#----------------------------------------------
# 1. Load Saved Data:
# Save:
# saveRDS(Fund_SCA_Full, file = "Fund_SCA_Full.Rds")
Fund_SCA_Full_new <- readRDS(file = "Fund_SCA_Full.Rds")
Fund_SCA_Full_new$Ticker <- NULL
Fund_SCA_Full_new$gvkey <- NULL 
dim(Fund_SCA_Full_new)
#---------------------------------------------

# 3. SET ASIDE MY COMPANY 

# a. This is the full complete dataset with both response variables settl.amunt, and if_sued
# saveRDS(Full_data_with_SettlementAmount, file = "Full_data_with_SettlementAmount.Rds")
Full_data_with_SettlementAmount <- readRDS(file = "Full_data_with_SettlementAmount.Rds")
dim(Full_data_with_SettlementAmount) # 904 232

# b. Extract the company so we can predict it. Remove response variables + unneeded ones
MOHAWK_INDUSTRIES <- subset(Full_data_with_SettlementAmount, gvkey =='25119')
MOHAWK_INDUSTRIES$gvkey <- NULL
MOHAWK_INDUSTRIES$if_sued <- NULL
MOHAWK_INDUSTRIES$SettlementAmount <- NULL
MOHAWK_INDUSTRIES$Ticker <- NULL
dim(MOHAWK_INDUSTRIES)
View(MOHAWK_INDUSTRIES)

MOHAWK_INDUSTRIES_PREDICT <- MOHAWK_INDUSTRIES
# c. Remove MOHAWK_INDUSTRIES from the full dataset before using the data to train the models
Fund_SCA_Full_complete <- Full_data_with_SettlementAmount[Full_data_with_SettlementAmount$gvkey != "25119", ] 
Fund_SCA_Full_complete$if_sued <- ifelse(Fund_SCA_Full_complete$if_sued==0,'No','Yes')
Fund_SCA_Full_complete$if_sued <- as.factor(Fund_SCA_Full_complete$if_sued)
Fund_SCA_Full_complete$SettlementAmount <- NULL
Fund_SCA_Full_complete$gvkey <- NULL
Fund_SCA_Full_complete$Ticker <- NULL
dim(Fund_SCA_Full_complete)

# Class imbalance 
t <- as.vector(Fund_SCA_Full_complete$if_sued)
t <- table(t)
barplot(t,main = "Proportion of Response Variable",
        xlab = "if_sued",
        col = "cornflowerblue")
#---------------------------------------------


# 2.A Data Partitioning - split into train and test datasets based on the balanced dataset 'balance.data.full'
set.seed(42) 
balance.data.full <- SMOTE(if_sued~. , data = Fund_SCA_Full_complete, perc.over =200, k=5)
dim(balance.data.full) # 1127  232

index <- createDataPartition(balance.data.full$if_sued, p = 0.7, list = FALSE)
train_data_balanced <- balance.data.full[index, ]
test_data_balanced  <- balance.data.full[-index, ]
test_data_balanced_without_y <- test_data_balanced[, !colnames(test_data_balanced)%in%c('if_sued')]

round(prop.table(table(train_data_balanced$if_sued))*100,2); round(prop.table(table(test_data_balanced$if_sued))*100,2)

dim(MOHAWK_INDUSTRIES_PREDICT) # 1 228

balance.col_names <- as.vector(colnames(balance.data.full))
MOHAWK_INDUSTRIES_Balance <- MOHAWK_INDUSTRIES_PREDICT[, colnames(MOHAWK_INDUSTRIES_PREDICT) %in% c(balance.col_names)]
MOHAWK_INDUSTRIES_Balance <- as.data.frame(MOHAWK_INDUSTRIES_Balance)
#-----------
# 2.B Partioning the data based on Boruta package
Boruta_model <- Boruta(if_sued~., balance.data.full, doTrace = 0, maxRuns=500)
# Save:
# saveRDS(Boruta_model, file = "Boruta_model.Rds")
Boruta_model <- readRDS(file = "Boruta_model.Rds")

boruta.tentitive.fix <- TentativeRoughFix(Boruta_model)
Boruta.finalvars = as.vector(getSelectedAttributes(Boruta_model, withTentative = T))  #  218 variables to be important out of 229

Boruta.balance.data.full <- balance.data.full[, colnames(balance.data.full)%in% c(Boruta.finalvars, 'if_sued')] # create new dataset for only variables deemed important from boruta model
Boruta.balance.data.full <- as.data.frame(Boruta.balance.data.full)

Boruta.index <- createDataPartition(Boruta.balance.data.full$if_sued, p = 0.7, list = FALSE)
Boruta.train_data <- Boruta.balance.data.full[Boruta.index, ]
Boruta.test_data  <- Boruta.balance.data.full[-Boruta.index, ]
Boruta.test_data_without_y <- Boruta.test_data[, !colnames(Boruta.test_data)%in%c('if_sued')]

round(prop.table(table(Boruta.train_data$if_sued))*100,2); round(prop.table(table(Boruta.test_data$if_sued))*100,2)

Boruta.col_names <- as.vector(colnames(Boruta.balance.data.full))
MOHAWK_INDUSTRIES_boruta <- MOHAWK_INDUSTRIES_PREDICT[, colnames(MOHAWK_INDUSTRIES_PREDICT) %in% c(Boruta.col_names)]
MOHAWK_INDUSTRIES_boruta <- as.data.frame(MOHAWK_INDUSTRIES_boruta)
Z_MOHAWK_INDUSTRIES_boruta <- scale(MOHAWK_INDUSTRIES_boruta, center = TRUE, scale=TRUE)
#-----------
# 3.B Data Partitioning - split based on the vairables where multicollinarity has been removed 
# Note: The absolute values of pair-wise correlations are considered. If two variables have a high correlation, the function looks at the mean absolute correlation of each variable and removes the variable with the largest mean absolute correlation.
# This function searches through a correlation matrix and returns a vector of integers corresponding to columns to remove to reduce pair-wise correlations.
Boruta.balance.data.full_temp <- Boruta.balance.data.full
Boruta.balance.data.full_temp$if_sued <- ifelse(Boruta.balance.data.full_temp$if_sued=='No',0, 1)
remove.collinearity <- findCorrelation(cor(Boruta.balance.data.full_temp), cutoff = .80, verbose = T, names = T, exact = TRUE)
length(remove.collinearity)  # 156 variables to remove

balance.data.no.collinearity <- Boruta.balance.data.full_temp[, !colnames(Boruta.balance.data.full_temp)%in% c(remove.collinearity)] # all vars except the ones in remove.collinearity
balance.data.no.collinearity <- as.data.frame(balance.data.no.collinearity)

coll.index <- createDataPartition(balance.data.no.collinearity$if_sued, p = 0.7, list = FALSE)
coll.train_data <- balance.data.no.collinearity[coll.index, ]
coll.test_data  <- balance.data.no.collinearity[-coll.index, ]

coll.train_data$if_sued <- ifelse(coll.train_data$if_sued==0,'No','Yes')
coll.train_data$if_sued <- as.factor(coll.train_data$if_sued)

coll.test_data$if_sued <- ifelse(coll.test_data$if_sued==0,'No','Yes')
coll.test_data$if_sued <- as.factor(coll.test_data$if_sued)

coll.test_data_without_y <- coll.test_data[, !colnames(coll.test_data)%in%c('if_sued')]

round(prop.table(table(coll.train_data$if_sued))*100,2); round(prop.table(table(coll.test_data$if_sued))*100,2)

coll.col_names <- as.vector(colnames(balance.data.no.collinearity))
MOHAWK_INDUSTRIES_coll <- MOHAWK_INDUSTRIES_PREDICT[, colnames(MOHAWK_INDUSTRIES_PREDICT) %in% c(coll.col_names)]
MOHAWK_INDUSTRIES_coll <- as.data.frame(MOHAWK_INDUSTRIES_coll)
Z_MOHAWK_INDUSTRIES_coll <-as.data.frame(scale(MOHAWK_INDUSTRIES_coll, center = TRUE, scale = TRUE))
#----------------------------------------------

# 4. Build Models: 

# a. Random Forests. We will use three random forest models:
# First Model:  will use the balance.data.full set which has dim = 1127 rows and 229 variables 
# Second Model: will use variables used by Boruta package that uses random forest to choose confirmed variables and remove unimportant variables 
# Third Model:  will use a new dataset where multicollinearity between variables are removed. So it will have only 126 variables 

# I. RANDOM FOREST with balance.data.full:
set.seed(42)
ctrl_orig <- trainControl(method = "repeatedcv", 
                          number = 10, 
                          repeats = 10, 
                          verboseIter = FALSE)

rf.model1 <-             caret::train(if_sued~.,
                                      data = train_data_balanced,
                                      method = "rf",
                                      preProcess = c("scale", "center"),
                                      trainControl = ctrl_orig)

# predict model on test data
pred1 <- predict(rf.model1, newdata = test_data_balanced_without_y, type = "prob")
final1 <- data.frame(actual = test_data_balanced$if_sued,  pred1)
head(final1)

# Confusion Matrix 
final1$predict <- as.factor(ifelse(final1$Yes > 0.5, "Yes", "No"))
cm <- confusionMatrix(final1$predict, test_data_balanced$if_sued, positive = 'Yes')
cm  # Accuracy : 0.8813

cm_balance.data.set <- cm


# Variable Importance
dev.off()
balance.data.varImp1 <- (varImp(rf.model1, scale = TRUE))
plot(balance.data.varImp1, top = 25, main="Variable Importance - Random Forest Model")

# Plot ROC and AUC
library('pROC')
test_data_balanced$if_sued <- ifelse(test_data_balanced$if_sued=='Yes',1, 0)
final1$predict <- ifelse(final1$predict=='Yes',1,0)
AUC1 <- roc(test_data_balanced$if_sued, final1$predict)
AUC.balance.data <- plot(roc(test_data_balanced$if_sued, final1$predict)) # Area under the curve: 0.877
AUC.balance.data

# Predict on MOHAWK_INDUSTRIES 
MOHAWK_INDUSTRIES_RF_Predict <- predict(rf.model1, newdata = MOHAWK_INDUSTRIES_Balance, type = "prob")
MOHAWK_INDUSTRIES_RF_Predict * 100  #       NO:40  Yes:60


# II. RANDOM FOREST with Boruta data set:

rf.model2 <-             caret::train(if_sued~.,
                                      data = Boruta.train_data,
                                      method = "rf",
                                      preProcess = c("scale", "center"),
                                      trainControl = ctrl_orig)


# predict model on test data
pred2 <- predict(rf.model2, newdata = Boruta.test_data_without_y, type = "prob")
final2 <- data.frame(actual = Boruta.test_data$if_sued,  pred2)
head(final2)

# Confusion Matrix 
final2$predict <- as.factor(ifelse(final2$Yes > 0.5, "Yes", "No"))
cm2 <- confusionMatrix(final2$predict, Boruta.test_data$if_sued, positive = 'Yes')
cm2  # Accuracy : 0.908 


# Variable Importance
dev.off()
varImp2 <- (varImp(rf.model2, scale = TRUE, label='r'))
plot(varImp2, top = 25, main='Random Forest')



# Plot ROC and AUC
library('pROC')
Boruta.test_data$if_sued <- ifelse(Boruta.test_data$if_sued=='Yes',1, 0)
final2$predict <- ifelse(final2$predict=='Yes',1,0)
AUC2 <- roc(Boruta.test_data$if_sued, final2$predict)
plot(AUC2) # Area under the curve: 0.9029



# Predict on MHK 
borouta.mhk_boruta <- predict(rf.model2,MOHAWK_INDUSTRIES_boruta, type = 'prob')
borouta.mhk_boruta  # No: 0.512 Yes:0.488


# III. RANDOM FOREST with balance.data.full:

rf.model3 <-             caret::train(if_sued~.,
                                      data = coll.train_data,
                                      method = "rf",
                                      preProcess = c("scale", "center"),
                                      trainControl = ctrl_orig)


# predict model on test data
pred3 <- predict(rf.model3, newdata = coll.test_data_without_y, type = "prob")
final3 <- data.frame(actual = coll.test_data$if_sued,  pred3)
head(final3)

# Confusion Matrix 
final3$predict <- as.factor(ifelse(final3$Yes > 0.5, "Yes", "No"))
cm3 <- confusionMatrix(final3$predict, coll.test_data$if_sued, positive = 'Yes')
cm3  # Accuracy : 0.7041

plot(rf.model3)


# Variable Importance
dev.off()
varImp3 <- (varImp(rf.model3, scale = TRUE))
plot(varImp3, top = 25, main='Random Forests')


# Plot ROC and AUC

coll.test_data$if_sued <- ifelse(coll.test_data$if_sued=='Yes',1, 0)
final3$predict <- ifelse(final3$predict=='Yes',1,0)
plot(roc(coll.test_data$if_sued, final3$predict)) # Area under the curve: 0.7002
coll.roc <- roc(coll.test_data$if_sued, final3$predict)


# Predict on MHK
MHK_coll <- predict(rf.model3, MOHAWK_INDUSTRIES_coll, type='prob')
MHK_coll # NO
#----------------------------------------
# Building KNN model on the three datasets: 

# A. KNN on balanced dataset
test_data_balanced$if_sued <- ifelse(test_data_balanced$if_sued==0,'No','Yes')
test_data_balanced$if_sued <- as.factor(test_data_balanced$if_sued)

knn_fit1 <- caret::train(if_sued~., data = train_data_balanced, method = "knn",
                         trControl=ctrl_orig,
                         preProcess = c("center", "scale"),
                         tuneLength = 10)

plot(knn_fit1)
balance.knn_fit1 <- knn_fit1


#Predictions on the test set
predict_knn1 <- predict(knn_fit1, test_data_balanced_without_y)
cm_knn1 <- confusionMatrix(predict_knn1, test_data_balanced$if_sued, positive = 'Yes')
cm_knn1   # Accuracy : 0.7418 


# AUC and ROC 
test_data_balanced$if_sued <- ifelse(test_data_balanced$if_sued=='Yes',1, 0)
predict_knn1 <- ifelse(predict_knn1=='Yes',1,0)
plot(roc(test_data_balanced$if_sued, predict_knn1)) # Area under the curve: 0.7473

# Predict on MOHAWK_INDUSTRIES: 
mhk_knn <- predict(knn_fit1, MOHAWK_INDUSTRIES_Balance)
mhk_knn # yes it will be sued

balance.knn.model <- knn_fit1
balance.cm.knn <- cm_knn1
balance.knn.roc <- roc(test_data_balanced$if_sued, predict_knn1)
balance.mhk_knn <- mhk_knn

#--------------------
# B. KNN on Boruta dataset
# Boruta.test_data$if_sued <- ifelse(Boruta.test_data$if_sued==0,'No','Yes')
# Boruta.test_data$if_sued <- as.factor(Boruta.test_data$if_sued)

knn_fit2 <- caret::train(if_sued~., data = Boruta.train_data, method = "knn",
                         trControl=ctrl_orig,
                         preProcess = c("center", "scale"),
                         tuneLength = 10)

plot(knn_fit2)
#Predictions on the test set
predict_knn2 <- predict(knn_fit2, Boruta.test_data_without_y)
cm_knn2 <- confusionMatrix(predict_knn2, Boruta.test_data$if_sued, positive = 'Yes')
cm_knn2   # Accuracy : 0.7715

# AUC and ROC 
Boruta.test_data$if_sued <- ifelse(Boruta.test_data$if_sued=='Yes',1, 0)
predict_knn2 <- ifelse(predict_knn2=='Yes',1,0)
plot(roc(Boruta.test_data$if_sued, predict_knn2)) # Area under the curve: 0.7586

Boruta.roc.knn <- evalmod(scores = predict_knn2, labels =Boruta.test_data$if_sued)
autoplot(Boruta.roc.knn)
# Predict on MHK 
mhk_boruta <- predict(knn_fit2,MOHAWK_INDUSTRIES_boruta)
mhk_boruta  # Yes

#--------------------

# C. KNN on collinearity removed dataset
coll.test_data$if_sued <- ifelse(coll.test_data$if_sued==0,'No','Yes')
coll.test_data$if_sued <- as.factor(coll.test_data$if_sued)

knn_fit3 <- caret::train(if_sued~., data = coll.train_data, method = "knn",
                         trControl=ctrl_orig,
                         preProcess = c("center", "scale"),
                         tuneLength = 10)

plot(knn_fit3)
#Predictions on the test set
predict_knn3 <- predict(knn_fit3, coll.test_data_without_y)
cm_knn3 <- confusionMatrix(predict_knn3, coll.test_data$if_sued, positive = 'Yes')
cm_knn3   # Accuracy : 0.6124 

# AUC and ROC 
# Needs to be converted to numeric to plot 
test_y <- coll.test_data$if_sued

predict_knn3 <- ifelse(predict_knn3=='Yes',1,0)
test_y <- ifelse(test_y=='Yes',1,0)
roc.knn <- evalmod(scores = predict_knn3, labels =test_y)
autoplot(roc.knn) # Area under the curve: 0.6479

# Predict on MHK
mhk_coll <- predict(knn_fit3, MOHAWK_INDUSTRIES_coll)
mhk_coll # NO

#--------------------

# Building SVM model on the three datasets:

# a. SVM on balanced dataset
test_data_balanced$if_sued <- ifelse(test_data_balanced$if_sued==1,'Yes','No')
test_data_balanced$if_sued <- as.factor(test_data_balanced$if_sued)
grid_radial <- expand.grid(sigma = c(0.01,0.1,1, 10, 100),
                           C = c(0.01,0.1,1, 10, 100))

svm_Radial1 <- train(if_sued ~., data = train_data_balanced, method = "svmRadial",
                     trControl=ctrl_orig,
                     preProcess = c("center", "scale"),
                     tuneGrid = grid_radial,
                     tuneLength = 10)

svm_Radial1  # The final values used for the model were sigma = 0.1 and C = 100.
pred_Radial1 <- predict(svm_Radial1, newdata = test_data_balanced_without_y)

cm_svm1 <- confusionMatrix(pred_Radial1, test_data_balanced$if_sued,positive = 'Yes')
cm_svm1 # Accuracy : 0.8635

# AUC and ROC 
test_y <- test_data_balanced$if_sued
roc.svm <- evalmod(scores = predict_svm, labels =test_y)
autoplot(roc.svm) # Area under the curve: 0.8709

# Predict on MHK:
mhk_svm <- predict(svm_Radial1, MOHAWK_INDUSTRIES_Balance)
mhk_svm  # no 


#--------------------

# B. SVM on Boruta dataset

Boruta.test_data$if_sued <- ifelse(Boruta.test_data$if_sued==0,'No','Yes')
Boruta.test_data$if_sued <- as.factor(Boruta.test_data$if_sued)

svm_Radial2 <- train(if_sued ~., data = Boruta.train_data, method = "svmRadial",
                     trControl=ctrl_orig,
                     preProcess = c("center", "scale"),
                     tuneGrid = grid_radial,
                     tuneLength = 10, 
                     probability = TRUE)

svm_Radial2  # The final values used for the model were sigma = 0.1 and C = 100.
pred_Radial2 <- predict(svm_Radial2, newdata = Boruta.test_data_without_y)

cm_svm2 <- confusionMatrix(pred_Radial2, Boruta.test_data$if_sued, positive = 'Yes')
cm_svm2 # Accuracy : 0.8843 

# AUC and ROC 
test_y <- Boruta.test_data$if_sued
roc.svm <- evalmod(scores = predict_svm2, labels =test_y)
autoplot(roc.svm) # # Area under the curve: 0.8501

# Predict on MHK 
mhk_boruta <- predict(svm_Radial2,MOHAWK_INDUSTRIES_boruta)
mhk_boruta  # NO


boruta.svm.model <- svm_Radial2
boruta.svm.cm <- cm_svm2
boruta.svm.roc <- roc.svm
boruta.svm.mhk <- mhk_boruta

#--------------------

# C. SVM on collinearity removed dataset

# coll.test_data$if_sued <- ifelse(coll.test_data$if_sued==0,'No','Yes')
# coll.test_data$if_sued <- as.factor(coll.test_data$if_sued)

svm_Radial3 <- train(if_sued ~., data = coll.train_data, method = "svmRadial",
                     trControl=ctrl_orig,
                     preProcess = c("center", "scale"),
                     tuneGrid = grid_radial,
                     tuneLength = 10,
                     metric="Accuracy")

svm_Radial3  # The final values used for the model were sigma = 100 and C = 100.
pred_Radial3 <- predict(svm_Radial3, newdata = coll.test_data_without_y)
plot(svm_Radial3)
cm_svm3 <- confusionMatrix(pred_Radial3, coll.test_data$if_sued, positive = 'Yes')
cm_svm3 # Accuracy : 0.6686

# AUC and ROC 
library(precrec)
test_y <- ifelse(coll.test_data$if_sued=='Yes',1,0)
roc.svm <- evalmod(scores = predict_svm3, labels =test_y)
autoplot(roc.svm) # ROC 0.6369447


# Predict on MHK 
mhk_svm <- predict(svm_Radial3, MOHAWK_INDUSTRIES_coll)
mhk_svm # Yes
#--------------------

# Averaging the results of all models: 

# RF:
train.balance.rf <- list(train_data_balanced , cm_balance.data.set, balance.data.varImp1, AUC1 ,MOHAWK_INDUSTRIES_RF_Predict, rf.model1)
train.boruta.rf <- list(train_data_boruta, cm.borouta, varimpor.borouta, AUC2, borouta.mhk_boruta, rf.model2)
coll.train_data <- list(coll.model.rf, coll.varimpor, coll.cm, coll.roc,MHK_coll)

train.balance.rf # Accuracy : 0.8843
train.boruta.rf  # Accuracy : 0.905
coll.train_data # Accuracy : 0.8639 

MOHAWK_INDUSTRIES_RF_Predict # 60% yes
borouta.mhk_boruta # 52% No
MHK_coll # 59% No

plot(balance.data.varImp1, top = 25, main="Variable Importance - Random Forest Model")
plot(varimpor.borouta, top = 25, main="Top 15 Predictor Importance - RF Model")
plot(coll.varimpor, top = 25, main="Variable Importance - Random Forest Model")


plot(coll.varimpor, top = 15, main="Top 15 Predictor Importance - RF Model")
varImpPlot(coll.varimpor$importance)



# KNN:
train.balance.knn <- list(balance.knn.model, balance.cm.knn, balance.knn.roc, balance.mhk_knn)
train.boruta.knn <- list(boruta.knn.model, boruta.knn.cm, boruta.knn.roc, mhk_boruta)
train.coll.knn <- list(coll.knn.model,coll.knn.cm , coll.knn.roc,coll.knn.mhk )

dev.off()
plot(balance.knn.model, main='229 Predictors',xlab='K = 5') # Accuracy : 0.7567  kappa 49
plot(boruta.knn.model, main='218 Predictors',xlab='K = 5') # Accuracy : 0.7656   kappa 40
plot(coll.knn.model, main='62 Predictors',xlab='K = 5')   # Accuracy : 0.7929  kappa 48



# Radial SVM:
train.balance.svm <- list(balance.svm.model, balance.svm.cm, balance.svm.roc, balance.svm.mhk)
train.boruta.svm <- list(boruta.svm.model, boruta.svm.cm, boruta.svm.roc, boruta.svm.mhk)
train.coll.svm <- list(coll.svm.model,coll.svm.cm , coll.svm.roc,coll.svm.mhk )


plot(balance.svm.model, main='SVM sigma = 0.1 and C = 100')
plot(boruta.svm.model,ylab='Accuracy' ,main='SVM sigma = 0.1 and C = 100')
plot(coll.svm.model, main='SVM sigma = 0.1 and C = 100')

balance.svm.cm # Accuracy : 0.8724 , sens: 0.8472 , spec: 0.8912
boruta.svm.cm # Accuracy : 0.8843, sens: 0.8194, spec: 0.9326
coll.svm.cm # Accuracy : 0.6627, sens: 0.6014 , spec: 0.7077





#------- SEVERITY ESTIMATION CODE -------------------------

# a. This is the full complete dataset with both response variables settl.amunt, and if_sued
# saveRDS(Full_data_with_SettlementAmount, file = "Full_data_with_SettlementAmount.Rds")
Full_data_with_SettlementAmount <- readRDS(file = "Full_data_with_SettlementAmount.Rds")
dim(Full_data_with_SettlementAmount) # 904 232

ratio_temp <- Full_data_with_SettlementAmount
# Do additional Calculations:

Full_data_with_SettlementAmount$MarketCap.Settl.amnt.perc <-  round(100 *(Full_data_with_SettlementAmount$mkvalt/Full_data_with_SettlementAmount$SettlementAmount),4)
Full_data_with_SettlementAmount$MarketCap.Settl.amnt.perc

# Add new variable settlement_level <- Low or high
Full_data_with_SettlementAmount$settlement.cat <- ifelse(Full_data_with_SettlementAmount$SettlementAmount< 20000000, 0, 1)

# b. Extract the company so we can predict it. Remove response variables + unneeded ones
MOHAWK_INDUSTRIES_Regression_dataset <- subset(Full_data_with_SettlementAmount, gvkey =='25119')
MOHAWK_INDUSTRIES_Regression_dataset$gvkey <- NULL
MOHAWK_INDUSTRIES_Regression_dataset$if_sued <- NULL
MOHAWK_INDUSTRIES_Regression_dataset$Ticker <- NULL
MOHAWK_INDUSTRIES_Regression_dataset$SettlementAmount <- NULL
MOHAWK_INDUSTRIES_Regression_dataset$settlement.cat <- NULL
MOHAWK_INDUSTRIES_Regression_dataset$MarketCap.Settl.amnt.perc <- NULL

# This new variable is where prediction for our company happens
MOHAWK_INDUSTRIES_PREDICT <- as.data.frame(MOHAWK_INDUSTRIES_Regression_dataset)

# Remove MOHAWK_INDUSTRIES from the full dataset before using the data to train the models
full_data_set <- Full_data_with_SettlementAmount[Full_data_with_SettlementAmount$gvkey != "25119", ] 
full_data_set$if_sued <- NULL
full_data_set$gvkey <- NULL
full_data_set$Ticker <- NULL

# Select only variables where settlement amount is not NA 
full_data_set <- subset(full_data_set, !is.na(full_data_set$SettlementAmount))


# Save to use:
# saveRDS(full_data_set, file = "full_data_set.Rds")
full_data_set <- readRDS(file = "full_data_set.Rds")
full_data_set <- as.data.frame(full_data_set)
#--------------------------------------------------------------------




#---------------------------------------------
# Using only variables selected by boruta algorithm:
Boruta_model <- readRDS(file = "Boruta_model.Rds")
boruta.tentitive.fix <- TentativeRoughFix(Boruta_model)
Boruta.finalvars = as.vector(getSelectedAttributes(Boruta_model, withTentative = F))  #  218 variables to be important out of 229
Boruta.balance.data.full <- full_data_set[, colnames(full_data_set)%in% c(Boruta.finalvars, 'mkvalt','MarketCap.Settl.amnt.perc','SettlementAmount','settlement.cat')] # create new dataset for only variables deemed important from boruta model

set.seed(123)
smp_size1 <- floor(0.70 * nrow(Boruta.balance.data.full))
Boruta.train_ind <- sample(seq_len(nrow(Boruta.balance.data.full)), size = smp_size1)
Boruta.train <- Boruta.balance.data.full[train_ind, ]
Boruta.test <- Boruta.balance.data.full[-train_ind, ]

#---------------------------------------------

# This is the data where collinearity between variables have been removed: 
remove.collinearity <- findCorrelation(cor(Boruta.balance.data.full), cutoff = .85, verbose = T, names = T, exact = TRUE)
length(remove.collinearity)  # 156 variables to remove
remove.collinearity <- as.vector(remove.collinearity)

x <- remove.collinearity[!remove.collinearity %in% c('mkvalt','MarketCap.Settl.amnt.perc','SettlementAmount','settlement.cat')] # all except these 
collinearity_dataset <- Boruta.balance.data.full[, !colnames(Boruta.balance.data.full)%in% c(x)]


collinearity_dataset$settlement.cat <- NULL
collinearity_dataset$MarketCap.Settl.amnt.perc <- NULL

dim(collinearity_dataset)

set.seed(123)
smp_size <- floor(0.70 * nrow(collinearity_dataset))
train_ind <- sample(seq_len(nrow(collinearity_dataset)), size = smp_size)
collinearity.train <- collinearity_dataset[train_ind, ]
collinearity.test <- collinearity_dataset[-train_ind, ]

z_collinearity.train <- as.data.frame(scale(collinearity.train, center = T, scale = T))
z_collinearity.test <- as.data.frame(scale(collinearity.test, center = T, scale = T))

t <- colnames(collinearity.train)

MOHAWK_Predict <- MOHAWK_INDUSTRIES_PREDICT[, colnames(MOHAWK_INDUSTRIES_PREDICT) %in% c(t)]
MOHAWK_Predict$SettlementAmount <- NULL

dim(MOHAWK_Predict)
dim(collinearity.train)

#---------------------------------------------
# Settlement amount Distribution 
hist(Boruta.balance.data.full$SettlementAmount, 
     breaks = 40, col = '#009999',main="Severity - Settlement Amount", 
     xlab="Severity", las=1,
     include.lowest=TRUE, plot = TRUE)


boxplot(log(Boruta.balance.data.full$SettlementAmount),
        main = "Severity - Settlement Amount",
        xlab = "log(settlement amount)",
        col = '#009999',
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)



# Correlation between mkvalt and ratio of mkvalue/settlement amount
ratio_temp <- collinearity_dataset

ratio_temp$mark.ratio <- round(100*(ratio_temp$mkvalt/ratio_temp$SettlementAmount),4)
ratio_temp$mark.ratio

plot(ratio_temp$mark.ratio, ratio_temp$mkvalt)
#---------------------------------------------

# Function to calculate MSE
calc_rmse = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}
#---------------------------------------------

# MODELS TO EXAMINE: USE MULTIPLE DATASETS:

# 5. Use L2 and L1 regularization: 

# no Regularization - general linear model
lmNoReg=lm(SettlementAmount~.,collinearity.train)
summary(lmNoReg)
pred_no_reg=predict(lmNoReg, collinearity.test)
(linear_Reg_rmse = calc_rmse(pred_no_reg, collinearity.test$SettlementAmount)) 


# RIDGE regularisation

x.tr <- model.matrix(SettlementAmount ~ ., data = collinearity.train)[, -1]  
y.tr <- z_collinearity.train$SettlementAmount

x.val <- model.matrix(SettlementAmount ~ ., data = collinearity.test)[,  -1]
y.val <- collinearity.test$SettlementAmount

# CV to obtain best lambda
set.seed(10)
rr.cv <- cv.glmnet(x.tr, y.tr, alpha = 0)
plot(rr.cv)

rr.bestlam <- rr.cv$lambda.min
rr.goodlam <- rr.cv$lambda.1se

# predict validation set using best lambda and calculate RMSE
rr.fit <- glmnet(x.tr, y.tr, alpha = 0)
plot(rr.fit, xvar = "lambda", label = TRUE)

# 6. Use ensemble or weak learner as they're more robust to small data size:
rr.pred <- predict(rr.fit, s = rr.bestlam, newx = x.val)
(ridge_rmse = calc_rmse(rr.pred, y.val))  # 6690027


MHK_pred  <- as.matrix(MOHAWK_Predict)
mhk.rr.pred <- predict(rr.fit, s = rr.bestlam, newx = MHK_pred)
mhk.rr.pred # 20,765,195


# LASSO:
# CV to obtain best lambda
set.seed(10)
las.cv <- cv.glmnet(x.tr, y.tr, alpha = 1)
plot(las.cv)
las.bestlam <- las.cv$lambda.min
las.goodlam <- las.cv$lambda.1se

# predict validation set using best lambda and calculate RMSE
las.fit <- glmnet(x.tr, y.tr, alpha = 1)
plot(las.fit, xvar = "lambda", label = TRUE)
plot(las.fit,xvar="dev",label=TRUE)

las.pred <- predict(las.fit, s = las.bestlam, newx = x.val)

(lasso_rmse = calc_rmse(las.pred, y.val)) # 6,690,027

# Predict on MHK
MHK_pred  <- as.matrix(MOHAWK_Predict)
mhk.las.pred <- predict(las.fit, s = las.bestlam, newx = MHK_pred)
mhk.las.pred # 20,627,000

# -----------

# Tree Model:
tree_model = rpart(SettlementAmount ~ ., data = collinearity.train)
tree_pred = predict(tree_model, newdata = collinearity.test)
plot(tree_pred, collinearity.test$SettlementAmount, 
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Single Tree, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)
(tree_tst_rmse = calc_rmse(tree_pred, collinearity.test$SettlementAmount)) # 9,951,619

# Predict MHK 
MHK <- MOHAWK_Predict 

tree_pred_mhk = predict(tree_model, newdata = MHK, interval="predict")
tree_pred_mhk  # 57,071,429


# Random Forest 

mtry <- round((dim(collinearity.train)[2])/3,0)
rf_bag = randomForest(SettlementAmount ~ ., data = collinearity.train, mtry = mtry, importance = TRUE, ntrees = 100)
rf_bag

rf_pred = predict(rf_bag, newdata = collinearity.test)
plot(rf_pred,collinearity.test$SettlementAmount,
     xlab = "Predicted", ylab = "Actual",
     main = "Predicted vs Actual: Bagged Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

(rf_rmse = calc_rmse(rf_pred, Boruta.test$SettlementAmount)) # 36,719,077

plot(rf_bag, col = "dodgerblue", lwd = 2, main = "Bagged Trees: Error vs Number of Trees")
grid()

mhk_rf_pred = predict(rf_bag, newdata = MHK, interval="predict")
mhk_rf_pred  #  27,441,548 


# RF Bourta dataset var importance
boruta.var_imp <- as.data.frame(importance(rf_bag, type = 1))
boruta.var_imp$rank <- boruta.var_imp$`%IncMSE`
boruta.row_names_rf <- subset(rownames(boruta.var_imp), boruta.var_imp$rank >=1.5)
boruta.row_names_rf <- as.vector(boruta.row_names_rf)
boruta.var.importance <- varImpPlot(rf_bag, type = 1, sort = TRUE, main = 'Variable Importance RF Model')
boruta.var.importance

coll.var_imp <- as.data.frame(importance(rf_bag, type = 1))
coll.var_imp$rank <- coll.var_imp$`%IncMSE`
coll.row_names_rf <- subset(rownames(coll.var_imp), coll.var_imp$rank >=1.5)
coll.row_names_rf <- as.vector(coll.row_names_rf)
coll.var.importance <- varImpPlot(rf_bag, type = 1, sort = TRUE, main = 'Variable Importance RF Model')
coll.var.importance # variables with a large mean decrease in accuracy are more important


# Boosting 
model_boost = gbm(SettlementAmount ~ ., data = collinearity.train, distribution = "gaussian", n.trees = 100, interaction.depth = 4, shrinkage = 0.01,n.minobsinnode = 1)
boost_pred <- predict(model_boost, newdata = collinearity.test, n.trees = 100)
(boost_rmse = calc_rmse(boost_pred, collinearity.test$SettlementAmount)) # 59,520,868

plot(boost_pred, collinearity.test$SettlementAmount,
     xlab = "Predicted", ylab = "Actual", 
     main = "Predicted vs Actual: Boosted Model, Test Data",
     col = "dodgerblue", pch = 20)
grid()
abline(0, 1, col = "darkorange", lwd = 2)

MHK_boost_pred <- predict(model_boost, newdata = MHK, n.trees = 100)
MHK_boost_pred # 42,410,405

# -------------------------------

# First attempt with Boruta.train - Results from all models
boruta.data.results <- (settlement_rmse = data.frame(
  Model = c("Linear regression","Lasso Model", "Ridge Model", "Bagging",  "Random Forest","Boosted Tree"),
  TestError = c(boruta.linear_Reg_rmse,boruta.lasso_rmse, boruta.ridge_rmse, boruta.tree_tst_rmse, boruta.rf_rmse, boruta.boost_rmse)
))



# Second attempt with multicolleaniry removed  - Results from all models
coll.all.models.results <- (coll.settlement_rmse = data.frame(
  coll.Model = c("Linear regression","Lasso Model", "Ridge Model", "Bagging",  "Random Forest","Boosted Tree"),
  coll.TestError = c(linear_Reg_rmse,lasso_rmse, ridge_rmse, tree_tst_rmse, rf_rmse, boost_rmse)
))
# -------------------------------

# Getting all boosted trees from the 4 models, which represent the lowest test errors among all models
boruta.data.test_errors <- c(13375524, 13375524, 23359488,8900335, 15387446)

# barchart with added parameters

barplot(sort(boruta.data.test_errors, decreasing = TRUE),
        main = "Test Errors for Different Models",
        xlab = "RMSE",
        ylab = "Data Sets",
        names.arg = c("Lasso", "Ridge", "Bagging","Random Forest","Boosted Trees"),
        col = "darkred",
        horiz = F, 
        angle = 45)



# Same thing for collinear dataset:
coll.data.test_errors <- c(13375524, 14581762, 29720104,20452111, 25399357)

# barchart with added parameters

barplot(sort(coll.data.test_errors, decreasing = TRUE),
        main = "Test Errors for Different Models",
        xlab = "RMSE",
        ylab = "Data Sets",
        names.arg = c( "Lasso", "Ridge", "Bagging","RF","Boosted Trees"),
        col = "cornflowerblue",
        horiz = F, 
        angle = 90)

# GBMs build an ensemble of shallow and weak successive trees with each tree learning and improving on the previous. 
# When combined, these many weak successive trees produce a powerful “committee” that are often hard to beat with other algorithms.

# Predict using all models then average the results: 
# Use confidence intervals 
# We use only collinearly removed dataset:


# These are the final results for prediction in dollar
MHK_boost_pred # 42,410,405
tree_pred_mhk  # 57,071,429
mhk_rf_pred  #  27,441,548 
mhk.las.pred # 20,627,000
mhk.rr.pred # 20,765,195


predictions_final <- c(MHK_boost_pred, tree_pred_mhk,mhk_rf_pred, mhk.las.pred, mhk.rr.pred)
dollar <- c(42410405, 57071429, 27441548, 20627000, 20765195)
median(dollar)

barplot(sort(predictions_final, decreasing = TRUE),
        main = "MOHAWK INDUSTRIES SEVERITY",
        xlab = "Model",
        names.arg = c( "GBM", "Bagging", "RF","Lasso","Ridge"),
        col = viridis(10),
        las=2,
        horiz = F, 
        cex.axis = 0.75,
        angle = 45)
legend("topright", c("$42,410,405","$57,071,429",'$27,441,548','$20,627,000','$20,765,195'), pch=15, 
       cex = 0.75,
       
       col= viridis(10), 
       bty="n")

#################################################################################
