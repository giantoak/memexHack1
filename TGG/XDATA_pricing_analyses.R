###############################################################################

# Client: Giant Oak
# Author(s): TGG
# Date created: 2016 01 15
# Purpose: Perform XDATA Analyses

###############################################################################
# Set the working directory and default paths
################################################################################

clientName <- 'Giant Oak'
projectName <- '01 Human Trafficking'
serverPath <- '~/Shared/00 Clients - Current'

rawDatasets <- ''
#paste(serverPath, '/', clientName, '/', projectName,
                     #'/Structured Data/01 Raw Datasets', sep="")
workingDatasets <- ''
#paste(serverPath, '/', clientName, '/', projectName,
                         #'/Structured Data/02 Working Datasets/xdata/', sep="")
finalDatasets <- ''

#paste(serverPath, '/', clientName, '/', projectName,
                       #'/Structured Data/03 Final Datasets/xdata/', sep="")
graphics <- ''
#paste(serverPath, '/', clientName, '/', projectName,
                  #'/Structured Data/04 Graphics and Output Data/xdata/', sep="")

regressions <- graphics
regression_objects <- finalDatasets
#code <- '~/Home/Git/'
code <- getwd()

################################################################################
# load in libraries and source files
################################################################################

# libraries:
librariesToLoad <- c("jsonlite", 'plyr',"ggplot2", "curl", "httr", 'data.table', "sandwich", "zoo", "lmtest", "stargazer")



sapply(librariesToLoad, function(package) {
  if(require(package, character.only=TRUE)){     
    print(paste(package, "loaded correctly", sep=" "))
  } else {
    print(paste("Installing", package, sep=" "))
    install.packages(package)
    if(require(package, character.only=TRUE)){
      print(paste(package, "loaded correctly", sep=" "))
    } else {
      stop(paste("Could not install", package, sep=" "))
    }
  }
})

search()

# Start time for source files
start_time <- Sys.time()

# Start time for whole thing
rstudio_test_start <-Sys.time()

# source files
#sourceFiles <- c('XDATA/xdata_analyses_functions', "XDATA/MSA_level_building")
sourceFiles <- c('xdata_analyses_functions')


if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, "/", x, ".R", sep = "")))

# End time for source files
end_source_files <- Sys.time()
paste0("It took ", end_source_files - start_time, " to load source files")


################################################################################
# Choose global vars
################################################################################
#Choose clustering variables for regression
clustering_variable <- 'CBSACode'

#loosen spam filter and restrict to hourly rates for regressions (test robustness - not for final specifications)
include_spam_exclude_nonhourlyrates <- FALSE


#skip some validations and summary statistics for speedier run
shortcut <- TRUE




#old globals (default: true)
#replace prostitution rate with proportion of providers arrested?
replace_prostitution_rate <- TRUE


#use new CDR data or no?
use_cdr_data <- TRUE

#Do you want many new MSAs? (irrelevant for CDR data)
more_msas_change <- TRUE

#Do you want to use TGG corrected prices? (irrelevant for CDR data)
use_tgg_prices <- TRUE




################################################################################
# Load & ready Ad-level data
################################################################################
# LOAD ####
if (use_cdr_data) {
  
  
  ad_price_ad_level <- fread(paste0(workingDatasets, "ad_price_ad_level_cdr.tsv"),
                             sep="\t",stringsAsFactors = FALSE, data.table=TRUE, colClasses = c('msa_code' = 'character', "flags" = "character"))
} else {
  #Initial download
  #temp <- tempfile()
  #download.file("http://giantoakmemex.s3.amazonaws.com/sex_ad_analysis/output/ad_price_ad_level.zip",temp)
  #ad_price_ad_level <- read.csv(unz(temp, "ad_price_ad_level.csv"), stringsAsFactors = FALSE, na.strings = "")
  #unlink(temp)
  #saveRDS(ad_price_ad_level, paste0(workingDatasets, "ad_price_ad_level.RDS"))
  #Subsequent reloading
  
  ad_price_ad_level <- readRDS(paste0(workingDatasets, "ad_price_ad_level.RDS"))
  
}

str(ad_price_ad_level)

if (!shortcut){
isTableAtThisLevel(ad_price_ad_level, c('ad_id'))
#ad-level
}
nrow(ad_price_ad_level)
#4.6million for CDR
#ad_id-level -- LATER TO-DO: correct for URLs

#Var name changes
if (use_cdr_data) {
  ad_price_ad_level$CBSACode <- ad_price_ad_level$msa_code
  ad_price_ad_level$msa_code <- NULL
  ad_price_ad_level$price_per_hour <- ad_price_ad_level$price_per_hour_tgg
  ad_price_ad_level$date_str <- ad_price_ad_level$post_date
  ad_price_ad_level$is_massage_parlor_ad <- ifelse(ad_price_ad_level$massage_ad == 'massage parlor', 1, 0)
  print(table(ad_price_ad_level$massage_ad, ad_price_ad_level$is_massage_parlor_ad))
  #about 100K massage ads. 11K unknown
  ad_price_ad_level$website <- ad_price_ad_level$site                             
} else {
  #clean T/F vars
  true_false_vars <- c('spam', 'X1hr', colnames(ad_price_ad_level)[grepl('call', colnames(ad_price_ad_level))])
  tf_table <- sapply(ad_price_ad_level[, true_false_vars], table)
  ad_price_ad_level[, true_false_vars] <- sapply(ad_price_ad_level[, true_false_vars], as.logical)
  #Validate
  print(tf_table == sapply(ad_price_ad_level[, true_false_vars], table))
  
  ad_price_ad_level$CBSACode <- gsub('31000US', '', ad_price_ad_level$census_msa_code)
  #Check
  unique(ad_price_ad_level[!is.na(ad_price_ad_level$census_msa_code), c('CBSACode', 'census_msa_code')])[1:10, ]
  ad_price_ad_level$census_msa_code <- NULL
}

# COMPLETENESS CHECKS
sapply(ad_price_ad_level, FUN = function(x) sum(is.na(x)))
sapply(ad_price_ad_level, FUN = function(x) sum(is.na(x))/nrow(ad_price_ad_level))
#CDR: 34% missing MSA code
#old: 31% missing MSA code

length(unique(ad_price_ad_level$CBSACode))
#CDR: 498 represented
# old data: 91 MSAs
min(nchar(ad_price_ad_level$CBSACode[!is.na(ad_price_ad_level$CBSACode)])) == 5 #verify leading zeroes didn't get dropped


# QUICK PRICE SUMMARY


quantile(ad_price_ad_level$price_per_hour, probs = seq(0, 1, 0.1))
quantile(ad_price_ad_level$price_per_hour, probs = seq(0.9, 1, 0.01))
#Top code price at 1500 & 1000 (GO). more than 99th percentile
ad_price_ad_level <- topCodeThis(data = ad_price_ad_level, topCodeValue = 1500, varToTopCode = 'price_per_hour', resultingVar = NULL)
ad_price_ad_level <- topCodeThis(data = ad_price_ad_level, topCodeValue = 1000, varToTopCode = 'price_per_hour', resultingVar = NULL)
ad_price_ad_level <- topCodeThis(data = ad_price_ad_level, topCodeValue = 500, varToTopCode = 'price_per_hour', resultingVar = NULL)

if (!shortcut){
qplot(ad_price_ad_level$price_per_hour_tc1500, geom = 'histogram', binwidth = 50)
qplot(ad_price_ad_level$price_per_hour_tc500, geom = 'histogram', binwidth = 25)
}
summary(ad_price_ad_level$price_per_hour_tc1500)  
summary(ad_price_ad_level$price_per_hour_tc500)

# CREATE ADDITIONAL ANALYSES VARS ####
# Date vars
ad_price_ad_level$date <- as.Date(ad_price_ad_level$date_str)
sum(is.na(ad_price_ad_level$date)) == sum(is.na(ad_price_ad_level$date_str))

ad_price_ad_level$year <- year(ad_price_ad_level$date)
ad_price_ad_level$month <- month(ad_price_ad_level$date)

table(ad_price_ad_level$year, ad_price_ad_level$month)
#CDR: 2005 onwards. up to Aug 2015
#nonCDR: 1900s included


#CALCULATE SIZE OF CLUSTERS (COUNT BY ADS) FOR SPAMMY POSTERS
#GO: 200 over all time get thrown out

if (!use_cdr_data) {
  ad_price_ad_level <- data.table(ad_price_ad_level)
  
  ad_deets_provider_lvl <- ad_price_ad_level[ ,list(total_posts = .N,
                                                    first_post = min(date),
                                                    last_post = max(date)),
                                              by = cluster_id][, posting_duration_days := as.numeric(last_post - first_post)]
  nrow(ad_deets_provider_lvl) == length(unique(ad_price_ad_level$cluster_id))
  
  quantile(ad_deets_provider_lvl$total_posts, probs = seq(0, 1, 0.05))
  #75% of posters post a single time
  quantile(ad_deets_provider_lvl$posting_duration_days[ad_deets_provider_lvl$total_posts > 1], probs = seq(0, 1, 0.05))
  #10% post multiple times in a day; interquartile range is 5-180 days (<1 week - 6 months)
  
  
  ad_price_ad_level[ ,":=" (total_posts_provider_lvl = .N,
                            first_post_provider_lvl = min(date),
                            last_post_provider_lvl = max(date)),
                     by = cluster_id][, posting_days_provider_lvl := as.numeric(last_post_provider_lvl - first_post_provider_lvl)]
  
  str(ad_price_ad_level)
  
  ad_price_ad_level$giant_oak_ad_spam <- ifelse(ad_price_ad_level$total_posts_provider_lvl > 200, 1, 0)
  table(ad_price_ad_level$giant_oak_ad_spam, ad_price_ad_level$spam)
  #these are equivalent variables
  ad_price_ad_level$giant_oak_ad_spam <- NULL
  
} else {
  
  ad_price_ad_level$spam <- ifelse(ad_price_ad_level$cluster_counts > 200, 1, 0)
}
table(ad_price_ad_level$spam )
prop.table(table(ad_price_ad_level$spam ))
summary(ad_price_ad_level$cluster_counts)

if (!use_cdr_data) {  
  #Dummies for website
  #42% backpage -- similar to overall figures (inc. no-price ads)
  
  ad_price_ad_level$website <- gsub("\\:.*", "", ad_price_ad_level$ad_id)
  
  
}
table(ad_price_ad_level$website)
prop.table(table(ad_price_ad_level$website))
#42% backpage -- similar to overall figures (inc. no-price ads)
# CDR: 50% backpage


#Service venue vars
if (use_cdr_data) {
  
  
  table(ad_price_ad_level$incall_outcall)
  ad_price_ad_level$incall_only <- ifelse(ad_price_ad_level$incall_outcall == 'incall', 1, 0)
  ad_price_ad_level$outcall_only <- ifelse(ad_price_ad_level$incall_outcall == 'outcall', 1, 0)
  ad_price_ad_level$both_incall_outcall <- ifelse(ad_price_ad_level$incall_outcall == 'incall and outcall', 1, 0)
  ad_price_ad_level$unclear_inc_out <- ifelse(ad_price_ad_level$incall_outcall == 'unknown', 1, 0)
  
  
} else {
  
  
  #Service venue vars
  
  ad_price_ad_level$incall_only <- ifelse(ad_price_ad_level$incall == TRUE & ad_price_ad_level$outcall == FALSE, 1, 0)
  ad_price_ad_level$outcall_only <- ifelse(ad_price_ad_level$incall == FALSE & ad_price_ad_level$outcall == TRUE, 1, 0)
  ad_price_ad_level$both_incall_outcall <- ifelse(ad_price_ad_level$incall == TRUE & ad_price_ad_level$outcall == TRUE, 1, 0)
  ad_price_ad_level$unclear_inc_out <- ifelse(ad_price_ad_level$incall_only == 0 &
                                                ad_price_ad_level$outcall_only == 0 & 
                                                ad_price_ad_level$both_incall_outcall == 0,
                                              1, 0)
  
  
  
}

if (!shortcut){
print(sapply(as.data.frame(ad_price_ad_level)[ , c('incall_only', 'outcall_only', 'both_incall_outcall', 'unclear_inc_out')], 
       FUN = function(x) table(x, exclude = NULL)))
print(sapply(as.data.frame(ad_price_ad_level)[ , c('incall_only', 'outcall_only', 'both_incall_outcall', 'unclear_inc_out')],
       FUN = function(x) prop.table(table(x, exclude = NULL))))
#CDR: 30% incall, 20% outcall, 40% both, 7% unknown
#old data: half unknown!!!

#Validate
print(all(rowSums(as.data.frame(ad_price_ad_level)[, c('incall_only', 'outcall_only', 'both_incall_outcall', 'unclear_inc_out')]) == 1, na.rm = TRUE))

if (use_cdr_data) {
  
  print(table(ad_price_ad_level$is_massage_parlor_ad, ad_price_ad_level$incall_outcall, exclude = NULL))
  #some massage parlors are incall & outcall and outcall.
  
  
}
}
#Save both incall and outcall dataset for within-provider / within-ad outcall premium

both_inc_out <- ad_price_ad_level[ad_price_ad_level$incall_outcall == 'incall and outcall', ]
prop.table(table(both_inc_out$hour_price_counts))
#20% have none, 50% have 1, 20% have 2

saveRDS(both_inc_out, file = paste0(workingDatasets, 'ad_level_both_inc_out.RDS'))




# Additional merging with old dataset
if (!use_cdr_data) {
  
  
  
  table(ad_price_ad_level$sex_ad, ad_price_ad_level$spam)
  #just under 2MM sex ads that aren't spam
  table(ad_price_ad_level$is_massage_parlor_ad[ad_price_ad_level$spam == FALSE & ad_price_ad_level$sex_ad == 1])
  #all but a few thousand are non-massage parlor ads
  
  # BRING IN TGG PRICES ####
  
  new_prices <- read.table(paste0(workingDatasets, "ad_price_ad_level_beta_tgg.tsv"),
                           sep = '\t', stringsAsFactors = FALSE, header = TRUE)
  str(new_prices)
  new_prices$CBSACode <- gsub('31000US', '', new_prices$census_msa_code)
  
  ad_level_merged <- merge(ad_price_ad_level, new_prices, by = 'ad_id')
  #Validate
  nrow(ad_level_merged) == nrow(ad_price_ad_level)
  ad_level_merged <- are_merged_repeat_vars_same(ad_level_merged)
  #MSA CODES don't match up - INVESTIGATE XXX
  
  ad_price_ad_level <- ad_level_merged
  ad_price_ad_level$CBSACode <- ad_price_ad_level$CBSACode.x
  ad_price_ad_level$CBSACode_other <- ad_price_ad_level$CBSACode.y
  ad_price_ad_level$CBSACode.x <- NULL
  ad_price_ad_level$CBSACode.y <- NULL
  
  print(summary(ad_price_ad_level $price_per_hour - ad_price_ad_level $price_per_hour_go))
  #alex's reconstruction of GO prices matches up perfectly
  print(summary(ad_price_ad_level $price_per_hour_tgg - ad_price_ad_level $price_per_hour_go))
  #our typical price is a little higher. average of $20 higher
  
  print(quantile(ad_price_ad_level $price_per_hour_tgg - ad_price_ad_level $price_per_hour_go, probs = seq(0,1,0.1)))
  
  ggplot(melt (ad_price_ad_level[ad_price_ad_level$price_per_hour_tgg < 500 & ad_price_ad_level$price_per_hour_go < 500, 
                                 list(price_per_hour_tgg, price_per_hour_go)]), mapping = aes (fill = variable, x = value)) + geom_density (alpha = .5)
  ggsave(filename = paste0(graphics, 'TGG vs GO aggregated hourly price comparison.png'))
  
  
  table(ad_price_ad_level$hour_price_counts, ad_price_ad_level$has_hour_price)
  prop.table(table(ad_price_ad_level$hour_price_counts))
  #70% have 1 hourly price. 16% have none. 10% have 2. 
  
  #MSA CODE RECONCILIATION
  length(unique(ad_price_ad_level$CBSACode))
  #91
  length(unique(ad_price_ad_level$CBSACode_other))
  #503?!?!?!?!
  table(ad_price_ad_level$CBSACode_other)[order(table(ad_price_ad_level$CBSACode_other), decreasing = TRUE)]
  unique(ad_price_ad_level$CBSACode_other[!(ad_price_ad_level$CBSACode_other %in% ad_price_ad_level$CBSACode)])
  
  #discrepancies 
  nrow(ad_price_ad_level[(ad_price_ad_level$CBSACode == ad_price_ad_level$CBSACode_other 
                          & !is.na(ad_price_ad_level$CBSACode) & !is.na(ad_price_ad_level$CBSACode_other)), ]) / nrow(ad_price_ad_level)
  #9% agree
  nrow(ad_price_ad_level[(is.na(ad_price_ad_level$CBSACode) & is.na(ad_price_ad_level$CBSACode_other)), ]) / nrow(ad_price_ad_level)
  #22% missing both
  #ie. 30% are not an issue
  
  
  #how many nonmissing and don't match?
  nrow(ad_price_ad_level[(ad_price_ad_level$CBSACode != ad_price_ad_level$CBSACode_other 
                          & !is.na(ad_price_ad_level$CBSACode) & !is.na(ad_price_ad_level$CBSACode_other)) 
                         , ])
  nrow(ad_price_ad_level[(ad_price_ad_level$CBSACode != ad_price_ad_level$CBSACode_other 
                          & !is.na(ad_price_ad_level$CBSACode) & !is.na(ad_price_ad_level$CBSACode_other)) 
                         , ])/nrow(ad_price_ad_level)
  #10% nonmissing and don't match (250K)
  
  
  #how many missing alex reconstruction
  nrow(ad_price_ad_level[(!is.na(ad_price_ad_level$CBSACode) & is.na(ad_price_ad_level$CBSACode_other)), ])
  nrow(ad_price_ad_level[(!is.na(ad_price_ad_level$CBSACode) & is.na(ad_price_ad_level$CBSACode_other)), ]) / nrow(ad_price_ad_level)
  #50% (1MM) not missing Giant Oak's but missing Alex reconstruction
  
  #how many missing GO but not Alex reconstruction
  nrow(ad_price_ad_level[(is.na(ad_price_ad_level$CBSACode) & !is.na(ad_price_ad_level$CBSACode_other)), ])
  nrow(ad_price_ad_level[(is.na(ad_price_ad_level$CBSACode) & !is.na(ad_price_ad_level$CBSACode_other)), ]) / nrow(ad_price_ad_level)
  #9% missing Giant Oak but not Alex reconstru
  
  
  #View(ad_price_ad_level[(ad_price_ad_level$CBSACode != ad_price_ad_level$CBSACode_other 
  #                      & !is.na(ad_price_ad_level$CBSACode) & !is.na(ad_price_ad_level$CBSACode_other)) | 
  #                       (!is.na(ad_price_ad_level$CBSACode) & is.na(ad_price_ad_level$CBSACode_other)) |
  #                       (is.na(ad_price_ad_level$CBSACode) & is.na(ad_price_ad_level$CBSACode_other)), ])[1:100, ]
  
  #Bring in combined MSA dataset
  msa_both <- readRDS(file = paste0(workingDatasets, "msa_both_rounds.RDS"))
  setnames(msa_both, c("census_msa_code"), c("census_msa_code_other"))
  msa_both_dedup <- msa_both[!duplicated(msa_both[['ad_id']]), ]
  msa_both_dedup$CBSACode_other <- gsub('31000US', '', msa_both_dedup$census_msa_code_other)
  msa_both_dedup$census_msa_code_other <- NULL
  
  ad_price_ad_level_merged <- merge(ad_price_ad_level, msa_both_dedup, by = 'ad_id', all.x = TRUE)
  #Validate
  nrow(ad_price_ad_level_merged) == nrow(ad_price_ad_level)
  str(ad_price_ad_level_merged)
  nrow(ad_price_ad_level_merged[!is.na(ad_price_ad_level_merged$CBSACode_other.y), ])
  nrow(ad_price_ad_level_merged[!is.na(ad_price_ad_level_merged$CBSACode), ])
  
  ad_price_ad_level_merged$CBSACode_other <- ad_price_ad_level_merged$CBSACode_other.y
  ad_price_ad_level_merged$CBSACode_other.x <- NULL
  ad_price_ad_level_merged$CBSACode_other.y <- NULL
  ad_price_ad_level <- ad_price_ad_level_merged
  rm(ad_price_ad_level_merged)
  
  length(unique(ad_price_ad_level$CBSACode_other[is.na(ad_price_ad_level$CBSACode)]))
  #additional 423 CBSAs from 200K ads
  length(unique(ad_price_ad_level$CBSACode_other[!is.na(ad_price_ad_level$CBSACode)]))
  #same 90 CBSAs
  
  #Change MSA if necessary
  
  if (more_msas_change) {
    ad_price_ad_level$CBSACode_original <- ad_price_ad_level$CBSACode
    ad_price_ad_level$CBSACode <- ad_price_ad_level$CBSACode_other
    ad_price_ad_level$CBSACode_other <- NULL
  }
  
  
  #use TGG prices
  
  if (use_tgg_prices) {
    ad_price_ad_level$price_per_hour <- ad_price_ad_level$price_per_hour_tgg
  }
}




if (use_cdr_data) {
  #use sex ad == 1 in absence of a marker in CDR data
  ad_price_ad_level$sex_ad <- 1
}



if (!shortcut) {
  
  
# Validations -- venue & massage
print(prop.table(table(ad_price_ad_level$massage_ad, ad_price_ad_level$incall_outcall, exclude = NULL)))
print(prop.table(table(ad_price_ad_level$massage_ad, ad_price_ad_level$is_massage_parlor_ad, exclude = NULL)))
#unknowns are a small %, spread across all service venues

# Validations -- price
print(prop.table(table(ad_price_ad_level$from_ist)))
#60% from IST, 40% not
print(prop.table(table(ad_price_ad_level$from_ist, ad_price_ad_level$hour_price_counts)))
#no from IST have hourly - probably by construction
#for non-IST, twice as many have an hourly rate than don't

#Validations - date
print(sum(is.na(ad_price_ad_level$date_str))/nrow(ad_price_ad_level))
#About 8% missing date

print(sapply(ad_price_ad_level, FUN = function(x) sum(is.na(x))/nrow(ad_price_ad_level)))

}

ad_price_ad_level <- ad_price_ad_level[!is.na(ad_price_ad_level$date_str) & !is.na(ad_price_ad_level$incall_outcall), ]

print(sapply(ad_price_ad_level, FUN = function(x) sum(is.na(x))/nrow(ad_price_ad_level)))

################################################################################
# Load & ready MSA datasets
################################################################################


# MSA-level ####
msa_level_master <- readRDS(file = paste0(finalDatasets, "msa_level_master.RDS"))
str(msa_level_master)
#law enforcement 2013 and STD 2012
msa_level_master$MetropolitanMicropolitanStatisticalArea <- NULL


# MERGE 
ad_price_ad_level <- data.table(ad_price_ad_level)
msa_level_master <- data.table(msa_level_master)
ad_level_with_msa_char <- merge(ad_price_ad_level, msa_level_master, by = 'CBSACode')



nrow(ad_level_with_msa_char) / nrow(ad_price_ad_level)
#about 77% of original ads (used to be 67% with old MSAs)
# CDR new data: 61% ie. 38% missing (5% more than are missing CBSA codes in ad data. are they all micropolitan?)
if (!shortcut) {
  #Validate
  print(isTableAtThisLevel(ad_level_with_msa_char, 'ad_id'))

#how many CBSAs in ad data don't match? and is it because they're micropolitan SAs
ad_cbsa_not_in_msa_master <- unique(ad_price_ad_level$CBSACode)[!(unique(ad_price_ad_level$CBSACode) %in% unique(msa_level_master$CBSACode))]
print(length(ad_cbsa_not_in_msa_master))
cbsa_level_crosswalk <- readRDS(file =paste0(workingDatasets, "CBSA List.RDS"))
print(table(cbsa_level_crosswalk$Metropolitan.Micropolitan.Statistical.Area[cbsa_level_crosswalk$CBSA.Code %in% ad_cbsa_not_in_msa_master]))
#they're all micropolitan statistical areas

print(sapply(ad_level_with_msa_char, FUN = function(x) sum(is.na(x))/nrow(ad_level_with_msa_char)))

}

# MSA - year ####
msa_year_level_master <- readRDS(file = paste0(finalDatasets, "msa_year_level_master.RDS"))
str(msa_year_level_master)
msa_year_level_master$MetropolitanMicropolitanStatisticalArea <- NULL
msa_year_level_master$CBSATitle <- NULL


# MERGE MSA-& YEAR-LEVEL WITH AD-LEVEL #######
msa_year_level_master <- data.table(msa_year_level_master)
ad_level_with_msa_year_char <- merge(ad_level_with_msa_char, msa_year_level_master, by = c('CBSACode', 'year'), all.x = TRUE)
browser()

if (!shortcut) {
print(str(ad_level_with_msa_year_char))

#Validate
print(isTableAtThisLevel(ad_level_with_msa_year_char, 'ad_id'))
print(nrow(ad_level_with_msa_year_char) / nrow(ad_price_ad_level))
#about 77% of original ads (used to be 67% with old MSAs)
#now 61% with CDR data
}

# CREATE ADDITIONAL ANALYSES VARS ####

# ad count & uniqueproviders
ad_level_with_msa_char[,":=" (adcount_MSAmonthlvl_GIANTOAK = .N,
                              uniqueproviders_MSAmonthlvl_GIANTOAK = length(unique(cluster_id))),
                       by = list(CBSACode, year, month)]

# ad count && unique providers for msa_year level

ad_level_with_msa_year_char[,":=" (adcount_MSAmonthlvl_GIANTOAK = .N,
                                   uniqueproviders_MSAmonthlvl_GIANTOAK = length(unique(cluster_id))),
                            by = list(CBSACode, year, month)]

#Calculate # of unique providers per YEAR for law enforcement proxy (yearly prostitution NIBRS arrests/# of providers)
cbsa_year_level_providers <- ad_level_with_msa_char[,list(uniqueproviders_MSAyearlvl = length(unique(cluster_id))),
                                                    by = list(CBSACode, year)]
if (!shortcut) {
print(str(cbsa_year_level_providers))
print(isTableAtThisLevel(cbsa_year_level_providers, c('CBSACode', 'year')))
print(tapply(cbsa_year_level_providers$uniqueproviders_MSAyearlvl, cbsa_year_level_providers$year, summary))
}
#2014-2015 stable -- but 2015 isn't done yet
cbsa_level_providers <- cbsa_year_level_providers[year == 2014,list(uniqueproviders_2014 = max(uniqueproviders_MSAyearlvl)),
                                                  by = CBSACode]
summary(cbsa_level_providers$uniqueproviders_2014)
#140 provider median, 400 average
ad_level_with_msa_char_prov <- merge(ad_level_with_msa_char, cbsa_level_providers, by = 'CBSACode', all.x = TRUE)
#Validate
nrow(ad_level_with_msa_char_prov) == nrow(ad_level_with_msa_char)
nrow(ad_level_with_msa_char_prov[is.na(uniqueproviders_2014), ])/nrow(ad_level_with_msa_char_prov)
#almost none missing
summary(ad_level_with_msa_char_prov$uniqueproviders_2014)
#higher median as ads in cities with more providers overrepresented obviously

ad_level_with_msa_char <- ad_level_with_msa_char_prov
rm(ad_level_with_msa_char_prov)

#Calculate LE proxy 
ad_level_with_msa_char$prop_prov_arrested <- ad_level_with_msa_char$prostitution_count_msa / ad_level_with_msa_char$uniqueproviders_2014
summary(ad_level_with_msa_char$prop_prov_arrested)

#replace prostitution rate with prop_prov_arrested?
if (replace_prostitution_rate){
  
  ad_level_with_msa_char$prostitution_rate_msa <- ad_level_with_msa_char$prop_prov_arrested
  
}
#replace prostitution rate

# Validations -- venue & massage
if (!shortcut) {
print(prop.table(table(ad_level_with_msa_char$massage_ad, ad_level_with_msa_char$incall_outcall, exclude = NULL)))
print(prop.table(table(ad_level_with_msa_char$massage_ad, ad_level_with_msa_char$is_massage_parlor_ad, exclude = NULL)))
#roughly same as ad_price_ad_level

# Validations -- venue & massage
print(prop.table(table(ad_level_with_msa_char$from_ist)))


#Missings count
print(sapply(ad_level_with_msa_char, FUN = function(x) sum(is.na(x))/nrow(ad_level_with_msa_char)))
#<1% missing MSA-level UCR vars
#2% missing Law enforcement
#7% missing any COL, 12% missing 2014 COL
#9% missing GO vars -- now none
#56% missing prostitution

print(sapply(ad_level_with_msa_year_char, FUN = function(x) sum(is.na(x))/nrow(ad_level_with_msa_year_char)))
#~30% missing UCR yearly vars, 25% missing avg. commute ~~~ from 2015
#90% missing NIBRS -- pointless
#33% missing COL
print(prop.table(table(ad_level_with_msa_year_char$year)))
}

################################################################################
# Analysis -- price regressions - GO REPLICATIONS
################################################################################
#KEEP SEX ADS
#KEEP SIZE < 200 CLUSTERS
#MSA QUARTER FE

#Verify your clustering variable picked earlier
clustering_variable



# REGRESSION - SIMPLE WITH JUST SERVICE VENUE (column 1, pg18 of working paper) ####
specify_vars_with_missings <- c('avg_commute_GIANTOAK')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings - necessary for clustering properly


#Run regression
reg <- lm(price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out,
          data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)
#coefficients resemble theirs but are a little off

go_replication_reg_1 <- tgg_reg(reg_dataset,price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out,"CBSACode")

#Misc stats
summary(reg)$adj.r.squared
summary(reg)$fstatistic
#Validate that regressions were run on all except those with missing vars you specified
length(reg$residuals) == nrow(reg_dataset[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ])
paste0(format(length(reg$residuals), big.mark = ","), " obs used in this regression")

#CDR: 1.6 MM 
#almost all 2.3M ads used if outer join is done
#about 1.5M ads used -- 800,000 (~31%, as per earlier) lack MSA info
#1.4 M after spam, massage parlor, price and sexad filters --- still > than obs (used to be 1.28)

#saveRDS(reg_dataset,file = paste0(finalDatasets,"go_replication_dataset_1.rds"))




# REGRESSION - SERVICE VENUE + CONTROLS + COMMUTE (column 2) ####
specify_vars_with_missings <- c('avg_commute_GIANTOAK', 'month', 'outcall_only','both_incall_outcall' , 'unclear_inc_out', 
                                'uniqueproviders_MSAmonthlvl_GIANTOAK' , 'adcount_MSAmonthlvl_GIANTOAK',
                                'website' , 'population_GIANTOAK' , 'unemployment_GIANTOAK' , 'frac_white_GIANTOAK')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings - necessary for clustering properly

#saveRDS(reg_dataset,file = paste0(finalDatasets,"go_replication_dataset_2.rds"))


go_replication_reg_2 <- tgg_reg(reg_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out 
                                + uniqueproviders_MSAmonthlvl_GIANTOAK + adcount_MSAmonthlvl_GIANTOAK
                                + avg_commute_GIANTOAK + website + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month), "CBSACode")

go_replication_reg_3 <- tgg_reg(reg_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out 
                                + uniqueproviders_MSAmonthlvl_GIANTOAK + adcount_MSAmonthlvl_GIANTOAK
                                + avg_commute_GIANTOAK + website + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month) +
                                  outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK, "CBSACode")


sapply(c("go_replication_reg_1","go_replication_reg_2","go_replication_reg_3"),FUN=function(reg_name){
  reg_obj = eval(parse(text=reg_name))
  # summary.lm(reg_obj)
  filename = paste(regression_objects, reg_name,".rds", sep="")
  saveRDS(reg_obj,file=filename)
})





#Run regression
reg <- lm(price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out 
          + uniqueproviders_MSAmonthlvl_GIANTOAK + adcount_MSAmonthlvl_GIANTOAK
          + avg_commute_GIANTOAK + website + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month),
          data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)
#coefficients more closely resemble theirs, not exactly

#Misc stats
summary(reg)$adj.r.squared
summary(reg)$fstatistic
#Validate that regressions were run on all except those with missing vars you specified
length(reg$residuals) == nrow(reg_dataset[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ])
paste0(format(length(reg$residuals), big.mark = ","), " obs used in this regression")






# REGRESSION - SERVICE VENUE INTERACTED WITH COMMUTE TIME (column 3)####
specify_vars_with_missings <- c('avg_commute_GIANTOAK','month')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings - necessary for clustering properly


#Run regression
reg <- lm(price_per_hour ~ outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK
          + uniqueproviders_MSAmonthlvl_GIANTOAK + adcount_MSAmonthlvl_GIANTOAK 
          + website + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)

#Misc stats
summary(reg)$adj.r.squared
summary(reg)$fstatistic
#Validate that regressions were run on all except those with missing vars you specified
length(reg$residuals) == nrow(reg_dataset[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ])
paste0(format(length(reg$residuals), big.mark = ","), " obs used in this regression")









# REGRESSION - MSA FIXED EFFECTS - SERVICE VENUE INTERACTED WITH COMMUTE TIME (column 4) ####
specify_vars_with_missings <- c('avg_commute_GIANTOAK','month')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings - necessary for clustering properly


#Run regression
reg <- lm(price_per_hour ~  outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK 
          + as.character(CBSACode) 
          + uniqueproviders_MSAmonthlvl_GIANTOAK + adcount_MSAmonthlvl_GIANTOAK
          + website + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)

length(names(reg$coefficients)[grepl('CBSACode', names(reg$coefficients))])
#66 MSA dummies -- now 168
# also == number of MSA clusters in their paper (minus 1 of course)

#Verify commute time doesn't vary within MSA
commutes_per_msa <- data.table(reg_dataset)[,.(num_commutes = length(unique(avg_commute_GIANTOAK)),
                                               min_commute = min(avg_commute_GIANTOAK),
                                               max_commute = max(avg_commute_GIANTOAK)),
                                            by = CBSACode][, diff_commute := max_commute - min_commute]
table(commutes_per_msa$num_commutes) 
table(commutes_per_msa$diff_commute) 
#each MSA has 1 commute

#Misc stats
summary(reg)$adj.r.squared
summary(reg)$fstatistic
#Validate that regressions were run on all except those with missing vars you specified
length(reg$residuals) == nrow(reg_dataset[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ])
paste0(format(length(reg$residuals), big.mark = ","), " obs used in this regression")



#GO STATA code for reference: 
#qui areg p1 out1 both unclear  avg_commute out1Xc bothXc unclearXc adcount uniqueproviders population unemployment frac_white web_2-web_12 i.m1 if massage!=1 & p1<1000 & size<201 , a(census_msa_code) cluster(census_msa_code)
#gen sample=e(sample)

#areg p1 out1 both unclear web_2-web_12 if massage!=1 & p1<1000 & size<201 & sample==1, a(m1) cluster(census_msa_code)
#est store reg1
#areg p1 out1 both unclear  avg_commute adcount uniqueproviders population unemployment frac_white web_2-web_12 if massage!=1 & p1<1000 & size<201 & sample==1, a(m1) cluster(census_msa_code)
#est store reg2
#areg p1 out1 both unclear  avg_commute out1Xc bothXc unclearXc adcount uniqueproviders population unemployment frac_white web_2-web_12 if massage!=1 & p1<1000 & size<201 & sample==1, a(m1) cluster(census_msa_code)
#est store reg3



################################################################################
# Analysis -- price summary statistics - TGG
################################################################################

#Note - this includes spam & no sex ads

if (!shortcut) {
  
  
ad_level_with_msa_char <- data.table(ad_level_with_msa_char)

msa_year_level_ads <- ad_level_with_msa_char[,.(ave_price = mean(price_per_hour), 
                                                median_price = median(price_per_hour),
                                                num_ads = .N,
                                                outcall_ads = sum(outcall_only),
                                                incall_ads = sum(incall_only)),
                                             by = list(CBSACode, year)][,":="(prop_outcall_ads = outcall_ads/num_ads,
                                                                              prop_incall_ads = incall_ads/num_ads)]


#Num ads
qplot(msa_year_level_ads$num_ads[msa_year_level_ads$year == 2014], geom = 'histogram')
for (i in unique(msa_year_level_ads$year)[order(unique(msa_year_level_ads$year))]) {
  print(paste("year", i))
  print(summary(msa_year_level_ads$num_ads[msa_year_level_ads$year == i]))
}


qplot(msa_year_level_ads$ave_price[msa_year_level_ads$year == 2014], geom = 'histogram')
summary(msa_year_level_ads$ave_price[msa_year_level_ads$year == 2014])
qplot(msa_year_level_ads$median_price[msa_year_level_ads$year == 2014], geom = 'histogram')
summary(msa_year_level_ads$median_price[msa_year_level_ads$year == 2014])



# Tale of two cities ####

str(msa_level_master)

#Pairwise comparisons
msa_list <- unique(msa_level_master$CBSACode)

#initialise row number and dataset
msa_comparison <- NULL
row_number <- 1


for (i in 1:(length(msa_list)-1)) {
  msa_1 <- msa_list[i]
  for (j in (i+1):length(msa_list)) {
    msa_2 <- msa_list[j]
    msa_comparison$msa_1[row_number] <- msa_1
    msa_comparison$msa_2[row_number] <- msa_2
    
    msa_comparison$msa_1_title[row_number] <- msa_level_master$CBSATitle[msa_level_master$CBSACode == msa_1]
    msa_comparison$msa_2_title[row_number] <- msa_level_master$CBSATitle[msa_level_master$CBSACode == msa_2]
    
    msa_comparison$commute_1[row_number] <- msa_level_master$avg_commute_GIANTOAK[msa_level_master$CBSACode == msa_1]
    msa_comparison$commute_2[row_number] <- msa_level_master$avg_commute_GIANTOAK[msa_level_master$CBSACode == msa_2]
    msa_comparison$commute_disparity[row_number] <- (max(msa_comparison$commute_1[row_number],
                                                         msa_comparison$commute_2[row_number]) - min(msa_comparison$commute_1[row_number],
                                                                                                     msa_comparison$commute_2[row_number])) / min(msa_comparison$commute_1[row_number],
                                                                                                                                                  msa_comparison$commute_2[row_number])
    
    msa_comparison$violence_1[row_number] <- msa_level_master$violent_rate_msa[msa_level_master$CBSACode == msa_1]
    msa_comparison$violence_2[row_number] <- msa_level_master$violent_rate_msa[msa_level_master$CBSACode == msa_2]
    msa_comparison$violence_disparity[row_number] <- (max(msa_comparison$violence_1[row_number],
                                                          msa_comparison$violence_2[row_number]) - min(msa_comparison$violence_1[row_number],
                                                                                                       msa_comparison$violence_2[row_number])) / min(msa_comparison$violence_1[row_number],
                                                                                                                                                     msa_comparison$violence_2[row_number])
    msa_comparison$more_violent_city[msa_comparison$violence_1 > msa_comparison$violence_2 & !is.na(msa_comparison$violence_1) & !is.na(msa_comparison$violence_2)] <- 'msa_1' 
    msa_comparison$more_violent_city[msa_comparison$violence_2 > msa_comparison$violence_1 & !is.na(msa_comparison$violence_1) & !is.na(msa_comparison$violence_2)] <- 'msa_2'
    
    msa_comparison$coli_1[row_number] <- msa_level_master$COL_Composite_2014[msa_level_master$CBSACode == msa_1]
    msa_comparison$coli_2[row_number] <- msa_level_master$COL_Composite_2014[msa_level_master$CBSACode == msa_2]
    msa_comparison$coli_disparity[row_number] <- (max(msa_comparison$coli_1[row_number],
                                                      msa_comparison$coli_2[row_number]) - min(msa_comparison$coli_1[row_number],
                                                                                               msa_comparison$coli_2[row_number])) / min(msa_comparison$coli_1[row_number],
                                                                                                                                         msa_comparison$coli_2[row_number])
    
    msa_comparison$population_1[row_number] <- msa_level_master$population_GIANTOAK[msa_level_master$CBSACode == msa_1]
    msa_comparison$population_2[row_number] <- msa_level_master$population_GIANTOAK[msa_level_master$CBSACode == msa_2]
    msa_comparison$population_disparity[row_number] <- (max(msa_comparison$population_1[row_number],
                                                            msa_comparison$population_2[row_number]) - min(msa_comparison$population_1[row_number],
                                                                                                           msa_comparison$population_2[row_number])) / min(msa_comparison$population_1[row_number],
                                                                                                                                                           msa_comparison$population_2[row_number])
    
    
    row_number <- row_number + 1
    
  }
  
  
}


msa_comparison <- as.data.frame(msa_comparison, stringsAsFactors = FALSE)
#msa_comparison[, sapply(msa_comparison, is.factor)] <- lapply(msa_comparison[, sapply(msa_comparison, is.factor)], as.character)
str(msa_comparison)

num_obs_check <- 0
for (i in 1:(length(msa_list)-1) ) {
  num_obs_check <- num_obs_check + length(msa_list) - i
  
}
num_obs_check == nrow(msa_comparison)
msa_comparison <- msa_comparison[complete.cases(msa_comparison), ]


#display outcall and incall prices for comparable cities
violence_difference <- msa_comparison[msa_comparison$commute_disparity < 0.05 & msa_comparison$coli_disparity < 0.05 & msa_comparison$population_disparity < 0.05
                                      & msa_comparison$violence_disparity > 0.5, ]


for (i in 1:nrow(violence_difference)) {
  
  two_cities <- c(violence_difference$msa_1[i], violence_difference$msa_2[i])
  
  prices_two_cities <- ad_level_with_msa_char[ad_level_with_msa_char$CBSACode %in% two_cities, ]
  table(prices_two_cities$CBSACode)
  
  saveRDS(prices_two_cities, paste0(finalDatasets, 'Tale of two cities/', unique(prices_two_cities$CBSATitle)[1],
                                    ' and ', unique(prices_two_cities$CBSATitle)[2], ".RDS"))
  
  #ggplot(prices_two_cities[prices_two_cities$price_per_hour < 400 & prices_two_cities$incall_only == 1, ], 
  #       aes(x = price_per_hour, ..density.., colour = CBSATitle)) + geom_freqpoly(binwidth = 50) + ggtitle('Incall only prices')
  ggplot(prices_two_cities[prices_two_cities$price_per_hour < 400 & prices_two_cities$incall_only == 1, ], 
         aes(x = price_per_hour, ..density.., colour = CBSATitle)) + geom_density(alpha = 0.5) + theme_backg + theme_axis + xlab("Aggregated hourly rate") + ylab("Density") + ggtitle('Incall only prices') #density plots
  
  ggsave(filename = paste0(graphics, '/Tale of two cities/', unique(prices_two_cities$CBSATitle)[1], ' and ', unique(prices_two_cities$CBSATitle)[2],   '- INCALL.png'))
  
  
  #ggplot(prices_two_cities[prices_two_cities$price_per_hour < 400 & prices_two_cities$outcall_only == 1, ], 
  #       aes(x = price_per_hour, ..density.. , colour = CBSATitle)) + geom_freqpoly(binwidth = 50) + ggtitle('Outcall only prices')
  ggplot(prices_two_cities[prices_two_cities$price_per_hour < 400 & prices_two_cities$outcall_only == 1, ], 
         aes(x = price_per_hour, ..density.. , colour = CBSATitle)) + geom_density(alpha = 0.5) + theme_backg + theme_axis + xlab("Aggregated hourly rate") + ylab("Density") + ggtitle('Outcall only prices') #density plots
  
  ggsave(filename = paste0(graphics, '/Tale of two cities/', unique(prices_two_cities$CBSATitle)[1], ' and ', unique(prices_two_cities$CBSATitle)[2],   '- OUTCALL.png')) 
  
  
}  

#should we be using max of our own commute times as MSA-level vars?
length(unique(msa_year_level_master$CBSACode[!is.na(msa_year_level_master$avg_com)]))
nrow(msa_level_master[!is.na(msa_level_master$avg_commute_GIANTOAK), ])
#388 MSAs represented vs. 206. except remember that the 5-year ACS is supposed to be more accurate



}

################################################################################
# Analysis -- price regressions - TGG
################################################################################

##See xdata_analysis_functions for no_missings_subset and tgg_reg function ##
## No missings subset applies the GO filters and removes rows with missing variablese in specify_vars_with_missings ##
## Regression runs regression on this new dataset and clusters at the clustering variable##
## Inputs: dataset, formula, then clustering variable

clustering_variable <- 'CBSACode'



#Example 1: This should work exactly as the original code below does
specify_vars_with_missings <- c('avg_commute_GIANTOAK', 'COL_Transport','avg_com')
regression_dataset <- no_missings_subset(ad_level_with_msa_year_char, specify_vars_with_missings)
reg1 <- tgg_reg(regression_dataset, 
                price_per_hour ~ outcall_only  + both_incall_outcall  + unclear_inc_out  + website  + as.character(month)+as.character(year), 
                "CBSACode")
reg1
##
# REGRESSION - SIMPLE SERVICE VENUE####

reg_dataset <- regression_dataset
#reg_dataset <- ad_level_with_msa_year_char

#Subset dataset
#reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000, ] #Giant Oak filter

#specify_vars_with_missings <- c('avg_commute_GIANTOAK', 'COL_Transport','avg_com')
#reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings - necessary for clustering properly


#COL TRANSPORT: Run regression with COL data instead of avg commute time #######
reg <- lm(price_per_hour ~  outcall_only * COL_Transport + both_incall_outcall * COL_Transport + unclear_inc_out * COL_Transport + website  + as.character(month)+as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)
#incall higher with COL transport, not outcall - possibly because 

col_transport_reg <- tgg_reg(reg_dataset,price_per_hour ~  COL_Transport*outcall_only   + COL_Transport*both_incall_outcall + COL_Transport*unclear_inc_out + website  + as.character(month)+as.character(year), clustering_variable)
col_transport_reg_2 <- tgg_reg(reg_dataset, price_per_hour ~  COL_Transport*outcall_only + COL_Transport*both_incall_outcall + COL_Transport*unclear_inc_out + COL_Composite +   website + as.character(month) + as.character(year), clustering_variable)
col_transport_reg_3 <- tgg_reg(reg_dataset, price_per_hour ~ outcall_only * COL_Composite + both_incall_outcall * COL_Composite + unclear_inc_out * COL_Composite + outcall_only * COL_Transport + both_incall_outcall * COL_Transport + unclear_inc_out * COL_Transport + website + as.character(month) + as.character(year), clustering_variable)
col_composite_reg <- tgg_reg(reg_dataset, price_per_hour ~ outcall_only * COL_Composite + both_incall_outcall * COL_Composite + unclear_inc_out * COL_Composite +website +as.character(month) + as.character(year), clustering_variable)
#col_composite_reg_2 <- tgg_reg(reg_dataset, price_per_hour ~ outcall_only * COL_Composite + both_incall_outcall * COL_Composite + unclear_inc_out * COL_Composite +website #+as.character(month) + as.character(year) + frac_white_GIANTOAK + population_GIANTOAK + avg_commute_GIANTOAK + unemployment_GIANTOAK , clustering_variable)

sapply(c("col_transport_reg", "col_transport_reg_2", "col_transport_reg_3","col_composite_reg"),FUN=function(reg_name){
  reg_obj = eval(parse(text=reg_name))
  # summary.lm(reg_obj)
  filename = paste(regression_objects, "/",reg_name,".rds", sep="")
  saveRDS(reg_obj,file=filename)
})

#saveRDS(col_transport_reg,"~/Home/col_transport_reg.rds")
#saveRDS(col_transport_reg_2,"~/Home/col_transport_reg_2.rds")
#saveRDS(col_transport_reg_3,"~/Home/col_transport_reg_3.rds")
#saveRDS(col_composite_reg,"~/Home/col_composite_reg.rds")


#Can we use transport COL vari able but include composite COL as control
cor(reg_dataset$COL_Composite, reg_dataset$COL_Transport)
#0.7 -- a little too correlated to be useful
reg <- lm(price_per_hour ~  outcall_only * COL_Transport + both_incall_outcall * COL_Transport + unclear_inc_out * COL_Transport + website  + as.character(month) +as.character(year) + COL_Composite, data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)
#COL transport no longer predicts incall premium

#as a placebo, try it with utilities cost instead of transportation
cor(reg_dataset$COL_Utilities, reg_dataset$COL_Transport)
#0.43
cor(reg_dataset$COL_Utilities, reg_dataset$COL_Composite)
#0.61
reg <- lm(price_per_hour ~  outcall_only * COL_Utilities + both_incall_outcall * COL_Utilities + unclear_inc_out * COL_Utilities + website  + as.character(month), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)


#Use TGG average commute vari able but include composite COL as control
reg <- lm(price_per_hour ~  outcall_only * avg_com + both_incall_outcall * avg_com + unclear_inc_out * avg_com + website  + as.character(month) +as.character(year) +outcall_only * COL_Composite, data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)



## PRELIMINARY REGRESSIONS
if (!shortcut) {

#### crime analysis
specify_vars_with_missings <- c('violent_rate_msa')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly



#try outcall and crime
reg <- lm(price_per_hour ~ outcall_only * violent_rate_msa +both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa + website + as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)
# violence now explains some premium

#try crime with GO controls

specify_vars_with_missings <- c('violent_rate_msa','population_GIANTOAK','unemployment_GIANTOAK','frac_white_GIANTOAK')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly


reg <- lm(price_per_hour ~ outcall_only * violent_rate_msa +both_incall_outcall * violent_rate_msa + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK+ unclear_inc_out * violent_rate_msa + website + as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)

#try crime with CBSA fixed effects
#erroneous since MSA_level
#reg <- lm(price_per_hour ~ outcall_only * violent_rate_msa +both_incall_outcall * violent_rate_msa + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + unclear_inc_out * violent_rate_msa + website + as.character(month) +as.character(year)+as.character(CBSACode), data = reg_dataset)
#Cluster standard errors by MSA
#cl_coeftest(reg_dataset, reg, clustering_variable)





### let's try property crime as placebo
specify_vars_with_missings <- c('property_rate_msa')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * property_rate_msa +both_incall_outcall * property_rate_msa + unclear_inc_out * property_rate_msa + website + as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)

#try property including MSA FEs
#reg <- lm(price_per_hour ~ outcall_only * property_rate_msa +both_incall_outcall * property_rate_msa + unclear_inc_out * property_rate_msa + website + as.character#(month) +as.character(year)+as.character(CBSACode), data = reg_dataset)
#Cluster standard errors by MSA
#cl_coeftest(reg_dataset, reg, clustering_variable)


# let's include GO controls
specify_vars_with_missings <- c('property_rate_msa','population_GIANTOAK','unemployment_GIANTOAK','frac_white_GIANTOAK')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * property_rate_msa +both_incall_outcall * property_rate_msa + unclear_inc_out * property_rate_msa + website +population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK+ as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)


### check effect of rape
specify_vars_with_missings <- c('rape_rate_msa')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * rape_rate_msa +both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa + website + as.character(month) +
            as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)
#no effect with just rape
#let's try again with CBSA fes?
#reg <- lm(price_per_hour ~ outcall_only * rape_rate_msa +both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa + website + as.character(month) +as.character(year)+as.character(CBSACode), data = reg_dataset)
#Cluster standard errors by MSA
#cl_coeftest(reg_dataset, reg, clustering_variable)

# let's include GO controls
specify_vars_with_missings <- c('rape_rate_msa','population_GIANTOAK','unemployment_GIANTOAK','frac_white_GIANTOAK')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * rape_rate_msa +both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa + website + as.character(month) + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK +as.character(year) , data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)


##how about STDs?
#first chlamydia
specify_vars_with_missings <- c('chl_rateper100k_2012')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * chl_rateper100k_2012 +both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 + website + as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)

#add msa fes
#erroneous
#reg <- lm(price_per_hour ~ outcall_only * chl_rateper100k_2012 +both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 + website + as.character(month) +as.character(year) +as.character(CBSACode), data = reg_dataset)
#Cluster standard errors by MSA
#cl_coeftest(reg_dataset, reg, clustering_variable)

# let's include GO controls
specify_vars_with_missings <- c('chl_rateper100k_2012','population_GIANTOAK','unemployment_GIANTOAK','frac_white_GIANTOAK')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * chl_rateper100k_2012 +both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 + website + as.character(month) + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK +as.character(year) , data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)


#now gonorrhea 
specify_vars_with_missings <- c('gon_rateper100k_2012')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * gon_rateper100k_2012 +both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 + website + as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)

#add msa fes
#reg <- lm(price_per_hour ~ outcall_only * gon_rateper100k_2012 +both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 + website + as.character(month) +as.character(year) +as.character(CBSACode), data = reg_dataset)
#Cluster standard errors by MSA
#cl_coeftest(reg_dataset, reg, clustering_variable)

#### law enforcement
specify_vars_with_missings <- c('ftsworn_rateper100k_2013')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly



#try outcall and LE
reg <- lm(price_per_hour ~ outcall_only * ftsworn_rateper100k_2013+both_incall_outcall * ftsworn_rateper100k_2013+ unclear_inc_out * ftsworn_rateper100k_2013 + website + as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)



#add msa fes
#reg <- lm(price_per_hour ~ outcall_only * ftsworn_rateper100k_2013.x +both_incall_outcall * ftsworn_rateper100k_2013.x + unclear_inc_out * ftsworn_rateper100k_2013.x + website + as.character(month) +as.character(year)+as.character(CBSACode), data = reg_dataset)
#Cluster standard errors by MSA
#cl_coeftest(reg_dataset, reg, clustering_variable)


#add GO controls 
specify_vars_with_missings <- c('ftsworn_rateper100k_2013', 'population_GIANTOAK', 'unemployment_GIANTOAK','frac_white_GIANTOAK')
reg_dataset <- ad_level_with_msa_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * ftsworn_rateper100k_2013 +both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 + website +population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)

#add COL control 
specify_vars_with_missings <- c('ftsworn_rateper100k_2013','COL_Composite')
reg_dataset <- ad_level_with_msa_year_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * ftsworn_rateper100k_2013 +both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 + website +COL_Composite + as.character(month) +as.character(year), data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)



### big mamma model to try and decompose all of the effects
specify_vars_with_missings <- c('avg_com','chl_rateper100k_2012','ftsworn_rateper100k_2013', 'violent_percap', 'population_GIANTOAK','COL_Composite', 'unemployment_GIANTOAK','frac_white_GIANTOAK')
reg_dataset <- ad_level_with_msa_year_char

#Subset dataset
reg_dataset <- reg_dataset[reg_dataset$spam == FALSE & reg_dataset$sex_ad == 1 & reg_dataset$is_massage_parlor_ad == 0 & reg_dataset$price_per_hour < 1000 & reg_dataset$year>2010, ] #Giant Oak filter

reg_dataset <- as.data.frame(reg_dataset)[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ] #automatically drop missings #necessary for clustering properly

reg <- lm(price_per_hour ~ outcall_only * chl_rateper100k_2012 + both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 + outcall_only * avg_com +both_incall_outcall * avg_com + unclear_inc_out * avg_com + outcall_only * ftsworn_rateper100k_2013 +both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +  outcall_only * violent_percap +both_incall_outcall * violent_percap + unclear_inc_out * violent_percap + website +COL_Composite+population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month) , data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)
#violence and commute still hold

#Add MSA fixed effects, drop LE and STD (keep violence per cap, COL, commute)

reg <- lm(price_per_hour ~ outcall_only * avg_com +both_incall_outcall * avg_com + unclear_inc_out * avg_com + outcall_only * violent_percap +both_incall_outcall * violent_percap + unclear_inc_out * violent_percap + website +COL_Composite+population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month) +as.character(CBSACode) , data = reg_dataset)
#Cluster standard errors by MSA
cl_coeftest(reg_dataset, reg, clustering_variable)

#Validate that regressions were run on all except those with missing vars you specified
length(reg$residuals) == nrow(reg_dataset[complete.cases(as.data.frame(reg_dataset)[, specify_vars_with_missings]), ])
paste0(format(length(reg$residuals), big.mark = ","), " obs used in this regression")
#900K used

}
################################
###### Regressions for Doc######
################################

regression_dataset <- ad_level_with_msa_char

#Quick verification that crime rates are per 100k
summary(regression_dataset$violent_rate_msa)



#We have decided to keep the same dataset throughout, so all 

specify_vars_with_missings = c("CBSACode","year","COL_Composite_mr","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month", 
                               "property_rate_msa","violent_rate_msa","rape_rate_msa","website", "unemployment_GIANTOAK",
                               "frac_white_GIANTOAK","adcount_MSAmonthlvl_GIANTOAK","uniqueproviders_MSAmonthlvl_GIANTOAK", "ftsworn_rateper100k_2013")




#manually specify filters for test run with preferred specifications including spam and excluding nonhourly rates
if (!include_spam_exclude_nonhourlyrates) {
  
  regression_dataset <- no_missings_subset(regression_dataset, specify_vars_with_missings)
  
} else {
  regression_dataset <- no_missings_subset(regression_dataset, specify_vars_with_missings, usual_filters = FALSE)
  regression_dataset <- regression_dataset[regression_dataset$has_hour_price == TRUE & regression_dataset$sex_ad == 1 & regression_dataset$is_massage_parlor_ad == 0 & regression_dataset$price_per_hour < 1000 & regression_dataset$year >2010,]
}

nrow(regression_dataset)
#1.5MM if usual filters
#pre-fixes:1.3MM if usual filters, ~1.5 million if no
table(regression_dataset$spam)

regression_dataset_nibrs <- no_missings_subset(regression_dataset, "prostitution_rate_msa")

nrow(regression_dataset_nibrs)
#700K

saveRDS(regression_dataset, paste0(regression_objects,"/regression_dataset_msa.RDS"))
saveRDS(regression_dataset_nibrs,paste0(regression_objects,"/regression_dataset_nibrs_msa.RDS"))

## Preferred specification ##

pref_reg1 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + chl_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
                       outcall_only * chl_rateper100k_2012 + both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 +
                       outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                       outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       median_income + poverty_rate + website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#cl_coeftest(regression_dataset, pref_reg1$reg, 'CBSACode')

pref_reg2 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
                       outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                       outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                       outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       median_income + poverty_rate + website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

pref_reg3 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + avg_commute_GIANTOAK +
                       outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                       outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       median_income + poverty_rate + website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

## Section 1: Abuse Risk ##

#Violent rate, no controls
abuse_reg1 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + violent_rate_msa +
                        
                        outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                        website + as.character(month) + as.character(year), "CBSACode")
#Outcall premium halved, other half correlated with violent crime. 
# incall DISCOUNT with violent crime

#Violent rate, with controls
abuse_reg2 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + violent_rate_msa +
                        outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                        website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                        adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#stargazer(abuse_reg1,abuse_reg2,title="Regression Results", align=TRUE, out = paste0(regressions,"regressions_violent.html",sep=""))

#Rape rate

abuse_reg3 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + rape_rate_msa +
                        outcall_only * rape_rate_msa + both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa +
                        website + as.character(month) + as.character(year), "CBSACode")

#Rape rate, with controls
abuse_reg4 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + rape_rate_msa +
                        outcall_only * rape_rate_msa + both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa +
                        website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                        adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")
#stargazer(abuse_reg3,abuse_reg4,title="Rape Regressions Results", align=TRUE, out = paste0(regressions,"regressions_rape.html",sep=""))


#Property Crime

abuse_reg5 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + property_rate_msa +
                        outcall_only * property_rate_msa + both_incall_outcall * property_rate_msa + unclear_inc_out * property_rate_msa +
                        website + as.character(month) + as.character(year), "CBSACode")

#property rate, with controls

abuse_reg6 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + property_rate_msa +
                        outcall_only * property_rate_msa + both_incall_outcall * property_rate_msa + unclear_inc_out * property_rate_msa +
                        website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                        adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#stargazer(abuse_reg5,abuse_reg6,title="Property Regressions Results", align=TRUE, out = paste0(regressions,"regressions_property.html",sep=""))


## Section 2: Arrest Risk ##

#FTSWORN, no controls
arrest_reg1 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + ftsworn_rateper100k_2013 +
                         outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                         website + as.character(month) + as.character(year), "CBSACode")
#FTSWORN, with controls
arrest_reg2 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + ftsworn_rateper100k_2013 +
                         outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                         website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                         adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#stargazer(arrest_reg1,arrest_reg2,title="FTSWORN Results", align=TRUE, out = paste0(regressions,"regressions_FTSWORN.html",sep=""))


#NIBRS, no controls
arrest_reg3 <- tgg_reg(regression_dataset_nibrs, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + prostitution_rate_msa +
                         outcall_only * prostitution_rate_msa + both_incall_outcall * prostitution_rate_msa + unclear_inc_out * prostitution_rate_msa +
                         website + as.character(month) + as.character(year), "CBSACode")
#NIBRS, with controls
arrest_reg4 <- tgg_reg(regression_dataset_nibrs, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + prostitution_rate_msa +
                         outcall_only * prostitution_rate_msa + both_incall_outcall * prostitution_rate_msa + unclear_inc_out * prostitution_rate_msa +
                         website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                         adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#stargazer(arrest_reg3,arrest_reg4,title="NIBRS Results", align=TRUE, out = paste0(regressions,"regressions_NIBRS.html",sep=""))


## Section 3 STD RISK ##

#Chlamydia, no controls
std_reg1 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + chl_rateper100k_2012 +
                      outcall_only * chl_rateper100k_2012 + both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 +
                      website + as.character(month) + as.character(year), "CBSACode")
#Chlamydia, with controls
std_reg2 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + chl_rateper100k_2012 +
                      outcall_only * chl_rateper100k_2012 + both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 +
                      website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                      adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#stargazer(std_reg1,std_reg2,title="Chlamydia Results", align=TRUE, out = paste0(regressions,"regressions_chlamydia.html",sep=""))

#Gonorrhea, no controls

std_reg3 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 +
                      outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                      website + as.character(month) + as.character(year), "CBSACode")
#Gonnorrhea, with controls
std_reg4 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 +
                      outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                      website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                      adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

cor(regression_dataset$gon_rateper100k_2012, regression_dataset$chl_rateper100k_2012)
#0.82

## Section 4 Adding in Commute Time ##

#Adding commute time to violent crime regression
com_reg1 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + violent_rate_msa + avg_commute_GIANTOAK +
                      outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                      outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                      website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                      adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Adding commute time to gonorrhea regression
com_reg2 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + avg_commute_GIANTOAK +
                      outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                      outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                      website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                      adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Adding commute time to gonorrhea regression
com_reg3 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
                      outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                      outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                      website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                      adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")


## Section 5 Combining Risks ## 

#Abuse + Arrest + Disease, using violent crime, number of law enforcement, chlamydia
risk_reg1 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + chl_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
                       outcall_only * chl_rateper100k_2012 + both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 +
                       outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                       outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Abuse + Arrest + Disease, using rape, number of law enforcement, chlamydia
risk_reg2 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + chl_rateper100k_2012 + rape_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
                       outcall_only * chl_rateper100k_2012 + both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 +
                       outcall_only * rape_rate_msa + both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa +
                       outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Abuse + Arrest + Disease, using violent crime, prostitution, chlamydia
risk_reg3 <- tgg_reg(regression_dataset_nibrs, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + chl_rateper100k_2012 + violent_rate_msa + prostitution_rate_msa + avg_commute_GIANTOAK +
                       outcall_only * chl_rateper100k_2012 + both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 +
                       outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                       outcall_only * prostitution_rate_msa + both_incall_outcall * prostitution_rate_msa + unclear_inc_out * prostitution_rate_msa +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Abuse + Arrest + Disease, using rape, prostitution, chlamydia
risk_reg4 <- tgg_reg(regression_dataset_nibrs, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + chl_rateper100k_2012 + rape_rate_msa + prostitution_rate_msa + avg_commute_GIANTOAK +
                       outcall_only * chl_rateper100k_2012 + both_incall_outcall * chl_rateper100k_2012 + unclear_inc_out * chl_rateper100k_2012 +
                       outcall_only * rape_rate_msa + both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa +
                       outcall_only * prostitution_rate_msa + both_incall_outcall * prostitution_rate_msa + unclear_inc_out * prostitution_rate_msa +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Abuse + Arrest + Disease, using violent crime, number of law enforcement, gonorrhea
risk_reg5 <- tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
                       outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                       outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                       outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Abuse + Arrest + Disease, using rape, number of law enforcement, gonorrhea
risk_reg6 <-tgg_reg(regression_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + rape_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
                      outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                      outcall_only * rape_rate_msa + both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa +
                      outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
                      outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                      website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                      adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Abuse + Arrest + Disease, using violent crime, prostitution, gonorrhea
risk_reg7 <- tgg_reg(regression_dataset_nibrs, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + prostitution_rate_msa + avg_commute_GIANTOAK +
                       outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                       outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                       outcall_only * prostitution_rate_msa + both_incall_outcall * prostitution_rate_msa + unclear_inc_out * prostitution_rate_msa +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

#Abuse + Arrest + Disease, using rape, prostitution, gonorrhea
risk_reg8 <- tgg_reg(regression_dataset_nibrs, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + rape_rate_msa + prostitution_rate_msa + avg_commute_GIANTOAK +
                       outcall_only * rape_rate_msa + both_incall_outcall * rape_rate_msa + unclear_inc_out * rape_rate_msa +
                       outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                       outcall_only * prostitution_rate_msa + both_incall_outcall * prostitution_rate_msa + unclear_inc_out * prostitution_rate_msa +
                       outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                       website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                       adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")


### save regression

sapply(c("pref_reg1", "pref_reg2","pref_reg3","abuse_reg1","abuse_reg2","abuse_reg3","abuse_reg4","abuse_reg5","abuse_reg6","arrest_reg1","arrest_reg2","arrest_reg3","arrest_reg4","std_reg1","std_reg2","std_reg3","std_reg4", "com_reg1", "com_reg2", "com_reg3", "risk_reg1", "risk_reg2", "risk_reg3","risk_reg4", "risk_reg5", "risk_reg6", "risk_reg7", "risk_reg8"),FUN=function(reg_name){
  reg_obj = eval(parse(text=reg_name))
  # summary.lm(reg_obj)
  filename = paste(regression_objects, "/",reg_name,".rds", sep="")
  saveRDS(reg_obj,file=filename)
})


#################################################
###### Regressions for Doc - msa-year level######
#################################################

ad_level_with_msa_year_char$median_income <- ad_level_with_msa_year_char$`median_income_in_the_past_12_months_--_total` 

#Verify violent rate is per 100K
summary(ad_level_with_msa_year_char$violent_rate)



#Specify missing variables to throw out in FE analysis

commute_vars = c("CBSACode","avg_com","year","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month","website","percent_poverty","median_income")

coli_vars = c("CBSACode","avg_com","violent_rate","year","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month", "website","COL_Composite","percent_poverty","median_income")

specify_vars_with_missings_violent = c("CBSACode","avg_com","year","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month","violent_rate","website","percent_poverty","median_income")

specify_vars_with_missings_property = c("CBSACode","avg_com","year","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month","property_rate","website","percent_poverty","median_income")


specify_vars_with_missings_rape= c("CBSACode","avg_com","year","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month","rape_rate","website","percent_poverty","median_income")



commute_regression_dataset <- no_missings_subset(ad_level_with_msa_year_char,commute_vars)

#coli_regression_dataset <- no_missings_subset(ad_level_with_msa_year_char,coli_vars)

violence_regression_dataset <- no_missings_subset(ad_level_with_msa_year_char, specify_vars_with_missings_violent)

property_regression_dataset <- no_missings_subset(ad_level_with_msa_year_char, specify_vars_with_missings_property)

rape_regression_dataset <- no_missings_subset(ad_level_with_msa_year_char, specify_vars_with_missings_rape)


## Section 1: Average Commute ##

msa_fe_reg_commute <- tgg_reg(commute_regression_dataset,price_per_hour ~  outcall_only + 
                                both_incall_outcall + unclear_inc_out + avg_com + outcall_only * avg_com + both_incall_outcall * avg_com + 
                                unclear_inc_out * avg_com + percent_poverty + median_income + as.character(CBSACode) + website +
                                as.character(month) +   as.character(year),"CBSACode")


## Section 2: Crime ##


#Violent rate, controls
msa_fe_reg_violent <- tgg_reg(violence_regression_dataset, 
                              price_per_hour ~violent_rate + outcall_only +  both_incall_outcall + unclear_inc_out + violent_rate * outcall_only +                                violent_rate * both_incall_outcall + violent_rate * unclear_inc_out + website + as.character(month) + 
                              median_income + poverty_rate +
                              as.character(year) +
                              as.character(CBSACode),
                              "CBSACode")


#property rate, controls
msa_fe_reg_property <- tgg_reg(property_regression_dataset, 
                               price_per_hour ~property_rate + outcall_only +  both_incall_outcall + unclear_inc_out + property_rate * outcall_only + property_rate * both_incall_outcall + 
                                 property_rate * unclear_inc_out + website + as.character(month) + median_income + poverty_rate +
                                 as.character(year) +
                                 as.character(CBSACode),
                               "CBSACode")

#rape rate, controls
msa_fe_reg_rape <- tgg_reg(rape_regression_dataset, 
                           price_per_hour ~rape_rate + outcall_only +  both_incall_outcall + unclear_inc_out + rape_rate * outcall_only + rape_rate * both_incall_outcall + 
                             rape_rate * unclear_inc_out + website + as.character(month) + median_income + poverty_rate +
                             as.character(year) +
                             as.character(CBSACode),
                           "CBSACode")


#violence + commute head2head
msa_fe_reg_violcom <- tgg_reg(violence_regression_dataset, 
                              price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + avg_com + 
                                violent_rate  + outcall_only * avg_com + both_incall_outcall * avg_com + unclear_inc_out * avg_com+
                                violent_rate * outcall_only + violent_rate * both_incall_outcall +
                                violent_rate * unclear_inc_out + website + as.character(month) +
                                as.character(year)  + median_income + percent_poverty +  as.character(CBSACode), "CBSACode")





sapply(c("msa_fe_reg_commute","msa_fe_reg_violcom"),FUN=function(reg_name){
  reg_obj = eval(parse(text=reg_name))
  # summary.lm(reg_obj)
  filename = paste(regression_objects, "/",reg_name,".rds", sep="")
  saveRDS(reg_obj,file=filename)
})

#################################################
###### Regressions for Doc - msa-year level with STDs######
#################################################

which_std <- "gon_rate"

commute_vars_std = c("CBSACode","avg_com","year","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month",
                 "website","percent_poverty","median_income","population", which_std)

commute_regression_dataset_std <- no_missings_subset(ad_level_with_msa_year_char,commute_vars_std)

msa_fe_reg_commute_std <- tgg_reg(commute_regression_dataset_std,price_per_hour ~  outcall_only + 
                                both_incall_outcall + unclear_inc_out + avg_com + outcall_only * avg_com + both_incall_outcall * avg_com + unclear_inc_out * avg_com + 
                                percent_poverty + median_income + log(population) + gon_rate + as.character(CBSACode) + website + as.character(month) + as.character(year),"CBSACode")

specify_vars_with_missings_violent_std = c("CBSACode","avg_com","year","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month",
                                       "violent_rate","website","percent_poverty","median_income", which_std)





#test run including spam and excluding nonhourly rates for robustness check of preferred specifications

if (!include_spam_exclude_nonhourlyrates) {
  
violence_regression_dataset_std <- no_missings_subset(ad_level_with_msa_year_char, specify_vars_with_missings_violent_std)


} else {
  
  violence_regression_dataset_std <- no_missings_subset(ad_level_with_msa_year_char, specify_vars_with_missings_violent_std, usual_filters = FALSE)
  violence_regression_dataset_std <- violence_regression_dataset[violence_regression_dataset$has_hour_price == TRUE & violence_regression_dataset$sex_ad == 1 & violence_regression_dataset$is_massage_parlor_ad == 0 & violence_regression_dataset$price_per_hour < 1000 & violence_regression_dataset$year >2010,]
}
table(violence_regression_dataset$spam)

msa_fe_reg_violcom_std <- tgg_reg(violence_regression_dataset_std, 
                              price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + avg_com + 
                                violent_rate  + outcall_only * avg_com + both_incall_outcall * avg_com + unclear_inc_out * avg_com+
                                violent_rate * outcall_only + violent_rate * both_incall_outcall +
                                violent_rate * unclear_inc_out + website + as.character(month) + 
                                as.character(year) +median_income + percent_poverty + log(population) + gon_rate +  as.character(CBSACode), "CBSACode")



sapply(c("msa_fe_reg_commute_std","msa_fe_reg_violcom_std"),FUN=function(reg_name){
  reg_obj = eval(parse(text=reg_name))
  # summary.lm(reg_obj)
  filename = paste(regression_objects, "/",reg_name,".rds", sep="")
  saveRDS(reg_obj,file=filename)
})


# End time for script
end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)


saveRDS(ad_level_with_msa_char,file = paste0(finalDatasets,"ad_price_msa_level.rds"))
saveRDS(ad_level_with_msa_year_char,file = paste0(finalDatasets,"ad_price_msa_year_level.rds"))



#################################################
###### Excluding unclear incall outcall ######
#################################################

### GO replications 

clustering_variable <- 'CBSACode'

specify_vars_with_missings <- c('avg_commute_GIANTOAK')
reg_dataset_unclear <- ad_level_with_msa_char

# Subset data
reg_dataset_unclear <- reg_dataset_unclear[reg_dataset_unclear$spam == FALSE & reg_dataset_unclear$sex_ad == 1 & reg_dataset_unclear$is_massage_parlor_ad == 0 & reg_dataset_unclear$price_per_hour < 1000, ] #Giant Oak filter
reg_dataset_unclear <- as.data.frame(reg_dataset_unclear)[complete.cases(as.data.frame(reg_dataset_unclear)[, specify_vars_with_missings]), ] #automatically drop missings - necessary for clustering properly
reg_dataset_unclear <- reg_dataset_unclear[reg_dataset_unclear$unclear_inc_out == 0, ]

# Run regression
go_rep_reg1_unclear <- tgg_reg(reg_dataset_unclear,price_per_hour ~ outcall_only + both_incall_outcall,"CBSACode")

# Save
saveRDS(reg_dataset_unclear, file = paste0(finalDatasets,"reg_dataset_unclear_go1.rds"))

# Respecify variables with missings
specify_vars_with_missings <- c('avg_commute_GIANTOAK', 'month', 'outcall_only','both_incall_outcall' , 
                                'uniqueproviders_MSAmonthlvl_GIANTOAK' , 'adcount_MSAmonthlvl_GIANTOAK',
                                'website' , 'population_GIANTOAK' , 'unemployment_GIANTOAK' , 'frac_white_GIANTOAK')

reg_dataset_unclear <- ad_level_with_msa_char

# Subset dataset
reg_dataset_unclear <- reg_dataset_unclear[reg_dataset_unclear$spam == FALSE & reg_dataset_unclear$sex_ad == 1 & reg_dataset_unclear$is_massage_parlor_ad == 0 & reg_dataset_unclear$price_per_hour < 1000, ] #Giant Oak filter
reg_dataset_unclear <- as.data.frame(reg_dataset_unclear)[complete.cases(as.data.frame(reg_dataset_unclear)[, specify_vars_with_missings]), ] #automatically drop missings - necessary for clustering properly
reg_dataset_unclear <- reg_dataset_unclear[reg_dataset_unclear$unclear_inc_out == 0, ]

# Run regressions and save
go_rep_reg2_unclear <- tgg_reg(reg_dataset_unclear, price_per_hour ~ outcall_only + both_incall_outcall
                               + uniqueproviders_MSAmonthlvl_GIANTOAK + adcount_MSAmonthlvl_GIANTOAK
                               + avg_commute_GIANTOAK + website + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month), "CBSACode")

saveRDS(reg_dataset_unclear, file = paste0(finalDatasets,"reg_dataset_unclear_go2.rds"))

go_rep_reg3_unclear <- tgg_reg(reg_dataset_unclear, price_per_hour ~ outcall_only + both_incall_outcall 
                               + uniqueproviders_MSAmonthlvl_GIANTOAK + adcount_MSAmonthlvl_GIANTOAK
                               + avg_commute_GIANTOAK + website + population_GIANTOAK + unemployment_GIANTOAK + frac_white_GIANTOAK + as.character(month) +
                                 outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK, "CBSACode")

saveRDS(reg_dataset_unclear, file = paste0(finalDatasets,"reg_dataset_unclear_go3.rds"))

### Pref reg for MSA-level (combined risks)

# Respecify missings
reg_dataset_unclear <- ad_level_with_msa_char

specify_vars_with_missings = c("CBSACode","year","COL_Composite_mr","price_per_hour","outcall_only","both_incall_outcall","month",
                               "property_rate_msa","violent_rate_msa","rape_rate_msa","website", "unemployment_GIANTOAK",
                               "frac_white_GIANTOAK","adcount_MSAmonthlvl_GIANTOAK","uniqueproviders_MSAmonthlvl_GIANTOAK", "ftsworn_rateper100k_2013")


reg_dataset_unclear <- no_missings_subset(reg_dataset_unclear, specify_vars_with_missings)

# Run regression and save
pref_reg3_unclear <- tgg_reg(reg_dataset_unclear, price_per_hour ~ outcall_only + both_incall_outcall + gon_rateper100k_2012 + violent_rate_msa + avg_commute_GIANTOAK +
                               outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 +
                               outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa +
                               outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK +
                               website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                               adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

saveRDS(reg_dataset_unclear, file = paste0(finalDatasets,"reg_dataset_unclear_pref.rds"))

### MSA fixed effects

# Respecify missings
reg_dataset_unclear <- ad_level_with_msa_year_char

specify_vars_with_missings = c("CBSACode","avg_com","year","COL_Composite_mr","price_per_hour","outcall_only","both_incall_outcall", "month",
                               "property_percap","violent_percap","rape_percap","website")

reg_dataset_unclear <- no_missings_subset(reg_dataset_unclear, specify_vars_with_missings)

commute_vars = c("CBSACode","avg_com","year","price_per_hour","outcall_only","both_incall_outcall", "month",
                 "website")

reg_dataset_unclear_com <- no_missings_subset(ad_level_with_msa_year_char,commute_vars)


# Run regreggion and save
msa_fe_reg_commute_unclear <- tgg_reg(reg_dataset_unclear_com,price_per_hour ~  outcall_only + both_incall_outcall + avg_com + outcall_only * avg_com + both_incall_outcall * avg_com 
                                      + as.character(CBSACode) + website + as.character(month),"CBSACode")


msa_fe_reg_violent_unclear <- tgg_reg(reg_dataset_unclear, 
                                      price_per_hour ~violent_percap + outcall_only +  both_incall_outcall + violent_percap * outcall_only + violent_percap * both_incall_outcall + 
                                        + website + as.character(month) +
                                        as.character(year) +
                                        as.character(CBSACode),
                                      "CBSACode")


saveRDS(reg_dataset_unclear_com, file = paste0(finalDatasets,"reg_dataset_unclear_fe1.rds"))
saveRDS(reg_dataset_unclear, file = paste0(finalDatasets,"reg_dataset_unclear_fe2.rds"))

sapply(c("go_rep_reg1_unclear","go_rep_reg2_unclear","go_rep_reg3_unclear","pref_reg3_unclear","msa_fe_reg_commute_unclear", "msa_fe_reg_violent_unclear"),FUN=function(reg_name){
  reg_obj = eval(parse(text=reg_name))
  # summary.lm(reg_obj)
  filename = paste(regression_objects, "/",reg_name,".rds", sep="")
  saveRDS(reg_obj,file=filename)
})


#################################################
###### Repeating regressions with log ftsworn ######
#################################################

# Respecify missings
reg_dataset <- ad_level_with_msa_char

specify_vars_with_missings = c("CBSACode","year","COL_Composite_mr","price_per_hour","outcall_only","both_incall_outcall","month",
                               "property_rate_msa","violent_rate_msa","rape_rate_msa","website", "unemployment_GIANTOAK",
                               "frac_white_GIANTOAK","adcount_MSAmonthlvl_GIANTOAK","uniqueproviders_MSAmonthlvl_GIANTOAK", "ftsworn_rateper100k_2013")


reg_dataset <- no_missings_subset(reg_dataset, specify_vars_with_missings)

# Run regression and save
log_reg1 <- tgg_reg(reg_dataset, price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + log(ftsworn_rateper100k_2013) + avg_commute_GIANTOAK +
                      outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
                      outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
                      outcall_only * log(ftsworn_rateper100k_2013) + both_incall_outcall * log(ftsworn_rateper100k_2013) + unclear_inc_out * log(ftsworn_rateper100k_2013) +
                      outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
                      website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
                      adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK, "CBSACode")

saveRDS(log_reg1, file = paste0(finalDatasets,"log_reg1.rds"))

sapply(c("go_rep_reg1_unclear","go_rep_reg2_unclear","go_rep_reg3_unclear","pref_reg3_unclear","msa_fe_reg_commute_unclear", "msa_fe_reg_violent_unclear"),FUN=function(reg_name){
  reg_obj = eval(parse(text=reg_name))
  # summary.lm(reg_obj)
  filename = paste(regression_objects, "/",reg_name,".rds", sep="")
  saveRDS(reg_obj,file=filename)
})


#################################################
######     Decomposition of Variance       ######
#################################################
reg_dataset <- ad_level_with_msa_char
specify_vars_with_missings = c("CBSACode","year","COL_Composite_mr","price_per_hour","outcall_only","both_incall_outcall","unclear_inc_out","month", "median_income", "poverty_rate",
                               "property_rate_msa","violent_rate_msa","rape_rate_msa","website", "unemployment_GIANTOAK",
                               "frac_white_GIANTOAK","adcount_MSAmonthlvl_GIANTOAK","uniqueproviders_MSAmonthlvl_GIANTOAK", "ftsworn_rateper100k_2013")


reg_dataset <- no_missings_subset(reg_dataset, specify_vars_with_missings)


reg_ctrl <- lm(price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
              website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
              adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK,reg_dataset)
r_reg_ctrl <- summary.lm(reg_ctrl)$r.squared
r_adj_reg_ctrl <- summary.lm(reg_ctrl)$adj.r.squared


reg_b <- lm(price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
              outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
              both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
              outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
              both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
              website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
              adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK,reg_dataset)

r_reg_b <- summary.lm(reg_b)$r.squared
r_adj_reg_b <- summary.lm(reg_b)$adj.r.squared

reg_no_out <- lm(price_per_hour ~ both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
              both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
              both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
              both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
              both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
              website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
              adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK,reg_dataset)

r_reg_no_out <- summary.lm(reg_no_out)$r.squared
r_adj_reg_no_out <- summary.lm(reg_no_out)$adj.r.squared



reg_v <- lm(price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
              outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
              outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
              outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
              both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
              website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
              adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK,reg_dataset)

r_reg_v <- summary.lm(reg_v)$r.squared
r_adj_reg_v <- summary.lm(reg_v)$adj.r.squared

delta_r_v_b <- r_reg_v - r_reg_b
delta_adj_r_v_b <- r_adj_reg_v - r_adj_reg_b


reg_c <- lm(price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
              outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
              both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
              outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
              outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
              website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
              adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK,reg_dataset)
r_reg_c <- summary.lm(reg_c)$r.squared
r_adj_reg_c <- summary.lm(reg_c)$adj.r.squared
delta_r_c_b <- r_reg_c -r_reg_b
delta_adj_r_c_b <- r_adj_reg_c - r_adj_reg_b

reg_vc <- lm(price_per_hour ~ outcall_only + both_incall_outcall + unclear_inc_out + gon_rateper100k_2012 + violent_rate_msa + ftsworn_rateper100k_2013 + avg_commute_GIANTOAK +
               outcall_only * gon_rateper100k_2012 + both_incall_outcall * gon_rateper100k_2012 + unclear_inc_out * gon_rateper100k_2012 +
               outcall_only * violent_rate_msa + both_incall_outcall * violent_rate_msa + unclear_inc_out * violent_rate_msa +
               outcall_only * ftsworn_rateper100k_2013 + both_incall_outcall * ftsworn_rateper100k_2013 + unclear_inc_out * ftsworn_rateper100k_2013 +
               outcall_only * avg_commute_GIANTOAK + both_incall_outcall * avg_commute_GIANTOAK + unclear_inc_out * avg_commute_GIANTOAK +
               website + as.character(month) + as.character(year) + COL_Composite_mr + log(population_GIANTOAK) + unemployment_GIANTOAK + frac_white_GIANTOAK +
               adcount_MSAmonthlvl_GIANTOAK + uniqueproviders_MSAmonthlvl_GIANTOAK,reg_dataset)
r_reg_vc <- summary.lm(reg_vc)$r.squared
r_adj_reg_vc <- summary.lm(reg_vc)$adj.r.squared

delta_r_vc_b <- r_reg_vc - r_reg_b
delta_r_vc_v <- r_reg_vc - r_reg_c
delta_r_vc_c <- r_reg_vc - r_reg_v

delta_r_adj_reg_vc_b <- r_adj_reg_vc - r_adj_reg_b
delta_r_adj_reg_vc_v <- r_adj_reg_vc - r_adj_reg_c
delta_r_adj_reg_vc_c <- r_adj_reg_vc - r_adj_reg_v

delta_r_vc_v/delta_r_vc_b
delta_r_vc_c/delta_r_vc_b

delta_r_vc_no_out <- r_reg_vc - r_reg_no_out

sapply(c("reg_ctrl","reg_b","reg_c","reg_v","reg_vc","reg_no_out"),FUN=function(reg_name){
  reg_obj = eval(parse(text=reg_name))
  # summary.lm(reg_obj)
  filename = paste(regression_objects, "/",reg_name,".rds", sep="")
  saveRDS(reg_obj,file=filename)
})






rstudio_test_end <-Sys.time()


print(rstudio_test_end - rstudio_test_start)

#2.4 hours with shortcut = FALSE
