
###############################################################################

# Client: Giant Oak
# Author(s): TGG
# Date created: 2016 01 15
# Purpose: Merge MSA-level (or MSA- & year-level) data

###############################################################################
# Set the working directory and default paths
################################################################################

clientName <- 'Giant Oak'
projectName <- '01 Human Trafficking'
serverPath <- '~/Shared/00 Clients - Current'

rawDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                     '/Structured Data/01 Raw Datasets/', sep="")
workingDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                         '/Structured Data/02 Working Datasets/xdata/', sep="")
finalDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                       '/Structured Data/03 Final Datasets/xdata/', sep="")
graphics <- paste(serverPath, '/', clientName, '/', projectName,
                  '/Structured Data/04 Graphics and Output Data/xdata/', sep="")

#code <- '~/Home/Git/'
code <- getwd()

################################################################################
# load in libraries and source files
################################################################################

# libraries:
librariesToLoad <- c("jsonlite", "ggplot2", "curl", "httr", 'data.table','dplyr', 'magrittr')



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

detach("package:plyr")

search()

# Start time for source files
start_time <- Sys.time()

# source files
sourceFiles <- c('XDATA/xdata_analyses_functions')



if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, "/", x, ".R", sep = "")))

# End time for source files
end_source_files <- Sys.time()


################################################################################
# Specify globals
################################################################################


replace_giant_oak_with_our_acs <- TRUE


################################################################################
# Start with master census crosswalk
################################################################################
#Read in master crosswalk used in ACS & STD data
crosswalk <- read.csv(paste0(rawDatasets, '2015-11-18 -- County MSA Crosswalk/crosswalk_cbsa_fips_county.csv'), 
                      stringsAsFactors = FALSE, colClasses = 'character', header = TRUE, skip = 2)

#Render into CBSA-level
cbsa_level_crosswalk <- crosswalk[!duplicated(crosswalk[['CBSA.Code']]), c('CBSA.Code', 'CBSA.Title', 'Metropolitan.Micropolitan.Statistical.Area')]
isTableAtThisLevel(cbsa_level_crosswalk, 'CBSA.Code')

saveRDS(cbsa_level_crosswalk, file =paste0(workingDatasets, "Crosswalks/CBSA List.RDS"))



#keep MSAs only
table(cbsa_level_crosswalk$Metropolitan.Micropolitan.Statistical.Area, exclude = NULL)
#388 MSAs
#Validate and clean
cbsa_level_crosswalk[cbsa_level_crosswalk$Metropolitan.Micropolitan.Statistical.Area == '', ]
#filler lines at end
cbsa_level_crosswalk <- cbsa_level_crosswalk[cbsa_level_crosswalk$Metropolitan.Micropolitan.Statistical.Area != '', ]

msa_level_crosswalk <- cbsa_level_crosswalk[cbsa_level_crosswalk$Metropolitan.Micropolitan.Statistical.Area == "Metropolitan Statistical Area", ]  
nrow(msa_level_crosswalk) #388

colnames(msa_level_crosswalk) <- sapply(colnames(msa_level_crosswalk), FUN = function(x) gsub("[[:punct:]]", "", x))
colnames(msa_level_crosswalk)


# fix weird encodnig of accents, etc
msa_level_crosswalk$CBSATitle <- iconv(msa_level_crosswalk$CBSATitle, from = 'UTF-8', to = 'UTF-8', sub = '')

msa_level_list <- msa_level_crosswalk
rm(msa_level_crosswalk)

#States assigned to MSAs
msa_level_list$all_states <- gsub("^.*,", "", msa_level_list$CBSATitle)

has_dash_index <- grepl("-", msa_level_list$all_states)
prop.table(table(has_dash_index))
msa_level_list$primary_state[has_dash_index] <- gsub("-.*$", "", msa_level_list$all_states[has_dash_index])
msa_level_list$primary_state[!has_dash_index] <- msa_level_list$all_states[!has_dash_index]
has_dash_index <- NULL
length(unique(msa_level_list$primary_state)) #52 (inc. DC, PR)
table(msa_level_list$primary_state, exclude = NULL)

#87% have one state. just pick the first state mentioned as primary for remaining 13%



saveRDS(msa_level_list, paste0(finalDatasets, "Master MSA-Level/List/msa_level_list.RDS"))
write.csv(msa_level_list, paste0(finalDatasets, "Master MSA-Level/List/msa_level_list.csv"), row.names = FALSE)


#Create master MSA- & year-level
years <- 2010:2014

msa_year_level_master <- NULL
#Append all years
for (i in years) {
  year <- msa_level_list
  year$year <- i
  msa_year_level_master <- rbind(msa_year_level_master, year)
}


nrow(msa_year_level_master) == nrow(msa_level_list) * length(years)

msa_year_level_master <- msa_year_level_master[order(msa_year_level_master$CBSACode, msa_year_level_master$year), ]
str(msa_year_level_master)

saveRDS(msa_year_level_master, paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))

################################################################################
# Merge in STD Data
################################################################################
# Read in data 
msa_level_master <- readRDS(paste0(finalDatasets, "Master MSA-Level/List/msa_level_list.RDS"))
std <-  readRDS(file = paste0(workingDatasets, '/STD/STD_cbsa.RDS'))
str(std)
nrow(std) #917 -- includes micropolitan statistical areas
isTableAtThisLevel(std, 'CBSA')

std_yearly <- read.csv(file = paste0(finalDatasets, '/STD/year_level_std.csv'),
                       colClasses = c('cbsa_code' = 'character', 'cbsa_name' = 'character'))
str(std_yearly)

# adjusting column names / drop columns
std$cbsa_title <- NULL
colnames(std)[colnames(std) == "CBSA"] <- "CBSACode"
colnames(std)[colnames(std) == "POPESTIMATE2012"] <- "pop_report_chl_2012"
colnames(std)[colnames(std) == "pop_gon"] <- "pop_report_gon_2012"
colnames(std)[colnames(std) == "chl_rate"] <- "chl_rateper100k_2012"
colnames(std)[colnames(std) == "gon_rate"] <- "gon_rateper100k_2012"
colnames(std)[colnames(std) == "num_gon"] <- "calc_num_gon_2012"
colnames(std)[colnames(std) == "num_chl"] <- "calc_num_chl_2012"

sapply(std_yearly, FUN = function(x) sum(is.na(x))/nrow(std_yearly))
data.table(std_yearly) [,.(missings = sum(is.na(population))),
                        by = year]
#all 2009 info is missing
str(std_yearly)
colnames(std_yearly)[colnames(std_yearly) == "cbsa_code"] <- "CBSACode"
colnames(std_yearly)[colnames(std_yearly) == "population"] <- "pop_stdreporting"
std_yearly$cbsa_name <- NULL

#Merge into MSA-level data
msa_level_master_std <- merge(msa_level_master, std, by = 'CBSACode', all.x = TRUE)
#Validate
nrow(msa_level_master_std) == nrow(msa_level_master)
isTableAtThisLevel(msa_level_master_std, 'CBSACode')

#Missings
sapply(msa_level_master_std, FUN = function(x) sum(is.na(x)))
sapply(msa_level_master_std, FUN = function(x) sum(is.na(x)) / nrow(msa_level_master_std))

msa_level_master_std$CBSATitle[!complete.cases(msa_level_master_std)]
#all Puerto Rico, one MT (the gonorrhoea)

msa_level_master <- msa_level_master_std
rm(msa_level_master_std)
str(msa_level_master)
saveRDS(msa_level_master, file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))

#Merge into MSA- year-level data
msa_year_level_master_std <- merge(msa_year_level_master, std_yearly, by = c('CBSACode', 'year'), all.x = TRUE)
  #Validate
  isTableAtThisLevel(msa_year_level_master_std, c('CBSACode', 'year'))
  sapply(msa_year_level_master_std, FUN = function(x) sum(is.na(x)))
  
msa_year_level_master <- msa_year_level_master_std
rm(msa_year_level_master_std)

saveRDS(msa_year_level_master, paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))

################################################################################
# Merge in Population Data
################################################################################
# read in data
msa_year_level_master <- readRDS(file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))
str(msa_year_level_master)

population_cbsa <- readRDS(file = paste0(workingDatasets, "population/population_cbsa.RDS"))
str(population_cbsa)

isTableAtThisLevel(population_cbsa, 'CBSA.Code')

#reshape to long
population_cbsa <- data.table(population_cbsa)

population_cbsa_year <- melt(population_cbsa, id.vars = 'CBSA.Code',
                             variable.name = 'yearvar',
                             value.name = 'population')
isTableAtThisLevel(population_cbsa_year, c('CBSA.Code', 'yearvar'))

population_cbsa_year$year <- as.numeric(gsub("[^[:digit:]]", "\\1", as.character(population_cbsa_year$yearvar)))
population_cbsa_year$yearvar <- NULL

#merge
msa_year_level_master_pop <- merge(msa_year_level_master, population_cbsa_year, by.x = c('CBSACode', 'year'), by.y = c('CBSA.Code', 'year'), all.x = TRUE)
#Validate
nrow(msa_year_level_master_pop) == nrow(msa_year_level_master)
sapply(msa_year_level_master_pop, FUN = function(x) sum(is.na(x)))
msa_year_level_master_pop[is.na(msa_year_level_master_pop$population), ]
#all Puerto Rico and Lynchburg, Virgnia

msa_year_level_master <- msa_year_level_master_pop
rm(msa_year_level_master_pop)
str(msa_year_level_master)
saveRDS(msa_year_level_master, file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))

################################################################################
# Merge in ACS Data
################################################################################
# read in data
msa_year_level_master <- readRDS(file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))
str(msa_year_level_master)


acs_master_long <- readRDS(file = paste0(workingDatasets, "/demographics/acs_master_long.RDS"))
str(acs_master_long)
isTableAtThisLevel(acs_master_long, c('cbsa', 'year'))

# rename population variable:
colnames(acs_master_long)[colnames(acs_master_long) == "popestimate"] <- "pop_acs"

# keep only variables used for analysis
analysis_variables <- c('avg_com', 'foster_kid_prop', 'educ_male_agg_no_hs_prop', 'educ_female_agg_no_hs_prop', 'pop_acs', 'med_age_tot',  'med_age_male',  'med_age_female', 'mean_age_male', 'mean_age_female', 'mean_age','poverty_pop_total','income_in_the_past_12_months_below_poverty_level','median_income_in_the_past_12_months_--_total', 'enrolled_male_prop', 'enrolled_female_prop', 'prop_unemployed','prop_white_alone')
age_vars <- names(acs_master_long)[grepl('prop_', names(acs_master_long)) & grepl('years', names(acs_master_long))]

acs_master_long <- acs_master_long[, c('name', 'year', 'cbsa',  analysis_variables, age_vars)]

# create poverty variable
acs_master_long$percent_poverty <- acs_master_long$income_in_the_past_12_months_below_poverty_level / acs_master_long$poverty_pop_total

acs_dataframe <- as.data.frame(acs_master_long)

poverty_dataset <- acs_dataframe %>% group_by(cbsa) %>% summarise(poverty_rate = mean(percent_poverty,na.rm=TRUE), median_income = mean(`median_income_in_the_past_12_months_--_total`,na.rm=TRUE))

msa_level_master <- merge(msa_level_master,poverty_dataset,by.x = "CBSACode",by.y="cbsa")

saveRDS(msa_level_master, file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))


# merge
merged <- merge(msa_year_level_master, acs_master_long, by.x = c('CBSACode', 'year'), by.y = c('cbsa', 'year'), all.x = TRUE)
# validate:
#   level:
isTableAtThisLevel(merged, c('CBSACode', 'year'))
#   number of rows
nrow(msa_year_level_master) # 1940
nrow(acs_master_long) # 2814
nrow(merged) # 1940
#   missings
table(is.na(merged$CBSACode)) # 0 missing cbsa code
table(is.na(merged$pop_acs)) # 800 missing (for 2010 and 2011??)
summary(merged[is.na(merged$pop_acs), 'year']) # (not all for 2010 and 2011)
merged[(is.na(merged$pop_acs) & (merged$year != 2010) & (merged$year != 2011)), c('CBSACode', 'name', 'year')]
# those missing populations are from PR and Lynchburg, VA (Bedford City whose independence as a city was removed)
# this is okay.

msa_year_level_master <- merged
rm(merged)
rm(acs_master_long)


# save dataset
str(msa_year_level_master)
saveRDS(msa_year_level_master, file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))


################################################################################
# Merge in UCR & LEMAS Data
################################################################################

# Reading in data
msa_level_master <- readRDS(file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))
msa_year_level_master <- readRDS(file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))
crime_msa_year <- readRDS(paste0(workingDatasets, "Crime Data/crime_msa_year_level.RDS"))
crime_msa <- readRDS(paste0(workingDatasets, "Crime Data/crime_msa.RDS"))

# Merge MSA-level
merged <- merge(msa_level_master, crime_msa, by = "CBSACode", all = TRUE)

# Validate
nrow(merged) == nrow(msa_level_master)

# Nope
nrow(merged[is.na(merged$CBSATitle), ])
table(cbsa_level_crosswalk$Metropolitan.Micropolitan.Statistical.Area[cbsa_level_crosswalk$CBSA.Code %in%  merged$CBSACode[is.na(merged$CBSATitle)]])

# They're all micropolitan statistical areas. do a left join instead (or missing)
merged <- merge(msa_level_master, crime_msa, by = "CBSACode", all.x = TRUE)
nrow(merged) == nrow(msa_level_master)
msa_level_master <- merged
rm(merged)

sapply(msa_level_master, FUN = function(x) sum(is.na(x)))
sapply(msa_level_master, FUN = function(x) sum(is.na(x)) / nrow(msa_level_master))

# Merge MSA-year-level
merged <- merge(msa_year_level_master, crime_msa_year, by = c("CBSACode", "year"), all.x = TRUE)
msa_year_level_master <- merged

# Get 100K rates for MSA-year-level
for (i in c('violent', 'property' ,'rape')) {
    msa_year_level_master[[paste0(i, '_rate')]] <- msa_year_level_master[[paste0(i, '_percap')]] * 100000
}

# Save
str(msa_year_level_master)
saveRDS(msa_year_level_master, file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))

str(msa_level_master)
saveRDS(msa_level_master, file= paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))

################################################################################
# Merge in COLI Data
################################################################################

msa_level_master <- readRDS(file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))
msa_year_level_master <- readRDS(file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))
coli_msa_year <- read.csv(paste0(finalDatasets,"COL/coli_master.csv"),strip.white=TRUE,stringsAsFactors = FALSE)
coli_msa_year <- as.data.table(coli_msa_year)

str(coli_msa_year)
#get rid of columns we dont need and rename columns where useful
coli_msa_year <- coli_msa_year[,COLA_City := NULL,]
coli_msa_year <- coli_msa_year[,Occurences := NULL,]
coli_msa_year <- coli_msa_year[,CBSA_name:= NULL,]
colnames(coli_msa_year)

oldnames <- c("Composite","Grocery","Housing","Utilities","Transport","Healthcare","Misc")
intermediatenames <- paste0("COL_",oldnames)
names2014 <- paste0(intermediatenames,"_2014")
namesmostrecent <- paste0(intermediatenames, "_mr")
setnames(coli_msa_year, oldnames,intermediatenames)
setnames(coli_msa_year,"CBSA_code","CBSACode")
colnames(coli_msa_year)
#make year level coli dataset
#first 2014 only
coli_2014_only <- coli_msa_year[year==2014,,]
#now just take most recent for each CBSACode
coli_msa_year[,max_year:=max(year),by=CBSACode]
coli_most_recent<- coli_msa_year[year==max_year,,]
setnames(coli_2014_only,intermediatenames,names2014)
setnames(coli_most_recent,intermediatenames,namesmostrecent)
#validate
stopifnot(nrow(coli_most_recent)==length(unique(coli_most_recent$CBSACode)))
# merge with msa year level master
merged_year_level <- merge(msa_year_level_master, coli_msa_year, by=c('CBSACode','year'),all.x=TRUE)
#Validate
isTableAtThisLevel(merged_year_level, c('CBSACode','year'))
# merge with msa level
colnames(coli_2014_only)[colnames(coli_2014_only) == "area_average" |
                           colnames(coli_2014_only) == "multi.match_average"] <- paste0(colnames(coli_2014_only)[colnames(coli_2014_only) == "area_average" |
                                                                                                                   colnames(coli_2014_only) == "multi.match_average"],
                                                                                        "_coli_2014")
colnames(coli_2014_only)
coli_2014_only$year <- NULL
merged_msa_level <- merge(msa_level_master, coli_2014_only, by='CBSACode',all.x=TRUE)

colnames(coli_most_recent)[colnames(coli_most_recent) == "area_average" |
                             colnames(coli_most_recent) == "multi.match_average" |
                             colnames(coli_most_recent) == "max_year"] <- paste0(colnames(coli_most_recent)[colnames(coli_most_recent) == "area_average" |
                                                                                                              colnames(coli_most_recent) == "multi.match_average"  |
                                                                                                              colnames(coli_most_recent) == "max_year"],
                                                                                 "_coli_mr")
colnames(coli_most_recent)
coli_most_recent$year <- NULL
merged_msa_level <- merge(merged_msa_level, coli_most_recent,by='CBSACode',all.x=TRUE)
stopifnot(nrow(merged_msa_level)==nrow(msa_level_master))
msa_year_level_master <- merged_year_level
msa_level_master <- merged_msa_level
msa_level_master$year <- NULL
rm(merged_year_level)
rm(merged_msa_level)


saveRDS(msa_year_level_master, file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))
saveRDS(msa_level_master, file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))
################################################################################
# Merge in age ad data
################################################################################
msa_year_level_master <- readRDS(file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))
age_msa_postdate_all_data <- readRDS(file = paste0(finalDatasets, "Master MSA- Year-Level/age_msa_postdate_all_data.RDS"))
age_bucket_msa_year <- readRDS(file = paste0(finalDatasets, "Master MSA- Year-Level/age_bucket_msa_year.RDS"))

str(age_msa_postdate_all_data)
isTableAtThisLevel(age_msa_postdate_all_data, c('msa', 'year'))

str(age_bucket_msa_year)
isTableAtThisLevel(age_bucket_msa_year, c('msa', 'year'))

length(unique(age_bucket_msa_year$msa))
length(unique(age_msa_postdate_all_data$msa))
#741 -- probably includes microSAs

age_bucket_msa_year$msa <- as.character(age_bucket_msa_year$msa)
age_msa_postdate_all_data$msa <- as.character(age_msa_postdate_all_data$msa)

colnames(age_bucket_msa_year) <- paste0(colnames(age_bucket_msa_year), "_agead")
colnames(age_msa_postdate_all_data) <- paste0(colnames(age_msa_postdate_all_data), "_agead")

#merge buckets
msa_year_level_master_ages <- merge(msa_year_level_master, age_bucket_msa_year,
                                    by.x = c('CBSACode', 'year'), by.y = c('msa_agead', 'year_agead'), all.x = TRUE)
#Validate
nrow(msa_year_level_master_ages) == nrow(msa_year_level_master)
isTableAtThisLevel(msa_year_level_master_ages, c('CBSACode', 'year'))
str(msa_year_level_master_ages)
msa_year_level_master <-  msa_year_level_master_ages 
rm(msa_year_level_master_ages)

#merge additional stats  
msa_year_level_master_ages <- merge(msa_year_level_master, age_msa_postdate_all_data,
                                    by.x = c('CBSACode', 'year'), by.y = c('msa_agead', 'year_agead'), all.x = TRUE)
#Validate
nrow(msa_year_level_master_ages) == nrow(msa_year_level_master)
isTableAtThisLevel(msa_year_level_master_ages, c('CBSACode', 'year'))
str(msa_year_level_master_ages)
msa_year_level_master <-  msa_year_level_master_ages 
rm(msa_year_level_master_ages)

#Quick summary stats
sapply(unique(msa_year_level_master$year), 
       FUN = function(x) quantile(msa_year_level_master$pct_below_21_agead[msa_year_level_master$year == x],
                                  probs = seq(0,1,0.05), na.rm = TRUE))
print(unique(msa_year_level_master$year))

#2010 iffy year. rest have 10-25% of MSAs with no <21 ads
for (i in unique(msa_year_level_master$year)) {
  dataset <- msa_year_level_master[msa_year_level_master$year == i, ]
  print(qplot(dataset$pct_below_21_agead,
              data = dataset,
              geom = 'histogram', main = i))
  
}
#2011 also sorta iffy

saveRDS(msa_year_level_master, file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))


################################################################################
# Add in Giant Oak variables (where necessary)
################################################################################
# MSA-level ####
#Load datasets
# TGG's
msa_level_master <- readRDS(file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))
str(msa_level_master)
# (GO's)
#msa_characteristics_GIANTOAK <- read.csv('http://giantoakmemex.s3.amazonaws.com/sex_ad_analysis/output/msa_characteristics.csv', stringsAsFactors = FALSE)
#write.csv(msa_characteristics_GIANTOAK, file = paste0(rawDatasets, "2015-11-02 -- GO export tables/msa_characteristics.csv"), row.names = FALSE)
msa_characteristics_GIANTOAK <- read.csv(paste0(rawDatasets, "2015-11-02 -- GO export tables/msa_characteristics.csv"), stringsAsFactors = FALSE)
str(msa_characteristics_GIANTOAK)
isTableAtThisLevel(msa_characteristics_GIANTOAK, 'census_msa_code')
colnames(msa_characteristics_GIANTOAK) <- paste0(colnames(msa_characteristics_GIANTOAK) , "_GIANTOAK")

# controls in their regression adcount uniqueproviders  population unemployment frac_white - controls for their pricing analyses
vars_to_keep <- c('census_msa_code_GIANTOAK', 'avg_commute_GIANTOAK', 'population_GIANTOAK', 'unemployment_GIANTOAK', 'frac_white_GIANTOAK')
msa_characteristics_GIANTOAK <- msa_characteristics_GIANTOAK[, vars_to_keep]
msa_characteristics_GIANTOAK$CBSACode <- gsub('31000US', '', msa_characteristics_GIANTOAK$census_msa_code_GIANTOAK)
msa_characteristics_GIANTOAK$census_msa_code_GIANTOAK <- NULL

#merge

msa_level_master_merge <- merge(msa_level_master, msa_characteristics_GIANTOAK, by = 'CBSACode', all.x = TRUE)
#Validate
nrow(msa_level_master_merge) == nrow(msa_level_master)
isTableAtThisLevel(msa_level_master_merge, 'CBSACode')
str(msa_level_master_merge)

msa_level_master <- msa_level_master_merge
rm(msa_level_master_merge)

saveRDS(msa_level_master, file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))





################################################################################
# Analogous Giant Oak variable creation from ACS data
################################################################################

msa_level_from_year <- data.table(msa_year_level_master)[,.(max_commute = max(avg_com, na.rm =TRUE),
                                                            mean_commute = mean(avg_com, na.rm =TRUE),
                                                            max_population = max(population, na.rm =TRUE),
                                                            mean_population = mean(population, na.rm = TRUE),
                                                            max_unemployed = max(prop_unemployed, na.rm = TRUE),
                                                            mean_unemployed = mean(prop_unemployed, na.rm = TRUE),
                                                            max_frac_white = max(prop_white_alone, na.rm = TRUE),
                                                            mean_frac_white = mean(prop_white_alone, na.rm = TRUE)),
                                                         by = CBSACode]    


sapply(msa_level_from_year, FUN = function(x) sum(is.na(x)))
msa_level_from_year <- lapply(msa_level_from_year, FUN = function(x) ifelse(x == -Inf, NA, x))
sapply(msa_level_from_year, FUN = function(x) sum(is.na(x)))
msa_level_from_year$mean_population <- round(msa_level_from_year$mean_population)

msa_level_comparison <- merge(msa_level_from_year, msa_level_master, by = 'CBSACode', all = TRUE)
sapply(msa_level_comparison, FUN = function(x) sum(is.na(x)))
nrow(msa_level_comparison) == nrow(msa_level_master)
ncol(msa_level_comparison) > ncol(msa_level_master)

#Commute disparity
summary(msa_level_comparison$max_commute - msa_level_comparison$mean_commute)
summary((msa_level_comparison$max_commute - msa_level_comparison$avg_commute_GIANTOAK)/ msa_level_comparison$avg_commute_GIANTOAK)
#-2% - 12%
summary((msa_level_comparison$mean_commute - msa_level_comparison$avg_commute_GIANTOAK)/ msa_level_comparison$avg_commute_GIANTOAK)
#-4% - 5% mean closer to 0 ie. mean_commute is better

#population disparity
summary((msa_level_comparison$max_population - msa_level_comparison$population_GIANTOAK)/ msa_level_comparison$population_GIANTOAK)
summary((msa_level_comparison$mean_population - msa_level_comparison$population_GIANTOAK)/ msa_level_comparison$population_GIANTOAK)
quantile((msa_level_comparison$mean_population - msa_level_comparison$population_GIANTOAK)/ msa_level_comparison$population_GIANTOAK, probs = seq(0,1,0.1), na.rm=TRUE)
#View(msa_level_comparison[, c('mean_population', 'max_population', 'population_GIANTOAK', 'CBSACode', 'CBSATitle')])
#Looks like Giant Oak's population variable underestimates population by around half. 

#fraction white disparity
summary((msa_level_comparison$max_frac_white - msa_level_comparison$frac_white_GIANTOAK)/ msa_level_comparison$frac_white_GIANTOAK)
summary((msa_level_comparison$mean_frac_white - msa_level_comparison$frac_white_GIANTOAK)/ msa_level_comparison$frac_white_GIANTOAK)
#Both approximate very closely

#unemployment disparity (pp)
summary((msa_level_comparison$max_unemployed - msa_level_comparison$unemployment_GIANTOAK))
summary((msa_level_comparison$mean_unemployed - msa_level_comparison$unemployment_GIANTOAK))
#max is closer
#View(msa_level_comparison[, c('mean_unemployed', 'max_unemployed', 'unemployment_GIANTOAK', 'CBSACode', 'CBSATitle')])

msa_level_master <- msa_level_comparison

#Replace Giant Oak variables with ours

if (replace_giant_oak_with_our_acs) {
    
    msa_level_master$population_GIANTOAK <- msa_level_master$mean_population
    msa_level_master$frac_white_GIANTOAK <- msa_level_master$mean_frac_white
    msa_level_master$unemployment_GIANTOAK <- msa_level_master$mean_unemployed
    msa_level_master$avg_commute_GIANTOAK <- msa_level_master$mean_commute
}

msa_level_master$CBSACode <- as.character(msa_level_master$CBSACode)
saveRDS(msa_level_master, file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.RDS"))

write.csv(msa_level_master, file = paste0(finalDatasets, "Master MSA-Level/msa_level_master.csv"), row.names = FALSE)
write.csv(msa_year_level_master, file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.csv"), row.names = FALSE)

################################################################################
# Give Giant Oak crime data
################################################################################

colnames(msa_year_level_master)
ucr_crime_msayearlevel <- msa_year_level_master[, c('CBSACode', 'year', 'CBSATitle', 'violent', 'property', 'rape', 'violent_rate', 'property_rate', 'rape_rate')]

write.csv(ucr_crime_msayearlevel, paste0(finalDatasets, 'Master MSA- Year-Level/ucr_crime_msayearlevel.csv'), row.names = FALSE)
