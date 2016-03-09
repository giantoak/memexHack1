###############################################################################

# Client: Giant Oak
# Author(s): Nicole Ozminkowski, Pete Hlawitschka
# Date created: 2015 12 11
# Purpose: Format ACS data to a usable R dataframe

###############################################################################
# Set the working directory and default paths
###############################################################################

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

code <- getwd() 

###############################################################################
# load in libraries and source files
###############################################################################
# detach dplyr because it gives an error when we try to run plyr rename function
if ('package:dplyr' %in% search()) {
    detach('package:dplyr')
}


# libraries:
librariesToLoad <- c("jsonlite", "ggplot2", "curl", "data.table", "plyr", "microbenchmark", "reshape2")

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



# Start time for source files
start_time <- Sys.time()

# Load source files 
#Specify files with constructor functions here
sourceFiles <- c()

if(length(sourceFiles > 0))
    sapply(sourceFiles, FUN = function(x) source(paste(code, x, ".R", sep = "")))

# End time for source files
end_source_files <- Sys.time()

#############################################################
# import complete datasets (leave commented out unless you're reloading this file from an empty environment and don't want to load data from the APIs)
#############################################################
# 
# # load acs_master
#   acs_master <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master.RDS'))
# 
# # load individual acs files 
#   acs_master_2012 <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master_2012.RDS'))
#   acs_master_2013 <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master_2013.RDS'))
#   acs_master_2014 <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master_2014.RDS'))
# 
# # load wide
#   acs_master_wide <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master_wide.RDS'))
# 
# # load long
#   acs_master_long <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master_long.RDS'))
# 
# # load ACS_SUMMARY file
# ACS_SUMMARY <- readRDS(file = paste0(workingDatasets, "ACS/ACS 2014 Data Dictionary.RDS"))
# acs_summary_string <- data.frame(lapply(ACS_SUMMARY, as.character), stringsAsFactors=FALSE)
# 
# 


######################################################################################################
######################################################################################################
########################## Write the functions we'll use for this file ###############################
######################################################################################################
######################################################################################################


### Function to pull ACS data from api for specified varaibles in a given year
get_acs <- function(varnames, year) {

    # Collapse vector of varnames into single string
    varstring <- paste0(varnames, collapse = ',')
    
    data <- read.csv(paste0('http://api.census.gov/data/', 
                      year,
                      '/acs1?get=NAME,GEOID,',
                      varstring,
                      '&for=metropolitan+statistical+area/micropolitan+statistical+area:*&key=771d1399d34e281a05a4c1303f4d6e62e6f95e04'),
               na.strings = 'null',
               stringsAsFactors = FALSE)
    
    
    # rename county
    data$cbsa <- data$metropolitan.statistical.area.micropolitan.statistical.area.
    data$metropolitan.statistical.area.micropolitan.statistical.area. <- NULL
    
    # rename x..name
    data$name <- data$X..NAME
    data$X..NAME <- NULL
    
    # remove bracket and comma
    data$cbsa <- gsub('\\]','',data$cbsa)
    data$name <- gsub('\\[', '', data$name)
    
    # drop X (empty column)
    sum(is.na(data$X)) == nrow(data)
    data$X <- NULL
    
    # remove unnecessary columns
    data$GEOID <- NULL
    
    # add year
    data$year <- year
    
    # validate level
    if (nrow(data) == length(unique(data$cbsa))) {
    print("dataset at CBSA level")
    } else {
    print("DATASET NOT AT CBSA LEVEL OH NOOOOOOO")
    }
    
    # add time lapse between requests:
    print(Sys.time())
    Sys.sleep(2)
    print(Sys.time())
    
    return(data)
}


###  Function to pull ACS variables for multiple years (calls get_acs) 
get_years <- function(varnames, year_list) {
    data_list <- list(NULL)
    for (i in 1:length(year_list)){
        year_data <- get_acs(varnames, year_list[i])
        data_list[[i]] <- year_data
    }
    
    return(data_list)
}


### Function to pull in only columns with at least 90% of the data available
get_avail <- function(dataframe, threshold = 0.1) {
    #Below step is (marginally) more efficient and elegant
    data <- dataframe[, sapply(dataframe, function(x) sum(is.na(x)) < (threshold * nrow(dataframe)))]
    return(data)
}


### Function to convert strings to numbers except for excluded columns
to_numeric <- function(dataframe, excl_col = NULL) {
    if(is.null(excl_col)) {
        dataframe[, names(dataframe)] <- sapply(dataframe[, names(dataframe)] , as.numeric )
    } else {
        dataframe[, names(dataframe) != excl_col] <- sapply(dataframe[, names(dataframe) != excl_col] , as.numeric )
    }
    return(dataframe)
}


### Function to create proportion variables
division_fun <- function(dataset, num, denom, newvarname) {
    dataset[[newvarname]] <- as.numeric(dataset[[num]])/as.numeric(dataset[[denom]])
    return(dataset)
}

### Function to aggregate/add up multiple columns into one totaled column
add_up <- function(dataset, varlist, new_var_name) {
    test <- sapply(1:length(varlist), function(x) cbind(as.numeric(dataset[[varlist[x]]])))
    dataset[[new_var_name]] <- rowSums(test)
    return(dataset)
}


### Function to drop variables based on a term they all contain
drop_vars <- function(dataset, search_term) {
    extra_columns <- grep(search_term, names(dataset))
    dataset <- dataset[, -extra_columns]
    return(dataset)
}

### Function to compute commute time variables
create_avg_com <- function(dataset){
  
  # Average of each interval (except we code '90 or more minutes' as 100 minutes)
  commute_centers = c(2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 52.5, 72.5, 100)
  # Multiply number of people in each commute time bucket by the midpoint of that time bucket
  # then divide by number of people to get average commute time per CBSA
  dataset$avg_com = ((dataset$B08303_002E * commute_centers[1] + 
                        dataset$B08303_003E * commute_centers[2] + 
                        dataset$B08303_004E * commute_centers[3] + 
                        dataset$B08303_005E * commute_centers[4] + 
                        dataset$B08303_006E * commute_centers[5] + 
                        dataset$B08303_007E * commute_centers[6] + 
                        dataset$B08303_008E * commute_centers[7] + 
                        dataset$B08303_009E * commute_centers[8] + 
                        dataset$B08303_010E * commute_centers[9] + 
                        dataset$B08303_011E * commute_centers[10] + 
                        dataset$B08303_012E * commute_centers[11] + 
                        dataset$B08303_013E * commute_centers[12])/
                       dataset$B08303_001E)
  return(dataset)
}

### Function to compute mean age by CBSA, for a given gender
create_avg_age <- function(dataset, male_or_female){
  
    # Average of each age bucket (85+ is coded as 85)
    age_bucket_center = c(2.5, 7, 12, 16, 18.5, 20, 21, 23, 27, 32, 37, 42, 47, 52, 57, 60.5, 63, 65.5, 68, 72, 77, 82, 85)
    
    # Multiply number of males/females in each age-bucket by the midpoint of that age-bucket
    # then divide by number of males/females to get average age of males/female, per CBSA
    dataset[paste0('mean_age_',male_or_female)] = 
        (dataset[paste0('Prop ',male_or_female,' Under_5_years')] * age_bucket_center[1] + 
        dataset[paste0('Prop ',male_or_female,' 5_to_9_years')] * age_bucket_center[2] + 
        dataset[paste0('Prop ',male_or_female,' 10_to_14_years')] * age_bucket_center[3] +
        dataset[paste0('Prop ',male_or_female,' 15_to_17_years')] * age_bucket_center[4] + 
        dataset[paste0('Prop ',male_or_female,' 18_and_19_years')] * age_bucket_center[5] + 
        dataset[paste0('Prop ',male_or_female,' 20_years')] * age_bucket_center[6] + 
        dataset[paste0('Prop ',male_or_female,' 21_years')] * age_bucket_center[7] + 
        dataset[paste0('Prop ',male_or_female,' 22_to_24_years')] * age_bucket_center[8] + 
        dataset[paste0('Prop ',male_or_female,' 25_to_29_years')] * age_bucket_center[9] + 
        dataset[paste0('Prop ',male_or_female,' 30_to_34_years')] * age_bucket_center[10] + 
        dataset[paste0('Prop ',male_or_female,' 35_to_39_years')] * age_bucket_center[11] + 
        dataset[paste0('Prop ',male_or_female,' 40_to_44_years')] * age_bucket_center[12] + 
        dataset[paste0('Prop ',male_or_female,' 45_to_49_years')] * age_bucket_center[13] + 
        dataset[paste0('Prop ',male_or_female,' 50_to_54_years')] * age_bucket_center[14] + 
        dataset[paste0('Prop ',male_or_female,' 55_to_59_years')] * age_bucket_center[15] + 
        dataset[paste0('Prop ',male_or_female,' 60_and_61_years')] * age_bucket_center[16] + 
        dataset[paste0('Prop ',male_or_female,' 62_to_64_years')] * age_bucket_center[17] + 
        dataset[paste0('Prop ',male_or_female,' 65_and_66_years')] * age_bucket_center[18] + 
        dataset[paste0('Prop ',male_or_female,' 67_to_69_years')] * age_bucket_center[19] + 
        dataset[paste0('Prop ',male_or_female,' 70_to_74_years')] * age_bucket_center[20] +
        dataset[paste0('Prop ',male_or_female,' 75_to_79_years')] * age_bucket_center[21] +
        dataset[paste0('Prop ',male_or_female,' 80_to_84_years')] * age_bucket_center[22] +
        dataset[paste0('Prop ',male_or_female,' 85_years_and_over')] * age_bucket_center[23]) / (dataset[paste0('Prop ', male_or_female)])
    return(dataset)
    }


### Function to get an average age if you have male and female ages
get_population_age <- function(dataset, male_var, male_pop, female_var, female_pop, new_var_name){
    dataset[[new_var_name]] <- ((dataset[[male_var]] * dataset[[male_pop]]) + (dataset[[female_var]] * dataset[[female_pop]]))
    return(dataset)
}


### Function to merge all datasets together into a master dataset
master_fun <- function(year) {
    acs_master <- merge(ACS_veteran[[year]], ACS_industry[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_immigrant[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_sex[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_educ[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_marital[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_com[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_foster[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_sex_age[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_race[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_median_age[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_enroll[[year]], by = c('cbsa', 'name'), all = TRUE)
    acs_master <- merge(acs_master, ACS_poverty[[year]], by = c('cbsa', 'name'), all = TRUE)
        acs_master <- merge(acs_master, ACS_employment[[year]], by = c('cbsa', 'name'), all = TRUE)

    
    return(acs_master)
}


### Function to create totals variables given a list of data sets
get_total <- function(dataset_list, var_list, name_list){
    for( i in 1:length(var_list)){
        dataset_list <- lapply(dataset_list, function(x) add_up(x, var_list[[i]], name_list[i]))
    }
    return(dataset_list)
}


### Function to apply division_fun across a list of inputs
proportion_list_fun <- function(dataset_list, num_list, denom_list, new_var_name_list ){
    for(i in 1:length(num_list)){
        dataset_list <- lapply(dataset_list, function(x) division_fun(x, num_list[i], denom_list[i], new_var_name_list[i]))
    }
    return(dataset_list)
}


### Function to check level of a dataset
isVarAtThisLevel <- function(dataset, varinquotes) {
    isVarAtThisLevel_yn <- nrow(as.data.frame(unique(dataset[, varinquotes]))) == nrow(dataset)
    return(isVarAtThisLevel_yn)
}


### Function to rename variables using data dictionary
rename_fun <- function(dataset, varlist, data_dict) {
    for(i in varlist) {
        names(dataset)[which(names(dataset) == i)] <- data_dict[['label']][data_dict$var %in% i]
    }
    return(dataset)
}


### Function to remove bad punctuation from variable names
removeBadPunc <- function(dataset) {
  colnames(dataset) <- gsub("!!", " ", gsub("/", "_", gsub(":", "", gsub(" ", "_", gsub("\\.","_",colnames(dataset))))))
  return(dataset)    
}



######################################################################################################
######################################################################################################
########################## Start pulling together all of our data sets ###############################
######################################################################################################
######################################################################################################


#############################################################
# Import ACS Summary File (data dictionary)
#############################################################

# Grab the raw data dictionary. We're going to need to reformat
ACS_url <- ('http://api.census.gov/data/2014/acs1/variables.json')
ACS_sum <- fromJSON(ACS_url)

# We have the data dictionary in json. Reshape and format as a data frame:

    ACS_sum_list <- unlist(ACS_sum$variables, recursive = TRUE, use.names = TRUE)
    # drop non-variables, turn into data frame
    ACS_sum_df <- (ACS_sum_list[!(grepl('for.', names(ACS_sum_list)) | grepl('in.', names(ACS_sum_list)))])
    ACS_sum_df<- data.frame(ACS_sum_df)
    # text parsing to get column names:
    ACS_sum_df$varname <- row.names(ACS_sum_df)
    ACS_sum_df$var <- strsplit(ACS_sum_df$varname, '[.]')
    
    # Get list of value descriptions (use to reshape data from long to wide eventually)
    ACS_var <- unlist(ACS_sum_df$var)
    index <- seq(1, length(ACS_var), 2)
    ACS_var <- ACS_var[index]
    
    # Use our list to fill in var column
    ACS_sum_df$var <- ACS_var
    
    # remove the rows that describe the data type (they're all ints) 
    ACS_sum_df <- ACS_sum_df[grepl('predicate',ACS_sum_df$varname, ignore.case = TRUE)==FALSE, ]
    
    # long dataset: 'label' and 'concept' variable are in alternating rows
        # get label variable
        index <- seq(1,nrow(ACS_sum_df), 2)
        ACS_label <- ACS_sum_df[index, ]
        ACS_label$varname <- NULL
        ACS_label$label <- ACS_label$ACS_sum_df
        ACS_label$ACS_sum_df <- NULL
        # get concept variable
        index <- seq(2, nrow(ACS_sum_df), 2)
        ACS_concept <- ACS_sum_df[index, ]
        ACS_concept$varname <- NULL
        ACS_concept$concept <- ACS_concept$ACS_sum_df
        ACS_concept$ACS_sum_df <- NULL

        # put concept and label variables together in one dataset 
        ACS_SUMMARY <- merge(ACS_label, ACS_concept)

# acs summary file with all variables as characters
acs_summary_string <- data.frame(lapply(ACS_SUMMARY, as.character), stringsAsFactors=FALSE)

saveRDS(ACS_SUMMARY, paste0(workingDatasets, "ACS/ACS 2014 Data Dictionary.RDS"))

#remove stragglers
rm(ACS_concept)
rm(ACS_label)
rm(ACS_sum_df)



###############################################################################
# Population Data
###############################################################################

# Grab crosswalk to help us translate CBSA FIPS numbers to CBSA names 
    crosswalk <- read.csv(paste0(rawDatasets, '2015-11-18 -- County MSA Crosswalk/crosswalk_cbsa_fips_county.csv'), stringsAsFactors = FALSE, colClasses = 'character', header = TRUE, skip = 2)
    crosswalk <- crosswalk[, c('CBSA.Code', 'CBSA.Title', 'FIPS.State.Code', 'FIPS.County.Code', 'County.County.Equivalent')]

# Grab csv with population information
    county_pop <- read.csv(paste0(rawDatasets, '2015-12-01 -- County Population/Census/CO-EST2014-alldata.csv'), stringsAsFactors = FALSE, colClasses = c('STATE' = 'character', 'COUNTY' = 'character'))
    
    # keep only relevant variables in county_pop
    county_pop <- county_pop[c('STATE', 'COUNTY', 'STNAME', 'CTYNAME', 'POPESTIMATE2010', 'POPESTIMATE2011', 'POPESTIMATE2012', 'POPESTIMATE2013', 'POPESTIMATE2014' )]
    
    # save county level dataset
    saveRDS(county_pop, file = paste0(workingDatasets, 'population/population_county.RDS'))

# Format county data
    county_pop <- readRDS(paste0(workingDatasets, 'population/population_county.RDS'))
    
    # This file has county total and state totals. Remove state totals -- we want just county-level data
    nrow(county_pop) # 3193
    # State totals are coded with the County ID == 000. Remove them
    county_pop <- county_pop[county_pop$COUNTY != '000', ]
    nrow(county_pop) # 3142
    # We drop 51 -- one for each state plus Washington DC
    
# Crosswalk file has some rows up top that are administrative/descriptive. Remove them
    crosswalk <- crosswalk[crosswalk$FIPS.State.Code != '', ]
    
# Merge
pop <- merge(county_pop, crosswalk, by.x = c('STATE', 'COUNTY'), by.y = c('FIPS.State.Code', 'FIPS.County.Code'), all = TRUE)

# validate merge:
nrow(county_pop) # 3142 num counties in us
nrow(crosswalk) # 1882 num counties in cbsa's
nrow(pop) # 3217 this is num counties in us + 75 in crosswalk but not in county_pop

# 75 mismatches (per year) are all PR + Bedford City (the city that lost independence):
nrow(pop[is.na(pop$POPESTIMATE2010), ]) # 75
nrow(pop[is.na(pop$POPESTIMATE2011), ]) # 75
nrow(pop[is.na(pop$POPESTIMATE2012), ]) # 75
nrow(pop[is.na(pop$POPESTIMATE2013), ]) # 75
nrow(pop[is.na(pop$POPESTIMATE2014), ]) # 75
nrow(pop[is.na(pop$CBSA.Code), ]) # 1335

# Aggregate using data.table
pop_dt <- as.data.table(pop)

# Population at cbsa level (aggregated) 
cbsa_pop <- pop_dt[, list('POPESTIMATE2010' = sum(POPESTIMATE2010), 'POPESTIMATE2011' = sum(POPESTIMATE2011), 'POPESTIMATE2012' = sum(POPESTIMATE2012), 'POPESTIMATE2013' = sum(POPESTIMATE2013), 'POPESTIMATE2014' = sum(POPESTIMATE2014)), by=CBSA.Code]

# check cbsa_pop level
length(unique(cbsa_pop$CBSA.Code)) # 930 = 929 cbsa's + 1 extra (NA -- estimated number of people not in a CBSA?)
cbsa_pop <- cbsa_pop[!is.na(cbsa_pop$CBSA.Code), ]
length(unique(cbsa_pop$CBSA.Code)) # 929 cbsa's
nrow(cbsa_pop) == length(unique(cbsa_pop$CBSA.Code)) # TRUE

# Make sure no crazy min or max pop values
summary(cbsa_pop$POPESTIMATE2012)

# save cbsa level dataset
write.csv(cbsa_pop, file = paste0(workingDatasets, 'population/population_cbsa.csv'))
saveRDS(cbsa_pop, file = paste0(workingDatasets, 'population/population_cbsa.RDS'))



################################################################################
# Sex ratio data
################################################################################

sex_vars <- c('B01001_001E','B01001_002E','B01001_026E')
ACS_SUMMARY[ACS_SUMMARY$var %in% sex_vars, ]

# Get sex data for 2012 - 2014
years <- c('2012', '2013', '2014')
ACS_sex <- get_years(sex_vars, years)
sapply(ACS_sex, ncol)  # 6 for all years

# Drop variables that are more than 10% null
ACS_sex_complete <- lapply(ACS_sex, function(x) get_avail(x))
sapply(ACS_sex, ncol) == sapply(ACS_sex_complete, ncol) # 6 for all years (none dropped)
rm(ACS_sex_complete)

# Create proportion variables 
ACS_sex <- lapply(ACS_sex, function(x) division_fun(x, num = 'B01001_002E', denom = 'B01001_001E', 'sex_male_prop'))
ACS_sex <- lapply(ACS_sex, function(x) division_fun(x, num = 'B01001_026E', denom = 'B01001_001E', 'sex_female_prop'))

# Validate that proportion of pop that is male + proportion of pop that is female sums to 1:
ACS_sex_1 <- ACS_sex[[1]]
lapply(ACS_sex, FUN = function(x) all(x$sex_male_prop + x$sex_female_prop == 1))

# Rename variables
ACS_sex <- lapply(ACS_sex, function(x) rename(x, replace = c('B01001_001E' = 'sex_total_pop', 'B01001_002E' = 'sex_male', 'B01001_026E' = 'sex_female')))


################################################################################
# Sex by educational attainment data: 
#   Hypothesis: Prices correlate with women's educational attainment but not male's
################################################################################

# Have to pull from API in two different batches because API only lets you pull up to 50 vars at a time
    
    # List of variables we want from the first API call
    educ_var1 <- sapply(4:9, function(x) paste0('B15001_00', x, 'E'))
    educ_var1 <- c(educ_var1, 'B15001_010E')
    educ_var1 <- c(educ_var1, 'B15001_001E')
    educ_var2 <- sapply(12:18, function(x) paste0('B15001_0', x, 'E'))
    educ_var3 <- sapply(20:26, function(x) paste0('B15001_0', x, 'E'))
    educ_var4 <- sapply(28:34, function(x) paste0('B15001_0', x, 'E'))
    educ_var_a <- c(educ_var1, educ_var2, educ_var3, educ_var4)
    educ_var_a <- paste(educ_var_a, collapse = ',')
    ACS_educ1 <- get_years(educ_var_a, years)
    
    # List of variables we want from the second API call
    educ_var5 <- sapply(36:42, function(x) paste0('B15001_0', x, 'E'))
    educ_var6 <- sapply(45:51, function(x) paste0('B15001_0', x, 'E'))
    educ_var7 <- sapply(53:59, function(x) paste0('B15001_0', x, 'E'))
    educ_var8 <- sapply(61:67, function(x) paste0('B15001_0', x, 'E'))
    educ_var9 <- sapply(69:75, function(x) paste0('B15001_0', x, 'E'))
    educ_var10 <- sapply(77:83, function(x) paste0('B15001_0', x, 'E'))
    educ_var_b <- c(educ_var5, educ_var6, educ_var7, educ_var8, educ_var9, educ_var10)
    educ_var_b <- paste(educ_var_b, collapse = ',')
    ACS_educ2 <- get_years(educ_var_b, years)
    
# Create a data frame list that combines batches 1 and 2
ACS_educ_12 <- merge(ACS_educ1[[1]], ACS_educ2[[1]], by = c('cbsa', 'name'))
ACS_educ_13 <- merge(ACS_educ1[[2]], ACS_educ2[[2]], by = c('cbsa', 'name'))
ACS_educ_14 <- merge(ACS_educ1[[3]], ACS_educ2[[3]], by = c('cbsa', 'name'))
    
# Here's our education data frame:
ACS_educ <- list(ACS_educ_12, ACS_educ_13, ACS_educ_14)

# aggregate variables
varlist <- list(c('B15001_004E', 'B15001_012E', 'B15001_020E', 'B15001_028E', 'B15001_036E'),
                c('B15001_005E', 'B15001_013E', 'B15001_021E', 'B15001_029E', 'B15001_037E'),
                c('B15001_006E', 'B15001_014E', 'B15001_022E', 'B15001_030E', 'B15001_038E'),
                c('B15001_007E', 'B15001_015E', 'B15001_023E', 'B15001_031E', 'B15001_039E'),
                c('B15001_008E', 'B15001_016E', 'B15001_024E', 'B15001_032E', 'B15001_040E'),
                c('B15001_009E', 'B15001_017E', 'B15001_025E', 'B15001_033E', 'B15001_041E'),
                c('B15001_010E', 'B15001_018E', 'B15001_026E', 'B15001_034E', 'B15001_042E'),
                c('B15001_045E', 'B15001_053E', 'B15001_061E', 'B15001_069E', 'B15001_077E'),
                c('B15001_046E', 'B15001_054E', 'B15001_062E', 'B15001_070E', 'B15001_078E'),
                c('B15001_047E', 'B15001_055E', 'B15001_063E', 'B15001_071E', 'B15001_079E'),
                c('B15001_048E', 'B15001_056E', 'B15001_064E', 'B15001_072E', 'B15001_080E'),
                c('B15001_049E', 'B15001_057E', 'B15001_065E', 'B15001_073E', 'B15001_081E'),
                c('B15001_050E', 'B15001_058E', 'B15001_066E', 'B15001_074E', 'B15001_082E'),
                c('B15001_051E', 'B15001_059E', 'B15001_067E', 'B15001_075E' , 'B15001_083E'), 
                c('B15001_045E', 'B15001_046E'))

namelist <- c('educ_male_less_hs', 'educ_male_some_hs', 'educ_male_hs_grad', 'educ_male_some_col', 
              'educ_male_col_ass', 'educ_male_col_bac', 'educ_male_grad_deg','educ_female_less_hs', 
              'educ_female_some_hs', 'educ_female_hs_grad', 'educ_female_some_col', 
              'educ_female_col_ass', 'educ_female_col_bac', 'educ_female_grad_deg', 'educ_female_18_24_no_hs_deg')

ACS_educ <- get_total(ACS_educ, varlist, namelist)


# Check % null
sum(sapply(1:length(names(ACS_educ[[2]])), function(x) sum(ACS_educ[[2]][, x]=='null')<(0.1*nrow(ACS_educ[[2]])))) # 90 all 3
length(names(ACS_educ[[2]])) # 90 all 3

# create proportion variables
numlist <- c('educ_male_less_hs', 'educ_male_some_hs', 'educ_male_hs_grad', 'educ_male_some_col', 
             'educ_male_col_ass', 'educ_male_col_bac', 'educ_male_grad_deg','educ_female_less_hs', 
             'educ_female_some_hs', 'educ_female_hs_grad', 'educ_female_some_col', 
             'educ_female_col_ass', 'educ_female_col_bac', 'educ_female_grad_deg', 'educ_female_18_24_no_hs_deg')

denomlist <- rep('B15001_001E', length(numlist))

newnamelist <-  c('educ_male_less_hs_prop', 'educ_male_some_hs_prop', 'educ_male_hs_grad_prop', 'educ_male_some_col_prop', 
                  'educ_male_col_ass_prop', 'educ_male_col_bac_prop', 'educ_male_grad_deg_prop','educ_female_less_hs_prop', 
                  'educ_female_some_hs_prop', 'educ_female_hs_grad_prop', 'educ_female_some_col_prop', 
                  'educ_female_col_ass_prop', 'educ_female_col_bac_prop', 'educ_female_grad_deg_prop', 'educ_female_18_24_no_hs_deg_prop')

ACS_educ <- proportion_list_fun(ACS_educ, numlist, denomlist, newnamelist)

# rename total population variable
ACS_educ <- lapply(ACS_educ, function(x) rename(x, replace = c('B15001_001E' = 'educ_total_pop')))

# drop unneeded variables
ACS_educ <- lapply(ACS_educ, function(x) drop_vars(x, 'B15001_'))

# aggregate different levels of educational attainment for use in our model 
ACS_educ <- lapply(ACS_educ, function(x) add_up(x, c('educ_male_less_hs_prop', 'educ_male_some_hs_prop'), 'educ_male_agg_no_hs_prop'))
ACS_educ <- lapply(ACS_educ, function(x) add_up(x, c('educ_male_hs_grad_prop', 'educ_male_some_col_prop'), 'educ_male_agg_hs_deg_prop'))
ACS_educ <- lapply(ACS_educ, function(x) add_up(x, c('educ_male_col_ass_prop', 'educ_male_col_bac_prop'), 'educ_male_agg_col_deg_prop'))
ACS_educ <- lapply(ACS_educ, function(x) add_up(x, 'educ_male_grad_deg_prop', 'educ_male_agg_grad_deg_prop'))
ACS_educ <- lapply(ACS_educ, function(x) add_up(x, c('educ_female_less_hs_prop', 'educ_female_some_hs_prop'), 'educ_female_agg_no_hs_prop'))
ACS_educ <- lapply(ACS_educ, function(x) add_up(x, c('educ_female_hs_grad_prop', 'educ_female_some_col_prop'), 'educ_female_agg_hs_deg_prop'))
ACS_educ <- lapply(ACS_educ, function(x) add_up(x, c('educ_female_col_ass_prop', 'educ_female_col_bac_prop'), 'educ_female_agg_col_deg_prop'))
ACS_educ <- lapply(ACS_educ, function(x) add_up(x, 'educ_female_grad_deg_prop', 'educ_female_agg_grad_deg_prop'))



################################################################################
# Commute time variables: 
#   Hypothesis: longer commute --> higher price for outcall
################################################################################

# Get data
years <- c('2012','2013','2014')
acs_vars1 <- sapply(1:9, function(x) paste0('B08303_00', x, 'E'))
acs_vars2 <- sapply(10:13, function(x) paste0('B08303_0', x, 'E'))
acs_vars <- paste(c(acs_vars1, acs_vars2), collapse = ',')
ACS_com <- get_years(acs_vars, years)

# Check % null
sum(is.na(ACS_com[[1]]$B08303_001E)) # No nulls

# Create avergage commute time variables (center * proportion of people in that range)
# Apply commute variable creation function to all years
ACS_com <- lapply(ACS_com, function(x) create_avg_com(x))

# Keep only average commute variable
ACS_com <-  lapply(ACS_com, function(x) drop_vars(x, 'B08303_'))


################################################################################
# Foster children variable
#  Hypothesis: Foster children more likely to be trafficking victims
################################################################################

# Get data
years <- c('2012', '2013', '2014')
foster_vars <- c('B09018_008E', 'B09018_001E')
foster_vars <- paste(foster_vars, collapse = ',')
ACS_foster <- get_years(foster_vars, years)

# Rename variables
ACS_foster <- lapply(ACS_foster, function(x) rename(x, replace = c('B09018_008E' = 'foster_total', 'B09018_001E' = 'foster_total_pop')))

# Create proportion variable
ACS_foster <- lapply(ACS_foster, function(x) division_fun(x, num = 'foster_total', denom = 'foster_total_pop', 'foster_kid_prop'))

# Check availability
length(names(ACS_foster[[1]])) # 6 for all
sum(sapply(1:length(names(ACS_foster[[1]])), function(x) sum(is.na(ACS_foster[[1]][, x]))<(0.1*nrow(ACS_foster[[1]])))) # 6 for all



################################################################################
# Bring in enrollment variables from summary file
#  Hypothesis: College towns are probably different from other towns
################################################################################

# Get data
years <- c('2012', '2013', '2014')
enrollment_variables <- c('B14002_019E', 'B14002_043E')
enrollment_denoms <- c('B14002_002E','B14002_026E')
all_enrollment_vars <- c(enrollment_variables, enrollment_denoms)

# Get variables from ACS website
ACS_enroll <- get_years(c(enrollment_variables, enrollment_denoms), c('2012','2013','2014'))
sapply(ACS_enroll, ncol) # 7

# keep only variables with enough representation
ACS_enroll_complete <- lapply(ACS_enroll, function(x) get_avail(x))
sapply(ACS_enroll_complete, ncol) == sapply(ACS_enroll, ncol)
#all variables kept
ACS_enroll <- ACS_enroll_complete
rm(ACS_enroll_complete)


# Create proportions for each enrollment bracket
    # numerators
    numlist <- enrollment_variables
    # denominators
    denomlist <- enrollment_denoms
    # new variable names
    newvarnamelist <- c('enrolled_male_prop','enrolled_female_prop')

# create proportion variables
ACS_enroll <- proportion_list_fun(ACS_enroll, num_list = numlist, denom_list = denomlist, new_var_name_list = newvarnamelist)

# Rename variables
    ACS_enroll <- lapply(ACS_enroll, function(x) rename_fun(x, all_enrollment_vars, acs_summary_string))
    # remove !! from variable names
    ACS_enroll <- lapply(ACS_enroll, function(x) removeBadPunc(x))
    # remove spaces that the "!!" left
    ACS_enroll <- lapply(ACS_enroll, function(x) removeBadPunc(x))
    # rename male and female
    ACS_enroll <- lapply(ACS_enroll, function(x) rename(x, replace = c('Male' = 'Male_enroll', 'Female' = 'Female_enroll')))


################################################################################
# Poverty variables
################################################################################

poverty_vars <- c('B17001_001E', 'B17001_002E', 'B06011_001E')

# Get vars from API
years <- c('2012', '2013', '2014')
ACS_poverty <- get_years(poverty_vars, years)
sapply(ACS_poverty, ncol) # 5

# keep only variables with enough representation
ACS_poverty_complete <- lapply(ACS_poverty, function(x) get_avail(x))
sapply(ACS_poverty_complete, ncol) == sapply(ACS_poverty, ncol)
#all variables kept
ACS_poverty <- ACS_poverty_complete
rm(ACS_poverty_complete)


# Rename variables
ACS_poverty <- lapply(ACS_poverty, function(x) rename_fun(x, poverty_vars, acs_summary_string))

# Rename the "Total" variable manually
ACS_poverty <- lapply(ACS_poverty, function(x) rename(x, c("Total:" = "poverty_pop_total")))


# remove !! from variable names
ACS_poverty <- lapply(ACS_poverty, function(x) removeBadPunc(x))
# remove spaces that the "!!" left
ACS_poverty <- lapply(ACS_poverty, function(x) removeBadPunc(x))


################################################################################
# Median age for each CBSA
################################################################################

# Select variables
median_age_vars <- c('B01002_001E','B01002_002E','B01002_003E')
ACS_SUMMARY[ACS_SUMMARY$var %in% median_age_vars, ]

# Get data from API
years <- c('2012', '2013', '2014')
ACS_median_age <- get_years(median_age_vars, years)
sapply(ACS_median_age, ncol)  # 6 for all years

# drop variables that are more than 10% null
ACS_median_age_complete <- lapply(ACS_median_age, function(x) get_avail(x))
sapply(ACS_median_age, ncol) == sapply(ACS_median_age_complete, ncol) # 6 for all years (none dropped)
rm(ACS_median_age_complete)

# rename variables
ACS_median_age <- lapply(ACS_median_age, function(x) rename(x, replace = c('B01002_001E' = 'med_age_tot', 'B01002_002E' = 'med_age_male', 'B01002_003E' = 'med_age_female' )))


######################################################################################################
######################################################################################################
########################## Additional data that we don't end up using ################################
######################################################################################################
######################################################################################################

###############################################################################
# Veteran data 
#  Hypothesis: Vets more likely to buy prostitutes
###############################################################################

# Get data from API
years <- c('2012', '2013', '2014')
veteran_variables <- c('B21001_001E', 'B21001_005E')
ACS_SUMMARY[ACS_SUMMARY$var %in% veteran_variables, ]


ACS_veteran <- get_years(veteran_variables, years)

# check availability
lapply(ACS_veteran, function(x) sum(!is.na(x$B21001_001E)))
# 530 for 2012, 515 for 2013, 2014
lapply(ACS_veteran, function(x) sum(!is.na(x$B21001_001E))/nrow(x))
#all not missing

# create proportion variable
ACS_veteran <- lapply(ACS_veteran, function(x) 
  division_fun(x, num = 'B21001_005E', denom = 'B21001_001E', 'vet_male_prop'))

# rename variables
ACS_veteran <- lapply(ACS_veteran, function(x) rename(x, replace = c('B21001_001E' = 'vet_total_pop', 'B21001_005E' = 'vet_male')))



################################################################################
# Industry data
# presence of more male-dominated industries could drive demand up
################################################################################

# Identify variables
length(ACS_SUMMARY$var[grepl('C24030', ACS_SUMMARY$var) & grepl('E', ACS_SUMMARY$var)])
years <- c('2012', '2013', '2014')

# Get male and female separately as API get request is limited to 50 variables
    # Identify male vars
    industry_male_variables <- ACS_SUMMARY$var[grepl('C24030', ACS_SUMMARY$var)
                                               & grepl('E', ACS_SUMMARY$var) 
                                               & (grepl('Male', ACS_SUMMARY$label) | grepl('Total', ACS_SUMMARY$label)) ]
    
    ACS_industry_male <- get_years(industry_male_variables, years)
    sapply(ACS_industry_male, ncol) # 31
    
    
    # Identify female vars (exclude total count)
    industry_female_variables <- ACS_SUMMARY$var[grepl('C24030', ACS_SUMMARY$var)
                                               & grepl('E', ACS_SUMMARY$var) 
                                               & (grepl('Female', ACS_SUMMARY$label)) ]
    ACS_industry_female <- get_years(industry_female_variables, years)
    sapply(ACS_industry_female, ncol) # 30 (No total in this one)
    
    # merge male and female data frames, and then drop the individual ones
    ACS_industry_12 <- merge(ACS_industry_male[[1]], ACS_industry_female[[1]][, colnames(ACS_industry_female[[1]]) != 'name'], by = c('cbsa', 'year'))
    ACS_industry_13 <- merge(ACS_industry_male[[2]], ACS_industry_female[[2]][, colnames(ACS_industry_female[[2]]) != 'name'], by = c('cbsa', 'year'))
    ACS_industry_14 <- merge(ACS_industry_male[[3]], ACS_industry_female[[3]][, colnames(ACS_industry_female[[3]]) != 'name'], by = c('cbsa', 'year'))

    rm(ACS_industry_female)
    rm(ACS_industry_male)

ACS_industry <- list('industry_2012' = ACS_industry_12, 'industry_2013' = ACS_industry_13, 'industry_2014' = ACS_industry_14)

# keep only variables with enough representation
sapply(ACS_industry, ncol)
ACS_industry_complete <- lapply(ACS_industry, function(x) get_avail(x))
sapply(ACS_industry_complete, ncol) == sapply(ACS_industry, ncol)
# all variables kept
ACS_industry <- ACS_industry_complete
rm(ACS_industry_complete)

# rename variables
ACS_industry <- lapply(ACS_industry, function(x) rename(x, c('C24030_001E' = 'industry_total_pop', 
                                                             'C24030_003E' = 'industry_male_agriculture', 
                                                             'C24030_006E' = 'industry_male_construction',
                                                             'C24030_007E' = 'industry_male_manufacturing',
                                                             'C24030_008E' = 'industry_male_wholesale',
                                                             'C24030_009E' = 'industry_male_retail',
                                                             'C24030_010E' = 'industry_male_transportation',
                                                             'C24030_013E' = 'industry_male_information',
                                                             'C24030_014E' = 'industry_male_finance',
                                                             'C24030_017E' = 'industry_male_professional',
                                                             'C24030_021E' = 'industry_male_education',
                                                             'C24030_024E' = 'industry_male_arts',
                                                             'C24030_027E' = 'industry_male_other',
                                                             'C24030_028E' = 'industry_male_public',
                                                             
                                                             'C24030_030E' = 'industry_female_agriculture', 
                                                             'C24030_033E' = 'industry_female_construction',
                                                             'C24030_034E' = 'industry_female_manufacturing',
                                                             'C24030_035E' = 'industry_female_wholesale',
                                                             'C24030_036E' = 'industry_female_retail',
                                                             'C24030_037E' = 'industry_female_transportation',
                                                             'C24030_040E' = 'industry_female_information',
                                                             'C24030_041E' = 'industry_female_finance',
                                                             'C24030_044E' = 'industry_female_professional',
                                                             'C24030_048E' = 'industry_female_education',
                                                             'C24030_051E' = 'industry_female_arts',
                                                             'C24030_054E' = 'industry_female_other',
                                                             'C24030_055E' = 'industry_female_public'
                                                             
                                                             )))


# create total for each industry
varlist <- list(c('industry_male_agriculture', 'industry_female_agriculture'), c('industry_male_construction', 'industry_female_construction'),
                c('industry_male_manufacturing', 'industry_female_manufacturing'), c('industry_male_wholesale', 'industry_female_wholesale'),
                c('industry_male_retail', 'industry_female_retail'),  c('industry_male_transportation', 'industry_female_transportation'),
                c('industry_male_information', 'industry_female_information'), c('industry_male_finance', 'industry_female_finance'),
                c('industry_male_professional', 'industry_female_professional'), c('industry_male_education', 'industry_female_education'),
                c('industry_male_arts', 'industry_female_arts'), c('industry_male_other', 'industry_female_other'), 
                c('industry_male_public', 'industry_female_public'))

namelist <- c('industry_total_agriculture', 'industry_total_construction', 'industry_total_manufacturing','industry_total_wholesale',
              'industry_total_retail', 'industry_total_transportation', 'industry_total_information', 'industry_total_finance', 
              'industry_total_professional','industry_total_eductaion', 'industry_total_arts', 'industry_total_other', 'industry_total_public')
             
length(varlist) == length(namelist)
ACS_industry <- get_total(ACS_industry, varlist, namelist)


# create proportion variables
numlist <- c('industry_male_agriculture', 'industry_male_construction', 'industry_male_manufacturing', 'industry_male_wholesale', 
             'industry_male_retail', 'industry_male_transportation', 'industry_male_information', 'industry_male_finance', 
             'industry_male_professional', 'industry_male_education', 'industry_male_arts', 'industry_male_other', 'industry_male_public')

denomlist <- namelist
length(numlist) == length(denomlist)
newnamelist <-  c('industry_male_agriculture_prop', 'industry_male_construction_prop', 'industry_male_manufacturing_prop', 
                  'industry_male_wholesale_prop', 'industry_male_retail_prop', 'industry_male_transportation_prop', 
                  'industry_male_information_prop', 'industry_male_finance_prop', 'industry_male_professional_prop', 
                  'industry_male_education_prop', 'industry_male_arts_prop', 'industry_male_other_prop', 'industry_male_public_prop') 
length(numlist) == length(newnamelist)

ACS_industry <- proportion_list_fun(ACS_industry, numlist, denomlist, newnamelist)

# Merge in population counts 
ACS_industry <- lapply(ACS_industry, function(x) merge(x, cbsa_pop, by.x = 'cbsa', by.y = 'CBSA.Code', all = TRUE))
nrow(ACS_industry[[1]]) # 961, because there were some cbsa's that existed in 2012 that no longer exist (and do not exist in the crosswalk)
nrow(ACS_industry[[2]]) # 929 -- number of cbsa's in the US
nrow(ACS_industry[[3]]) # 929 -- number of cbsa's in the US

# let's drop the cbsa's that once existed (in 2012) but are no longer with us
ACS_industry[[1]] <- ACS_industry[[1]][is.na(ACS_industry[[1]]$POPESTIMATE2012) == FALSE,]
nrow(ACS_industry[[1]]) # 916 -- 45 cbsa's seem to have disappeared after 2012

# validate merge:
nrow(cbsa_pop) # 929 cbsa's in US

# Caution: We bring in Popestimates for all years into each data set. The below code is us manually dropping, e.g., popestimate2014 from the 2012 dataset
# update population estimate name
ACS_industry[[1]] <- ACS_industry[[1]][, !(names(ACS_industry[[1]]) %in% c('POPESTIMATE2013', 'POPESTIMATE2014'))]
ACS_industry[[2]] <- ACS_industry[[2]][, !(names(ACS_industry[[2]]) %in% c('POPESTIMATE2012', 'POPESTIMATE2014'))]
ACS_industry[[3]] <- ACS_industry[[3]][, !(names(ACS_industry[[3]]) %in% c('POPESTIMATE2012', 'POPESTIMATE2013'))]

ACS_industry[[1]]$POPESTIMATE <- ACS_industry[[1]]$POPESTIMATE2012
ACS_industry[[2]]$POPESTIMATE <- ACS_industry[[2]]$POPESTIMATE2013
ACS_industry[[3]]$POPESTIMATE <- ACS_industry[[3]]$POPESTIMATE2014

ACS_industry[[1]]$POPESTIMATE2012 <- NULL
ACS_industry[[2]]$POPESTIMATE2013 <- NULL
ACS_industry[[3]]$POPESTIMATE2014 <- NULL


# set threshold for which we consider an industry male-dominated: 66%
threshold <- 0.66

# create function to calculate proportion of people in a CBSA who are men that work in a male-dominated industry
male_dom_fun <- function(dataset, threshold) {
  # get rid of unused columns
  extra_columns <- grep('C24030', names(dataset))
  dataset <- dataset[, -extra_columns]
  
  # get names of proportion columns
  prop_vars <- colnames(dataset)[grepl('_prop', names(dataset))]
                        
  
  #Calculate national unweighted means of proportion of men in an industry
  ind_unwt_means <- colMeans(dataset[, prop_vars], na.rm = TRUE)
  print(ind_unwt_means)
  #Assign this to global environment so we can check it out later 
  assign('indust_male_prop_unweighted_mean', ind_unwt_means, envir = globalenv())
 
  # Calculate national weighted means of proportion of men in an industry
    
    # Get a list of all of the proportion variables in the industry data set
    prop_vars <- colnames(dataset)[grepl('_prop', names(dataset))]
    # Make an empty DF with as many columns as there are prop_vars
    ind_wt_means = data.frame(matrix(ncol = length(prop_vars)))
    # Name the columns after the prop_vars
    colnames(ind_wt_means) = prop_vars
    
    # for each var in prop_list, calculate the weighted mean of the male/female ratio
    for (i in prop_vars) {
      ind_wt_means[[which(prop_vars == i)]] <- sum(dataset[i]*dataset$POPESTIMATE/sum(dataset$POPESTIMATE[!is.na(dataset$industry_total_pop)], na.rm = TRUE), na.rm = TRUE)
    }
    
    #Assign this to global environment so we can check it out later 
    assign('indust_male_prop_weighted_mean', ind_wt_means, envir = globalenv())
    
  # decide which industries are male-dominated
  male_dom_prop_columns <- names(ind_wt_means)[ind_wt_means > threshold]
  male_dom_columns <- gsub('_prop', '', male_dom_prop_columns)
  
  # add up total num men in male dominated industries
  dataset <- add_up(dataset, male_dom_columns, 'industry_male_dom_tot') 
  
  # create proportion variable
  dataset <- division_fun(dataset, num = 'industry_male_dom_tot', denom = 'industry_total_pop', 'industry_male_dom_prop')
  
  return(dataset)
}

# create variable: proportion of people who are men working in a male-dominated industry
ACS_industry <- lapply(ACS_industry, function(x) male_dom_fun(x, threshold))


################################################################################
# Immigrant inflow data
#   sex by place of birth: aggregate place of birth by continent: 
#       - influx of immigration could result in influx of trafficking
#       - influx of ads w/out influx of official immigration could indicate 
#         trafficking
################################################################################

# Get data from API
years <- c('2012', '2013', '2014')
immigrant_variables <- c('B05002_001E','B05002_002E','B05002_014E','B05002_015E')
ACS_SUMMARY[ACS_SUMMARY$var %in% immigrant_variables, ]

ACS_immigrant <- get_years(immigrant_variables, years)

# drop variables that are more than 10% null
sapply(ACS_immigrant, ncol)
ACS_immigrant_complete <- lapply(ACS_immigrant, function(x) get_avail(x))
sapply(ACS_immigrant_complete, ncol) == sapply(ACS_immigrant, ncol)

ACS_immigrant <- ACS_immigrant_complete
rm(ACS_immigrant_complete)

# create proportion variables
ACS_immigrant <- lapply(ACS_immigrant, function(x)
  division_fun(x, num =  'B05002_002E', denom =  'B05002_001E', 'immigrant_native_prop'))
head(ACS_immigrant[[3]])

ACS_immigrant <- lapply(ACS_immigrant, function(x)
  division_fun(x, num = 'B05002_014E', denom = 'B05002_001E', 'immigrant_naturalized_prop'))

ACS_immigrant <- lapply(ACS_immigrant, function(x)
  division_fun(x, num = 'B05002_015E', denom = 'B05002_001E', 'immigrant_not_cit_prop'))


lapply(ACS_immigrant, FUN = function(x) lapply(x, FUN = function(y) sum(is.na(y))))
# 2012, 2013: 2na
# 2014: 1 na
# for all variables created

# rename variables:
ACS_immigrant <- lapply(ACS_immigrant, function(x) rename(x, replace = c('B05002_001E' = 'immigrant_total_pop', 'B05002_002E' = 'immigrant_native', 'B05002_014E' = 'immigrant_naturalized', 'B05002_015E' = 'immigrant_not_cit')))



################################################################################
# Sex by marital status: 
#   unmarried males: may drive demand up
#   unmarried females: more prostitution?
#   unmarried females 18-24: more young sex workers?
#   unmarried females 35-39: for the placebo reg?
################################################################################

# Get data from API
years <- c('2012', '2013', '2014')
vars_toget_1d <- c(1, 2, 3, 4, 9)
marital_vars1 <- sapply(vars_toget_1d, function(x) 
  paste0('B12001_00', x, 'E'))

vars_toget_2d <- c(10, 11, 12, 13, 18, 19)
marital_vars2 <- sapply(vars_toget_2d, function(x) 
  paste0('B12001_0', x, 'E'))

marital_vars3 <- c('B12002_098E', 'B12002_099E', 'B12002_102E', 'B12002_001E', 'B12002_114E', 'B12002_115E', 'B12002_118E')

marital_vars <- paste(c(marital_vars1, marital_vars2, marital_vars3), collapse = ',')
ACS_marital <- get_years(marital_vars, years)


# % null
length(names(ACS_marital[[3]])) # 13 for all ( 18 now)
sum(sapply(1:length(names(ACS_marital[[3]])), function(x) 
  sum(ACS_marital[[3]][, x]=='null')<(0.1*nrow(ACS_marital[[3]])))) # 13 for all (now NA?? is this a problem)


# create variables for:
# proportion never married, now married, widowed, divorced for both male and female
numlist <- c('B12001_003E', 'B12001_004E', 'B12001_009E', 'B12001_010E', 
             'B12001_012E', 'B12001_013E', 'B12001_018E', 'B12001_019E')

denomlist <- rep('B12001_001E', length(numlist))

newnamelist <- c('marital_male_never_married_prop', 'marital_male_now_married_prop', 
                 'marital_male_widowed_prop', 'marital_male_divorced_prop', 
                 'marital_female_never_married_prop', 'marital_female_now_married_prop', 
                 'marital_female_widowed_prop', 'marital_female_divorced_prop')

ACS_marital <- proportion_list_fun(ACS_marital, numlist, denomlist, newnamelist)

# create variables for:
# proportion 18-24 naver married, proportion 35-39 never married, proportion 18-24 married, proportion 35-39 married:
numlist <- c('B12002_098E', 'B12002_099E', 'B12002_102E', 'B12002_114E', 'B12002_115E', 'B12002_118E')

denomlist <- rep('B12002_001E', length(numlist))

newnamelist <- c('marital_female_single_18_19_prop', 'marital_female_single_20_24_prop', 'marital_female_single_35_39_prop', 'marital_female_married_18_19_prop', 'marital_female_married_20_24_prop', 'marital_female_married_35_39_prop')

ACS_marital <- proportion_list_fun(ACS_marital, numlist, denomlist, newnamelist)

# Could add in total from b12002 here: (but don't think we need it right now)
# rename variables
ACS_marital <- lapply(ACS_marital, function(x) rename(x, replace = c('B12001_001E' = 
                'marital_total_pop', 'B12001_002E' = 'marital_male', 'B12001_003E' = 
                'marital_male_never_married', 'B12001_004E' = 'marital_male_now_married',
                'B12001_009E' = 'marital_male_widowed', 'B12001_010E' = 'marital_male_divorced', 
                'B12001_011E' = 'marital_female', 'B12001_012E' = 'marital_female_never_married', 
                'B12001_013E' = 'marital_female_now_married', 'B12001_018E' = 'marital_female_widowed', 
                'B12001_019E' = 'marital_female_divorced' )))

ACS_marital <- lapply(ACS_marital, function(x) rename(x, replace = c('B12002_098E' = 'marital_female_single_18_19', 
                                                                     'B12002_099E' = 'marital_female_single_20_24', 
                                                                     'B12002_102E' = 'marital_female_single_35_39', 
                                                                     'B12002_114E' = 'marital_female_married_18_19',
                                                                     'B12002_115E' = 'marital_female_married_20_24', 
                                                                     'B12002_118E' = 'marital_female_married_35_39',
                                                                     'B12002_001E' = 'marital_by_age_total_pop')))

# add up 18&19 and 20-24
ACS_marital <- lapply(ACS_marital, function(x) add_up(x, c('marital_female_single_18_19_prop', 'marital_female_single_20_24_prop'), 'marital_female_single_18_24_prop'))
ACS_marital <- lapply(ACS_marital, function(x) add_up(x, c('marital_female_married_18_19_prop', 'marital_female_married_20_24_prop'), 'marital_female_married_18_24_prop'))


################################################################################
# Racial breakdown
################################################################################


# Identify variables
length(ACS_SUMMARY$var[grepl('B02001', ACS_SUMMARY$var) & grepl('E', ACS_SUMMARY$var)])
# bring in race variables from summary file
race_variables <- ACS_SUMMARY$var[grepl('B02001', ACS_SUMMARY$var)
                                           & grepl('E', ACS_SUMMARY$var) ]
# get vars from api
years <- c('2012', '2013', '2014')
ACS_race <- get_years(race_variables, years)
sapply(ACS_race, ncol) # 13

# keep only variables with enough representation
sapply(ACS_race, ncol)
ACS_race_complete <- lapply(ACS_race, function(x) get_avail(x))
sapply(ACS_race_complete, ncol) == sapply(ACS_race, ncol)
#all variables kept
ACS_race <- ACS_race_complete
rm(ACS_race_complete)

# change names
ACS_race <- lapply(ACS_race, function(x) rename_fun(x, race_variables, acs_summary_string ))

# remove !! from variable names
ACS_race <- lapply(ACS_race, function(x) removeBadPunc(x))

# create proportions for each race bracket
# numerators
numlist <- names(ACS_race[[1]])

# denominators
denomlist <- rep('Total', length(numlist))

# new variable names
newvarnamelist <- paste('Prop', numlist, sep = ' ')

# create proportion variables
ACS_race <- proportion_list_fun(ACS_race, num_list = numlist, denom_list = denomlist, new_var_name_list = newvarnamelist)

# drop unnecessary proportion variables
drop_vars <- function(dataset, search_term) {
  extra_columns <- grep(search_term, names(dataset))
  dataset <- dataset[, -extra_columns]
  return(dataset)
}

ACS_race <- lapply(ACS_race, function(x) drop_vars(x, 'Prop name'))
ACS_race <- lapply(ACS_race, function(x) drop_vars(x, 'Prop cbsa'))
ACS_race <- lapply(ACS_race, function(x) drop_vars(x, 'Prop year'))


# rename total for merging later
ACS_race[[1]]$race_total <- ACS_race[[1]]$Total
ACS_race[[1]]$Total <- NULL

ACS_race[[2]]$race_total <- ACS_race[[2]]$Total
ACS_race[[2]]$Total <- NULL

ACS_race[[3]]$race_total <- ACS_race[[3]]$Total
ACS_race[[3]]$Total <- NULL


################################################################################
# Unemployment
################################################################################

# bring in unemployment variables from summary file
# B23025_002E == In labor force
# B23025_005E == Unemployed

unemployment_variables <- c('B23025_002E', 'B23025_005E')
# get vars from api
years <- c('2012', '2013', '2014')
ACS_employment <- get_years(unemployment_variables, years)
sapply(ACS_employment, ncol) # 5

# keep only variables with enough representation
sapply(ACS_employment, ncol)
ACS_employment_complete <- lapply(ACS_employment, function(x) get_avail(x))
sapply(ACS_employment_complete, ncol) == sapply(ACS_employment, ncol)
#all variables kept
ACS_employment <- ACS_employment_complete
rm(ACS_employment_complete)

# Rename variables
    # change names from BXXXX_XXXE to the corresponding entry in ACS_SUMMARY
    ACS_employment <- lapply(ACS_employment, function(x) rename_fun(x, unemployment_variables, acs_summary_string ))
    # remove !! from variable names
    ACS_employment <- lapply(ACS_employment, function(x) removeBadPunc(x))
    # removeBadPunc replaces some chars with spaces. Run it again to replace those new spaces with underscores. 
    ACS_employment <- lapply(ACS_employment, function(x) removeBadPunc(x))
    # Rename unemployment
    ACS_employment[[1]]$unemployed <- ACS_employment[[1]]$In_labor_force_Civilian_labor_force_Unemployed
    ACS_employment[[1]]$In_labor_force_Civilian_labor_force_Unemployed <- NULL
    ACS_employment[[2]]$unemployed <- ACS_employment[[2]]$In_labor_force_Civilian_labor_force_Unemployed
    ACS_employment[[2]]$In_labor_force_Civilian_labor_force_Unemployed <- NULL
    ACS_employment[[3]]$unemployed <- ACS_employment[[3]]$In_labor_force_Civilian_labor_force_Unemployed
    ACS_employment[[3]]$In_labor_force_Civilian_labor_force_Unemployed <- NULL
    

# create proportions for each race bracket
# numerators
numlist <- 'unemployed'

# denominators
denomlist <- 'In_labor_force'

# new variable names
newvarnamelist <- paste('Prop', numlist, sep = '_')

# create proportion variables
ACS_employment <- proportion_list_fun(ACS_employment, num_list = numlist, denom_list = denomlist, new_var_name_list = newvarnamelist)

# rename total for merging later
ACS_employment[[1]]$prop_unemployed <- ACS_employment[[1]]$Total
ACS_employment[[1]]$Total <- NULL

ACS_employment[[2]]$race_total <- ACS_employment[[2]]$Total
ACS_employment[[2]]$Total <- NULL

ACS_employment[[3]]$race_total <- ACS_employment[[3]]$Total
ACS_employment[[3]]$Total <- NULL


################################################################################
# load in sex by age variable
#   for use in transforming our variables into ones that we can compare on a 
#   per capita basis
################################################################################

length(ACS_SUMMARY$var[grepl('B01001_', ACS_SUMMARY$var) & grepl('E', ACS_SUMMARY$var)])
# 49 variables needed

# Identify male vars and total
sex_age_male_variables <- ACS_SUMMARY$var[grepl('B01001_', ACS_SUMMARY$var)
                                           & grepl('E', ACS_SUMMARY$var) 
                                           & (grepl('Male', ACS_SUMMARY$label) | grepl('Total', ACS_SUMMARY$label)) ]

years <- c('2012', '2013', '2014')
ACS_sex_age_male <- get_years(sex_age_male_variables, years)
sapply(ACS_sex_age_male, ncol) # 28


# Identify female vars
sex_age_female_variables <- ACS_SUMMARY$var[grepl('B01001_', ACS_SUMMARY$var)
                                      & grepl('E', ACS_SUMMARY$var) 
                                      & grepl('Female', ACS_SUMMARY$label) ]

ACS_sex_age_female <- get_years(sex_age_female_variables, years)
sapply(ACS_sex_age_female, ncol) # 27


# merge male and female data frames, and then drop the individual ones 
ACS_sex_age_12 <- merge(ACS_sex_age_male[[1]], ACS_sex_age_female[[1]][, colnames(ACS_sex_age_female[[1]]) != 'name'], by = c('cbsa', 'year'))
ACS_sex_age_13 <- merge(ACS_sex_age_male[[2]], ACS_sex_age_female[[2]][, colnames(ACS_sex_age_female[[2]]) != 'name'], by = c('cbsa', 'year'))
ACS_sex_age_14 <- merge(ACS_sex_age_male[[3]], ACS_sex_age_female[[3]][, colnames(ACS_sex_age_female[[3]]) != 'name'], by = c('cbsa', 'year'))

rm(ACS_sex_age_female)
rm(ACS_sex_age_male)

ACS_sex_age <- list('sex_age_2012' = ACS_sex_age_12, 'sex_age_2013' = ACS_sex_age_13, 'sex_age_2014' = ACS_sex_age_14)

# keep only variables with enough representation
sapply(ACS_sex_age, ncol) # 52
ACS_sex_age_complete <- lapply(ACS_sex_age, function(x) get_avail(x))
sapply(ACS_sex_age_complete, ncol) == sapply(ACS_sex_age, ncol)
#all variables kept
ACS_sex_age <- ACS_sex_age_complete
rm(ACS_sex_age_complete)

# rename variables using data dictionary
sex_age_variables <- c(sex_age_male_variables, sex_age_female_variables)
ACS_sex_age <- lapply(ACS_sex_age, function(x) rename_fun(x, sex_age_variables, acs_summary_string ))

# remove !! from variable names
ACS_sex_age <- lapply(ACS_sex_age, function(x) removeBadPunc(x))

# numerators
numlist <- names(ACS_sex_age[[1]][grepl('Male', names(ACS_sex_age[[1]]))])
numlist_fe <- names(ACS_sex_age[[1]][grepl('Female', names(ACS_sex_age[[1]]))])
numlist <- c(numlist, numlist_fe)

# denominators
denomlist <- rep('Total', length(numlist))

# new variable names
newvarnamelist <- paste('Prop', numlist, sep = ' ')

# create proportion variables
ACS_sex_age <- proportion_list_fun(ACS_sex_age, num_list = numlist, denom_list = denomlist, new_var_name_list = newvarnamelist)

# get mean ages of males and females, by CBSA
ACS_sex_age <- lapply(ACS_sex_age, function(x) create_avg_age(x, 'Male'))
ACS_sex_age <- lapply(ACS_sex_age, function(x) create_avg_age(x, 'Female'))
ACS_sex_age <- lapply(ACS_sex_age, function(x) get_population_age(x,'mean_age_Male','Prop Male', "mean_age_Female", 'Prop Female', 'mean_age'))




#****************************************************************************#
#****************************************************************************#
#------------------------- Build master dataset -----------------------------#
#****************************************************************************#
#****************************************************************************#


################################################################################
# merge datasets together: one dataset for each year
################################################################################

acs_master_2012 <- master_fun(1)
acs_master_2013 <- master_fun(2)
acs_master_2014 <- master_fun(3)

# make sure all years match for a given year's dataset
head(acs_master_2012[, c('year.x', 'year.y', 'year.x.y', 'year.y.y', 'year.y.x', 'year.x.y', 'year.x.x')])
head(acs_master_2013[, c('year.x', 'year.y', 'year.x.y', 'year.y.y', 'year.y.x', 'year.x.y', 'year.x.x')])
head(acs_master_2014[, c('year.x', 'year.y','year.x.y', 'year.y.y', 'year.y.x', 'year.x.y', 'year.x.x')])

# create new year variable that exists for every obs, and delete all other year vars
acs_master_2012$year <- '2012'
acs_master_2013$year <- '2013'
acs_master_2014$year <- '2014'

# delete extra years using grep: (one for each year)
dropme <- c(names(acs_master_2012)[grepl('year\\.' , names(acs_master_2012))])
acs_master_2012 <- acs_master_2012[, !(names(acs_master_2012) %in% dropme)]

dropme <- c(names(acs_master_2013)[grepl('year\\.' , names(acs_master_2013))])
acs_master_2013 <- acs_master_2013[, !(names(acs_master_2013) %in% dropme)]

dropme <- c(names(acs_master_2014)[grepl('year\\.' , names(acs_master_2014))])
acs_master_2014 <- acs_master_2014[, !(names(acs_master_2014) %in% dropme)]

# drop population variables for 2010 and 2011
acs_master_2012$POPESTIMATE2010 <- NULL
acs_master_2013$POPESTIMATE2010 <- NULL
acs_master_2014$POPESTIMATE2010 <- NULL

acs_master_2012$POPESTIMATE2011 <- NULL
acs_master_2013$POPESTIMATE2011 <- NULL
acs_master_2014$POPESTIMATE2011 <- NULL

acs_master <- list(acs_master_2012, acs_master_2013, acs_master_2014)



# create wide dataset
acs_master_wide_2012 <- acs_master_2012
names(acs_master_wide_2012) <- paste0(names(acs_master_wide_2012), '_2012')

acs_master_wide_2013 <- acs_master_2013
names(acs_master_wide_2013) <- paste0(names(acs_master_wide_2013), '_2013')

acs_master_wide_2014 <- acs_master_2014
names(acs_master_wide_2014) <- paste0(names(acs_master_wide_2014), '_2014')

acs_master_wide <- merge(acs_master_wide_2012, acs_master_wide_2013, by.x = 'cbsa_2012', by.y = 'cbsa_2013')
acs_master_wide <- merge(acs_master_wide, acs_master_wide_2014, by.x = 'cbsa_2012', by.y = 'cbsa_2014')

colnames(acs_master_wide)[colnames(acs_master_wide) == "cbsa_2012"] <- "cbsa"

# # make a list of all of the variables we're interested in using for analysis (drop intermittent variables)
# analysis_variables <- c('industry_male_dom_prop', 'immigrant_native_prop', 'immigrant_naturalized_prop', 'immigrant_not_cit_prop', 'sex_male_prop', 'marital_male_never_married_prop', 'marital_male_now_married_prop', 'marital_male_divorced_prop', 'marital_female_never_married_prop', 'marital_female_now_married_prop', 'marital_female_divorced_prop', 'avg_com', 'foster_kid_prop', 'educ_male_agg_no_hs_prop', 'educ_male_agg_hs_deg_prop','educ_male_agg_col_deg_prop', 'educ_male_agg_grad_deg_prop',  'educ_female_agg_no_hs_prop', 'educ_female_agg_hs_deg_prop',  'educ_female_agg_col_deg_prop', 'educ_female_agg_grad_deg_prop', 'med_age_tot',  'med_age_male',  'med_age_female', 'mean_age_Male', 'mean_age_Female', 'mean_age', 'enrolled_male_prop',  'enrolled_female_prop', 'educ_female_18_24_no_hs_prop', 'marital_female_single_18_19_prop', 'marital_female_single_20_24_prop', 'marital_female_single_35_39_prop', 'marital_female_married_18_19_prop', 'marital_female_married_20_24_prop', 'marital_female_married_35_39_prop')


# # now apply this function for all variables we're interested in
# for (i in analysis_variables) {
#   dataset <- acs_master_wide
#   acs_master_wide <- yearly_change(dataset, i, list_of_years)
# }

# change to numeric
acs_master <- lapply(acs_master, function(x) to_numeric(x, excl_col = 'name'))

# create long dataset
acs_master_2012$year <- '2012'
acs_master_2013$year <- '2013'
acs_master_2014$year <- '2014'

acs_master_long <-rbind(acs_master_2012, acs_master_2013, acs_master_2014)

# make sure all bad punc is removed
acs_master <- lapply(acs_master, function(x) removeBadPunc(x))
acs_master_long <- removeBadPunc(acs_master_long)
acs_master_wide <- removeBadPunc(acs_master_wide)

names(acs_master_long) <- tolower(names(acs_master_long))
names(acs_master_wide) <- tolower(names(acs_master_wide))

################################################################################
# save master datasets
################################################################################
# save acs_master
saveRDS(acs_master, file = paste0(workingDatasets, '/demographics/acs_master.RDS'))

# save individual acs files
saveRDS(acs_master_2012, file = paste0(workingDatasets, '/demographics/acs_master_2012.RDS'))
saveRDS(acs_master_2013, file = paste0(workingDatasets, '/demographics/acs_master_2013.RDS'))
saveRDS(acs_master_2014, file = paste0(workingDatasets, '/demographics/acs_master_2014.RDS'))

# save wide
saveRDS(acs_master_wide, file = paste0(workingDatasets, '/demographics/acs_master_wide.RDS'))

# save long
saveRDS(acs_master_long, file = paste0(workingDatasets, '/demographics/acs_master_long.RDS'))