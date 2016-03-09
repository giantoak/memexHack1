###############################################################################

# Client: Giant Oak
# Author(s): Emily Oehlsen
# Date created: 2015 12 11
# Purpose: Re-create UCR portion of Giant Oak crime data set

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
librariesToLoad <- c("ggplot2", "curl", "data.table", "stringr", "plyr", "dplyr")

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


# load source files
#Specify files with constructor functions here
sourceFiles <- c()


if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, x, ".R", sep = "")))


# End time for source files
end_source_files <- Sys.time()


################################################################################
# Merge city level LEMAS and UCR
################################################################################

# Read in lemas 2013 data
lemas_2013_tgg <- read.csv(paste0(workingDatasets, "LEMAS/lemas_2013_tgg.csv"), strip.white = TRUE,stringsAsFactors = FALSE)

# Keeps variables we need
keeps <- c("main_city","main_state","ftsworn_2013")
lemas_2013_city_level_tgg <- lemas_2013_tgg[,keeps]

# Read in UCR city level data
ucr_all_city <- read.csv(paste0(workingDatasets,"UCR/Clean/UCR_all_city.csv"),strip.white=TRUE,stringsAsFactors = FALSE)

# Merge on main_city, main_state pairs
ucr_lemas_city_level_tgg <- merge(lemas_2013_city_level_tgg,ucr_all_city,by=c("main_city","main_state"),all=TRUE)

# Read in Population/MSA number dataset
population_msa <- read.csv(paste0(workingDatasets,"population/population_cbsa.csv",sep=""), stringsAsFactors=FALSE)

# Match population naming convention
population_msa$X <- NULL
names(population_msa) <- c("MSA.No.","population_2010","population_2011","population_2012","population_2013","population_2014")



################################################################################
# Merge MSA level LEMAS and UCR
################################################################################

# Import master MSA list
cbsa_crosswalk <- readRDS(paste0(workingDatasets, "Crosswalks/CBSA List.RDS"))

# Remove final three rows (source US Census Bureau, date, etc)
cbsa_crosswalk <- cbsa_crosswalk[!(cbsa_crosswalk$CBSA.Code == ""),]
cbsa_crosswalk <- cbsa_crosswalk[!(cbsa_crosswalk$CBSA.Code == "Source: U.S. Census Bureau, Population Division; Office of Management and Budget, February 2013 delineations"),]
cbsa_crosswalk <- cbsa_crosswalk[!(cbsa_crosswalk$CBSA.Code == "Internet Release Date: March 2013"),]

    # Correctly accents
    cbsa_crosswalk[cbsa_crosswalk$CBSA.Code == 15860, colnames(cbsa_crosswalk) == "CBSA.Title"] <- "Canon City, CO"
    cbsa_crosswalk[cbsa_crosswalk$CBSA.Code == 21580, colnames(cbsa_crosswalk) == "CBSA.Title"] <- "Espanola, NM"
    cbsa_crosswalk[cbsa_crosswalk$CBSA.Code == 32420, colnames(cbsa_crosswalk) == "CBSA.Title"] <- "Mayaguez, PR"
    cbsa_crosswalk[cbsa_crosswalk$CBSA.Code == 41900, colnames(cbsa_crosswalk) == "CBSA.Title"] <- "San German, PR"
    
    # Create main state variable
    
    # Take state mentioned in Metropolitan.Statistical.Area
    cbsa_crosswalk$main_state <- gsub("^.*,", "", cbsa_crosswalk$CBSA.Title)
    
    # If there are multiple states (separated by dash), keep first (main state)
    has_dash_index <- grep("-", cbsa_crosswalk$main_state)
    cbsa_crosswalk$main_state[has_dash_index] <- gsub("-.*$", "", cbsa_crosswalk$main_state[has_dash_index])
    has_dash_index <- NULL 
    
    has_space_index <- grep(" ", cbsa_crosswalk$main_state)
    cbsa_crosswalk$main_state[has_space_index] <- gsub("-.*$", "", cbsa_crosswalk$main_state[has_space_index])
    has_space_index <- NULL 
    
    # Remove punctuation in state variable 
    cbsa_crosswalk$main_state <- gsub("[[:punct:]]", "", cbsa_crosswalk$main_state)
    
    # Strip white space
    cbsa_crosswalk$main_state <- str_trim(cbsa_crosswalk$main_state)
    
    # Validate
    # table(cbsa_crosswalk$main_state) 
    
    # Create main city column
    
    # Take the city mentioned in the MSA string
    cbsa_crosswalk$main_city <- gsub(",.*$", "", cbsa_crosswalk$CBSA.Title)
    
    # If there are multiple cities (separated by dash or slash), keep first (main city)
    has_dash_index <- grep("-", cbsa_crosswalk$main_city)
    cbsa_crosswalk$main_city[has_dash_index] <- gsub("-.*$", "", cbsa_crosswalk$main_city[has_dash_index])
    has_dash_index <- NULL
    
    has_slash_index <- grep("/", cbsa_crosswalk$main_city)
    cbsa_crosswalk$main_city[has_slash_index] <- gsub("/.*$", "", cbsa_crosswalk$main_city[has_slash_index])
    has_slash_index <- NULL
    
    # Replace St. with Saint
    cbsa_crosswalk$main_city <- gsub("St. ", "Saint ", cbsa_crosswalk$main_city)
    
    # Replace unencoded dash
    cbsa_crosswalk$main_city <- gsub('\x97', "-", cbsa_crosswalk$main_city)
    cbsa_crosswalk$main_city <- gsub('\x96', "-", cbsa_crosswalk$main_city)
    
    # Strip punctuation from primary city, get rid of leading/trailing white space
    cbsa_crosswalk$main_city <- gsub("[[:punct:]]", " ", cbsa_crosswalk$main_city)
    cbsa_crosswalk$main_city <- str_trim(cbsa_crosswalk$main_city)
    
    # Strip white space
    cbsa_crosswalk$main_city <- str_trim(cbsa_crosswalk$main_city)
    
    # Keep columns we need and rename appropriately
    #keeps <- c("main_city", "main_state", "CBSA.Code")
    #cbsa_crosswalk <- cbsa_crosswalk[, keeps]
    names(cbsa_crosswalk)[names(cbsa_crosswalk)=="CBSA.Code"] <- "MSA.No."

# Import UCR MSA file
UCR_all_MSA <- read.csv(paste0(workingDatasets,"UCR/Clean/UCR_all_msa.csv"), row.names = NULL, stringsAsFactors = FALSE)

nrow(UCR_all_MSA) #397

UCR_all_MSA$main_state <- gsub("Puerto Rico", "PR", UCR_all_MSA$main_state)

# Read in lemas 2013 MSA data
lemas_2013_tgg <- read.csv(paste0(workingDatasets, "LEMAS/lemas_2013_msa_tgg.csv"), strip.white = TRUE,stringsAsFactors = FALSE, row.names = NULL)

nrow(lemas_2013_tgg) #374 
keeps <- c("ftsworn_2013_msa", "MSA.No.")
lemas_2013_msa_level <- lemas_2013_tgg[,keeps]


# Merge UCR on to CBSA crosswalk 
ucr_lemas_msa_level_tgg <- merge(cbsa_crosswalk, UCR_all_MSA, by = c("main_city", "main_state"), all = TRUE)
nrow(ucr_lemas_msa_level_tgg[is.na(ucr_lemas_msa_level_tgg$MSA.No.), c('main_city', 'main_state'), ])
#10 missing ie. # 10 MSAs in UCR that do not correspond to MSAs in the census crosswalk, all very small and/or in Puerto Rico
ucr_lemas_msa_level_tgg[is.na(ucr_lemas_msa_level_tgg$MSA.No.), c('main_city', 'main_state')]
missing_cities <- ucr_lemas_msa_level_tgg[is.na(ucr_lemas_msa_level_tgg$MSA.No.), c('main_city')]
sapply(missing_cities, FUN = function(x) cbsa_crosswalk[grepl(x, cbsa_crosswalk$main_city), ])

#only Homosassa Spring and Honolulu match
#Rename and remerge
UCR_all_MSA$main_city[UCR_all_MSA$main_city == 'Honolulu'] <- 'Urban Honolulu'
UCR_all_MSA$main_city[UCR_all_MSA$main_city == 'Homosassa Spring'] <- 'Homosassa Springs'
UCR_all_MSA[grepl('Homosassa', UCR_all_MSA$main_city), ]
#Need to coalesce Homosassa
Homosassa <- UCR_all_MSA[UCR_all_MSA$main_city == "Homosassa Springs", ]
Homosassa <- cbind(unique(Homosassa[, (colnames(Homosassa) %in% c('main_city', 'main_state'))]), 
                   t(colSums(Homosassa[, !(colnames(Homosassa) %in% c('main_city', 'main_state'))], na.rm = TRUE)))

UCR_all_MSA_newHomosassa <- rbind(UCR_all_MSA[!grepl('Homosassa', UCR_all_MSA$main_city), ], Homosassa)
#Validate
nrow(UCR_all_MSA) == nrow(UCR_all_MSA_newHomosassa) + 1
UCR_all_MSA <- UCR_all_MSA_newHomosassa
rm(UCR_all_MSA_newHomosassa)


# ReMerge UCR on to CBSA crosswalk 
# 8 MSAs in UCR that do not correspond to MSAs in the census crosswalk, all very small and/or in Puerto Rico
ucr_lemas_msa_level_tgg <- merge(cbsa_crosswalk, UCR_all_MSA, by = c("main_city", "main_state"), all = TRUE)
nrow(ucr_lemas_msa_level_tgg)
ucr_drops <- nrow(ucr_lemas_msa_level_tgg[is.na(ucr_lemas_msa_level_tgg$MSA.No.), c('main_city', 'main_state'), ])
ucr_drops
#8 missing -- drop em

ucr_lemas_msa_level_tgg <- ucr_lemas_msa_level_tgg[!is.na(ucr_lemas_msa_level_tgg$MSA.No.), ]
nrow(ucr_lemas_msa_level_tgg)
length(unique(ucr_lemas_msa_level_tgg$MSA.No.)) == nrow(ucr_lemas_msa_level_tgg)

# Merge Lemas onto CBSA crosswalk 
ucr_lemas_msa_level_tgg <- merge(ucr_lemas_msa_level_tgg, lemas_2013_msa_level, by = c("MSA.No."), all.x = TRUE)
length(unique(ucr_lemas_msa_level_tgg$MSA.No.)) == nrow(ucr_lemas_msa_level_tgg)

# Remove city, state columns
ucr_lemas_msa_level_tgg$main_city <- NULL
ucr_lemas_msa_level_tgg$main_state <- NULL

# Validate
sapply(ucr_lemas_msa_level_tgg, FUN = function(x) sum(is.na(x)))
#500+ missing -- likiely micropolitan statistical areas

sapply(ucr_lemas_msa_level_tgg, FUN = function(x) tapply(x, ucr_lemas_msa_level_tgg[['Metropolitan.Micropolitan.Statistical.Area']], FUN = function(x) sum(is.na(x))))
#35-65 MSAs missing UCR/LEMAS data, 500++ micropolitan areas missing

ucr_lemas_msa_level_tgg$CBSA.Title <- NULL
ucr_lemas_msa_level_tgg$Metropolitan.Micropolitan.Statistical.Area <- NULL

################################################################################
# Merging population and MSA-level data
################################################################################

# We have law enforcement numbers and UCR crime data for 300 unique MSAs
names(ucr_lemas_msa_level_tgg)
nrow(ucr_lemas_msa_level_tgg) == length(unique(ucr_lemas_msa_level_tgg$MSA.No.))

# Need populations for weighted means/per capita stats
ucr_lemas_msa_level_tgg<- merge(ucr_lemas_msa_level_tgg,population_msa,by="MSA.No.",all.x=TRUE)
summary(is.na(ucr_lemas_msa_level_tgg$population_2010.y))
summary(is.na(ucr_lemas_msa_level_tgg$population_2011.y))
summary(is.na(ucr_lemas_msa_level_tgg$population_2012.y))
summary(is.na(ucr_lemas_msa_level_tgg$population_2013.y))
summary(is.na(ucr_lemas_msa_level_tgg$population_2014.y))


################################################################################
# Save city and MSA level lemas and UCR
################################################################################

write.csv(ucr_lemas_city_level_tgg,file = paste(workingDatasets,"Crime Data","/ucr_lemas_city_level_tgg.csv",sep=""))
write.csv(ucr_lemas_msa_level_tgg,file = paste(workingDatasets,"Crime Data","/ucr_lemas_msa_level_tgg.csv",sep=""))


################################################################################
# Read in NIBRS
################################################################################

NIBRS_violence <- read.csv(paste0(workingDatasets,"/Crime Data/NIBRS_prostitution.csv"))
NIBRS_female_violence <- read.csv(paste0(workingDatasets,"/Crime Data/NIBRS_violence.csv"))
NIBRS_prostitution <- read.csv(paste0(workingDatasets,"/Crime Data/NIBRS_female_violence.csv"))

NIBRS_violence$X <- NULL
NIBRS_female_violence$X <- NULL
NIBRS_prostitution$X<- NULL

################################################################################
# Create long msa-level data set
################################################################################

# Change to data table
crime_msa_tgg <- data.table(ucr_lemas_msa_level_tgg)
crime_vars <- colnames(crime_msa_tgg)[grepl('20', colnames(crime_msa_tgg))]

# Change to long dataset
crime_msa_long <- melt(crime_msa_tgg,id.vars = 'MSA.No.',measure.vars = crime_vars)
crime_msa_long <- data.frame(crime_msa_long)
crime_msa_long$variable <- as.character(crime_msa_long$variable)
crime_msa_long <- crime_msa_long[order(crime_msa_long[['MSA.No.']]),]

# Extract year and varnames
crime_msa_long$year <- gsub("[^[:digit:]]","\\1",crime_msa_long$variable)
table(crime_msa_long$year)

# Note extra 298 in 2013 since we have ftsworn (LEMAS) for that year

crime_msa_long$key_var <- gsub("^([[:alpha:]]*).*", "\\1", crime_msa_long$variable)
crime_msa_long$variable <- NULL
crime_msa_year_level <- dcast(crime_msa_long, formula = MSA.No. + year ~ key_var, value.var = 'value',fun.aggregate = mean)

# Creating per capita variables
crime_msa_year_level$violent_percap <- crime_msa_year_level$violent / crime_msa_year_level$population
crime_msa_year_level$rape_percap <- crime_msa_year_level$rape / crime_msa_year_level$population
crime_msa_year_level$property_percap <- crime_msa_year_level$property / crime_msa_year_level$population
crime_msa_year_level$ftsworn_percap <- crime_msa_year_level$ftsworn / crime_msa_year_level$population

# Merge in NIBRS
crime_msa_year_level <- merge(crime_msa_year_level,NIBRS_violence,by=c("MSA.No.","year"),all.x=TRUE)
crime_msa_year_level <- merge(crime_msa_year_level,NIBRS_female_violence,by=c("MSA.No.","year"),all.x=TRUE)
crime_msa_year_level <- merge(crime_msa_year_level,NIBRS_prostitution,by=c("MSA.No.","year"),all.x=TRUE)

# Save
write.csv(crime_msa_year_level,file = paste(workingDatasets,"Crime Data","/crime_msa_year_level.csv",sep=""))


################################################################################
# Create long data set with complete cases
################################################################################

# Change to data table
complete_crime_msa <- data.table(ucr_lemas_msa_level_tgg[complete.cases(ucr_lemas_msa_level_tgg),])
crime_vars <- colnames(complete_crime_msa)[grepl('20',colnames(complete_crime_msa))]

# Change to long dataset
crime_msa_long_complete <- melt(complete_crime_msa,id.vars = 'MSA.No.',measure.vars = crime_vars)
crime_msa_long_complete <- data.frame(crime_msa_long_complete)
crime_msa_long_complete$variable <- as.character(crime_msa_long_complete$variable)
crime_msa_long_complete <- crime_msa_long_complete[order(crime_msa_long_complete[['MSA.No.']]),]

# Extract year and varnames
crime_msa_long_complete$year <- gsub("[^[:digit:]]","\\1",crime_msa_long_complete$variable)
table(crime_msa_long_complete$year)

# Note extra 298 in 2013 since we have ftsworn (LEMAS) for that year

crime_msa_long_complete$key_var <- gsub("^([[:alpha:]]*).*", "\\1", crime_msa_long_complete$variable)
crime_msa_long_complete$variable <- NULL
crime_msa_year_level_complete <- dcast(crime_msa_long_complete, formula = MSA.No. + year ~ key_var, value.var = 'value',fun.aggregate = mean)

# Create per capita variables
crime_msa_year_level_complete$violent_percap <- crime_msa_year_level_complete$violent / crime_msa_year_level_complete$population
crime_msa_year_level_complete$rape_percap <- crime_msa_year_level_complete$rape / crime_msa_year_level_complete$population
crime_msa_year_level_complete$property_percap <- crime_msa_year_level_complete$property / crime_msa_year_level_complete$population
crime_msa_year_level_complete$ftsworn_percap <- crime_msa_year_level_complete$ftsworn / crime_msa_year_level_complete$population

# Save
write.csv(crime_msa_year_level_complete,file = paste(workingDatasets,"Crime Data","/crime_msa_year_level_complete.csv",sep=""))


################################################################################
# Create rates and take mean UCR figures across years
################################################################################

# Reading in data
crime_msa_year <- read.csv(paste0(workingDatasets,"Crime Data/crime_msa_year_level.csv"),strip.white=TRUE,stringsAsFactors = FALSE, row.names = NULL)
crime_msa <- read.csv(paste0(workingDatasets,"Crime Data/ucr_lemas_msa_level_tgg.csv"),strip.white=TRUE,stringsAsFactors = FALSE, row.names = NULL)

# Create rate variables for MSA-year level
crime_msa_year_level$rape_rate <- crime_msa_year_level$rape/crime_msa_year_level$population
crime_msa_year_level$violent_rate <- crime_msa_year_level$violent/crime_msa_year_level$population
crime_msa_year_level$property_rate <- crime_msa_year_level$property/crime_msa_year_level$population

# Save
write.csv(crime_msa_year_level,file = paste(workingDatasets,"Crime Data","/crime_msa_year_level.csv",sep=""))

# Validate
str(crime_msa)
length(unique(crime_msa$MSA.No.)) #929
sapply(crime_msa, FUN = function(x) sum(is.na(x)))

# Merging with msa- level (adjusting column names)
colnames(crime_msa)[colnames(crime_msa) == "MSA.No."] <- "CBSACode"
colnames(crime_msa)[colnames(crime_msa) == "ftsworn_2013_msa"] <- "ftsworn_2013"
colnames(crime_msa)[colnames(crime_msa) == "population_2013.y"] <- 'pop_estimate_2013'
crime_msa$ftsworn_rateper100k_2013 <- crime_msa$ftsworn_2013 / crime_msa$pop_estimate_2013 * 100 * 1000
summary(crime_msa$ftsworn_rateper100k_2013 )
quantile(crime_msa$ftsworn_rateper100k_2013, probs = seq(0,1,0.05), na.rm = TRUE) #One large outlier

# Keeping MEAN (previous max) UCR value across 2010-2014 (based on summary stats), warnings refer to rows with all missings
crime_msa$violent_msa <- apply(crime_msa[ , c("violent_crime_2010", "violent_crime_2011", "violent_crime_2012", "violent_crime_2013", "violent_crime_2014")], 1,
                               FUN = function(x) mean(x, na.rm=TRUE))
crime_msa$violent_msa[crime_msa$violent_msa == "-Inf"] <- NA

crime_msa$rape_msa <- apply(crime_msa[ , c("rape_2010", "rape_2011", "rape_2012", "rape_2013", "rape_2014")], 1,
                            FUN = function(x) mean(x, na.rm=TRUE))
crime_msa$rape_msa[crime_msa$rape_msa == "-Inf"] <- NA

crime_msa$property_msa <- apply(crime_msa[ , c("property_crime_2010", "property_crime_2011", "property_crime_2012", "property_crime_2013", "property_crime_2014")], 1,
                                FUN = function(x) mean(x, na.rm=TRUE))
crime_msa$property_msa[crime_msa$property_msa == "-Inf"] <- NA

# Creating rate variables
crime_msa$violent_rate_2010 <- crime_msa$violent_crime_2010 / crime_msa$population_2010.x
crime_msa$violent_rate_2011 <- crime_msa$violent_crime_2011 / crime_msa$population_2011.x
crime_msa$violent_rate_2012 <- crime_msa$violent_crime_2012 / crime_msa$population_2012.x    
crime_msa$violent_rate_2013 <- crime_msa$violent_crime_2013 / crime_msa$population_2013.x    
crime_msa$violent_rate_2014 <- crime_msa$violent_crime_2014 / crime_msa$population_2014.x    

crime_msa$property_rate_2010 <- crime_msa$property_crime_2010 / crime_msa$population_2010.x
crime_msa$property_rate_2011 <- crime_msa$property_crime_2011 / crime_msa$population_2011.x
crime_msa$property_rate_2012 <- crime_msa$property_crime_2012 / crime_msa$population_2012.x    
crime_msa$property_rate_2013 <- crime_msa$property_crime_2013 / crime_msa$population_2013.x    
crime_msa$property_rate_2014 <- crime_msa$property_crime_2014 / crime_msa$population_2014.x    

crime_msa$rape_rate_2010 <- crime_msa$rape_2010 / crime_msa$population_2010.x
crime_msa$rape_rate_2011 <- crime_msa$rape_2011 / crime_msa$population_2011.x
crime_msa$rape_rate_2012 <- crime_msa$rape_2012 / crime_msa$population_2012.x    
crime_msa$rape_rate_2013 <- crime_msa$rape_2013 / crime_msa$population_2013.x    
crime_msa$rape_rate_2014 <- crime_msa$rape_2014 / crime_msa$population_2014.x    

# Keeping MEAN (previous max) UCR rate value across 2010-2014
crime_msa$violent_rate_msa <- apply(crime_msa[ , c("violent_rate_2010", "violent_rate_2011", "violent_rate_2012", "violent_rate_2013", "violent_rate_2014")], 1,
                                    FUN = function(x) mean(x, na.rm=TRUE))
crime_msa$violent_rate_msa[crime_msa$violent_rate_msa == "-Inf"] <- NA

crime_msa$rape_rate_msa <- apply(crime_msa[ , c("rape_rate_2010", "rape_rate_2011", "rape_rate_2012", "rape_rate_2013", "rape_rate_2014")], 1,
                                 FUN = function(x) mean(x, na.rm=TRUE))
crime_msa$rape_rate_msa[crime_msa$rape_rate_msa == "-Inf"] <- NA

crime_msa$property_rate_msa <- apply(crime_msa[ , c("property_rate_2010", "property_rate_2011", "property_rate_2012", "property_rate_2013", "property_rate_2014")], 1,
                                     FUN = function(x) mean(x, na.rm=TRUE))
crime_msa$property_rate_msa[crime_msa$property_rate_msa == "-Inf"] <- NA

saveRDS(crime_msa, file = paste(workingDatasets,"Crime Data","/crime_data_wide.rds",sep=""))


################################################################################
# Merge NIBRS and other crime/arrest data
################################################################################

# Bringing in NIBRS, merging with crime, and keeping prostitution MEAN (previous max)
NIBRS_prostitution <- read.csv(paste0(workingDatasets,"Crime Data/NIBRS_prostitution_wide.csv"),strip.white=TRUE,stringsAsFactors = FALSE)
NIBRS_prostitution$X <- NULL
colnames(NIBRS_prostitution)[colnames(NIBRS_prostitution) == "CBSA.Code"] <- "CBSACode"

crime_msa <- merge(crime_msa, NIBRS_prostitution, by = "CBSACode", all= TRUE)
str(crime_msa)

crime_msa$prostitution_rate_msa <- apply(crime_msa[ , c("prostitution_rate_2011", "prostitution_rate_2012", "prostitution_rate_2013")], 1,
                                         FUN = function(x) mean(x, na.rm=TRUE))
crime_msa$prostitution_count_msa <- apply(crime_msa[ , c("prostitution_NIBRS.2011", "prostitution_NIBRS.2012", "prostitution_NIBRS.2013")], 1,
                                          FUN = function(x) mean(x, na.rm=TRUE))

crime_msa$prostitution_rate_msa[crime_msa$prostitution_rate_msa == "-Inf"] <- NA
crime_msa$prostitution_count_msa[crime_msa$prostitution_count_msa == "-Inf"] <- NA

summary(crime_msa$prostitution_count_msa)

crime_msa <- crime_msa[, c('CBSACode','pop_estimate_2013', 'ftsworn_2013', 'ftsworn_rateper100k_2013', 'violent_msa', 'rape_msa', 'property_msa', 'violent_rate_msa', 'rape_rate_msa', 'property_rate_msa','prostitution_rate_msa', 'prostitution_count_msa')]

crime_msa <- crime_msa[!duplicated(crime_msa$CBSACode, crime_msa$ftsworn_2013), ]


################################################################################
# Change rates to per 100K
################################################################################

# Change rates to 100K
crime_msa$violent_rate_msa <- crime_msa$violent_rate_msa * 100000
crime_msa$property_rate_msa <- crime_msa$property_rate_msa * 100000
crime_msa$rape_rate_msa <- crime_msa$rape_rate_msa * 100000


################################################################################
# Cleaning up MSA-year-level
################################################################################

# Merging with msa- and year- level 
# Adjusting column names
colnames(crime_msa_year)[colnames(crime_msa_year) == "MSA.No."] <- "CBSACode"
colnames(crime_msa_year)[colnames(crime_msa_year) == "population"] <- "population_UCR"
crime_msa_year$X <- NULL
crime_msa_year$violent_rate <- NULL
crime_msa_year$rape_rate<- NULL
crime_msa_year$property_rate<- NULL  


################################################################################
# Save
################################################################################

# Save
saveRDS(crime_msa_year, file = paste0(workingDatasets, "Crime Data/crime_msa_year_level.RDS"))
saveRDS(crime_msa, file = paste0(workingDatasets, "Crime Data/crime_msa.RDS"))
