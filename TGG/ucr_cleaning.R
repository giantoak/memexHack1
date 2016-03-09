###############################################################################

# Client: Giant Oak
# Author(s): Emily Oehlsen
# Date created: 2015 12 11
# Purpose: Clean UCR data

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
librariesToLoad <- c("ggplot2", "curl", "data.table", "stringr", "dplyr")

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
# Create UCR city-level data set
################################################################################

page_list <- list.files(paste0(workingDatasets, "UCR/Excel", sep=""))

for (i in 1:length(page_list)){
  
  # Read in data
  current_file <- read.csv(paste0(workingDatasets, "UCR/Excel/", page_list[i], sep=""), stringsAsFactors=FALSE)
  
  # Select those rows that correspond to cities (i.e. partial string match, "City of..")
  current_file <- current_file[grep("City of", current_file$Counties.principal.cities), ]
  
  # Remove numbers in Counties.principal.cities (relevant to create state variable below)
  has_number_index <- grep("[0-9]", current_file$Counties.principal.cities)
  current_file$Counties.principal.cities_denumbered <- current_file$Counties.principal.cities
  current_file$Counties.principal.cities_denumbered[has_number_index] <- gsub("[0-9]", "", current_file$Counties.principal.cities[has_number_index])
  has_number_index <- NULL 
  
  # Remove trailing comma and white space for Counties.principal.cities 
  current_file$Counties.principal.cities_denumbered <- str_trim(current_file$Counties.principal.cities_denumbered)
  current_file$Counties.principal.cities_denumbered <- gsub(",$", "", current_file$Counties.principal.cities_denumbered)
  
  # Create state variable
  
  # Take this initially from Metropolitan.Statistical.Area
  current_file$state <- gsub('\\W*M\\.S\\.A\\..*$', '', current_file$Metropolitan.Statistical.Area)    
  current_file$state <- gsub("^.*,", "", current_file$state)
  
  # If Counties.principal.cities has a state, use this instead (main state)
  has_comma_index <- grep(",", current_file$Counties.principal.cities_denumbered)
  current_file$state[has_comma_index] <- gsub("^.*,", "", current_file$Counties.principal.cities_denumbered[has_comma_index])
  has_comma_index <- NULL
  
  # If there are multiple states (separated by dash), keep first (main state)
  has_dash_index <- grep("-", current_file$state)
  current_file$state[has_dash_index] <- gsub("-.*$", "", current_file$state[has_dash_index])
  has_dash_index <- NULL
  
  # Remove punctuation in state variable 
  current_file$state <- gsub("[[:punct:]]", "", current_file$state)
  
  # Strip white space
  current_file$state <- str_trim(current_file$state)
  
  # Validate
  table(current_file$state)
  
  # Create city column
  
  # Remove City of...
  current_file$city <- gsub(pattern="City of", replacement="", current_file$Counties.principal.cities_denumbered)
  
  # If there is a comma followed by a state, remove this
  has_comma_index <- grep(",", current_file$city)
  current_file$city[has_comma_index] <- gsub(",.*$", "", current_file$city[has_comma_index])
  has_comma_index <- NULL
  
  # Replace St. with Saint
  current_file$city <- gsub("St. ", "Saint ", current_file$city)
  
  # Strip white space
  current_file$city <- str_trim(current_file$city)
  
  # Validate
  table(current_file$city)
  
  # Keep relevant columns for replicating Giant Oak's data frame 
  current_file <- current_file[, c("city", "state", "Population", "Violent.crime", "Rape", "Property.crime")]
  
  # Rename columns
  colnames(current_file)[colnames(current_file) == "city"] <- "main_city"
  colnames(current_file)[colnames(current_file) == "state"] <- "main_state"
  colnames(current_file)[colnames(current_file) == "Population"] <- "population_year"
  colnames(current_file)[colnames(current_file) == "Violent.crime"] <- "violent_crime_year"
  colnames(current_file)[colnames(current_file) == "Rape"] <- "rape_year"
  colnames(current_file)[colnames(current_file) == "Property.crime"] <- "property_crime_year"
  
  # Strip punctuation from violent crime, rape, property crime, population
  current_file$violent_crime_year <- gsub("[[:punct:]]", "", current_file$violent_crime_year)
  current_file$rape_year <- gsub("[[:punct:]]", "", current_file$rape_year)
  current_file$property_crime_year <- gsub("[[:punct:]]", "", current_file$property_crime_year)
  current_file$population_year <- gsub("[[:punct:]]", "", current_file$population_year)
  
  # Keep as separate files  
  name <- gsub(".csv", "_city_GO", page_list[i])
  assign(name, current_file)
  
}

# Rename variables (for variables not called city_state (crime metrics), append dataset year to varname)
names(UCR_2010_city_GO) <- gsub("_year", "_2010", names(UCR_2010_city_GO))
names(UCR_2011_city_GO) <- gsub("_year", "_2011", names(UCR_2011_city_GO))
names(UCR_2012_city_GO) <- gsub("_year", "_2012", names(UCR_2012_city_GO))
names(UCR_2013_city_GO) <- gsub("_year", "_2013", names(UCR_2013_city_GO))
names(UCR_2014_city_GO) <- gsub("_year", "_2014", names(UCR_2014_city_GO))

# Merge into one file
UCR_all_city <- merge(UCR_2010_city_GO, UCR_2011_city_GO, by = c("main_city","main_state"), all=TRUE)
UCR_all_city <- merge(UCR_all_city, UCR_2012_city_GO, by = c("main_city","main_state"), all=TRUE)
UCR_all_city <- merge(UCR_all_city, UCR_2013_city_GO, by = c("main_city","main_state"), all=TRUE)
UCR_all_city <- merge(UCR_all_city, UCR_2014_city_GO, by = c("main_city","main_state"), all=TRUE)

# Look for multiple entries for cities and states (no duplicates)
UCR_all_city_citystate_only <- UCR_all_city[, c("main_city", "main_state")]
UCR_all_city_citystate_only[duplicated(UCR_all_city_citystate_only), ]

# Validate missings
nrow(UCR_all_city[!complete.cases(UCR_all_city), ]) / nrow(UCR_all_city)

# Correct discrepancies
"Las Vegas" -> UCR_all_city[grep("Las Vegas Metropolitan Police Department", UCR_all_city$main_city), 1]

# Save data
write.csv(UCR_all_city, paste0(workingDatasets, "UCR/Clean/UCR_all_city.csv"))


################################################################################
# Create UCR MSA level data set
################################################################################

page_list <- list.files(paste0(workingDatasets, "UCR/Excel", sep=""))

for (i in 1:length(page_list)){
  
  # Read in data
  current_file <- read.csv(paste0(workingDatasets, "UCR/Excel/", page_list[i], sep=""), stringsAsFactors=FALSE, encoding = 'UTF-8')
  
  msas_reporting <- length(unique(current_file[['Metropolitan.Statistical.Area']][grepl('M.S.A.', current_file[['Metropolitan.Statistical.Area']])]))
  
  # Select those rows that correspond to estimated MSAs, keep population figures
  current_file <- current_file[grep("M.S.A.", current_file$Metropolitan.Statistical.Area), ]
  current_file1 <- current_file[current_file$Population == "100.00%", ]
  if (nrow(current_file1) != msas_reporting) {
      print("WARNING: NOT PULLING ALL MSAs")
  }
  current_file2 <- current_file[grep("Includes", current_file$Counties.principal.cities), ]
  current_file2 <- current_file2[, c("Metropolitan.Statistical.Area", "Population")]
  
  current_file <- merge(current_file1, current_file2, by = "Metropolitan.Statistical.Area", all=TRUE)
  
  # Create main state variable
  
  # Take state mentioned in Metropolitan.Statistical.Area
  current_file$main_state <- gsub("\\W*M\\.S\\.A\\..*$", "", current_file$Metropolitan.Statistical.Area)
  current_file$main_state <- gsub("MSA", "", current_file$main_state)    
  current_file$main_state <- gsub("^.*,", "", current_file$main_state)
  
  # If there are multiple states (separated by dash), keep first (main state)
  has_dash_index <- grep("-", current_file$main_state)
  current_file$main_state[has_dash_index] <- gsub("-.*$", "", current_file$main_state[has_dash_index])
  has_dash_index <- NULL 
  
  has_space_index <- grep(" ", current_file$main_state)
  current_file$main_state[has_space_index] <- gsub("-.*$", "", current_file$main_state[has_space_index])
  has_space_index <- NULL 
  
  # Remove punctuation in state variable 
  current_file$main_state <- gsub("[[:punct:]]", "", current_file$main_state)
  
  # Strip white space
  current_file$main_state <- str_trim(current_file$main_state)
  
  # Validate
  table(current_file$main_state)
  
  # Create main city column
  
  # Take the city mentioned in the MSA string
  current_file$main_city <- gsub('\\W*M\\.S\\.A\\..*$', '', current_file$Metropolitan.Statistical.Area)    
  current_file$main_city <- gsub(",.*$", "", current_file$main_city)
  
  # Replace unencoded dash
  current_file$main_city <- gsub('\x97', "-", current_file$main_city)
  current_file$main_city <- gsub('\x96', "-", current_file$main_city)
  
  # If there are multiple cities (separated by dash or slash), keep first (main city)
  has_dash_index <- grep("-", current_file$main_city)
  current_file$main_city[has_dash_index] <- gsub("-.*$", "", current_file$main_city[has_dash_index])
  has_dash_index <- NULL
  
  has_slash_index <- grep("/", current_file$main_city)
  current_file$main_city[has_slash_index] <- gsub("/.*$", "", current_file$main_city[has_slash_index])
  has_slash_index <- NULL
  
  # Replace St. with Saint
  current_file$main_city <- gsub("St. ", "Saint ", current_file$main_city)
  

  # Strip punctuation from primary city, get rid of leading/trailing white space
  current_file$main_city <- gsub("[[:punct:]]", " ", current_file$main_city)
  current_file$main_city <- str_trim(current_file$main_city)
  
  # Strip white space
  current_file$main_city <- str_trim(current_file$main_city)
  
  # MSA renamed from Santa Barbara-Santa Maria-Goleta (2011) to Santa Maria-Santa Barbara (2012); keep later name
  current_file$main_city <- gsub("Santa Barbara", "Santa Maria", current_file$main_city)
  
  # Validate
  table(current_file$main_city)
  
  # Keep relevant columns for replicating Giant Oak's data frame 
  current_file <- current_file[, c("main_city", "main_state", "Population.y", "Violent.crime", "Rape", "Property.crime")]
  
  # Rename columns
  colnames(current_file)[colnames(current_file) == "Population.y"] <- "population_year"
  colnames(current_file)[colnames(current_file) == "Violent.crime"] <- "violent_crime_year"
  colnames(current_file)[colnames(current_file) == "Rape"] <- "rape_year"
  colnames(current_file)[colnames(current_file) == "Property.crime"] <- "property_crime_year"
  
  # Strip punctuation from violent crime, rape, property crime
  current_file$population_year <- gsub("[[:punct:]]", "", current_file$population_year)
  current_file$violent_crime_year <- gsub("[[:punct:]]", "", current_file$violent_crime_year)
  current_file$rape_year <- gsub("[[:punct:]]", "", current_file$rape_year)
  current_file$property_crime_year <- gsub("[[:punct:]]", "", current_file$property_crime_year)
  
  # Keep variables we need to replicate Giant Oak dataset
  current_file <- current_file[, c("violent_crime_year", "rape_year", "property_crime_year", "population_year", "main_city", "main_state")]
  
  missing_violent_crime_year <- nrow(current_file[current_file$violent_crime_year == '', ])
  missing_property_crime_year <- nrow(current_file[current_file$property_crime_year == '', ])
  missing_population_year <- nrow(current_file[current_file$population_year == '', ])
  missing_rape_year <- nrow(current_file[current_file$rape_year == '', ])
  
  current_file[, c("violent_crime_year", "rape_year", "property_crime_year", "population_year")] <- 
      sapply(current_file[, c("violent_crime_year", "rape_year", "property_crime_year", "population_year")], as.numeric)
  
  if (missing_violent_crime_year < sum(is.na(current_file$violent_crime_year))) {
      print(paste0(sum(is.na(current_file$violent_crime_year)) - missing_violent_crime_year, " violence dropped in ", page_list[i]))
      print((current_file[is.na(current_file$violent_crime_year), ]))
  }
  if (missing_rape_year < sum(is.na(current_file$rape_year))) {
      print(paste0(sum(is.na(current_file$rape_year)) - missing_rape_year, " rape dropped in ", page_list[i]))
      print((current_file[is.na(current_file$rape_year), ]))
  }
  if (missing_population_year < sum(is.na(current_file$population_year))) {
      print(paste0(sum(is.na(current_file$population_year)) - missing_population_year, " population dropped in ", page_list[i]))
      print((current_file[is.na(current_file$population_year), ]))
  }
  if (missing_property_crime_year < sum(is.na(current_file$property_crime_year))) {
      print(paste0(sum(is.na(current_file$property_crime_year)) - missing_property_crime_year, " property dropped in ", page_list[i]))
      print((current_file[is.na(current_file$property_crime_year), ]))
  }
 
  
  # Keep as separate files  
  name <- gsub(".csv", "_MSA_GO", page_list[i])
  assign(name, current_file)
  
}

# Rename variables
names(UCR_2010_MSA_GO) <- gsub("_year", "_2010", names(UCR_2010_MSA_GO))
names(UCR_2011_MSA_GO) <- gsub("_year", "_2011", names(UCR_2011_MSA_GO))
names(UCR_2012_MSA_GO) <- gsub("_year", "_2012", names(UCR_2012_MSA_GO))
names(UCR_2013_MSA_GO) <- gsub("_year", "_2013", names(UCR_2013_MSA_GO))
names(UCR_2014_MSA_GO) <- gsub("_year", "_2014", names(UCR_2014_MSA_GO))

# Merge into one file
UCR_all_MSA <- merge(UCR_2010_MSA_GO, UCR_2011_MSA_GO, by = c("main_city", "main_state"), all=TRUE)
UCR_all_MSA <- merge(UCR_all_MSA, UCR_2012_MSA_GO, by = c("main_city", "main_state"), all=TRUE)
UCR_all_MSA <- merge(UCR_all_MSA, UCR_2013_MSA_GO, by = c("main_city", "main_state"), all=TRUE)
UCR_all_MSA <- merge(UCR_all_MSA, UCR_2014_MSA_GO, by = c("main_city", "main_state"), all=TRUE)

# Validate missings
nrow(UCR_all_MSA[!complete.cases(UCR_all_MSA), ]) / nrow(UCR_all_MSA)

# Look for multiple entries for cities and states (no duplicates)
UCR_all_MSA_citystate_only <- UCR_all_MSA[, c("main_city", "main_state")]
UCR_all_MSA_citystate_only[duplicated(UCR_all_MSA_citystate_only), ]
#UCR_all_MSA[duplicated(UCR_all_MSA_citystate_only), ]

# Mistake with Billings, MN, there are no conflicts, the top row contains all information
UCR_all_MSA <- UCR_all_MSA[!duplicated(UCR_all_MSA_citystate_only),]
#UCR_all_MSA <- UCR_all_MSA[-(39:41),]
nrow(UCR_all_MSA) #308

#Verify duplicates at city level
UCR_all_MSA[UCR_all_MSA$main_city %in% UCR_all_MSA$main_city[duplicated(UCR_all_MSA$main_city) & !complete.cases(UCR_all_MSA)], c('main_city', 'main_state')]
#View(UCR_all_MSA[UCR_all_MSA$main_city %in% UCR_all_MSA$main_city[duplicated(UCR_all_MSA$main_city) & !complete.cases(UCR_all_MSA)], ])

#Texarkana needs to be coalesced
Texarkana <- UCR_all_MSA[UCR_all_MSA$main_city == "Texarkana", ]
Texarkana <- cbind(Texarkana[Texarkana$main_state == 'TX', (colnames(Texarkana) %in% c('main_city', 'main_state'))], 
                                                                              t(colSums(Texarkana[, !(colnames(Texarkana) %in% c('main_city', 'main_state'))], na.rm = TRUE)))

UCR_all_MSA_newTexarkana <- rbind(UCR_all_MSA[UCR_all_MSA$main_city != 'Texarkana', ], Texarkana)
    #Validate
    nrow(UCR_all_MSA) == nrow(UCR_all_MSA_newTexarkana) + 1
    UCR_all_MSA <- UCR_all_MSA_newTexarkana
    rm(UCR_all_MSA_newTexarkana)
    
    


# violent crime missing <=> rape missing?
for (i in 2010:2014) {
    print(paste0('year', i))
    print("missing violence not missing rape")
    miss_viol_not_rape <- nrow(UCR_all_MSA[(is.na(UCR_all_MSA[[paste0('violent_crime_', i)]]) & !is.na(UCR_all_MSA[[paste0('rape_', i)]])), ])
    print(miss_viol_not_rape)
    if (miss_viol_not_rape > 0) {
        print(UCR_all_MSA[(is.na(UCR_all_MSA[[paste0('violent_crime_', i)]]) & !is.na(UCR_all_MSA[[paste0('rape_', i)]])), ])
        }
    print("missing violence not missing rape")
    print("missing rape not missing violence")
    miss_rape_not_viol <- nrow(UCR_all_MSA[(!is.na(UCR_all_MSA[[paste0('violent_crime_', i)]]) & is.na(UCR_all_MSA[[paste0('rape_', i)]])), ])
    print(miss_rape_not_viol)
    if (miss_rape_not_viol > 0){
        print(UCR_all_MSA[(!is.na(UCR_all_MSA[[paste0('violent_crime_', i)]]) & is.na(UCR_all_MSA[[paste0('rape_', i)]])), ])
    }
    print("missing violence and missing rape")
    miss_viol_miss_rape <- nrow(UCR_all_MSA[(is.na(UCR_all_MSA[[paste0('violent_crime_', i)]]) & is.na(UCR_all_MSA[[paste0('rape_', i)]])), ])
    print(miss_viol_miss_rape)
    
}
#violence is missing when rape isn't; never reverse (there is never a case where rape is missing when violence isn't)
#most of the time they are missing together
#ie. missing rape => missing violence, but missing violence !=> missing rape (likely missing some other violence metric e.g. robbery/aggravated assault)
    

nrow(UCR_all_MSA) #397

sapply(UCR_all_MSA, FUN = function(x) sum(is.na(x)))
#40-70 missing for each crime var


# Save data
write.csv(UCR_all_MSA, paste0(workingDatasets, "UCR/Clean/UCR_all_msa.csv"), row.names = FALSE)
