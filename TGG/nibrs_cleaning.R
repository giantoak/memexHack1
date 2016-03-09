###############################################################################

# Client: Giant Oak
# Author(s): Tom Burr
# Date created: 2015 12 11
# Purpose: Clean NIBRS data

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
# Clean NIBRS data
################################################################################

# Read in NIBRS data
NIBRS_female_violence <- read.csv('http://giantoakmemex.s3.amazonaws.com/sex_ad_analysis/intermediate/female_violence_nibrs.csv')
NIBRS_violence <- read.csv('http://giantoakmemex.s3.amazonaws.com/sex_ad_analysis/intermediate/violence_nibrs.csv')
NIBRS_prostitution <- read.csv('http://giantoakmemex.s3.amazonaws.com/sex_ad_analysis/intermediate/prostitution_nibrs.csv')

# Convert census msa code to msa code
NIBRS_violence$census_msa_code<- substr(NIBRS_violence$census_msa_code,8,13)
NIBRS_female_violence$census_msa_code <- substr(NIBRS_female_violence$census_msa_code,8,13)
NIBRS_prostitution$census_msa_code <- substr(NIBRS_prostitution$census_msa_code,8,13)

# Keep only the number of incidents
NIBRS_violence$mean <- NULL
NIBRS_female_violence$mean <- NULL
NIBRS_prostitution$mean <- NULL

# Keep the overall incident count (all crimes) for only one set since they are identical
NIBRS_violence$size <- NULL
NIBRS_female_violence$size <- NULL

# Rename Variables
names(NIBRS_violence) <- c("MSA.No.","year","violence_NIBRS")
names(NIBRS_female_violence) <- c("MSA.No.","year","female_violence_NIBRS")
names(NIBRS_prostitution) <- c("MSA.No.","year","total_incidents_NIBRS","prostitution_NIBRS")

# Add in population
population_cbsa <- readRDS(file = paste0(workingDatasets, "population/population_cbsa.RDS"))
str(population_cbsa)

# Create wide version of NIBRS
NIBRS_prostitution_wide <- NIBRS_prostitution
colnames(NIBRS_prostitution_wide)[colnames(NIBRS_prostitution_wide) == "MSA.No."] <- "CBSA.Code"
reshape(NIBRS_prostitution_wide, idvar = "CBSA.Code", timevar = "year", direction = "wide") -> NIBRS_prostitution_wide

NIBRS_prostitution_wide <- merge(NIBRS_prostitution_wide, population_cbsa, by = "CBSA.Code", all.x = TRUE)

# Calculate rates
NIBRS_prostitution_wide$prostitution_rate_2011 <- NIBRS_prostitution_wide$prostitution_NIBRS.2011 / NIBRS_prostitution_wide$POPESTIMATE2011
NIBRS_prostitution_wide$prostitution_rate_2012 <- NIBRS_prostitution_wide$prostitution_NIBRS.2012 / NIBRS_prostitution_wide$POPESTIMATE2012
NIBRS_prostitution_wide$prostitution_rate_2013 <- NIBRS_prostitution_wide$prostitution_NIBRS.2013 / NIBRS_prostitution_wide$POPESTIMATE2013

NIBRS_prostitution_wide <- NIBRS_prostitution_wide[, c("CBSA.Code", "prostitution_rate_2011", "prostitution_rate_2012", "prostitution_rate_2013", "prostitution_NIBRS.2011", "prostitution_NIBRS.2012", "prostitution_NIBRS.2013")]

# Save wide version of NIBRS
write.csv(NIBRS_prostitution_wide, file = paste(workingDatasets,"Crime Data", "/NIBRS_prostitution_wide.csv", sep=""))

# Save NIBRS datasets
write.csv(NIBRS_prostitution, file = paste(workingDatasets, "Crime Data", "/NIBRS_prostitution.csv", sep=""))
write.csv(NIBRS_violence, file = paste(workingDatasets, "Crime Data", "/NIBRS_violence.csv", sep=""))
write.csv(NIBRS_female_violence, file = paste(workingDatasets, "Crime Data", "/NIBRS_female_violence.csv", sep=""))