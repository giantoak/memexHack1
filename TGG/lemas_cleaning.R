###############################################################################

# Client: Giant Oak
# Author(s): Tom Burr
# Date created: 2015 11 03
# Purpose: Clean LEMAS data

################################################################################
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

code <- getwd()


################################################################################
# load in libraries and source files
################################################################################

# libraries:
librariesToLoad <- c("stringr", "dplyr")



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

detach("package:plyr", unload = TRUE)
search()


# load source files

sourceFiles <- NULL

if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, "/ucr_lemas/", x, ".R", sep = "")))


################################################################################
# Functions
################################################################################

leading_zeroes <- function(x,numchar){
  while(nchar(x) < numchar) {
    x <- paste("0",x,sep="")
  }
  return(x)
}


single_mode <- function(x) {
  names(sort(-table(x)))[1]
}


################################################################################
# Creating city-level 2013 Lemas dataset
################################################################################

# Read in Giant Oak UCR/LEMAS Dataset to examine it (we will be recreating a new set from scratch)
ucr_lemas <- read.csv('http://giantoakmemex.s3.amazonaws.com/sex_ad_analysis/input/ucr_lemas_msa.csv', stringsAsFactors = FALSE)

# Current data is at city-state level (238) rather than msa (141)
length(unique(ucr_lemas$msa_parents))

ucr_lemas$city_state <- paste(ucr_lemas$city,ucr_lemas$state,sep=", ")

length(unique(ucr_lemas$city_state))
length(unique(ucr_lemas$city_state)) == nrow(ucr_lemas)

# Read in the LEMAS 2013 Data
lemas_2013 <- read.table(paste0(rawDatasets, "2015-11-23 -- LEMAS/2013/ICPSR_36164/DS0001/36164-0001-Data.tsv"),
                         sep="\t", header=TRUE, fill = TRUE, quote = "", stringsAsFactors = FALSE)

# Select variables to keep
keeps <- c("CITY","STATECODE","FTSWORN","ZIPCODE", "POP2012")
lemas_2013 <- lemas_2013[,keeps]

# Create main_city and main_state variables to merge with UCR Dataset later
names(lemas_2013) <- c("main_city","main_state","ftsworn","zip","population_2012")

# Create city_state variable
lemas_2013$city_state <- paste(lemas_2013$main_city,lemas_2013$main_state,sep=", ")

# Data is unique at the agency, not the city level, so we must combine police forces (e.g., sheriff and police)
length(unique(lemas_2013$city_state)) #2537 cities
nrow(lemas_2013) # 2826 police forces

# Clean city names

# Replace St. with Saint for primary city
lemas_2013$main_city <- gsub("St. ", "Saint ", lemas_2013$main_city)

# Strip punctuation from primary city, get rid of leading/trailing white space
lemas_2013$main_city <- gsub("[[:punct:]]", " ", lemas_2013$main_city)
lemas_2013$main_city <- str_trim(lemas_2013$main_city)


# Combine police forces to city level

# Some counties have multiple forces, e.g. police dept. vs sherriff's 
lemas_city_level <-
  lemas_2013 %>%
  group_by(main_city,main_state) %>%
  summarise(ftsworn_2013 = sum(ftsworn),
            zip =max(zip),
            city_state = single_mode(city_state),
            population_2012 = max(population_2012))


length(unique(lemas_city_level$city_state)) == nrow(lemas_city_level)

# Validate counts
sum(lemas_city_level$ftsworn_2013) == sum(lemas_2013$ftsworn)


################################################################################
# Creating msa-level ftsworn counts
################################################################################

# Import file linking zip codes and MSA numbers
zip_msa <- read.table(paste0(rawDatasets, "2015-11-18 -- County MSA Crosswalk/zip_msa.txt"),
                      sep ="\t", header=TRUE, fill = TRUE, quote = "", colClasses = 'character')

zip_msa$zip <- zip_msa$ZIP.CODE
zip_msa$ZIP.CODE <- NULL

# Remove duplicated zip codes
zip_msa$duplicated <- duplicated(zip_msa$zip) #All NAs, can delete
zip_msa <- zip_msa[!zip_msa$duplicated,]
length(unique(zip_msa$zip)) == nrow(zip_msa)
zip_msa <- zip_msa[,c("zip","MSA.No.")]

# Attempt merge, showing missing matches that need to be corrected
lemas_city_level_zipmerged <- merge(lemas_city_level, zip_msa, by="zip", all.x = TRUE)
lemas_city_level_zipmerged[is.na(lemas_city_level_zipmerged$MSA.No.),]

# Fix cities that are either missing or misnumbered

# Fix 7 incorrect zip codes
lemas_city_level$zip[lemas_city_level$city_state=="Bixby, OK"] <- 74008
lemas_city_level$zip[lemas_city_level$city_state=="Silverton, OH"] <- 45236
lemas_city_level$zip[lemas_city_level$city_state=="Jefferson, WI"] <- 53549
lemas_city_level$zip[lemas_city_level$city_state=="Tangipahoa, LA"] <- 70465
lemas_city_level$zip[lemas_city_level$city_state=="East Saint Louis, IL"] <- 62201
lemas_city_level$zip[lemas_city_level$city_state=="Fort Pierce, FL"] <- 34945
lemas_city_level$zip[lemas_city_level$city_state=="O'Fallon, MO"] <- 63366

# Fix 7 missing zip codes
lemas_city_level$zip[lemas_city_level$city_state=="Saint Paul, MN"] <- 55101
lemas_city_level$zip[lemas_city_level$city_state=="Cleveland, TN"] <- 37311
lemas_city_level$zip[lemas_city_level$city_state=="Palatka, FL"] <- 32177
lemas_city_level$zip[lemas_city_level$city_state=="Philipsburg, MT"] <- 59858
lemas_city_level$zip[lemas_city_level$city_state=="Minden, NV"] <- 89423
lemas_city_level$zip[lemas_city_level$city_state=="Salt Lake City, UT"] <- 84101
lemas_city_level$zip[lemas_city_level$city_state=="Pensacola, FL"] <- 32501


# Perform actual merge, verifying no errors
lemas_city_level_zipmerged <- merge(lemas_city_level, zip_msa, by="zip", all.x = TRUE)

lemas_city_level_zipmerged[is.na(lemas_city_level_zipmerged$MSA.No.),]
length(unique(lemas_city_level_zipmerged$MSA.No.))
# 379 MSAs represented

# Validate
nrow(lemas_city_level) == nrow(lemas_city_level_zipmerged)
str(lemas_city_level_zipmerged)
lemas_city_level <- lemas_city_level_zipmerged
rm(lemas_city_level_zipmerged)


# Update MSAs

# 21 of our MSA numbers appear outdated, bringing in an update file so we can try merging again
msa_changes <- read.csv(paste0(workingDatasets,"Crosswalks/msa_changes.csv",sep=""), stringsAsFactors=FALSE)

lemas_city_level <- merge(lemas_city_level,msa_changes,by="MSA.No.",all.x=TRUE)
lemas_city_level$updated_msa <- lemas_city_level$MSA.No.
index <- !is.na(lemas_city_level$msa_new)
lemas_city_level$updated_msa[index] <- lemas_city_level$msa_new[index]

#The updated msa number is different in 242 cases
summary(lemas_city_level$updated_msa == lemas_city_level$msa_new)

lemas_city_level$MSA.No. <- lemas_city_level$updated_msa
lemas_city_level$updated_msa <- NULL
lemas_city_level$temp <- NULL
lemas_city_level$msa_new <- NULL

# Create MSA-level FTSWORN

# lemas_city_level$ftsworn_2013[is.na(lemas_city_level$ftsworn_2013)] <- 0

msa_level_ftsworn <-
  lemas_city_level %>%
  group_by(MSA.No.) %>%
  summarise(ftsworn_2013_msa = sum(ftsworn_2013))

#Validate
length(unique(lemas_city_level[['MSA.No.']])) == nrow(msa_level_ftsworn)

# Merge back into city level
lemas_city_level<- merge(lemas_city_level,msa_level_ftsworn,by="MSA.No.")


# Validate
table(lemas_city_level$main_city)

#Write City Level File

write.csv(lemas_city_level,file = paste(workingDatasets,"LEMAS","/lemas_2013_tgg.csv",sep=""), row.names = FALSE)

#Drop Duplicates for MSA-level File

lemas_msa_level <- lemas_city_level[,c("MSA.No.","ftsworn_2013_msa")]

lemas_msa_level <- unique(lemas_msa_level)

#Validation
length(unique(lemas_msa_level$MSA.No.)) == nrow(lemas_msa_level)

write.csv(lemas_msa_level,file = paste(workingDatasets,"LEMAS","/lemas_2013_msa_tgg.csv",sep=""), row.names = FALSE)

