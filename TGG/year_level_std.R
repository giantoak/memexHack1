###############################################################################

# Client: Giant Oak
# Author(s): Nicole Ozminkowski
# Date created: 2016 09 February
# Purpose: Clean year-level STD data set

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
librariesToLoad <- c("jsonlite", "ggplot2", "curl", "httr", 'data.table', 'foreign', 'readstata13')



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


# Load source files 
#Specify files with constructor functions here
sourceFiles <- c()

if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, x, ".R", sep = "")))

# End time for source files
end_source_files <- Sys.time()

################################################################################
# load in year-level STD dataset
################################################################################
# load in data
rawStdCountyYearLevel <- read.dta13(file = paste0(rawDatasets, '/2015-11-23 -- STD Data/std_all.dta'))
str(rawStdCountyYearLevel)
# validate level
nrow(rawStdCountyYearLevel) # 18809
length(unique(paste(rawStdCountyYearLevel$county, rawStdCountyYearLevel$state, sep = '-'))) # 3139 (out of 3149 counties in US)
names(rawStdCountyYearLevel)
summary(rawStdCountyYearLevel$year) # 2009 to 2014
length(unique(paste(rawStdCountyYearLevel$county, rawStdCountyYearLevel$state, rawStdCountyYearLevel$year, sep = '-'))) # 18809 - level established: county-year
# investigate missings
rawStdCountyYearLevel[rawStdCountyYearLevel$county=='', ] # (only one obs: WY and everything else is NA)
rawStdCountyYearLevel <- rawStdCountyYearLevel[rawStdCountyYearLevel$county!='', ]

# keep only complete cases
nrow(rawStdCountyYearLevel[complete.cases(rawStdCountyYearLevel), ])
nrow(rawStdCountyYearLevel)
sapply(rawStdCountyYearLevel, FUN = function(x) sum(is.na(x)))
rawStdCountyYearLevel <- rawStdCountyYearLevel[complete.cases(rawStdCountyYearLevel), ]
nrow(rawStdCountyYearLevel) #18772

################################################################################
# Merge with crosswalk file (TEXT MERGE)
################################################################################

# import crosswalk
crosswalk <- read.csv(paste0(rawDatasets, '2015-11-18 -- County MSA Crosswalk/crosswalk_cbsa_fips_county.csv'), stringsAsFactors = FALSE, colClasses = 'character', header = TRUE, skip = 2)

# Remove bad punctuation from variable names
removeBadPunc <- function(dataset) {
  colnames(dataset) <- tolower(gsub("/", "_", gsub("#", "Num", gsub(" ", "_", gsub("\\.","_",colnames(dataset))))))
  return(dataset)    
}
#Swaps / and spaces out with underscore, swaps # with Num

# apply bad punc function:
crosswalk <- removeBadPunc(crosswalk)

# fix level: remove last two rows of crosswalk (documentation)
crosswalk <- crosswalk[crosswalk$state_name!='',]
# check level:
length(unique(paste(crosswalk$state_name, crosswalk$county_county_equivalent, sep = '-'))) == nrow(crosswalk)
# true

# fix weird encoding of accents, etc
crosswalk$county_county_equivalent <- iconv(crosswalk$county_county_equivalent, from = 'UTF-8', to = 'UTF-8', sub = '')

# change all state names to abbreviations:
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='AL'] <- 'Alabama'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='AK'] <- 'Alaska'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='AZ'] <- 'Arizona'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='AR'] <- 'Arkansas'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='CA'] <- 'California'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='CO'] <- 'Colorado'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='CT'] <- 'Connecticut'

rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='DE'] <- 'Delaware'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='FL'] <- 'Florida'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='GA'] <- 'Georgia'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='HI'] <- 'Hawaii'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='ID'] <- 'Idaho'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='IL'] <- 'Illinois'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='IN'] <- 'Indiana'

rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='IA'] <- 'Iowa'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='KS'] <- 'Kansas'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='KY'] <- 'Kentucky'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='LA'] <- 'Louisiana'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='ME'] <- 'Maine'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='MD'] <- 'Maryland'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='MA'] <- 'Massachusetts'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='MI'] <- 'Michigan'

rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='MN'] <- 'Minnesota'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='MS'] <- 'Mississippi'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='MO'] <- 'Missouri'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='MT'] <- 'Montana'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='NE'] <- 'Nebraska'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='NV'] <- 'Nevada'

rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='NH'] <- 'New Hampshire'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='NJ'] <- 'New Jersey'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='NM'] <- 'New Mexico'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='NY'] <- 'New York'

rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='NC'] <- 'North Carolina'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='ND'] <- 'North Dakota'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='OH'] <- 'Ohio'

rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='OK'] <- 'Oklahoma'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='OR'] <- 'Oregon'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='PA'] <- 'Pennsylvania'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='RI'] <- 'Rhode Island'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='SC'] <- 'South Carolina'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='SD'] <- 'South Dakota'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='TN'] <- 'Tennessee'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='TX'] <- 'Texas'

rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='UT'] <- 'Utah'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='VT'] <- 'Vermont'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='VA'] <- 'Virginia'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='WA'] <- 'Washington'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='WV'] <- 'West Virginia'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='WI'] <- 'Wisconsin'
rawStdCountyYearLevel$state[rawStdCountyYearLevel$state=='WY'] <- 'Wyoming'

# sometimes 'city' has a lowercase c. to fix we will lower-case all names
rawStdCountyYearLevel$county <- tolower(rawStdCountyYearLevel$county)
rawStdCountyYearLevel$state <- tolower(rawStdCountyYearLevel$state)

crosswalk$county_county_equivalent <- tolower(crosswalk$county_county_equivalent)
crosswalk$state_name <- tolower(crosswalk$state_name)

# merge:
stdCountyYearLevel <- merge(rawStdCountyYearLevel, crosswalk, by.x = c('county', 'state'), by.y = c('county_county_equivalent', 'state_name'), all = TRUE)
nrow(stdCountyYearLevel[is.na(stdCountyYearLevel$chl_case),])# 102 in crosswalk not matched with go data
nrow(stdCountyYearLevel[is.na(stdCountyYearLevel$cbsa_code),]) # 8107 in go data but not crosswalk
nrow(stdCountyYearLevel[stdCountyYearLevel$state == 'puerto rico' & is.na(stdCountyYearLevel$chl_case), ]) # 74 from PR. 64 to be accounted for
nrow(unique(stdCountyYearLevel[stdCountyYearLevel$state != 'puerto rico' & is.na(stdCountyYearLevel$chl_case), c('county', 'state', 'chl_rate', 'cbsa_code')])) # 28 unique to be matched
unique(stdCountyYearLevel[stdCountyYearLevel$state != 'puerto rico' & is.na(stdCountyYearLevel$chl_case), c('county', 'state', 'chl_rate', 'cbsa_code')])
# all of these are either not in the year-level STD dataset or are NA in the year-level STD dataset
# thus, all mismatches have been accounted for! (the STD dataset just doesn't have data for 34 of these)

# next, aggregate to MSA level (need to bring in population for this)

################################################################################
# Merge with population file
################################################################################
# bring in population counts from 2010 to 2014 (what do we do about 2009??)
countyPop <- read.csv(paste0(rawDatasets, '2015-12-01 -- County Population/Census/CO-EST2014-alldata.csv'), stringsAsFactors = FALSE, colClasses = c('STATE' = 'character', 'COUNTY' = 'character'))

head(countyPop)
countyPop <- countyPop[, c('STATE', 'COUNTY', 'CTYNAME', 'POPESTIMATE2010', 'POPESTIMATE2011', 'POPESTIMATE2012', 'POPESTIMATE2013', 'POPESTIMATE2014')]
countyPop$fips <- paste0(countyPop$STATE, countyPop$COUNTY)

# reshape pop wide to long
countyPopLong <- reshape(data = countyPop, varying = c('POPESTIMATE2010', 'POPESTIMATE2011', 'POPESTIMATE2012', 'POPESTIMATE2013', 'POPESTIMATE2014'), direction = 'long', v.names = 'population', timevar = 'year', times = c('2010', '2011', '2012', '2013', '2014'), idvar = 'fips' )
head(countyPopLong)
nrow(countyPopLong) # 15965

# drop state totals in population dataset
countyPopLong <- countyPopLong[countyPopLong$COUNTY != '000', ]
nrow(countyPopLong) # 15710

# ensure countyPopLong is at the right level
nrow(countyPopLong) == length(unique(paste(countyPopLong$fips, countyPopLong$year, sep = '-'))) # TRUE

# create fips code variable
stdCountyYearLevel$fips <- paste0(stdCountyYearLevel$fips_state_code, stdCountyYearLevel$fips_county_code)
str(stdCountyYearLevel$fips) # character
# drop counties that weren't in the crosswalk file in stdCountyYearLevel
stdCountyYearLevel <- stdCountyYearLevel[!is.na(stdCountyYearLevel$fips_county_code), ]
summary(stdCountyYearLevel$fips)
head(stdCountyYearLevel)

# merge
stdPop <- merge(stdCountyYearLevel, countyPopLong, by.x = c('fips', 'fips_county_code', 'fips_state_code', 'year'), by.y = c('fips', 'COUNTY', 'STATE', 'year'), all.x = TRUE)

# validate merge
nrow(stdPop) # 10767
nrow(stdPop[is.na(stdPop$population) & !is.na(stdPop$year) & stdPop$year!='2009', ]) # 0
nrow(stdPop[complete.cases(stdPop), ])

################################################################################
# Aggregate stdPop to msa level
################################################################################
# keep only relevant variables
stdPop <- stdPop[, c('fips', 'fips_county_code', 'fips_state_code', 'year', 'county', 'state', 'chl_case', 'chl_rate', 'gon_case', 'gon_rate', 'syp1_case', 'syp1_rate', 'syp2_case', 'syp2_rate', 'cbsa_code', 'cbsa_title', 'population')]

# number of cases = number of cases/100,000 people * number of people/100,000 people
# check to make sure case rates we calculate are the same as the case rates in year-level STD dataset
stdPop$population <- as.numeric(stdPop$population)
stdPop$chl_case_check <- stdPop$chl_rate*stdPop$population/100000
head(stdPop)
# chl_case and chl_case_check are the same! (they should be)
# get rid of the check column
stdPop$chl_case_check <- NULL

# aggregate
stdPop_dt <- as.data.table(stdPop)
temp <- stdPop_dt[, .('total_chl'=sum(chl_case), 
                      'total_gon'=sum(gon_case),
                      'total_syp1'=sum(syp1_case),
                      'total_syp2'=sum(syp2_case),
                      'population'=sum(population),
                      'cbsa_name' =unique(cbsa_title)),
                    by=.(cbsa_code, year)]

stdAgg <- data.frame(temp)
head(stdAgg)

# create new msa-level case-rate variables
# cases/100,000 people = cases/population*100,000
stdAgg$chl_rate <- (stdAgg$total_chl/stdAgg$population)*100000
stdAgg$gon_rate <- (stdAgg$total_gon/stdAgg$population)*100000
stdAgg$syp1_rate <- (stdAgg$total_syp1/stdAgg$population)*100000
stdAgg$syp2_rate <- (stdAgg$total_syp2/stdAgg$population)*100000
head(stdAgg)

# double check level: cbsa-year level
nrow(stdAgg)==length(unique(paste(stdAgg$year,stdAgg$cbsa_code))) # TRUE

################################################################################
# validate with msa-level 2012 dataset
################################################################################
std_msa_level <- readRDS(file = paste0(workingDatasets, '/STD/STD_cbsa.RDS'))
head(std_msa_level)

# chlamydia:
summary(std_msa_level$chl_rate)
summary(stdAgg$chl_rate[stdAgg$year==2012])

quantile(std_msa_level$chl_rate, probs = seq(0, 1, .05), na.rm = TRUE)
quantile(stdAgg$chl_rate, probs = seq(0, 1, .05), na.rm = TRUE)

# gonorrhea:
summary(std_msa_level$gon_rate)
summary(stdAgg$gon_rate[stdAgg$year==2012])
summary(stdAgg$gon_rate[stdAgg$year==2012 & stdAgg$gon_rate!=0])
# 2013 just out of curiosity
summary(stdAgg$gon_rate[stdAgg$year==2013])

# where are there zero gonorrhea cases?
stdAgg[stdAgg$year==2012 & stdAgg$gon_rate==0 & !is.na(stdAgg$gon_rate),]

# compute differences 
# only keep 2012:
stdAgg12 <- stdAgg[stdAgg$year==2012, ]
nrow(stdAgg12) # 937
# merge:
compare <- merge(stdAgg12, std_msa_level, by.x = c('cbsa_code', 'cbsa_name'), by.y = c('CBSA', 'cbsa_title'), all = TRUE)
# validate:
nrow(stdAgg12) # 937
nrow(std_msa_level) # 917
nrow(compare) # 948

# calculate pct differences
compare$pctDiffGon <- (compare$gon_rate.x - compare$gon_rate.y)/compare$gon_rate.y
compare$pctDiffChl <- (compare$chl_rate.x - compare$chl_rate.y)/compare$chl_rate.y

summary(compare$pctDiffGon)
summary(compare$pctDiffChl)

qplot(compare$pctDiffGon, geom="histogram")
qplot(compare$pctDiffChl, geom="histogram")

# save the dataset
write.csv(stdAgg, file = paste0(finalDatasets, '/STD/year_level_std.csv'), row.names = FALSE)

