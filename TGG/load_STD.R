###############################################################################

# Client: Giant Oak
# Author(s): Nicole Ozminkowski
# Date created: 2015 11 12
# Purpose: Upload/Clean STD Data from Health Indicators Warehouse

###############################################################################
# Set the working directory and default paths
################################################################################
clientName <- 'Giant Oak'
projectName <- '01 Human Trafficking'
serverPath <- '~/Shared/00 Clients - Current'

rawDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                     '/Structured Data/01 Raw Datasets/', sep="")
workingDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                         '/Structured Data/02 Working Datasets/xdata/STD/', sep="")
finalDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                       '/Structured Data/03 Final Datasets/xdata/STD/', sep="")
graphics <- paste(serverPath, '/', clientName, '/', projectName,
                  '/Structured Data/04 Graphics and Output Data/xdata/STD/', sep="")

#code <- '~/Home/Git/'
code <- getwd()

################################################################################
# load in libraries and source files
################################################################################

# libraries:
librariesToLoad <- c("jsonlite", "ggplot2", "curl", "httr", 'data.table')



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
# load in STD data
################################################################################

chlamydia <- read.csv(file = paste0(rawDatasets, 
                                    '2015-11-23 -- STD Data/chlamydia.csv'), stringsAsFactors = FALSE, header = FALSE)
gonorrhea <- read.csv(file = paste0(rawDatasets, 
                                    '2015-11-23 -- STD Data/gonorrhea.csv'), stringsAsFactors = FALSE, header = FALSE)


# rename variables
names(chlamydia) <- c('dimension', 'dimension_value', 'locale', 'county', 
                      'y2007', 'y2008', 'y2009', 'y2010', 'y2011', 'y2012')
names(gonorrhea) <- c('dimension', 'dimension_value', 'locale', 'county', 
                      'y2007', 'y2008', 'y2009', 'y2010', 'y2011', 'y2012')


# keep only most recent year(s) (2012 adn 2013 if it exists)
chlamydia <- chlamydia[, c('locale', 'county', 'y2012')]
gonorrhea <- gonorrhea[, c('locale', 'county', 'y2012')]

# check the level of the datasets
# there are 3,143 counties in US (census website)
# http://censtats.census.gov/usa/usainfo.shtml
length(unique(paste(chlamydia$county, chlamydia$locale, sep = '-'))) # == nrow(chlamydia)
nrow(chlamydia) # 3193 (too many!)
3193 - 3143 # = 50

length(unique(paste(gonorrhea$county, gonorrhea$locale, sep = '-')))
nrow(gonorrhea) # 3196
3196 - 3143 # = 53


# get rid of state and national totals (anything without a county listed)
chlamydia <- chlamydia[chlamydia$county!="", ]
head(chlamydia)
nrow(chlamydia) # 3142 (because DC was included as a state making it 51)

gonorrhea <- gonorrhea[gonorrhea$county!="", ]
head(gonorrhea)
nrow(gonorrhea) # 3144

# determine why gonorrhea data has 2 extra counties:
sum(paste(gonorrhea$county, gonorrhea$locale, sep = '-') %in% paste(chlamydia$county, chlamydia$locale, sep = '-')) # 3142
gonorrhea[!(paste(gonorrhea$county, gonorrhea$locale, sep = '-') %in% paste(chlamydia$county, chlamydia$locale, sep = '-')), ]
# gonorrhea includes richland and saluda, south carolina. chlamydia does not. 
# both include Prince of Wales - Outer Ketchikan, Alaska. Not considered by census to be a county,
# which is why gonorrhea totals 3144, while census reports that there are only 3143 counties in the US


# check missing values (NA)
table(is.na(chlamydia))
table(is.na(gonorrhea))
# no missing values

# check missing values ('')
table(chlamydia$y2012=='') # 2 
table(gonorrhea$y2012=='') # 2


chlamydia[chlamydia$y2012=='',] 
# dimension dimension_value locale                          county y2007 y2008 y2009 y2010 y2011 y2012
# 90               Total Alaska Prince of Wales-Outer Ketchikan 144.3 361.5                        
# 97               Total Alaska             Wrangell-Petersburg 282.9 355.3    

gonorrhea[gonorrhea$y2012=='',]
# dimension dimension_value locale                          county y2007 y2008 y2009 y2010 y2011 y2012
# 91               Total Alaska Prince of Wales-Outer Ketchikan  90.2   DSU                        
# 98               Total Alaska             Wrangell-Petersburg   DSU   DSU  


# recode DSU to NA
# chlamydia
is.na(chlamydia) <- chlamydia == 'DSU'
table(is.na(chlamydia)) # 183 missing
# gonorrhea
is.na(gonorrhea) <- gonorrhea == 'DSU'
table(is.na(gonorrhea)) # 1127 missing

# validate
head(chlamydia)
head(gonorrhea)


# change to numeric
# chlamydia
chlamydia$y2012 <- gsub(',', '', chlamydia$y2012)
chlamydia$y2012 <- as.numeric(chlamydia$y2012) 
table(is.na(chlamydia$y2012)) # 185 (should be 183)
# the two extra missings were just '' rather than NA, so it's good that
# they are now categorized as missing
# gonorrhea
gonorrhea$y2012 <- as.numeric(gonorrhea$y2012)
table(is.na(gonorrhea$y2012)) # 1129 (should be 1127)
# same issue: two were coded as '' and are now coded as NA

# save county level datasets
write.table(chlamydia, file = paste0(workingDatasets, '/chlamydia_county.csv'))
write.table(gonorrhea, file = paste0(workingDatasets, '/gonorrhea_county.csv'))



################################################################################
# merge to one data set and look at correlations between diseases
################################################################################

names(gonorrhea)[names(gonorrhea) == 'y2012'] <- 'gon_2012'
names(chlamydia)[names(chlamydia) == 'y2012'] <- 'chl_2012'


# merge gon and chl
STD.df <- merge(gonorrhea, chlamydia, by = c('county', 'locale'), all = TRUE)
nrow(STD.df) # 3144
table(is.na(STD.df$gon_2012)) # 1129 na
table(is.na(STD.df$chl_2012)) # 187 na (2 more than before because 2 additional gon counties)

# rename for merging with HIV later on
STD_for_hiv <- STD.df

# correlation between gonorrhea and chlamydia
cor(STD.df$gon_2012, STD.df$chl_2012, use = 'pairwise.complete.obs') # 0.867

################################################################################
# Merge with crosswalk file
################################################################################

# import crosswalk
crosswalk <- read.csv(paste0(rawDatasets, '2015-11-18 -- County MSA Crosswalk/crosswalk_cbsa_fips_county.csv'), stringsAsFactors = FALSE, colClasses = 'character', header = TRUE, skip = 2)

# Function: Remove bad punctuation from variable names
removeBadPunc <- function(dataset) {
  colnames(dataset) <- tolower(gsub("/", "_", gsub("#", "Num", gsub(" ", "_", gsub("\\.","_",colnames(dataset))))))
  return(dataset)    
}
#Swaps / and spaces out with underscore, swaps # with Num

# apply bad punc function:
crosswalk <- removeBadPunc(crosswalk)

# fix level: remove last two rows of crosswalk (documentation)
crosswalk <- crosswalk[crosswalk$state_name!='',]
length(unique(paste(crosswalk$state_name, crosswalk$county_county_equivalent, sep = '-'))) == nrow(crosswalk)
# true

# fix weird encoding of accents, etc
crosswalk$county_county_equivalent <- iconv(crosswalk$county_county_equivalent, from = 'UTF-8', to = 'UTF-8', sub = '')

# rename crosswalk county so that it matches with STD data sets 
# replace 'County' with ''
crosswalk$county_county_equivalent <- gsub('County', '', crosswalk$county_county_equivalent)
# replace 'Parish' with ''
crosswalk$county_county_equivalent <- gsub('Parish', '', crosswalk$county_county_equivalent)
# replace 'Municipio' and 'Municipality' with ''
crosswalk$county_county_equivalent <- gsub('Municipio', '', crosswalk$county_county_equivalent)
crosswalk$county_county_equivalent <- gsub('Municipality', '', crosswalk$county_county_equivalent)
# replace 'City' with ''
crosswalk$county_county_equivalent <- gsub('City', '', crosswalk$county_county_equivalent)
crosswalk$county_county_equivalent <- gsub('city', '', crosswalk$county_county_equivalent)
# replace 'Borough' with ''
crosswalk$county_county_equivalent <- gsub('Borough', '', crosswalk$county_county_equivalent)
nrow(crosswalk) # 1882

# fix merge errors:
#   washington DC:
#   change std set to say district of columbia for county
STD.df$county[STD.df$locale=='District of Columbia'] <- 'District of Columbia'
#   dona ana, NM
crosswalk$county_county_equivalent[crosswalk$county_county_equivalent == 'Doa Ana '] <- 'Dona Ana'
#   juneau
crosswalk$county_county_equivalent[crosswalk$county_county_equivalent == 'Juneau  and '] <- 'Juneau' 
crosswalk$county_county_equivalent[crosswalk$state_name == 'Alaska']
#   add city back in for carson, nevada; charles, virginia; james, virginia
crosswalk$county_county_equivalent[crosswalk$county_county_equivalent == 'Carson ' & crosswalk$state_name == 'Nevada'] <- 'Carson City'
crosswalk$county_county_equivalent[crosswalk$county_county_equivalent == 'Charles  ' & crosswalk$state_name == 'Virginia'] <- 'Charles City'
crosswalk$county_county_equivalent[crosswalk$county_county_equivalent == 'James  ' & crosswalk$state_name == 'Virginia'] <- 'James City'

# keep only relevant columns of both datasets
# crosswalk:
crosswalk <- crosswalk[, c('county_county_equivalent', 'cbsa_code', 'state_name', 'fips_state_code', 'fips_county_code', 'cbsa_title')]
nrow(crosswalk) # 1882


# merge on county and state names: make sure theese match fully:
#   make sure there are no extra spaces in county name or in state name in either dataset

head(crosswalk)

#lower case all names 
crosswalk$county_county_equivalent <- tolower(crosswalk$county_county_equivalent)
crosswalk$state_name <- tolower(crosswalk$state_name)

STD.df$county <- tolower(STD.df$county)
STD.df$locale <- tolower(STD.df$locale)

# get rid of spaces
crosswalk$county_county_equivalent <- gsub(' ', '', crosswalk$county_county_equivalent)
crosswalk$state_name <- gsub(' ', '', crosswalk$state_name)

STD.df$county <- gsub(' ', '', STD.df$county)
STD.df$locale <- gsub(' ', '', STD.df$locale)

# get rid of any hyphens
crosswalk$county_county_equivalent <- gsub('-', '', crosswalk$county_county_equivalent)
crosswalk$state_name <- gsub('-', '', crosswalk$state_name)

STD.df$county <- gsub('-', '', STD.df$county)
STD.df$locale <- gsub('-', '', STD.df$locale)

# get rid of apostrophes
crosswalk$county_county_equivalent <- gsub("'", '', crosswalk$county_county_equivalent)
crosswalk$state_name <- gsub("'", '', crosswalk$state_name)

STD.df$county <- gsub("'", '', STD.df$county)
STD.df$locale <- gsub("'", '', STD.df$locale)

# get rid of periods
crosswalk$county_county_equivalent <- gsub('\\.', '', crosswalk$county_county_equivalent)
crosswalk$state_name <- gsub('\\.', '', crosswalk$state_name)

STD.df$county <- gsub('\\.', '', STD.df$county)
STD.df$locale <- gsub('\\.', '', STD.df$locale)



# add an indicator for STD (for merge validation)
STD.df$indicator <- 1

nrow(crosswalk) # 1882 
nrow(STD.df) # 3144

STD_merged <- merge(STD.df, crosswalk, by.x = c('county', 'locale'), by.y = c('county_county_equivalent', 'state_name'), all = TRUE)
sum(is.na(STD_merged$county)) # 0
length(unique(crosswalk$cbsa_code)) #929 - all cbsa's are accounted for


# validate the merge
nrow(STD_merged) #3223
sum(is.na(STD_merged$chl_2012)) # 261
sum(is.na(STD.df$chl_2012)) # 187

# 261-187 = 74: these are all in PR:
STD_merged[is.na(STD_merged$indicator),] # in crosswalk but not in STD set, 74 of them
sum(is.na(STD_merged$cbsa_code)) # 1341 in STD set but not in crosswalk
sum(is.na(STD_merged$cbsa_code)) / nrow(STD_merged) #42% of counties are missing CBSAs

nrow(STD_merged[!(STD_merged$locale %in% STD.df$locale), ]) # 74
nrow(unique(STD_merged)) # 3223

STD_merged[duplicated(paste(STD_merged$county, STD_merged$locale, sep = '-')),]
# 5 false duplicates: Bedford City, VA got renamed to Bedford earlier on; however,
# Bedford, VA also exists, so now we have two Bedford, VA's and no Bedford City.
# So, we need fix that. 

# The following cities have the same issue as Bedford, VA: their CBSA codes are
# hand-coded in the comments
STD_merged[STD_merged$county == 'baltimorecity', ]
# cbsa code: 12580
STD_merged[STD_merged$county == 'bedfordcity', ] 
# cbsa code: 31340
STD_merged[STD_merged$county == 'fairfaxcity', ] 
# cbsa code: 47900
STD_merged[STD_merged$county == 'roanokecity', ] 
# cbsa code: 40220
STD_merged[STD_merged$county == 'stlouiscity', ] 
# cbsa code: 41180


# code in correct geographic information for nonmatches

# baltimore:
# put in correct geographic information for baltimore city
STD_merged$cbsa_code[STD_merged$county == 'baltimorecity'] <- '12580'
STD_merged$fips_state_code[STD_merged$county == 'baltimorecity'] <- '24'
STD_merged$fips_county_code[STD_merged$county == 'baltimorecity'] <- '510'
# remove obs that has baltimore as the county but baltimore city's fips code (this was an incorrect merge)
STD_merged$county[(STD_merged$county == 'baltimore' & STD_merged$fips_county_code == '510') ] <- 'drop_me'
# baltimore city in same cbsa as baltimore
STD_merged$cbsa_title[STD_merged$county == 'baltimorecity'] <- STD_merged$cbsa_title[STD_merged$county=='baltimore']


# bedford:
STD_merged$cbsa_code[STD_merged$county == 'bedfordcity'] <- '31340'
STD_merged$fips_state_code[STD_merged$county == 'bedfordcity'] <- '51'
STD_merged$fips_county_code[STD_merged$county == 'bedfordcity'] <- '515'
STD_merged$county[(STD_merged$county == 'bedford' & STD_merged$fips_county_code == '515') ] <- 'drop_me'
STD_merged$cbsa_title[STD_merged$county == 'bedfordcity'] <- STD_merged$cbsa_title[STD_merged$county=='bedford' & STD_merged$locale == 'virginia']

# fairfax
STD_merged$cbsa_code[STD_merged$county == 'fairfaxcity'] <- '47900'
STD_merged$fips_state_code[STD_merged$county == 'fairfaxcity'] <- '51'
STD_merged$fips_county_code[STD_merged$county == 'fairfaxcity'] <- '600'
STD_merged$county[(STD_merged$county == 'fairfax' & STD_merged$fips_county_code == '600') ] <- 'drop_me'
STD_merged$cbsa_title[STD_merged$county == 'fairfaxcity'] <- STD_merged$cbsa_title[STD_merged$county=='fairfax' & STD_merged$locale == 'virginia']

# roanoke
STD_merged$cbsa_code[STD_merged$county == 'roanokecity'] <- '40220'
STD_merged$fips_state_code[STD_merged$county == 'roanokecity'] <- '51'
STD_merged$fips_county_code[STD_merged$county == 'roanokecity'] <- '770'
STD_merged$county[(STD_merged$county == 'roanoke' & STD_merged$fips_county_code == '770') ] <- 'drop_me'
STD_merged$cbsa_title[STD_merged$county == 'roanokecity'] <- STD_merged$cbsa_title[STD_merged$county=='roanoke']

# st louis
STD_merged$cbsa_code[STD_merged$county == 'stlouiscity'] <- '41180'
STD_merged$fips_state_code[STD_merged$county == 'stlouiscity'] <- '29'
STD_merged$fips_county_code[STD_merged$county == 'stlouiscity'] <- '510'
STD_merged$county[(STD_merged$county == 'stlouis' & STD_merged$fips_county_code == '510') ] <- 'drop_me'
STD_merged$cbsa_title[STD_merged$county == 'stlouiscity'] <- STD_merged$cbsa_title[STD_merged$county=='stlouis' & STD_merged$locale == 'missouri']


# drop those with 'drop_me' as the county
nrow(STD_merged) # 3223
STD_merged <- STD_merged[STD_merged$county!='drop_me', ]
nrow(STD_merged) # 3218

# verify number of rows (74 from puerto rico)
nrow(STD_merged) - 74 == nrow(STD.df) # TRUE

# verify level of the dataset to make sure the right obvs were kept
nrow(STD_merged) == length(unique(paste(STD_merged$county, STD_merged$locale, sep = '-'))) # TRUE

# need to aggregate to CSBA level since there is more than one county in some CBSAs 
# aggregation done in agg_STD.R

# save the dataset
saveRDS(STD_merged, file = paste0(workingDatasets, 'STD_unaggregated.RDS'))


################################################################################
# Explore Giant Oak's STD dataset
################################################################################

# read in Giant Oak STD data set
GO_STD <- read.table(file = paste0(rawDatasets, 
                                   '/2015-11-23 -- STD Data/From GO/std.tsv'), 
                     header = TRUE, sep = '\t')
# cases = number of cases in that MSA
# rate = number of cases per 100,000 population (this matches our units)

# check level: assumed year-CBSA-disease
nrow(GO_STD) == length(unique(paste(GO_STD$CBSA, GO_STD$Year, GO_STD$Disease, sep = '-'))) # TRUE

# look at county representation
length(unique(GO_STD$CBSA))/929 # 5% (50 CBSA's represented)
table(GO_STD$Disease=='Chlamydia') # 250 
table(GO_STD$Disease=='Gonorrhea') # 250
table(GO_STD$Disease=='Syphilis')  # 250
table(GO_STD$Disease)
# makes sense because there is one entry per disease per year per MSA


# overall summary
summary(GO_STD$Rate)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.7    23.1   122.8   205.8   385.4  1021.0 

# chlamydia summary
summary(GO_STD$Rate[GO_STD$Disease=='Chlamydia' & GO_STD$Year==2012])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 246.8   417.9   476.9   487.5   534.1   949.8 

# gonorrhea summary
summary(GO_STD$Rate[GO_STD$Disease=='Gonorrhea' & GO_STD$Year==2012])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 30.40   93.45  121.60  127.60  154.00  335.20 

# syphilis summary
summary(GO_STD$Rate[GO_STD$Disease=='Syphilis' & GO_STD$Year==2012])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.70   12.30   16.90   19.45   24.62   45.00 

# reshape data: long to wide

GO_chl <- GO_STD[GO_STD$Disease=='Chlamydia' & GO_STD$Year==2012, c('Rate', 'CBSA', 'Name')]
GO_gon <- GO_STD[GO_STD$Disease=='Gonorrhea' & GO_STD$Year==2012, c('Rate', 'CBSA', 'Name')]
GO_syp <- GO_STD[GO_STD$Disease=='Syphilis' & GO_STD$Year==2012, c('Rate', 'CBSA', 'Name')]

GO_chl$chl_rate <- GO_chl$Rate
GO_gon$gon_rate <- GO_gon$Rate
GO_syp$syp_rate <- GO_syp$Rate

GO_chl$Rate <- NULL
GO_gon$Rate <- NULL
GO_syp$Rate <- NULL

# merge
GO_disease_corr <- merge(GO_chl, GO_gon, by = c('Name', 'CBSA'))
GO_disease_corr <- merge(GO_disease_corr, GO_syp)

# validate merge
nrow(GO_chl) # 50
nrow(GO_gon) # 50
nrow(GO_syp) # 50
nrow(GO_disease_corr) # 50
table(is.na(GO_disease_corr)) # all false

# correlations
cor(GO_disease_corr$chl_rate, GO_disease_corr$gon_rate) # 0.8623
cor(GO_disease_corr$chl_rate, GO_disease_corr$syp_rate) # 0.2583
cor(GO_disease_corr$gon_rate, GO_disease_corr$syp_rate) # 0.2531


################################################################################
# HIV data 
# HIV deaths data is very incomplete (and instance of death is different from 
# instance of disease for chl and gon), so we do not use it in analysis
################################################################################
# 
# # bring in data
# hiv_death <- read.csv(file = paste0(rawDatasets, 
#                                     '2015-11-23 -- STD Data/hiv.csv'), stringsAsFactors = FALSE, 
#                       fileEncoding = 'latin1', header = FALSE)
# 
# # rename variables
# names(hiv_death) <- c('dimension', 'dimension_value', 'locale', 'county', 
#                       'y2008_2012', 'y2010_2012', 'y2012', 'y2007_2013', 
#                       'y2009_2013', 'y2011_2013', 'y2013')
# 
# # keep only location and rate variables
# hiv_death <- hiv_death[, c('locale', 'county', 'y2008_2012', 'y2010_2012', 'y2012', 'y2007_2013', 'y2009_2013', 'y2011_2013', 'y2013')]
# 
# 
# # check level of hiv dataset
# length(unique(paste(hiv_death$county, hiv_death$locale, sep = '-'))) # 3176
# nrow(hiv_death) # 3611
# 3611 - 3143 # 468
# 3176 - 3143 # 33
# # this is because hiv_death has a bunch of rows that are blank except for uncertainty ranges
# 
# # take out blank uncertainty rows:
# hiv_death <- hiv_death[hiv_death$county!="", ]
# head(hiv_death)
# nrow(hiv_death) # 3123 
# hiv_death[!(paste(hiv_death$county, hiv_death$locale, sep = '-') %in% paste(chlamydia$county, chlamydia$locale, sep = '-')), ] 
# # hiv_death has no counties that are not also in chlamydia
# 
# # verify level issue has been resolved:
# length(unique(paste(hiv_death$county, hiv_death$locale, sep = '-'))) == nrow(hiv_death) # TRUE
# 
# # check missing
# table(is.na(hiv_death))
# table(hiv_death$y2012=='') # 0
# 
# # recode DSU to missing
# is.na(hiv_death) <- (hiv_death == 'DSU')
# table(is.na(hiv_death)) # 20393 missing 
# 
# # remove y from rates
# hiv_death$y2012 <- substr(hiv_death$y2012, 1, nchar(hiv_death$y2012) - 1)
# hiv_death$y2008_2012 <- substr(hiv_death$y2008_2012, 1, 
#                                nchar(hiv_death$y2008_2012) -1)
# hiv_death$y2010_2012 <- substr(hiv_death$y2010_2012, 1, 
#                                nchar(hiv_death$y2010_2012) -1)
# hiv_death$y2013 <- substr(hiv_death$y2013, 1, nchar(hiv_death$y2013) -1)
# hiv_death$y2007_2013 <- substr(hiv_death$y2007_2013, 1, 
#                                nchar(hiv_death$y2007_2013) -1)
# hiv_death$y2009_2013 <- substr(hiv_death$y2009_2013, 1, 
#                                nchar(hiv_death$y2009_2013) -1)
# hiv_death$y2011_2013 <- substr(hiv_death$y2011_2013, 1, 
#                                nchar(hiv_death$y2011_2013) -1)
# head(hiv_death)
# 
# # determine county representation for hiv deaths - see which variable 
# # has the least NAs 
# table(is.na(hiv_death$y2008_2012)) # 2829
# table(is.na(hiv_death$y2010_2012)) # 2930
# table(is.na(hiv_death$y2012)) # 3055
# table(is.na(hiv_death$y2007_2013)) # 2737
# table(is.na(hiv_death$y2009_2013)) #2841
# table(is.na(hiv_death$y2013)) # 3071
# # all are about the same size
# nrow(hiv_death) # 3123
# 2936/3610 # 81%
# 
# # change to numeric
# hiv_death$y2008_2012 <- as.numeric(hiv_death$y2008_2012)
# hiv_death$y2010_2012 <- as.numeric(hiv_death$y2010_2012)
# hiv_death$y2007_2013 <- as.numeric(hiv_death$y2007_2013)
# hiv_death$y2009_2013 <- as.numeric(hiv_death$y2009_2013)
# hiv_death$y2011_2013 <- as.numeric(hiv_death$y2011_2013)
# hiv_death$y2012 <- as.numeric(hiv_death$y2012)
# hiv_death$y2013 <- as.numeric(hiv_death$y2013)
# 
# # save county level dataset
# write.table(hiv_death, file = paste0(workingDatasets, '/hiv_deaths_county.csv'))
# 
# # prep hiv_death for merge
# hiv_death$county <- tolower(hiv_death$county)
# hiv_death$locale <- tolower(hiv_death$locale)
# hiv_death$county <- gsub(' ', '', hiv_death$county)
# hiv_death$locale <- gsub(' ', '', hiv_death$locale)
# hiv_death$in_hiv_death <- 1
# # merge in hiv_death
# STD_with_hiv <- merge(STD_merged, hiv_death, by = c('county', 'locale'), all = TRUE)
# 
# # verify merge:
# nrow(STD_merged) # 3218
# nrow(hiv_death) # 3123
# nrow(STD_with_hiv) # 3256
# 
# # determine mismatches
# head(STD_with_hiv)
# 
# # delete puerto rico
# STD_with_hiv <- STD_with_hiv[STD_with_hiv$locale!='puertorico', ]
# nrow(STD_with_hiv) # 3182
# 
# # find those in STD_merged that were not in hiv_death
# nrow(STD_with_hiv[is.na(STD_with_hiv$in_hiv_death), ]) # 59
# STD_with_hiv[is.na(STD_with_hiv$in_hiv_death), c('county', 'locale', 'fips_state_code', 'fips_county_code') ]
# STD_with_hiv[STD_with_hiv$county=='districtofcolumbia', ] # not a mismatch (just not in hiv set)
# # note that those that do not have fips/cbsa codes are counties which are too small to be in CBSAs
# 
# 3182-3123 # = 59, so all the extras just weren't originally in the hiv_set. that's okay.
# 
# head(hiv_death)
# 
# # check missing
# table(is.na(STD_with_hiv$chl_2012)) # 299
# table(is.na(STD_with_hiv$y2010_2012)) # 3077
# 
# # determine correlations
# cor(STD_with_hiv$gon_2012, STD_with_hiv$y2008_2012, use = 'pairwise.complete.obs') # 0.365
# cor(STD_with_hiv$chl_2012, STD_with_hiv$y2008_2012, use = 'pairwise.complete.obs') # 0.425
# cor(STD_with_hiv$gon_2012, STD_with_hiv$y2010_2012, use = 'pairwise.complete.obs') # 0.386
# cor(STD_with_hiv$chl_2012, STD_with_hiv$y2010_2012, use = 'pairwise.complete.obs') # 0.445
# cor(STD_with_hiv$gon_2012, STD_with_hiv$y2012, use = 'pairwise.complete.obs') # 0.515
# cor(STD_with_hiv$chl_2012, STD_with_hiv$y2012, use = 'pairwise.complete.obs') # 0.580
# cor(STD_with_hiv$gon_2012, STD_with_hiv$y2013, use = 'pairwise.complete.obs') # 0.528
# cor(STD_with_hiv$gon_2012, STD_with_hiv$y2007_2013, use = 'pairwise.complete.obs') # 0.411
# cor(STD_with_hiv$gon_2012, STD_with_hiv$y2009_2013, use = 'pairwise.complete.obs') # 0.413
# cor(STD_with_hiv$gon_2012, STD_with_hiv$y2011_2013, use = 'pairwise.complete.obs') # 0.552
# 
# # Baltimore summary stats (county level)
# STD_merged[STD_merged$county=='baltimore', ]
# STD_merged[STD_merged$county=='baltimorecity', ]
# STD_merged[STD_merged$county=='montgomery'&STD_merged$locale=='maryland', ]

# End time for script
end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)
