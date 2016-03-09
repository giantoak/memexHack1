###############################################################################

# Client: Giant Oak
# Author(s): Nicole Ozminkowski
# Date created: 2015 11 12
# Purpose: Aggregate STD Data to CBSA level correctly

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

# source files
sourceFiles <- c('/demographics/load_STD')



if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, x, ".R", sep = "")))

# End time for source files
end_source_files <- Sys.time()


################################################################################
# load, clean up datasets and merge
################################################################################

# load unaggregated STD data set and county populations data set and crosswalk for easier merging
STD.df <- readRDS(file = paste0(workingDatasets, 'STD_unaggregated.RDS'))

county_pop.df <- read.csv(paste0(rawDatasets, '2015-12-01 -- County Population/Census/CO-EST2014-alldata.csv'), stringsAsFactors = FALSE, colClasses = c('STATE' = 'character', 'COUNTY' = 'character'))

head(STD.df)
head(county_pop.df)


# keep only relevant variables in county_pop2.df
county_pop.df <- county_pop.df[c('STATE', 'COUNTY', 'STNAME', 'CTYNAME', 'POPESTIMATE2012')]


# check level of county_pop.df (expected to be at location level)
nrow(county_pop.df) # 3193
length(unique(paste(county_pop.df$STATE, county_pop.df$COUNTY, sep = '-'))) # 3193. no duplicates
nrow(unique(county_pop.df)) # 3193

# there are only 3143 counties. Are some state totals?
sum(county_pop.df$STNAME == county_pop.df$CTYNAME) # 52
county_pop.df[(county_pop.df$STNAME == county_pop.df$CTYNAME),]
# 50 are state totals, 2 are DC (District of Columbia, District of Columbia)

# do they all have 000 as the county code?
county_pop.df[county_pop.df$COUNTY == '000', ] # yes!
nrow(county_pop.df[county_pop.df$COUNTY == '000', ]) # 51 

# drop state totals
county_pop.df <- county_pop.df[(county_pop.df$COUNTY!= '000'), ]
nrow(county_pop.df) # 3142


# fix formatting on STD dataset: remove STD indicator variable
STD.df$indicator <- NULL

nrow(STD.df) # 3218
nrow(county_pop.df) # 3142

# remove any rows that don't have CBSA information
sum(is.na(STD.df$cbsa_code)) # 1336
STD.df <- STD.df[!(is.na(STD.df$cbsa_code)), ]
nrow(STD.df) # 1882
length(unique(STD.df$cbsa_code)) # 929 (all cbsa's represented)

# merge on fips code
STD_pop <- merge(STD.df, county_pop.df, by.x = c('fips_state_code', 'fips_county_code'), by.y = c('STATE', 'COUNTY'), all = TRUE)

# validate merge
nrow(STD.df) # 1882
nrow(county_pop.df) # 3142
nrow(STD_pop) # 3217
3217 - 3142   # = 75 mismatches (minimum)

# view mismatches
nrow(STD_pop[is.na(STD_pop$STNAME), ]) # 75
STD_pop[is.na(STD_pop$STNAME), ]
# all puearto rico except bedford city, VA
# bedford city lost independence as a city in 2013
# so it's not in this dataset. drop the observation

# investigate missings (missing from STD set)
nrow(STD_pop[is.na(STD_pop$county), ]) # 1335
# drop missings
STD_pop <- STD_pop[!is.na(STD_pop$county), ]
nrow(STD_pop) # 1882

# drop bedford city obs
STD_pop <- STD_pop[STD_pop$county != 'bedfordcity' & !is.na(STD_pop$county), ]
nrow(STD_pop) # 1881

# any missings?
sum(is.na(STD_pop)) # 681
sum(is.na(STD_pop$gon_2012)) # 363
sum(is.na(STD_pop$chl_2012)) # 96
sum(is.na(STD_pop$cbsa_code)) # 0
sum(is.na(STD_pop$STNAME)) # 74
sum(is.na(STD_pop$CTYNAME)) # 74

# investigate state and city name missings
STD_pop[is.na(STD_pop$STNAME), ]
length(unique(STD_pop$cbsa_code)) # 929
# all puerto rico
# drop
STD_pop <- STD_pop[!is.na(STD_pop$STNAME), ]
nrow(STD_pop) # 1807
sum(is.na(STD_pop$CTYNAME)) # 0 
length(unique(STD_pop$cbsa_code)) # 917 (the 12 that were dropped were from PR)
sum(is.na(STD_pop$cbsa_title)) # 4 missing cbsa names


################################################################################
# aggregate correctly
################################################################################


# number of cases = number of cases/100,000 people * number of people/100,000 people
STD_pop$POPESTIMATE2012 <- as.numeric(STD_pop$POPESTIMATE2012)
STD_pop$num_gon <- STD_pop$gon_2012*STD_pop$POPESTIMATE2012/100000
STD_pop$num_chl <- STD_pop$chl_2012*STD_pop$POPESTIMATE2012/100000
head(STD_pop)


# aggregate to CBSA level
# *** double check na.rm business

summary(STD_pop$gon_2012) # no zeros

# split into two datasets
STD_gon <- STD_pop[, c('num_gon', 'POPESTIMATE2012', 'cbsa_code')]
STD_chl <- STD_pop[, c('num_chl', 'POPESTIMATE2012', 'cbsa_code')]

# quick validation of the two datasets
head(STD_gon)
nrow(STD_gon) # 1807
sum(is.na(STD_gon$num_gon)) # 289
head(STD_chl)
nrow(STD_chl) # 1807
sum(is.na(STD_chl$num_chl)) # 22

# remove NAs (gon)
STD_gon <- STD_gon[!is.na(STD_gon$num_gon), ]
nrow(STD_gon) # 1518
sum(is.na(STD_gon)) # 0

# remove NAs (chl)
STD_chl <- STD_chl[!is.na(STD_chl$num_chl), ]
nrow(STD_chl) # 1785
sum(is.na(STD_gon)) # 0

# aggregate gon
STD_gon_agg <- aggregate(STD_gon[, c('num_gon', 'POPESTIMATE2012')], by = list(STD_gon$cbsa_code), FUN = sum)
nrow(STD_gon_agg) # 827/929 CBSAs
length(unique(STD_gon_agg$Group.1)) # 827 - no duplicates
STD_gon_agg$CBSA <- STD_gon_agg$Group.1
STD_gon_agg$Group.1 <- NULL
sum(is.na(STD_gon_agg)) # 0 - no missings

# aggregate chl
STD_chl_agg <- aggregate(STD_chl[, c('num_chl', 'POPESTIMATE2012')], by = list(STD_chl$cbsa_code), FUN = sum)
nrow(STD_chl_agg) # 917/929 CBSAs
length(unique(STD_chl_agg$Group.1)) # 917
STD_chl_agg$CBSA <- STD_chl_agg$Group.1
STD_chl_agg$Group.1 <- NULL
sum(is.na(STD_chl_agg)) # 0

# merge con and chl back to one dataset
STD_agg <- merge(STD_gon_agg, STD_chl_agg, by = 'CBSA', all = TRUE)
nrow(STD_agg) # 917
sum(is.na(STD_agg)) # 180
sum(is.na(STD_agg$num_gon)) # 90: this makes sense because 917-827 = 90
sum(is.na(STD_agg$num_chl)) # 0
STD_agg$pop_gon <- STD_agg$POPESTIMATE2012.x
STD_agg$POPESTIMATE2012 <- STD_agg$POPESTIMATE2012.y
STD_agg$POPESTIMATE2012.x <- NULL
STD_agg$POPESTIMATE2012.y <- NULL


# check level (numbers should be the same)
nrow(STD_agg) # 917 
length(unique(STD_agg$CBSA)) # 917
nrow(STD_agg) == length(unique(STD_agg$CBSA)) # TRUE


# create CBSA-level rate variable
# cases/100,000 people = cases/population*100,000
STD_agg$chl_rate <- (STD_agg$num_chl/STD_agg$POPESTIMATE2012)*100000
STD_agg$gon_rate <- (STD_agg$num_gon/STD_agg$pop_gon)*100000
head(STD_agg)
# STD_agg$num_gon <- NULL
# STD_agg$num_chl <- NULL
# keeping raw chl and gon numbers, dividing gon by gon pop (not chl pop)

# merge location names back in:
#  create names dataset for simplicity
STD_names <- STD_pop[, c('cbsa_code', 'cbsa_title')]
nrow(STD_names) # 1807
length(unique(STD_names$cbsa_code)) # 917

nrow(STD_names) # 1807
STD_names <- unique(STD_names)
nrow(STD_names) # 917

# check missings
sum(is.na(STD_names$cbsa_code)) # 0
sum(is.na(STD_names$cbsa_title)) # 0

# merge:
STD_agg_names <- merge(STD_agg, STD_names, by.x = 'CBSA', by.y = 'cbsa_code', all = TRUE)

# validate merge:
nrow(STD_agg) # 917
nrow(STD_names) # 917
nrow(STD_agg_names) # 917
# no mismatches!

# simplify name and save dataset
STD <- STD_agg_names
saveRDS(STD, file = paste0(workingDatasets, '/STD_cbsa.RDS'))

################################################################################
# merge and compare with Giant Oak's dataset
################################################################################

GO_STD <- read.table(file = paste0(rawDatasets, '/2015-11-23 -- STD Data/From GO/std.tsv'), header = TRUE, sep = '\t')

GO_chl.df <- GO_STD[GO_STD$Disease=='Chlamydia' & GO_STD$Year==2012, c(2, 3, 7)]
GO_gon.df <- GO_STD[GO_STD$Disease=='Gonorrhea' & GO_STD$Year==2012, c(2, 3, 7)]
GO_syp.df <- GO_STD[GO_STD$Disease=='Syphilis' & GO_STD$Year==2012, c(2, 3, 7)]

names(GO_chl.df)[3] <- 'chl_go'
names(GO_gon.df)[3] <- 'gon_go'
names(GO_syp.df)[3] <- 'syp_go'

GO_gon.df$Name <- NULL
GO_syp.df$Name <- NULL

nrow(GO_chl.df) # 50
nrow(GO_gon.df) # 50
nrow(GO_syp.df) # 50

# do the merge - only include CBSA's in their data (top 50)
Compare.df1 <- merge(STD, GO_chl.df, by = 'CBSA' , all = FALSE)
nrow(Compare.df1) # 49
Compare.df2 <- merge(Compare.df1, GO_gon.df, by = 'CBSA', all = FALSE)
nrow(Compare.df2) # 49
Compare.df3 <- merge(Compare.df2, GO_syp.df, by = 'CBSA', all = FALSE)
nrow(Compare.df3) # 49
Compare.df <- Compare.df3

# compare our numbers to GO's
cor(Compare.df$gon_rate, Compare.df$gon_go, use = 'pairwise.complete.obs') # 0.9955
cor(Compare.df$chl_rate, Compare.df$chl_go, use = 'pairwise.complete.obs') # 0.97945.

# percent difference
Compare.df$dif_chl <- (Compare.df$chl_rate - Compare.df$chl_go)/Compare.df$chl_go
Compare.df$dif_gon <- (Compare.df$gon_rate - Compare.df$gon_go)/Compare.df$gon_go
head(Compare.df)

summary(Compare.df$dif_chl)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -2.848e-01 -3.068e-05  1.522e-05 -5.691e-03  2.296e-04  1.059e-03 

summary(Compare.df$dif_gon)
#     Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -2.677e-01 -6.718e-05  4.282e-04 -5.182e-04  5.263e-03  5.035e-02 


# plot differences:
dif_gon_plot <- ggplot(data = Compare.df, aes(Compare.df$dif_gon)) + geom_histogram(fill = 'aquamarine4', colour = 'black') + ggtitle('Differences between gonorrhea rates (TGG vs GO data)') + labs(x = 'Percent difference in gonorrhea rate', y = 'Count')
plot(dif_gon_plot)
#one entry is large negative

dif_chl_plot <- ggplot(data = Compare.df, aes(Compare.df$dif_chl)) + geom_histogram(fill = 'aquamarine4', colour = 'black') + ggtitle('Differences between chlamydia rates (TGG vs GO data)') + labs(x = 'Percent difference in chlamydia rate', y = 'Count')
plot(dif_chl_plot)
# one entry large negative


# examine differences ( show those that are off by more than 15% )
Compare.df[abs(Compare.df$dif_chl) > 0.15, ] # Richmond, VA
Compare.df[abs(Compare.df$dif_gon) > 0.15, ] # Richmond, VA


################################################################################
# HISTOGRAMS
################################################################################

# just chlamydia:
chlamydia_plot <- ggplot(data = STD, aes(STD$chl_rate)) + geom_histogram(fill = 'aquamarine4', colour = 'black') +
  ggtitle('Chlamydia Rates: Instance per 100,000 people') + 
  labs(x = 'Chlamydia Rate',  y = 'Count') 

plot(chlamydia_plot)

# just gonorrhea:
gonorrhea_plot <-  ggplot(data = STD, aes(STD$gon_rate)) + geom_histogram(fill = 'aquamarine4', colour = 'black') +
  ggtitle('Gonorrhea Rates: Instance per 100,000 people') + 
  labs(x = 'Gonorrhea Rate',  y = 'Count') 

plot(gonorrhea_plot)

# GO data syphilis plot:
syp_plot <- ggplot(data = Compare.df, aes(Compare.df$syp_go)) + geom_histogram(fill = 'aquamarine4', colour = 'black') +
  ggtitle('Syphilis Rates (GO Data): Instance per 100,000 people') + 
  labs(x = 'Syphilis Rate',  y = 'Count') 

plot(syp_plot)

# GO data chl: (only top 50 msa's)
go_chl_plot <- ggplot(data = Compare.df, aes(Compare.df$chl_go)) + geom_histogram(fill = 'aquamarine4', colour = 'black') +
  ggtitle('Chlamydia Rates (GO Data): Instance per 100,000 people') + 
  labs(x = 'Chlamydia Rate',  y = 'Count') 

plot(go_chl_plot)

# GO data gon: (only top 50 msa's)
go_gon_plot <- ggplot(data = Compare.df, aes(Compare.df$gon_go)) + geom_histogram(fill = 'aquamarine4', colour = 'black') +
  ggtitle('Gonorrhea Rates (GO Data): Instance per 100,000 people') + 
  labs(x = 'Gonorrhea Rate', y = 'Count') 

plot(go_gon_plot)

# chlamydia/gonorrhea overlay
chl <- data.frame(STD$chl_rate)
chl$rate <- chl$STD.chl_rate
chl$STD.chl_rate <- NULL
gon <- data.frame(STD$gon_rate)
gon$rate <- gon$STD.gon_rate
gon$STD.gon_rate <- NULL
chl$name <- 'Chlamydia'
gon$name <- 'Gonorrhea'
std_hist <- rbind(chl, gon)
chl_gon_overlay <- ggplot(std_hist, aes(std_hist$rate, fill = std_hist$name)) + geom_histogram(colour = 'black') + ggtitle('STD Rates: Instance per 100,000 people') + labs(x = 'STD Rate (disease instance/100,000 people)', y = 'Count') + scale_fill_manual(values=c("dimgrey", "aquamarine4"), name = 'Disease') 


plot(chl_gon_overlay)

########################################################
# Baltimore/Montgomery info for hackathon
########################################################
# head(STD)
# STD[STD$CBSA==12580, ]
# STD[STD$CBSA==47900, ]

# End time for script
end_time <- Sys.time()
run_time <- end_time - start_time
print(run_time)