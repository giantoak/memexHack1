###############################################################################

# Client: Giant Oak
# Author(s): Hadi
# Date created: 2016 02 03
# Purpose: Create summary stats for crime data

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
# Default Graphics Options
################################################################################

# TGG-style graphs in R
# TGG color palette
library(ggplot2)
tgg.palette <- c("#AFCCBF", "#88888C", "#97BDD2", "#CDA39E", "#C8C8CA", "#FFEAB7")

theme_axis <- theme(panel.border = element_blank(), axis.line = element_line(colour = "black"), legend.title=element_blank())
theme_colors <- scale_colour_manual(values=tgg.palette)
theme_backg <- theme_bw(base_size=12)

################################################################################
# load in libraries and source files
################################################################################

# libraries:

librariesToLoad <- c("ggplot2", "curl", "data.table", "stringr","plyr","magrittr","dplyr","reshape2")

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
 sourceFiles <- c("summary_stat_functions.R")

  source(paste(code,"xdata","sum_stat_functions.R",sep="/"))

# End time for source files
end_source_files <- Sys.time()

detach("package:plyr")
################################################################################
# Read in data
################################################################################

ad_level_msa_level <- readRDS(paste0(finalDatasets,"ad_price_msa_level.rds"))
ad_level_msa_level <- as.data.table(ad_level_msa_level)
msa_level_master <- readRDS(paste0(finalDatasets,"Master MSA-Level/msa_level_master.rds"))


colnames(ad_level_msa_level)
#msa_year_level_master <- readRDS(paste0(finalDatasets,"/Master MSA- Year-Level/msa_year_level_master.rds"))

colnames(ad_level_msa_level)

###########################################
# Start Creating Wide Dataset
###########################################

ad_level_with_msa_year_char <- readRDS(paste0(finalDatasets,"ad_price_msa_year_level.RDS"))

#Subset to ads with a price of <1000 
ad_level_with_msa_year_char <- subset(ad_level_with_msa_year_char, price_per_hour <1000)
ad_level_with_msa_year_char <- as.data.frame(ad_level_with_msa_year_char)

#Price by year, median

price_change_long_median<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(median_price = median(price_per_hour,na.rm = TRUE))

#Check to make sure we see the right number of unique obs
nrow(price_change_long_median) == length(unique(paste(ad_level_with_msa_year_char$CBSACode,ad_level_with_msa_year_char$year)))

price_change_median <- dcast(data = price_change_long_median, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "median_price" ,fill=NA_real_)

names(price_change_median) = c("CBSACode","median_price_2005", "median_price_2008","median_price_2009", "median_price_2010" ,"median_price_2011", "median_price_2012" , "median_price_2013","median_price_2014", "median_price_2015")

#Price by year, mean


price_change_mean_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(mean_price = mean(price_per_hour,na.rm = TRUE))

price_change_mean <- dcast(data = price_change_mean_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "mean_price",fill=NA_real_)

names(price_change_mean) = c("CBSACode","mean_price_2005", "mean_price_2008","mean_price_2009", "mean_price_2010" ,"mean_price_2011", "mean_price_2012" , "mean_price_2013","mean_price_2014", "mean_price_2015")



#Price by Venue, Median

price_by_venue_long_median <- ad_level_with_msa_year_char %>% group_by(CBSACode,year,incall_outcall) %>% summarise(median_price = median(price_per_hour,na.rm = TRUE))


price_by_venue_median <- dcast(data = price_by_venue_long_median, formula = CBSACode ~ year + incall_outcall ,fun.aggregate = mean, value.var = "median_price", fill=NA_real_)

price_by_venue_median <- as.data.frame(price_by_venue_median)[,c(1,16:34)]

names(price_by_venue_median) <- c("CBSACode",
                           "both_incall_outcall_2011_median","outcall_only_2011_median","unclear_2011_median",
                           "incall_only_2012_median", "both_incall_outcall_2012_median","outcall_only_2012_median","unclear_2012_median",
                           "incall_only_2013_median", "both_incall_outcall_2013_median","outcall_only_2013_median","unclear_2013_median",
                           "incall_only_2014_median", "both_incall_outcall_2014_median","outcall_only_2014_median","unclear_2014_median",
                           "incall_only_2015_median", "both_incall_outcall_2015_median","outcall_only_2015_median","unclear_2015_median")

#Price by Venue, Mean



price_by_venue_mean_long <- ad_level_with_msa_year_char %>% group_by(CBSACode,year,incall_outcall) %>% summarise(average_price = mean(price_per_hour,na.rm = TRUE))


price_by_venue_mean <- dcast(data = price_by_venue_mean_long, formula = CBSACode ~ year + incall_outcall ,fun.aggregate = mean, value.var = "average_price", fill=NA_real_)

price_by_venue_mean <- as.data.frame(price_by_venue_mean)[,c(1,16:34)]

names(price_by_venue_mean) <- c("CBSACode",
                           "both_incall_outcall_2011_mean","outcall_only_2011_mean","unclear_2011_mean",
                           "incall_only_2012_mean", "both_incall_outcall_2012_mean","outcall_only_2012_mean","unclear_2012_mean",
                           "incall_only_2013_mean", "both_incall_outcall_2013_mean","outcall_only_2013_mean","unclear_2013_mean",
                           "incall_only_2014_mean", "both_incall_outcall_2014_mean","outcall_only_2014_mean","unclear_2014_mean",
                           "incall_only_2015_mean", "both_incall_outcall_2015_mean","outcall_only_2015_mean","unclear_2015_mean")


#Ads

num_ads_long <- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% tally()

num_ads <- dcast(data = num_ads_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "n")

names(num_ads) = c("CBSACode","num_ads_2005", "num_ads_2008","num_ads_2009", "num_ads_2010" ,"num_ads_2011", "num_ads_2012" , "num_ads_2013","num_ads_2014", "num_ads_2015")


#Commute

commute_time_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(commute_time = median(avg_com,na.rm = TRUE))

commute_time <- dcast(data = commute_time_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "commute_time", fill=NA_real_)

names(commute_time) = c("CBSACode","commute_time_2005", "commute_time_2008","commute_time_2009", "commute_time_2010" ,"commute_time_2011", "commute_time_2012" , "commute_time_2013","commute_time_2014", "commute_time_2015")



#Bring in Population Dataset

population_dataset <- msa_level_master[,c("CBSACode","pop_estimate_2013")]



#Violence Rate
ad_level_with_msa_year_char$violent_per_100k<- ad_level_with_msa_year_char$violent_percap * 100000
violence_rate_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(violence_rate = median(violent_per_100k,na.rm = TRUE))

violence_rate <- dcast(data = violence_rate_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "violence_rate", fill=NA_real_)

names(violence_rate) = c("CBSACode","violence_rate_2005", "violence_rate_2008","violence_rate_2009", "violence_rate_2010" ,"violence_rate_2011", "violence_rate_2012" , "violence_rate_2013","violence_rate_2014", "violence_rate_2015")


#Rape
ad_level_with_msa_year_char$rape_per_100k<- ad_level_with_msa_year_char$rape_percap * 100000

rape_rate_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(rape_rate = median(rape_per_100k, na.rm = TRUE))

rape_rate <- dcast(data = rape_rate_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "rape_rate",fill=NA_real_)

names(rape_rate) = c("CBSACode","rape_rate_2005", "rape_rate_2008","rape_rate_2009", "rape_rate_2010" ,"rape_rate_2011", "rape_rate_2012" , "rape_rate_2013","rape_rate_2014", "rape_rate_2015")

#Property
ad_level_with_msa_year_char$property_per_100k<- ad_level_with_msa_year_char$property_percap * 100000


property_rate_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(property_rate = median(property_per_100k, na.rm = TRUE))

property_rate <- dcast(data = property_rate_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "property_rate" ,fill=NA_real_)

names(property_rate) = c("CBSACode","property_rate_2005", "property_rate_2008","property_rate_2009", "property_rate_2010" ,"property_rate_2011", "property_rate_2012" , "property_rate_2013","property_rate_2014", "property_rate_2015")



#FTSWORN

ftsworn_rate_long<- ad_level_with_msa_year_char%>% group_by(CBSACode,year) %>% summarise(ftsworn_per100k_2013 = median(ftsworn_rateper100k_2013,na.rm = TRUE))

ftsworn_rate <- as.data.frame(dcast(data = ftsworn_rate_long, formula = CBSACode~year,fun.aggregate = mean, value.var = "ftsworn_per100k_2013",fill=NA_real_))

names(ftsworn_rate) = c("CBSACode","ftsworn_rate_per_100k_2005", "ftsworn_rate_per_100k_2008","ftsworn_rate_per_100k_2009", "ftsworn_rate_per_100k_2010" ,"ftsworn_rate_per_100k_2011", "ftsworn_rate_per_100k_2012" , "ftsworn_rate_per_100k_2013","ftsworn_rate_per_100k_2014", "ftsworn_rate_per_100k_2015")

ftsworn_rate = ftsworn_rate[,c(1,8)]


#Composite
COL_Composite_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(COL_Composite = median(COL_Composite, na.rm = TRUE))

COL_Composite <- dcast(data = COL_Composite_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "COL_Composite", fill=NA_real_)

names(COL_Composite) = c("CBSACode","COL_Composite_2005", "COL_Composite_2008","COL_Composite_2009", "COL_Composite_2010" ,"COL_Composite_2011", "COL_Composite_2012" , "COL_Composite_2013","COL_Composite_2014", "COL_Composite_2015")

#Transport

COL_Transport_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(COL_Transport = median(COL_Transport, na.rm = TRUE))

COL_Transport <- dcast(data = COL_Transport_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "COL_Transport",fill=NA_real_)

names(COL_Transport) = c("CBSACode","COL_Transport_2005", "COL_Transport_2008","COL_Transport_2009", "COL_Transport_2010" ,"COL_Transport_2011", "COL_Transport_2012" , "COL_Transport_2013","COL_Transport_2014", "COL_Transport_2015")



#STD

gon_rate_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(gon_rate = median(gon_rate, na.rm = TRUE))

gon_rate <- dcast(data = gon_rate_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "gon_rate",fill=NA_real_)

names(gon_rate) = c("CBSACode","gon_rate_2005", "gon_rate_2008","gon_rate_2009", "gon_rate_2010" ,"gon_rate_2011", "gon_rate_2012" , "gon_rate_2013","gon_rate_2014", "gon_rate_2015")


chl_rate_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(chl_rate = median(chl_rate, na.rm = TRUE))

chl_rate <- dcast(data = chl_rate_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "chl_rate",fill=NA_real_)

names(chl_rate) = c("CBSACode","chl_rate_2005", "chl_rate_2008","chl_rate_2009", "chl_rate_2010" ,"chl_rate_2011", "chl_rate_2012" , "chl_rate_2013","chl_rate_2014", "chl_rate_2015")


#Income

median_income_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(median = mean(median_income,na.rm=TRUE))

median_income <- dcast(data = median_income_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "median",fill=NA_real_)

names(median_income) = c("CBSACode","median_income_2005", "median_income_2008","median_income_2009", "median_income_2010" ,"median_income_2011", "median_income_2012" , "median_income_2013","median_income_2014", "median_income_2015")




poverty_rate_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(median = mean(poverty_rate,na.rm=TRUE))

poverty_rate <- dcast(data = poverty_rate_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "median",fill=NA_real_)

names(poverty_rate) = c("CBSACode","poverty_rate_2005", "poverty_rate_2008","poverty_rate_2009", "poverty_rate_2010" ,"poverty_rate_2011", "poverty_rate_2012" , "poverty_rate_2013","poverty_rate_2014", "poverty_rate_2015")




prostitution_arrests_long <- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(mean = mean(prostitution_NIBRS,na.rm=TRUE))


prostitution_arrests <- dcast(data = prostitution_arrests_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "mean",fill=NA_real_)

names(prostitution_arrests_long) <- c("CBSACode","year","prostitution_arrests")

names(prostitution_arrests) = c("CBSACode","prostitution_arrests_2005", "prostitution_arrests_2008","prostitution_arrests_2009", "prostitution_arrests_2010" ,"prostitution_arrests_2011", "prostitution_arrests_2012" , "prostitution_arrests_2013","prostitution_arrests_2014", "prostitution_arrests_2015")

###Population by year

population_long<- ad_level_with_msa_year_char %>% group_by(CBSACode,year) %>% summarise(median = median(population_UCR,na.rm=TRUE))


population <- dcast(data = population_long, formula = CBSACode ~ year,fun.aggregate = mean, value.var = "median",fill=NA_real_)

names(population_long) <- c("CBSACode","year","population")

names(population) = c("CBSACode","population_2005", "population_2008","population_2009", "population_2010" ,"population_2011", "population_2012" , "population_2013","population_2014", "population_2015")



msa_level_sumstats <- merge(price_change_median,population_dataset)

msa_level_sumstats <- merge(msa_level_sumstats,price_change_mean)


msa_level_sumstats <- merge(msa_level_sumstats, num_ads)

msa_level_sumstats <- merge(msa_level_sumstats,commute_time)

msa_level_sumstats <- merge(msa_level_sumstats,violence_rate)

msa_level_sumstats <- merge(msa_level_sumstats,rape_rate)

msa_level_sumstats <- merge(msa_level_sumstats,property_rate)

msa_level_sumstats <- merge(msa_level_sumstats,ftsworn_rate)

msa_level_sumstats <- merge(msa_level_sumstats,price_by_venue_median)

msa_level_sumstats <- merge(msa_level_sumstats,COL_Composite)

msa_level_sumstats <- merge(msa_level_sumstats,COL_Transport)

msa_level_sumstats <- merge(msa_level_sumstats,gon_rate)

msa_level_sumstats <- merge(msa_level_sumstats,chl_rate)

msa_level_sumstats <- merge(msa_level_sumstats,median_income)

msa_level_sumstats <- merge(msa_level_sumstats,poverty_rate)

msa_level_sumstats <- merge(msa_level_sumstats,population_dataset)

msa_level_sumstats <- merge(msa_level_sumstats,price_by_venue_mean)

msa_level_sumstats <- merge(msa_level_sumstats,prostitution_arrests)



#Median Outcall Premium

msa_level_sumstats$outcall_premium_2015_median <- msa_level_sumstats$outcall_only_2015_median - msa_level_sumstats$incall_only_2015_median

msa_level_sumstats$outcall_premium_2014_median <- msa_level_sumstats$outcall_only_2014_median - msa_level_sumstats$incall_only_2014_median

msa_level_sumstats$outcall_premium_2013_median <- msa_level_sumstats$outcall_only_2013_median - msa_level_sumstats$incall_only_2013_median



msa_level_sumstats$outcall_premium_2015_median_percent <- msa_level_sumstats$outcall_only_2015_median / msa_level_sumstats$incall_only_2015_median

msa_level_sumstats$outcall_premium_2014_median_percent <- msa_level_sumstats$outcall_only_2014_median / msa_level_sumstats$incall_only_2014_median

msa_level_sumstats$outcall_premium_2013_median_percent <- msa_level_sumstats$outcall_only_2013_median / msa_level_sumstats$incall_only_2013_median


#Outcall premiums by mean

msa_level_sumstats$outcall_premium_2015_mean <- msa_level_sumstats$outcall_only_2015_mean - msa_level_sumstats$incall_only_2015_mean

msa_level_sumstats$outcall_premium_2014_mean <- msa_level_sumstats$outcall_only_2014_mean - msa_level_sumstats$incall_only_2014_mean

msa_level_sumstats$outcall_premium_2013_mean <- msa_level_sumstats$outcall_only_2013_mean - msa_level_sumstats$incall_only_2013_mean



msa_level_sumstats$outcall_premium_2015_mean_percent <- msa_level_sumstats$outcall_only_2015_mean / msa_level_sumstats$outcall_only_2015_mean

msa_level_sumstats$outcall_premium_2014_mean_percent <- msa_level_sumstats$outcall_only_2014_mean / msa_level_sumstats$outcall_only_2014_mean

msa_level_sumstats$outcall_premium_2013_mean_percent <- msa_level_sumstats$outcall_only_2013_mean / msa_level_sumstats$outcall_only_2013_mean




msa_level_sumstats$violent_crime_change_2014 <- (msa_level_sumstats$violence_rate_2014/msa_level_sumstats$violence_rate_2013 - 1)
msa_level_sumstats$rape_crime_change_2014 <- (msa_level_sumstats$rape_rate_2014/msa_level_sumstats$rape_rate_2013 - 1)
msa_level_sumstats$property_crime_change_2014 <- (msa_level_sumstats$property_rate_2014/msa_level_sumstats$property_rate_2013 - 1)

msa_level_sumstats$chl_change_2014 <- (msa_level_sumstats$chl_rate_2014/msa_level_sumstats$chl_rate_2013 - 1)
msa_level_sumstats$gon_change_2014 <- (msa_level_sumstats$gon_rate_2014/msa_level_sumstats$gon_rate_2013 - 1)

msa_level_sumstats$COL_Composite_change_2014 <- (msa_level_sumstats$COL_Composite_2014/msa_level_sumstats$COL_Composite_2013 - 1)
msa_level_sumstats$COL_Transport_change_2014 <- (msa_level_sumstats$COL_Transport_2014/msa_level_sumstats$COL_Transport_2013 - 1)

msa_level_sumstats$commute_time_change_2014 <- (msa_level_sumstats$commute_time_2014/msa_level_sumstats$commute_time_2013 - 1)
msa_level_sumstats$median_income_change_2014 <- (msa_level_sumstats$median_income_2014/msa_level_sumstats$median_income_2013 - 1)
msa_level_sumstats$poverty_rate_change_2014 <- (msa_level_sumstats$poverty_rate_2014/msa_level_sumstats$poverty_rate_2013 - 1)
msa_level_sumstats$poverty_rate_change_2014 <- (msa_level_sumstats$poverty_rate_2014/msa_level_sumstats$poverty_rate_2013 - 1)

msa_level_sumstats$median_price_change_2014 <- (msa_level_sumstats$median_price_2014/msa_level_sumstats$median_price_2013 - 1)
msa_level_sumstats$outcall_premium_change_2014_median <- (msa_level_sumstats$outcall_premium_2014_median/msa_level_sumstats$outcall_premium_2013_median - 1)
msa_level_sumstats$outcall_premium_change_2014_mean <- (msa_level_sumstats$outcall_premium_2014_mean/msa_level_sumstats$outcall_premium_2013_mean - 1)

msa_level_sumstats$mean_price_change_2014 <- (msa_level_sumstats$mean_price_2014/msa_level_sumstats$mean_price_2013 - 1)


msa_year_level_sumstats <- as.data.frame(msa_level_sumstats)
msa_level_sumstats <- rm(msa_level_sumstats)


saveRDS(msa_year_level_sumstats,file = paste0(finalDatasets,"MSA_year_level_sumstats_less_1000.RDS"))


############################
#Merge and save Long Dataset
############################

msa_level_long_data <- merge(COL_Composite_long,COL_Transport_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,commute_time_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,chl_rate_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,gon_rate_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,property_rate_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,violence_rate_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,rape_rate_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,ftsworn_rate_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,prostitution_arrests_long,by = c("CBSACode","year"))
msa_level_long_data <- merge(msa_level_long_data,population_long,by = c("CBSACode","year"))

msa_level_long_data$prostitution_rate_per_100k<- msa_level_long_data$prostitution_arrests  /  (msa_level_long_data$population / 100000)


#Median Outcall Premium Long
outcall_premiums <- msa_year_level_sumstats[,c("CBSACode","outcall_premium_2015_median","outcall_premium_2014_median","outcall_premium_2013_median")]
outcall_premiums <- data.table(outcall_premiums)
outcall_vars <- colnames(outcall_premiums)[grepl('20', colnames(outcall_premiums))]
outcall_premiums_long <- melt(outcall_premiums, id.vars = 'CBSACode', measure.vars = outcall_vars)
outcall_premiums_long <- data.frame(outcall_premiums_long)
outcall_premiums_long$variable <- as.character(outcall_premiums_long$variable)
outcall_premiums_long <- outcall_premiums_long[order(outcall_premiums_long[['CBSACode']]), ]
#extract year and varnames
outcall_premiums_long$year <-  gsub("[^[:digit:]]", "\\1", outcall_premiums_long$variable)
table(outcall_premiums_long$year)

outcall_premiums_long$variable <- NULL

names(outcall_premiums_long) <- c("CBSACode","outcall_premium_median", "year")

msa_level_long_data <- merge(msa_level_long_data,outcall_premiums_long,by = c("CBSACode","year"))

#Mean Outcall Premium Long
#reshape into MSA-year
outcall_premiums_mean <- msa_year_level_sumstats[,c("CBSACode","outcall_premium_2015_mean","outcall_premium_2014_mean","outcall_premium_2013_mean")]
outcall_premiums_mean <- data.table(outcall_premiums_mean)
outcall_vars <- colnames(outcall_premiums_mean)[grepl('20', colnames(outcall_premiums_mean))]
outcall_premiums_mean_long <- melt(outcall_premiums_mean, id.vars = 'CBSACode', measure.vars = outcall_vars)
outcall_premiums_mean_long <- data.frame(outcall_premiums_mean_long)
outcall_premiums_mean_long$variable <- as.character(outcall_premiums_mean_long$variable)
outcall_premiums_mean_long <- outcall_premiums_mean_long[order(outcall_premiums_mean_long[['CBSACode']]), ]
#extract year and varnames
outcall_premiums_mean_long$year <-  gsub("[^[:digit:]]", "\\1", outcall_premiums_mean_long$variable)
table(outcall_premiums_mean_long$year)

outcall_premiums_mean_long$variable <- NULL

names(outcall_premiums_mean_long) <- c("CBSACode","outcall_premium_mean", "year")

msa_level_long_data <- merge(msa_level_long_data,outcall_premiums_mean_long,by = c("CBSACode","year"))


saveRDS(msa_level_long_data,file = paste0(finalDatasets,"msa_level_long_data.RDS"))



###########################################
# DONE CREATING WIDE DATASET
###########################################


#create mean prices by msa

#create categorical variable for venue
ad_level_msa_level$venue <- "no_dummies"
ad_level_msa_level[incall_only==1,venue:="incall_only",]
ad_level_msa_level[outcall_only==1,venue:="outcall_only",]
ad_level_msa_level[both_incall_outcall==1,venue:="both",]
ad_level_msa_level[unclear_inc_out==1,venue:="unclear",]

msa_venue_prices <- ad_level_msa_level[, .(med_price_cbsa_venue = median(price_per_hour)),by=.(CBSACode, venue)]
msa_venue_prices <- ad_level_msa_level[price_per_hour<1000, .(med_price_cbsa_venue = median(price_per_hour)),by=.(CBSACode, venue)]
msa_year_venue_prices <- ad_level_msa_level[price_per_hour<1000, .(med_price_cbsa_venue = median(price_per_hour)),by=.(CBSACode, venue, year)]

#ad_level_msa_level$venue <- names(ad_level_msa_level[c("unclear_inc_out","outcall_only","both_inc_out","incall_only")])[max.col(ad_level_msa_level[c("unclear_inc_out","outcall_only","both_inc_out","incall_only")])]

median_prices <- ad_level_msa_level[, .(med_price = median(price_per_hour), mean_price=mean(price_per_hour)),by=CBSACode]
median_prices_outcall <- ad_level_msa_level[outcall_only==1,.(med_out = median(price_per_hour), mean_out=mean(price_per_hour)),by=.(CBSACode)]
median_prices_both <- ad_level_msa_level[both_incall_outcall==1,.(med_both = median(price_per_hour), mean_both=mean(price_per_hour)),by=.(CBSACode)]
median_prices_unclear <- ad_level_msa_level[unclear_inc_out==1,.(med_unc = median(price_per_hour), mean_unc=mean(price_per_hour)),by=.(CBSACode)]
median_prices_incall <- ad_level_msa_level[unclear_inc_out==0 & both_incall_outcall==0 & outcall_only==0, .(med_inc = median(price_per_hour), mean_inc=mean(price_per_hour)),by=CBSACode]

median_year_prices <- ad_level_msa_level[, .(med_price = median(price_per_hour), mean_price=mean(price_per_hour)),by=.(CBSACode,year)]
median_year_prices_outcall <- ad_level_msa_level[outcall_only==1,.(med_out = median(price_per_hour), mean_out=mean(price_per_hour)),by=.(CBSACode,year)]
median_year_prices_both <- ad_level_msa_level[both_incall_outcall==1,.(med_both = median(price_per_hour), mean_both=mean(price_per_hour)),by=.(CBSACode,year)]
median_year_prices_unclear <- ad_level_msa_level[unclear_inc_out==1,.(med_unc = median(price_per_hour), mean_unc=mean(price_per_hour)),by=.(CBSACode,year)]
median_year_prices_incall <- ad_level_msa_level[unclear_inc_out==0 & both_incall_outcall==0 & outcall_only==0, .(med_inc = median(price_per_hour), mean_inc=mean(price_per_hour)),by=.(CBSACode,year)]

msa_level <- merge(msa_level_master, median_prices, by= 'CBSACode', all.x=TRUE)
msa_level <- merge(msa_level, median_prices_outcall, by='CBSACode', all.x=TRUE)
msa_level <- merge(msa_level, median_prices_both, by='CBSACode', all.x=TRUE)
msa_level <- merge(msa_level, median_prices_unclear, by='CBSACode', all.x=TRUE)
msa_level <- merge(msa_level, median_prices_incall, by='CBSACode', all.x=TRUE)

# msa_year_level <- merge(msa_year_level_master,median_year_prices, by='CBSACode', all.x=TRUE)
# msa_year_level <- merge(msa_year_level,median_year_prices_outcall, by='CBSACode', all.x=TRUE)
# msa_year_level <- merge(msa_year_level,median_year_prices_both, by='CBSACode', all.x=TRUE)
# msa_year_level <- merge(msa_year_level,median_year_prices_unclear, by='CBSACode', all.x=TRUE)
# msa_year_level <- merge(msa_year_level,median_year_prices_incall, by='CBSACode', all.x=TRUE)

#msa_level <- merge(msa_level, msa_venue_prices,by='CBSACode', all.x=TRUE)
msa_level <- as.data.table(msa_level)
msa_level[,out_prem:= mean_out - mean_inc, ]
msa_level[,out_prem_med:= med_out - med_inc, ]

msa_level_not_missing <- msa_level[!is.na(COL_Composite_mr) & !is.na(med_price) & !is.na(violent_msa) & !is.na(rape_msa) & !is.na(population_GIANTOAK),,]
msa_level_prem <- msa_level_not_missing[,.(CBSACode=CBSACode, out_prem=out_prem), by=CBSACode ]

# create summary stats - key variables

temp <- msa_level
temp$violent_rate_msa -> temp$violent_per100k
temp$property_rate_msa -> temp$property_per100k
temp$rape_rate_msa -> temp$rape_per100k
temp$prostitution_rate_msa -> temp$prostitution_per100k

# median prices
summary_stats_msa <- sumStat(temp, "med_price") 
  colnames(summary_stats_msa)[colnames(summary_stats_msa) == "Value"] <- "med_price"

# outcall premium
summary_stats_msa_temp <- sumStat(temp, "out_prem") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "out_prem"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# violent rate   
summary_stats_msa_temp <- sumStat(temp, "violent_per100k") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "violent_per100k"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")  

# rape rate
summary_stats_msa_temp <- sumStat(temp, "rape_per100k") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "rape_per100k"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# property rate
summary_stats_msa_temp <- sumStat(temp, "property_per100k") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "property_per100k"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# chlamydia per 100K 2012
summary_stats_msa_temp <- sumStat(temp, "chl_rateper100k_2012") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "chl_rateper100k_2012"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# gonorrhea per 100k 2012
summary_stats_msa_temp <- sumStat(temp, "gon_rateper100k_2012") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "gon_rateper100k_2012"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# law enforcement per 100k 2013  
summary_stats_msa_temp <- sumStat(temp, "ftsworn_rateper100k_2013") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "ftsworn_rateper100k_2013"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# prostitution rate
summary_stats_msa_temp <- sumStat(temp, "prostitution_per100k") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "prostitution_per100k"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
   
# COLI composite  
summary_stats_msa_temp <- sumStat(temp, "COL_Composite_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "COL_Composite_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# COLI transport 
summary_stats_msa_temp <- sumStat(temp, "COL_Transport_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "COL_Transport_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# average commute Giant Oak
summary_stats_msa_temp <- sumStat(temp, "avg_commute_GIANTOAK") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "avg_commute_GIANTOAK"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# poverty
summary_stats_msa_temp <- sumStat(temp, "poverty_rate") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "poverty_rate"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# median income 
summary_stats_msa_temp <- sumStat(temp, "median_income") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "median_income"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

temp <- NULL 

# create summary stats - key variables changes between 2013 to 2014   

temp <- msa_year_level_sumstats

# median_price_change_2014
summary_stats_msa_temp <- sumStat(temp, "median_price_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "median_price_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# mean_price_change_2014
summary_stats_msa_temp <- sumStat(temp, "mean_price_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "mean_price_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# outcall_premium_change_2014
summary_stats_msa_temp <- sumStat(temp, "outcall_premium_change_2014_median") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "outcall_premium_change_2014_median"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# violent_crime_change_2014  
summary_stats_msa_temp <- sumStat(temp, "violent_crime_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "violent_crime_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# rape_crime_change_2014
summary_stats_msa_temp <- sumStat(temp, "rape_crime_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "rape_crime_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# property_crime_change_2014
summary_stats_msa_temp <- sumStat(temp, "property_crime_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "property_crime_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# chl_change_2014
summary_stats_msa_temp <- sumStat(temp, "chl_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "chl_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# gon_change_2014
summary_stats_msa_temp <- sumStat(temp, "gon_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "gon_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# COL_Composite_change_2014
summary_stats_msa_temp <- sumStat(temp, "COL_Composite_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "COL_Composite_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# COL_Transport_change_2014
summary_stats_msa_temp <- sumStat(temp, "COL_Transport_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "COL_Transport_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# commute_time_change_2014
summary_stats_msa_temp <- sumStat(temp, "commute_time_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "commute_time_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# poverty_rate_change_2014
summary_stats_msa_temp <- sumStat(temp, "poverty_rate_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "poverty_rate_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
# median_income_change_2014
summary_stats_msa_temp <- sumStat(temp, "median_income_change_2014") 
  colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "median_income_change_2014"
  summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
  
summary_stats_msa_temp <- NULL 
temp <- NULL 

# Create table for report
summary_stats_msa1 <- summary_stats_msa[, c(1:6)]
colnames(summary_stats_msa1) <- c("Stats", "Med Price","Outcall Prem", "Violent p100K", "Rape p100K", "Prop p100K")
summary_stats_msa1 <- summary_stats_msa1[c(13,2,4,9,10,12,18,17),] 

summary_stats_msa2 <- summary_stats_msa[,c(1,7:8,9:10)]
colnames(summary_stats_msa2) <- c("Stats", "Chl p100K", "Gon p100K", "FTSWO p100K", "Pro Arr p100K")
summary_stats_msa2 <- summary_stats_msa2[c(13,2,4,9,10,12,18,17),]

summary_stats_msa3 <- summary_stats_msa[,c(1,11,13:15)]
colnames(summary_stats_msa3) <- c("Stats", "COL Comp 2014", "Avg Comm", "Poverty Rate", "Median Income")
summary_stats_msa3 <- summary_stats_msa3[c(13,2,4,9,10,12,18,17),]

# msa_year_level <- as.data.table(msa_year_level)
# msa_year_level[,out_prem:= med_out - med_inc, ]

msa_year_level_sumstats$cbsa_id <- msa_year_level_sumstats$CBSACode

msa_year_level_sumstats$chg_price_2014 <- (msa_year_level_sumstats$median_price_2014 / msa_year_level_sumstats$median_price_2013)-1
msa_year_level_sumstats$chg_price_2013 <- (msa_year_level_sumstats$median_price_2013 / msa_year_level_sumstats$median_price_2012)-1
msa_year_level_sumstats$chg_price_2012 <- (msa_year_level_sumstats$median_price_2012 / msa_year_level_sumstats$median_price_2011)-1

msa_year_level_sumstats$chg_mean_price_2014 <- (msa_year_level_sumstats$mean_price_2014 / msa_year_level_sumstats$mean_price_2013)-1
msa_year_level_sumstats$chg_mean_price_2013 <- (msa_year_level_sumstats$mean_price_2013 / msa_year_level_sumstats$mean_price_2012)-1
msa_year_level_sumstats$chg_mean_price_2012 <- (msa_year_level_sumstats$mean_price_2012 / msa_year_level_sumstats$mean_price_2011)-1

msa_year_level_sumstats$chg_inc_2014 <- (msa_year_level_sumstats$incall_only_2014_median / msa_year_level_sumstats$incall_only_2013_median)-1
msa_year_level_sumstats$chg_inc_2013 <- (msa_year_level_sumstats$incall_only_2013_median / msa_year_level_sumstats$incall_only_2012_median)-1
#msa_year_level_sumstats$chg_inc_2012 <- (msa_year_level_sumstats$incall_only_2012 / msa_year_level_sumstats$incall_only_2011)-1


msa_year_level_sumstats$chg_out_2014 <- (msa_year_level_sumstats$outcall_only_2014_median / msa_year_level_sumstats$outcall_only_2013_median)-1
msa_year_level_sumstats$chg_out_2013 <- (msa_year_level_sumstats$outcall_only_2013_median / msa_year_level_sumstats$outcall_only_2012_median)-1
msa_year_level_sumstats$chg_out_2012 <- (msa_year_level_sumstats$outcall_only_2012_median / msa_year_level_sumstats$outcall_only_2011_median)-1

msa_year_level_sumstats$chg_both_2014 <- (msa_year_level_sumstats$outcall_only_2014_median / msa_year_level_sumstats$outcall_only_2013_median)-1
msa_year_level_sumstats$chg_both_2013 <- (msa_year_level_sumstats$outcall_only_2013_median / msa_year_level_sumstats$outcall_only_2012_median)-1
msa_year_level_sumstats$chg_both_2012 <- (msa_year_level_sumstats$outcall_only_2012_median / msa_year_level_sumstats$outcall_only_2011_median)-1

msa_year_level_sumstats$chg_violence_2014 <- (msa_year_level_sumstats$violence_rate_2014 / msa_year_level_sumstats$violence_rate_2013) -1
msa_year_level_sumstats$chg_violence_2013 <- (msa_year_level_sumstats$violence_rate_2013 / msa_year_level_sumstats$violence_rate_2012) -1
msa_year_level_sumstats$chg_violence_2012 <- (msa_year_level_sumstats$violence_rate_2012 / msa_year_level_sumstats$violence_rate_2011) -1

msa_year_level_sumstats$chg_rape_2014 <- (msa_year_level_sumstats$rape_rate_2014 / msa_year_level_sumstats$rape_rate_2013) -1
msa_year_level_sumstats$chg_rape_2013 <- (msa_year_level_sumstats$rape_rate_2013 / msa_year_level_sumstats$rape_rate_2012) -1
msa_year_level_sumstats$chg_rape_2012 <- (msa_year_level_sumstats$rape_rate_2012 / msa_year_level_sumstats$rape_rate_2011) -1

msa_year_level_sumstats$chg_property_2014 <- (msa_year_level_sumstats$property_rate_2014 / msa_year_level_sumstats$property_rate_2013) -1
msa_year_level_sumstats$chg_property_2013 <- (msa_year_level_sumstats$property_rate_2013 / msa_year_level_sumstats$property_rate_2012) -1
msa_year_level_sumstats$chg_property_2012 <- (msa_year_level_sumstats$property_rate_2012 / msa_year_level_sumstats$property_rate_2011) -1

msa_year_level_sumstats$chg_commute_2014 <- (msa_year_level_sumstats$commute_time_2014 / msa_year_level_sumstats$commute_time_2013) -1
msa_year_level_sumstats$chg_commute_2013 <- (msa_year_level_sumstats$commute_time_2013 / msa_year_level_sumstats$commute_time_2012) -1
msa_year_level_sumstats$chg_commute_2012 <- (msa_year_level_sumstats$commute_time_2012 / msa_year_level_sumstats$commute_time_2011) -1

msa_year_level_sumstats$out_prem_2014_median <- msa_year_level_sumstats$outcall_only_2014_median - msa_year_level_sumstats$incall_only_2014_median 
msa_year_level_sumstats$out_prem_2013_median  <- msa_year_level_sumstats$outcall_only_2013_median  - msa_year_level_sumstats$incall_only_2013_median 
msa_year_level_sumstats$out_prem_2012_median  <- msa_year_level_sumstats$outcall_only_2012_median  - msa_year_level_sumstats$incall_only_2012_median 

msa_year_level_sumstats$chg_coli_2014 <- (msa_year_level_sumstats$COL_Composite_2014 / msa_year_level_sumstats$COL_Composite_2013) -1
msa_year_level_sumstats$chg_coli_2013 <- (msa_year_level_sumstats$COL_Composite_2013 / msa_year_level_sumstats$COL_Composite_2012) -1
msa_year_level_sumstats$chg_coli_2012 <- (msa_year_level_sumstats$COL_Composite_2012 / msa_year_level_sumstats$COL_Composite_2011) -1

msa_year_level_sumstats$chg_coli_trans_2014 <- (msa_year_level_sumstats$COL_Transport_2014 / msa_year_level_sumstats$COL_Transport_2013) -1
msa_year_level_sumstats$chg_coli_trans_2013 <- (msa_year_level_sumstats$COL_Transport_2013 / msa_year_level_sumstats$COL_Transport_2012) -1
msa_year_level_sumstats$chg_coli_trans_2012 <- (msa_year_level_sumstats$COL_Transport_2012 / msa_year_level_sumstats$COL_Transport_2011) -1

msa_year_level_sumstats$demeaned_violence_2014 <- msa_year_level_sumstats$violence_rate_2014 - mean(msa_year_level_sumstats$violence_rate_2014,na.rm=TRUE)
msa_year_level_sumstats$demeaned_violence_2013 <- msa_year_level_sumstats$violence_rate_2013 - mean(msa_year_level_sumstats$violence_rate_2013,na.rm=TRUE)
msa_year_level_sumstats$demeaned_violence_2012 <- msa_year_level_sumstats$violence_rate_2012 - mean(msa_year_level_sumstats$violence_rate_2012,na.rm=TRUE)

msa_year_level_sumstats$demeaned_commute_2014 <- msa_year_level_sumstats$commute_time_2014 - mean(msa_year_level_sumstats$commute_time_2014,na.rm=TRUE)
msa_year_level_sumstats$demeaned_commute_2013 <- msa_year_level_sumstats$commute_time_2013 - mean(msa_year_level_sumstats$commute_time_2013,na.rm=TRUE)
msa_year_level_sumstats$demeaned_commute_2012 <- msa_year_level_sumstats$commute_time_2012 - mean(msa_year_level_sumstats$commute_time_2012,na.rm=TRUE)

###
saveRDS(msa_level_prem, paste0(workingDatasets,"/Summary Datasets/msa_level_prem_not_missing.rds"))
saveRDS(msa_level, paste0(workingDatasets,"/Summary Datasets/msa_level.rds"))
saveRDS(msa_venue_prices, paste0(workingDatasets,"/Summary Datasets/msa_venue_level.rds"))
saveRDS(summary_stats_msa1, paste0(workingDatasets,"/Summary Datasets/summary_stats_msa1.rds"))
saveRDS(summary_stats_msa2, paste0(workingDatasets,"/Summary Datasets/summary_stats_msa2.rds"))
saveRDS(summary_stats_msa3, paste0(workingDatasets,"/Summary Datasets/summary_stats_msa3.rds"))
saveRDS(msa_year_level_sumstats, paste0(workingDatasets,"/Summary Datasets/msa_year_level_sumstats.rds"))

nrow(msa_year_level_sumstats) == length(unique(msa_year_level_sumstats$CBSACode))


##Correlation Plot
correlation_vars <- c("frac_white_GIANTOAK","unemployment_GIANTOAK","avg_commute_GIANTOAK",
                      "COL_Composite_2014","COL_Transport_2014","rape_rate_msa","property_rate_msa","violent_rate_msa","prostitution_rate_msa",
                      "ftsworn_rateper100k_2013","pop_estimate_2013","median_income","gon_rateper100k_2012",
                      "chl_rateper100k_2012")
m <- cor(as.data.frame(msa_level_master)[,correlation_vars], use = "complete.obs")



colnames(m) <- c("% White","Unemployment", "Avg. Commute", "COL Composite", "COL Transport", "Rape Rate", "Property Crime Rate", "Violent Crime Rate",
                 "Prostitution Rate", "FT Sworn Officer Rate", "Population", "Median Income", "Gonorrhea Rate", "Chlamydia Rate")

rownames(m) <- c("% White","Unemployment", "Avg. Commute", "COL Composite", "COL Transport", "Rape Rate", "Property Crime Rate", "Violent Crime Rate",
                 "Prostitution Rate", "FT Sworn Officer Rate", "Population", "Median Income", "Gonorrhea Rate", "Chlamydia Rate")



saveRDS(m,file = paste0(graphics,"corrplot.RDS"))


