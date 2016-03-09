###############################################################################

# Client: Giant Oak
# Author(s): Emily Oehlsen + Tom Burr
# Date created: 2016 01 13
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
tgg.palette <- c("#AFCCBF", "#88888C", "#97BDD2", "#CDA39E", "#C8C8CA", "#FFEAB7")

theme_axis <- theme(panel.border = element_blank(), axis.line = element_line(colour = "black"), legend.title=element_blank())
theme_colors <- scale_colour_manual(values=tgg.palette)
theme_backg <- theme_bw(base_size=12)

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
sourceFiles <- c("summary_stat_functions.R")

source(paste(code,"crime_arrests","summary_stat_functions.R",sep="/"))

# End time for source files
end_source_files <- Sys.time()


################################################################################
# Read in data
################################################################################

# TGG-prepared UCR/LEMAS data at City and MSA Level
crime_city_tgg <- read.csv(paste0(workingDatasets, "Crime Data/ucr_lemas_city_level_tgg.csv", sep=""), stringsAsFactors=FALSE)
crime_msa_tgg <- read.csv(paste0(workingDatasets, "Crime Data/ucr_lemas_msa_level_tgg.csv", sep=""), stringsAsFactors=FALSE)

# TGG-prepared UCR/LEMAS data at the MSA-Year Level
crime_msa_year_level <- read.csv(paste0(workingDatasets, "Crime Data/crime_msa_year_level.csv", sep=""), stringsAsFactors=FALSE)

# TGG-prepared UCR/LEMAS data at the MSA-Year Level with only complete cases
crime_msa_year_level_complete <- read.csv(paste0(workingDatasets, "Crime Data/crime_msa_year_level_complete.csv", sep=""), stringsAsFactors=FALSE)

# Original GO UCR/LEMAS Data, which is at the city level despite name
crime_GO <- read.csv(paste0(rawDatasets, "2015-12-04 -- GO UCR LEMAS/ucr_lemas_msa.csv", sep=""), stringsAsFactors=FALSE)

# Remove unnecessary columns
crime_city_tgg$X.1 <- NULL
crime_city_tgg$X <- NULL
crime_msa_tgg$X.1 <- NULL
crime_msa_tgg$X <- NULL
crime_msa_year_level$X <- NULL
crime_msa_year_level_complete$X <- NULL


################################################################################
# Comparing GO and TGG datasets 
################################################################################

# Rename columns in TGG msa data
comparison_tgg <- crime_city_tgg

names(comparison_tgg) <- gsub("_crime", "", names(comparison_tgg))
colnames(comparison_tgg)[colnames(comparison_tgg) == "main_city"] <- "city"
colnames(comparison_tgg)[colnames(comparison_tgg) == "main_state"] <- "state"
colnames(comparison_tgg)[colnames(comparison_tgg) == "MSA.No."] <- "msa"

# Merge TGG msa data with Giant Oak data (we want to keep only those cities that are present in the Giant Oak data)
comparison <- merge(comparison_tgg, crime_GO, by = c("city", "state"), all.y=TRUE)

# N.B. If data is missing for a city, Giant Oak deletes this city from its data set

# RAPE

# Create comparison rape table and remove missings
comparison_rape <- comparison[, c("city", "state", "rape_2010.x", "rape_2010.y", "rape_2011.x", "rape_2011.y", "rape_2012.x", "rape_2012.y")]
View(comparison_rape)
comparison_rape <- comparison_rape[complete.cases(comparison_rape),]

# 2010
comparison_rape$identical_2010 <- ifelse(comparison_rape$rape_2010.x==comparison_rape$rape_2010.y, 1, 0)
identical(comparison_rape$rape_2010.x, comparison_rape$rape_2010.y)
table(comparison_rape$identical_2010)

# 2011
comparison_rape$identical_2011 <- ifelse(comparison_rape$rape_2011.x==comparison_rape$rape_2011.y, 1, 0)
identical(comparison_rape$rape_2011.x, comparison_rape$rape_2011.y)
table(comparison_rape$identical_2011)

# 2012
comparison_rape$identical_2012 <- ifelse(comparison_rape$rape_2012.x==comparison_rape$rape_2012.y, 1, 0)
identical(comparison_rape$rape_2011.x, comparison_rape$rape_2011.y)
table(comparison_rape$identical_2012)

# VIOLENT CRIME

# Create comparison violent table and remove missings
comparison_violent <- comparison[, c("city", "state", "violent_2010.x", "violent_2010.y", "violent_2011.x", "violent_2011.y", "violent_2012.x", "violent_2012.y")]
View(comparison_violent)
comparison_violent <- comparison_violent[complete.cases(comparison_violent),]

# 2010
comparison_violent$identical_2010 <- ifelse(comparison_violent$violent_2010.x==comparison_violent$violent_2010.y, 1, 0)
identical(comparison_violent$violent_2010.x, comparison_violent$violent_2010.y)
table(comparison_violent$identical_2010)

# 2011
comparison_violent$identical_2011 <- ifelse(comparison_violent$violent_2011.x==comparison_violent$violent_2011.y, 1, 0)
identical(comparison_violent$violent_2011.x, comparison_violent$violent_2011.y)
table(comparison_violent$identical_2011)

# 2012
comparison_violent$identical_2012 <- ifelse(comparison_violent$violent_2012.x==comparison_violent$violent_2012.y, 1, 0)
identical(comparison_violent$violent_2011.x, comparison_violent$violent_2011.y)
table(comparison_violent$identical_2012)

# PROPERTY CRIME

# Create comparison property table and remove missings
comparison_property <- comparison[, c("city", "state", "property_2010.x", "property_2010.y", "property_2011.x", "property_2011.y", "property_2012.x", "property_2012.y")]
View(comparison_property)
comparison_property <- comparison_property[complete.cases(comparison_property),]

# 2010
comparison_property$identical_2010 <- ifelse(comparison_property$property_2010.x==comparison_property$property_2010.y, 1, 0)
identical(comparison_property$property_2010.x, comparison_property$property_2010.y)
table(comparison_property$identical_2010)

# 2011
comparison_property$identical_2011 <- ifelse(comparison_property$property_2011.x==comparison_property$property_2011.y, 1, 0)
identical(comparison_property$property_2011.x, comparison_property$property_2011.y)
table(comparison_property$identical_2011)

# 2012
comparison_property$identical_2012 <- ifelse(comparison_property$property_2012.x==comparison_property$property_2012.y, 1, 0)
identical(comparison_property$property_2011.x, comparison_property$property_2011.y)
table(comparison_property$identical_2012)


################################################################################
# Information on missing data
################################################################################

# MSA Level
na_count <-data.frame(sapply(crime_msa_tgg, function(y) sum(length(which(is.na(y))))))
na_count

# We have ft_sworn and population for each MSA, and are missing between 35 and 52 of each crime variable
# 179 of the 299 are complete cases

summary(complete.cases(crime_msa_tgg))

# City Level
na_count_city <-data.frame(sapply(crime_city_tgg, function(y) sum(length(which(is.na(y))))))
na_count_city = na_count_city / 2730

# We are missing ft_sworn and population data for 7% of cities, and 80% of crime data is missing
# There are 324 Complete Cases

summary(complete.cases(crime_city_tgg))


################################################################################
# MSA-level summary stats
################################################################################

# Rape
summary_stats_msa <- sumStat(crime_msa_tgg, "rape_2010")
colnames(summary_stats_msa)[colnames(summary_stats_msa) == "Value"] <- "rape_2010"
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "rape_2011") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "rape_2011"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "rape_2012") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "rape_2012"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "rape_2013") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "rape_2013"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "rape_2014") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "rape_2014"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# Property
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "property_crime_2010") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "property_crime_2010"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "property_crime_2011") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "property_crime_2011"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "property_crime_2012") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "property_crime_2012"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "property_crime_2013") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "property_crime_2013"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "property_crime_2014") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "property_crime_2014"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# Violent
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "violent_crime_2010") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "violent_crime_2010"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "violent_crime_2011") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "violent_crime_2011"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "violent_crime_2012") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "violent_crime_2012"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "violent_crime_2013") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "violent_crime_2013"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "violent_crime_2014") 
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "violent_crime_2014"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# Law enforcement  
summary_stats_msa_temp <- sumStat(crime_msa_tgg, "ftsworn_2013_msa")
colnames(summary_stats_msa_temp)[colnames(summary_stats_msa_temp) == "Value"] <- "ftsworn_2013"
summary_stats_msa <- merge(summary_stats_msa, summary_stats_msa_temp, by = "Stats")

# View summary stats
View(summary_stats_msa)

# Complete cases
complete_crime_msa <- crime_msa_tgg %>% na.omit()

# 178 Complete Cases
# Only 4 of the top 10 MSAs have complete crime data

View(complete_crime_msa)


################################################################################
# Correlations of crimes 
################################################################################

# Keep relevant columns
corr <- crime_msa_tgg[, c("violent_crime_2010", "violent_crime_2011", "violent_crime_2012", "violent_crime_2013", "violent_crime_2014", 
                          "rape_2010", "rape_2011", "rape_2012", "rape_2013", "rape_2014",
                          "property_crime_2010", "property_crime_2011", "property_crime_2012", "property_crime_2013", "property_crime_2014")]

# Create correlation matrix
correlation_matrix <- cor(corr, use = "pairwise.complete.obs")

# View correlation matrix
View(correlation_matrix)


################################################################################
# Crime percentage change over time 
################################################################################

# File path for images
crime_summary <- paste(serverPath, '/', clientName, '/', projectName, "/Structured Data/04 Graphics and Output Data/msa_level_crime_graphs/", sep="")

require(plyr)
require(quantmod)

percentage_changes <- crime_msa_year_level[, c("MSA.No.", "year", "property", "rape", "violent")]

# Property
percentage_changes <- ddply(percentage_changes, "MSA.No.", transform,  DeltaCol = Delt(property))
colnames(percentage_changes)[colnames(percentage_changes) == "Delt.1.arithmetic"] <- "property_perc_change"

# Violent  
percentage_changes <- ddply(percentage_changes, "MSA.No.", transform,  DeltaCol = Delt(violent))
colnames(percentage_changes)[colnames(percentage_changes) == "Delt.1.arithmetic"] <- "violent_perc_change"

# Rape  
percentage_changes <- ddply(percentage_changes, "MSA.No.", transform,  DeltaCol = Delt(rape))
colnames(percentage_changes)[colnames(percentage_changes) == "Delt.1.arithmetic"] <- "rape_perc_change"

# View percentage changes
View(percentage_changes)

# Distributions of percentage changes
ggplot(percentage_changes, aes(property_perc_change)) + geom_histogram(bins = 100, na.rm = TRUE)
ggsave("property_perc_change_hist.png", plot = last_plot(), path = crime_summary)

ggplot(percentage_changes, aes(violent_perc_change)) + geom_histogram(bins = 100, na.rm = TRUE)
ggsave("violent_perc_change_hist.png", plot = last_plot(), path = crime_summary)

ggplot(percentage_changes, aes(rape_perc_change)) + geom_histogram(bins = 100, na.rm = TRUE)
ggsave("rape_perc_change_hist.png", plot = last_plot(), path = crime_summary)


################################################################################
# Distribution of crime, crime rate, and law enforcement 
################################################################################

# Keep relevant columns
dist <- crime_msa_year_level[, c("MSA.No.", "year", "property", "rape", "violent", "property_rate", "rape_rate", "violent_rate", "ftsworn_percap")]

# View probability densities 
dist_rape <- overlayed_density(dist, "rape")
dist_property <- overlayed_density(dist, "property")
dist_violence <- overlayed_density(dist, "violent")
dist_rape_rate <- overlayed_density(dist, "rape_rate")
dist_property_rate <- overlayed_density(dist, "property_rate")
dist_violent_rate <- overlayed_density(dist, "violent_rate")

png(file = paste(crime_summary, "normalqq_property.png"))
qqnorm(dist$property)
dev.off()

png(file = paste(crime_summary, "normalqq_rape.png"))
qqnorm(dist$rape)
dev.off()

png(file = paste(crime_summary, "normalqq_violent.png"))
qqnorm(dist$violent)
dev.off()

png(file = paste(crime_summary, "normalqq_property_rate.png"))
qqnorm(dist$property_rate)
dev.off()

png(file = paste(crime_summary, "normalqq_rape_rate.png"))
qqnorm(dist$rape_rate)
dev.off()

png(file = paste(crime_summary, "normalqq_violent_rate.png"))
qqnorm(dist$violent_rate)
dev.off()

png(file = paste(crime_summary, "ftsworn_percap.png"))
qqnorm(dist$ftsworn_percap)
dev.off()


################################################################################
# Graphics
################################################################################

## BoxPlot Sandbox ##
p <- ggplot(crime_msa_year_level,aes(factor(year),violent_percap))
p+geom_boxplot(fill = "#AFCCBF") +theme_axis + theme_colors + theme_backg + 
  labs(x = 'Year',  y = 'Violent Crimes (Per Capita)') +
  ggtitle("Violent Crimes Per Capita") 

p <- ggplot(crime_msa_year_level,aes(factor(year),rape_percap))
p+geom_boxplot(fill = "#AFCCBF") +theme_axis + theme_colors + theme_backg + 
  labs(x = 'Year',  y = 'Rapes (Per Capita)') +
  ggtitle("Rapes Per Capita") 

p <- ggplot(crime_msa_year_level_complete,aes(factor(year),rape_percap))
p+geom_boxplot(fill = "#AFCCBF") +theme_axis + theme_colors + theme_backg + 
  labs(x = 'Year',  y = 'Rapes (Per Capita)') +
  ggtitle("Rapes Per Capita") 

p <- ggplot(crime_msa_year_level,aes(factor(year),property_percap))

p+geom_boxplot(fill = "Aquamarine") +theme_axis + theme_colors + theme_backg + 
  labs(x = 'Year',  y = 'Property Crimes (Per Capita)') +
  ggtitle("Property Crimes Per Capita")
