
###############################################################################

# Client: Giant Oak
# Author(s): AS
# Date created: 2015 12 14
# Purpose: Cluster ads using contact information

###############################################################################
# Set the working directory and default paths
################################################################################

clientName <- 'Giant Oak'
projectName <- '01 Human Trafficking'
serverPath <- '~/Shared/00 Clients - Current'

rawDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                     '/Structured Data/01 Raw Datasets/', sep="")
workingDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                         '/Structured Data/02 Working Datasets/', sep="")
finalDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                       '/Structured Data/03 Final Datasets/', sep="")
graphics <- paste(serverPath, '/', clientName, '/', projectName,
                  '/Structured Data/04 Graphics and Output Data/', sep="")

code <- '~/Clients/Giant Oak/giant-oak'


################################################################################
# load in libraries and source files
################################################################################

# libraries:
librariesToLoad <- c("jsonlite", "data.table", "reshape2", "ggplot2", "plyr", "Hmisc", "LaF", "ffbase", "scales")



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


# load source files
#Specify files with constructor functions here
sourceFiles <- c()


if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, x, ".R", sep = "")))

################################################################################
# Do chi squared test for incall outcall proportions for with prices and
# without prices to compare sampling
################################################################################

with_prices = readRDS(paste0(workingDatasets, "2016-01-21 -- Price Aggregations/Archive/All rates.RDS"))

with_prices = as.data.table(with_prices)

services = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/service-text.tsv"), sep="\t",
                 header=FALSE, stringsAsFactors = FALSE, data.table=TRUE)

setnames(services, c("V1","V2"), c("ad_id", "incall_outcall"))

with_prices = merge(with_prices, services, by="ad_id", all.x=TRUE)

with_prices = unique(with_prices, by = 'ad_id')

# Note that there are ad_ids with prices that do not have incall or outcall information. There are only 1431 of these
# so we just drop them.

priced = with_prices[, .(price_extracted = .N), by=incall_outcall]

content = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/content.tsv"), sep="\t",
                header=FALSE, stringsAsFactors = FALSE, data.table=TRUE, select=c(1,2))

setnames(content, c("V1", "V2"), c("ad_id", "site"))

without_prices = merge(content, with_prices[,.(ad_id, from_ist)], all.x=TRUE)

# Subset to ad ids we know don't have price extractions

without_prices = without_prices[is.na(from_ist)]

without_prices = without_prices[,.(ad_id)]

without_prices = merge(without_prices, services, all.x=TRUE)

not_priced = without_prices[, .(no_price_extracted =.N), by=incall_outcall]

contingency = merge(priced, not_priced, by="incall_outcall")

# Drop the NAs

contingency_drop_nas = contingency[!is.na(incall_outcall)]

contingency_drop_nas = as.data.frame(contingency_drop_nas)

contingency_drop_nas$incall_outcall = NULL

rownames(contingency_drop_nas) = c("Incall", "Both", "Outcall", "Unknown")

contingency_drop_nas$pct_price = round(contingency_drop_nas$price_extracted/sum(contingency_drop_nas$price_extracted)*100, digits = 2)

contingency_drop_nas$pct_no_price = round(contingency_drop_nas$no_price_extracted/sum(contingency_drop_nas$no_price_extracted)*100, digits = 2)

contingency_drop_nas$pct_price = paste0("(", as.character(contingency_drop_nas$pct_price), "%)")

contingency_drop_nas$pct_no_price = paste0("(", as.character(contingency_drop_nas$pct_no_price), "%)")

contingency_drop_nas$'With price extractions' = paste(prettyNum(contingency_drop_nas$price_extracted, big.mark=","), contingency_drop_nas$pct_price, sep=" ")

contingency_drop_nas$'Without price extractions' = paste(prettyNum(contingency_drop_nas$no_price_extracted, big.mark=","), contingency_drop_nas$pct_no_price, sep=" ")

final = contingency_drop_nas[, c('With price extractions', 'Without price extractions')]

final['Total',] = c(prettyNum(sum(contingency_drop_nas$price_extracted), big.mark=","), prettyNum(sum(contingency_drop_nas$no_price_extracted), big.mark=","))

saveRDS(final, paste0(graphics, "xdata/venue_extraction.RDS"))

