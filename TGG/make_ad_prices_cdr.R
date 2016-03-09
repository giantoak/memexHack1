
###############################################################################

# Client: Giant Oak
# Author(s): AS
# Date created: 2015 12 14
# Purpose: Price aggregation code conversion. Translates the code in make_ad_pr
#          ices.py from python to R. This script uses CDR datasets.

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
# load in ad price extractions, does some cleaning, outputs aggregated prices
################################################################################


rates = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/rates-text.tsv"), sep="\t",
              data.table = TRUE, header=FALSE, select = c(2,3))

rates_ist = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/rates-from-ist.tsv"), sep="\t",
                  data.table=TRUE, header=FALSE)

setnames(rates, c("V2","V3"), c("ad_id", "rate"))

setnames(rates_ist, c("V1", "V2", "V3"), c("ad_id", "price", "time_str"))

# Drop '999+' and random tuple prices from rates_ist. There are only 18 of these anyway.

rates_ist[, price := as.numeric(price)]
rates_ist = rates_ist[!is.na(price)]

# Do string splits to get information about rates in separate columns

rates[, c("price", "time_str") := tstrsplit(rate, ",", fixed=TRUE)]

rates[, c("timeValue", "unit") := tstrsplit(time_str, " ", fixed=TRUE)]

rates_ist[, timeValue := sapply(time_str, FUN= function(x) gsub("\\D", "", x, perl=TRUE))]

rates_ist[, unit := "MINS"]

rates_ist[, rate := paste(paste0(price, ",", timeValue), unit)]

rates[, from_ist := 0]

rates_ist[, from_ist := 1]

# rbind rates and rates_ist, then drop duplicates at the ad-price-duration level. Hard code 1 hour time strings for rates_ist.

rates = rbind(rates, rates_ist)

rates = unique(rates, by = c('ad_id', 'price', 'timeValue'))

rates[time_str == '60min', time_str := '1 HOUR']

# Drop no duration rows

rates = rates[unit != 'DURATION']

cat('There are', dim(rates)[1], 'observations after dropping no duration prices')

# Change reference to multiple hours to just have unit string HOUR

rates[unit == 'HOURS', unit := 'HOUR']

# Creates a minutes column to normalize everything to minutes.

rates[unit == 'MINS', minutes := as.numeric(timeValue)]

rates[unit == 'HOUR', minutes := 60*as.numeric(timeValue)]

dollar_synonyms = c('\\$', 'roses', 'rose', 'bucks', 'kisses', 'kiss', 'dollars', 'dollar', 'dlr')

# Construct a regex to remove these terms from the price column entries

dollar_regex = paste0("(", paste0(dollar_synonyms, collapse="|"), ")")

rates[, price := sapply(price, FUN = function(x) gsub(dollar_regex, "", x))]

# Construct a regex to remove entries with foreign currencies

other_currencies = c('euro', 'eur', 'eu', 's', '¥', '\xef\xbc\x90', 'aud')

currency_regex = paste0("(", paste0(other_currencies, collapse="|"), ")")

rates = rates[!grepl(currency_regex, price)]

cat('There are', dim(rates)[1], 'prices after dropping foreign prices')

rates[, price := as.numeric(price)]

# saveRDS(rates, file = paste0(workingDatasets, "2016-01-21 -- Price Aggregations/All rates.RDS"))

# Start importing in the other data sources and merging onto the rates dataset

# Merge in content.tsv site names. This takes a while since content.tsv is huge.

content = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/content.tsv"), sep="\t",
                header=FALSE, stringsAsFactors = FALSE, data.table=TRUE, select=c(1,2))

setnames(content, c("V1", "V2"), c("ad_id", "site"))

# Output some tables for the XDATA report. Not related to price aggregation.

website = content[, .(count = .N), by='site']

setorder(website, -count)

website[, data_rank := .I]

# Get Alexa rankings

alexa = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/alexa_rankings.tsv"), sep="\t",
              header=TRUE, stringsAsFactors = FALSE, data.table=TRUE)

setnames(alexa, c("Site name", "US rank"), c("site", "us_rank"))

website = merge(website, alexa[,.(site, us_rank)], by="site")

website = website[!is.na(us_rank)]

setorder(website, us_rank)

website[, alexa_rel_rank := .I]

saveRDS(website, paste0(graphics, "xdata/alexa_rank.RDS"))

# Done with output for XDATA report. Continue merging datasets into rates.

rates = merge(rates, content, by = 'ad_id', all.x = TRUE)

rm(content)

# Drop rates that were extracted by IST and from the website naughtyreviews. A lot of the
# prices that were suspicious (over 10,000) satisfied these two conditions. 

dimrates = dim(rates)[1]

rates = rates[from_ist == 0 | site != "naughtyreviews"]

cat("Dropped", dimrates - dim(rates)[1], "observations that were from IST and naughtyreviews.com")

# Merge in massage parlor info

massage = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/ismassageparlorad_text.tsv"),
                sep="\t", header=FALSE, stringsAsFactors = FALSE, data.table=TRUE)

massage[, V3 := NULL]

setnames(massage, c("V1", "V2"), c("ad_id", "massage_ad"))

rates = merge(rates, massage, by = 'ad_id', all.x = TRUE)

rm(massage)

# Count the number of ads per ad id.

counts = rates[, .N, by='ad_id']

cat("The", dim(rates)[1], "extracted prices pertain to", dim(counts)[1], "observations")

setnames(counts, "N", "prices_from_ad")

out = merge(rates, counts, by = 'ad_id', all.x = TRUE)

# Merge in MSA data

msa = readRDS(paste0(rawDatasets, '2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/cbsa_no_duplicates.rds'))

out = merge(out, msa, all.x=TRUE)

rm(msa)

# Merge in post date data

ts = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/post_date-dom.tsv"),
           sep="\t", header=FALSE, stringsAsFactors = FALSE, data.table=TRUE)

setnames(ts, c("V1", "V2"), c("ad_id", "post_date"))

out = merge(out, ts, all.x=TRUE)

rm(ts)

# Merge in service types data

service = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/service-text.tsv"),
                sep="\t", header=FALSE, stringsAsFactors = FALSE, data.table=TRUE)

setnames(service, c("V1","V2"), c("ad_id", "incall_outcall"))

out = merge(out, service, all.x=TRUE)

rm(service)

# Merge in flags data

flags = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/flags-text.tsv"),
              sep="\t", header=FALSE, stringsAsFactors = FALSE, data.table=TRUE)

setnames(flags, c("V1", "V2"), c("ad_id", "flags"))

out = merge(out, flags, all.x=TRUE)

rm(flags)

# Merge in ethnicities data

ethnicities = fread(paste0(rawDatasets, "2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/ethnicities-text.tsv"),
                    sep="\t", header=FALSE, stringsAsFactors = FALSE, data.table=TRUE)

ethnicities[, V1 := NULL]

setnames(ethnicities, c("V2","V3"), c("ad_id", "ethnicity"))

out = merge(out, ethnicities, all.x=TRUE)

rm(ethnicities)

# Use NAs instead of \N

out[post_date == '\\N', post_date := NA]

# Remove some irrelvant columns

out[, unit := NULL]

out[, timeValue := NULL]

setorder(out, 'ad_id', na.last=TRUE)

# Now we have a merged dataset with cleaned rates and other relevant fields, start doing aggregations
# up to the ad level. First, we construct the multiplier that GO uses to normalize prices to the hour

# Find all the listed durations in the data. Only compute multipliers for durations that are referenced
# in more than 0.01% of the ads. This follows the original GO script for price aggregation (make_ad_prices.py).

minute_values = as.data.table(table(out$minutes))

total_minutes = sum(!is.na(out$minutes))

minute_values[, frac := N/total_minutes]

minute_values = minute_values[frac > 0.0001]

minute_values[, c('frac','N') := NULL]

# Create the dataframe specifiying the average price for a given duration (the column) for a 
# given ad id

result = out[,.(ad_id)]

result = unique(result, by='ad_id')

count = 0;

for(mins in as.list(minute_values$V1)){
  
  if(count == 0){
    temp_data = out[minutes == as.numeric(mins),  mean(price), by='ad_id']
    setnames(temp_data, "V1", paste0("minutes_", mins))
    result = merge(result, temp_data, by='ad_id')
    count = count + 1
  } else {
    temp_data = out[minutes == as.numeric(mins),  mean(price), by='ad_id']
    setnames(temp_data, "V1", paste0("minutes_", mins))
    result = merge(result, temp_data, by='ad_id', all=TRUE)
  }
  
  
}

# Compute the multipliers


price_ratios = data.table()
price_ratios_counts = data.table()


for(mins in as.list(minute_values$V1)){
  
  nam = paste0("minutes_", mins)
  hour_price = result[!is.na(minutes_60) & !is.na(get(nam)), mean(minutes_60)]
  m_price = result[!is.na(get(nam)) & !is.na(minutes_60), mean(get(nam))]
  tuple = list(as.numeric(mins), hour_price/m_price)
  count_tuple = list(as.numeric(mins), result[!is.na(get(nam)) & !is.na(get(nam)), .N])
  price_ratios = rbind(price_ratios, tuple, use.names=FALSE)
  price_ratios_counts = rbind(price_ratios_counts, count_tuple, use.names=FALSE)
}

setnames(price_ratios, c("V1","V2"), c("duration", "price_ratio"))
setnames(price_ratios_counts, c("V1","V2"), c("duration", "counts"))


# Output multiplier table for XDATA report. 

price_ratios_output = copy(price_ratios)

price_ratios_output[, price_ratio := round(price_ratio, digits=2)]

setnames(price_ratios_output, c("duration", "price_ratio"), c("Duration (min.)", "Rate Multiplier"))

saveRDS(price_ratios_output, file = paste0(graphics, "xdata/multiplier_table.RDS"))


# Split data to prices with and without hourly rates.

out[, onehr := (time_str == '1 HOUR')]

a = out[, .(hour_price_counts = sum(onehr)), by='ad_id']
a[, has_hour_price := (hour_price_counts > 0) ]

out = merge(out, a, by='ad_id', all.x=TRUE)

out = out[!is.na(ad_id)]

# The hourly prices are the ones that are TRUE in both has_hour_price and onehr columns.
# We take the simple average of these prices by ad id.

price_level_hourly = out[out$has_hour_price & out$onehr]

price_level_hourly[, price_per_hour_tgg := mean(price), by='ad_id']

# The non hourly rates are the ones with FALSE in has_hour_price

price_level_no_hourly = out[!out$has_hour_price]

# Merge with the price multipliers constructed before and then multiply.

price_level_no_hourly = merge(price_level_no_hourly, price_ratios, by.x = 'minutes', by.y = 'duration', all.x=TRUE)

price_level_no_hourly[, price_per_hour_tgg := price * price_ratio]

# Take the average of all the non hourly prices normalized to the hourly rate. Row bind to the ads with hourly prices
# Then drop duplicates.

price_level_no_hourly = price_level_no_hourly[, price_per_hour_tgg := mean(price_per_hour_tgg), by='ad_id']

price_level_hourly[, price_ratio := NA]

price_level = rbind(price_level_hourly, price_level_no_hourly)

price_level = unique(price_level, by='ad_id')

# Merge the aggregations with the master dataset constructed before

out = merge(out, price_level[,.(ad_id, price_per_hour_tgg)], by='ad_id', all.x=TRUE)

out = unique(out, by='ad_id')

# Merge in cluster ids, generated by clustering.R script.

cluster = fread(paste0(workingDatasets, '2016-01-21 -- Price Aggregations/clusters.tsv'), sep="\t",
                stringsAsFactors = FALSE, data.table=TRUE)

out = merge(out, cluster, all.x=TRUE)

# Compute the number of ads in the same cluster

out[, cluster_counts := .N, by='cluster_id']


# Subset to columns that are actually needed for regressions/analysis

out = out[, .(ad_id, from_ist, site, massage_ad, prices_from_ad, msa_code, post_date, incall_outcall,
              flags, ethnicity, hour_price_counts, has_hour_price, price_per_hour_tgg, cluster_id,
              cluster_counts)]

# Write to file

write.table(out, file=paste0(workingDatasets, '2016-01-21 -- Price Aggregations/ad_price_ad_level_cdr.tsv'), quote=FALSE,
            eol='\r\n', row.names=FALSE, sep="\t")

