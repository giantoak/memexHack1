###############################################################################

# Client: Giant Oak
# Author(s): Nicole Ozminkowski, Pete Hlawitschka
# Date created: 2016 01 19
# Purpose: Compute Summary Statistics on UCR and Lemas variables
# test Git push

###############################################################################
# Set the working directory and default paths
###############################################################################

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


###############################################################################
# load in libraries and source files
###############################################################################

# libraries:
librariesToLoad <- c("jsonlite", "ggplot2", "curl", "data.table", "plyr", "microbenchmark")

sourceFiles <-c()

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

# Start time for source files
startTime <- Sys.time()

# End time for source files
endSourceFiles <- Sys.time()

###############################################################################
# function to compute summary statistics
###############################################################################

sumStat <- function(dataset, variableName, denomName = NULL) {
  
  # create a matrix for all information to go into
  summaryStats <- matrix(data=NA,nrow=18,ncol=2)
  summaryStats[1,1] <- 'Min'
  summaryStats[2,1] <- '5th Percentile'
  summaryStats[3,1] <- '10th Percentile'
  summaryStats[4,1] <- '25th Percentile'
  summaryStats[5,1] <- 'Median'
  summaryStats[6,1] <- 'Mean (unweighted)'
  summaryStats[7,1] <- 'Mean (weighted)'
  summaryStats[8,1] <- '75th Percentile'
  summaryStats[9,1] <- '90th Percentile'
  summaryStats[10,1] <- '95th Percentile'
  summaryStats[11,1] <- 'Max'
  summaryStats[12,1] <- 'Number NA'
  summaryStats[13,1] <- 'Percent NA'
  summaryStats[14,1] <- 'Standard Deviation'
  summaryStats[15,1] <- 'Average Distance from Mean (unweighted)'
  summaryStats[16,1] <- 'Percent Avg Dist from Mean (unweighted)'
  summaryStats[17,1] <- 'Average Distance from Mean (weighted)'
  summaryStats[18,1] <- 'Percent Avg Dist from Mean (weighted)'
  summaryStats <- data.frame(summaryStats, stringsAsFactors = FALSE)
  colnames(summaryStats) <- c('Stats', 'Value')
  
  # summary stats (min, max, mean, median, 1st&3rd quartiles, num NA (and create % NAs)) 
  summ <- summary(dataset[[variableName]])
  summaryStats$Value[summaryStats$Stats=='Min'] <- summ[['Min.']]
  summaryStats$Value[summaryStats$Stats=='25th Percentile'] <- summ[['1st Qu.']]
  summaryStats$Value[summaryStats$Stats=='Median'] <- summ[['Median']]
  summaryStats$Value[summaryStats$Stats=='Mean (unweighted)'] <- summ[['Mean']]
  summaryStats$Value[summaryStats$Stats=='75th Percentile'] <- summ[['3rd Qu.']]
  summaryStats$Value[summaryStats$Stats=='Max'] <- summ[['Max.']]
  #summaryStats$Value[summaryStats$Stats=='Number NA'] <- summ[["NA's"]]
  #summaryStats$Value[summaryStats$Stats=='Percent NA'] <- summ[["NA's"]]/nrow(dataset) 
  
  # quantiles: return 5%, 10%, 90%, 95%    
  quant <- quantile(dataset[[variableName]], probs = seq(0, 1, 0.05), na.rm = TRUE)
  summaryStats$Value[summaryStats$Stats=='5th Percentile'] <- quant[['5%']]
  summaryStats$Value[summaryStats$Stats=='10th Percentile'] <- quant[['10%']]  
  summaryStats$Value[summaryStats$Stats=='90th Percentile'] <- quant[['90%']]
  summaryStats$Value[summaryStats$Stats=='95th Percentile'] <- quant[['95%']]
  
  # standard deviation  
  stnd <- round(sd(dataset[[variableName]], na.rm = TRUE), 3)          
  summaryStats$Value[summaryStats$Stats=='Standard Deviation'] <- stnd
  
  # average distance from mean:  
  avg <- mean(dataset[[variableName]], na.rm = TRUE)
  absDist <- list(rep(NA, length(dataset[[variableName]])))
  absDist <- sapply(dataset[[variableName]], function(x) abs(x - avg))
  avgAbsDist <- mean(absDist, na.rm = TRUE)
  
  summaryStats$Value[summaryStats$Stats=='Average Distance from Mean (unweighted)'] <- avgAbsDist
  summaryStats$Value[summaryStats$Stats=='Percent Avg Dist from Mean (unweighted)'] <- avgAbsDist/summ[['Mean']]
  
  
  # weighted values, only if denoms/weights were supplied:
  
  if(!is.null(denomName)) {
    # weighted mean:
    totDenom <- sum(as.numeric(dataset[[denomName]]), na.rm = TRUE)
    wt <- as.numeric(dataset[[denomName]])/totDenom
    wtAvg <- sum(wt*dataset[[variableName]], na.rm = TRUE)
    summaryStats$Value[summaryStats$Stats=='Mean (weighted)'] <- wtAvg
    
    
    # average distance from weighted mean: $$$ you might have an error here or in other avg dist from changing to NA
    absDistWt <- list(rep(NA, length(dataset[[variableName]])))
    absDistWt <- sapply(dataset[[variableName]], function(x) abs(x - wtAvg))
    avgAbsDistWt <- mean(absDistWt, na.rm = TRUE)
    
    summaryStats$Value[summaryStats$Stats=='Average Distance from Mean (weighted)'] <- avgAbsDistWt
    summaryStats$Value[summaryStats$Stats=='Percent Avg Dist from Mean (weighted)'] <- avgAbsDistWt/wtAvg
  }
  
  # make sure all values are numeric and round to 3 decimal places
  summaryStats[, 2] <- as.numeric(summaryStats[, 2])
  summaryStats[, 2] <- round(summaryStats[, 2], 3)
  
  # $$$ include acs commute (prioritize)
  # $$$ don't forget to think about variation over time
  
  return(summaryStats)
}


# create function to apply summary stats to a list of datasets and save results in a csv file
listSumStats <- function(datasetList, yearList, variableName, denomName = NULL) {
  
  if (!is.null(denomName)) {
    output <- lapply(datasetList, function(x) sumStat(x, variableName, denomName ))
  } else {
    output <- lapply(datasetList, function(x) sumStat(x, variableName))
  }
  
  stats <- output[[1]]
  for (i in 1:length(yearList)) {
    varname <- paste0('Value (', yearList[i], ')')
    stats[[varname]] <- output[[i]][['Value']]
  }
  stats$Value <- NULL
  
  write.csv(stats, file = paste0(graphics, '/msa_level_crime_graphs/', variableName, '.csv'), row.names = FALSE)
  
  return(stats)
}


# Write a function to overlay, for each year, trendline-histograms for a given variable

overlayed_density <- function(longDataset, varname) {
  
  # remove underscores from varname, to make the a-axis label pretty
  label <- gsub("_", " ", varname)
  
  # draw the dang plot
  dens <- ggplot(longDataset, aes(x =get(varname), fill = year)) + 
    geom_density(alpha = 0.3) +
    labs(x = gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", label, perl=TRUE)
         , y ="Density")
  # plot the plot
  plot(dens)
  
  # save the plot $$$ CHANGE SO YOU USE ACS LONG
  ggsave(filename = paste0(graphics, '/msa_level_crime_graphs/', varname, '_density_plot.png'))
}
