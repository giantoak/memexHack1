###############################################################################
# Client: Giant Oak
# Author(s): Nicole Ozminkowski, Pete Hlawitschka
# Date created: 2016 01 19

# Purpose: Create regDataSet, the filtered dataset we use for regressions, by creating new variables and filtering data.
#          Additionally, we make heat maps at the bottom of this file.


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

#skip old regressions?
skip_old_regressions <- TRUE

###############################################################################
# Load: Libraries and source files
###############################################################################

# Libraries:
librariesToLoad <- c("jsonlite", "ggplot2", "curl", "data.table", "plyr", "microbenchmark",
                     "lmtest", "sandwich", "Hmisc", "compositions", "robCompositions")

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

# Load source files 
# Specify files with constructor functions here
sourceFiles <- c('/XDATA/xdata_analyses_functions')

if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, x, ".R", sep = "")))


######################################################################
# Bring in data sets from other scripts that we'll be using
######################################################################

master <- readRDS(file = paste0(finalDatasets, "Master MSA- Year-Level/msa_year_level_master.RDS"))
acs_master_wide <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master_wide.RDS'))
acs_master_long <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master_long.RDS'))
acs_master <- readRDS(file = paste0(workingDatasets, '/demographics/acs_master.RDS'))

# Turn all the column names to lower case before working with them
names(master) <- tolower(names(master)) 
names(acs_master[[1]]) <- tolower(names(acs_master[[1]]))
names(acs_master[[2]]) <- tolower(names(acs_master[[2]]))
names(acs_master[[3]]) <- tolower(names(acs_master[[3]]))



###############################################################################
# Functions:
###############################################################################


### Function: Summary statistics
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
    if("NA's" %in% names(summ)){
        summaryStats$Value[summaryStats$Stats=='Number NA'] <- summ[["NA's"]]
        summaryStats$Value[summaryStats$Stats=='Percent NA'] <- summ[["NA's"]]/nrow(dataset) 
    } else {
        summaryStats$Value[summaryStats$Stats=='Number NA'] <- 0
        summaryStats$Value[summaryStats$Stats=='Percent NA'] <- 0
    }
      
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
        
        # average distance from weighted mean: 
        absDistWt <- list(rep(NA, length(dataset[[variableName]])))
        absDistWt <- sapply(dataset[[variableName]], function(x) abs(x - wtAvg))
        avgAbsDistWt <- mean(absDistWt, na.rm = TRUE)
        
        summaryStats$Value[summaryStats$Stats=='Average Distance from Mean (weighted)'] <- avgAbsDistWt
        summaryStats$Value[summaryStats$Stats=='Percent Avg Dist from Mean (weighted)'] <- avgAbsDistWt/wtAvg
        }
      
    # make sure all values are numeric and round to 3 decimal places
    summaryStats[, 2] <- as.numeric(summaryStats[, 2])
    summaryStats[, 2] <- round(summaryStats[, 2], 3)


    return(summaryStats)
}
    
    
### Function: Apply sumStats to an msa-year level dataset
msaYearLevelSumStats <- function(longDataset, varname, yearList, yearVarName) {
  stats <- matrix(data=NA,nrow=18,ncol=0)

  for (i in 1:length(yearList)) {
    varNameYear <- paste(varname, yearList[i], sep = '_')
    longDataset[[varNameYear]] <- longDataset[longDataset[[yearVarName]] == yearList[i], varname]
    # automatically weight by 2013 population. If not, change 'pop_acs_13' below to weighting variable 
    stats <- cbind(stats, data.frame(sumStat(longDataset, varNameYear, 'pop_acs_13'))) 
    
  }
  
  write.csv(stats, file = paste0(graphics, '/demographics/summary_stats/', varname, '.csv'), row.names = FALSE)
  
}


### Function: Aggregate/add up (used in get_total for list of datasets)
add_up <- function(dataset, varlist, new_var_name) {
  cbound <- sapply(1:length(varlist), function(x) cbind(as.numeric(dataset[[varlist[x]]])))
  dataset[[new_var_name]] <- rowSums(cbound)
  return(dataset)
}


### Function: Create totals variables given a list of data sets
get_total <- function(dataset_list, var_list, name_list){
  for( i in 1:length(var_list)){
    dataset_list <- lapply(dataset_list, function(x) add_up(x, var_list[[i]], name_list[i]))
  }
  return(dataset_list)
}


### Function: Aggregate/add up (used for master (long) dataset, not for list of datasets)
add_up_long <- function(dataset, varlist, new_var_name, tot_names_f, tot_names_m) { # maybe add varnames of female/male </>18?
  
  newname_oftotal <- rep(NA, length(new_var_name))
  newname_offemale <- rep(NA, length(new_var_name))
  newname_ofmale <- rep(NA, length(new_var_name))
  
  for (i in 1:length(varlist)) {
    newname_oftotal[i] <- paste0('prop_of_total_', new_var_name[i])
    newname_offemale[i] <- paste0('prop_of_female_', new_var_name[i])
    newname_ofmale[i] <- paste0('prop_of_male_', new_var_name[i])
    data <- dataset[, varlist[[i]]]
    rowsum <- rowSums(data)
    dataset[[newname_oftotal[i]]] <- rowsum
  }
  
  dataset[['tot_prop_female']] <- dataset[[tot_names_f[1]]] + dataset[[tot_names_f[2]]]
  dataset[['tot_prop_male']] <- dataset[[tot_names_m[1]]] + dataset[[tot_names_m[2]]]
  for(i in 1:length(varlist)) {
    dataset[[newname_offemale[i]]] <- dataset[[newname_oftotal[i]]]/dataset[['tot_prop_female']]
    dataset[[newname_ofmale[i]]] <- dataset[[newname_oftotal[i]]]/dataset[['tot_prop_male']]
  }
  
  # get rid of vars that don't make sense
  bad_vars <- c(names(dataset)[grepl('of_female_male', names(dataset))], names(dataset)[grepl('of_male_female', names(dataset))])
  for(i in bad_vars) {
    dataset[[i]] <- NULL
  }
  
  # create list of age buckets and assign them to global variables
  final_namelist <- c(newname_oftotal, newname_offemale, newname_ofmale)
  assign('final_namelist', final_namelist, envir = globalenv())
  assign('propsOfTotal', newname_oftotal, envir = globalenv())
  assign('propsOfFemale', newname_offemale[grepl('female_female', newname_offemale)], envir = globalenv())
  assign('propsOfMale', newname_ofmale[grepl('male_male', newname_offemale)], envir = globalenv())
  
  return(dataset)
}

### Heat maps function ###

# Need these packages for the heat map to run
require(maps)
require(ggmap)
require(maptools)
require(RColorBrewer)
require(data.table)

plot_msa_data_on_map <- function(data_to_plot, value_column, low_color = "white", high_color = "red", na_color = 'gray', plot_na=TRUE, plot_folder = "~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/04 Graphics and Output Data/map plots/"){
    
    #This function has two required arguments and a few optional
    # REQUIRED: data_to_plot: is a dataframe with cbsa_id as a column another column
    # cbsa_id   num_ads
    # 43620     1000
    # 29340     2500
    # 45900     0
    # REQUIRED: value_column: is the name of the column to plot (e.g. num_ads)
    # OPTIONAL: low_color, high_color, and na_color are the colors to associate with the fill_scale. Importantly, there is a disctinction between 0 and NA. If you want them to be the same, either fill in NAs with 0, or give them the same color
    # OPTIONAL: plot_na is boolean for whether to even draw the border for MSAs with no value
    # OPTIONAL: plot_folder is just the path to the shared drive folder, is an argument to change if run locally and not the server
    
    
    area_cbsa <- readShapePoly(paste0(plot_folder, "boundaries/cb_2014_us_cbsa_500k/cb_2014_us_cbsa_500k.shp"))
    area_cbsa.points <- fortify(area_cbsa)
    
    area_states <- readShapePoly(paste0(plot_folder,"boundaries/cb_2014_us_state_500k/cb_2014_us_state_500k.shp"))
    area_state.points <- fortify(area_states)
    
    # continental USA
    area_state.points <- area_state.points[area_state.points$long > -124.8489 & area_state.points$long < -66.88544, ]
    area_state.points <- area_state.points[area_state.points$lat > 24.396 & area_state.points$lat < 49.384, ]
    area_cbsa.points <- area_cbsa.points[area_cbsa.points$long > -124.8489 & area_cbsa.points$long < -66.88544, ]
    area_cbsa.points <- area_cbsa.points[area_cbsa.points$lat > 24.396 & area_cbsa.points$lat < 49.384, ]
    

    cbsa_data <- as.data.table(area_cbsa@data)
    setnames(cbsa_data, c("csafp", "cbsa_fp", "aff_geo_id", "cbsa_id", "name", "lsad", "area_land", "area_water"))
    cbsa_data[, id := .I-1]
    merged_cbsa_data <- merge(cbsa_data, data_to_plot, by='cbsa_id', all.x=TRUE)
    
    if(plot_na){
        cbsa_data_plot <- merge(area_cbsa.points, merged_cbsa_data[, list(id=id, val=get(value_column))], by='id', all.x=TRUE)
    } else{
        cbsa_data_plot <- merge(area_cbsa.points, merged_cbsa_data[, list(id=id, val=get(value_column))], by='id')
    }
    
    colors <- brewer.pal(9, "BuGn")
    
    plt <- ggplot() + 
        geom_polygon(aes(x=long, y=lat, group=group, fill=val), 
                     color = colors[9],
                     alpha = 0.5, 
                     data=cbsa_data_plot) + scale_fill_gradient(low="yellow", high="black", na.value = "white") + 
        geom_polygon(aes(x=long, y=lat, group=group),
                     color='black',
                     fill=NA,
                     alpha=0.5,
                     data=area_state.points) + theme_bw() + scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL)
    
    plt
    
}

###############################################################################
### Create new variables we're going to want to use
###############################################################################

# Variable for the ilr regressions
master$pop_acs_millions <- master$pop_acs/1000000

# Make lists of variables to sum for our age buckets:    
    
# Make a bucket for each males and females of all ages
newvarnamelist <- names(master[grepl('prop_', names(master)) & grepl('years', names(master))])
male_all_ages <- newvarnamelist[grepl('_male', newvarnamelist)]
female_all_ages <- newvarnamelist[grepl('female', newvarnamelist)]

# Make buckets for other sex/age combinations
male_less_18 <- c('prop_male_under_5_years', 'prop_male_5_to_9_years', 'prop_male_10_to_14_years', 'prop_male_15_to_17_years')
female_less_18 <- c('prop_female_under_5_years', 'prop_female_5_to_9_years', 'prop_female_10_to_14_years', 'prop_female_15_to_17_years')
pop_less_18 <- c(male_less_18, female_less_18)

male_10_to_17 <- c('prop_male_10_to_14_years', 'prop_male_15_to_17_years')
female_10_to_17 <- c('prop_female_10_to_14_years', 'prop_female_15_to_17_years')

male_18_and_older <- male_all_ages[!(male_all_ages %in% male_less_18)]
female_18_and_older <- female_all_ages[!(female_all_ages %in% female_less_18)]
pop_18_and_older <- c(male_18_and_older, female_18_and_older)

female_18_to_21 <- c('prop_female_18_and_19_years', 'prop_female_20_years', 'prop_female_21_years')
male_18_to_21 <- c('prop_male_18_and_19_years', 'prop_male_20_years', 'prop_male_21_years')

female_18_to_24 <- c('prop_female_18_and_19_years', 'prop_female_20_years', 'prop_female_21_years', 'prop_female_22_to_24_years')
male_18_to_24 <- c('prop_male_18_and_19_years', 'prop_male_20_years', 'prop_male_21_years', 'prop_male_22_to_24_years')

femlist <- c('prop_of_total_female_less_18', 'prop_of_total_female_18_and_older')
manlist <- c('prop_of_total_male_less_18', 'prop_of_total_male_18_and_older')        
    

# A list of our buckets
varlist <- list(male_less_18, female_less_18, male_10_to_17, female_10_to_17, male_18_and_older, female_18_and_older, pop_less_18, pop_18_and_older, female_18_to_21, female_18_to_24, male_18_to_24, male_18_to_21)

# And a list of what we want to name the buckets
namelist <- c('male_less_18', 'female_less_18',  'male_10_to_17', 'female_10_to_17', 'male_18_and_older', 'female_18_and_older', 'pop_less_18', 'pop_18_and_older', 'female_18_to_21', 'female_18_to_24', 'male_18_to_24', 'male_18_to_21')


acs_master <- get_total(acs_master, varlist, namelist)
master <- add_up_long(master, varlist, namelist, femlist, manlist)

#Create larger buckets
for (i in c('female','male')) {
  
  master[[paste0('prop_', i, '_under_10')]] <- rowSums(master[, c(paste0("prop_", i, "_under_5_years") ,
                                                                          paste0("prop_", i, "_5_to_9_years"))])
  master[[paste0('prop_', i, '_10_17')]] <- rowSums(master[, c(paste0("prop_", i, "_10_to_14_years") ,
                                                                       paste0("prop_", i, "_15_to_17_years"))])
  master[[paste0('prop_', i, '_25_34')]]  <- rowSums(master[, c(paste0("prop_", i, "_25_to_29_years") ,
                                                                        paste0("prop_", i, "_30_to_34_years"))])
  master[[paste0('prop_', i, '_35_44')]] <- rowSums(master[, c(paste0("prop_", i, "_35_to_39_years"),
                                                                       paste0("prop_", i, "_40_to_44_years"))])
  master[[paste0('prop_', i, '_45_above')]] <- rowSums(master[, c(paste0("prop_", i, "_45_to_49_years"),
                                                                          paste0("prop_", i, "_50_to_54_years"),
                                                                          paste0("prop_", i, "_55_to_59_years"),
                                                                          paste0("prop_", i, "_60_and_61_years"),
                                                                          paste0("prop_", i, "_62_to_64_years"),
                                                                          paste0("prop_", i, "_65_and_66_years"),
                                                                          paste0("prop_", i, "_67_to_69_years"),
                                                                          paste0("prop_", i, "_70_to_74_years"),
                                                                          paste0("prop_", i, "_75_to_79_years"),
                                                                          paste0("prop_", i, "_80_to_84_years"),
                                                                          paste0("prop_", i, "_85_years_and_over"))])  
}


# Ads for providers under 24
master$ads_18_to_24_agead <- rowSums(master[, c('ads_18_to_19_agead', 'ads_20_to_20_agead', 'ads_21_to_21_agead', 'ads_22_to_24_agead')])

# List of variables for analysis with corresponding denominators
analysisListDenoms<- list(c('industry_male_dom_prop', NULL), c('immigrant_native_prop', NULL), c('immigrant_naturalized_prop', NULL), c('immigrant_not_cit_prop', NULL), c('sex_male_prop', NULL), c('marital_male_never_married_prop', NULL), c('marital_male_now_married_prop', NULL), c('marital_male_divorced_prop', NULL), c('marital_female_never_married_prop', NULL), c('marital_female_now_married_prop', NULL), c('marital_female_divorced_prop', NULL), c('avg_com', 'popestimate'), c('foster_kid_prop', 'pop_less_18'), c('educ_male_agg_hs_deg_prop', 'pop_18_and_older'), c('educ_male_agg_col_deg_prop', 'pop_18_and_older'), c('educ_male_agg_grad_deg_prop', 'pop_18_and_older'),  c('educ_female_agg_no_hs_prop', 'pop_18_and_older'), c('educ_female_agg_hs_deg_prop', 'pop_18_and_older'), c('educ_female_agg_col_deg_prop', 'pop_18_and_older'), c('educ_female_agg_grad_deg_prop', 'pop_18_and_older'))



###############################################################################
# Create regDataSet:
###############################################################################
    
### Filter data

# Filter -- we only want places with 1000+ ads/year 
regDataSet <- master[master$num_ads_agead > 1000 & !is.na(master$num_ads_agead ), ]
# This drops over half of our rows
nrow(regDataSet)/nrow(master)

# look at places with prop ads 18-24 below 5th pctile: 
quants <- quantile(regDataSet$ads_18_to_24_agead, probs = c(0, 0.005, 0.01, 0.05, 0.95, 0.99, 0.995), na.rm = TRUE)
regDataSet[regDataSet$ads_18_to_24_agead < quants[['5%']], c('cbsatitle', 'ads_18_to_24_agead', 'num_ads_agead')]
# Nothing crazy looking about them...

# Look at distribution
qplot(regDataSet$ads_18_to_24_agead)
# Looks fine, nice variation

# Create a set that excludes the bottom 1%
regDataSet1pp <- regDataSet[regDataSet$ads_18_to_24_agead > quants[['1%']], ]
regDataSet <- regDataSet1pp
nrow(regDataSet) #707
nrow(regDataSet)/nrow(master)
summary(regDataSet$num_ads_agead)

# Let's look at ads/capita
regDataSet$num_ads_per_capita <- regDataSet$num_ads_agead / regDataSet$pop_acs
# Let's drop obs where ad/capita is > .1  -- these seem like spam
nrow(regDataSet) # 707
regDataSet <- regDataSet[regDataSet$num_ads_per_capita < .1 & !is.na(regDataSet$num_ads_per_capita), ]
nrow(regDataSet) # 644

#  Save regDataSet so we can use it in other scripts
saveRDS(regDataSet, file = paste0(finalDatasets, '/demographics/regDataSet.RDS' ))




############################################################################################################################
### Create yearly percent change of each analysis variable, to display change from 2013 - 2014                            ##
############################################################################################################################

regDataSet <- readRDS(file = paste0(finalDatasets, '/demographics/regDataSet.RDS' ))

# Grab only the variables we end up using in the report:
age_str_analysis_vars <- c('ads_18_to_24_agead', 'prop_female_15_to_17_years', 'prop_of_total_female_18_to_24', 'prop_female_25_to_29_years', 'prop_female_30_to_34_years', 
                           'prop_female_35_to_39_years', 'ads_35_to_39_agead', 'prop_female_35_44', 'prop_female_45_above', 'foster_kid_prop', 'educ_female_agg_no_hs_prop',
                           'pop_acs', 'mean_age_female', 'mean_agead', 'med_age_female', 'median_agead', 'num_ads_agead')

# Create a data set with just years 2013 and 2014    
# Grab variables listed above for just years 2013 and 2014
pct_change_age_str_df_13 <- regDataSet[regDataSet$year==2013, c(age_str_analysis_vars, 'cbsacode', 'cbsatitle', 'year')]
pct_change_age_str_df_14 <- regDataSet[regDataSet$year==2014, c(age_str_analysis_vars, 'cbsacode', 'cbsatitle', 'year')]

# Check missing counts for each data set
sapply(pct_change_age_str_df_13, FUN = function(x) sum(is.na(x)))
sapply(pct_change_age_str_df_14, FUN = function(x) sum(is.na(x)))


# Merge the two into one data set
pct_change_age_str_df <- merge(pct_change_age_str_df_13, pct_change_age_str_df_14, by=c('cbsacode', 'cbsatitle'), all = TRUE)

# Quick validate merge:
nrow(pct_change_age_str_df_13) # 219
nrow(pct_change_age_str_df_14) # 254
nrow(pct_change_age_str_df) # 258

# We can drop the 2013- and 2014-only data sets, because we no longer use them
rm(pct_change_age_str_df_13)
rm(pct_change_age_str_df_14)

# Rename: Change names to y13 and y14:
names(pct_change_age_str_df) <- sub('\\.x', '_13', names(pct_change_age_str_df))
names(pct_change_age_str_df) <- sub('\\.y', '_14', names(pct_change_age_str_df))

# Create newname vectors, which we'll fill in with y-o-y percent changes
newname_pct_change_vars <- paste0('pctchange_13_14_', age_str_analysis_vars)
newname_abs_pct_change_vars <- paste0('abs_pctchange_13_14_', age_str_analysis_vars)

# Calculate the percentage changes (and absolute percent changes)
for(i in 1:length(age_str_analysis_vars)){
    
    # Calculate percent change from 2013 to 2014:
    pct_change_age_str_df[[newname_pct_change_vars[i]]] <- (pct_change_age_str_df[[paste0(age_str_analysis_vars[i], '_14')]] - pct_change_age_str_df[[paste0(age_str_analysis_vars[i], '_13')]])/pct_change_age_str_df[[paste0(age_str_analysis_vars[i], '_13')]]
    # Calculate absolute pct change 2013 to 2014:
    pct_change_age_str_df[[newname_abs_pct_change_vars[i]]] <- abs((pct_change_age_str_df[[paste0(age_str_analysis_vars[i], '_14')]] - pct_change_age_str_df[[paste0(age_str_analysis_vars[i], '_13')]]))/pct_change_age_str_df[[paste0(age_str_analysis_vars[i], '_13')]]
}


# Create summary stats files
years <- c('2012', '2013', '2014')

# uncomment to create new versions of summary stats files for all ACS variables:
# # create sum stats files for vars we didn't already have
# msaYearLevelSumStats(master, 'pct_below_21_agead', years, 'year')
# msaYearLevelSumStats(master, 'median_agead', years, 'year')

pct_change_age_str_df$year <- 2014

# Exclude zeros for the annoying ones that keep becoming +/- infinity: 
pct_change_age_str_nozero_df <- pct_change_age_str_df
nrow(pct_change_age_str_nozero_df)

# Keep only rows where value != 0 for vars where there used to be some values == 0
pct_change_age_str_nozero_df <- pct_change_age_str_nozero_df[pct_change_age_str_nozero_df$foster_kid_prop_13>0.0001, ]
pct_change_age_str_nozero_df <- pct_change_age_str_nozero_df[pct_change_age_str_nozero_df$foster_kid_prop_14>0.0001, ]
pct_change_age_str_nozero_df <- pct_change_age_str_nozero_df[pct_change_age_str_nozero_df$ads_18_to_24_agead_13>0.0001, ]
pct_change_age_str_nozero_df <- pct_change_age_str_nozero_df[pct_change_age_str_nozero_df$ads_18_to_24_agead_14>0.0001, ]
pct_change_age_str_nozero_df <- pct_change_age_str_nozero_df[pct_change_age_str_nozero_df$ads_35_to_39_agead_13>0.0001, ]
pct_change_age_str_nozero_df <- pct_change_age_str_nozero_df[pct_change_age_str_nozero_df$ads_35_to_39_agead_14>0.0001, ]
nrow(pct_change_age_str_nozero_df)
# Drop one obs where ads_35_to_39_agead_13 was == 0


### Filter 
    
    # Drop NAs (result of cbsa number/titles changing from 2013 to 2014)
    regDataSetPctChange <- pct_change_age_str_nozero_df[!is.na(pct_change_age_str_nozero_df$num_ads_agead_13 ), ]
    regDataSetPctChange <- pct_change_age_str_nozero_df[!is.na(pct_change_age_str_nozero_df$num_ads_agead_14 ), ]
    nrow(regDataSetPctChange) # 213
    
    # Keep only places with prop ads 18-24 above the 1st ptile of the 3-year data set (this is the min of RegDataSet bc we filter earlier on)
    
    # Cutoff = 1st percentile of our data. regDataSet already has this filter applied, so it will be the min(regDataSet$ads_18_to_24)
    cutoff <- quantile(regDataSet$ads_18_to_24_agead, probs = c(.0), na.rm = TRUE)
    
    # Apply cut off to our one-year data (2013)
    regDataSetPctChange1pp <- regDataSetPctChange[regDataSetPctChange$ads_18_to_24_agead_13 > cutoff, ]
    # Apply cut off to our one-year data (2014)
    regDataSetPctChange1pp <- regDataSetPctChange[regDataSetPctChange$ads_18_to_24_agead_14 > cutoff, ]
    
    nrow(regDataSetPctChange1pp) #213
    
    # Change percent changes into percentages by multiplying by 100
    regDataSetPctChange1pp[,grep("^pctchange",names(regDataSetPctChange1pp))] <- (regDataSetPctChange1pp[,grep("^pctchange",names(regDataSetPctChange1pp))] * 100)
    
    # Min should be no less than -100%
    # Check how often the minimum for any column is less than -100
    sum(sapply(regDataSetPctChange1pp, function(x) min(x) < -100), na.rm = TRUE)
    # 0. Nice.


# Use dataset filtered by prop ads:
regDataSetPctChange <- regDataSetPctChange1pp
saveRDS(regDataSetPctChange, file = paste0(finalDatasets, '/demographics/pct_change_nozero_data.RDS'))



####################################################################
### Heat maps
####################################################################

# Heat map of prop ads 18-24

plot_df <- regDataSet
plot_df$cbsa_id <- plot_df$cbsacode
plot_df$ageadSq <- (plot_df$ads_18_to_24_agead)
heatMapAds18to24 <- plot_msa_data_on_map(plot_df, 'ageadSq')
saveRDS(heatMapAds18to24, file = paste0(graphics,"/demographics/youngProviderMap.RDS"))


####################################################################
### Get states for CBSAs
####################################################################

crosswalk <- read.csv(paste0(rawDatasets,'/2015-11-18 -- County MSA Crosswalk/crosswalk_cbsa_fips_county.csv'), skip = 2)
crosswalk <- crosswalk[, c('CBSA.Code','CBSA.Title','State.Name','FIPS.County.Code')]
crosswalk <- crosswalk[(!is.na(crosswalk$FIPS.County.Code)),]

# Function: Report the modal value when given a vector
modal <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Apply modal function to each unique CBSA in the crosswalk file
for (code in unique(crosswalk$CBSA.Code)){
    # Subset data to one CBSA at a time
    peter <- crosswalk[crosswalk$CBSA.Code == code,]
    # Find the modal state in that one CBSA
    state <- modal(peter$State.Name)
    # Record that state in a new column called ModalState
    crosswalk[crosswalk$CBSA.Code == code, 'ModalState'] <- state
}

# For the crosswalk, we just care about CBSA Code and State
crosswalk <- crosswalk[,c('CBSA.Code','ModalState')]
crosswalk$ModalState <- as.character(crosswalk$ModalState)

# Aggregate the data to the CBSA level
crosswalk <- aggregate(.~CBSA.Code,data=crosswalk, FUN = head,1)

# Save
write.csv(crosswalk, file = paste0(workingDatasets, '/demographics/crosswalkState.csv'))



