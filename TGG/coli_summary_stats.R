###############################################################################

# Client: Giant Oak
# Author(s): TGG
# Date created: 2016 01 15
# Purpose: Bring in COLI data

###############################################################################
# Set the working directory and default paths
################################################################################

clientName <- 'Giant Oak'
projectName <- '01 Human Trafficking'
serverPath <- '~/Shared/00 Clients - Current'

rawDatasets <- paste(serverPath, '/', clientName, '/', projectName,
                     '/Structured Data/01 Raw Datasets/xdata/', sep="")
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
sourceFiles <- c()



if(length(sourceFiles > 0))
  sapply(sourceFiles, FUN = function(x) source(paste(code, x, ".R", sep = "")))

# End time for source files
end_source_files <- Sys.time()

################################################################################
# Load Hadi's cleaned datasets
################################################################################

years <- 2010:2014

#Load and append datasets
master_coli <- NULL
for (i in 1:length(years)) {
  year <- years[i]
  coli <- read.csv(file = paste0(workingDatasets, 'COLI/prices_', year, '.csv'),
                   stringsAsFactors = FALSE)
  coli$year <- years[i]
  assign(paste0('coli_', year), coli, envir = globalenv())
  #print(colnames(coli))
  if (all(colnames(coli) == colnames(coli_2010))) {
    print(paste('columns in year', year, 'are same as in 2010'))
    #APPEND ALL FILES
    master_coli <- rbind(master_coli, coli)
    
  } else {
    print(paste('WARNING: columns in year', year, 'are DIFFERENT as in 2010'))
  }
}

#Validate master coli takes all rows of subordinate files
nrow(master_coli) == sum(sapply(years, FUN = function(x) nrow(get(paste0('coli_', x)))))

 
################################################################################
# Calculate basic statistics
################################################################################
#datanames = sapply(years, function(i) paste('coli_',i,sep=""))
#sapply(datanames, function(i) parse(eval(text='i')))
datalist <- list(coli_2010,coli_2011,coli_2012,coli_2013,coli_2014)
summary(coli_2010)
summary(coli_2011)
summary(coli_2012)
summary(coli_2013)
summary(coli_2014)

#count occurences of CBSA Code
var_names <- c("Composite","Grocery","Housing","Utilities","Transport","Healthcare","Misc")
lapply(seq_along(datalist), function(i) length(unique(datalist[[i]]$CBSA_name)))

#get means
lapply(seq_along(datalist), function(i) sapply(seq_along(var_names), function(j) mean(datalist[[i]][[var_names[j]]])))

#get medians
lapply(seq_along(datalist), function(i) sapply(seq_along(var_names), function(j) median(datalist[[i]][[var_names[j]]])))

#get sd
lapply(seq_along(datalist), function(i) sapply(seq_along(var_names), function(j) sd(datalist[[i]][[var_names[j]]])))

#lapply(seq_along(datalist), function(i) sd(datalist[[i]]$Composite))

#check number of observations for each cbsa name
master_coli <- as.data.table(master_coli)
master_coli[,Occurences:=.N, by=CBSA_name]
master_coli_all_years <- master_coli[Occurences>4,,]
coli_cbsas <- master_coli[,.N, by=CBSA_name]
table(coli_cbsas$N)
combo_10_14 <- merge(coli_2010,coli_2014,by='CBSA_name')
cor(combo_10_14$Composite.x,combo_10_14$Composite.y)

write.csv(master_coli, paste0(finalDatasets, "COL/coli_master.csv"), row.names = FALSE)


################################################################################
# Graph Distributions
################################################################################
ggplot(master_coli, aes(x=Composite, color=as.factor(year))) + geom_density()
ggplot(master_coli, aes(x=Grocery, color=as.factor(year))) + geom_density()
ggplot(master_coli, aes(x=Housing, color=as.factor(year))) + geom_density()
ggplot(master_coli, aes(x=Transport, color=as.factor(year))) + geom_density()
ggplot(master_coli, aes(x=Utilities, color=as.factor(year))) + geom_density()
ggplot(master_coli, aes(x=Healthcare, color=as.factor(year))) + geom_density()
ggplot(master_coli, aes(x=Misc, color=as.factor(year))) + geom_density()

ggplot(combo_10_14, aes(x=Composite.x, y=Composite.y)) + geom_point() +theme_bw() +xlab('Composite 2010') +ylab('Composite 2014')


