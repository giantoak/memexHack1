
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
librariesToLoad <- c("jsonlite", "data.table", "reshape2", "ggplot2", "plyr", "Hmisc", "LaF", "ffbase", "scales", "igraph")



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
# Compute clusters, proxied as the transitive closure of links of contact      
# information.
################################################################################

# Read in rates from ad_prices_ad_level_cdr.tsv, and phones and email data from CDR sets.

phones = fread(paste0(rawDatasets, '2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/phone_numbers-text-and-dom.tsv'), 
               sep="\t", data.table=TRUE, header=FALSE)

emails = fread(paste0(rawDatasets, '2015-11-16 -- Stanford S3 Pulls/Escort CDR/CDR/email_addresses-text.tsv'),
               sep="\t", data.table=TRUE, header=FALSE)

rates = fread(paste0(workingDatasets, '2016-01-21 -- Price Aggregations/ad_price_ad_level_cdr.tsv'), sep="\t",
              data.table=TRUE, select=1)


setnames(phones, c("V1","V2"), c("ad_id", "contact_info"))

setnames(emails, c("V2", "V3"), c("ad_id", "contact_info"))

emails[, V1 := NULL]

# Row bind the phones and emails sets to create a master contact_info set

contact = rbind(phones, emails)

rm(emails)

rm(phones)

# Merge contact_info set onto rates by ad id

contact = merge(rates, contact, by= 'ad_id', all.x=TRUE)

# Create the "edgelist" that specifies the network graph by matching
# contact info for any ad_id with the first ad_id that occured with that 
# piece of contact information (itself, if the contact information is unqiue)
# Technically, we only have edges between ads sharing contact information 
# if one of the ads is the first occurence of that contact information. For example
# If two ads both have phone 1234, they will not have an edge in our edgelist if
# there is an ad before them with phone 1234. Instead, they will both have edges
# to that ad. But since they are connected through that ad, we will be able to cluster
# them together anyway.

contact[, links := contact$ad_id[match(contact_info, contact$contact_info)]]

# Create the vertex name data frame to label the nodes with contact information rather than ad_id

vertex = contact[, .(ad_id, contact_info)]

# Since sometimes there are multiple pieces of contact info associated with ads, we'd like to label those nodes
# with both pieces of contact info. These lapply creates the labeling string by pasting the contact info 
# together, separated by a comma.

vertex[, contact_string := lapply(.SD, paste0, collapse=","), by=ad_id]

vertex[, contact_info := NULL]

# Now we can drop duplicates, since we preserved all the contact information

vertex = unique(vertex, by="ad_id")

# Construct the graph data structure with the data frames we created. We wanted an undirected graph
# with the edgelist above and vertices labeled with the vertex dataframe

g = graph_from_data_frame(d = contact[, .(ad_id, links)], directed=FALSE, vertices = vertex)

# components calculates the maximal connected components of the graph. This is exactly the transitive closure

comps = components(g)

# Put the appropriate return values from components into a dataframe

clusters = as.data.frame(comps[[1]])

clusters$ad_id = rownames(clusters)

clusters = as.data.table(clusters)

setnames(clusters, "comps[[1]]", "cluster_id")

# Write table to file

write.table(clusters, file=paste0(workingDatasets, '2016-01-21 -- Price Aggregations/clusters.tsv'), quote=FALSE,
            eol='\r\n', row.names=FALSE, sep="\t")

# Function for plotting entire cluster (with all phone numbers for every node), given the ad_id string

plot_subgraph = function(x){
  
  subcom = subcomponent(g, v=x)
  test = subgraph(g, subcom)
  plot(test, vertex.size=3, vertex.label.cex=0.5, vertex.label=V(test)$contact_string, vertex.color=NA, sub= x)
  return()
  
}

# Cleaner function that only labels unique contact_string fields, given the ad_id string. Gives a cleaner
# representation so you can actually see what's going on.

plot_subgraph_clean = function(x){
  
  subcom = subcomponent(g, v = x)
  test = subgraph(g, subcom)
  node_names = vertex_attr(test, "contact_string", V(test))
  node_indexer = duplicated(node_names)
  vertex_attr(test, "contact_string", index = V(test)[node_indexer]) = ""
  plot(test, vertex.size=3, vertex.label.cex=1.2, vertex.label=V(test)$contact_string, vertex.color=NA, main= x)
  return()
  
}





