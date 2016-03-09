# install.packages(c("ggmap", "maptools", "RColorBrewer", "maps", "data.table"))
require(maps)
require(ggmap)
require(maptools)
require(RColorBrewer)
require(data.table)

per_capita_plot <- function(){
  locations <- fread("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/01 Raw Datasets/2015-11-16 -- Stanford S3 pulls/Escort CDR/CDR/cbsa_all.tsv")
  populations <- readRDS(file = paste0("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/02 Working Datasets/xdata/", "population/population_cbsa.RDS"))
  setnames(locations, c("ad_id", "cbsa_id", "test", "type"))
  locations <- locations[cbsa_id != "\\N"]
  count_by_cbsa <- locations[, list(num_ads=.N), by=cbsa_id]
  setnames(populations, c("cbsa_id",  "pop2010", "pop2011", "pop2012", "pop2013", "pop2014"))
  df <- merge(count_by_cbsa, populations, all.x=T, by='cbsa_id')
  df <- df[!is.na(pop2014)]
  
  df[, ads_per_thousand_population := num_ads/pop2014 * 1000]
  
  plt <- plot_msa_data_on_map(df, "ads_per_thousand_population", low_color="yellow", high_color="black", na_color = "white", plot_na = FALSE) + labs(x="", y="", title="Number of ads per capita in MSAs across the US", fill="Ads per\nthousand populationt\n(thousands)") + theme(legend.position = c(0.93,0.22))

  saveRDS(plt, "~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/04 Graphics and Output Data/Data summaries/counts_per_capita_by_msa_map.rds")
  
}

example_usage <- function(){
  # Let's do the count per MSA
  locations <- fread("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/01 Raw Datasets/2015-11-16 -- Stanford S3 pulls/Escort CDR/CDR/cbsa_all.tsv")
  setnames(locations, c("ad_id", "cbsa_id", "test", "type"))
  locations <- locations[cbsa_id != "\\N"]
  count_by_cbsa <- locations[, list(num_ads=.N), by=cbsa_id]
  plot_msa_data_on_map(count_by_cbsa, "num_ads")
}


# IMPORTANT: the input dataframe must have a column cbsa_id (5 digit number), as that is what it merges on

plot_msa_data_on_map <- function(data_to_plot, value_column, low_color = "gray", high_color = "red", na_color = 'gray', plot_na=TRUE, plot_folder = "~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/04 Graphics and Output Data/map plots/"){
  
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
  
  
  if(plot_na){
    merged_cbsa_data <- merge(cbsa_data, data_to_plot, by='cbsa_id', all.x=TRUE)
    cbsa_data_plot <- merge(area_cbsa.points, merged_cbsa_data[, list(id=id, val=get(value_column))], by='id', all.x=TRUE)
  } else{
    merged_cbsa_data <- merge(cbsa_data, data_to_plot, by='cbsa_id', all.x=TRUE)
    merged_cbsa_data <- merged_cbsa_data[!is.na(get(value_column))]
    cbsa_data_plot <- merge(area_cbsa.points, merged_cbsa_data[, list(id=id, val=get(value_column))], by='id')
    cbsa_data_plot <- data.table(cbsa_data_plot)[order(id, order)]
  }
  
  colors <- brewer.pal(9, "BuGn")
  
  plt <- ggplot() + 
    geom_polygon(aes(x=long, y=lat, group=group, fill=val), 
                                 color = colors[9],
                                 alpha = 0.5, 
                                 data=cbsa_data_plot) + scale_fill_gradient(low=low_color, high=high_color,  na.value = na_color) + 
    geom_polygon(aes(x=long, y=lat, group=group),
                 color='black',
                 fill=NA,
                 alpha=0.5,
                 data=area_state.points) + theme_bw() + scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL)
  
  plt

}
  