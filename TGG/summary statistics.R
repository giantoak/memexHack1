require(data.table)
require(ggplot2)

setwd("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/01 Raw Datasets/2015-11-16 -- Stanford S3 pulls/Escort CDR/CDR/")

content <- fread("content.tsv", header=FALSE, col.names = c("ad_id", "source", "ads", "url", "text", "extractions"), sep="\t")
setkey(content, ad_id)
rates <- fread("rates-text.tsv", header=FALSE, col.names = c("ad_rates_id", "ad_id", "rate"), sep="\t")
rates_ist <- fread("rates-from-ist.tsv", header=FALSE, col.names = c("ad_id", "rate_ist", "duration_ist"), sep = "\t")
email <- fread("email_addresses-text.tsv", header=FALSE, sep="\t")
service <- fread("service-text.tsv", header=FALSE, sep="\t", col.names=c("ad_id", "services_offered"))
post_date <- fread("post_date-dom.tsv", header=FALSE, sep="\t", col.names = c("ad_id", "post_date"))
age_dom <- fread("age-dom.tsv", header=FALSE, sep="\t", col.names = c("ad_id", "advertised_age"))
cbsa <- fread("cbsa_all.tsv", header=FALSE, sep="\t", col.names = c("ad_id", "msa_code", "city", "location_type"))

# Number of ads, by origin
content[, .N]
setnames(content, "source", "website_origin")


by_source <- content[, list(num_ads=.N), by="website_origin"][order(-num_ads)]
by_source[, origin_factor := factor(website_origin, levels=website_origin)]
by_source[, percent_ads := num_ads/sum(num_ads) * 100]

saveRDS(by_source, "~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/02 Working Datasets/ad level/count_by_source.rds")

ggplot(data=by_source, aes(x=origin_factor, y=percent_ads)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x="Ad Website", y="Percent of scraped ads", title="Origin of ads")

age_dom <- age_dom[advertised_age != "\\N"]
age_dom[, length(unique(ad_id))]

age_dom_ad_level <- age_dom[, .(num_ages = .N), by=ad_id]
combined_rates <- merge(rates, rates_ist, by='ad_id', all=T)
rates_ad_level <- combined_rates[, .(num_rates = .N), by=ad_id]
cbsa_ad_level <- cbsa[, .(num_cbsa = .N), by=ad_id]

just_origin <- content[, list(ad_id, website_origin)]
rm(content)
rm(rates)
rm(rates_ist)
gc()

df <- merge(just_origin, age_dom_ad_level, by='ad_id', all=T)
df <- merge(df, rates_ad_level, by='ad_id', all=T)
df <- merge(df, cbsa_ad_level, by='ad_id', all=T)
df <- merge(df, service, by='ad_id', all=T)
df <- merge(df, post_date[post_date != "\\N"], by='ad_id', all=T)

df[, num_missing := rowSums(is.na(df))]

saveRDS(df, "~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/02 Working Datasets/ad level/ad_level.rds")

output_folder <- "~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/04 Graphics and Output Data/Data summaries/"

df <- readRDS("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/02 Working Datasets/ad level/ad_level.rds")

# View(df[1:100])

# apply(df, 1, function(x) sum(is.na(x)))

num_non_missing = sapply(df, function(x) sum(!is.na(x)))

num_non_missing = data.table(num_ads = num_non_missing, data = names(num_non_missing))
new_df = num_non_missing[data %in% c("ad_id", "num_ages", "num_rates", "num_cbsa", "services_offered")][order(-num_ads)]

new_df[, data_factor := factor(data, levels=data)]

data_availability_plot <- ggplot(new_df, aes(x=data_factor, y=num_ads)) + geom_bar(stat='identity') + scale_x_discrete(labels=c("Number of ads", "Incall/Outcall\n data", "MSA location \ndata", "Advertised age\ndata", "Price\nextractions")) + labs(x="", y="Number of ads (Millions)") + scale_y_continuous(labels=function(x)x/1000000, limits=c(0,50000000), minor_breaks=waiver())

# a + theme_axis + theme_backg

saveRDS(object=data_availability_plot, file=paste0(output_folder, "data_availability_plot.rds"))

new_df[, percent_total := num_ads/42793523 * 100]


theme_axis <- theme(panel.border = element_blank(), axis.line = element_line(colour = "black"), legend.title=element_blank())
theme_colors <- scale_colour_manual(values=tgg.palette)
theme_backg <- theme_bw(base_size=16)




by_source <- readRDS("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/02 Working Datasets/ad level/count_by_source.rds")

ad_origin_plot <- ggplot(data=by_source, aes(x=origin_factor, y=percent_ads)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x="Ad Website", y="Percent of scraped ads", title="Origin of ads")

saveRDS(object=ad_origin_plot, file=paste0(output_folder, "ad_origin_plot.rds"))


# over time

View(post_date[1:10])

clean_post_date <- post_date[post_date != "\\N"]
by_day <- clean_post_date[, .N, by=post_date]
by_day[, post_date_dt := as.Date(post_date, format="%Y-%m-%d")]
by_day <- by_day[post_date > as.Date("2010-01-01", formate="%Y-%m-%d")]

ads_scraped_over_time_plot <- ggplot(by_day[post_date > as.Date("2012-01-01", formate="%Y-%m-%d"), list(num_ads=N, post_date=post_date_dt)], aes(x=post_date, y=num_ads)) + geom_line() + labs(x="Ad post date", y="Number of Ads (thousands)", title="Number of ads scraped daily") + scale_y_continuous(labels=function(x)x/1000)


saveRDS(ads_scraped_over_time_plot, paste0(output_folder, "ads_scraped_over_time.rds"))



# subset to ads for analysis

ad_level_msa_char <- readRDS("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/03 Final Datasets/xdata/ad_price_msa_level.rds")
final_data <- ad_level_msa_char[, list(ad_id=ad_id, final_dataset=TRUE)]

by_source_with_prices <- ad_level_msa_char[, .N, by=site][order(-N)]
by_source_with_prices[, site_factor := factor(site, levels=site)]
by_source_with_prices[, percent_ads := N/sum(N) * 100]
final_ads_origin_plot <- ggplot(by_source_with_prices, aes(x=site_factor, y=percent_ads)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(x="Ad Website", y="Percent of scraped ads", title="Origin of ads")
saveRDS(final_ads_origin_plot, paste0(output_folder, "final_ads_origin_plot.rds"))

by_day_final_ads <- ad_level_msa_char[, .N, by=post_date]
by_day_final_ads[, post_date := as.Date(post_date, format="%Y-%m-%d")]

final_ads_post_date <- ggplot(by_day_final_ads[post_date > as.Date("2012-01-01", formate="%Y-%m-%d")], aes(x=post_date, y=N)) + geom_line()
saveRDS(final_ads_post_date, paste0(output_folder, "final_ads_post_date.rds"))

final_ads_count_by_cbsa <- ad_level_msa_char[, list(num_ads=.N), by=CBSACode]
setnames(final_ads_count_by_cbsa, "CBSACode", "cbsa_id")
final_ads_count_map_plot <- plot_msa_data_on_map(final_ads_count_by_cbsa, "num_ads")
saveRDS(final_ads_count_map_plot, paste0(output_folder, "final_ads_count_map.rds"))


df <- readRDS("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/02 Working Datasets/ad level/ad_level.rds")

locations <- fread("~/Shared/00 Clients - Current/Giant Oak/01 Human Trafficking/Structured Data/01 Raw Datasets/2015-11-16 -- Stanford S3 pulls/Escort CDR/CDR/cbsa_all.tsv")
setnames(locations, c("ad_id", "cbsa_id", "city", "type"))
locations <- locations[!duplicated(ad_id)]
df <- merge(df, final_data, by='ad_id', all=T)
df <- merge(df, locations[, list(ad_id, cbsa_id)], by='ad_id', all.x=TRUE)
df[is.na(final_dataset), final_dataset:=FALSE]


by_cbsa_dataset <- df[cbsa_id != "\\N", list(num_ads=.N), by=list(final_dataset, cbsa_id)]

levels <- by_cbsa_dataset[final_dataset == FALSE][order(num_ads)][, cbsa_id]

by_cbsa_dataset[, pct_ads_cbsa := 100 * num_ads / sum(num_ads), by=final_dataset]

by_cbsa_dataset[, factor_cbsa_id := factor(cbsa_id, levels=levels)]

cbsa_sample_comparisons <- ggplot(by_cbsa_dataset[pct_ads_cbsa > 0.1], aes(x=factor_cbsa_id, y=pct_ads_cbsa, group=final_dataset, colour=final_dataset)) + geom_point() + theme(axis.ticks.x=element_blank(), axis.text.x = element_blank()) + labs(x="MSA location of ad", y="Percent of ads in sample", colour="Dataset", title="Comparison of locations of ads with and without prices") + scale_colour_discrete(labels=c("All ads without\nclean prices", "Sample of ads\nwith clean prices"))

# cbsa_sample_comparisons

saveRDS(cbsa_sample_comparisons, paste0(output_folder, "sample_comparisons_by_cbsa.rds"))





# number by msa



