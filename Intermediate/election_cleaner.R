library(stringr)
library(reshape)


## reformat_cd('2014', 'general', 'precinct', '')
## reformat_cd <- function(year, election_type, geography, district){

year <- '2014'
election_type <- 'general'
geography <- 'precinct'
district <- ''

directory <- paste0(year, "/", election_type, "/", geography)
files <- list.files(directory)
file_names <- str_extract(files, ".{3}") 

# throw the files into a list
raw_csvs <- lapply(paste0(directory, "/", files), read.csv)
names(raw_csvs) <- file_names

# merge the files together 
merged <- do.call(what = cbind, args = raw_csvs)
merged <- data.frame(lapply(merged, as.character), stringsAsFactors=FALSE)

# remove columns with no values
merged <- merged[colSums(!is.na(merged)) > 0]

# rename first column, remove excess
colnames(merged)[1] <- "geog"
merged <- merged[ , -grep("City.Town", colnames(merged))]
merged <- merged[ , -grep("Total.Votes.Cast", colnames(merged))]
merged <- merged[ , -grep("Ward", colnames(merged))]
for (i in 1:length(merged[, grep("Pct", colnames(merged))[1]])) {
  if (merged[i, grep("Pct", colnames(merged))[1]] > 0) {
    merged$precinct[i] <- merged[i, grep("Pct", colnames(merged))[1]]
  } else {
    merged$precinct[i] <- merged$geog[i]
  }
}
merged <- merged[ , -grep("Pct", colnames(merged))]

# store values separately
merged_var <- data.frame(
  "office" = str_extract(colnames(merged), ".{3}"),
  "party" = unname(unlist(merged[1,])),
  "candidate" = str_replace_all(str_sub(colnames(merged), 5, nchar(colnames(merged))), "\\.", " "))

# drop fake row
merged_var <- merged_var[-1, ]

# convert abbreviated labels
office_full <- data.frame(
  "abbrev" = c("agn", "atg", "gov", "ltg", "prs", "rep", "sos", "sen", "trs"),
  "full" = c("Auditor General", "Attorney General", "Governor", "Lieutenant Governor", "President", "U.S. House", "Secretary of State", "U.S. Senate", "Treasurer"))
merged_var$office <- office_full$full[match(merged_var$office, office_full$abbrev)]


# ==
# Clean Merged
# ==

# drop party & merged row from merged
merged <- merged[-1, ]
merged <- merged[merged$geog != 'TOTALS', ]

# clean municipality name
merged$geog <- gsub("N\\.", "North", merged$geog)
merged$geog <- gsub("E\\.", "East", merged$geog)
merged$geog <- gsub("S\\.", "South", merged$geog)
merged$geog <- gsub("W\\.", "West", merged$geog)
merged$geog <- gsub("St\\.", "Saint ", merged$geog)

# melt the data frame
merged_melt <- melt(merged, c("geog", "precinct"))
colnames(merged_melt) <- c("town", "precinct", "id", "votes")

# add in office, candidate, and party
merged_melt$district <- district
merged_melt$office <- str_extract(merged_melt$id, ".{3}")
merged_melt$candidate <- str_replace_all(str_sub(merged_melt$id, 5, 50), "\\.", " ")
merged_melt$candidate <- str_replace_all(merged_melt$candidate, "  ", " ")
merged_melt$office <- office_full$full[match(merged_melt$office, office_full$abbrev)]
merged_melt <- merged_melt[-3]
merged_melt$party <- merged_var$party[match(merged_melt$candidate, merged_var$candidate)]

# import county using vermont legend
geog_legend <- read.csv("legend_vermont.csv")
merged_melt$county <- geog_legend$county[match(toupper(merged_melt$town), geog_legend$town)]

# reformat vote column
merged_melt$votes <- as.numeric(gsub("," , "", merged_melt$votes))

# reorder
merged_melt <- merged_melt[,c(8,1,2,5,4,7,6,3)] 

# write the csv
write.csv(merged_melt, paste0("final/vt_", year, "_", election_type, "_", geography, ".csv"), row.names = FALSE, na = "")

##}

paste0(year, "_", election_type, "_", geography)
