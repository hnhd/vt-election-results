# load the libraries
library(stringr)
library(reshape2)

# set the working directory
location <- '~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/'
setwd(location)
split_by_year_elec <- F

# ======
# Build the Raw Index Files
# ======

# build the combinations vector
years <- c('2000', '2002', '2004', '2006', '2008', '2010', '2012', '2014', '2016')
election_types <- c('General', 'Primary')
geographies <- 'Precinct'
combinations <- sprintf('%s/%s/%s', 
                        expand.grid(years, election_types, geographies)[ ,1], 
                        expand.grid(years, election_types, geographies)[ ,2], 
                        expand.grid(years, election_types, geographies)[ ,3])

# build file name index
index.files <- character(0)
for (i in 1:length(combinations)) {
  index.files <- append(index.files, paste0(combinations[i], '/', list.files(combinations[i])))
}

# split the file name index to get the 
index.instruction <- str_split(index.files, '[/._]+')

# convert the list into a usable data 
raw.instruction <- data.frame(t(data.frame(lapply(index.instruction, head, n=8))), stringsAsFactors = FALSE)
raw.instruction$X7 <- ifelse(raw.instruction$X7 == 'pct', raw.instruction$X8, raw.instruction$X7)
rownames(raw.instruction) <- NULL
raw.instruction <- raw.instruction[, -c(5,6,8)]
colnames(raw.instruction) <- c('year', 'election_type', 'geography', 'office', 'district')

# remove years from the district type
raw.instruction$district <- ifelse(grepl('2...', raw.instruction$district), '', raw.instruction$district)

# add a wd column
raw.instruction$wd <- index.files

# load in index and add a party column for primary elections
index.party <- read.csv('../Index/index_party.csv', stringsAsFactors = FALSE)
raw.instruction$party <- ifelse(raw.instruction$election_type == 'Primary', index.party$party[match(str_sub(raw.instruction$wd, 31, 33), index.party$party_abbrev)], "")

# We have now created an index that will allow us to both find the files and reference the
# attributes needed for the creation of the list of dataframes. Next, we're going to build
# and then run the for loop that allows us build the list of data frames.

# ======
# Create a List of Dataframes
# ======

# create a list to store the tables
tables <- list()

# loop through and build the merged-cleaned tables
for (i in 1:length(raw.instruction$year)) {

# load the file in
raw_table <- read.csv(index.files[i], stringsAsFactors = FALSE)

# remove unnecessary columns columns
raw_table <- raw_table[ , -which(names(raw_table) %in% c("Ward"))]
# raw_table <- raw_table[ , -which(names(raw_table) %in% c("Total.Votes.Cast", "Ward", "Write.Ins", "Blanks"))]

# remove rows the are marked as TOTALS
raw_table <- raw_table[!grepl('TOTALS', raw_table$City.Town),]

# ncol = 2 means there were no candidates running
# nrow = 1 means that there was no votes by geography

# melt the raw table
if(nrow(raw_table) == 1 | ("No.Nomination" %in% names(raw_table))) {
  intermediate_table <- data.frame("City.Town" <- "", "Pct" <- "", "variable" <- colnames(raw_table[3]), "value" <- "")
  } else {
  intermediate_table <- melt(raw_table[-1 , ], c("City.Town", "Pct"))
}

# create an index table to load in the 
index.table <- data.frame(
  "party" = unname(unlist(raw_table[1,])),
  "candidate" = str_sub(colnames(raw_table), 1, nchar(colnames(raw_table))),
  stringsAsFactors = FALSE
)
index.table$party[index.table$party == ""] <- NA
index.table <- index.table[!(index.table$candidate %in% c("City.Town", "Pct")),]
index.table$party <- ifelse(is.na(index.table$party), index.table$candidate, index.table$party)

# add the candidates party, conditional upon being a party primary
if(raw.instruction$election_type[i] == 'General'){
  intermediate_table$party <- index.table$party[match(intermediate_table$variable, index.table$candidate)]
} else if (raw.instruction$election_type[i] == 'Primary') {
  intermediate_table$party <- NA
}

# add election year, district, and election type
intermediate_table$year <- raw.instruction$year[i]
intermediate_table$district <- raw.instruction$district[i]
intermediate_table$election_type <- ifelse(raw.instruction$election_type[i] == 'Primary', paste0(raw.instruction$party[i], ' Primary'), raw.instruction$election_type[i])

# rename the columns
colnames(intermediate_table) <- c('City/Town', 'Precinct', 'Candidate', 'Votes', 'Party', 'Year', 'District', 'Election Type')

# clean party name
intermediate_table$Party <- str_replace_all(intermediate_table$Party, '[\\.]', ' ')

# clean the candidate names (must be done after loading in the party)
intermediate_table$Candidate <- str_replace_all(intermediate_table$Candidate, '[\\.]{2}', '-.')
intermediate_table$Candidate <- str_replace_all(intermediate_table$Candidate, '[\\.]', ' ')
intermediate_table$Candidate <- str_replace_all(intermediate_table$Candidate, '[\\-]', '.')

# clean the City/Town names
intermediate_table$`City/Town` <- gsub("N\\.", "North", intermediate_table$`City/Town`)
intermediate_table$`City/Town` <- gsub("E\\.", "East", intermediate_table$`City/Town`)
intermediate_table$`City/Town` <- gsub("S\\.", "South", intermediate_table$`City/Town`)
intermediate_table$`City/Town` <- gsub("W\\.", "West", intermediate_table$`City/Town`)
intermediate_table$`City/Town`<- gsub("St\\.", "Saint", intermediate_table$`City/Town`)

# add election office
intermediate_table$Office <- raw.instruction$office[i]

# convert the abbreviations into the full names
index.office <- read.csv('../Index/index_office.csv', stringsAsFactors = FALSE)
intermediate_table$Office <- index.office$office[match(intermediate_table$Office, index.office$office_abbrev)]
intermediate_table$LoopValue <- i

# sort the columns
intermediate_table <- intermediate_table[c(10,6,9,8,7,1,2,3,5,4)]

# throw the intermediate table into a list
tables[[i]] <- intermediate_table
#names(tables[[i]]) <- raw.instruction$wd[i]
}

# perform some checks on the tables
# row.check <- unlist(lapply(tables, nrow))
# row.check
# tables[row.check == 1]
# col.check <- unlist(lapply(tables, ncol))
# col.check
# tables[col.check != 9]
# tables[[1512]]

# ======
# Merge the Dataframes and Export
# ======

# write a function that ensures dataframe doesn't have factors
rbind_clean <- function(...){rbind(..., stringsAsFactors = FALSE)}

# merge and write the combined tables
merged.table <- do.call(rbind_clean, tables)

# fill in total/write-in/blanks/spoiled to make sorting by party easier
#merged.table$Party <- ifelse(is.na(merged.table$Party) & merged.table$`Election Type` == "General", merged.table$Candidate, merged.table$Party)

sample <- subset(merged.table, LoopValue == "1521")

View(reshape2::dcast(sample, `City/Town`+Precinct ~ Candidate, value.var = "Votes"))
View(reshape2::dcast(sample, `City/Town`+Precinct ~ Party, value.var = "Votes"))

rm(list=setdiff(ls(), c("tables", "merged.table")))

# manually correct database lack of city/town specification
merged.table$`City/Town` <- ifelse(merged.table$LoopValue == 1275, 'Brattleboro', merged.table$`City/Town`) 
merged.table$`City/Town` <- ifelse(merged.table$LoopValue %in% c(1256, 1471, 1472, 1473), 'Rutland City', merged.table$`City/Town`) 

# add county names indexed by town name 
index.geography <- read.csv('../Index/index_geography.csv') # read in the index

# add in county column
merged.table$County <- index.geography$county[match(toupper(merged.table$`City/Town`), index.geography$town)]

# load in election values to serve as a reference for individuals/easy check
index.value <- read.csv('../Index/index_value.csv') # read in the index

# load in to properly translate the older names
index.district <- read.csv('../Index/index_district.csv')

# add in DistLong column for easier merging
merged.table$DistLong <- index.district$district[match(merged.table$District, index.district$dist_abbrev)]

remerge <- list()

# manually correct all typos to allow a proper join
merged.table$DistLong <- str_replace_all(merged.table$DistLong, "Chittdenden", "Chittenden")
index.value$district <- str_replace_all(index.value$district, "Chittdenden", "Chittenden")
merged.table$DistLong <- str_replace_all(merged.table$DistLong, "Windosr", "Windsor")
index.value$district <- str_replace_all(index.value$district, "Windosr", "Windsor")
merged.table$DistLong <- str_replace_all(merged.table$DistLong, "Caldeonia", "Caledonia")
index.value$district <- str_replace_all(index.value$district, "Caldeonia", "Caledonia")
index.value$district <- str_replace_all(index.value$district, "Bennington County", "Bennington")
index.value$district <- str_replace_all(index.value$district, "Chittenden County", "Chittenden")
index.value$district <- str_replace_all(index.value$district, "Caledonia County", "Caledonia")
merged.table$DistLong <- str_replace_all(merged.table$DistLong, "Windham Bennington Windsor 1", "Windham-Bennington-Windsor 1")
index.value$district <- str_replace_all(index.value$district, "Windham Bennington Windsor 1", "Windham-Bennington-Windsor 1")

# in order to properly append the data, I'll be subsetting to merges that will be unique
# subset to just general statewide
gnl_swd_only_merged <- subset(merged.table, `Election Type` == "General" & `Office` != 'State Senator' & `Office` != 'State Representative')
gnl_swd_only_index <- subset(index.value, election_type == "General" & office != 'State Senator' & office != 'State Representative')
remerge[[1]] <- merge(gnl_swd_only_merged, gnl_swd_only_index, by.x = c("Year", "Office", "District"), by.y = c("year", "office", "district"), all.x = T, all.y = F)[1:13]

# subset to just primary statewide
pmy_swd_only_merged <- subset(merged.table, str_detect(merged.table$`Election Type`, ".*Primary") & `Office` != 'State Senator' & `Office` != 'State Representative')
pmy_swd_only_index <- subset(index.value, str_detect(index.value$election_type, ".*Primary") & office != 'State Senator' & office != 'State Representative')
remerge[[2]] <-merge(pmy_swd_only_merged, pmy_swd_only_index, by.x = c("Year", "Office", "District", "Party"), by.y = c("year", "office", "district", "party"), all.x = T, all.y = F)[1:13]
 
# subset to just primary state senator and state rep
gnl_sdhd_only_merged <- subset(merged.table, `Election Type` == "General" & (`Office` == 'State Senator' | `Office` == 'State Representative'))
gnl_sdhd_only_index <- subset(index.value, election_type == "General" & (office == 'State Senator' | office == 'State Representative'))
remerge[[3]] <- merge(gnl_sdhd_only_merged, gnl_sdhd_only_index, by.x = c("Year", "Office", "DistLong"), by.y = c("year", "office", "district"), all.x = T, all.y = F)[1:13]

# subset to just primary state senator and state rep
pmy_sdhd_only_merged <- subset(merged.table, str_detect(merged.table$`Election Type`, ".*Primary") & (`Office` == 'State Senator' | `Office` == 'State Representative'))
pmy_sdhd_only_index <- subset(index.value, str_detect(index.value$election_type, ".*Primary") & (office == 'State Senator' | office == 'State Representative'))
remerge[[4]] <- merge(pmy_sdhd_only_merged, pmy_sdhd_only_index, by.x = c("Year", "Office", "DistLong", "Party"), by.y = c("year", "office", "district", "party"), all.x = T, all.y = F)[1:13]

# remerge the data frames
cleaned <- do.call(rbind, remerge)

# final layers of cleaning
names(cleaned)[13] <- "Election Value" # rename election value column
cleaned$DistLong <- NULL # remove dist long
cleaned$LoopValue <- NULL # remove loop value
cleaned$Votes <- ifelse(cleaned$Votes %in% c("", NA), 0, cleaned$Votes) # set all NA and blank votes to 0
cleaned <- subset(cleaned, Candidate != "No Nomination") # exclude elections with no candidate

write.csv(cleaned, '../Exports/vt_merged_precinct.csv')

# split and write the tables by years and election type
if (split_by_year_elec) {
# create an export index to divide the files
index.export <- expand.grid("Years" = years, "Election Types" = election_types)

# subset and export the tables by year x election_type combination
for (i in 1:length(index.export$Years)) {
  subset.table <- subset(merged.table, Year == index.export$Years[i] & `Election Type` == index.export$`Election Types`[i])
  subset_name <- paste0("../Exports/vt_", index.export$Years[i], "_", tolower(index.export$`Election Types`[i]), ".csv")
  write.csv(subset.table, subset_name)
}
}


# === BELOW ARE NOTES AND TESTS =====

# resolve No.Nomination
# value 1255 fails because there are no values -> need to come up with a solution
r <- GET("http://vtelectionarchive.sec.state.vt.us/elections/download/75586/precincts_include:1/")
bin <- content(r, "raw")
writeBin(bin, 'test.csv')

index.files[1512]
read.csv(index.files[1512], stringsAsFactors = FALSE)
# tables[[1192]]
# is.null(tables[[1365]])
# tables[[1]]


dist.town.county <- subset(cleaned, !is.na(County))
dist.town.county <- dist.town.county[ , c("District", "City/Town", "County")]
dist.town.county <- unique(dist.town.county)


# if nrow == 1, there can be a candidate who has gotten no votes (as getting votes = geography)
# no city/town, no precinct, have office, have candidate, potential party, potential no voters 

# Write code to capture 
# add the new rows to the correct tables
# export the tables
# iterate through function
# final step is to merge the files based on the year and election type