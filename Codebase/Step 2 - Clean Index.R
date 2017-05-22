library(stringr)
library(httr) #for GET

# read the cleaned index
vt.titles <- na.omit(read.csv('../Index/index_titles.csv'))

# build vt.titles component columns
vt.titles$year <- str_extract(vt.titles$title, '\\d{4}')
vt.titles$election_type <- ifelse(grepl("Primary", vt.titles$title), "Primary", "General")
vt.titles$district <- ifelse(vt.titles$election_type == "Primary", trimws(str_replace(str_replace(str_extract(vt.titles$title, "Primary.*"), "Primary", ""), "District", "")), 
                      ifelse(vt.titles$election_type == "General", trimws(str_replace(str_replace(str_extract(vt.titles$title, "Election.*"), "Election", ""), "District", "")), ""))
vt.titles$party <- ifelse(vt.titles$election_type == "Primary", str_extract(str_extract(vt.titles$title, "\\w+ Primary"), "\\w+"), "")
vt.titles$party <- ifelse(grepl("Union", vt.titles$title), "Liberty Union", str_extract(str_extract(vt.titles$title, "\\w+ Primary"), "\\w+"))
vt.titles$party <- str_replace(vt.titles$party, 'Repubilcan', 'Republican')
vt.titles$office <- ifelse(grepl("President", vt.titles$title), "President", 
                    ifelse(grepl("U.S. Senate", vt.titles$title), "U.S. Senate", 
                    ifelse(grepl("U.S. House", vt.titles$title), "U.S. House", 
                    ifelse(grepl("Lieutenant Governor", vt.titles$title), "Lieutenant Governor", 
                    ifelse(grepl("Governor", vt.titles$title), "Governor", 
                    ifelse(grepl("Treasurer", vt.titles$title), "Treasurer",
                    ifelse(grepl("Secretary of State", vt.titles$title), "Secretary of State", 
                    ifelse(grepl("Auditor", vt.titles$title), "Auditor", 
                    ifelse(grepl("State's Attorney General", vt.titles$title), "Local Election", 
                    ifelse(grepl("Attorney General", vt.titles$title), "Attorney General", 
                    ifelse(grepl("State Senator", vt.titles$title), "State Senator", 
                    ifelse(grepl("State Representative", vt.titles$title), "State Representative", "Local Election"))))))))))))

# save cleaned vt.titles as index_value
write.csv(vt.titles, '../Index/index_value.csv')

# subset to the openelections range
vt.cleaned <- subset(vt.titles, office != "Local Election" & year >= 2000, -title)

subset(vt.cleaned, election_value == 68297)
# remove 'County' from districts containing 'County' in their title
vt.cleaned$district <- trimws(ifelse(grepl('County', vt.cleaned$district), str_replace(vt.cleaned$district, 'County', ''), vt.cleaned$district))

# build the office_fill index
index.office <- read.csv('../Index/index_office.csv')

# merge the office abbreviations into the cleaned index
vt.cleaned <- merge(vt.cleaned, index.office, by = "office", all = TRUE)

# load and merge the district abbreviations into the cleaned index
index.dist <- read.csv('dist_index_2000+.csv')
vt.cleaned$district <- trimws(vt.cleaned$district)
index.dist$dist_abbrev <- trimws(index.dist$dist_abbrev)
vt.cleaned <- merge(vt.cleaned, index.dist, by = "district", all = TRUE)

# load and merge the district abbreviations into the cleaned index
index.party <- data.frame(
  "party" = c(na.omit(unique(vt.cleaned$party))), 
  "party_abbrev" = c('dem', 'lbu', 'prg', 'rep'))
vt.cleaned <- merge(vt.cleaned, index.party, by = "party", all = TRUE)

# remove values where atg district is populated
vt.cleaned <- subset(vt.cleaned, !(office_abbrev == 'atg' & district != ''))

# create all the folders for the length of the years
sapply(unique('Election Results/', vt.cleaned$year), dir.create)
sapply(paste0('Election Results/', rep(unique(vt.cleaned$year),2), '/', rep(unique(vt.cleaned$election_type),2)), dir.create)
sapply(paste0('Election Results/', rep(unique(vt.cleaned$year),2), '/', rep(unique(vt.cleaned$election_type),2), '/Precinct'), dir.create)

# download files into the correct folders
for (i in 1:length(vt.cleaned$office)) {
  r <- GET(sprintf("http://vtelectionarchive.sec.state.vt.us/elections/download/%s/precincts_include:1/", vt.cleaned$election_value[i]))
  bin <- content(r, "raw")
  filename <- paste0(vt.cleaned$year[i], '/', vt.cleaned$election_type[i], '/Precinct/', vt.cleaned$office_abbrev[i], '_',tolower(substr(vt.cleaned$election_type[i],1,3)),
         if(vt.cleaned$election_type[i] == 'Primary') {paste0("_", vt.cleaned$party_abbrev[i])}, '_pct', 
         if(vt.cleaned$office_abbrev[i] == 'srp' | vt.cleaned$office_abbrev[i] == 'ssn'){paste0("_", vt.cleaned$dist_abbrev[i])}, "_", vt.cleaned$year[i], 
         ".csv")
  writeBin(bin, filename)
}


subset(vt.cleaned, is.na(year))
View(subset(vt.cleaned, is.na(office_abbrev)))
subset(vt.cleaned, office_abbrev == 'atg' & year == 2014 & district == '' & election_type == 'General')
