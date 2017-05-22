library(XML)
library(stringr)
library(httr)

#tb <- readHTMLTable("http://vtelectionarchive.sec.state.vt.us/elections/view/68156/")
#df <- data.frame(tb$precinct_data)
#xpathApply(raw$children, path="//p", fun=xmlValue)
#test <- raw$file

library(rvest)

election_value <- 1:90000
title <- character(90000)

for (i in 1:90000) {
   url <- sprintf("http://vtelectionarchive.sec.state.vt.us/elections/view/%s", election_value[i])
   if (is.null(tryCatch({read_html(url) %>% html_nodes(xpath='//*[@id="content_wrapper"]/div[2]/div/h1')  %>% html_text()}, error=function(e){}))) {
     title[i] <- NA } else {
       title[i] <- read(url) %>% html_nodes(xpath='//*[@id="content_wrapper"]/div[2]/div/h1')}
}

title

url <- "http://vtelectionarchive.sec.state.vt.us/elections/view/68156"
if (is.null(tryCatch({read_html(url) %>% html_nodes(xpath='//*[@id="content_wrapper"]/div[2]/div/h1')  %>% html_text()}, error=function(e){}))) {
  title[1] <- NA } else {
  title[1] <- read_html(url) %>% html_nodes(xpath='//*[@id="content_wrapper"]/div[2]/div/h1')}

url <- "http://vtelectionarchive.sec.state.vt.us/elections/view/68156"
read_html(url) %>% 
  html_nodes(xpath='//*[@id="content_wrapper"]/div[2]/div/h1')
    
df2 <- data.frame (
  election_value,
  unlist(title)
)
  
df <- data.frame(election_value, # create empty data frame
                 "year" = rep(NA, length(election_value)),
                 "election_type" = rep(NA, length(election_value)),
                 "party" = rep(NA, length(election_value)),
                 "district" = rep(NA, length(election_value)),
                 "office" = rep(NA, length(election_value)),
                 stringsAsFactors = FALSE)

for (i in 1:2) {
  tryCatch({str_extract(string = htmlTreeParse(url), pattern ="VT.*")[3]}, error=function(e){})
  elec_unclean <- str_extract(string = htmlTreeParse(url), pattern ="VT.*")[3]
  elec_clean <- str_replace(str_extract(elec_unclean, '».*[[:alpha:]]'), "».", "")
  df$year[i] <- str_extract(elec_clean, '\\d{4}')
  df$election_type[i] <- na.omit(str_extract(elec_clean, elec_types))[1]
  if(df$election_type[i] == "Primary") {
    df$party[i] <- str_extract(str_extract(elec_clean, "\\w+ Primary"), "\\w+")
    df$district[i] <- trimws(str_replace(str_replace(str_extract(elec_clean, "Primary.*"), "Primary", ""), "District", ""))
  } else {
    df$party[i] <- NA
    df$district[i] <- trimws(str_replace(str_replace(str_extract(elec_clean, "Election.*"), "Election", ""), "District", ""))
  }
  if(grepl("Liberty Union", elec_clean)) {df$party[i] <- "Liberty Union"}
  if(grepl("President", elec_clean)) {df$office[i] <- "President"} else
    if(grepl("U.S. Senate", elec_clean)) {df$office[i] <- "U.S. Senate"} else 
      if(grepl("U.S. House", elec_clean)) {df$office[i] <- "U.S. House"} else
        if(grepl("Lieutenant Governor", elec_clean)) {df$office[i] <- "Lieutenant Governor"} else
          if(grepl("Governor", elec_clean)) {df$office[i] <- "Governor"} else
            if(grepl("Treasurer", elec_clean)) {df$office[i] <- "Treasurer"} else 
              if(grepl("Secretary of State", elec_clean)) {df$office[i] <- "Secretary of State"} else 
                if(grepl("Auditor", elec_clean)) {df$office[i] <- "Auditor"} else 
                  if(grepl("Attorney General", elec_clean)) {df$office[i] <- "Attorney General"} else 
                    if(grepl("State Senator", elec_clean)) {df$office[i] <- "State Senator"} else 
                      if(grepl("State Representative", elec_clean)) {df$office[i] <- "State Representative"} else {df$office[i] <- "Local Election"}
  if(str_extract(election_value[i], "\\d$") == "0") {print(paste("Processing:", election_value[i]))}
}




?left

str_pad(election_value[2], 5,)

for (i in 68000:68005) {
  url <- sprintf("http://vtelectionarchive.sec.state.vt.us/elections/view/%s", election_value[i])
  tryCatch({
    elec_unclean <- str_extract(string = htmlTreeParse(url), pattern ="VT.*")[3]
    elec_clean <- str_replace(str_extract(elec_unclean, '».*[[:alpha:]]'), "».", "")
    df$year[i] <- str_extract(elec_clean, '\\d{4}')
    df$election_type[i] <- na.omit(str_extract(elec_clean, elec_types))[1]
    if(df$election_type[i] == "Primary") {
      df$party[i] <- str_extract(str_extract(elec_clean, "\\w+ Primary"), "\\w+")
      df$district[i] <- trimws(str_replace(str_replace(str_extract(elec_clean, "Primary.*"), "Primary", ""), "District", ""))
    } else {
      df$party[i] <- NA
      df$district[i] <- trimws(str_replace(str_replace(str_extract(elec_clean, "Election.*"), "Election", ""), "District", ""))
    }
    if(grepl("Liberty Union", elec_clean)) {df$party[i] <- "Liberty Union"}
    if(grepl("President", elec_clean)) {df$office[i] <- "President"} else
      if(grepl("U.S. Senate", elec_clean)) {df$office[i] <- "U.S. Senate"} else 
        if(grepl("U.S. House", elec_clean)) {df$office[i] <- "U.S. House"} else
          if(grepl("Lieutenant Governor", elec_clean)) {df$office[i] <- "Lieutenant Governor"} else
            if(grepl("Governor", elec_clean)) {df$office[i] <- "Governor"} else
              if(grepl("Treasurer", elec_clean)) {df$office[i] <- "Treasurer"} else 
                if(grepl("Secretary of State", elec_clean)) {df$office[i] <- "Secretary of State"} else 
                  if(grepl("Auditor", elec_clean)) {df$office[i] <- "Auditor"} else 
                    if(grepl("Attorney General", elec_clean)) {df$office[i] <- "Attorney General"} else 
                      if(grepl("State Senator", elec_clean)) {df$office[i] <- "State Senator"} else 
                        if(grepl("State Representative", elec_clean)) {df$office[i] <- "State Representative"} else {df$office[i] <- "Local Election"}
    print(paste("PROCESSING", election_value[i]))
    }, error=function(e){print(paste0("ERROR ", i, ": ",conditionMessage(e)))})
}

for (i in 1:10) {
  tryCatch({
    str_extract(string = htmlTreeParse(sprintf("http://vtelectionarchive.sec.state.vt.us/elections/view/%s", election_value[i])), pattern ="VT.*")[3]
    }, error=function(e){print(paste0("ERROR ", i, ": ",conditionMessage(e)))})
}

str_extract(string = htmlTreeParse(url), pattern ="VT.*")[3]

url <- "http://vtelectionarchive.sec.state.vt.us/elections/view/3"

url <- "http://vtelectionarchive.sec.state.vt.us/elections/view/68156/"

htmlTreeParse(url, isURL = TRUE)
htmlParse(url, isURL = TRUE)
