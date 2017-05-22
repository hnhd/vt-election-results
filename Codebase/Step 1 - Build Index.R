library(XML)
library(rvest)

election_value <- 1:90000
title <- character(90000)

for (i in 1:90000) {
  url <- sprintf("http://vtelectionarchive.sec.state.vt.us/elections/view/%s", election_value[i])
  if (is.null(tryCatch({read_html(url) %>% html_nodes(xpath='//*[@id="content_wrapper"]/div[2]/div/h1')  %>% html_text()}, error=function(e){}))) {
    title[i] <- NA } else {
      title[i] <- read_html(url) %>% html_nodes(xpath='//*[@id="content_wrapper"]/div[2]/div/h1') %>% html_text()
    }
}

vermont_titles <- data.frame(election_value, title)
write.csv(vermont_titles, '../Index/index_titles.csv')

