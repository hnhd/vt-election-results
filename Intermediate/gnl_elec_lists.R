getwd()

setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2016/General/Precinct/')
gnl_2016 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2014/General/Precinct/')
gnl_2014 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2012/General/Precinct/')
gnl_2012 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2010/General/Precinct/')
gnl_2010 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2008/General/Precinct/')
gnl_2008 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2006/General/Precinct/')
gnl_2006 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2004/General/Precinct/')
gnl_2004 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2002/General/Precinct/')
gnl_2002 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2000/General/Precinct/')
gnl_2000 <- list.files()

setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2016/Primary/Precinct/')
pry_2016 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2014/Primary/Precinct/')
pry_2014 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2012/Primary/Precinct/')
pry_2012 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2010/Primary/Precinct/')
pry_2010 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2008/Primary/Precinct/')
pry_2008 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2006/Primary/Precinct/')
pry_2006 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2004/Primary/Precinct/')
pry_2004 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2002/Primary/Precinct/')
pry_2002 <- list.files()
setwd('~/Desktop/Tech Resources/GitHub Projects/Vermont-Election-Results/Election Results/2000/Primary/Precinct/')
pry_2000 <- list.files()

length(gnl_2016) <- 130
length(gnl_2014) <- 130
length(gnl_2012) <- 130
length(gnl_2010) <- 130
length(gnl_2008) <- 130
length(gnl_2006) <- 130
length(gnl_2004) <- 130
length(gnl_2002) <- 130
length(gnl_2000) <- 130

require(plyr)

gnl_names <- data.frame(gnl_2000, gnl_2002, gnl_2004, gnl_2006, gnl_2008, gnl_2010, gnl_2012, gnl_2014, gnl_2016)

setwd('~/Desktop/')
write.csv(gnl_names, 'gnl_names.csv')
