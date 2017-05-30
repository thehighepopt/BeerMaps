##https://www.pastemagazine.com/articles/2016/08/247-of-the-best-american-ipas-blind-tasted-and-ran.html
####create table from website using rvest
suppressMessages(library(rvest))
suppressMessages(library(magrittr))
suppressMessages(library(plyr))
suppressMessages(library(stringr))

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")

##Capture HTML From BA Top 250 website and Parse it
top250web <- read_html("http://www.beeradvocate.com/lists/top/")

t <- top250web %>%
      html_nodes("table") %>%
      .[[1]]

##Parse Beer, Brewery, and Style, and add Rank
o <- t %>%
      html_nodes("a") %>% ##gives beer, brewery, style
      html_text()
BBS <- as.data.frame(matrix(o,nrow=250,ncol=3,byrow = TRUE))
BBS$Rank <- seq(1,250, by =1)
colnames(BBS) <- c("Beer", "Brewery", "Style", "Rank")

##Parse Rank, Beer, Hads remove bang from Hads, Merge with BBS
# h <- t %>%
#       html_nodes("div") %>%
#       html_text()
# #h <- h[2:501]
# ABV <- as.data.frame(matrix(h,nrow=250,ncol=1,byrow = TRUE))
# ABV[,1] <- as.character(ABV[,1])
# j <- word(ABV[,1],-2)
# ABV <- data.frame(c(1:250),j, stringsAsFactors = FALSE)
#
# colnames(ABV) <- c("Rank","ABV")
# Merge1 <- merge(BBS, ABV, by.x = "Rank", by.y = "Rank")


##Parse Beer, Rating, Reviews, merge with Merge1
u <- t %>%
      html_nodes("b") %>% ##gives beer, rating, reviews
      html_text()
BRR <- as.data.frame(matrix(u,nrow=250,ncol=3,byrow = TRUE))
colnames(BRR) <- c("Beer", "Rating", "Reviews")
Merge1 <- merge(BBS,BRR, by.x = "Beer", by.y = "Beer")

q <- t %>%
    html_nodes("div") %>%
    html_text()
abv <- as.data.frame(matrix(q,nrow=250,ncol=1,byrow = TRUE))

#close.connection(top250web)

abv[,1] <- as.character(abv[,1])
pars <- function(x) word(x,-2,-2)
abv[,2] <- sapply(abv[,1],pars)
abv$Rank <- seq(1,250, by =1)
colnames(abv) <- c("Mush", "ABV", "Rank")
for (i in 1:nrow(abv)) {
    if (nchar(abv[i,2]) > 6) {
        abv[i,2] <- NA
    }
}
Merge2 <- merge(Merge1,abv[,2:3], by = "Rank")
Merge2$Ratings <- NA
Top250 <- Merge2[c(1,3,2,4,7,5,8,6)]
rm(Merge1,o,Merge2,t,u,BBS,BRR,abv,pars,q,i,top250web)


###Make Rank, Rating, Reviews and Hads numeric
# for(i in c(1,6:ncol(Top250))) {Top250[,i] <- as.numeric(Top250[,i])}
Top250[,6] <- as.numeric(as.character(Top250[,6]))
###This will fix non-American letters back to the original
 Top250[,2] <- as.character(Top250[,2])
 Top250[,3] <- as.character(Top250[,3])

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
Top250[,3] <- trim(Top250[,3])
# Top250[,3] <- gsub("3", "Three",Top250[,3])
Top250[,2] <- gsub("Co\\.", "Company",Top250[,2])
Top250 <- Top250[order(Top250[,1]),]
Encoding(Top250[,2]) <- "UTF-8"
Encoding(Top250[,3]) <- "UTF-8"
# Top250 <- Top250[,c(1,3,2,4:8)]

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps/top250")
currentDate <- Sys.Date()
csvFileName <- paste("Top 250 Beers_",currentDate,".csv",sep="")
write.csv(Top250, file=csvFileName, row.names = FALSE)

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")

addr <- read.csv("brewery_addr.csv")

Top250 <- merge(Top250,addr,by = "Brewery", all.x = TRUE)
Top250 <- Top250[,c(2,1,3:15)]
Top250 <- Top250[order(Top250[,1]),]
rownames(Top250) <- 1:nrow(Top250)

##Save the data frame as a .csv
currentDate <- Sys.Date()
csvFileName <- paste("Top 250 Beers_Addr_",currentDate,".csv",sep="")
write.csv(Top250, file=csvFileName, row.names = FALSE)
rm(csvFileName,currentDate)

##################Test for a new Brewery

nas <- nrow(subset(Top250, is.na(Top250[,13])))
if (nas > 0) {
    print("You're Missing a Brewery")
    }

###################Test for changes, if true, compile all beers ever
setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps/top250")

n <- as.data.frame(substr(list.files(),15,30))

lastrun <- paste("./Top 250 Beers_",n[nrow(n)-1,1],sep="")
top1 <- read.csv(lastrun)
top3 <- data.frame(top1[order(top1[,3]),3],Top250[order(Top250[,3]),3])

if (identical(top3[,1],top3[,2]) == FALSE) {
    ##Compile all lists in folder to one with unique beers left
    setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")
    alltops <- read.csv("All Top 250 Beers.csv")
    alltops <- rbind(alltops[,2:ncol(alltops)], Top250[,c(2,3,9:ncol(Top250))])
    alltops <- unique(alltops)
    rownames(alltops) <- 1:nrow(alltops)
    alltops <- alltops[order(alltops[,1]),]

    write.csv(alltops,"All Top 250 Beers.csv")
}
rm(n,lastrun,top3,top1)

########
##When it finally works, save to G Drive
#top250put <- gs_new("All Top 250 Beers", input = Top250, trim = TRUE)
#library("googlesheets")
#suppressPackageStartupMessages(library("dplyr"))



# auth <- getGoogleAuth("thehighepopt@gmail.com", "Ferm1lab23f", service = "wise")
#
# if(exists("ps")){
#     print("got password, keep going")
# } else {
#     ps <-readline(prompt="get the password in ")
# }
#
# options(RCurlOptions = list(
#     capath = system.file("CurlSSL", "cacert.pem",
#                          package = "RCurl"), ssl.verifypeer = FALSE)
# )
#
# sheets.con = getGoogleDocsConnection(
#     getGoogleAuth("thehighepoptl@gmail.com", ps, service ="wise"))





######################################################################################
#####Make a map, chug some numbers
suppressMessages(library(maps,plyr))
suppressMessages(library(htmlwidgets))
suppressMessages(library(leaflet))
suppressMessages(library(magrittr))

top250 <- read.csv("Top 250 Beers.csv")
BeerMap <- leaflet() %>%
      addTiles() %>%
      setView(-97.258997, 37.651974, zoom = 2) %>%
      addCircleMarkers(data = Top250, lng = ~ Longitude, lat = ~ Latitude, popup = Top250$Brewery,radius = 5, color = 'blue', clusterOptions = markerClusterOptions())
BeerMap


currentDate <- Sys.Date()
htmlFileName <- paste("Beer_Map_",currentDate,".html",sep="")
saveWidget(widget = BeerMap, file=htmlFileName, selfcontained = FALSE)

top250byState <- arrange(as.data.frame(table(top250$State)),desc(Freq))
top250byBrewery <- arrange(as.data.frame(table(top250$Brewery)),desc(Freq))

top25 <- subset(top250, Rank < 26)
top25byBrewery <- arrange(as.data.frame(table(top25$Brewery)),desc(Freq))

##http://beermapsandstats.weebly.com/
##Beer Mapping Project API key:
##b110d4b01e66ce5aaf3452736aaa1f88



