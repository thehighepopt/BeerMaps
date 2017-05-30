

    ##https://www.pastemagazine.com/articles/2016/08/247-of-the-best-american-ipas-blind-tasted-and-ran.html
####create table from website using rvest
suppressMessages(library(rvest))
suppressMessages(library(magrittr))
suppressMessages(library(plyr))
suppressMessages(library(stringr))

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")

##Capture HTML From BA Top 250 website and Parse it
topNewWeb <- read_html("https://www.beeradvocate.com/lists/new/")

t <- topNewWeb %>%
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
#     html_nodes("span") %>% ##gives rank, beer, hads
#     html_text()
# h <- h[2:501]
# RBH <- as.data.frame(matrix(h,nrow=250,ncol=2,byrow = TRUE))
# colnames(RBH) <- c("Rank", "Ratings")
# Merge1 <- merge(BBS, RBH, by.x = "Rank", by.y = "Rank")
# Merge1[,5] <- gsub("\\W","",Merge1[,5])

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

abv[,1] <- as.character(abv[,1])
pars <- function(x) word(x,-2,-2)
abv[,2] <- sapply(abv[,1],pars)
for (i in 1:nrow(abv)) {
    if (nchar(abv[i,2]) > 6) {
        abv[i,2] <- NA
    }
}
abv$Rank <- seq(1,250, by =1)
colnames(abv) <- c("Mush", "ABV", "Rank")

Merge2 <- merge(Merge1,abv, by = "Rank")
New250 <- Merge2[c(1,3,2,4,8,5,6)]
rm(Merge1,o,Merge2,Merge3,h,t,u,BBS,BRR,abv,pars,q)


###Make Rank, Rating, Reviews and Hads numeric
# for(i in c(1,7:ncol(New250))) {New250[,i] <- as.numeric(New250[,i])}
New250[,6] <- as.numeric(as.character(New250[,6]))
###This will fix non-American letters back to the original
New250[,2] <- as.character(New250[,2])
New250[,3] <- as.character(New250[,3])

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
New250[,2] <- trim(New250[,2])
# New250[,2] <- gsub("3", "Three",New250[,2])
New250[,2] <- gsub("Co\\.", "Company",New250[,2])
New250 <- New250[order(New250[,1]),]
Encoding(New250[,2]) <- "UTF-8"
Encoding(New250[,3]) <- "UTF-8"
# New250 <- New250[,c(1,3,2,4:8)]

addr <- read.csv("brewery_addr.csv")

New250 <- merge(New250,addr,by = "Brewery", all.x = TRUE)
New250 <- New250[,c(2,1,3:ncol(New250))]
New250 <- New250[order(New250[,1]),]
rownames(New250) <- 1:nrow(New250)
rm(trim,topNewWeb,i)

##Save the data frame as a .csv
setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps/topNew")
currentDate <- Sys.Date()
csvFileName <- paste("Top New Beers_Addr_",currentDate,".csv",sep="")
write.csv(New250, file=csvFileName, row.names = FALSE)

##################Test for a new Brewery
nas <- nrow(subset(New250, is.na(New250[,13])))
if (nas > 0) {
    print("You're Missing a Brewery")
}

##################Compile a full list of top new beers

n <- as.data.frame(substr(list.files(),20,35))
setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps/topNew")
lastrun <- paste("./Top New Beers_Addr_",(n[nrow(n)-1,1]),sep="")
new1 <- read.csv(lastrun)
new3 <- data.frame(new1[order(new1[,3]),3],New250[order(New250[,3]),3])

if (identical(new3[,1],new3[,2]) == FALSE) {
    ##Compile all lists in folder to one with unique beers left
    setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")
    allnews <- read.csv("All Top New Beers.csv")
    allnews <- rbind(allnews[,1:2], New250[,2:3])
    allnews <- unique(allnews)
    allnews <- merge(allnews,addr,by = "Brewery", all.x = TRUE)
    allnews <- allnews[order(allnews[,1]),]

    write.csv(allnews,"All Top New Beers.csv", row.names = FALSE)
}
rm(n,allnews,lastrun,addr,new3,new1,nas,csvFileName)




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

New250 <- read.csv("All Top New Beers.csv") ##
BeerMapNew <- leaflet() %>%
    addTiles() %>%
    setView(-97.258997, 37.651974, zoom = 2) %>%
    addCircleMarkers(data = New250, lng = ~ Longitude, lat = ~ Latitude, popup = New250$Brewery,
                     radius = 5, color = 'blue', clusterOptions = markerClusterOptions())
BeerMapNew


currentDate <- Sys.Date()
htmlFileName <- paste("Beer_Map_New250",currentDate,".html",sep="")
saveWidget(widget = BeerMapNew, file=htmlFileName, selfcontained = FALSE)

New250byState <- arrange(as.data.frame(table(New250$State)),desc(Freq))
New250byBrewery <- arrange(as.data.frame(table(New250$Brewery)),desc(Freq))

top25New <- subset(New250, Rank < 26)
top25NewbyBrewery <- arrange(as.data.frame(table(New250$Brewery)),desc(Freq))

##http://beermapsandstats.weebly.com/
##Beer Mapping Project API key:
##b110d4b01e66ce5aaf3452736aaa1f88




