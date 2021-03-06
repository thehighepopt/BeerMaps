##https://www.pastemagazine.com/articles/2016/08/247-of-the-best-american-ipas-blind-tasted-and-ran.html
####create table from website using rvest
library(rvest)
library(magrittr)
library(plyr)
library(stringr)

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
h <- t %>%
      html_nodes("span") %>% ##gives rank, beer, hads
      html_text()
h <- h[2:501]
RBH <- as.data.frame(matrix(h,nrow=250,ncol=2,byrow = TRUE))
colnames(RBH) <- c("Rank", "Ratings")
Merge1 <- merge(BBS, RBH, by.x = "Rank", by.y = "Rank")
Merge1[,5] <- gsub("\\W","",Merge1[,5])

##Parse Beer, Rating, Reviews, merge with Merge1
u <- t %>%
      html_nodes("b") %>% ##gives beer, rating, reviews
      html_text()
BRR <- as.data.frame(matrix(u,nrow=250,ncol=3,byrow = TRUE))
colnames(BRR) <- c("Beer", "Rating", "Reviews")
Merge2 <- merge(Merge1,BRR, by.x = "Beer", by.y = "Beer")

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

###Split apart ABV
# ABVSplit <- data.frame(String=character(),ABV=character())
#
# for (i in abv) {
#       if (str_count(i,"/") == 2){
#             String <- i ##word(i, 1, 2, " / ")
#             ABV <- word(i, 3, -1, " / ")
#       } else if ((str_count(i,"/") == 1)&(str_detect(i,'/ [:digit:]') == TRUE)){
#             String <- i ##word(i, 1, 1, " / ")
#             ABV <- word(i, 2, -1, " / ")
#       } else {
#             String <- i
#             ABV <- NA
#       }
#       df2 <- data.frame(String,ABV)
#       ABVSplit <- rbind(ABVSplit,df2)
# }


Merge3 <- merge(Merge2,abv, by = "Rank")
Top250 <- Merge3[c(1,2,3,4,9,6,5,7)]
rm(Merge1,o,Merge2,Merge3,h,t,u,BBS,BRR,RBH,abv,pars,q)


###Make Rank, Rating, Reviews and Hads numeric
for(i in c(1,7:ncol(Top250))) {Top250[,i] <- as.numeric(Top250[,i])}
Top250[,6] <- as.numeric(as.character(Top250[,6]))
###This will fix non-American letters back to the original
# Top250[,2] <- as.character(Top250[,2])
# Top250[,3] <- as.character(Top250[,3])

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
Top250[,3] <- trim(Top250[,3])
Top250[,3] <- gsub("3", "Three",Top250[,3])
Top250[,3] <- gsub("Co\\.", "Company",Top250[,3])
Top250 <- Top250[order(Top250[,1]),]
Encoding(Top250[,2]) <- "UTF-8"
Encoding(Top250[,3]) <- "UTF-8"
Top250 <- Top250[,c(1,3,2,4:8)]

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps/top250")
currentDate <- Sys.Date()
csvFileName <- paste("Top 250 Beers_",currentDate,".csv",sep="")
write.csv(Top250, file=csvFileName, row.names = FALSE)

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")

addr <- read.csv("brewery_addr.csv")

Top250 <- merge(Top250,addr,by = "Brewery", all.x = TRUE)
Top250 <- Top250[,c(2,1,3:13)]
Top250 <- Top250[order(Top250[,1]),]
rownames(Top250) <- 1:250

##Save the data frame as a .csv
currentDate <- Sys.Date()
csvFileName <- paste("Top 250 Beers_Addr_",currentDate,".csv",sep="")
write.csv(Top250, file=csvFileName, row.names = FALSE)
rm(csvFileName,currentDate,addr,trim,top250web)

###################Test for changes
setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps/top250")
top1 <- read.csv("./Top 250 Beers_2017-02-01.csv")
top1 <- read.csv("./Top 250 Beers_2017-01-31.csv")




##Compile all lists in folder to one - some repeats will happen
setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps/top250")
history <- as.data.frame(matrix(NA,1,8))
  colnames(history) <- c("Rank","Brewery","Beer","Style","ABV","Rating","Ratings","Reviews")

  n <- as.data.frame(list.files())
  n[,2] <- substr(n[,1],15,30)
  b <- nrow(n)

  for (i in 1:b) {
    date <- n[i,2]
    csvFileName <- paste("Top 250 Beers_",date,sep="")
    a <- read.csv(csvFileName)
    history <- rbind(history,a)
  }

  alltops <- unique( history[2:nrow(history),2:3] )
  alltops <- alltops[order(alltops[,1]),]
  setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")
  write.csv(alltops,"All Top 250 Beers.csv")


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
library(maps,plyr)
library(htmlwidgets)
library(leaflet)
library(magrittr)

top250 <- read.csv("Top 250 Beers.csv")
BeerMap <- leaflet() %>%
      addTiles() %>%
      setView(-97.258997, 37.651974, zoom = 2) %>%
      addCircleMarkers(data = top250, lng = ~ Long, lat = ~ Lat, popup = top250$Brewery,radius = 5, color = 'blue', clusterOptions = markerClusterOptions())
BeerMap

saveWidget(widget = BeerMap, file="Beer_Map.html", selfcontained = FALSE)

top250byState <- arrange(as.data.frame(table(top250$State)),desc(Freq))
top250byBrewery <- arrange(as.data.frame(table(top250$Brewery)),desc(Freq))

top25 <- subset(top250, Rank < 26)
top25byBrewery <- arrange(as.data.frame(table(top25$Brewery)),desc(Freq))

##http://beermapsandstats.weebly.com/
##Beer Mapping Project API key:
##b110d4b01e66ce5aaf3452736aaa1f88



