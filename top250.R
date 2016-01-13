
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
h <- h[2:751]
RBH <- as.data.frame(matrix(h,nrow=250,ncol=3,byrow = TRUE))
colnames(RBH) <- c("Rank", "Mush", "Hads")
Merge1 <- merge(BBS, RBH, by.x = "Rank", by.y = "Rank")
Merge1[,6] <- gsub("\\W","",Merge1[,6])

##Parse Beer, Rating, Reviews, merge with Merge1
u <- t %>%
      html_nodes("b") %>% ##gives beer, rating, reviews
      html_text()
BRR <- as.data.frame(matrix(u,nrow=250,ncol=3,byrow = TRUE))
colnames(BRR) <- c("Beer", "Rating", "Reviews")
Merge2 <- merge(Merge1,BRR, by.x = "Beer", by.y = "Beer")

###Split apart ABV
ABVSplit <- data.frame(String=character(),ABV=character())

for (i in Merge2[,5]) {
      if (str_count(i,"/") == 2){
            String <- i ##word(i, 1, 2, " / ")
            ABV <- word(i, 3, -1, " / ")
      } else if ((str_count(i,"/") == 1)&(str_detect(i,'/ [:digit:]') == TRUE)){
            String <- i ##word(i, 1, 1, " / ")
            ABV <- word(i, 2, -1, " / ")
      } else {
            String <- i
            ABV <- NA
      }
      df2 <- data.frame(String,ABV)
      ABVSplit <- rbind(ABVSplit,df2)
} 


Merge3 <- merge(Merge2,ABVSplit, by.x = "Mush", by.y = "String")
Top250 <- Merge3[c(3,2,4,5,9,7,8,6)]
Top250 <- Top250[order(Top250[,1]),]
rm(df2,String,i,Merge1,o,Merge2,Merge3,h,t,ABV,u,BBS,BRR,RBH,ABVSplit)


###Make Rank, Rating, Reviews and Hads numeric
for(i in c(1,7:ncol(Top250))) {Top250[,i] <- as.numeric(Top250[,i])}
Top250[,6] <- as.numeric(as.character(Top250[,6]))
###This will fix non-American letters back to the original
Top250[,2] <- as.character(Top250[,2])
Top250[,3] <- as.character(Top250[,3])
Encoding(Top250[,2]) <- "UTF-8"
Encoding(Top250[,3]) <- "UTF-8"

rownames(Top250) <- 1:250

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
Top250[,3] <- trim(Top250[,3])
Top250[,3] <- gsub("3", "Three",Top250[,3])

##Save the data frame as a .csv
currentDate <- Sys.Date() 
csvFileName <- paste("Top 250 Beers_",currentDate,".csv",sep="") 
write.csv(Top250, file=csvFileName, row.names = FALSE) 
rm(i,csvFileName,currentDate)

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



