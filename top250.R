library(maps,plyr)
library(htmlwidgets)
library(leaflet)
library(magrittr)

####create table from website using rvest
library(rvest)
top250web <- read_html("http://www.beeradvocate.com/lists/top/")

top250web %>% 
      html_node("span") %>%
      html_text() %>%
      as.numeric()

table250 <- top250web %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_table()

table250 <- table250[-c(1,2),]
rownames(table250) <- seq(length=nrow(table250))

###split Reviews and Hads and make them numeric
table250 <- cbind(table250, with(table250, data.frame(do.call('rbind', strsplit(X4, '|', fixed=TRUE)))))[,-4]
for(i in c(1,3:ncol(table250))) {table250[,i] <- as.numeric(table250[,i])}

###Split apart Beer, Brewery, Style, and ABV
library(stringr, qdap)
ABVSplit <- data.frame(String=character(),ABV=character())

for (i in table250[,2]) {
      if (str_count(i,"/") == 2){
            String <- word(i, 1, 2, " / ")
            ABV <- word(i, 3, -1, " / ")
      } else if ((str_count(i,"/") == 1)&(str_detect(i,'/ [:digit:]') == TRUE)){
            String <- word(i, 1, 1, " / ")
            ABV <- word(i, 2, -1, " / ")
      } else {
            String <- i
            ABV <- NA
      }
      df2 <- data.frame(String,ABV)
      ABVSplit <- rbind(ABVSplit,df2)
} 
 
rm(df2,ABV,String,i)

table250 <- cbind(table250,ABVSplit)[,-2]

###This is a bunch of clean up that needs to be done better, qdap doesn't really work
table250[,5] <- gsub('\\.','',table250[,5])
table250[,5] <- gsub("3 Floyds","Three Floyds",table250[,5])
table250[,5] <- gsub("Co\\.","Company",table250[,5])
table250[,5] <- gsub("IPA","IPa",table250[,5])
table250[,5] <- gsub(")","z",table250[,5])
table250[,5] <- gsub("Inc.","Inc",table250[,5])
table250[,5] <- gsub("#4","#Four",table250[,5])
table250[,5] <- gsub("#5","#Five",table250[,5])
table250[,5] <- gsub("#6","#Six",table250[,5])
table250[,5] <- gsub("!","!z",table250[,5])
table250[,5] <- gsub("Trappistes Rochefort 10Brasserie","Trappistes Rochefort TenBrasserie",table250[,5])
table250[,5] <- gsub("St. Bernardus Abt 12","St. Bernardus Abt Twelve",table250[,5])
table250[,5] <- gsub(" NV"," Nv",table250[,5])
table250[,5] <- gsub("\\&\\+Peg's","&+zPeg's",table250[,5])
table250[,5] <- gsub("Enlightenment\\?Hill","EnlightenmentHill",table250[,5])
table250[,5] <- gsub("2014New","2014zNew",table250[,5])
table250[,5] <- gsub("FIDYOskar","FidyOskar",table250[,5])
table250[,5] <- gsub("3The","ThreeThe",table250[,5])
table250[,5] <- gsub("4 Hands","Four Hands",table250[,5])
table250[,5] <- gsub("1Peg's","OnePeg's",table250[,5])
table250[,5] <- gsub("BDCSOzark","BDCsOzark",table250[,5])
table250[,5] <- gsub("10","Ten",table250[,5])
table250[,5] <- gsub("#15","#15z",table250[,5])
table250[,5] <- gsub("PseudoSue","Pseudosue",table250[,5])
table250[,5] <- gsub("AleSmith","Alesmith",table250[,5])
table250[,5] <- gsub("Blåbær","Blabaer",table250[,5])
table250[,5] <- gsub("FiftyFifty","Fifty50",table250[,5])
table250[,5] <- gsub("é","e",table250[,5])
table250[,5] <- gsub("è","e",table250[,5])
table250[,5] <- gsub("JuJu","Juju",table250[,5])
table250[,5] <- gsub("HefeWeizen","Hefeweizen",table250[,5])
table250[,5] <- gsub("ScareCity","Scarecity",table250[,5])
table250[,5] <- gsub("ö","o",table250[,5])
table250[,5] <- gsub("ä","a",table250[,5])


BeerSplit <- data.frame(Beer = character(),Brewery=character(),Style=character())

for (i in table250[,5]) {
      o <- unlist(str_locate_all(pattern = '[:lower:][:upper:]',i))
      Beer <- str_sub(i,end = o[[1]])
      Brewery <- str_sub(i,o[[3]],o[[2]])
      Style <- str_sub(i,o[[4]],str_length(i))
      df1 <- data.frame(Beer,Brewery,Style)
      BeerSplit <- rbind(BeerSplit,df1)
      rm(i,o)
}

Top250clean <- cbind(table250,BeerSplit)[,-5]

rm(BeerSplit,ABVSplit,df1,table250,Beer,Brewery,i,o)

currentDate <- Sys.Date() 
csvFileName <- paste("Top 250 Beers_",currentDate,".csv",sep="") 
write.csv(Top250clean, file=csvFileName, row.names = FALSE) 

#####Make a map, chug some numbers
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

