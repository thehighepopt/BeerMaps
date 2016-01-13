library(rvest)
library(magrittr)
library(plyr)
library(stringr)

twentyplaces <- read_html("http://coolmaterial.com/feature/bucket-list-breweries-20-u-s-breweries-you-should-visit-once/")
      
r <- twentyplaces %>%
      html_nodes("h2") %>%
      as.data.frame(html_text())

Encoding(r) <- "UTF-8"

getUrladdr <- function(brewery,sensor = "false") {
      root <- "http://beermapping.com/webservice/locquery/"
      bmapikey <- "b110d4b01e66ce5aaf3452736aaa1f88"
      u <- paste0(root,bmapikey,"/",brewery)
      return(URLencode(u))
}

Breweries <- substr(r,1,10) 
r <- gsub("3", "Three",r)

###loop it to create a df with brewery and addresses
Brewaddr <- as.data.frame(matrix(,nrow = 0, ncol = 10))


for (x in Breweries) {
      e <- getUrladdr(x)
      t <- content(GET(e, accept_xml()))
      g <- data.frame(BMid = xpathSApply(t, "//location//id", xmlValue),
                      name = xpathSApply(t, "//location//name", xmlValue),
                      status = xpathSApply(t, "//location//status", xmlValue),
                      street = xpathSApply(t, "//location//street", xmlValue),
                      city = xpathSApply(t, "//location//city", xmlValue),
                      state = xpathSApply(t, "//location//state", xmlValue),
                      zip = xpathSApply(t, "//location//zip", xmlValue),
                      country = xpathSApply(t, "//location//country", xmlValue),
                      phone = xpathSApply(t, "//location//phone", xmlValue),
                      rating= xpathSApply(t, "//location//overall", xmlValue)
      )
      
      Brewaddr <- rbind(Brewaddr,g)
}
   
      rm(g,e,t)
      
Brewaddr[,1] <- as.numeric(as.character(Brewaddr[,1]))
Brewaddr <- Brewaddr[!(Brewaddr[,1]==0),]
Brewaddr <- Brewaddr[!(Brewaddr[,3]=="Beer Bar"),]
rownames(Brewaddr) <- 1:50
Brewaddr <- Brewaddr[c(1,4,14,15,17,21,25,26,29,31,33,35,36
,40,41,43,46,49,50),]
rownames(Brewaddr) <- 1:19
d <- c("NA","Monhegan Brewing Company","Brewpub","1 Boody Lane","Monhegan","ME","04852","United States","(207) 975-3958","0")
Brewaddr <- as.data.frame(insertRow(as.matrix(Brewaddr),20,d,rName = "20"))

getUrlmap <- function(id,sensor = "false") {
      root <- "http://beermapping.com/webservice/locmap/"
      bmapikey <- "b110d4b01e66ce5aaf3452736aaa1f88"
      u <- paste0(root,bmapikey,"/",id)
      return(URLencode(u))
}

Brewids <- Brewaddr[,1] 
Brewids <- 55

###loop it to create a df with brewery and addresses
Brewmap <- as.data.frame(matrix(,nrow = 0, ncol = 3))


for (x in Brewids) {
      e <- getUrlmap(x)
      t <- content(GET(e, accept_xml()))
      g <- data.frame(name = xpathSApply(t, "//location//name", xmlValue),
                      lat = xpathSApply(t, "//location//lat", xmlValue),
                      lng = xpathSApply(t, "//location//lng", xmlValue)
                      )
      
      Brewmap <- rbind(Brewmap,g)
}

rm(g,e,t)

Brewfull <- merge(Brewaddr,Brewmap, all.x = TRUE)
Brewfull[,11] <- as.numeric(as.character(Brewfull[,11]))
Brewfull[,12] <- as.numeric(as.character(Brewfull[,12]))
g[15,12] <- "39.7225465"
g[15,13] <- "-121.8218357"

write.csv(g,"20 Bucket List Breweries.csv", row.names = FALSE)

g <- read.csv("20 Bucket List Breweries.csv",colClasses = 'character')

#####################################

library(maps,plyr)
library(htmlwidgets)
library(leaflet)
library(magrittr)

TwentyBreweriesMap <- leaflet() %>% 
      addTiles() %>% 
      setView(-97.258997, 37.651974, zoom = 2) %>% 
      addCircleMarkers(data = g, lng = ~ lng, lat = ~ lat, popup = g$name,radius = 5, color = 'blue', clusterOptions = markerClusterOptions())
TwentyBreweriesMap

saveWidget(widget = TwentyBreweriesMap, file="20_BList_Breweries.html", selfcontained = FALSE)

Brewery_lkup <- g[,c(1,4,5,6,7,11,12)]
##adds leading 0 to NE zip code
##Brewery_lkup[19,5] <- str_pad(Brewery_lkup[19,5], 5, side = "left", pad = "0")
write.csv(Brewery_lkup,"Brewery_lkup.csv", row.names = FALSE)




