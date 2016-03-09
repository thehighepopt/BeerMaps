####Beer Mapping Project API key:
##b110d4b01e66ce5aaf3452736aaa1f88
####BreweryDB API Key:
##a8f42586b1a7e5c1492493fd4fad37d2


#Load packages, set working directory
#Download one example
#Extract the necessary information
#Store the information
#Repeat.

###Beer Mapping services:
#locscore
#locimage
#loccity
#locmap - lat and long
#locstate
#locquery - id,name, status, address, city,state, zip, country, phone, review overall, 
library(httr)
library(XML)

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")

##send API request
###URL generating function
getUrladdr <- function(brewery,sensor = "false") {
      root <- "http://beermapping.com/webservice/locquery/"
      bmapikey <- "b110d4b01e66ce5aaf3452736aaa1f88"
      u <- paste0(root,bmapikey,"/",brewery)
      return(URLencode(u))
}

Breweries <- substr(unique(Top250[1,3]),1,20) 

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
##set up to take the lowest ID if there are multiple locations
##Then you want to use ids in Brewloc to get lat and long from locmap
rm(g,e,t)
Brewaddr[,1] <- as.numeric(as.character(Brewaddr[,1]))
Brewaddr[,10] <- as.numeric(as.character(Brewaddr[,10]))

Brewaddr <- Brewaddr[!(Brewaddr[,10]==0),]
Brewaddr <- Brewaddr[!(Brewaddr[,1]==0),]
Brewaddr <- Brewaddr[complete.cases(Brewaddr),]

getUrlmap <- function(id,sensor = "false") {
      root <- "http://beermapping.com/webservice/locmap/"
      bmapikey <- "b110d4b01e66ce5aaf3452736aaa1f88"
      u <- paste0(root,bmapikey,"/",id)
      return(URLencode(u))
}

Brewids <- Brewaddr[,1]

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

Brewmap[,2] <- as.numeric(as.character(Brewmap[,2]))

Brewmap <- Brewmap[!(Brewmap[,2]==0),]

##creates URL for map point static map
base="http://maps.googleapis.com/maps/api/staticmap?center="
latitude=55.75
longitude=37.62
zoom=13
maptype="hybrid"
suffix ="&size=800x800&sensor=false&format=png"

target <- paste0(base,latitude,",",longitude,
                 "&zoom=",zoom,"&maptype=",maptype,suffix)


#Map Making
query="cambridge university"
target=paste0("http://geocode-maps.yandex.ru/1.x/?format=json&lang=en-BR&geocode=",query)
rd <- readLines(target, warn="F") 
dat <- fromJSON(rd)

#Exctract address and location data for Map Making
address <- dat$response$GeoObjectCollection$featureMember[[1]]$
      GeoObject$metaDataProperty$GeocoderMetaData$AddressDetails$Country$AddressLine
pos <- dat$response$GeoObjectCollection$featureMember[[1]]$
      GeoObject$Point
require(stringr)
temp <- unlist(str_split(pos," "))
latitude=as.numeric(temp)[1]
longitude=as.numeric(temp)[2]


#Linkedin
url="http://www.theguardian.com/uk-news/2014/mar/10/rise-zero-hours-contracts"
target=paste0("http://www.linkedin.com/countserv/count/share?url=$",url,"&format=json")
rd <- readLines(target, warn="F") 
dat <- fromJSON(rd)

#StumbleUpon
url="http://www.theguardian.com/uk-news/2014/mar/10/rise-zero-hours-contracts"
target=paste0("http://www.stumbleupon.com/services/1.01/badge.getinfo?url=",url)
rd <- readLines(target, warn="F") 
