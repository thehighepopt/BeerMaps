##Beer Mapping Project API key:
##b110d4b01e66ce5aaf3452736aaa1f88

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
getUrl <- function(brewery,sensor = "false") {
      root <- "http://beermapping.com/webservice/locquery/"
      bmapi <- "b110d4b01e66ce5aaf3452736aaa1f88"
      u <- paste0(root,bmapi,"/",brewery)
      return(URLencode(u))
}
###loop it
Brewloc <- as.data.frame(matrix(,nrow = 0, ncol = 10))

e <- getUrl("Deschutes")
t <- content(GET(e, accept_xml()))
x <- data.frame(BMid = xpathSApply(t, "//location//id", xmlValue),
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

Brewloc <- rbind(Brewloc,x)
rm(x)

##Then you want to use ids in Brewloc to get lat and long from locmap

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
