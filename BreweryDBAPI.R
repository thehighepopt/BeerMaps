####Beer Mapping Project API key:
##b110d4b01e66ce5aaf3452736aaa1f88
####BreweryDB API Key:
##a8f42586b1a7e5c1492493fd4fad37d2
##http://www.brewerydb.com/


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
getUrl <- function(request) {
      root <- "http://api.brewerydb.com/v2/breweries?name="
      bdbapi <- "&key=a8f42586b1a7e5c1492493fd4fad37d2"
      u <- paste0(root,request,bdbapi)
      return(URLencode(u))
}
##https://api.brewerydb.com/v2/beers?name=60%20minute%20IPA&key=[mykey]
##https://api.brewerydb.com/v2/adjuncts?key=[yourkey]
"http://api.brewerydb.com/v2/breweries?name=Austin%20Beerworks&key=a8f42586b1a7e5c1492493fd4fad37d2"
e <- getUrl("Three Floyds Brewing Company")
e <- getUrl("locations?postalCode=78736")

Breweries <- unique(Top250[4:10,3])

###loop it
Brewid <- as.data.frame(matrix(,nrow = 0, ncol = 2))

for (x in Breweries) {
e <- getUrl(x)
t <- content(GET(e, accept_xml()))
x <- data.frame(BDBid = xpathSApply(t, "//item/id", xmlValue),
                name = xpathSApply(t, "//item//name", xmlValue))
Brewid <- rbind(Brewid,x)

}

getUrlbeer <- function(request) {
    root <- "http://api.brewerydb.com/v2/beers?name="
    bdbapi <- "&key=a8f42586b1a7e5c1492493fd4fad37d2"
    u <- paste0(root,request,bdbapi)
    return(URLencode(u))
}
##https://api.brewerydb.com/v2/beers?name=60%20minute%20IPA&key=[mykey]
##https://api.brewerydb.com/v2/adjuncts?key=[yourkey]
"http://api.brewerydb.com/v2/breweries?name=Austin%20Beerworks&key=a8f42586b1a7e5c1492493fd4fad37d2"
e <- getUrlbeer("Mosaic IPA")
Beers <- Top250$Beer

Beerid <- as.data.frame(matrix(,nrow = 0, ncol = 7))

for (i in Beers) {
    e <- getUrlbeer(i)
    t <- content(GET(e, accept_xml()))
    x <- data.frame(BDBeerid = xpathSApply(t, "//item/id", xmlValue),
                    name = xpathSApply(t, "//item/name", xmlValue),
                    SRM = xpathSApply(t, "//item/srmId", xmlValue),
                    icon = xpathSApply(t, "//labels/icon", xmlValue),
                    medLabel = xpathSApply(t, "//labels/medium", xmlValue),
                    lrgLabel = xpathSApply(t, "//labels/large", xmlValue),
                    style = xpathSApply(t, "//style/name", xmlValue)
                    )
    Beerid <- rbind(Beerid,x)
    
}





Brewloc <- as.data.frame(matrix(,nrow = 0, ncol = 10))
x <- data.frame(BDBid = xpathSApply(t, "//item/id", xmlValue),
                breweryId = xpathSApply(t, "//brewery/id", xmlValue),
                name = xpathSApply(t, "//brewery//name", xmlValue),
                type = xpathSApply(t, "//item//locationTypeDisplay", xmlValue),
                street = xpathSApply(t, "//item//streetAddress", xmlValue),
                city = xpathSApply(t, "//item//locality", xmlValue),
                state = xpathSApply(t, "//item//region", xmlValue),
                zip = xpathSApply(t, "//item//postalCode", xmlValue),
                country = xpathSApply(t, "//country//name", xmlValue),
                phone = xpathSApply(t, "//item//phone", xmlValue),
                rating= xpathSApply(t, "//item//overall", xmlValue),
                lng = xpathSApply(t, "//item//longitude", xmlValue),
                lat = xpathSApply(t, "//item//latitude", xmlValue),
                phone = xpathSApply(t, "//item//phone", xmlValue),
                site = xpathSApply(t, "//brewery//website", xmlValue),
                openSince = xpathSApply(t, "//brewery//established", xmlValue),
                openToPublic = xpathSApply(t, "//item//openToPublic", xmlValue),
                clob = xpathSApply(t, "//brewery//description", xmlValue),
                status = xpathSApply(t, "//brewery//status", xmlValue),
                isPrimary = xpathSApply(t, "//item//isPrimary", xmlValue),
                isClosed = xpathSApply(t, "//item//isClosed", xmlValue)
)

Brewloc <- rbind(Brewloc,x)
rm(x)
}
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
