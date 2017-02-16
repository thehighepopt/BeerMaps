

library(rvest)
library(magrittr)
library(RCurl)
library(XML)

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")

##Capture HTML From Zymurgy Top Beers webpage and Parse it
zymurgy <- read_html("https://www.homebrewersassociation.org/news/zymurgys-2016-best-beers-america-results/")

###Beers
a <- zymurgy %>%
    html_nodes("tbody") %>%
    .[1]  %>% ##Beers
    html_nodes("td") %>%      
    html_text() 

zBeers <- as.data.frame(matrix(a,nrow=125,ncol=2,byrow = TRUE))
zBeers <- zBeers[2:52,]
colnames(zBeers) <- c("Rank","Beer")
zBeers[,2] <- as.character(zBeers[,2])
Encoding(zBeers[,2]) <- "UTF-8"
rownames(zBeers) <- 1:51

###Breweries
b <- zymurgy %>%
    html_nodes("tbody") %>%
    .[2]  %>% ##Breweries
    html_nodes("td") %>%      
    html_text() 

zBreweries <- as.data.frame(matrix(b,nrow=26,ncol=3,byrow = TRUE))
colnames(zBreweries) <- c("Rank","Brewery","Location")
zBreweries <- zBreweries[2:26,]
zBreweries[,2] <- as.character(zBreweries[,2])
Encoding(zBreweries[,2]) <- "UTF-8"
rownames(zBreweries) <- 1:25

###Best Portfolio
c <- zymurgy %>%
    html_nodes("tbody") %>%
    .[3]  %>% ##Best Portfolio
    html_nodes("td") %>%      
    html_text() 

zPort <- as.data.frame(matrix(c,nrow=11,ncol=3,byrow = TRUE))
colnames(zPort) <- c("Rank","Brewery","Portfolio")
zPort <- zPort[2:11,]
zPort[,2] <- as.character(zPort[,2])
Encoding(zPort[,2]) <- "UTF-8"
zPort[,1] <- as.character(zPort[,1])
Encoding(zPort[,1]) <- "UTF-8"
rownames(zPort) <- 1:10

###Best Imports
d <- zymurgy %>%
    html_nodes("tbody") %>%
    .[4]  %>% ##Best Imports
    html_nodes("td") %>%      
    html_text() 

zImport <- as.data.frame(matrix(d,nrow=11,ncol=3,byrow = TRUE))
colnames(zImport) <- c("Rank","Beer","Country")
zImport <- zImport[2:11,]
zImport[,2] <- as.character(zImport[,2])
Encoding(zImport[,2]) <- "UTF-8"
zImport[,1] <- as.character(zImport[,1])
Encoding(zImport[,1]) <- "UTF-8"
rownames(zImport) <- 1:10

Zymurgy <- bind_rows(Beers, zBreweries, zImport, zPort)

write.csv(Zymurgy, file="Zymurgy Top Beers.csv", row.names = FALSE) 
rm(zBeers,zBreweries,zImport,zPort,Zymurgy,a,b,c,d,zymurgy)






