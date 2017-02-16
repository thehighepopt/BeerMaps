###http://texascraftbrewersfestival.org/beers

library(rvest)
library(magrittr)
library(plyr)
library(stringr)
library(RCurl)
library(XML)

setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")

##Capture HTML From BA Top 250 website and Parse it
tcbf <- read_html("http://texascraftbrewersfestival.org/beers")

a <- tcbf %>%
    html_nodes("article") 

b <- a %>%
    html_nodes("h3") %>% ##gives brewery      
    html_text()

tcbf_brewery <- as.data.frame(matrix(b,nrow=77,ncol=1,byrow = TRUE))
BBS$Rank <- seq(1,250, by =1)
colnames(tcbf_brewery) <- c("Brewery")

c <- a %>%
    html_nodes("small") %>% ##gives style     
    html_text()
tcbf_style <- as.data.frame(matrix(c,nrow=233,ncol=1,byrow = TRUE))
tcbf_style <- as.data.frame(tcbf_style[!grepl("ROTATING", tcbf_style$V1),])
colnames(tcbf_style) <- c("Style")

d <- a %>%
    html_nodes("h4") %>% ##gives beer and style   
    html_nodes("a") %>%
    html_text()
tcbf_beer <- as.data.frame(matrix(d,nrow=209,ncol=1,byrow = TRUE))

j <- cbind(tcbf_style,tcbf_beer)
j <- cbind(j,nchar(as.character(j$Style)))

j$Beer <- str_sub(j[,2],1,-j[,3]-1)
tcbf_bs <- j[1:181,c(4,1)]





tcbf_rot <- j[192:209,]
tcbf_rot <- tcbf_rot[,c(4,1)]
loc <- function(x) str_locate(x,"[a-z][A-Z]")[1]
loc2 <- function(x) str_locate(x,"[a-z][A-Z]")[2]
tcbf_rot$Brewery <- str_sub(tcbf_rot[,1],1,sapply(tcbf_rot[,1], loc))
tcbf_rot$Beer <- str_sub(tcbf_rot[,1],sapply(tcbf_rot[,1], loc2),-1)
tcbf_rot <- tcbf_rot[,c(3,1,2)]
rownames(tcbf_rot) <- 1:18

write.csv(tcbf_rot, file="Rotating TCBF.csv", row.names = FALSE) 
















