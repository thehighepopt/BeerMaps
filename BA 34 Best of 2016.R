setwd("C:/Users/Stephen.P.Duffy/Documents/GitHub/BeerMaps")

##Capture HTML From BA Top 250 website and Parse it
Bestof2016 <- read_html("https://www.beeradvocate.com/mag/14961/class-of-2016-34-of-the-best-new-breweries-in-the-us/")

l <- Bestof2016 %>%
    html_nodes("div") %>% ##gives brewery 
    .[4] 



##Parse Beer, Brewery, and Style, and add Rank
bb <- l %>%
    html_nodes("p") %>% ##gives brewery    
    html_nodes("a") %>%
    html_text()
bbs <- as.data.frame(matrix(bb,nrow=34,ncol=1,byrow = TRUE))
colnames(bbs) <- c("Brewery")

##Parse Rank, Beer, Hads remove bang from Hads, Merge with BBS
all <- l %>%
    html_nodes("p") %>% ##     
    html_text()
clob <- as.data.frame(matrix(all,nrow=85,ncol=1,byrow = TRUE))
Brwy <- as.data.frame(clob[c(2,5,7,10,12,15,18,22,25,27,29,31,33,35,38,41,44,47,50,53,
                             56,58,60,62,64,66,68,70,72,75,77,80,82,84),1])
pars <- function(x) word(x,1,sep = '\n')
p <- as.data.frame(sapply(Brwy[,1],pars))
p[,2] <- word(Brwy[,1],-1,sep = '\n')
p[,3] <- word(p[,2],1,sep= ',')
p[,4] <- word(p[,2],2,sep= ',')
p[,4] <- word(p[,4],1,2)
p <- p[,c(1,3:4)]

write.csv(p,"BA Best Breweries of 2016.csv",row.names = FALSE)





