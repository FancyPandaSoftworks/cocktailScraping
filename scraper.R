rm(list=ls())
                                            ################################################
                                      ###Create a crawler to scrape all cocktail data from wiki###
                                            ################################################

###############
###Libraries###
###############
library(rvest)
library(magrittr)
library(sqldf)
library(XML)
library(dplyr)
library(purrr)
library(tidyr)
library(dplyr)
library(data.table)


###################
###Preprocessing###
###################
###Base URL with all the cocktails###
baseUrl <- read_html("https://en.wikipedia.org/wiki/List_of_cocktails")


###Create empty cocktail list###
cocktailList <- data.table("attributes" = c("Type",
                                            "Primary alcohol by volume",
                                            "Served",
                                            "Standard garnish",
                                            "Standard drinkware",
                                            "IBA specifiedingredients",
                                            "Preparation",
                                            "Timing"))



###Retrieve all the cocktails in the list, after analyzing the wiki cocktail page we set a certain range in which we want to crawl###
for (i in 8:34) {
  
  
  ###########################################################################################################################
  ###First part is getting the list where the cocktails belong(Gin, Vodka etc). Then we crawl the cocktails from that list###
  ###########################################################################################################################
  ###Get xpath of the list###
  xpath <- paste0('//*[@id="mw-content-text"]/div/div[', i, ']/ul')
  
  ###It can occur that the list is empty, which means nothing to scrape. In this case, we skip it###
  if(length(html_nodes(baseUrl, xpath = xpath))==0){
    next
  }
  
  ###Start scraping###
  else{
    mainCocktail <- html_nodes(baseUrl, xpath = xpath)
    mainCocktail <- html_nodes(mainCocktail ,xpath = 'li/a/@href[1]')
    
    ###Extract the links out of the xml nodes###
    webList <- c()
    for (i in 1:length(mainCocktail)) {
      webList[i] <- as.character(mainCocktail[i])
    }
    
    webList <- as.data.frame(webList)
    
    
    ###Cleanse the url and make the url correct so it can be used to crawl###
    webList$webList <- sub("href=", "https://en.wikipedia.org", webList$webList)
    webList$webList <- gsub('"', '', webList$webList)
    webList$webList <- gsub('\\s',"", webList$webList)
    
    ###It seems like some have %C3 in their names as a kind of hyperlink, so we cannot remove it if that is the case###
    for (i in 1:nrow(webList)) {
      if(!('%C3' %in% webList$webList[i])){
        webList$webList <- gsub('#.*', '', webList$webList)
      }
      
    }
    
    ###After this is finished, we obtain the main type with all the cocktail links included###
    
    ######################################################
    ###Here we start crawling the cocktails of the list###
    ######################################################
    
    ###Create a loop so it crawls all the links of the main type###
    for (i in 1:nrow(webList)) {
      
      ###Read the page into R###
      cocktailPage <- read_html(webList$webList[i])
      
      ###Scrape the table that includes information about the cocktail###
      cocktail <- cocktailPage %>% 
        html_nodes("table.infobox") %>%
        html_table(header=TRUE)
      
      ###The length of the cocktail shows the number of cocktails on one page###
      ###If the length is 0, then it means that there is no information, so we skip that###
      if(length(cocktail)>0){
        for (j in 1:length(cocktail)) {
          
          ###Convert into dataframe###
          cocktailDf <- cocktail[[j]]
          
          ###Scrape name of the cocktail###
          cocktailName <- cocktailPage %>% html_nodes(xpath = paste0('//*[@id="mw-content-text"]/div/table[', j, ']/caption')) ###todo
          
          ###It occasionally happens that the name is in the second OR EVEN ANOTHER RANDOM index for some reason, thanks wiki###
          if(j == 1){
            h <-1
            while(length(cocktailName)==0){
              
              cocktailName <- cocktailPage %>% html_nodes(xpath = paste0('//*[@id="mw-content-text"]/div/table[', h+j, ']/caption'))
              h <- h+1
            }
          }
          ###Adding the number to the index (j) so it gets the right name###
          else{
            if(length(cocktailName)==0){
              cocktailName <- cocktailPage %>% html_nodes(xpath = paste0('//*[@id="mw-content-text"]/div/table[', h+j, ']/caption'))
            }
          }
          cocktailName <- as.character(cocktailName)
          
          ###Cleanse cocktail name so html gets removed###
          cocktailName <- gsub(".*>(.*)<.*", "\\1", cocktailName)
          
          ###Because there is one stupid name that makes the cocktail not showing the table###
          cocktailName<- gsub('[[:punct:]]aka.*',"",cocktailName)
          ###If the cocktail already exists, skip###
          if(cocktailName %in% colnames(cocktailList)){
            print(webList$webList[i])
            next
            
          }
          
          ###Change column name so we can merge it with the cocktail table###
          cocktailDf <- as.data.table(cocktailDf)
          colnames(cocktailDf)[1] <- "attributes"
          
          ###The name of the cocktail###
          colnames(cocktailDf)[2] <- cocktailName
          
          ###Change \n into <br/> of all tables###
          for (k in 1:nrow(cocktailDf)) {
            cocktailDf[k,2] <- gsub("\n", "NNN",cocktailDf[k,2])
          }
          
          ###Merging the cocktails with the cocktail table###
          cocktailList <- merge(cocktailList, cocktailDf, by = "attributes", all = TRUE)
        }
      }
      ###If the length of the cocktail page is 0, it means there is no table to retrieve information. Then we skip to the next one###
      else{
        next
      }
    }
    
    
  }
}

###End Crawling###

#########################################################################
#########################################################################
###Start preprocessing the dataframe so it can be used in the shinyapp###
#########################################################################
#########################################################################

#################################
###Column and row manipulation###
#################################
###Some ingredients are in another row, we put that in the commonly used ingredients###
myCocktail<- as.data.frame(cocktailList)
for (i in 2:ncol(myCocktail)) {
if(!is.na(myCocktail[75,i])&&is.na(myCocktail[52,i])){
myCocktail[52,i]<- myCocktail[75,i]
}
}
myCocktail <- data.table(myCocktail)
###Since the merge will cause a lot of unnecessary rows(Due to force merge), we only select rows that are needed###
myCocktail <- cocktailList[c("attributes","Type",
"Primary alcohol by volume",
"Served",
"Standard garnish",
"Standard drinkware",
"IBA specifiedingredients",
"Preparation",
"Timing",
"Commonly used ingredients"),]
###Column to row name, because it is not a cocktail###
myCocktail <- as.data.frame(myCocktail)
myCocktail <- myCocktail[-c(1),]
###Reindex###
row.names(myCocktail)<- 1:nrow(myCocktail)
View(myCocktail)
#View(myCocktail[,order(names(myCocktail))])
myCocktail <- as.data.frame((t(myCocktail)))
###########################
###Cocktail manipulation###
###########################
###Change factor to character so it can be split###
myCocktail$`1`<- as.character(myCocktail$`1`)
myCocktail$`2`<- as.character(myCocktail$`2`)
myCocktail$`3`<- as.character(myCocktail$`3`)
myCocktail$`4`<- as.character(myCocktail$`4`)
myCocktail$`5`<- as.character(myCocktail$`5`)
myCocktail$`6`<- as.character(myCocktail$`6`)
myCocktail$`7`<- as.character(myCocktail$`7`)
myCocktail$`8`<- as.character(myCocktail$`8`)
myCocktail$`9`<- as.character(myCocktail$`9`)
###Split the ingredients based on certain regular expression###
myCocktail$cocktailName <- rownames(myCocktail)
myCocktail <-myCocktail %>%
mutate(`6` = strsplit(as.character(`6`), "NNN")) %>%
unnest(`6`)
myCocktail <-myCocktail %>%
mutate(`9` = strsplit(as.character(`9`), "NNN")) %>%
unnest(`9`)
###first row to colnames##3
colnames(myCocktail)<- myCocktail[1,]
###Remove first row###
myCocktail <- myCocktail[c(-1),]
###Change column name
colnames(myCocktail)[8]<- "cocktailName"
###Reindex###
row.names(myCocktail)<- 1:nrow(myCocktail)
########################
###Removing cocktails###
########################
###Remove unneeded cocktails###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Astro pop",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Fizz",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Baby Guinness",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Black Velvet",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="BLT",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="pop",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="pop",]
###Cocktail without a type gets removed###
myCocktail<-myCocktail[!is.na(myCocktail$Type),]

############################################################################################################
###Some cocktails aren't show correctly, those are investigated, corrected or removed if it isn't fixable###
############################################################################################################
###Write###
write.csv(myCocktail, "myCocktail.csv")
