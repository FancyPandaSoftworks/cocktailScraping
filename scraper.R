#rm(list=ls())
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
library(splitstackshape)
library(tidyverse)

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
for (i in c(8:33, 35:36,38:43)) {
  
  print(i)
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
            #print(webList$webList[i])
            next
            
          }
          
          ###Change column name so we can merge it with the cocktail table###
          cocktailDf <- as.data.table(cocktailDf)
          colnames(cocktailDf)[1] <- "attributes"
          
          ###The name of the cocktail###
          colnames(cocktailDf)[2] <- cocktailName
          
          ###Change \n into <br/> of all tables###
          for (k in 1:nrow(cocktailDf)) {
            cocktailDf[k,2] <- gsub("\n", "~~~",cocktailDf[k,2])
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

###@@@@@@@@@@@@@@@###
###End of crawling###
###@@@@@@@@@@@@@@@###
