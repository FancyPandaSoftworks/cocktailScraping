
                                                    ###########################
                                                ###preprocessing the dataframe###
                                                    ###########################

library(dplyr)
library(tm)        
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
###############
###Functions###
###############
                                                                                 
###Change factors to character function###
changeFactorToCharacter <- function(){
  for (i in 1:ncol(myCocktail)) {
    myCocktail[,i] <<- as.character(myCocktail[,i])
  }
  myCocktail$`Standard drinkware` <<- as.character(myCocktail$`Standard drinkware`)
  myCocktail$`IBA specifiedingredients` <<- as.character(myCocktail$`IBA specifiedingredients`)
  myCocktail$`Commonly used ingredients` <<- as.character(myCocktail$`Commonly used ingredients`)
}







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

###Transpose the dataframe so the cocktails are rows and variables become columns###
myCocktail <- as.data.frame((t(myCocktail)))

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

###Names of the cocktails become a column###
myCocktail$cocktailName <- rownames(myCocktail)

###first row to colnames###
colnames(myCocktail)<- myCocktail[1,]

###Remove first row###
myCocktail <- myCocktail[c(-1),]

###Change column name
names(myCocktail)[names(myCocktail)=="attributes"] <- "cocktailName"

###Reindex###
row.names(myCocktail)<- 1:nrow(myCocktail)


###Replace the dot in the names with a space###
names(myCocktail) <- gsub(names(myCocktail), pattern = "\\.", replacement = " ")  







#######################################################################################
###Modifiying the cocktail informations needed for Shinyapp so it is shown correctly###
#######################################################################################

###Something goes wrong that causes a bug when publish Shinyapp###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Agent Orange",]

###Split the ingredients based on certain regular expression###
myCocktail <- cSplit(myCocktail, "IBA specifiedingredients", sep = "~~~", direction = "long")
myCocktail <- cSplit(myCocktail, "Commonly used ingredients", sep = "~~~", direction = "long")
myCocktail <- myCocktail[order(myCocktail$cocktailName),]








#################################################################################
###Some cocktails aren't shown correctly, those are investigated and corrected###
#################################################################################

###Zombie was not shown correctly because the ingredients were in the drinkware column###
myCocktail <- cSplit(myCocktail, "Standard drinkware", sep = "~~~", direction = "long")
myCocktail <- as.data.frame(myCocktail)
changeFactorToCharacter()
myCocktail$`Commonly used ingredients`[myCocktail$cocktailName== "Zombie"] <- myCocktail$`Standard drinkware`[myCocktail$cocktailName== "Zombie"]


####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
###WARNING WARNING WARNING WARNING WARNING WARNING###
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###

###It seems like there are some ingredients in the "Ingredients as listed at CocktailDB" column. ###
###Maybe this column needs to be added in the dataframe, but we will see###

###Example###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Red Russian",]

### all in one place###
# <- myCocktail[!myCocktail$cocktailName=="Breakfast martini",]

####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###
###WARNING WARNING WARNING WARNING WARNING WARNING###
###!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!###







########################
###Removing cocktails###
########################

###Strange format###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Tom and Jerry",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Buck",]

###Bobby burns has the ingredients in a different column, but I am not interested in this one anyway###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Bobby Burns",]

###Not an interesting shot###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Astro pop",]

###This is the basic of all Fizz cocktails, so this one is empty. Thus, we remove it###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Fizz",]

###Bad version of B-52###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Baby Guinness",]


###Drink I would never make anyway###
myCocktail <- myCocktail[!myCocktail$cocktailName=="BLT",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Scotch and soda",]

###No proper description###
myCocktail <- myCocktail[!myCocktail$cocktailName=="Black Velvet",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Chicago Cocktail",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Club-Mate",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="El Toro Loco",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Pimm's",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Red Russian",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Salmiakki Koskenkorva",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Shirley Temple",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Sours",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Toronto",]
myCocktail <- myCocktail[!myCocktail$cocktailName=="Death in the Afternoon",]


###Cocktail without a type gets removed###
myCocktail<-myCocktail[!is.na(myCocktail$Type),]
View(myCocktail)







###################################################################################################
###Split the primary alcohol so you can sort it by alcohol, a bit easier to search for cocktails###
###################################################################################################

###Split the alcohol row into single parts###
myCocktail <- cSplit(myCocktail, "Primary alcohol by volume", sep = "~~~", direction = "long")
myCocktail <- as.data.frame(myCocktail)
changeFactorToCharacter()

###Change all alcohol into lower case###
myCocktail$`Primary alcohol by volume` <- tolower(myCocktail$`Primary alcohol by volume`)

###Change all whiskey into whisky###
myCocktail$`Primary alcohol by volume` <- gsub(pattern = ".*whisky", replacement = "whiskey", myCocktail$`Primary alcohol by volume`)

###Remove everything before whiskey so it is easier to sort###
myCocktail$`Primary alcohol by volume` <- gsub(pattern = ".*whiskey", replacement = "whiskey", myCocktail$`Primary alcohol by volume`)

###Change ç to c###
myCocktail$`Primary alcohol by volume` <- gsub("ç", "c", myCocktail$`Primary alcohol by volume`)
myCocktail$`Commonly used ingredients` <- gsub("ç", "c", myCocktail$`Commonly used ingredients`)
myCocktail$`IBA specifiedingredients` <- gsub("ç", "c", myCocktail$`IBA specifiedingredients`)


############################################################################################################################
###Advanced method of looking for alcohol: Instead of going for the primary, we go with every ingredient the cocktail has###
############################################################################################################################

###All ingredients in one list###
myCocktail$ingredientList <- myCocktail$`IBA specifiedingredients`
myCocktail <- mutate(myCocktail, ingredientList = coalesce(ingredientList, `Commonly used ingredients`))

###Remove all numbers##
myCocktail$ingredientList <- removeNumbers(myCocktail$ingredientList)

###-----------------------------------###
###Remove words that are not necessary###
###-----------------------------------###

###To lower###
myCocktail$ingredientList <- tolower(myCocktail$ingredientList)

###Remove stopwords###
myCocktail$ingredientList <- removeWords(myCocktail$ingredientList, stopwords("english"))

###Remove everything in between bracket###
myCocktail$ingredientList <-gsub("\\([^\\)]*\\)", "", myCocktail$ingredientList, perl=TRUE)

###Remove certain parts before the main ingredient###
myCocktail$ingredientList <- gsub(pattern = ".*whisky", replacement = "whiskey", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = ".*whiskey", replacement = "whiskey", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = ".*vermouth", replacement = "vermouth", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = ".*brandy", replacement = "brandy", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = ".*rum", replacement = "rum", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = ".*bitter", replacement = "bitters", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = ".*bitters", replacement = "bitters", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = "lillet.*", replacement = "lillet", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = "coca-cola", replacement = "cola", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = "bourbon", replacement = "whiskey", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = "kahlÃ", replacement = "kahlua", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = "\\–  vodka", replacement = "vodka", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = "coke", replacement = "cola", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = "dashes ", replacement = "", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = ".*syrup", replacement = "syrup", myCocktail$ingredientList)
myCocktail$ingredientList <- gsub(pattern = "bitterss", replacement = "bitters", myCocktail$ingredientList)
###Wordlist with words that need to be removed###
wordList <- c("cl", "oz", "parts", "ml", "one", "part", "dash", "ounces", "ounce", "tsp", "½", "¾","us fluid", "teaspoon", "tsp"
              ,"drops", "wedge", "hot", "tspn", "tbsp", "two", "chopped", "salt", "onion", "extract", "unsweetened", "shots", "carbonated"
              , "water", "fresh", "shot", "slices", "slice", "fresh", "sprig", "sprigs", "teaspoons", "freshly", "squeezed"
              , "lemonÂ", "squash", "spoon", "dâ€™anu", "cup")
myCocktail$ingredientList <- removeWords(myCocktail$ingredientList, wordList)

###Remove non characters###
myCocktail$ingredientList <- gsub("[[:punct:]]","",myCocktail$ingredientList)


###Remove leading white space###
### returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

myCocktail$ingredientList <- trimws(myCocktail$ingredientList, "l")
myCocktail$ingredientList <- trim(myCocktail$ingredientList)

View(table(myCocktail$ingredientList))
######################################################################################
###World cloud, creating one so we know which words are needed but are standing out###
######################################################################################
docs <- Corpus(VectorSource(myCocktail$ingredientList))

###Replace characters to space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

###Transform to document matrix###
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
tail(d, 10)

###Create word cloud###
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=400, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

####@@@@@@@@@@@@@@@@@@@###
###End of preprocessing###
###@@@@@@@@@@@@@@@@@@@@###

###Write###
write.csv(myCocktail, "myCocktail.csv")
