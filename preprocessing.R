
                                                    ###########################
                                                ###preprocessing the dataframe###
                                                    ###########################


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





####@@@@@@@@@@@@@@@@@@@###
###End of preprocessing###
###@@@@@@@@@@@@@@@@@@@@###

###Write###
write.csv(myCocktail, "myCocktail.csv")
