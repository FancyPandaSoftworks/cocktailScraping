###############
###Libraries###
###############
library(data.table)
library(shiny)
library(shinydashboard, warn.conflicts = FALSE)
library(readr)
library(sqldf)



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






######################################
###Load in and preprocess dataframe###
######################################

myCocktail <- read.csv("myCocktail.csv", strip.white = TRUE)
myCocktail$X<-NULL

###Replace the dot in the names with a space###
names(myCocktail) <- gsub(names(myCocktail), pattern = "\\.", replacement = " ")  

###Change column name###
names(myCocktail)[which(names(myCocktail)=="cocktailName")]<- "cocktail name"

###Change type###
changeFactorToCharacter()

###########################################################
###Get frequency of all the alcohol, useful for ordering###
###########################################################

###Get frequency table###
alcoholFreq <- as.data.frame(table(myCocktail$ingredientList))

###Order the alcohol by frequency, highest frequency on top###
alcoholFreq <- alcoholFreq[order(-alcoholFreq$Freq),]

###Change variable name so it is more understandable###
names(alcoholFreq)[1] <- "alcohol"
alcoholFreq$alcohol <- as.character(alcoholFreq$alcohol)

###@@@@@@@###
###Removal###
###@@@@@@@###

###Remove "" from the list###
alcoholFreq <- alcoholFreq[!alcoholFreq$alcohol=="",]

###Reindex###
row.names(alcoholFreq) <- 1:nrow(alcoholFreq)


###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@###
###Store the ingredients in separate lists###
###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@###

###All alcohol###
listAlcohol <- c("gin", 
                    "rum", 
                    "vodka", 
                    "tequila", 
                    "whiskey", 
                    "vermouth", 
                    "brandy",
                    "triple sec",
                    "cognac",
                    "campari", 
                    "amaretto",
                    "grand marnier",
                    "cointreau",
                    "blue curacao",
                    "baileys irish cream",
                    "cachaca",
                    "lillet",
                    "absinthe",
                    "yellow chartreuse",
                    "coffee liqueur",
                    "orange curacao",
                    "peach schnapps",
                    "maraschino liqueur",
                    "sake",
                    "champagne")

###Sort by name###
listAlcohol <- sort(listAlcohol)


###All juice###
listJuice <- c("lemon juice", 
               "lime juice", 
               "orange juice", 
               "pineapple juice", 
               "cranberry juice", 
               "cola", 
               "grapefruit juice",
               "lemonade",
               "sour mix",
               "guava juice",
               "passion fruit juice")

###Sort by name###
listJuice <- sort(listJuice)

listOthers <- c("syrup", 
                "bitters", 
                "egg white", 
                "soda",
                "mint",
                "sugar")
listOthers <- sort(listOthers)

#######################
###Building Shinyapp###
#######################

###Shinyapp UI###
ui <- dashboardPage(
  dashboardHeader(title = "cocktail dashboard"),
  dashboardSidebar(
    sidebarMenu(
      tags$head(
        tags$style(Type="text/css", 
                   ".test_type{color: white;
                   font-size: 20px; 
                   font-style: italic;}"
        )
        ),
      
      ###Which alcohol do you have###
      # checkboxGroupInput(inputId = "yourAlcohol", 
      #                    label = "Select the ingredients you have",
      #                    choices = alcoholFreq$alcohol)
      
      ###Under construction###
      
      ###Alcohol selection###
      selectizeInput(inputId = 'yourAlcohol', 
                     label = "Alcohol", 
                     choices = listAlcohol,
                     multiple = TRUE),
      
      ###Juice selection###
      selectizeInput(inputId = 'yourJuice', 
                     label = "Juice", 
                     choices = listJuice,
                     multiple = TRUE),
      
      ###Other selections###
      selectizeInput(inputId = "yourOthers",
                     label = "Others,like syrup and bitters",
                     choices = listOthers,
                     multiple = TRUE
      )
        ),
    
    sidebarMenuOutput("cocktailAlcohol")
    ),
  dashboardBody(
    fluidPage(
      tabsetPanel(
        ##################################
        ###All cocktails are shown here###  ###The order afterwards is from highest frequency to the 
        ##################################                                      lowest frequency of the main alcohol### 
        
        ###App title###
        tabPanel(title = "All cocktails here", value = "allcocktail", fluid = TRUE,  titlePanel("In the land of beers, 
                                                                                                the cocktail is king"),
                 ###Sidebar layout with input and output definitions###
                 sidebarLayout(
                   
                   ###Sidebar panel for inputs###
                   sidebarPanel(
                     
                     ###Input for cocktail###
                     uiOutput("cocktailInput")
                     
                   ),
                   
                   mainPanel(
                     
                     ###Output###
                     dataTableOutput('cocktailTableAll'),
                     htmlOutput('descriptionAll')
                     
                   )
                 )
        ),
        
        
        ####################  
        ###Gin cocktails####  
        ####################
        
        tabPanel(title = "Gin", value = "gin", fluid = TRUE,  titlePanel("Shaken, not stirred (please no)"),
                 ###Sidebar layout with input and output definitions###
                 sidebarLayout(
                   
                   ###Sidebar panel for inputs###
                   sidebarPanel(
                     
                     ###all gin cocktails###
                     selectInput(inputId = "cocktailGin", 
                                 label = "James Bond is that you?", 
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$ingredientList=="gin"]))
                     )
                     
                   ),
                   
                   ###Main panel###
                   mainPanel(
                     
                     ###Output###
                     dataTableOutput('cocktailTableGin'),
                     htmlOutput('descriptionGin')
                     
                   )
                 )
        ),
        
        
        
        
        
        ####################  
        ###Rum cocktails####  
        ####################
        
        tabPanel(title = "Rum", value = "rum", fluid = TRUE,  titlePanel("Drink rum like pirates"),
                 ###Sidebar layout with input and output definitions###
                 sidebarLayout(
                   
                   ###Sidebar panel for inputs###
                   sidebarPanel(
                     
                     ###all gin cocktails###
                     selectInput(inputId = "cocktailRum", 
                                 label = "So pirate, so unicorn", 
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$ingredientList=="rum"]))
                     )
                     
                   ),
                   
                   ###Main panel###
                   mainPanel(
                     
                     ###Output###
                     dataTableOutput('cocktailTableRum'),
                     htmlOutput('descriptionRum')
                     
                   )
                 )
        ),
        
        
        
        ######################
        ###Vodka cocktails####  
        ######################
        
        tabPanel(title = "Vodka", value = "vodka", fluid = TRUE,  titlePanel("With joy and vodka in 1969"),
                 ###Sidebar layout with input and output definitions###
                 sidebarLayout(
                   
                   ###Sidebar panel for inputs###
                   sidebarPanel(
                     
                     ###all vodka cocktails###
                     selectInput(inputId = "cocktailVodka", 
                                 label = "Do you celebrate with vodka after the next landing?", 
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$ingredientList=="vodka"]))
                     )
                     
                   ),
                   
                   ###Main panel###
                   mainPanel(
                     
                     ###Output###
                     dataTableOutput('cocktailTableVodka'),
                     htmlOutput('descriptionVodka')
                     
                   )
                 )
        ),
        
        
        
        
        #######################
        ###Whiskey cocktails### 
        #######################
        
        tabPanel(title = "Whiskey", value = "rum", fluid = TRUE,  titlePanel("Love some whiskey girls"),
                 ###Sidebar layout with input and output definitions###
                 sidebarLayout(
                   
                   ###Sidebar panel for inputs###
                   sidebarPanel(
                     
                     ###all gin cocktails###
                     selectInput(inputId = "cocktailWhiskey", 
                                 label = "Mysterious, full of character or a bit sweet?", 
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$ingredientList=="whiskey"]))
                     )
                     
                   ),
                   
                   mainPanel(
                     
                     ###Output###
                     dataTableOutput('cocktailTableWhiskey'),
                     htmlOutput('descriptionWhiskey')
                     
                   )
                 )
        ),
        
        
        
        
        ####################  
        ###Tequila cocktails####  
        ####################
        
        tabPanel(title = "Tequila", value = "rum", fluid = TRUE,  titlePanel("Pablo Tequilabar"),
                 ###Sidebar layout with input and output definitions###
                 sidebarLayout(
                   
                   ###Sidebar panel for inputs###
                   sidebarPanel(
                     
                     ###all gin cocktails###
                     selectInput(inputId = "cocktailTequila", 
                                 label = "Pablo trades Tequila, not drugs", 
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$ingredientList=="tequila"]))
                     )
                     
                   ),
                   
                   mainPanel(
                     
                     ###Output###
                     dataTableOutput('cocktailTableTequila'),
                     htmlOutput('descriptionTequila')
                     
                   )
                 )
        )
      )
  )
  )
)


                                              
                                              #####################
                                              ###Shinyapp server###
                                              #####################


server <- function(input, output){
  
  
  
  ###################
  ###All cocktails###
  ###################
  
  alcoholList <- c()
  resultList <- c()
  output$cocktailInput <- renderUI({
    
    ###Get the input###
    yourAlcohol <- input$yourAlcohol
    yourJuice <- input$yourJuice
    yourOthers <- input$yourOthers
    
    ###Combine the inputs
    combineList <- c(yourAlcohol, yourJuice, yourOthers)
    ###If nothing is chosen in the sidebar, then we show all the cocktails###
    if(is.null(combineList)){
      choice <- sort(unique(myCocktail$`cocktail name`)) 
    }
    
    ###We store the chosen ingredients in a list and shows the cocktails that contain the ingredients###
    else{
      alcoholList <- append(alcoholList, combineList)
      
      
      ###Store all the cocktails that has the ingredient in the list, even if it is only one ingredient, the cocktail will be added.
      for (i in 1:length(alcoholList)) {
        resultList <- unique(append(resultList, myCocktail$`cocktail name`[myCocktail$ingredientList==alcoholList[i]]))
      }
      # print(resultList)
      # print("------------")
      ###Remove all the NA###
      resultList <- resultList[complete.cases(resultList)]
      
      ###Get the alcohol information that is in the alcohol list###
      getCocktail <- myCocktail[myCocktail$`cocktail name` %in% resultList,]
      
      ###Get the ingredients and the name of the cocktail###
      getCocktail <- unique(getCocktail[,c(11,10)])
      #print(getCocktail)
      ###Reindex###
      rownames(getCocktail) <- 1:nrow(getCocktail)
      

      ###Which ingredients are clicked###
      getCocktail$isClicked <- getCocktail$ingredientList %in% alcoholList
      getCocktail$isClicked <- as.integer(getCocktail$isClicked)
      colnames(getCocktail)[2] <- "cocktailName"
      #print(getCocktail)
      
      ###Cocktails that can't be made due to lack of ingredients
      lackIngredient <- getCocktail[getCocktail$isClicked==0,]
      #print(lackIngredient)
      
      
      ###!!!!!!!!!!!!!!!!!!!!!!!!!###
      ###Get all the cocktails you are able to make, if there is at least one cocktail available###
      if(nrow(getCocktail[!getCocktail$cocktailName %in% lackIngredient$cocktailName,])!=0){
        getCocktail <- getCocktail[!getCocktail$cocktailName %in% lackIngredient$cocktailName,]
      }
      
      ###Get the total count of all the alcohol matched with the cocktail###
      getCocktail <- sqldf("select cocktailName, sum(isClicked) as count
                            from getCocktail
                            group by cocktailName")

      ###Order these by names###
      getCocktail <- getCocktail[order(getCocktail$cocktailName),]
      
      ###Rename column###
      colnames(getCocktail)[1] <- "cocktail name"
      #print(getCocktail)
      
      
      choice <- getCocktail$`cocktail name`
    }
    
    selectInput(inputId = "cocktailInput",
                label = "Time to move on, crownless king",
                choices = choice)
  })
  
  output$cocktailTableAll <- renderDataTable({ 
    
    ###Create the ingredients and the ratio dataframe###
    res <- myCocktail[,c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[myCocktail$`cocktail name`==input$cocktailInput,]
    ###Either choose IBA or commonly used ingredient###
    res <- res[,colSums(is.na(res))<nrow(res)]
    res <- unique(res)
    
    ###Reindex###
    row.names(res)<- 1:nrow(res)
    
    ###Only the first row needs to show the cocktail name, other rows don't need to###
    if(nrow(res)>1){
      for (i in 2:nrow(res)) {
        res$`cocktail name`[i]<- NA
      }
    }
    
    res <- res
  })
  
  ###description below the table###
  output$descriptionAll <- renderText({
    paste("<h4><br><br><br>",unique(myCocktail$Preparation[myCocktail$`cocktail name`==input$cocktailInput]), "</h4>") 
  })
  
  
  
  
  
  
  #########
  ###Gin###
  #########
  
  ###Creating output###
  output$cocktailTableGin <- renderDataTable({ 
    
    
    ###Gin cocktail only###
    name <- myCocktail$`cocktail name`[myCocktail$ingredientList=="gin"]
    res <- myCocktail[myCocktail$`cocktail name` %in% name,]
    ###Create the ingredients and the ratio###
    res <- res[, c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[res$`cocktail name`==input$cocktailGin,]
    print(res)
    ###Remove na###
    res <- res[rowSums(is.na(res)) != ncol(res), ]
    ###Either choose IBA or commonly used ingredient###
    res <- res[,colSums(is.na(res))<nrow(res)]
    res <- unique(res)
    
    ###Reindex###
    row.names(res)<- 1:nrow(res)
    
    ###Only the first row needs to show the cocktail name, other rows don't need to###
    for (i in 2:nrow(res)) {
      res$`cocktail name`[i]<- NA
    }
    res <- res
    
    
  })
  
  ###description below the table###
  output$descriptionGin <- renderText({
    paste("<h4><br><br><br>",unique(myCocktail$Preparation[myCocktail$`cocktail name`==input$cocktailGin]), "</h4>") 
  })
  
  
  
  
  
  #########
  ###Rum###
  #########
  
  ###Creating output###
  output$cocktailTableRum <- renderDataTable({ 
    
    
    ###Rum cocktail only###
    name <- myCocktail$`cocktail name`[myCocktail$ingredientList=="rum"]
    res <- myCocktail[myCocktail$`cocktail name` %in% name,]
    ###Create the ingredients and the ratio###
    res <- res[, c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[res$`cocktail name`==input$cocktailRum,]
    print(res)
    ###Remove na###
    res <- res[rowSums(is.na(res)) != ncol(res), ]
    ###Either choose IBA or commonly used ingredient###
    res <- res[,colSums(is.na(res))<nrow(res)]
    res <- unique(res)
    
    ###Reindex###
    row.names(res)<- 1:nrow(res)
    
    ###Only the first row needs to show the cocktail name, other rows don't need to###
    for (i in 2:nrow(res)) {
      res$`cocktail name`[i]<- NA
    }
    res <- res
    
  })
  
  ###description below the table###
  output$descriptionRum <- renderText({
    paste("<h4><br><br><br>",unique(myCocktail$Preparation[myCocktail$`cocktail name`==input$cocktailRum]), "</h4>") 
  })
  
  
  
  
  
  
  
  
  ###########
  ###Vodka###
  ###########
  
  ###Creating output###
  output$cocktailTableVodka <- renderDataTable({ 
    
    
    ###Vodka cocktail only###
    name <- myCocktail$`cocktail name`[myCocktail$ingredientList=="vodka"]
    res <- myCocktail[myCocktail$`cocktail name` %in% name,]
    
    ###Create the ingredients and the ratio###
    res <- res[, c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[res$`cocktail name`==input$cocktailVodka,]
    res <- res[rowSums(is.na(res)) != ncol(res), ]
    ###Either choose IBA or commonly used ingredient###
    res <- res[,colSums(is.na(res))<nrow(res)]
    res <- unique(res)
    
    ###Reindex###
    row.names(res)<- 1:nrow(res)
    
    ###Only the first row needs to show the cocktail name, other rows don't need to###
    for (i in 2:nrow(res)) {
      res$`cocktail name`[i]<- NA
    }
    res <- res
    
    
  })
  
  ###description below the table###
  output$descriptionVodka <- renderText({
    paste("<h4><br><br><br>",unique(myCocktail$Preparation[myCocktail$`cocktail name`==input$cocktailVodka]), "</h4>") 
  })
  
  
  
  
  #############
  ###Whiskey###
  #############
  
  ###Creating output###
  output$cocktailTableWhiskey <- renderDataTable({ 
    
    
    ###Whiskey cocktail only###
    name <- myCocktail$`cocktail name`[myCocktail$ingredientList=="whiskey"]
    res <- myCocktail[myCocktail$`cocktail name` %in% name,]
    
    ###Create the ingredients and the ratio###
    res <- res[, c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[res$`cocktail name`==input$cocktailWhiskey,]
    res <- res[rowSums(is.na(res)) != ncol(res), ]
    ###Either choose IBA or commonly used ingredient###
    res <- res[,colSums(is.na(res))<nrow(res)]
    res <- unique(res)
    
    ###Reindex###
    row.names(res)<- 1:nrow(res)
    
    ###Only the first row needs to show the cocktail name, other rows don't need to###
    for (i in 2:nrow(res)) {
      res$`cocktail name`[i]<- NA
    }
    res <- res
    
    
  })
  
  ###description below the table###
  output$descriptionWhiskey <- renderText({
    paste("<h4><br><br><br>",unique(myCocktail$Preparation[myCocktail$`cocktail name`==input$cocktailWhiskey]), "</h4>") 
  })
  
  
  
  
  #############
  ###Tequila###
  #############
  
  ###Creating output###
  output$cocktailTableTequila <- renderDataTable({ 
    
    
    ###Tequila cocktail only###
    name <- myCocktail$`cocktail name`[myCocktail$ingredientList=="tequila"]
    res <- myCocktail[myCocktail$`cocktail name` %in% name,]
    
    ###Create the ingredients and the ratio###
    res <- res[, c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[res$`cocktail name`==input$cocktailTequila,]
    res <- res[rowSums(is.na(res)) != ncol(res), ]
    ###Either choose IBA or commonly used ingredient###
    res <- res[,colSums(is.na(res))<nrow(res)]
    res <- unique(res)
    print(res)
    ###Reindex###
    row.names(res)<- 1:nrow(res)
    
    ###Only the first row needs to show the cocktail name, other rows don't need to###
    for (i in 2:nrow(res)) {
      res$`cocktail name`[i]<- NA
    }
    res <- res
    
    
  })
  
  ###description below the table###
  output$descriptionTequila <- renderText({
    paste("<h4><br><br><br>",unique(myCocktail$Preparation[myCocktail$`cocktail name`==input$cocktailTequila]), "</h4>") 
  })
  
}


shinyApp(ui, server)