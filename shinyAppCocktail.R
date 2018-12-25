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

myCocktail <- read.csv("myCocktail.csv")
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
alcoholFreq <- as.data.frame(table(myCocktail$`Primary alcohol by volume`))

###Order the alcohol by frequency, highest frequency on top###
alcoholFreq <- alcoholFreq[order(-alcoholFreq$Freq),]

###Change variable name so it is more understandable###
names(alcoholFreq)[1] <- "alcohol"

###Reindex###
row.names(alcoholFreq) <- 1:nrow(alcoholFreq)

###Get the top 15 alcohol, other alcohols are not needed due to low frequency###
alcoholFreq <- alcoholFreq[1:15,]

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
      checkboxGroupInput(inputId = "yourAlcohol", 
                         label = "Select the alcohol you have",
                         choices = alcoholFreq$alcohol)
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
                   
                   ###Main panel###
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
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$`Primary alcohol by volume`=="gin"]))
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
                                 label = "ARRRRR, I am pirate or something, or a unicorn", 
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$`Primary alcohol by volume`=="rum"]))
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
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$`Primary alcohol by volume`=="vodka"]))
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
                                 label = "Smoky, heavy, or a bit sweet?", 
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$`Primary alcohol by volume`=="whiskey"]))
                     )
                     
                   ),
                   
                   ###Main panel###
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
        
        tabPanel(title = "Tequila", value = "rum", fluid = TRUE,  titlePanel("Use your nose first for Tequila"),
                 ###Sidebar layout with input and output definitions###
                 sidebarLayout(
                   
                   ###Sidebar panel for inputs###
                   sidebarPanel(
                     
                     ###all gin cocktails###
                     selectInput(inputId = "cocktailTequila", 
                                 label = "With or without the lemon, that's the question", 
                                 choices = sort(unique(myCocktail$`cocktail name`[myCocktail$`Primary alcohol by volume`=="tequila"]))
                     )
                     
                   ),
                   
                   ###Main panel###
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




####Shinyapp server###
server <- function(input, output){
  
  
  
  ###################
  ###All cocktails###
  ###################
  alcoholList <- c()
  resultList <- c()
  output$cocktailInput <- renderUI({
    
    ###If nothing is chosen in the sidebar, then we show all the cocktails###
    if(is.null(input$yourAlcohol)){
      choice <- sort(unique(myCocktail$`cocktail name`)) 
    }
    
    ###We store the chosen alcohol in a list and shows the cocktails that contain the alcohol###
    else{
      alcoholList <- append(alcoholList, input$yourAlcohol)
      
      for (i in 1:length(alcoholList)) {
        resultList <- unique(append(resultList, myCocktail$`cocktail name`[myCocktail$`Primary alcohol by volume`==alcoholList[i]]))
      }
      
      ###Remove all the NA###
      resultList <- resultList[complete.cases(resultList)]
      
      ###Get the alcohol information that is in the alcohol list###
      alcoholCount <- myCocktail[myCocktail$`cocktail name` %in% resultList,]
      alcoholCount <- unique(alcoholCount[,c(2,10)])
      
      ###Reindex###
      rownames(alcoholCount) <- 1:nrow(alcoholCount)
      
      ###1 if the alcohol is in the list, 0 if not###
      alcoholCount$isClicked <- alcoholCount$`Primary alcohol by volume` %in% alcoholList
      alcoholCount$isClicked <- as.integer(alcoholCount$isClicked)
      colnames(alcoholCount)[2] <- "cocktailName"
      
      ###Get the total count of all the alcohol matched with the cocktail###
      alcoholCount <- sqldf("select cocktailName, sum(isClicked) as count 
                            from alcoholCount 
                            group by cocktailName 
                            order by count
                            desc")
      colnames(alcoholCount)[1] <- "cocktail name"
      choice <- alcoholCount$`cocktail name`
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
    res <- myCocktail[myCocktail$`Primary alcohol by volume`=="gin",]
    
    ###Create the ingredients and the ratio###
    res <- res[, c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[res$`cocktail name`==input$cocktailGin,]
    
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
    res <- myCocktail[myCocktail$`Primary alcohol by volume`=="rum",]
    
    ###Create the ingredients and the ratio###
    res <- res[, c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[res$`cocktail name`==input$cocktailRum,]
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
    res <- myCocktail[myCocktail$`Primary alcohol by volume`=="vodka",]
    
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
    res <- myCocktail[myCocktail$`Primary alcohol by volume`=="whiskey",]
    
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
    res <- myCocktail[myCocktail$`Primary alcohol by volume`=="tequila",]
    
    ###Create the ingredients and the ratio###
    res <- res[, c(10,6,9)]
    
    ###Select the cocktail###
    res <- res[res$`cocktail name`==input$cocktailTequila,]
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
  output$descriptionTequila <- renderText({
    paste("<h4><br><br><br>",unique(myCocktail$Preparation[myCocktail$`cocktail name`==input$cocktailTequila]), "</h4>") 
  })
  
}


shinyApp(ui, server)
