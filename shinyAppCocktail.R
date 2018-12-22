

library(data.table)
library(shiny)
library(shinydashboard, warn.conflicts = FALSE)
library(readr)
myCocktail <- read.csv("myCocktail.csv")
myCocktail$X<-NULL

###Shinyapp UI ###
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
      
      #All my alcohol 
      div(class="test_type",
          menuItem("Whiskey"), 
          menuItem("Vodka"), 
          menuItem("Gin"),
          menuItem("White Rum"),
          menuItem("Tequila"),
          menuItem("Brandy"),
          menuItem("Blue Curacao"),
          menuItem("Triple Sec"),
          menuItem("Cointreau"),
          menuItem("Absolut Citron"),
          menuItem("Vermouth"),
          menuItem("Maraschino Liqueur"),
          menuItem("Coffee Liqueur"),
          menuItem("Mezcal"),
          menuItem("cachaça"),
          menuItem("Absinth"),
          menuItem("Lemon juice"),
          menuItem("Lime juice"),
          menuItem("Bitters"),
          menuItem("Syrup"))
        )
    ),
  dashboardBody(
    fluidPage(
      tabsetPanel(
        
        # App title ----
        tabPanel(title = "All cocktails here!!!", value = "allcocktail", fluid = TRUE,  titlePanel("cocktailsssssssssss :D"),
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                   
                   # Sidebar panel for inputs ----
                   sidebarPanel(
                     
                     # Input for cocktail
                     selectInput(inputId = "cocktailAll", 
                                 label = "Which nice cocktail do you wanna make :D", 
                                 choices = sort(unique(myCocktail$cocktailName)) 
                     )
                     
                   ),
                   
                   # Main panel
                   mainPanel(
                     
                     # Output
                     dataTableOutput('cocktailTableAll'),
                     htmlOutput('descriptionAll')
                     
                   )
                 )
        ),
        
        #Whiskey cocktails
        tabPanel(title = "Whiskey lovers", value = "whiskey", fluid = TRUE,  titlePanel("Whiskey lovers :D"),
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                   
                   # Sidebar panel for inputs ----
                   sidebarPanel(
                     
                     #cocktail for Whiskey lovers :D
                     selectInput(inputId = "cocktailWhiskey", 
                                 label = "cocktails for Whiskey lovers >:D", 
                                 choices = sort(myCocktail$cocktail[myCocktail$type=="whiskey"|myCocktail$type=="empty"])
                     )
                     
                   ),
                   
                   # Main panel
                   mainPanel(
                     
                     # Output
                     dataTableOutput('cocktailTableWhiskey'),
                     htmlOutput('descriptionWhiskey')
                     
                   )
                 )
        )
      )
    )
  )
)

#Shinyapp server
server <- function(input, output){
  
  ############################################
  ################All cocktails###############
  ############################################
  #Creating output
  #res <- res[myCocktail$cocktailName=="B and B",]
  
  output$cocktailTableAll <- renderDataTable({ 
    
    #Create the ingredients and the ratio 
    #res<-myCocktail[c(3,4,5,6,9,7),]
    
    #Output will be based on which cocktail 
    #colNumber<- grep(input$cocktailAll, colnames(myCocktail))
    #res[,c(1,colNumber)]
    res <- myCocktail[,c(8,9,10)]
    res <- res[myCocktail$cocktailName==input$cocktailAll,]
    
    for (i in 2:nrow(res)) {
      res$cocktailName[i]<- NA
    }
    res <- res[,colSums(is.na(res))<nrow(res)]
    res <- res
  })
  
  #description below the table
  output$descriptionAll <- renderText({
    paste("<h4><br><br><br>",unique(myCocktail$Preparation[myCocktail$cocktail==input$cocktailAll]), "</h4>") 
  })
  
  ############################################
  ##################Whiskey###################
  ############################################
  #Creating output 
  output$cocktailTableWhiskey <- renderDataTable({ 
    
    #Create the ingredients and the ratio 
    res<-myCocktail[, c(2,3)]
    
    #Output will be based on which cocktail 
    res<- res[myCocktail$cocktail==input$cocktailWhiskey]
    
    
    
  })
  
  #description below the table
  output$descriptionWhiskey <- renderText({
    paste("<h4>",unique(myCocktail$description[myCocktail$cocktail==input$cocktailWhiskey]), "</h4>") 
  })
  
}


shinyApp(ui, server)
