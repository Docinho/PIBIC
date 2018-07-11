library(shiny)
library(reticulate)
library(dplyr)
library(stringr)

source_python("LRGrades.py")
shinyUI(
  fluidPage(
    
    tagList(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="style.css")
      )
    ),
    
    div(class = "login",
        uiOutput("uiLogin"),
        textOutput("pass"),
        tags$head(tags$style("#pass{color: red;"))
    ),    
    
    fluidRow(
      column(3,
             div(class = "span1",      
                 uiOutput("obs")
             )
      ),
      column(8,
             div(class = "logininfo",
                 uiOutput("userPanel")
             ),
             hr(),
             div(class = "DataTable",      
                 uiOutput('dataTable')
             )     
      )      
    )  
    
  )
)

########################################################

shinyServer(function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE , session = session$user) 
  
  source("www/Login.R",  local = TRUE)
  
  # output$dataTable <- renderUI({    
  #   if (USER$Logged == TRUE) {      
  #     dataTableOutput(USER$df)
  #   }
  # })
  output$dataTable <- renderUI({    
    if (USER$Logged == TRUE) {      
      dataTableOutput('table')
    }
  })
  
  output$dataTable1 <- renderUI({    
    if (USER$Logged == TRUE) {      
      dataTableOutput('table1')
    }
  })
  
  output$table1 <- renderDataTable(
    predicao(),
    options = list(
      pageLength = 100
    )                              
  )
  
  output$table <- renderDataTable(
    USER$df,
    options = list(
      pageLength = 100
      
    )       
  )
  
  
  
    
})
