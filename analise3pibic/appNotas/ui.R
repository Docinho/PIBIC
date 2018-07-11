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
    # column(3,div(class = "logininfo",
    #              uiOutput("userPanel")))
    
    fluidRow(
      column(3,
             div(class = "span1",      
                 uiOutput("obs")
             )
      ),
      fluidRow(column(3,div(class = "logininfo",
                   uiOutput("userPanel")))),
      hr(),
    column(5,
             # hr(),
             div(class = "DataTable",      
                 uiOutput('dataTable')
             )     
      ),
      column(5, 
             div(class = "DataTable",      
                    uiOutput('dataTable1')
      ))
    )
  )
)
