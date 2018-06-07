library(dplyr)
library(stringr)
library(shiny)
library(rvest)
library(reticulate)

source_python("LRGrades.py")

# url de login do Controle Acadêmico
url <- "https://pre.ufcg.edu.br:8443/ControleAcademicoOnline"

tabelaNotas <- function(matricula, senha) {
  pgsession<-html_session(url)
  pgform<-html_form(pgsession)[[1]]  
  filled_form<-set_values(pgform, login=matricula, senha=senha)
  submit_form(pgsession, filled_form)
  
  urlLogada <- "https://pre.ufcg.edu.br:8443/ControleAcademicoOnline/Controlador?command=AlunoHistorico"
  
  page <- jump_to(pgsession, urlLogada)
  
  table <- page %>% read_html() %>% html_nodes(xpath = "/html/body/div[3]/div[3]/div[3]/table") %>% html_table() 
  df <- as.data.frame(table)
  df_notas <- df %>% filter(Situação != "Em Curso", !str_detect(Disciplina, "^TECC")) %>% select('Disciplina', 'Média')
  
  tabelaNotasFiltradas(df_notas)
  return(df_notas)
}

tabelaNotasFiltradas <- function(df_notas_base) {
  df_notas_base <- df_notas_base %>% select('Disciplina', 'Média')
  df_notas_base["Matricula"] <- "0"
  write.csv(df_notas_base, "temp.csv", row.names = FALSE)
  
}

shinyApp(
  
  ui <- fluidPage(
    sidebarPanel(
      textInput("matricula", "Matrícula"),
      textInput("senha", "Senha"),width = 2), 
    fluidRow(
      column(5,
             tableOutput('table')
             ),
      column(4,
        tableOutput('table1')
      )
    )
  ),
  
  server <- function(input, output) {
    
    output$value <- renderText({
      input$matricula
    })
    
    output$value <- renderText({
      input$senha
    })
    
    
    output$table <- renderTable(tabelaNotas(matricula, senha))
    output$table1 <- renderTable(predicao())
    
    
  }
)
