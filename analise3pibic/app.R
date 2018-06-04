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
  df_notas <- df %>% select('Disciplina', 'Média', 'Situação')
  
  df_notas_base <- df_notas %>% filter(Situação != "Em Curso" , Situação != "Dispensa", !str_detect(Disciplina, "^TECC"))
  tabelaNotasFiltradas(df_notas_base)
  return(df_notas_base)
}

tabelaNotasFiltradas <- function(df_notas_base) {
  df_notas_base <- df_notas_base %>% select('Disciplina', 'Média')
  df_notas_base["Matricula"] <- "0"
  write.csv(df_notas_base, "temp.csv", row.names = FALSE)
  
}

matricula = "116110076"
senha = "100%mais"
shinyApp(
  
  ui <- fluidPage( 
    textInput("matricula", "Matrícula",width = 9),
    
    textInput("senha", "Senha",width = 20),
    
    tableOutput('table')
    
  ),
  
  server <- function(input, output) {
    
    output$value <- renderText({
      input$matricula
    })
    
    output$value <- renderText({
      input$senha
    })
    
    
    output$table <- renderTable(tabelaNotas(matricula, senha))
    output$table <- renderTable(predicao())
    #         output$table <- renderTable(tabelaNotas(input$matricula, input$senha))
    #         output$table <- renderTable((predicao(tabelaNotasFiltradas(input$matricula, input$senha))))
    
  }
)
