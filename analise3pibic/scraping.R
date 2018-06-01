library(dplyr)
library(stringr)
library(shiny)
library(rvest)
library(reticulate)
source_python("LRGrades.py")

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
  return(df_notas_base)
}
data <- tabelaNotas(matricula, senha)
write.csv(data, file="docinho.csv")

tabelaNotasFiltradas <- function(matricula,senha) {
  dfNotas <- tabelaNotas(matricula, senha)
  dfNotas <- dfNotas %>% select('Disciplina', 'Média')
  dfNotas["Matricula"] <- 0
  return(dfNotas)
}

matricula = "116110076"
senha = "100%mais"
# shinyApp(
#   
#   ui <- fluidPage( 
#     textInput("matricula", "Matrícula",width = 9),
#     
#     textInput("senha", "Senha",width = 20),
#     
#     tableOutput('table')
#     
#   ),
#   
#   server <- function(input, output) {
#     
#     output$value <- renderText({
#       input$matricula
#     })
#     
#     output$value <- renderText({
#       input$senha
#     })
#     
#     
#     #         output$table <- renderTable(tabelaNotas(matricula, senha))
#     output$table <- renderTable((predicao(tabelaNotasFiltradas(matricula, senha))))
#     #         output$table <- renderTable(tabelaNotas(input$matricula, input$senha))
#     #         output$table <- renderTable((predicao(tabelaNotasFiltradas(input$matricula, input$senha))))
#     
#   }
# )