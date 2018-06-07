## Proposito: Coleta de notas do aluno do controle academico e uso de regressao linear para possivel desempenho

library(dplyr)
library(stringr)
library(shiny)
library(rvest)
library(reticulate)

# arquivo com o algoritmo para selecionar as cadeiras que podem ser cursadas e fazendo predicao de desempenho se houver dados suficientes
source_python("LRGrades.py")
# url de login do Controle Acadêmico
url <- "https://pre.ufcg.edu.br:8443/ControleAcademicoOnline"


tabelaNotas <- function(matricula, senha) {
  # logando no controle academico
  pgsession<-html_session(url)
  pgform<-html_form(pgsession)[[1]]  
  filled_form<-set_values(pgform, login=matricula, senha=senha)
  submit_form(pgsession, filled_form)
  
  # url que possui os dados necessarios para a aplicacao
  urlLogada <- "https://pre.ufcg.edu.br:8443/ControleAcademicoOnline/Controlador?command=AlunoHistorico"
  
  # mudando para a pagina do historico e selecionando a tabela de notas e transformando-a no formato de df 
  page <- jump_to(pgsession, urlLogada)
  
  table <- page %>% read_html() %>% html_nodes(xpath = "/html/body/div[3]/div[3]/div[3]/table") %>% html_table() 
  df <- as.data.frame(table)
  # Eliminando disciplinas que são optativas e que o aluno está cursando
  df_notas <- df %>% filter(Situação != "Em Curso", !str_detect(Disciplina, "^TECC")) %>% select('Disciplina', 'Média')
  
  # escrevendo arquivo de notas
  write.csv(df_notas_base, "temp.csv", row.names = FALSE)
  return(df_notas)
}


# aplicacao
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