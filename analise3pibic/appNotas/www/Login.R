#### Log in module ###
library(rvest)
library(plyr)

output$uiLogin <- renderUI({
  if (USER$Logged == FALSE) {
    wellPanel(
      textInput("userName", "User Name:"),
      passwordInput("passwd", "Pass word:"),
      br(),
      actionButton("Login", "Log in")
    )
  }
})

output$pass <- renderText({  
  if (USER$Logged == FALSE) {
    USER$pass
  }  
})

# Login info during session ----
output$userPanel <- renderUI({
  if (USER$Logged == TRUE) {
    fluidRow(
      column(2,
             "User: ", USER$name
      ),
      column(1, actionLink("logout", "Logout"))
    )
  }  
})

# control login
observeEvent(input$Login , {
  Username <- isolate(input$userName)
  Password <- isolate(input$passwd)
  df_grades <- succeedLogged(Username, Password)
  if (! empty(df_grades)) {
    df_notas_base <- df_grades %>% filter(Situação != "Em Curso", !str_detect(Disciplina, "^TECC")) %>% select('Disciplina', 'Média')
    USER$Logged <- TRUE
    USER$name <- Username
    USER$df <- df_notas_base
    # escrevendo arquivo de notas
    write.csv(df_notas_base, "temp.csv", row.names = FALSE)
    } else {
    USER$pass <- "User name or password failed!"
  }
})
succeedLogged <- function(username, password) {
  url <- "https://pre.ufcg.edu.br:8443/ControleAcademicoOnline"
  pgsession<-html_session(url)
  pgform<-html_form(pgsession)[[1]]  
  filled_form<-set_values(pgform, login=username, senha=password)
  submit_form(pgsession, filled_form)
  # url que possui os dados necessarios para a aplicacao
  urlLogada <- "https://pre.ufcg.edu.br:8443/ControleAcademicoOnline/Controlador?command=AlunoHistorico"
  
  # mudando para a pagina do historico e selecionando a tabela de notas e transformando-a no formato de df 
  page <- jump_to(pgsession, urlLogada)
  
  table <- page %>% read_html() %>% html_nodes(xpath = "/html/body/div[3]/div[3]/div[3]/table") %>% html_table() 
  df <- as.data.frame(table)
  return(df)
}
# control logout
observeEvent(input$logout , {
  USER$Logged <- FALSE
  USER$pass <- ""
})

