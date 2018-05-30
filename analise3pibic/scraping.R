library(rvest)
library(tidyverse)
library(stringr)

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
df_notas <- df %>% select(Código, Disciplina, Média, Situação)

df_notas_base <- df_notas %>% filter(Situação != "Em Curso")
}
