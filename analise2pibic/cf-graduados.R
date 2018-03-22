library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(reshape2)
library(caret)

# leitura dos dados dos alunos da UFCG
dados_alunos <- read.csv("alunosUFCGAnon.csv") 

# filtragem dos alunos de Computação que não evadiram e suas respectivas notas nas cadeiras obrigatórias
dados_aluno_cc <- dados_alunos %>% filter(Cod_Curso == 14102100 & Cod_Evasao == 0 & Tipo == "Obrigatória")

# ordenação dos alunos pela matricula e seleção das features utilizadas pelo algoritmo
dados_aluno_cc <- dados_aluno_cc %>% mutate(Matricula = factor(Matricula)) %>% arrange(Matricula) %>% 
  select(Matricula, Cod_Disciplina, Nome_Disciplina, Periodo, Creditos, Media_Disciplina, Situacao, Periodo_Ingresso, Periodo_Relativo)

# cálculo da média das notas dos alunos em todas as disciplinas removevendo os alunos que trancaram alguma cadeira, pois com isso sua média fica NA
dados_aluno_cc <- dados_aluno_cc %>% group_by(Matricula) %>% mutate(Media = round(mean(Media_Disciplina), digits = 2)) %>% filter(!is.na(Media))
# storing data as factors insures that the modeling functions will treat such data correctly. 
# Factors in R are stored as a vector of integer values with a corresponding set of character values to use when the factor is displayed

# Calulo do CRA
alunos_cra <- dados_aluno_cc %>% mutate(Cra.Crontibute = Media*Creditos) %>% summarise(cra = sum(Cra.Crontibute)/sum(Creditos))

# criando data frame com as notas de aprovação de cada aluno na respectiva cadeira e seu CRA e renomeando as disicplinas
alunos_max_media <- dados_aluno_cc %>% group_by(Matricula, Media_Disciplina) %>% filter(Media_Disciplina == max(Media_Disciplina)) %>% ungroup() %>%
  select(Nome_Disciplina, Matricula, Media_Disciplina) %>% mutate(Nome_Disciplina = as.factor(gsub(" ", ".", Nome_Disciplina))) %>%
  dcast(Matricula ~ Nome_Disciplina, mean)

alunos_max_media <- alunos_max_media %>% subset(select = -`SEMINÁRIOS.(EDUCAÇÃO.AMBIENTAL)`)

# selecionando apenas os alunos que concluíram o curso
alunos_graduados <- alunos_max_media[complete.cases(alunos_max_media), ]

# data frame de notas em cada cadeira
dados_graduados <- (alunos_graduados[,!(names(alunos_graduados) %in% c("Matricula"))])


###############################################
# Matriz de Similaridade entre as disciplinas #
###############################################

# Função de cálculo de similaridade
getCosine <- function(x,y) {
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# Data frame vazio de disciplina vs. disciplina
correlacao_disciplinas <- matrix(NA, nrow=ncol(dados_graduados),ncol=ncol(dados_graduados),dimnames=list(colnames(dados_graduados),colnames(dados_graduados)))
similaridade_dados_graduados <- as.data.frame(correlacao_disciplinas)

# Calculo da similaridade por disciplina
for(i in 1:ncol(dados_graduados)) {
  for(j in 1:ncol(dados_graduados)) {
    similaridade_dados_graduados[i,j]= getCosine(dados_graduados[i],dados_graduados[j])
  }
}

# Calculando as disciplinas mais similares
vizinhos_dados_graduados <- matrix(NA, nrow=ncol(similaridade_dados_graduados),ncol=11,dimnames=list(colnames(similaridade_dados_graduados)))

for(i in 1:ncol(dados_graduados)) {
  vizinhos_dados_graduados[i,] <- (t(head(n=11,rownames(similaridade_dados_graduados[order(similaridade_dados_graduados[,i],decreasing=TRUE),][i]))))
}


##########################################
# Matriz de Similaridade entre os alunos #
##########################################

# Calculo da nota 
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}

# Matriz alunos vs disciplinas
holder <- matrix(NA, nrow=nrow(alunos_graduados),ncol=ncol(alunos_graduados)-1,dimnames=list((alunos_graduados$Matricula),colnames(alunos_graduados[-1])))
dados.graduados.ibs.similarity

# Loop pelos alunos(linhas)
for(i in 1:nrow(holder)) 
{
  # Loops pelas disciplinas (colunas)
  for(j in 1:ncol(holder)) 
  {
    # Guardando o usuario e a disciplina atual da iteração
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    # Evitar indicar as cadeiras que já foram pagas
    if(as.integer(alunos_graduados[alunos_graduados$Matricula==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # Seleção das cadeiras pagas pelos alunos mais similares
      topN<-((head(n=11,(dados.graduados.ibs.similarity[order(dados.graduados.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Elimina a primeira nota, pois sempre é a mesma
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # Histórico de nota do aluno para as disciplinas do aluno previamente selecionadas
      topN.purchases<- alunos_graduados[,c("Matricula",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$Matricula==user,]
      topN.userPurchases <- (topN.userPurchases[!(names(topN.userPurchases) %in% c("Matricula"))])
      
      # Cálculo da nota do aluno para a respectiva cadeira
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } 
  } 
} 
dados.graduados.user.scores <- holder

# Ajustando as recomendações
dados.graduados.user.scores.holder <- matrix(NA, nrow=nrow(dados.graduados.user.scores),ncol=100,dimnames=list(rownames(dados.graduados.user.scores)))
for(i in 1:nrow(dados.graduados.user.scores)) {
  dados.graduados.user.scores.holder[i,] <- names(head(n=10,(dados.graduados.user.scores[,order(dados.graduados.user.scores[i,],decreasing=TRUE)])[i,]))
}
