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
dados.graduados.ibs <- (alunos_graduados[,!(names(alunos_graduados) %in% c("Matricula"))])


###############################################
# Matriz de Similaridade entre as disciplinas #
###############################################

# Função de cálculo de similaridade
getCosine <- function(x,y) {
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# Data frame vazio de disciplina vs. disciplina
holder <- matrix(NA, nrow=ncol(dados.graduados.ibs),ncol=ncol(dados.graduados.ibs),dimnames=list(colnames(dados.graduados.ibs),colnames(dados.graduados.ibs)))
dados.graduados.ibs.similarity <- as.data.frame(holder)

# Calculo da similaridade por disciplina
for(i in 1:ncol(dados.graduados.ibs)) {
  for(j in 1:ncol(dados.graduados.ibs)) {
    dados.graduados.ibs.similarity[i,j]= getCosine(dados.graduados.ibs[i],dados.graduados.ibs[j])
  }
}

# Calculando as disciplinas mais similares
dados.graduados.neighbours <- matrix(NA, nrow=ncol(dados.graduados.ibs.similarity),ncol=11,dimnames=list(colnames(dados.graduados.ibs.similarity)))

for(i in 1:ncol(dados.graduados.ibs)) {
  dados.graduados.neighbours[i,] <- (t(head(n=11,rownames(dados.graduados.ibs.similarity[order(dados.graduados.ibs.similarity[,i],decreasing=TRUE),][i]))))
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

# A placeholder matrix
holder <- matrix(NA, nrow=nrow(alunos_graduados),ncol=ncol(alunos_graduados)-1,dimnames=list((alunos_graduados$Matricula),colnames(alunos_graduados[-1])))
dados.graduados.ibs.similarity
# Loop through the users (rows)
for(i in 1:nrow(holder)) 
{
  # Loops through the products (columns)
  for(j in 1:ncol(holder)) 
  {
    # Get the user's name and th product's name
    # We do this not to conform with vectors sorted differently 
    user <- rownames(holder)[i]
    product <- colnames(holder)[j]
    
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store an empty string
    if(as.integer(alunos_graduados[alunos_graduados$Matricula==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(dados.graduados.ibs.similarity[order(dados.graduados.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Drop the first one because it will always be the same song
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # We then get the user's purchase history for those 10 items
      topN.purchases<- alunos_graduados[,c("Matricula",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$Matricula==user,]
      topN.userPurchases <- (topN.userPurchases[!(names(topN.userPurchases) %in% c("Matricula"))])
      topN
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } # close else statement
  } # end product for loop   
} # end user for loop
dados.graduados.user.scores <- holder

# Lets make our recommendations pretty
dados.graduados.user.scores.holder <- matrix(NA, nrow=nrow(dados.graduados.user.scores),ncol=100,dimnames=list(rownames(dados.graduados.user.scores)))
for(i in 1:nrow(dados.graduados.user.scores)) 
{
  dados.graduados.user.scores.holder[i,] <- names(head(n=10,(dados.graduados.user.scores[,order(dados.graduados.user.scores[i,],decreasing=TRUE)])[i,]))
}
