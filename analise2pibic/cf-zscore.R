library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(reshape2)
library(caret)
library(tidyr)
library(data.table)
library(recommenderlab)
library(Matrix)
library(stringr)
library(recosystem)

dados_alunos <- read.csv("alunosUFCGAnon.csv") 
dados_aluno_cc <- dados_alunos %>% filter(Cod_Curso == 14102100 & Cod_Evasao == 0 & Tipo == "Obrigatória")
dados_aluno_cc <- dados_aluno_cc %>% mutate(Matricula = factor(Matricula)) %>% arrange(Matricula) %>% 
  select(Matricula, Cod_Disciplina, Nome_Disciplina, Periodo, Creditos, Media_Disciplina, Situacao, Periodo_Ingresso, Periodo_Relativo)
dados_aluno_cc <- dados_aluno_cc %>% group_by(Matricula) %>% mutate(Media = round(mean(Media_Disciplina), digits = 2)) %>% filter(!is.na(Media))
# storing data as factors insures that the modeling functions will treat such data correctly. 
# Factors in R are stored as a vector of integer values with a corresponding set of character values to use when the factor is displayed

# Calulo do CRA
alunos_cra <- dados_aluno_cc %>% 
  mutate(Cra.Crontibute = Media*Creditos) %>%
  mutate(cra = sum(Cra.Crontibute)/sum(Creditos)) %>% 
  select(cra, Periodo)

alunos_max_media <- dados_aluno_cc %>% 
  group_by(Matricula, Media_Disciplina) %>% 
  filter(Media_Disciplina == max(Media_Disciplina)) %>% 
  ungroup() %>%
  group_by(Cod_Disciplina, Periodo) %>%
  mutate(Desvio_Padrao_Turma = round(sd(Media_Disciplina, na.rm = T), digit = 2), Media_Disciplina_Turma = round(mean(Media_Disciplina), digit = 2)) %>%
  select(Matricula, Periodo, Nome_Disciplina, Media_Disciplina, Desvio_Padrao_Turma, Media_Disciplina_Turma) %>% 
  mutate(Nome_Disciplina = as.factor(gsub(" ", ".", Nome_Disciplina))) %>%
  mutate(z_score_Disciplina = (Media_Disciplina - Media_Disciplina_Turma)/Desvio_Padrao_Turma)

names(alunos_max_media)

alunos <- alunos_max_media %>% group_by(Matricula,Nome_Disciplina) %>% 
  select(Matricula,Nome_Disciplina,Media_Disciplina, Desvio_Padrao_Turma, Media_Disciplina_Turma, z_score_Disciplina) %>% 
  group_by(Matricula,Nome_Disciplina) %>% 
  select(Matricula, z_score_Disciplina,Nome_Disciplina)




### collaborative filtering com recommenderlab ###

# ajeitando o data frame para ter apenas numeros
melt_alunos <- alunos %>% melt(id.vars = 1:2) %>% select(-variable)

reshape_alunos <- reshape(melt_alunos, timevar = "value", idvar = "Matricula", direction = "wide")
reshape_colunas<-colnames(reshape_alunos)
reshape_colunas <- reshape_colunas[-1]
colnames(reshape_alunos) <- c(1:46)
head(reshape_alunos)
disciplina_numero <- data.frame(reshape_colunas, c(1:45))
extract<-"z_score_Disciplina.."
disciplina_numero <- disciplina_numero %>% mutate(reshape_colunas = str_extract(disciplina_numero$reshape_colunas, "[^extract]+$"))
colnames(disciplina_numero) <- c("Disciplina", "Código")
head(disciplina_numero)

alunos_cf <- reshape_alunos %>% melt(id.vars = 1)
head(alunos_cf)
colnames(alunos_cf) <- c("Matricula", "Disciplina", "z-score")
alunos_cf<-alunos_cf %>% mutate(Matricula = str_extract(Matricula, "[^B]+$")) 
alunos_cf$Matricula <- as.numeric(alunos_cf$Matricula)
alunos_cf$Disciplina <- as.numeric(alunos_cf$Disciplina)
alunos_cf <- alunos_cf[complete.cases(alunos_cf), ]
head(alunos_cf)

#Represent the data as a real rating matrix
as(as(alunos_cf, "matrix"), "dgCMatrix")
RR.matrix <- as(alunos_cf, "realRatingMatrix")
alunos_rating_matrix<-as(RR.matrix, "data.frame")
colnames(RR.matrix)
head(as(RR.matrix, "list"))

#4. Generate a ccollaborativeommendation model

#Use the method “UBCF”, user based content based filtering:                                         
RS.model <- Recommender(RR.matrix, method="POPULAR")
names(getModel(RS.model))
getModel(RS.model)$topN
getRatingMatrix(RR.matrix)

#5. To display top N items and item affinity Recommended items: 
Recommended.items <- predict(RS.model,  RR.matrix, n=20)
lista<-as(Recommended.items, "matrix")
lista

#Extraindo as melhores recomendações
best.recom <- bestN(Recommended.items, n = 5)
as(best.recom, "list")

#8.  Validation of the model
#Split the data into train and test:
Vmatrix <- evaluationScheme(RR.matrix, method="split", train=0.9, given=1)
#Generate the recommendation model:
RM <- Recommender(getData(Vmatrix,"train"), "POPULAR")
#Making predictions on test data
test <- predict( RM, getData(Vmatrix, "known"), type="ratings")
#Obtain the error matrix
error <- calcPredictionAccuracy(test, getData(Vmatrix,"unknown"))
error
# FONTE: https://www.linkedin.com/pulse/create-recommendation-engine-using-r-simple-steps-minta-thomas


### cf usando recosystem ###
set.seed(123) # This is a randomized algorithm
train_set = data_file(system.file("dat", "smalltrain.txt", package = "recosystem"))
test_set  = data_file(system.file("dat", "smalltest.txt",  package = "recosystem"))
r = Reco()
opts = r$tune(train_set, opts = list(dim = c(29000), lrate = c(0.1, 0.5)))
opts
r$train(train_set, opts = list(costp_l1 = 0, costq_l1 = 0,
                           lrate = c(0.2), niter = 50, nthread = 2, verbose = T))
# com non-negative matrix factorization os erros aumentaram 

# print(scan(pred_file, n = 10))
pred_rvec = r$predict(test_set, out_memory())
pred_rvec
head(pred_rvec, 10)
