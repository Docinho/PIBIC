library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(reshape2)
library(caret)
library(mlbench)
library(hydroGOF)

dados_alunos <- read.csv("alunosUFCGAnon.csv") 
dados_aluno_cc <- dados_alunos %>% filter(Cod_Curso == 14102100 & Cod_Evasao == 0 & Tipo == "Obrigatória")
dados_aluno_cc <- dados_aluno_cc %>% mutate(Matricula = factor(Matricula)) %>% arrange(Matricula) %>% 
  select(Matricula, Cod_Disciplina, Nome_Disciplina, Periodo, Creditos, Media_Disciplina, Situacao, Periodo_Ingresso, Periodo_Relativo)
dados_aluno_cc <- dados_aluno_cc %>% group_by(Matricula) %>% mutate(Media = round(mean(Media_Disciplina), digits = 2)) %>% filter(!is.na(Media))
# storing data as factors insures that the modeling functions will treat such data correctly. 
# Factors in R are stored as a vector of integer values with a corresponding set of character values to use when the factor is displayed

# Calulo do CRA
alunos_cra <- dados_aluno_cc %>% mutate(Cra.Crontibute = Media*Creditos) %>% summarise(cra = sum(Cra.Crontibute)/sum(Creditos))
alunos_max_media <- dados_aluno_cc %>% group_by(Matricula, Media_Disciplina) %>% filter(Media_Disciplina == max(Media_Disciplina)) %>% ungroup() %>%
  select(Nome_Disciplina, Matricula, Media_Disciplina) %>% mutate(Nome_Disciplina = as.factor(gsub(" ", ".", Nome_Disciplina))) %>%
  dcast(Matricula ~ Nome_Disciplina, mean) %>% merge(alunos_cra)
head(alunos_max_media)
alunos_max_media <- bind_cols(alunos_max_media,distinct(dados_aluno_cc %>% select(Matricula, Periodo_Ingresso))%>% select(Periodo_Ingresso)) %>% select(-Matricula1)
alunos_graduados <- alunos_max_media[complete.cases(alunos_max_media), ]

##Organizando os data frames principais
# separando alunos por periodo 
primeiro_periodo <- alunos_max_media %>% select(Matricula, cra, CALCULO.DIFERENCIAL.E.INTEGRAL.I, ÁLGEBRA.VETORIAL.E.GEOMETRIA.ANALÍTICA, PROGRAMAÇÃO.I, 
                                                LABORATÓRIO.DE.PROGRAMAÇÃO.I, INTRODUÇÃO.A.COMPUTAÇÃO, LEITURA.E.PRODUCAO.DE.TEXTOS, Periodo_Ingresso) %>%
  na.omit(primeiro_periodo) %>% arrange(Matricula) %>%
  rename(matricula = Matricula, cra = cra,calculo1 = CALCULO.DIFERENCIAL.E.INTEGRAL.I, vetorial = ÁLGEBRA.VETORIAL.E.GEOMETRIA.ANALÍTICA, p1 = PROGRAMAÇÃO.I,
         lp1 = LABORATÓRIO.DE.PROGRAMAÇÃO.I, ic = INTRODUÇÃO.A.COMPUTAÇÃO, lpt = LEITURA.E.PRODUCAO.DE.TEXTOS)
head(primeiro_periodo)

segundo_periodo <- alunos_max_media %>% 
  select(Matricula,cra, CALCULO.DIFERENCIAL.E.INTEGRAL.II, FUNDAMENTOS.DE.FÍSICA.CLÁSSICA, TEORIA.DOS.GRAFOS, PROGRAMAÇÃO.II,
         LABORATÓRIO.DE.PROGRAMAÇÃO.II, MATEMÁTICA.DISCRETA) %>% na.omit() %>% 
  arrange(Matricula) %>%
  rename(matricula = Matricula, cra = cra,calculo2 = CALCULO.DIFERENCIAL.E.INTEGRAL.II, classica = FUNDAMENTOS.DE.FÍSICA.CLÁSSICA,
         grafos = TEORIA.DOS.GRAFOS, p2 = PROGRAMAÇÃO.II, lp2 = LABORATÓRIO.DE.PROGRAMAÇÃO.II, discreta = MATEMÁTICA.DISCRETA)
head(segundo_periodo)

terceiro_periodo <- alunos_max_media %>%
  select(Matricula,cra, ESTRUTURA.DE.DADOS.E.ALGORITMOS, LAB.DE.ESTRUTURA.DE.DADOS.E.ALGORITMOS, FUNDAMENTOS.DE.FÍSICA.MODERNA, 
         ALGEBRA.LINEAR.I,PROBABILIDADE.E.ESTATISTICA, TEORIA.DA.COMPUTAÇÃO, GERÊNCIA.DA.INFORMAÇÃO) %>% 
  na.omit() %>% 
  arrange(Matricula) %>%
  rename(matricula = Matricula, eda = ESTRUTURA.DE.DADOS.E.ALGORITMOS, leda =  LAB.DE.ESTRUTURA.DE.DADOS.E.ALGORITMOS, moderna =  FUNDAMENTOS.DE.FÍSICA.MODERNA, 
         linear = ALGEBRA.LINEAR.I, prob = PROBABILIDADE.E.ESTATISTICA, tc = TEORIA.DA.COMPUTAÇÃO, gi = GERÊNCIA.DA.INFORMAÇÃO)
head(terceiro_periodo)

quarto_periodo <- alunos_max_media %>% select(Matricula, cra, PARADIGMAS.DE.LING..DE.PROGRAMAÇÃO, METODOS.ESTATISTICOS, ORG.E.ARQUITETURA.DE.COMPUTADORES.I, 
                                              LAB.DE.ORG.E.ARQUITETURA.DE.COMPUTADORES, LÓGICA.MATEMÁTICA, ENGENHARIA.DE.SOFTWARE.I, SISTEMAS.DE.INFORMAÇÃO.I) %>%
  na.omit() %>%
  arrange(Matricula)%>% 
  rename(matricula = Matricula, plp = PARADIGMAS.DE.LING..DE.PROGRAMAÇÃO, metodos = METODOS.ESTATISTICOS, oac = ORG.E.ARQUITETURA.DE.COMPUTADORES.I, 
         loac = LAB.DE.ORG.E.ARQUITETURA.DE.COMPUTADORES, logica = LÓGICA.MATEMÁTICA, es = ENGENHARIA.DE.SOFTWARE.I, si1 = SISTEMAS.DE.INFORMAÇÃO.I)
head(quarto_periodo)

# Unindo os periodos
primeiro_segundo_periodos <- merge(primeiro_periodo, segundo_periodo)
head(primeiro_segundo_periodos)

# Primeiro, segundo e terceiro periodo
primeiro_a_terceiro_periodo <- merge(primeiro_segundo_periodos, terceiro_periodo)
periodos_dados <- merge(primeiro_a_terceiro_periodo, quarto_periodo) %>% select(matricula, everything())
head(periodos_dados)

COL_QUARTO_PER <- colnames(quarto_periodo %>% select(-cra, -matricula))
COL_QUARTO_PER

## /// FUNCOES E CONSTANTES \\\##
K= 10
NEIGH = 0.7

get_sim <- function(df) {
  
  row.names(df) <- df$matricula
  df <- df %>% subset(select=-c(matricula))
  
  inv_df <- as.data.frame(t(df))
  
  res <- cor(inv_df[sapply(inv_df, is.numeric)], use="p", method='pearson')
  return(res);
}

# retorna um vector de Named num, cujo nome é a matrícula e o valor a similaridade
get_neigh <- function(df, index, corr) {
  
  matr <- (df[index, 1])
  
  # todos os vizinhos, porém temos que "invalidar" ele mesmo
  corr[as.double(matr), as.double(matr)] = 0
  all_neigh <- corr[matr, ]

  k_neigh <- sort(all_neigh, decreasing = T)[1:K]

  return(k_neigh);
}

# calcula score ignorando vizinhos com NAs
get_score <- function(df, k_neigh, item) {
  
  notas <- subset(df, matricula %in% names(k_neigh))
  
  # removendo vizinhos que não possuem notas
  notas <- na.omit(notas) 

  # se todas as notas dos vizinhos forem NAs ou nenhum vizinho com
  # nota tenha similaridade > NEIGH consideramos que esse aluno
  # não tem vizinhos
  if(nrow(notas) == 0) {
    return(NA)
  }
  
  # atualizando similaridade
  notas$sim <- 0
  for(i in 1:length(notas$matricula)) {
    notas$sim[i] <- k_neigh[as.character(notas$matricula[i])] 
  }
  
  # se todas as notas dos vizinhos forem NAs ou nenhum vizinho com
  # nota tenha similaridade > NEIGH consideramos que esse aluno
  # não tem vizinhos
  eh_valido <- notas[notas$sim > NEIGH,]
  if(nrow(eh_valido) == 0) {
    return(NA)
  }
  
  # print(item)
  # print(notas[, item])
  # print(sum(notas[, item] * notas$sim) / sum(notas$sim))
  
  res <- sum(notas[, item] * notas$sim) / sum(notas$sim)
  return(res)
}

## // CONSTURINDO OS DATA FRAMES E COLLABORATIVE FILTERING \\##

# separando em teste e treino
temp <- createDataPartition(periodos_dados$Periodo_Ingresso, p = 0.95, list = F)

# os dados de teste são zerados, copiados para uma nova tabela
# zerando
treino <- periodos_dados
treino
for(i in 1:length(COL_QUARTO_PER)) {
  treino[-temp,][COL_QUARTO_PER[i]] <- NA
}

 # atribuindo valores numeros as matriculas
matricula_2 = c(1:121)
matricula_2
treino <- cbind(treino,matricula_2)
treino <- treino %>% select(-matricula) %>% select(matricula_2, everything()) %>% rename(matricula = matricula_2)
head(treino)
# copiando
teste_valores <- treino[-temp, ]
teste_indices <- rownames(teste_valores)
teste_indices
## Realizando predição


# calcula a similaridade entre todos os alunos (de todos para todos) 
corr <- treino %>% get_sim()
head(corr)


# calcula predição: média ponderada dos K vizinhos mais próximos
for(i in 1:length(teste_indices)) {
  
  index <- teste_indices[i]
  k_proximos <- get_neigh(teste, index, corr)
  
  for(j in 1:length(COL_QUARTO_PER)) {
    pred <- get_score(treino[, c("matricula", COL_QUARTO_PER[j])],
                      k_proximos, COL_QUARTO_PER[j])
    teste_valores[index, COL_QUARTO_PER[j]] <- pred
  }
}

# simplificando os dados
dados_reais <- periodos_dados[-temp, COL_QUARTO_PER]
predicao <- teste_valores[, COL_QUARTO_PER]
dados_reais
predicao
# número de elementos em cada coluna de teste = 5
total_alunos <- sapply(predicao, function(x) length(x))

#foi prossivel predizer para todos os alunos
# número de NA em cada coluna, ou seja número de alunos sem predição por disciplina
sem_predicao <- sapply(predicao, function(x) sum(is.na(x)))

# porcentagem de alunos sem predição por disciplina
(sem_predicao/total_alunos) * 100

# porcentagem total de variáveis sem predição
(sum(sem_predicao)/sum(total_alunos)) * 100

# os 5 alunos que aparecem nas cadeiras do quarto periodo com NA são os ultilizados no teste
sapply(teste, function(x) sum(is.na(x)))

rmse(sim=predicao, obs=dados_reais)


#### // PREDIZENDO NOTA PARA TODOS OS ALUNOS \\ ####

teste2 <- periodos_dados %>% bind_cols(matricula_2 = c(1:121)) %>% select(-matricula) %>% select(matricula_2, everything()) %>% rename(matricula = matricula_2)
teste_valores2 <- periodos_dados %>% mutate(plp = NA, si1 = NA, logica = NA, oac = NA, loac = NA, es = NA, metodos = NA)
# teste para todos os alunos
teste_indices2 <- rownames(teste2)

## Realizando predição

# calcula predição: média ponderada dos K vizinhos mais próximos
for(i in 1:length(teste_indices2)) {
  
  index <- teste_indices2[i]
  k_proximos <- get_neigh(teste2, index, corr)
  
  for(j in 1:length(COL_QUARTO_PER)) {
    pred <- get_score(teste2[, c("matricula", COL_QUARTO_PER[j])],
                      k_proximos, COL_QUARTO_PER[j])
    teste_valores2[index, COL_QUARTO_PER[j]] <- pred
  }
}

# simplificando os dados
dados_reais2 <- periodos_dados[, COL_QUARTO_PER]
predicao2 <- teste_valores2[, COL_QUARTO_PER]
head(dados_reais2)
head(predicao2)
erro_rmse <- matrix(nrow = nrow(periodos_dados), ncol = length(COL_QUARTO_PER))
colnames(erro_rmse) <- COL_QUARTO_PER
rownames(erro_rmse) <-(periodos_dados$matricula)

for(i in 1:length(teste_indices2)){
  for(j in 1:7) {
  erro_rmse[i,j] <- (rmse(sim=predicao2[i,j], obs=dados_reais2[i,j]))
  }
}
matriculas <- periodos_dados %>% select(matricula)
erro_rmse <- as.data.frame(erro_rmse) %>% mutate(media_rmse = rowMeans(as.data.frame(erro_rmse)[,1:7])) 
erro_rmse <-bind_cols(as.data.frame(erro_rmse),matriculas) %>% select(matricula, everything()) 
head(erro_rmse)

periodos_dados %>% filter(matricula == "B818") %>% select(plp:si1)
predicao2[110, ]%>% select(plp:si1)
