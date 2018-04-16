library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(reshape2)
library(caret)
library(hydroGOF)


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
alunos_max_media <- dados_aluno_cc %>% group_by(Matricula, Nome_Disciplina) %>% filter(Media_Disciplina == max(Media_Disciplina)) %>% ungroup() %>%
  select(Nome_Disciplina, Matricula, Media_Disciplina) %>% mutate(Nome_Disciplina = as.factor(gsub(" ", ".", Nome_Disciplina))) %>%
  dcast(Matricula ~ Nome_Disciplina, mean) %>% merge(alunos_cra)
head(alunos_max_media)

# adicionando coluna de período de ingresso
alunos_max_media <- bind_cols(alunos_max_media,distinct(dados_aluno_cc %>% select(Matricula, Periodo_Ingresso))%>% select(Periodo_Ingresso)) %>% 
  select(-Matricula1) %>% na.omit()

# selecionando apenas os alunos que concluíram o curso
alunos_graduados <- alunos_max_media[complete.cases(alunos_max_media), ]
head(alunos_graduados)

# selecionando as cadeiras por periodo
PRIMEIRO_PER <- list("CALCULO.DIFERENCIAL.E.INTEGRAL.I","ÁLGEBRA.VETORIAL.E.GEOMETRIA.ANALÍTICA","PROGRAMAÇÃO.I",
                     "LABORATÓRIO.DE.PROGRAMAÇÃO.I","INTRODUÇÃO.A.COMPUTAÇÃO","LEITURA.E.PRODUCAO.DE.TEXTOS")
SEGUNDO_PER <- list("METODOLOGIA.CIENTÍFICA", "FUNDAMENTOS.DE.FÍSICA.CLÁSSICA", "CALCULO.DIFERENCIAL.E.INTEGRAL.II",
                    "MATEMÁTICA.DISCRETA", "PROGRAMAÇÃO.II", "TEORIA.DOS.GRAFOS","LABORATÓRIO.DE.PROGRAMAÇÃO.II")
TERCEIRO_PER <- list("ALGEBRA.LINEAR.I", "FUNDAMENTOS.DE.FÍSICA.MODERNA", "TEORIA.DA.COMPUTAÇÃO", "ESTRUTURA.DE.DADOS.E.ALGORITMOS",
                     "GERÊNCIA.DA.INFORMAÇÃO", "LAB.DE.ESTRUTURA.DE.DADOS.E.ALGORITMOS", "PROBABILIDADE.E.ESTATISTICA")
QUARTO_PER <- list("METODOS.ESTATISTICOS","PARADIGMAS.DE.LING..DE.PROGRAMAÇÃO", "LÓGICA.MATEMÁTICA", "ORG.E.ARQUITETURA.DE.COMPUTADORES.I", 
                   "ENGENHARIA.DE.SOFTWARE.I","SISTEMAS.DE.INFORMAÇÃO.I","LAB.DE.ORG.E.ARQUITETURA.DE.COMPUTADORES")
QUINTO_PER <- list("INFORMÁTICA.E.SOCIEDADE", "LABORATÓRIO.DE.ENGENHARIA.DE.SOFTWARE", "ANÁLISE.E.TÉCNICA.DE.ALGORITMOS",
                   "COMPILADORES", "REDES.DE.COMPUTADORES", "BANCO.DE.DADOS.I", "SISTEMAS.DE.INFORMAÇÃO.II")
SEXTO_PER <- list("DIREITO.E.CIDADANIA", "LAB.DE.INTERCON.DE.REDES.DE.COMPUTADORES", "INTERCONEXÃO.DE.REDES.DE.COMPUTADORES",
                  "SISTEMAS.OPERACIONAIS", "BANCO.DE.DADOS.II","INTELIGENCIA.ARTIFICIAL.I")
SETIMO_PER <- list("PROJETO.EM.COMPUTAÇÃO.I", "MÉTODOS.E.SOFTWARE.NUMÉRICOS", "AVAL.DE.DESEMPENHO.DE.SISTEMAS.DISCRETOS")
OITAVO_PER <- list("PROJETO.EM.COMPUTAÇÃO.II")

# criando data frame com todas as cadeiras por periodo
lista_periodos <- list(PRIMEIRO_PER, SEGUNDO_PER, TERCEIRO_PER, QUARTO_PER, QUINTO_PER,SEXTO_PER, SETIMO_PER, OITAVO_PER)

lista_periodos

## /// FUNCOES E CONSTANTES \\\##
# numero minimo de vizinhos
K = 3
# porcentagem minima para ser considerado vizinho
NEIGH = 0.7
# alunos na amostra de teste
N_ALUNOS = 4
# numero maximo de cadeiras por periodo
N_DISC = 7

# função de calculo de similaridades dos alunos através do data frame de notas
get_sim <- function(df) {
  
  # retira a coluna matricula, colocando-a como o nome da linha
  row.names(df) <- df$Matricula
  df <- df %>% subset(select=-c(Matricula))
  
  # transposta do data frame para poder passar para a função de correlação
  inv_df <- as.data.frame(t(df))
  
  res <- cor(inv_df[sapply(inv_df, is.numeric)], use="p", method='pearson')
  
  # retorna um data frame de correlação 
  return(res);
}

# calcula os vizinhos de um determinado aluno a patir de sua correlação com os outros
# retorna um vector de Named num, cujo nome é a matrícula e o valor a similaridade
get_neigh <- function(df, index, corr) {
  
  # pegando a matricula do aluno
  matr <- df[index, 1]
  
  # todos os vizinhos, porém temos que "invalidar" ele mesmo
  corr[as.double(matr), as.double(matr)] = 0
  all_neigh <- corr[matr, ]
  
  # organizando os vizinhos pela correlação e pegando os K mais proximos
  k_neigh <- sort(all_neigh, decreasing = T)[1:K]
  
  return(k_neigh);
}

# calcula score(nota) ignorando vizinhos com NAs
get_score <- function(df, k_neigh, item) {
  
  # pega as notas dos vizinhos do aluno
  notas <- subset(df, Matricula %in% names(k_neigh))
  
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
  for(i in 1:length(notas$Matricula)) {
    notas$sim[i] <- k_neigh[as.character(notas$Matricula[i])] 
  }
  
  # se todas as notas dos vizinhos forem NAs ou nenhum vizinho com
  # nota tenha similaridade > NEIGH consideramos que esse aluno
  # não tem vizinhos
  eh_valido <- notas[notas$sim > NEIGH,]
  if(nrow(eh_valido) == 0) {
    return(NA)
  }
  
  res <- sum(notas[, item] * notas$sim) / sum(notas$sim)
  return(res)
}

# separando em teste e treino
temp <- createDataPartition(alunos_graduados$Periodo_Ingresso, p = 0.90, list = F)
# data frame do erro de todos os periodos
rmse_periodo <- as.data.frame(matrix(NA, nrow = 7))


#### // PREDIZENDO NOTA PARA TODOS OS ALUNOS \\ ####

# transformando a matricula em valores numericos e dispensando se CRA
dados_treino2 <- alunos_graduados %>% mutate(Matricula = 1:59) %>% select(-cra, - Periodo_Ingresso)
head(dados_treino2)

#data frame onde serão guardadas as predições e a media delas
predicao_geral <- as.data.frame(matrix(ncol = 7))
media_rmse <- c(NA)
predicao_geral <- cbind(predicao_geral, as.data.frame(media_rmse))

# o valor real ao qual  as predições deve corresponder
resultados2 <- dados_treino2[-temp,]
resultados2

## Realizando predição
# calcula para todas as cadeiras por periodo
for(disciplinas_periodo in 2:length(lista_periodos)){
  # cadeiras cursadas até o periodo 'atual' que serao usadas no calculo
  indices_da_vez <- unlist(lista_periodos[1:disciplinas_periodo])
  # seleção dos valores usados para treino para essa iteração
  treino_valores2 <-  dados_treino2 %>% select(Matricula,indices_da_vez) %>% na.omit()

  # cadeiras para as quais as notas devem ser preditas  
  cadeiras_atuais <- unlist(lista_periodos[disciplinas_periodo])
  
  # omitindo os valores das notas de teste
  for(l in 1:lengths(lista_periodos[disciplinas_periodo])) {
    treino_valores2[-temp,][cadeiras_atuais[l]] <- NA
  }
  
  # definição do valor de teste e treino da iteração
  teste_valores2 <- treino_valores2[-temp,]
  teste_indices2 <- rownames(teste_valores2)
  
  # calculando a correlacao entre os alunos que jah pagaram a cadeira e os que nós queremos calcular
  corr <- treino_valores2 %>% get_sim()

  # calculo para cada aluno
  for(matricula_aluno in 1:length(teste_indices2)) {
    # selecionando as notas do aluno atual
    index <- teste_indices2[matricula_aluno]
    # calculo dos seus K vizinhos mais proximos
    k_proximos <- get_neigh(teste_valores2, index, corr)

    # calculando a nota para cada disiciplina
    for(j in 1:length(cadeiras_atuais)) {
      pred <- get_score(treino_valores2[, c("Matricula", cadeiras_atuais[j])],
                        k_proximos, cadeiras_atuais[j])
      teste_valores2[index, cadeiras_atuais[j]] <- pred
    }
  }

  # simplificando os dados
  dados_reais2 <- alunos_graduados[-temp,cadeiras_atuais]
  # atualizando os dados da predição
  predicao2 <- teste_valores2[, cadeiras_atuais]
  
  # conversão para quando dados_reais e predicao foram vetores e nao data.frames, a fim de se evitar erro no for seguinte
  if(class(predicao2) == "numeric"){
    predicao2 <- as.data.frame(predicao2)
  }
  if(class(dados_reais2) == "numeric"){
    dados_reais2 <- as.data.frame(dados_reais2)
  }
  
  # data frame de calculo do erro de acordo com o numeor de alunos e disiciplinas na iteração para a predição
  erro_rmse <- as.data.frame(matrix(0, nrow = N_ALUNOS, ncol = N_DISC))

  # calculo do RMSE por aluno
  for(aluno in 1:(N_ALUNOS)){
    # calculo do RMSE por cadeira
    for(cadeira in 1:length(cadeiras_atuais)) {
      erro_rmse[aluno,cadeira] <- (rmse(sim=predicao2[aluno,cadeira], obs=dados_reais2[aluno,cadeira]))
    }
  }
  
  # calculando a media do erro
  erro_rmse <- as.data.frame(erro_rmse) %>% 
  mutate(media_rmse = rowMeans(as.data.frame(erro_rmse)[,1:7])) 
  
  # calculando a media do erro das disciplinas do periodo da iteração
  rmse_periodo[disciplinas_periodo,] <- mean(as.data.frame(erro_rmse %>% select(media_rmse) %>% 
  na.omit())$media_rmse)
  
  # data frame de predição e seu respectivo erro
  predicao_geral <- rbind(predicao_geral, erro_rmse)
  
}

# retirando os NA dos data frames
predicao_geral <- predicao_geral %>% na.omit()
rmse_periodo <- rmse_periodo %>% select(V1) %>% na.omit()
head(rmse_periodo)

# vetor dos periodos preditos
periodo = c(2:8)
head(predicao_geral)

# geração do grefico de erro
rmse_periodo <- rmse_periodo %>% rename(media = V1)
rmse_periodo %>% ggplot(aes(periodo, media)) + geom_line()
rmse_periodo

  