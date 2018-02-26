library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(reshape2)
library(caret)
library(mlbench)
library(grid)

setwd("../")
dados_alunos <- read.csv("alunosUFCGAnon.csv") 
dados_aluno_cc <- dados_alunos %>% filter(Cod_Curso == 14102100 & Cod_Evasao == 0 & Tipo == "Obrigatória")
dados_aluno_cc <- dados_aluno_cc %>% mutate(Matricula = factor(Matricula)) %>% arrange(Matricula) %>% 
  select(Matricula, Cod_Disciplina, Nome_Disciplina, Periodo, Creditos, Media_Disciplina, Situacao, Periodo_Ingresso, Periodo_Relativo)
dados_aluno_cc <- dados_aluno_cc %>% group_by(Matricula) %>% mutate(Media = round(mean(Media_Disciplina), digits = 2)) %>% filter(!is.na(Media))

# Calulo do CRA
alunos_cra <- dados_aluno_cc %>% mutate(Cra.Crontibute = Media*Creditos) %>% summarise(cra = sum(Cra.Crontibute)/sum(Creditos))
alunos_max_media <- dados_aluno_cc %>% group_by(Matricula, Media_Disciplina) %>% filter(Media_Disciplina == max(Media_Disciplina)) %>% ungroup() %>%
  select(Nome_Disciplina, Matricula, Media_Disciplina) %>% mutate(Nome_Disciplina = as.factor(gsub(" ", ".", Nome_Disciplina))) %>%
  dcast(Matricula ~ Nome_Disciplina, mean) %>% merge(alunos_cra)
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

## Divindo em arquivo de teste e treino
COL_QUARTO_PER <- colnames(quarto_periodo %>% select(-cra, -matricula))
temp <- createDataPartition(periodos_dados$Periodo_Ingresso, p = 0.95, list = F)

# os dados de teste são zerados, copiados para uma nova tabela
# zerando
teste <- periodos_dados

resultados_geral <-teste[-temp,] 
resultados <-teste[-temp, COL_QUARTO_PER] 
resultados

for(i in 1:length(COL_QUARTO_PER)) {
  teste[-temp,][COL_QUARTO_PER[i]] <- NA
}

# atribuindo valores numeros as matriculas
teste <- teste %>% bind_cols(matricula_2 = c(1:121)) %>% select(-matricula) %>% select(matricula_2, everything()) %>% rename(matricula = matricula_2)
head(teste)

# copiando
teste_valores <- teste[-temp, ]
teste_valores
teste_indices <- rownames(teste_valores)

corr_cadeiras <- (periodos_dados %>% select(-matricula, -Periodo_Ingresso) %>% cor())
corrplot(corr_cadeiras)
corr_cadeiras <- as.data.frame(corr_cadeiras)
# fazendo predicao para cada cadeira do quarto periodo
# oac
analise_oac <- corr_cadeiras %>% select(oac) 
ranking_oac <- cbind(analise_oac, as.data.frame(rownames(analise_oac))) 
ranking_oac <- top_n(ranking_oac, 12,oac) %>% rename(cadeiras = "rownames(analise_oac)")
ranking_oac %>% select(cadeiras)

modelo_oac <- lm(oac ~ cra + ic + p2 + discreta + linear + prob + metodos + logica + es + si1, data = teste)
summary(modelo_oac)
plot(modelo_oac, which = 1:2)

# loac
analise_loac <- corr_cadeiras %>% select(loac) 
ranking_loac <- cbind(analise_loac, as.data.frame(rownames(analise_loac))) 
ranking_loac <- top_n(ranking_loac, 12, loac) %>% rename(cadeiras = "rownames(analise_loac)")
ranking_loac %>% select(cadeiras)

modelo_loac <- lm(loac ~ cra + ic + p2 + lp2 + leda + linear + metodos + logica + es + si1, data = teste)
summary(modelo_loac)
plot(modelo_oac, which = 1:2)

# metodos
analise_metodos <- corr_cadeiras %>% select(metodos) 
ranking_metodos <- cbind(analise_metodos, as.data.frame(rownames(analise_metodos))) 
ranking_metodos <- top_n(ranking_metodos, 11, metodos) %>% rename(cadeiras = "rownames(analise_metodos)")
ranking_metodos %>% select(cadeiras)

modelo_metodos <- lm(metodos ~ cra + ic + lpt + grafos + p2 + linear + prob + plp + logica + oac, data = teste)
summary(modelo_metodos)
plot(modelo_metodos, which = 1:2)

# logica
analise_logica <- corr_cadeiras %>% select(logica) 
ranking_logica <- cbind(analise_logica, as.data.frame(rownames(analise_logica))) 
ranking_logica <- top_n(ranking_logica, 11, logica) %>% rename(cadeiras = "rownames(analise_logica)")
ranking_logica %>% select(cadeiras)

modelo_logica <- lm(logica ~ cra + p2 + linear + plp + leda + oac + prob + metodos + si1 + es, data = teste)
summary(modelo_logica)
plot(modelo_logica, which = 1:2)

# es
analise_es <- corr_cadeiras %>% select(es) 
ranking_es <- cbind(analise_es, as.data.frame(rownames(analise_es))) 
ranking_es <- top_n(ranking_es, 11, es) %>% rename(cadeiras = "rownames(analise_es)")
ranking_es %>% select(cadeiras)

modelo_es <- lm(es ~ cra + p2 + loac + plp + leda + oac + prob + metodos + si1 + logica, data = teste)
summary(modelo_es)
plot(modelo_es, which = 1:2)

# si1
analise_si1 <- corr_cadeiras %>% select(si1) 
ranking_si1 <- cbind(analise_si1, as.data.frame(rownames(analise_si1))) 
ranking_si1 <- top_n(ranking_si1, 11, si1) %>% rename(cadeiras = "rownames(analise_si1)")
ranking_si1 %>% select(cadeiras)

modelo_si1 <- lm(si1 ~ cra + p2 + leda + linear + gi + oac + prob + es + loac + logica, data = teste)
summary(modelo_si1)
plot(modelo_si1, which = 1:2)

# plp
analise_plp <- corr_cadeiras %>% select(plp) 
ranking_plp <- cbind(analise_plp, as.data.frame(rownames(analise_plp))) 
ranking_plp <- top_n(ranking_plp, 11, plp) %>% rename(cadeiras = "rownames(analise_plp)")
ranking_plp %>% select(cadeiras)

modelo_plp <- lm(si1 ~ cra + ic + discreta + eda + leda + prob + tc + metodos + es + logica, data = teste)
summary(modelo_plp)
plot(modelo_plp, which = 1:2)

# predição
RMSE <- function(predicted, true) mean((predicted-true)^2)^.5

#oac
teste_oac <- resultados_geral %>% select(-matricula, -Periodo_Ingresso, -calculo1, -vetorial, -p1, -lp1, -lpt, -calculo2, -classica, -grafos, -lp2, -eda, -leda, -moderna, -tc, -gi, -plp, -oac)
teste_oac
resultados$oac
predict(modelo_oac, teste_oac)
RMSE(predict(modelo_oac, teste_oac), resultados$oac)

#loac
ranking_loac$cadeiras
atributos_desnecessarios <- setdiff(colnames(resultados_geral), ranking_loac$cadeiras)
atributos_desnecessarios
teste_loac <- resultados_geral %>% select(-matricula,-calculo1,-vetorial,-p1, -lp1,-lpt,-Periodo_Ingresso, -calculo2,-classica,-grafos,-discreta,-eda,-moderna,-prob,-tc,-gi,-plp)
predict(modelo_loac, teste_loac)
resultados_geral$loac
RMSE(predict(modelo_loac, teste_loac), resultados$loac)

#metodos
ranking_metodos$cadeiras
atributos_desnecessarios <- setdiff(colnames(resultados_geral), ranking_metodos$cadeiras)
atributos_desnecessarios
teste_metodos <- resultados_geral%>% select(-matricula,-calculo1,-vetorial,-p1, -lp1,-Periodo_Ingresso, -calculo2,-classica,-discreta,-lp2, -eda, -leda, -moderna,-tc,-gi,-si1, -metodos)
teste_metodos
modelo_metodos
predict(modelo_metodos, teste_metodos)
resultados_geral$metodos
RMSE(predict(modelo_metodos, teste_metodos), resultados$metodos)

#plp
ranking_plp$cadeiras
atributos_desnecessarios <- setdiff(colnames(resultados_geral), ranking_plp$cadeiras)
atributos_desnecessarios
resultados_geral
teste_plp <- resultados_geral%>% select(-matricula,-calculo1,-vetorial,-p1, -lp1, -lpt, -Periodo_Ingresso, -calculo2,-classica,-grafos, -p2, -lp2,-moderna,-linear, -gi,-oac, -loac, -si1)
predict(modelo_plp, teste_plp)
resultados_geral$plp
RMSE(predict(modelo_plp, teste_plp), resultados$plp)

#logica
ranking_logica$cadeiras
atributos_desnecessarios <- setdiff(colnames(resultados_geral), ranking_logica$cadeiras)
atributos_desnecessarios
resultados_geral
teste_logica <- resultados_geral%>% select(-matricula,-calculo1,-vetorial,-p1, -lp1, -Periodo_Ingresso, -ic, -lpt, -calculo2,-grafos, -classica, - discreta, -lp2,-moderna, -eda, - tc, -gi, -loac)
teste_metodos
predict(modelo_logica, teste_logica)
resultados_geral$logica
RMSE(predict(modelo_logica, teste_logica), resultados$logica)

#es
ranking_es$cadeiras
atributos_desnecessarios <- setdiff(colnames(resultados_geral), ranking_es$cadeiras)
atributos_desnecessarios
resultados_geral
teste_es <- resultados_geral%>% select(-matricula,-calculo1,-vetorial,-p1, -lp1, -Periodo_Ingresso, -ic, -lpt, -calculo2,-grafos, -classica, - discreta, -lp2,-moderna, -eda, - tc, -gi, -linear)
teste_es
predict(modelo_es, teste_es)
resultados_geral$es
RMSE(predict(modelo_es, teste_es), resultados$es)

#si1
ranking_si1$cadeiras
atributos_desnecessarios <- setdiff(colnames(resultados_geral), ranking_si1$cadeiras)
atributos_desnecessarios
resultados_geral
teste_si1 <- resultados_geral%>% select(-matricula,-calculo1,-vetorial,-p1, -lp1, -Periodo_Ingresso, -ic, -lpt, -calculo2,-grafos, -classica, -discreta, -lp2,-moderna, -eda, -tc, -metodos)
teste_si1
predict(modelo_si1, teste_si1)
resultados_geral$si1
RMSE(predict(modelo_si1, teste_si1), resultados$si1)

#plot comparando rmse 

rmse_regressao <- c(RMSE(predict(modelo_oac, teste_oac), resultados$oac),RMSE(predict(modelo_loac, teste_loac), resultados$loac),RMSE(predict(modelo_metodos, teste_metodos), resultados$metodos), 
                    RMSE(predict(modelo_plp, teste_plp), resultados$plp), RMSE(predict(modelo_logica, teste_logica), resultados$logica), RMSE(predict(modelo_es, teste_es), resultados$es), 
                    RMSE(predict(modelo_si1, teste_si1), resultados$si1))
rmse_regressao <- as.data.frame(rmse_regressao)

colnames(rmse_regressao) <- "LinearRegression"

cf<-NULL
ColaborativeFiltering <- c(1.2445437, 2.3916986, 0.9132360, 0.9025889, 1.2459712, 0.5549775, 0.9422195)
cf <- as.data.frame(ColaborativeFiltering)
disciplinas <- c("oac","loac", "metodos","plp","logica", "es", "si1")
disciplinas <- as.data.frame(disciplinas)
rmse_regressao
cf
comparando_rmse1 <- cbind(rmse_regressao, cf)
rmse_regressao1 <- rmse_regressao %>% rename(media_rmse = LinearRegression) %>% mutate(tipo = "RegressaoLinear")
rmse_regressao1 <- cbind(disciplinas, rmse_regressao1)
rmse_regressao1
cf1 <- cf %>% rename(media_rmse = ColaborativeFiltering) %>% mutate(tipo = "FiltragemColaborativa")
cf1 <- cbind(disciplinas, cf1)
df <- rbind(rmse_regressao1, cf1)
df
write.csv(df, file = "LReCF.csv",row.names = F)

df %>% ggplot(aes(disciplinas, media_rmse,color = tipo)) + geom_point( show.legend = T)
