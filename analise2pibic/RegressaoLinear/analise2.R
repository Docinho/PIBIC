library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(reshape2)
library(caret)
require(randomForest)
library(mlbench)

### Regressão Linear ###

## preparando os dados
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
alunos_max_media <- alunos_max_media %>% subset(select = -`SEMINÁRIOS.(EDUCAÇÃO.AMBIENTAL)`)
alunos_graduados <- alunos_max_media[complete.cases(alunos_max_media), ]
 
##Organizando os data frames principais
# separando alunos por periodo 
primeiro_periodo <- alunos_max_media %>% select(Matricula, cra, CALCULO.DIFERENCIAL.E.INTEGRAL.I, ÁLGEBRA.VETORIAL.E.GEOMETRIA.ANALÍTICA, PROGRAMAÇÃO.I, LABORATÓRIO.DE.PROGRAMAÇÃO.I,
                                                INTRODUÇÃO.A.COMPUTAÇÃO, LEITURA.E.PRODUCAO.DE.TEXTOS) %>%
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
periodos_dados <- merge(primeiro_segundo_periodos, terceiro_periodo)
periodos_dados <- merge(periodos_dados, quarto_periodo) %>% select(matricula, everything())
head(periodos_dados)

## Pergunta principal

# Modelo primeiro periodo
lm.p1 <- lm(cra~ ., data = primeiro_periodo %>% select(-matricula))
summary(lm.p1)
lm.p1
par(mfrow = c(1, 6))
termplot(lm.p1, partial.resid = TRUE, smooth = panel.smooth, span.smth = 1/4)
par(mfrow=c(2,1))
plot(lm.p1, which = 1:2)

# Modelo do segundo periodo
lm.p2 <- lm(cra~., data = segundo_periodo %>% select(-matricula))
summary(lm.p2)
par(mfrow = c(1,6))
termplot(lm.p2, partial.resid = TRUE, smooth = panel.smooth, span.smth = 1/4)
par(mfrow=c(2,1))
plot(lm.p2, which=1:2)

# Modelo do primeiro e segundo periodos
lm.p1.p2 <- lm(cra~ ., data = primeiro_segundo_periodos %>% select(-matricula))
lm.p1.p2
summary(lm.p1.p2)
par(mfrow = c(2,6))
termplot(lm.p1.p2, partial.resid = TRUE, smooth = panel.smooth, span.smth = 1/4)


# comparando as regressoes

resultado_p1 <- data.frame(pred = predict(lm.p1, primeiro_periodo %>% select(-matricula) %>% select(-cra)), obs = primeiro_periodo$cra)
resultado_p2 <- data.frame(pred = predict(lm.p2, segundo_periodo %>% select(-matricula) %>% select(-cra)), obs = segundo_periodo$cra)
resultado_p1_p2 <- data.frame(pred = predict(lm.p1.p2, primeiro_segundo_periodos %>% select(-matricula) %>% select(-cra)), obs = primeiro_segundo_periodos$cra)

resultado_p1$modelo <- "P1"
resultado_p2$modelo <- "P2"
resultado_p1_p2$modelo <- "P1+P2"

head(resultado_p1)
head(resultado_p2)
head(resultado_p1_p2)
comparacao <- rbind(resultado_p1, resultado_p2, resultado_p1_p2)

ggplot(comparacao, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) + facet_grid(. ~modelo) + geom_abline(color = "red")
round(defaultSummary(resultado_p1), digits = 3)
round(defaultSummary(resultado_p2), digits = 3)
round(defaultSummary(resultado_p1_p2), digits = 3)

## Analise das Variaveis
ggplot(melt(primeiro_segundo_periodos), aes(x = value)) + facet_wrap(~variable, scales = "free_x") +
  geom_histogram(aes(fill =..count..))

ggcorr(primeiro_segundo_periodos %>% select(-matricula), palette = "RdBu", label = TRUE, label_round =3)
ggpairs(primeiro_segundo_periodos %>% select(-matricula))

head(alunos_graduados)

df_tentativa1 <- alunos_graduados %>%
  select("CALCULO.DIFERENCIAL.E.INTEGRAL.I", "CALCULO.DIFERENCIAL.E.INTEGRAL.II", "ÁLGEBRA.VETORIAL.E.GEOMETRIA.ANALÍTICA", "PROGRAMAÇÃO.I", "PROGRAMAÇÃO.II", "TEORIA.DOS.GRAFOS", "LEITURA.E.PRODUCAO.DE.TEXTOS", "MATEMÁTICA.DISCRETA", "FUNDAMENTOS.DE.FÍSICA.CLÁSSICA", "LABORATÓRIO.DE.PROGRAMAÇÃO.I",  Matricula, cra) %>%
  na.omit()
head(df_tentativa1)
colnames(df_tentativa1) <- c("calculo1","calculo2", "vetorial", "p1", "p2", "grafos", "lpt", "discreta", "classica", "lp1", "matricula", "cra")
head(df_tentativa1)
# tirar classica aumenta muito o p-value
lm_tentativa1 <- lm(cra~., data = df_tentativa1 %>% select(-matricula))
summary(lm_tentativa1)

resultado_tentativa1 <- data.frame(pred = predict(lm_tentativa1, df_tentativa1 %>% select(-matricula) %>% select(-cra)), obs = df_tentativa1$cra)
resultado_tentativa1$modelo <- "Tentativa1"
head(resultado_tentativa1)
ggplot(resultado_tentativa1, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +
  facet_grid(. ~modelo) +
  geom_abline(color = "red")
round(defaultSummary(resultado_tentativa1))

par(mfrow = c(2, 1))
plot(lm_tentativa1, which = 1:2)


notas.p1.p2 = data.frame(calculo1 = 8.3, vetorial = 10, lpt = 9.2, p1 = 10, ic=9.9, lp1 =10, calculo2 = 9.8, discreta = 10, p2 = 9.8, grafos = 10, classica = 9.7, lp2 = 9.7)

notas.tentativa3 = data.frame(calculo1 = 8.3, vetorial = 10, lpt = 9.2, lp1 = 10, discreta = 10, grafos = 10, p2 = 9.8)

predict(lm.p1.p2, notas.p1.p2)
lm.p1.p2

#################### ADAPTANDO #######################

# usar alunos_max_media ao inves de alunos_graduados causa uma diferença de 0.3 na predição do cra, sendo max_media mais preciso

# calculando as features mais importantes
cadeiras_pri_seg_periodos <- primeiro_segundo_periodos%>% select(-matricula)
cra_notas <- cadeiras_pri_seg_periodos %>% select(cra)
set.seed(7)

## Calculando as cadeiras mais importantes 

# calculando as features mais importantes
matriz_correlacao <- periodos_dados %>% select(-matricula) %>% cor()
head(matriz_correlacao)
cadeiras_correlatas <- matriz_correlacao %>% findCorrelation(cutoff=0.5)
# indices de atributos altamente correlatos
cadeiras_correlatas

# descobrir as features mais importantes
controle <- trainControl(method = "repeatedcv", number = 50, repeats = 3)
modelo_cra <- train(cra~., data = cadeiras_pri_seg_periodos, method ="knn", preProcess = "scale", trControl = controle, tuneLength = 20)
modelo_cra
importancia <- varImp(modelo_cra, scale = F)
plot(importancia)

#confirma a necessidade de todas as cadeiras para ter um baixo RMSE
controle1 <- rfeControl(functions=rfFuncs, method = "cv", number = 10)
resultados <- rfe(cadeiras_pri_seg_periodos, as.vector(unlist(cra_notas)), sizes = c(1:10), rfeControl = controle1)
print(resultados)
predictors(resultados)
#com seis cadeiras jah eh suficiente
plot(resultados, type=c("g", "o"))
resultados$fit
resultados$resample
resultados$summary
#FONTE: https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/


# grafico de correlaçao -> pouco eficiente
newdatacor = cor(periodos_dados[2:28])
corrplot(newdatacor, method = "square")

lm.periodos <- lm(oac~ ., data = periodos_dados %>% select(-matricula, -cra))
summary(lm.periodos)

resultado_tentativa1 <- data.frame(pred = predict(lm.periodos, periodos_dados %>% select(-matricula) %>% select(-oac)), obs = periodos_dados$oac)
plot(periodos_dados %>% select(-matricula), pch=16, col="blue")
resultado_tentativa1$modelo <- "Tentativa1"
head(resultado_tentativa1)
ggplot(resultado_tentativa, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) #+

#RMSE e MAE altos
round(defaultSummary(resultado_tentativa1))

## Calculando as cadeiras mais importantes e a quantidade ideal de features a serem usadas

cadeiras_pri_qua_periodos <- periodos_dados %>% select(-matricula)

# calculando as features mais importantes
matriz_correlacao <- cadeiras_pri_qua_periodos %>% cor()
head(matriz_correlacao)
cadeiras_correlatas <- matriz_correlacao %>% findCorrelation(cutoff=0.5)
# indices de atributos autamente correatos
cadeiras_correlatas
cadeiras_reduzidas <- cadeiras_pri_qua_periodos %>% select(-cra, -discreta, -prob, -p2, -calculo1, -leda, -grafos, -ic, -lp1)


## para oac
cadeiras_reduzidas_sem_oac <- cadeiras_reduzidas %>% select(-oac)
oac_notas <- cadeiras_pri_qua_periodos %>% select(oac)
# descobrir as features mais importantes
controle <- trainControl(method = "repeatedcv", number = 50, repeats = 3)
modelo_oac <- train(oac~., data = cadeiras_pri_qua_periodos,method ="knn", preProcess = c("center","scale"), trControl = controle, tuneLength = 20)
modelo_oac
modelo_oac <- train(oac~., data = cadeiras_reduzidas,method ="knn", preProcess = c("center","scale"), trControl = controle, tuneLength = 20)
modelo_oac
#diminuir a quantidade de cadeiras diminuiu o erro de tornou o k = 7(RMSE = 0,78) ideal (ao invés de 9(RMSE = 0,82))
importancia_oac <- varImp(modelo_oac, scale = F)
importancia_oac
plot(importancia_oac)

resultado_oac <- rfe(cadeiras_reduzidas_sem_oac, as.vector(unlist(oac_notas)), sizes = c(1:14), rfeControl = controle1)
print(resultado_oac)
predictors(resultado_oac)
plot(resultado_oac, type=c("g", "o"))

cadeiras_necessarias_oac <- cadeiras_pri_qua_periodos %>% select(oac, logica, metodos, loac, linear,si1, vetorial, lpt, es, lp2, gi, calculo2, tc, eda, p1, classica)

## para si1
si1_notas <- cadeiras_pri_qua_periodos %>% select(si1)
cadeiras_reduzidas_sem_si1 <- cadeiras_pri_qua_periodos %>% select(-si1)

# descobrir as features mais importantes
# k = 9
modelo_si1 <- train(si1~., data = cadeiras_reduzidas,method ="knn", preProcess = c("center","scale"), trControl = controle, tuneLength = 20)
modelo_si1
# k = 5
modelo_si1 <- train(si1~., data = cadeiras_pri_qua_periodos,method ="knn", preProcess = c("center","scale"), trControl = controle, tuneLength = 20)
modelo_si1
#diminuira quantidade de cadeiras aumentou o erro logo o melhor eh k = 5(RMSE = 0.73) (ao invés de 9)
importancia_si1 <- varImp(modelo_si1, scale = F)
importancia_si1
plot(importancia_si1)

resultado_si1 <- rfe(cadeiras_pri_qua_periodos, as.vector(unlist(si1_notas)), sizes = c(1:14), rfeControl = controle1)
print(resultado_si1)
predictors(resultado_si1)
plot(resultado_si1, type=c("g", "o"))

cadeiras_necessarias_si1 <- c("cra","gi","loac","linear","logica","leda")


## para prob

prob_notas <- cadeiras_pri_qua_periodos %>% select(prob)
cadeiras_reduzidas_sem_prob <- cadeiras_pri_qua_periodos %>% select(-prob)

# descobrir as features mais importantes
modelo_prob <- train(prob~., data = cadeiras_pri_qua_periodos,method ="knn", preProcess = c("center","scale"), trControl = controle, tuneLength = 20)
modelo_prob
modelo_prob <- train(prob~., data = cadeiras_reduzidas,method ="knn", preProcess = c("center","scale"), trControl = controle, tuneLength = 20)
modelo_prob
#diminuir a quantidade de cadeiras nao fez diferença (RMSE = 0,81 k = 7)
importancia_prob <- varImp(modelo_prob, scale = F)
importancia_prob
plot(importancia_prob)

resultado_prob <- rfe(cadeiras_reduzidas_sem_prob, as.vector(unlist(prob_notas)), sizes = c(1:14), rfeControl = controle1)
print(resultado_prob)
predictors(resultado_prob)
plot(resultado_prob, type=c("g", "o"))

cadeiras_necessarias_prob <- predictors(resultado_prob)


# notas preditas para oac por regressão linear
predict(modelo_oac, periodos_dados %>% select(-oac))
# notas reais
periodos_dados %>% select(oac)
