library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(reshape2)
library(caret)
library(FNN)

## preparando os dados
setwd("Área de Trabalho")
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
 
# separando alunos por periodo 
primeiro_periodo <- alunos_max_media %>% select(Matricula, cra, CALCULO.DIFERENCIAL.E.INTEGRAL.I, ÁLGEBRA.VETORIAL.E.GEOMETRIA.ANALÍTICA, PROGRAMAÇÃO.I, LABORATÓRIO.DE.PROGRAMAÇÃO.I,
                                                INTRODUÇÃO.A.COMPUTAÇÃO, LEITURA.E.PRODUCAO.DE.TEXTOS) %>%
  na.omit(primeiro_periodo) %>% arrange(Matricula)
colnames(primeiro_periodo) <- c("matricula", "cra","calculo1", "vetorial", "p1", "lp1", "ic", "lpt")
head(primeiro_periodo)

segundo_periodo <- alunos_max_media %>% select(Matricula,cra, CALCULO.DIFERENCIAL.E.INTEGRAL.II, FUNDAMENTOS.DE.FÍSICA.CLÁSSICA, TEORIA.DOS.GRAFOS, PROGRAMAÇÃO.II,
                                               LABORATÓRIO.DE.PROGRAMAÇÃO.II, MATEMÁTICA.DISCRETA) %>% na.omit() %>% arrange(Matricula)
colnames(segundo_periodo) <- c("matricula", "cra","calculo2", "classica", "grafos", "p2", "lp2", "discreta")
head(segundo_periodo)

primeiro_segundo_periodos <- merge(primeiro_periodo, segundo_periodo)
head(primeiro_segundo_periodos)

## Pergunta principal

lm.p1 <- lm(cra~ ., data = primeiro_periodo %>% select(-matricula))
summary(lm.p1)
lm.p1
#graficos de regressão de cada variavel // os graficos estão diferentes
par(mfrow = c(1, 6))
termplot(lm.p1, partial.resid = TRUE, smooth = panel.smooth, span.smth = 1/4)
par(mfrow=c(2,1))
plot(lm.pl, which = 1:2)

lm.p2 <- lm(cra~., data = segundo_periodo %>% select(-matricula))
summary(lm.p2)
par(mfrow = c(1,6))
termplot(lm.p2, partial.resid = TRUE, smooth = panel.smooth, span.smth = 1/4)
par(mfrow=c(2,1))
plot(lm.p2, which=1:2)
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
summary(lm.tentativa1)

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

terceiro_periodo <- alunos_max_media %>%
  select(Matricula,cra, ESTRUTURA.DE.DADOS.E.ALGORITMOS, LAB.DE.ESTRUTURA.DE.DADOS.E.ALGORITMOS, FUNDAMENTOS.DE.FÍSICA.MODERNA, 
         ALGEBRA.LINEAR.I,PROBABILIDADE.E.ESTATISTICA, TEORIA.DA.COMPUTAÇÃO, GERÊNCIA.DA.INFORMAÇÃO) %>% 
  na.omit() %>% 
  arrange(Matricula) %>%
  rename(matricula = Matricula, eda = ESTRUTURA.DE.DADOS.E.ALGORITMOS, leda =  LAB.DE.ESTRUTURA.DE.DADOS.E.ALGORITMOS, moderna =  FUNDAMENTOS.DE.FÍSICA.MODERNA, 
   linear = ALGEBRA.LINEAR.I, prob = PROBABILIDADE.E.ESTATISTICA, tc = TEORIA.DA.COMPUTAÇÃO, gi = GERÊNCIA.DA.INFORMAÇÃO)
head(terceiro_periodo)

#usar alunos_max_media ao inves de alunos_graduados causa uma diferença de 0.3 na predição do cra, sendo max_media mais preciso
quarto_periodo <- alunos_max_media %>% select(Matricula, cra, PARADIGMAS.DE.LING..DE.PROGRAMAÇÃO, METODOS.ESTATISTICOS, ORG.E.ARQUITETURA.DE.COMPUTADORES.I, 
                                             LAB.DE.ORG.E.ARQUITETURA.DE.COMPUTADORES, LÓGICA.MATEMÁTICA, ENGENHARIA.DE.SOFTWARE.I, SISTEMAS.DE.INFORMAÇÃO.I) %>%
  na.omit() %>%
  arrange(Matricula)%>% 
  rename(matricula = Matricula, plp = PARADIGMAS.DE.LING..DE.PROGRAMAÇÃO, metodos = METODOS.ESTATISTICOS, oac = ORG.E.ARQUITETURA.DE.COMPUTADORES.I, 
         loac = LAB.DE.ORG.E.ARQUITETURA.DE.COMPUTADORES, logica = LÓGICA.MATEMÁTICA, es = ENGENHARIA.DE.SOFTWARE.I, si1 = SISTEMAS.DE.INFORMAÇÃO.I)
head(quarto_periodo)

periodos_dados <- merge(primeiro_segundo_periodos, terceiro_periodo)
periodos_dados <- merge(periodos_dados, quarto_periodo)
head(periodos_dados)

newdatacor = cor(periodos_dados[2:28])
corrplot(newdatacor, method = "square")

lm.periodos <- lm(oac~ ., data = periodos_dados %>% select(-matricula))
summary(lm.periodos)

resultado_tentativa <- data.frame(pred = predict(lm.periodos, periodos_dados %>% select(-matricula) %>% select(-oac)), obs = periodos_dados$oac)
plot(periodos_dados %>% select(-matricula), pch=16, col="blue")
resultado_tentativa1$modelo <- "Tentativa1"
head(resultado_tentativa1)
ggplot(resultado_tentativa, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) 
  # facet_grid(. ~modelo) +
  # geom_abline(color = "red")
round(defaultSummary(resultado_tentativa))



notas.p1.p2 = data.frame(calculo1 = 8.3, vetorial = 10, lpt = 9.2, p1 = 10, ic=9.9, lp1 =10, calculo2 = 9.8, discreta = 10, p2 = 9.8, grafos = 10, classica = 9.7, lp2 = 9.7)
notas.tentativa3 = data.frame(calculo1 = 8.3, vetorial = 10, lpt = 9.2, lp1 = 10, discreta = 10, grafos = 10, p2 = 9.8)
predict(lm.periodos, notas.p1.p2)



################################
df_tentativa1 <- alunos_graduados %>%
  select("CALCULO.DIFERENCIAL.E.INTEGRAL.I", "CALCULO.DIFERENCIAL.E.INTEGRAL.II", "ÁLGEBRA.VETORIAL.E.GEOMETRIA.ANALÍTICA", "PROGRAMAÇÃO.I", "PROGRAMAÇÃO.II", "TEORIA.DOS.GRAFOS", "LEITURA.E.PRODUCAO.DE.TEXTOS", "MATEMÁTICA.DISCRETA", "FUNDAMENTOS.DE.FÍSICA.CLÁSSICA", "LABORATÓRIO.DE.PROGRAMAÇÃO.I",  Matricula, cra) %>%
  na.omit()
head(df_tentativa1)
colnames(df_tentativa1) <- c("calculo1","calculo2", "vetorial", "p1", "p2", "grafos", "lpt", "discreta", "classica", "lp1", "matricula", "cra")
head(df_tentativa1)
# tirar classica aumenta muito o p-value
lm_tentativa1 <- lm(cra~., data = df_tentativa1 %>% select(-matricula))
summary(lm.tentativa1)
df_tentativa1
newdatacor = cor(df_tentativa1[1:20])
corrplot(newdatacor, method = "number")

resultado_tentativa1 <- data.frame(pred = predict(lm_tentativa1, df_tentativa1 %>% select(-matricula) %>% select(-cra)), obs = df_tentativa1$cra)
resultado_tentativa1$modelo <- "Tentativa1"
head(resultado_tentativa1)
ggplot(resultado_tentativa1, aes(x = pred, y = obs)) + geom_point(alpha = 0.5, position = position_jitter(width = 0.2)) +
  facet_grid(. ~modelo) +
  geom_abline(color = "red")
round(defaultSummary(resultado_tentativa1))


train <- periodos_dados[1:10]
test <- periodos_dados[11:20]
cl <- factor(c(rep("s",19), rep("c",19), rep("v",20)))
cl
knn(train, test, cl, k = 3, prob=TRUE)
attributes(.Last.value)

