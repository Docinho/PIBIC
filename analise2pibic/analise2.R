library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)
library(reshape2)
library(caret)

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
primeiro_periodo <- alunos_max_media %>% select(Matricula, CALCULO.DIFERENCIAL.E.INTEGRAL.I, ÁLGEBRA.VETORIAL.E.GEOMETRIA.ANALÍTICA, PROGRAMAÇÃO.I, LABORATÓRIO.DE.PROGRAMAÇÃO.I,
                                                INTRODUÇÃO.A.COMPUTAÇÃO, LEITURA.E.PRODUCAO.DE.TEXTOS, cra) %>%
  na.omit(primeiro_periodo)
colnames(primeiro_periodo) <- c("calculo 1", "vetorial", "p1", "lp1", "ic", "lpt", "cra", "matricula")
head(primeiro_periodo)

segundo_periodo <- alunos_max_media %>% select(Matricula, CALCULO.DIFERENCIAL.E.INTEGRAL.II, FUNDAMENTOS.DE.FÍSICA.CLÁSSICA, TEORIA.DOS.GRAFOS, PROGRAMAÇÃO.II,
                                               LABORATÓRIO.DE.PROGRAMAÇÃO.II, MATEMÁTICA.DISCRETA, cra) %>% na.omit()
colnames(segundo_periodo) <- c("matricula", "calculo 2", "fisica classica", "grafos", "p2", "lp2", "discreta", "cra")
head(segundo_periodo)

primeiro_segundo_periodos <- merge(primeiro_periodo, segundo_periodo)
head(primeiro_segundo_periodos)

## Pergunta principal