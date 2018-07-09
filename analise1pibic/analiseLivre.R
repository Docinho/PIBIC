library(plyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(Hmisc)
#devtools::install_github("ropensci/plotly")

# organizando os dados 
dados_UFCG <- read.csv("../alunosUFCGAnon.csv")

## PARTE 1

# fazendo mediana
dados_mediana <- dados_UFCG %>% group_by(Cod_Curso) %>% mutate(Mediana_Curso = median(Media_Disciplina, na.rm = T))
dados_mediana <-  dados_mediana[order(dados_mediana$Mediana_Curso, decreasing = T),]

# fazendo a o grafico
dados_mediana %>% ggplot(aes (x = factor(reorder(Cod_Curso, Mediana_Curso, na.rm = T)), y = Media_Disciplina), na.rm = T) + 
  geom_boxplot(alpha = 1/8, na.rm = T) + theme(axis.text.x  = element_blank(), axis.ticks.x = element_blank()) + xlab("Cursos") + ylab("Distribuição das médias")


#agrupando os cursos com maior mediana
medianas_cursos <- aggregate(x = dados_mediana$Media_Disciplina, by = list("Cod_Curso" = factor(dados_mediana$Cod_Curso), "Nome_Curso" = dados_mediana$Nome_Curso, "Campus" = dados_mediana$Campus), FUN = median, na.rm = T)
colnames(medianas_cursos)[4] <- "Mediana_Curso"
medianas_cursos <- medianas_cursos[order(medianas_cursos$Mediana_Curso), ]

cursos_maiores_medianas <- medianas_cursos[95:85, ]
cursos_menores_medianas <- medianas_cursos[1:10, ]

# selecionando apenas o nome dos cursos
dados_menores_medianas <- na.omit(subset(dados_UFCG, Cod_Curso %in% cursos_menores_medianas[,1]))
dados_menores_medianas <- mutate(dados_menores_medianas, Curso_Campus = paste(dados_menores_medianas$Nome_Curso, dados_menores_medianas$Campus, sep = " : "))
dados_maiores_medianas <- na.omit(subset(dados_UFCG , Cod_Curso %in% cursos_maiores_medianas[,1]))
dados_maiores_medianas <- mutate(dados_maiores_medianas, Curso_Campus = paste(dados_maiores_medianas$Nome_Curso, dados_maiores_medianas$Campus, sep = " : "))

# plots
dados_maiores_medianas %>% ggplot(aes(x = reorder(Cod_Curso, Media_Disciplina, FUN = "median"), y = Media_Disciplina, fill = Curso_Campus)) + geom_boxplot() + 
  xlab("Código do Curso") + ylab("Distribuição das Médias") + labs(fill = "Nome do Curso : Campus") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("10 cursos com maiores medianas")
dados_menores_medianas %>% ggplot(aes(x = reorder(Cod_Curso, Media_Disciplina, FUN = "median"), y = Media_Disciplina, fill = Curso_Campus)) + geom_boxplot() + 
  xlab("Código do Curso") + ylab("Distribuição das Médias") + labs(fill = "Nome do Curso : Campus") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ggtitle("10 cursos")


# calculando a mediana e o desvio padrão
dados_mediana_desvio <- dados_UFCG %>% group_by(Cod_Curso, Nome_Curso, Campus)
dados_mediana_desvio <- dados_mediana_desvio %>% summarise(Media_Curso = mean(Media_Disciplina, trim = 0.1, na.rm = T), Desvio_Padrao = sd(Media_Disciplina, na.rm = T))
dados_mediana_desvio <- dados_mediana_desvio[order(dados_mediana_desvio$Media_Curso), ]
dados_mediana_desvio$Curso_Campus <- paste(dados_mediana_desvio$Nome_Curso, dados_mediana_desvio$Campus, sep = " : ")

# plots
ggplot() + geom_point(data = dados_mediana_desvio, aes(x = factor(reorder(Cod_Curso, Media_Curso)), y = Media_Curso), size = 2) + 
  geom_errorbar(data = dados_mediana_desvio, aes(x = factor(Cod_Curso), ymin = Media_Curso - Desvio_Padrao, ymax = 
                                                   Media_Curso + Desvio_Padrao), colour = "red", width = 0.4) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  xlab("Cursos") + ylab("Média") + ggtitle("Cursos ordenados por média aparada")

# cursos com maiores e menores medias
dados_media_curso <- aggregate(x = dados_UFCG$Media_Disciplina, by = list(Cod_Curso = factor(dados_UFCG$Cod_Curso), Nome_Curso = dados_UFCG$Nome_Curso), FUN = function(x) mean(x, trim = 0.1, na.rm = T))
dados_media_curso <- dados_media_curso[order(dados_media_curso$x), ]

maiores_medias <- dados_mediana_desvio[1:10,]
menores_medias <- dados_mediana_desvio[95:86,]

# plot
ggplot() + geom_point(data = maiores_medias, aes(x = factor(reorder(Curso_Campus, Media_Curso)), y = Media_Curso), size = 2) + 
  geom_errorbar(data = maiores_medias, aes(x = factor(reorder(Curso_Campus, Media_Curso)), ymin = Media_Curso - Desvio_Padrao, ymax = Media_Curso + Desvio_Padrao), width = 0.4) +
  ylab("Media") + xlab("Curso") + coord_flip() + ggtitle("10 Maiores Médias Ordenadas")

ggplot() + geom_point(data = menores_medias, aes(x = factor(reorder(Curso_Campus, Media_Curso)), y = Media_Curso), size = 2) + 
  geom_errorbar(data = menores_medias, aes(x = factor(reorder(Curso_Campus, Media_Curso)), ymin = Media_Curso - Desvio_Padrao, ymax = Media_Curso + Desvio_Padrao), width = 0.4) +
  ylab("Media") + xlab("Curso") + coord_flip() + ggtitle("10 Menores Médias Ordenadas")


## PARTE 2

# selecionando os alunos de Computação, cálculo da média por período e ordenação pela média
dados_cc <- dados_UFCG %>% subset(Cod_Curso == 14102100, na.rm = T)
dados_cc <- dados_cc %>% group_by(Periodo) %>% mutate(Media_Periodo = mean(Media_Disciplina, trim = 0.1, na.rm = T), Desvio_Periodo = sd(Media_Disciplina, na.rm = T))
dados_cc <- dados_cc[order(dados_cc$Media_Periodo), ]

#plot
dados_cc %>% ggplot(aes(factor(Periodo), Media_Disciplina, na.rm = T)) + geom_boxplot(alpha = 1/4, na.rm = T) + xlab("Periodo") + ylab("Distribuição das Médias") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot() + geom_point(data = dados_cc, aes(x = factor(Periodo), y = Media_Periodo, size = 2)) + 
  geom_errorbar(data = dados_cc, aes(x= factor(Periodo), ymin = Media_Periodo - Desvio_Periodo, ymax = Media_Periodo + Desvio_Periodo), colour = "red", width = 0.4) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## PARTE 3

# selecionando apenas as linhas com calculo e discreta
dados_discreta <- dados_cc %>% subset(Cod_Disciplina == 1109113)
dados_calculo <- dados_cc %>% subset(Cod_Disciplina == 1109103)

# seleciona a última média do aluno na disciplina
dados_discreta <- dados_discreta %>% group_by(Matricula) %>% top_n(1, Media_Disciplina)
dados_calculo <- dados_calculo %>% group_by(Matricula) %>% top_n(1, Media_Disciplina)

# descarta as colunas que não agregam informação à análise
dados_discreta <- dados_discreta %>% subset(select = c("Matricula", "Media_Disciplina"))
dados_calculo <- dados_calculo %>% subset(select = c("Matricula", "Media_Disciplina"))

# junta os dados das cadeiras em uma única estrutura de dados
dados_discreta_calculo <- merge(dados_discreta, dados_calculo, by.x = "Matricula", by.y = "Matricula", na.rm = T)
colnames(dados_discreta_calculo)<- c("Matricula", "Medias_Discreta", "Medias_Calculo")
dados_discreta_calculo <- dados_discreta_calculo[order(dados_discreta_calculo$Media_Disciplina.x), ]

# plot
dados_discreta_calculo %>% ggplot(aes(Medias_Discreta, Medias_Calculo)) + geom_point(shape = 19, alpha = 0.4)



### PERGUNTAS

# Há diferença no desempenho do aluno do .1 e do .2?
detach(package:plyr)
dados_primeiro_periodo <- filter(dados_UFCG, Periodo_Relativo == 1, !is.na(Media_Disciplina)) %>% 
  group_by(Matricula, Cod_Curso) %>% 
  mutate(Media_Aluno = mean(Media_Disciplina, rm.na = T)) 

#plot geral
plot_ly(data = dados_primeiro_periodo, x = as.character(dados_primeiro_periodo$Periodo), y = dados_primeiro_periodo$Media_Curso, symbol = dados_primeiro_periodo$Periodo_Ano, color = dados_primeiro_periodo$Nome_Curso, type = "scatter", legendgroup =~dados_primeiro_periodo$Nome_Curso )

# Checagem de diferença estatística
dados_periodo.1 <- dados_primeiro_periodo %>% filter((Periodo*10)%%2 != 0)
dados_periodo.2 <- dados_primeiro_periodo %>% filter((Periodo*10)%%2 == 0 && Nome_Curso %in% dados_periodo.1$Nome_Curso)
dados_periodo.1 <- dados_periodo.1 %>% filter(Nome_Curso %in% dados_periodo.2$Nome_Curso)
t.test(dados_periodo.1$Media_Aluno, dados_periodo.2$Media_Aluno)  

# Transformando o Perido em 0, se .1 e 1, caso contrário
dados_primeiro_periodo <- dados_primeiro_periodo %>% 
  group_by(Periodo, Nome_Curso) %>% mutate(Periodo_Ano = (Periodo*10)%%2, Media_Curso = mean(Media_Aluno)) 
# dados_primeiro_periodo$Periodo_Ano <- as.logical(dados_primeiro_periodo$Periodo_Ano == 1)

# plot humanas
cursos_humanas = c(13301100, 13301200,31301200, 13317100, 13305210,71305210, 13306100, 13306200, 13316110, 21316110, 21316210, 13316210,13309110, 21309110, 13309210, 21309210, 13310214, 13310115, 13310116, 21310116, 13310110, 21310110, 13310210, 21310210, 13345110, 13311150, 13311110, 21312110, 21312210, 13312110, 13312210, 12208200, 31340100)
dados_primeiro_periodo_humanas <- dados_primeiro_periodo %>% 
  filter(Cod_Curso %in% cursos_humanas)                                                                
plot_ly(data = dados_primeiro_periodo_humanas, x = as.character(dados_primeiro_periodo_humanas$Periodo), y = dados_primeiro_periodo_humanas$Media_Curso, symbol = dados_primeiro_periodo_humanas$Periodo_Ano, color = dados_primeiro_periodo_humanas$Nome_Curso, type = "scatter", legendgroup =~dados_primeiro_periodo_humanas$Nome_Curso )

# plot biologicas
cursos_biologicas = c(41201110, 51201110, 21201110, 41201210, 51201210, 12204100, 21204100, 51204100, 12205100, 21205100, 41410100, 51206100, 41207100)
dados_primeiro_periodo_biologicas <- dados_primeiro_periodo %>% 
  filter(Cod_Curso %in% cursos_biologicas)                                                                
plot_ly(data = dados_primeiro_periodo_biologicas, x = as.character(dados_primeiro_periodo_biologicas$Periodo), y = dados_primeiro_periodo_biologicas$Media_Curso, symbol = dados_primeiro_periodo_biologicas$Periodo_Ano, color = dados_primeiro_periodo_biologicas$Nome_Curso, type = "scatter", legendgroup =~dados_primeiro_periodo_biologicas$Nome_Curso )

# plot exatas
dados_primeiro_periodo_exatas <- dados_primeiro_periodo %>% 
  filter(Cod_Curso %nin% cursos_biologicas & Cod_Curso %nin% cursos_humanas)                                                                
plot_ly(data = dados_primeiro_periodo_exatas, x = as.character(dados_primeiro_periodo_exatas$Periodo), y = dados_primeiro_periodo_exatas$Media_Curso, symbol = dados_primeiro_periodo_exatas$Periodo_Ano, color = dados_primeiro_periodo_exatas$Nome_Curso, type = "scatter", legendgroup =~dados_primeiro_periodo_exatas$Nome_Curso )



## Diferenças de médias e reprovações quando há greve e quando não há
# Periodos em que houve greve
condition <- c(2003.1, 2004.1, 2005.1, 2012.1, 2015.1)
# Comparando quantidade de faltas, trancamentos e reprovacoes (sem diferenca significativa, no geral, mas há aumento no numero de trancamentos)
num_greve = 97522
num_nao_greve = 846240

# Separando os dados dos períodos que houve greve
dados_greve <- dados_UFCG %>%
  filter(as.character(Periodo) == "2003.1" | as.character(Periodo) == "2004.1" | as.character(Periodo) == "2005.1" | as.character(Periodo) == "2012.1")

# Contagem das situações dos alunos (Reprovado, Reprovado por falta, Trancado, Aprovado) de quando houve greve
occurrance  <- data.frame(xtabs(~Periodo + Situacao, dados_greve)) %>% mutate(Porcentagem = Freq/num_greve * 100)

# Separando os dados dos períodos que não houve greve
dados_nao_greve <- dados_UFCG %>%
  filter(as.character(Periodo) != "2003.1" & as.character(Periodo) != "2004.1" & as.character(Periodo) != "2005.1" & as.character(Periodo) != "2012.1") %>%
  group_by(Periodo)
# Contagem das situações dos alunos (Reprovado, Reprovado por falta, Trancado, Aprovado) de quando não houve greve
occurrance_nao_greve <- data.frame(xtabs(~Periodo + Situacao, dados_nao_greve)) %>% mutate(Porcentagem = Freq/num_nao_greve * 1000)

# Plot do número de situações de quando houve greve
ggplot(occurrance, aes(x =  occurrance$Situacao,y = occurrance$Freq, fill = occurrance$Periodo)) +
  geom_bar(stat="identity") + ylab("Quantidade de alunos") + xlab("Situacao do aluno") +
labs(fill = "Periodo") #+ layout(title("Situação dos alunos nas greves"))

#Nao passa boa representatividade
# ggplot(occurances_nao_greve, aes(x =  occurances_nao_greve$Situacao,y = occurances_nao_greve$Porcentagem, fill = occurances_nao_greve$Periodo)) +
#   geom_bar(stat="identity", position = "dodge") + ylab("Quantidade de alunos") + xlab("Situacao do aluno") +
#   labs(fill = "Periodo")



# Plot de porcentagem de situações por periodo de quando houve greve
ggplot(occurrance, aes(group =  occurrance$Situacao,y = occurance$Porcentagem, x = occurrance$Periodo, color = occurrance$Situacao)) +
  geom_line(stat="identity") + ylab("Porcentagem de alunos(100%)") + xlab("Periodo") +
  labs(color = "Periodo") #+ layout(title("Porcentagem de situações por periodo de quando houve greve"))

# Plot per mil de situações por periodo de quando não houve greve
ggplot(occurrance_nao_greve, aes(group =  occurrance_nao_greve$Situacao,y = occurrance_nao_greve$Porcentagem, x = occurrance_nao_greve$Periodo, color = occurrance_nao_greve$Situacao)) +
  geom_line(stat="identity") + ylab("Porcentagem de alunos(1000%)") + xlab("Periodo") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(color = "Periodo") #+ layout(title("Por mil de situações por periodo de quando não houve greve"))



## Situação dos alunos apenas para cc
# Filtrando alunos de cc
occurrance_cc <-filter(dados_greve, Cod_Curso == 14102100)
occurrance_nao_greve_cc <- dados_nao_greve %>% filter(Cod_Curso == 14102100)

# Contando a frequencia das situacoes e fazendo porcentagem para periodos de greve
occurrance_cc <- data.frame(xtabs(~Periodo + Situacao, occurrance_cc)) 
num_greve_cc = sum(occurrance_cc$Freq, na.rm = T) 
occurrance_cc <- occurrance_cc %>% mutate(Porcentagem = Freq/num_greve_cc * 100)

# Contando a frequencia das situacoes e fazendo porcentagem para periodos de não-greve
occurrance_nao_greve_cc <- data.frame(xtabs(~Periodo + Situacao, occurrance_nao_greve_cc)) 
num_nao_greve_cc = sum(occurrance_nao_greve_cc$Freq, na.rm = T)
occurrance_nao_greve_cc <- occurrance_nao_greve_cc %>% mutate(Porcentagem = Freq/num_nao_greve_cc *100)

# Plotando grafico para Computacao comparando a situacao dos alunos
#para quando houve e quando não houve greve, separadamente
ggplot(occurrance_cc, aes(group =  occurrance_cc$Situacao,y = occurrance_cc$Porcentagem, x = occurrance_cc$Periodo, color = occurrance_cc$Situacao)) +
  geom_line(stat="identity") + ylab("Porcentagem de alunos(100%)") + xlab("Periodo") +
  labs(color = "Periodo") #+ layout(title("Porcentagem de situações em Computação por periodo de quando houve greve"))

ggplot(occurrance_nao_greve_cc, aes(group =  occurrance_nao_greve_cc$Situacao,y = occurrance_nao_greve_cc$Porcentagem, x = occurrance_nao_greve_cc$Periodo, color = occurrance_nao_greve_cc$Situacao)) +
  geom_line(stat="identity") + ylab("Porcentagem de alunos(100%)") + xlab("Periodo") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
