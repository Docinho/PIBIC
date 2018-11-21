import pandas as pd
from sklearn import linear_model
import numpy as np
import math
exec(open("myLibrary.py").read())

## CONSTANTES -- nomes de variáveis nos df
# nome da coluna disciplina nos df de Periodo e Pré-requisito
disciplinaString = 'DISCIPLINA'
disciplinaStringLower = 'Disciplina'
# nome da coluna de pré-requisitos no df de Pré-requisito
prerequisitoPrerequisito = 'PREREQUISITO'
# nome da coluna disciplina no df de Periodo
periodoString = 'PERIODO'
# nome da média do aluno no historico
mediaString = 'Media'


# eliminacao de acentos
def corrigir_nomes(nome):

    nome = nome.replace('í', 'I').replace('Í', 'I').replace('Á', 'A').replace('Ó', 'O'.replace('É','E')).replace('Ã', 'A').replace('É','E').replace('Ê','E').replace('Ç', 'C')
    return nome


# ajustando data frame que servirá de entrada para o algoritmo -- retorno: notas dos alunos(alunos x disciplina) e matriz de correlacao de disciplinas
def entradas_regressao(alunos):

    alunos_cc = alunos.query("Cod_Curso == 14102100 & Tipo == 'Obrigatória' ")
    alunos_cc['Nome_Disciplina'] = alunos_cc['Nome_Disciplina'].apply(corrigir_nomes).apply(lambda x: x.upper())
    notas_alunos = alunos_cc
    notas_alunos['Matricula'] = alunos_cc['Matricula'].map(lambda x: x.lstrip('B'))
    notas_alunos = pd.pivot_table(notas_alunos, values = 'Media_Disciplina', index = ['Matricula'], columns = 'Nome_Disciplina')
    corr_disciplinas = notas_alunos.corr()

    return(notas_alunos, corr_disciplinas)


# criação dos modelos de regressao
def regressao(periodo_disc, corr_disciplinas, notas_alunos):

    # guardandos as regressoes para cada disciplina em um dicionario
    disciplinas_regressoes = {}
    regressoes = {}
    regressao = linear_model.LinearRegression()

    for disciplina in periodo_disc[disciplinaString]:
        # pegando o periodo da disciplina da iteração
        periodo = periodo_disc.loc[periodo_disc[disciplinaString] == disciplina][periodoString].values[0]

        # pegando as disciplinas que são do mesmo periodo ou anteriores a disciplina em questão
        disciplinas_regressao = periodo_disc.loc[periodo_disc[periodoString]<=periodo][disciplinaString].values

        # selecionando as cinco disciplinas mais correlacionadas com a atual e que serão usadas para formar as equações
        disciplinas_regressao = np.setdiff1d(corr_disciplinas.loc[disciplinas_regressao].sort_values(by=disciplina,ascending=False).drop([disciplina])[[disciplina]][:5].index, disciplina)

        # guardando as disciplinas que devem participar do calculo da nota
        disciplinas_regressoes[disciplina] = disciplinas_regressao

        # selecionando as notas dos alunos nas disciplinas que devem ser usadas para o calculo
        disciplinas_regressao = notas_alunos[np.append(disciplinas_regressao,disciplina)].dropna(axis=0,how="any")

        # formulando as equações
        X = disciplinas_regressao.loc[:,disciplinas_regressao.columns != disciplina]
        y = disciplinas_regressao.loc[:,disciplinas_regressao.columns == disciplina]
        regressoes[disciplina] = regressao.fit(X,y)
    return(disciplinas_regressoes, regressoes)


def predicao():

    # lendo arquivos
    nota_aluno = pd.read_csv("temp.csv")
    nota_aluno[disciplinaStringLower] = nota_aluno[disciplinaStringLower].apply(corrigir_nomes).apply(lambda x: x.upper())

    # informações gerais dos alunos
    alunos = pd.read_csv("../../alunosUFCGAnon.csv")

    # df de disciplinas e prerrequitos
    pre_requisitos = pd.read_csv("../../preRequisitos.csv")
    pre_requisitos[disciplinaString] = pre_requisitos[disciplinaString].apply(corrigir_nomes).apply(lambda x: x.upper())
    pre_requisitos[prerequisitoPrerequisito] = pre_requisitos[prerequisitoPrerequisito].apply(corrigir_nomes).apply(
        lambda x: x.upper())

    # disciplinas e seus respectivos periodos
    periodo_disc = pd.read_csv("../../periodoDisciplinas.csv")
    periodo_disc[disciplinaString] = periodo_disc[disciplinaString].apply(corrigir_nomes).apply(
        lambda x: x.upper())

    # cadeiras que o aluno pode pagar a qualquer momento
    sem_prerequisito = ['DIREITO E CIDADANIA','GERENCIA DA INFORMACAO','INFORMATICA E SOCIEDADE','MATEMATICA DISCRETA',
    'METODOLOGIA CIENTIFICA','SEMINARIOS (EDUCACAO AMBIENTAL)','CALCULO DIFERENCIAL E INTEGRAL I',
    'PROGRAMACAO I','LABORATORIO DE PROGRAMACAO I','ALGEBRA VETORIAL E GEOMETRIA ANALITICA',
    'LEITURA E PRODUCAO DE TEXTOS','INTRODUCAO A COMPUTACAO']

    ## Organizando os dados e calculando as equações
    # df de notas e correlacao para a regressao
    notas_alunos, corr_disciplinas = entradas_regressao(alunos)

    # diciplinas e modelos das equacoes de regressao
    disciplinas_regressoes, regressoes = regressao(periodo_disc, corr_disciplinas, notas_alunos)

    ## Calculo do desempenho do aluno
    # cadeiras pagas pelo aluno
    cadeiras_pagas_df = nota_aluno[["Disciplina"]]
    cadeiras_pagas = nota_aluno["Disciplina"].values
    cadeira_possivel = []

    # vendo quais cadeiras sem prerrequisito já foram pagas pelo aluno
    for cadeira in sem_prerequisito:
        if not (cadeira in cadeiras_pagas):
            cadeira_possivel.append(cadeira)

    # cadeiras que o aluno pode cursar proximo periodo
    prox_possiveis_cadeiras = prox_cadeiras_nome(cadeiras_pagas_df, cadeira_possivel, pre_requisitos)

    dict_notas = {}
    # calculando possivel desempenho do aluno
    for cadeira in prox_possiveis_cadeiras:
        # verificando se todas as cadeiras necessarias para o calculo foram cursadas pelo aluno
        # caso contrario o modelo nao eh possivel de ser utilizado
        if set(disciplinas_regressoes[cadeira]).issubset(set(cadeiras_pagas)):
            
            # coletando as disciplinas necesarias para o calculo e suas respectivas notas refatoradas
            grades_to_be_predicted = list()
            for ref in disciplinas_regressoes[cadeira]:
                for index in range(0,len(nota_aluno)):
                    disc = nota_aluno.Disciplina[index]
                    if disc == ref:
                        value = float(nota_aluno[mediaString][index])
                        #.replace(',','.'))
                        grades_to_be_predicted.append(value)
            # calculando os possiveis desempenhos
            a = np.array(grades_to_be_predicted).reshape(1,-1)
            notas = regressoes[cadeira].predict(a)[0][0]
            dict_notas[cadeira] = str(math.floor(abs(notas)))[0:2]
            print(notas)

    # organizando df resultante a ser exibido
    df = pd.Series(data = dict_notas).to_frame()
    df.columns = ["Probabilidade de Aprovação (%)"]
    df[disciplinaStringLower] = df.index
    df = df[[disciplinaStringLower,"Probabilidade de Aprovação (%)"]]
    return df

print(predicao())
