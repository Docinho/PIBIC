import pandas as pd
from sklearn import linear_model
import numpy as np
import math
exec(open("myLibrary.py").read())

# nome da coluna disciplina nos df de Periodo e Pré-requisito
disciplinaString = 'DISCIPLINA'
disciplinaStringLower = 'Disciplina'
# nome da coluna de pré-requisitos no df de Pré-requisito
prerequisitoPrerequisito = 'PREREQUISITO'
# nome da coluna disciplina no df de Periodo
periodoString = 'PERIODO'
mediaString = 'Média'



# aplicar correção nos df
def corrigir_nomes(nome):
    nome = nome.replace('í', 'I').replace('Í', 'I').replace('Á', 'A').replace('Ó', 'O'.replace('É','E')).replace('Ã', 'A').replace('É','E').replace('Ê','E').replace('Ç', 'C')
    return nome

# ajustando data frame que servirá de entrada
def entradas_regressao(alunos):
    alunos_cc = alunos.query("Cod_Curso == 14102100 & Tipo == 'Obrigatória' ")
    alunos_cc['Nome_Disciplina'] = alunos_cc['Nome_Disciplina'].apply(corrigir_nomes).apply(lambda x: x.upper())
    notas_alunos = alunos_cc
    notas_alunos['Matricula'] = alunos_cc['Matricula'].map(lambda x: x.lstrip('B'))
    notas_alunos = pd.pivot_table(notas_alunos, values = 'Media_Disciplina', index = ['Matricula'], columns = 'Nome_Disciplina')
    corr_disciplinas = notas_alunos.corr()
    return(notas_alunos, corr_disciplinas)

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
        disciplinas_regressao = np.setdiff1d(corr_disciplinas.loc[disciplinas_regressao].sort_values(by=disciplina,ascending=False).drop([disciplina])[[disciplina]][:5].index, disciplina)


        disciplinas_regressoes[disciplina] = disciplinas_regressao
        disciplinas_regressao = notas_alunos[np.append(disciplinas_regressao,disciplina)].dropna(axis=0,how="any")

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
    pre_requisitos = pd.read_csv("../../preRequisitos.csv")
    pre_requisitos[disciplinaString] = pre_requisitos[disciplinaString].apply(corrigir_nomes).apply(lambda x: x.upper())
    pre_requisitos[prerequisitoPrerequisito] = pre_requisitos[prerequisitoPrerequisito].apply(corrigir_nomes).apply(
        lambda x: x.upper())
    periodo_disc = pd.read_csv("../../periodoDisciplinas.csv")
    periodo_disc[disciplinaString] = periodo_disc[disciplinaString].apply(corrigir_nomes).apply(
        lambda x: x.upper())

    sem_prerequisito = ['DIREITO E CIDADANIA','GERENCIA DA INFORMACAO','INFORMATICA E SOCIEDADE','MATEMATICA DISCRETA',
    'METODOLOGIA CIENTIFICA','SEMINARIOS (EDUCACAO AMBIENTAL)','CALCULO DIFERENCIAL E INTEGRAL I',
    'PROGRAMACAO I','LABORATORIO DE PROGRAMACAO I','ALGEBRA VETORIAL E GEOMETRIA ANALITICA',
    'LEITURA E PRODUCAO DE TEXTOS','INTRODUCAO A COMPUTACAO']
    notas_alunos, corr_disciplinas = entradas_regressao(alunos)


    # print(list(notas_alunos.columns.values))
    # notas_alunos.columns = [x.lower() for x in corrigir_nomes(list(notas_alunos.columns.values))]
    #


    disciplinas_regressoes, regressoes = regressao(periodo_disc, corr_disciplinas, notas_alunos)
    # vendo quais cadeiras sem prerrequisito já foram pagas pelo aluno
    # print(nota_aluno["Disciplina"].values)
    cadeiras_pagas_df = nota_aluno[["Disciplina"]]
    cadeiras_pagas = nota_aluno["Disciplina"].values
    cadeira_possivel = []

    for cadeira in sem_prerequisito:
        if not (cadeira in cadeiras_pagas):
            cadeira_possivel.append(cadeira)

    prox_possiveis_cadeiras = prox_cadeiras_nome(cadeiras_pagas_df, cadeira_possivel, pre_requisitos)
    # print("Cadeiras possíveis 2: ", prox_possiveis_cadeiras)
    # print(prox_possiveis_cadeiras)
    dict_notas = {}

    for cadeira in prox_possiveis_cadeiras:
        # if(cadeira == "FUNDAMENTOS DE FISICA MODERNA"):
        #     print("Cadeira: ", cadeira)
        #     print(disciplinas_regressoes[cadeira])
        #     print(set(cadeiras_pagas))
        #     print("=====================================")
        if set(disciplinas_regressoes[cadeira]).issubset(set(cadeiras_pagas)):

            notas_to_be_predicted = list()
            for ref in disciplinas_regressoes[cadeira]:
                for index in range(0,len(nota_aluno)):
                    disc = nota_aluno.Disciplina[index]
                    if disc == ref:
                        value = float(nota_aluno[mediaString][index].replace(',','.'))
                        notas_to_be_predicted.append(value)

            a = np.array(notas_to_be_predicted).reshape(1,-1)
            notas = regressoes[cadeira].predict(a)[0][0]
            dict_notas[cadeira] = math.floor(notas * 10)
    # dict_notas = {'Disciplina':dict_notas.keys(), 'Notas':dict_notas.values()}

    df = pd.Series(data = dict_notas).to_frame()
    df.columns = ["Probabilidade de Aprovação (%)"]
    df[disciplinaStringLower] = df.index
    df = df[[disciplinaStringLower,"Probabilidade de Aprovação (%)"]]
    return df

