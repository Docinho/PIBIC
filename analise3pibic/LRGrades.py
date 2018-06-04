import pandas as pd
from sklearn import linear_model
import numpy as np
exec(open("myLibrary.py").read())

def entradas_regressao(alunos):
	# ajustando data frame que servirá de entrada
	alunos_cc = alunos.query("Cod_Curso == 14102100 & Tipo == 'Obrigatória' ")
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
    for disciplina in periodo_disc["DISCIPLINA"]:
        # pegando o periodo da disciplina da iteração
        periodo = periodo_disc.loc[periodo_disc["DISCIPLINA"] == disciplina]['PERIODO'].values[0]
        # pegando as disciplinas que são do mesmo periodo ou anteriores a disciplina em questão
        disciplinas_regressao = periodo_disc.loc[periodo_disc["PERIODO"]<=periodo]["DISCIPLINA"].values
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
    aluno = nota_aluno.pivot(index="Matricula", values="Média", columns="Disciplina")
    # informações gerais dos alunos
    alunos = pd.read_csv("../alunosUFCGAnon.csv")
    pre_requisitos = pd.read_csv("../preRequisitos.csv")
    periodo_disc = pd.read_csv("../periodoDisciplinas.csv")

    sem_prerequisito = ['DIREITO E CIDADANIA','GERÊNCIA DA INFORMAÇÃO','INFORMÁTICA E SOCIEDADE','MATEMÁTICA DISCRETA',
    'METODOLOGIA CIENTÍFICA','SEMINÁRIOS (EDUCAÇÃO AMBIENTAL)','CALCULO DIFERENCIAL E INTEGRAL I',
    'PROGRAMAÇÃO I','LABORATÓRIO DE PROGRAMAÇÃO I','ALGEBRA VETORIAL E GEOMETRIA ANALÍTICA',
    'LEITURA E PRODUCAO DE TEXTOS','INTRODUÇÃO A COMPUTAÇÃO']
    notas_alunos, corr_disciplinas = entradas_regressao(alunos)

    disciplinas_regressoes, regressoes = regressao(periodo_disc, corr_disciplinas, notas_alunos)
    # vendo quais cadeiras sem prerrequisito já foram pagas pelo aluno
    cadeiras_pagas = nota_aluno["Disciplina"]
    cadeira_possivel = []
    print(pre_requisitos)
    for cadeira in sem_prerequisito:
        if not (cadeira in cadeiras_pagas.values):
            cadeira_possivel.append(cadeira)

    prox_possiveis_cadeiras = prox_cadeiras_nome(cadeiras_pagas, cadeira_possivel, pre_requisitos)
    print("Cadeiras possíveis 2: ", prox_possiveis_cadeiras)
    dict_notas = {}

    for cadeira in prox_possiveis_cadeiras:
        if set(disciplinas_regressoes[cadeira]).issubset(set(cadeiras_pagas)):

            notas_to_be_predicted = list()
            for ref in disciplinas_regressoes[cadeira]:
                for index in range(0,len(nota_aluno)):
                    disc = nota_aluno.Disciplina[index]
                    if disc == ref:
                        value = float(nota_aluno["Média"][index].replace(',','.'))
                        notas_to_be_predicted.append(value)

            a = np.array(notas_to_be_predicted).reshape(1,-1)
            notas = regressoes[cadeira].predict(a)
            dict_notas[cadeira] = notas
    return pd.DataFrame({'Disciplina':dict_notas.keys(), 'Notas':dict_notas.values()})

predicao()
