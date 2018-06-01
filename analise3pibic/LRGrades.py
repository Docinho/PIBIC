import pandas as pd
from sklearn import linear_model
import numpy as np
import operator
#from rpy2.robjects import pandas2ri
#pandas2ri.activate()
import os
exec(open("myLibrary.py").read())
# import myLibrary // quando roda diretamente no terminal



def predicao(nota_aluno):
    alunos = pd.read_csv("../alunosUFCGAnon.csv")
    pre_requisitos = pd.read_csv("../preRequisitos.csv")
    periodo_disc = pd.read_csv("../periodoDisciplinas.csv")

    # ajustando data frame que servirá de entrada
    alunos_cc = alunos.query("Cod_Curso == 14102100 & Tipo == 'Obrigatória' ")
    # alunos_cc.sort_values('Media_Disciplina', ascending=False).drop_duplicates(['Nome_Disciplina','Matricula'])
    COMPUTACAOtdf = pd.pivot_table(alunos_cc, values = 'Media_Disciplina', index = ['Nome_Disciplina'], columns = 'Matricula')
    notas_alunos = alunos_cc
    notas_alunos['Matricula'] = alunos_cc['Matricula'].map(lambda x: x.lstrip('B'))
    notas_alunos = pd.pivot_table(notas_alunos, values = 'Media_Disciplina', index = ['Matricula'], columns = 'Nome_Disciplina')
    # cadeiras que não exigem prerrequisito
    sem_prerequisito = ['DIREITO E CIDADANIA','GERÊNCIA DA INFORMAÇÃO','INFORMÁTICA E SOCIEDADE','MATEMÁTICA DISCRETA',
                        'METODOLOGIA CIENTÍFICA','SEMINÁRIOS (EDUCAÇÃO AMBIENTAL)','CALCULO DIFERENCIAL E INTEGRAL I',
                         'PROGRAMAÇÃO I','LABORATÓRIO DE PROGRAMAÇÃO I','ÁLGEBRA VETORIAL E GEOMETRIA ANALÍTICA',
                           'LEITURA E PRODUCAO DE TEXTOS','INTRODUÇÃO A COMPUTAÇÃO']

    notas_aprovados = alunos_cc.loc[alunos_cc.Situacao == 'Aprovado']
    notas_aprovados['Matricula'] = notas_aprovados['Matricula'].map(lambda x: x.lstrip('B'))
    notas_aprovados = pd.pivot_table(notas_aprovados, values = 'Media_Disciplina', index = ['Matricula'], columns = 'Nome_Disciplina')

    #%run util.ipynb
    corr_disciplinas = notas_alunos.corr() 

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

    sem_prerequisito = ['DIREITO E CIDADANIA','GERÊNCIA DA INFORMAÇÃO','INFORMÁTICA E SOCIEDADE','MATEMÁTICA DISCRETA',
                            'METODOLOGIA CIENTÍFICA','SEMINÁRIOS (EDUCAÇÃO AMBIENTAL)','CALCULO DIFERENCIAL E INTEGRAL I',
                            'PROGRAMAÇÃO I','LABORATÓRIO DE PROGRAMAÇÃO I','ÁLGEBRA VETORIAL E GEOMETRIA ANALÍTICA',
                            'LEITURA E PRODUCAO DE TEXTOS','INTRODUÇÃO A COMPUTAÇÃO']

        # coletando quais cadeiras o aluno já foi aprovado
    cadeiras_aluno = pd.DataFrame(data=nota_aluno)
    cadeiras_pagas = cadeiras_aluno.Disciplina
    cadeira_possivel = []

        # vendo quais cadeiras sem prerrequisito já foram pagas pelo aluno
    for cadeira in sem_prerequisito:
        if not cadeira in cadeiras_pagas.index:
            cadeira_possivel.append(cadeira)
    prox_possiveis_cadeiras = prox_cadeiras_nome(cadeiras_pagas, cadeira_possivel, pre_requisitos)

    dict_notas = {}
    for cadeira in prox_possiveis_cadeiras:
        if np.sum(np.isin(disciplinas_regressoes[cadeira], cadeiras_pagas)) == len(disciplinas_regressoes[cadeira]):

            #notas = [regressoes[cadeira].predict(test) for test in disciplinas_regressoes[cadeira]]

            notas_to_be_predicted = list()
            for ref in disciplinas_regressoes[cadeira]:
                for index in range(0,len(nota_aluno)):
                    disc = nota_aluno.Disciplina[index]
                    if disc == ref:
                        value = float(nota_aluno.Média[index].replace(',','.'))
                        notas_to_be_predicted.append(value)

            a = np.array(notas_to_be_predicted).reshape(1,-1)
            notas = regressoes[cadeira].predict(a)
            dict_notas[cadeira] = notas
            #info = np.array(cadeiras_aluno[disciplinas_regressoes[cadeira]].values.reshape(1,-1))
            #dict_notas[cadeira]=(regressoes[cadeira].predict(info))
            


    return nota_aluno

notas = pd.read_csv("../docinho.csv", sep=',')
#print(notas.Disciplina[2])
#LEITURA E PRODUCAO DE TEXTOS
predicao(notas)