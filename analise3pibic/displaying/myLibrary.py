# definição das cadeiras que o aluno pode se matricular a partir do histórico
def prox_cadeiras(historico, sem_prerequisito):
    #seleciona na coluna de prerequisitos as cadeiras que já foram pagas
    possibilidades = prerequisitos.loc[prerequisitos['codigo_prerequisito'].isin(historico)]
    possibilidades = possibilidades[['codigo_disciplina']]
    for cadeira in possibilidades.codigo_disciplina:
        if not cadeira_paga(cadeira,historico):
            prerequisitos_cadeira = prerequisitos.loc[prerequisitos['codigo_disciplina'] == cadeira]

            pre=prerequisitos_cadeira['codigo_prerequisito'].isin(historico)
            prerequisitos_pagos = True
            for  c in pre:
                if c == False:
                    prerequisitos_pagos = False
                    break
            if prerequisitos_pagos:
                sem_prerequisito.append(cadeira)
    return sem_prerequisito

# definição das cadeiras que o aluno pode se matricular a partir do histórico
def prox_cadeiras_nome(historico, proximas_cadeiras, prerrequisitos):

    possibilidades = prerrequisitos.loc[prerrequisitos[prerrequisitos.columns[1]].isin(historico.Disciplina)]
    possibilidades = possibilidades[[possibilidades.columns[0]]]

    for cadeira in list(possibilidades[possibilidades.columns[0]]):

        if not cadeira_paga(cadeira,historico):
            prerrequisitos_cadeira = prerrequisitos.loc[prerrequisitos[prerrequisitos.columns[0]] == cadeira]
            pre=prerrequisitos_cadeira[prerrequisitos_cadeira.columns[1]].isin(historico.Disciplina)
            
            prerrequisitos_pagos = True
            for c in pre:
                if c == False:
                    prerrequisitos_pagos = False
                    break
            if prerrequisitos_pagos and cadeira not in proximas_cadeiras:
                proximas_cadeiras.append(cadeira)
    return proximas_cadeiras

# retorna quais cadeiras o aluno já foi aprovado
def cadeira_paga(cadeira, historico):
    return cadeira in set(historico.Disciplina)

# retorna o código de uma cadeira a partir do seu nome
def nomeCadeira_para_cod(conj_cadeiras, cod_nome_df):
    cod_conj_cadeiras = []
    for cadeira in conj_cadeiras:
        aux = cod_nome_df.loc[cod_nome_df['Nome_Disciplina'] == cadeira]
        aux = aux[['Cod_Disciplina']]
        if not aux.empty:
            cod_conj_cadeiras.append(aux.iloc[0]['Cod_Disciplina'])
    return cod_conj_cadeiras

# retorna o nome de uma cadeira a partir do seu código
def cod_para_nomeCadeira(conj_cadeiras, cod_nome_df):
    cod_conj_cadeiras = []
    for cadeira in conj_cadeiras:
        aux = cod_nome_df.loc[cod_nome_df['Cod_Disciplina'] == cadeira]
        aux = aux[['Nome_Disciplina']]
    
        if not aux.empty:
            cod_conj_cadeiras.append(aux.iloc[0]['Nome_Disciplina'])
    return cod_conj_cadeiras
