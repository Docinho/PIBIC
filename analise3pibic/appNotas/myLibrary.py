## Identifica as cadeiras que podem ser passadas para o algoritmo de predicao a partir das disciplinas ja cursadas pelo aluno


# definição das cadeiras que o aluno pode se matricular a partir do histórico
def prox_cadeiras_nome(historico, proximas_cadeiras, prerrequisitos):

    # identifica possíveis cadeiras que podem ser cursadas a partir dos prerrequisitos já cursados
    possibilidades = prerrequisitos.loc[prerrequisitos[prerrequisitos.columns[1]].isin(historico.Disciplina)]
    possibilidades = possibilidades[[possibilidades.columns[0]]]

    for cadeira in list(possibilidades[possibilidades.columns[0]]):
        # verifica se a cadeira da iteracao ja foi paga
        if not cadeira_paga(cadeira,historico):
            prerrequisitos_cadeira = prerrequisitos.loc[prerrequisitos[prerrequisitos.columns[0]] == cadeira]
            pre=prerrequisitos_cadeira[prerrequisitos_cadeira.columns[1]].isin(historico.Disciplina)

            # checa se todos os prerrequisitos necessarios foram pagos
            prerrequisitos_pagos = True
            for c in pre:
                if c == False:
                    prerrequisitos_pagos = False
                    break
            # identifica a cadeira como possível de ser cursada se todos os prerrequisitos foram pagos
            if prerrequisitos_pagos and cadeira not in proximas_cadeiras:
                proximas_cadeiras.append(cadeira)
    return proximas_cadeiras

# retorna se um aluno já foi aprovado em uma determinada cadeira
def cadeira_paga(cadeira, historico):
    return cadeira in set(historico.Disciplina)
