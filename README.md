# Machine learning e análise de sentimento: Projetando o risco e insolvência bancária

A principal motivação deste artigo é utilizar técnicas de machine learning para construir uma nova métrica de classificação de risco de insolvência para os bancos negociados na B3. Em seguida, será utilizado um conjunto de modelos de predição para projetar a classificação de risco destas instituições. Convencionalmente, a literatura analisa o risco de insolvência bancária a partir dos dados contábeis e variáveis macroeconômicas. Além dessas variáveis, esse artigo irá construir uma série de sentimento do gestor da instituição bancária, via relatórios trimestrais (ITR), e essa será utilizada para melhorar a acurácia das previsões do risco bancário. Os resultados indicam que a classificação de risco bancário, via algoritmo k-means, foi capaz de classificar 17\% da amostra no grupo de maior risco (1), enquanto 83\% da amostra ficou no grupo de menor risco de falência (0). Utilizando a métrica do Z-score verificamos que 65\% da amostra faz parte do grupo de baixo risco e 35\% da amostra no grupo de risco elevado. Desse modo, o algoritmo k-means é mais rigoroso em classificar um banco na categoria de maior risco. Na sequência utilizamos os dados já descritos para projetar o risco de insolvência bancária. Os resultados desta etapa mostraram que o modelo de árvore de decisão apresentou o melhor desempenho para a amostra de teste. Além disso, constatou-se que a inclusão da variável de sentimento bancário foi capaz de melhorar o desempenho dos modelos de previsão, principalmente, quando o sentimento bancário é construído a partir de um dicionário variante no tempo.
