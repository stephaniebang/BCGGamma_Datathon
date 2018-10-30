# BCGGamma_Datathon
Este repositório possui a solução do grupo 18 para o desafio da BCG de usar Ciência de Dados para gerar novos insights acionáveis para a Fundação Lemann.

## Bases de dados utilizadas
As bases de dados utilizadas foram:
- ideb_municipios_anosfinais2005_2017.csv
- ideb_uf_regioes_anosfinais2005_2017.csv
- escolas20170101.csv
- municipios20170101.csv
- docencias20170101000000000000 a docencias20170101000000000007
- Base_MUNIC_2017.xls

As bases foram tratadas e unidas para montar uma base final na visão município.
> A base Base_MUNIC_2017.xls foi obtida pelo IBGE, que pode ser encontrado no [site do IBGE](https://www.ibge.gov.br/estatisticas-novoportal/sociais/educacao/10586-pesquisa-de-informacoes-basicas-municipais.html?=&t=resultados)

> As bases Base_MUNIC_2017.xls e escolas20170101.csv foram modificadas para as bases Base_Munic_Resumo.xlsx e escolas20170101.csv, que podem ser encontradas no diretório  [data](https://github.com/stephaniebang/BCGGamma_Datathon/tree/master/data).

## Análise
A resposta do modelo foi categorizada, de modo que ela teria valor 1 caso a nota média do IDEB que o município obteve foi maior ou igual a 5, que é a meta desejada para 2017. Caso contrário, o valor da resposta do município seria 0.

Para a análise do modelo, foi escolhido um modelo eficiente de machine learning, a regressão logística. Devido a alta quantidade de variáveis incluídas na base de dados, poderiam existir problemas na interpretação dos dados, já que muitos coeficientes regressores poderiam causar um overfitting.

Sendo assim, foi usado o LASSO (Least Absolute Shrinkage and Selection Operator), um método de penalização para reduzir o peso de variáveis correlacionadas.
