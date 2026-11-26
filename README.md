# Escolaridade do eleitorado e voto presidencial por seção eleitoral (Brasil, 2022)

Trabalho final de Econometria I, curso de Economia da FGV EESP.
Autores: Thomás Mariano Dardenne Tavares e Gustavo Lamarca Moreira.

## O trabalho

**Pergunta:** seções eleitorais com eleitorado mais escolarizado deram menor proporção de votos válidos a Lula no 2º turno de 2022?

- **Unidade de análise:** seção eleitoral. Amostra final de 421.305 seções em 5.319 municípios.
- **Variável dependente:** % de votos válidos em Lula no 2º turno = 100 × votos Lula / (votos Lula + votos Bolsonaro).
- **Variáveis de interesse:** % de eleitores da seção em cada grau de escolaridade. A referência é o ensino médio completo.
- **Controles:**
  - faixas etárias (referência: 16 a 24 anos);
  - % de mulheres;
  - log do número de eleitores;
  - % de escolaridade não informada;
  - log da renda média do responsável no setor censitário do local de votação (Censo 2022);
  - latitude e longitude, UF ou efeitos fixos de município ou de local de votação, conforme o modelo.
- **Estimação:** MQO com erros-padrão agrupados por município.

Como os dados são agregados por seção, os resultados descrevem seções, e não o voto de cada eleitor (inferência ecológica).

## Dados

| Base | Fonte | Uso |
|---|---|---|
| Votação por seção, presidente, 1º e 2º turnos (`votacao_secao_2022_BR.zip`) | TSE, Dados Abertos | Votos de Lula e Bolsonaro |
| Perfil do eleitorado por seção (`perfil_eleitor_secao_2022_<UF>.zip`, um por UF) | TSE, Dados Abertos | Escolaridade, idade e gênero |
| Eleitorado por local de votação (`eleitorado_local_votacao_2022.zip`) | TSE, Dados Abertos | Local, latitude, longitude e seções agregadas |
| Rendimento do responsável por setor censitário (`Agregados_por_setores_renda_responsavel_BR_20260508_csv.zip`) | IBGE, Censo 2022 | Renda média do setor (variável V06004) |
| Malha de setores censitários (`<UF>_setores_CD2022.gpkg`, uma por UF) | IBGE, Censo 2022 | Polígonos dos setores para a junção espacial |

**Bases tratadas** (geradas pelo código):
- `base_secao_2022.rds` (e `.csv.gz`): as três bases do TSE juntas, uma linha por seção, já com a renda do setor.
- `base_analise.rds`: a base com as variáveis da regressão e a marcação da amostra.
- `dic_escolaridade.csv`, `dic_genero.csv` e `dic_faixa_etaria.csv`: os códigos usados pelo TSE.

As bases são grandes demais para o repositório e ficam na aba
[Releases](https://github.com/thomasdtavares/trabalho-de-econometria/releases/tag/dados).
Para rodar o código sem baixar tudo de novo, coloque as bases cruas em `dados/brutos/` e as tratadas em `dados/`. Se as pastas estiverem vazias, o código baixa as bases cruas sozinho dos sites do TSE (`https://cdn.tse.jus.br/estatistica/sead/odsele/`) e do IBGE (`https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/`). A primeira execução demora.

## O código (`cod_econo.R`)

O script roda do início ao fim e está dividido em partes:

| Parte | O que faz |
|---|---|
| 0 | Pacotes, pastas e funções auxiliares (download e leitura dos zips do TSE) |
| 1 | Importa a votação por seção para presidente, 1º e 2º turnos |
| 2 | Importa o perfil do eleitorado por seção (um zip por UF) e conta os eleitores por escolaridade, faixa etária e gênero |
| 3 | Importa o eleitorado por local de votação (local, latitude, longitude e seções agregadas) |
| 4 | Junta as três bases pela chave UF + município + zona + seção |
| 5 | Confere a soma dos votos da base com o resultado oficial do TSE (a diferença é zero nos dois turnos) |
| 5B | Associa cada local de votação ao setor censitário do IBGE em que ele está (ponto dentro do polígono, pacote `sf`) e traz a renda do setor |
| 6 | Salva a base tratada |
| 7 | Define a amostra e cria as variáveis: exclui seções que receberam eleitores de seções agregadas e seções sem renda, calcula os percentuais e o voto em Lula, e cria log da renda, região, interações e variáveis centradas no município |
| 8 | Estatísticas descritivas, histograma, binscatter e mapas |
| 9 | Regressões M1 a M10, testes F, Breusch-Pagan, VIF, conferência com o `fixest`, testes de especificação e robustez somando o perfil das seções agregadas ao da seção principal |
| 10 | Tabelas (stargazer) e gráfico de coeficientes |

### Modelos

| Modelo | Especificação |
|---|---|
| M1 | Só escolaridade |
| M2 | + controles demográficos |
| M3 | + log da renda do setor |
| M4 | + latitude e longitude |
| M5 | + UF |
| M6 | Efeitos fixos de município, sem renda |
| M7 | Efeitos fixos de município, com renda (**modelo principal**) |
| M8 | M7 + interações de superior completo e analfabeto com Nordeste |
| M9a / M9b | M6 e M7 sem o Nordeste |
| M10 | Efeitos fixos de local de votação |

Os efeitos fixos são estimados subtraindo de cada variável a média do seu município ou local (teorema de Frisch-Waugh-Lovell). O resultado é conferido com `fixest::feols`.

## Como rodar

1. Abra o R com a pasta do repositório como diretório de trabalho.
2. Instale os pacotes:
   ```r
   install.packages(c("data.table", "lmtest", "sandwich", "car", "ggplot2",
                      "patchwork", "stargazer", "sf", "fixest"))
   ```
3. Rode `cod_econo.R` do início ao fim. As tabelas (`.html` e `.csv`) e as figuras (`.png`) são salvas na pasta do projeto.
