# Educação Profissional e Tecnológica em Goiás: êxito, empregabilidade e renda

Código e dados da tese de doutorado de Yury Machado de Moura (Programa de Pós-Graduação em Ciência Política, Instituto de Ciência Política, Universidade de Brasília), orientada pelo Prof. Dr. Frederico Bertholini.

A análise tem três etapas:

1. **Etapa 1, êxito:** determinantes da conclusão com êxito dos cursos (regressões logísticas CM1 a CM6, modelos multinomiais e decomposição de Shapley do pseudo-R²).
2. **Etapa 2, empregabilidade e renda:** mudanças na renda, na ocupação e na formalização entre o ingresso e a pesquisa com egressos (modelos EM1 a EM4, com correção por IPW para a não resposta).
3. **Etapa 3, comparação com a PNAD Contínua:** efeito médio sobre os tratados (ATT) por *Coarsened Exact Matching*, com controles da PNAD Contínua (suplemento anual de Educação, 2022 a 2024), limites de Lee e especificações de robustez.

## Estrutura

```
Dados/
  dadosSE.csv     base dos ingressantes (55.860 matrículas)
  dadosEG.xlsx    pesquisa com egressos (3.944 respondentes)
  EG.xlsx         identificação da escola de cada egresso (id_alunos, id_escolas)
scripts/
  script_tese_2026-11-11.R      análises das três etapas
  graficos_tese_2026-11-11.py   gráficos da tese, a partir das saídas do script R
```

## Recorte da amostra

Entram as matrículas com ingresso entre 2022 e 2024 e questionário respondido até dezembro de 2024. Os demais filtros (cor/raça, renda, situação ocupacional e situação da matrícula informadas, idade de 16 a 100 anos) estão registrados passo a passo na aba `T1_fluxo_amostra` do arquivo de resultados.

## Como reproduzir

Rode os dois scripts a partir da pasta raiz do repositório, nesta ordem.

```
Rscript scripts/script_tese_2026-11-11.R
python scripts/graficos_tese_2026-11-11.py
```

O script R instala os pacotes que faltarem (dplyr, tidyr, openxlsx, readxl, lubridate, pscl, sandwich, lmtest, nnet, ggplot2, MatchIt, estimatr, PNADcIBGE). Na primeira execução, ele baixa os microdados da PNAD Contínua pelo pacote PNADcIBGE e os guarda em `Dados/pnadc_cache` (pasta fora do controle de versão). As saídas ficam em `Resultados_2026-11-11/`:

- `resultados_tese_2026-11.xlsx`: tabelas de todos os modelos, uma aba por análise;
- `complemento_graficos_2026-11.xlsx`: tabelas auxiliares usadas pelo script Python;
- `graficos/`: gráficos gerados em R;
- `sessionInfo.txt`: versões do R e dos pacotes usados.

O script Python requer numpy, pandas, matplotlib e openpyxl, e salva os gráficos em `Resultados_2026-11-11/graficos_python/`. Para usar outra pasta de resultados, passe o caminho como argumento: `python scripts/graficos_tese_2026-11-11.py <pasta>`.

## Anonimização

Os dados foram anonimizados antes da publicação:

- `id_alunos` e `id_escolas` foram substituídos por códigos sequenciais, mantendo a correspondência entre os arquivos;
- a data de nascimento foi removida (a idade continua disponível);
- a coluna de índice de linhas foi removida da base dos ingressantes.

Nenhuma dessas alterações muda os resultados, porque os scripts usam os identificadores apenas para cruzar as bases e agrupar os erros-padrão.
