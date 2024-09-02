
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- cadastro_ESALQ <img src="man/figures/compendium-sticker.png" align="right" style="float:right; height:120px;"/> -->

# Cadastro das árvores da ESALQ

<!-- badges: start -->

[![License: GPL (\>=
2)](https://img.shields.io/badge/License-GPL%20%28%3E%3D%202%29-blue.svg)](https://choosealicense.com/licenses/gpl-2.0/)
<!-- badges: end -->

<p align="left">
• <a href="#overview">Overview</a><br> •
<a href="#conteúdo">Conteúdo</a><br> • <a href="#uso">Uso</a><br> •
<a href="#equipe">Equipe</a><br> •
<a href="#agradecimentos">Agradecimentos</a><br>
</p>

## Overview

Repositório para limpar e atualizar o **cadastro das árvores da ESALQ**.

## Conteúdo

Este repositório esta organizado da seguinte forma:

- [`analyses/`](https://github.com/LimaRAF/cadastro_ESALQ/tree/master/analyses):
  pasta contendo os scripts criados especialmente para esse repositório

- [`data/`](https://github.com/LimaRAF/cadastro_ESALQ/tree/master/data):
  pasta contendo os dados brutos e editados necessários para rodar os
  scripts do repositório. Na verdade os dados brutos, estão todos online
  em pastas do Google Sheets [nesta
  pasta](https://drive.google.com/drive/folders/1dnMfdS6csM8ZJIffZ6FUPPsXN6QgGzPY)

- [`make.R`](https://github.com/LimaRAF/cadastro_ESALQ/tree/master/make.R):
  script do R para rodar todos os códigos, do download da planilha a
  criação do script SQL para envio ao Jefferson

- [`output/`](https://github.com/LimaRAF/cadastro_ESALQ/tree/master/output):
  pasta na qual onde é salva a versão mais atual do script SQL

- [`R/`](https://github.com/LimaRAF/cadastro_ESALQ/tree/master/R):
  contém as funções internas usadas no repositório

## Uso

Se tudo correr bem, você deve apenar usar o seguinte código

``` r
source("make.R")
```

**Notas**

Ao executar o script `make.R`: - Todos os pacotes listados no
`DESCRIPTION` serão instalados, se necessário - Todos os pacotes e
funções do R serão carregados - Algumas análises listadas no `make.R`
podem demorar

## Equipe

- Renato A. Ferreira de Lima (LCB/ESALQ)
- André Montanari (Eng. Agronômica/ESALQ)
- Pedro Neves (Eng. Florestal/ESALQ)

## Agradecimentos

- PUB
- FEALQ
