# Estimando ponto ideal com Stan

library(devtools)
install_github("leobarone/bRasilLegis")
library(XML)
library(httr)
library(bRasilLegis)
library(dplyr)
library(stringr)

## coletando todas as proposições votadas de 1995 a 2016
listaProposicoes <- list()
for ( i in 1995:2016) {
  listaProposicoes[[i]] <- listarProposicoesVotadasEmPlenario(i)
}

## juntando num data.frame
proposicoesVotadas <- bind_rows(listaProposicoes)


## pegando apenas info que preciso, e guardando num data.frame
tipo <- str_match(proposicoesVotadas$nomeProposicao, "[A-Z]+")
numero <- str_match(proposicoesVotadas$nomeProposicao, "[0-9]+")
ano <- str_match(proposicoesVotadas$nomeProposicao, "/[0-9]+")
str_sub(ano, 1, 1) <- "" ## removendo barra

# data frame com proposições, ano de proposição e data de votação
proposicoes <- data.frame(tipo=tipo, numero = numero, ano = ano, dataVotacao = proposicoesVotadas$dataVotacao)

# retirando duplicados
proposicoes <- proposicoes %>%
  distinct(tipo, numero, ano, dataVotacao, .keep_all = T)

## agora, pegando roll call data para cada votação



votesPec358.2013 <- obterVotacaoProposicao(tipo = "PEC", numero = 171,ano = 1993)
votesPec358.2013 <- votesPec358.2013 %>%
  mutate(Resumo = iconv(Resumo, "UTF-8", "LATIN1"),
         ObjVotacao = iconv(ObjVotacao, "UTF-8", "LATIN1"),
         Nome = iconv(Nome, "UTF-8", "LATIN1"),
         Voto = iconv(Voto, "UTF-8", "LATIN1"))
         
         
