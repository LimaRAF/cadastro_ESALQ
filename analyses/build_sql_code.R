#' Building the SQL Code itself for DB Update
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' 
#' @details
#' Creation date: 28 Fev 2024
#' 
obj.to.keep <- ls()

## Getting the most up-to-date full db
db <- readRDS("./data/derived-data/cadastro_arvores_esalq_nome_cientifico_popular.RDS")

## Creating the generic code lines
standard_text <-
  "UPDATE tabela_mestre_link_web SET genero = 'GENERO', nome = 'NOME_POP', especieconf = 'sim' WHERE narvore = 'TREE_ID';"
script <- rep(standard_text, dim(db)[1])

## Replacing the info of each tree
for(i in 1:length(script)) {
  ind.i <- db[i, ]
  script[i] <- gsub("GENERO", ind.i$nome.cientifico.correto, script[i], perl = TRUE)
  
  if (!is.na(ind.i$nome.popular)) {
    script[i] <- gsub("NOME_POP", ind.i$nome.popular, script[i], perl = TRUE)
  } else {
    script[i] <- gsub("NOME_POP", '', script[i], perl = TRUE)
  }
    
  script[i] <- gsub("TREE_ID", ind.i$NumeroPlaca, script[i], perl = TRUE)
}

## Saving
data.versao <- as.character(format(Sys.Date(), "%b_%Y"))
save.path <- paste0("./outputs/script_", data.versao, ".txt")
writeLines(script, save.path)


## -------------------------------------------------------------------
#### OLD CODES ####
# ## Adding missing vernacular names to the update script
# script <- readLines("script.txt")
# spp.script <- lapply(strsplit(script, ", "), head, 1)
# spp.script <- lapply(spp.script, strsplit, "= ")
# spp.script <- unlist(lapply(spp.script, function(x) tail(x[[1]], 1)))
# spp.script <- gsub("'", "", spp.script)
# 
# unique(spp.script[!(spp.script %in% bd.names2$Gênero | spp.script %in% bd.names2$search.str)])
# 
# for(i in 1:length(spp.script)) {
#   spp.i <- spp.script[i]
#   common.i <- tail(bd.names2$Nome[bd.names2$Gênero %in% spp.i], 1)
#   if (length(common.i) > 0) {
#     replace_these <- grepl(spp.i, script)
#     script[replace_these] <- 
#       gsub("qual é\\?", common.i, script[replace_these])
#   } else {
#     common.i <- tail(bd.names2$Nome[bd.names2$search.str %in% spp.i], 1)
#     replace_these <- grepl(spp.i, script)
#     script[replace_these] <- 
#       gsub("qual é\\?", common.i, script[replace_these])
#   }
# }

