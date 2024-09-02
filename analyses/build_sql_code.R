#' Building the SQL Code itself for DB Update
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' 
#' @details
#' Creation date: 28 Fev 2024
#' 
cat("\nRunning script:", script, "...")
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
data.versao <- as.character(format(Sys.Date(), "%Y_%m"))
save.path <- paste0("./outputs/script_", data.versao, ".txt")
writeLines(script, save.path)
cat("\nDone with all scripts!\nCheck the folder 'output' for the result")
