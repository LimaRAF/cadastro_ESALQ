#' Building the SQL Code itself for DB Update
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' 
#' @details
#' Creation date: 28 Fev 2024
#' 

# cat("\nRunning script:", script, "...")
cat(paste0("\033[0;", 31, "m", "\nRunning script: ", script, "...","\033[0m","\n"))

obj.to.keep <- ls()

## Getting the most up-to-date full db
db <- readRDS("./data/derived-data/cadastro_arvores_esalq_nome_cientifico_popular.RDS")

## Creating the generic code lines
standard_text <-
  "UPDATE tabela_mestre_link_web SET genero = 'GENERO', nome = 'NOME_POP', especieconf = 'sim' WHERE narvore = 'TREE_ID';"
script_sql <- rep(standard_text, dim(db)[1])

## Replacing the info of each tree
for(i in 1:length(script_sql)) {
  ind.i <- db[i, ]
  script_sql[i] <- gsub("GENERO", ind.i$nome.cientifico.correto, 
                        script_sql[i], perl = TRUE)
  
  if (!is.na(ind.i$nome.popular)) {
    script_sql[i] <- gsub("NOME_POP", ind.i$nome.popular, 
                          script_sql[i], perl = TRUE)
  } else {
    script_sql[i] <- gsub("NOME_POP", '', script_sql[i], perl = TRUE)
  }
    
  script_sql[i] <- gsub("TREE_ID", ind.i$NumeroPlaca, script_sql[i], 
                        perl = TRUE)
}

## Saving
data.versao <- as.character(format(Sys.Date(), "%Y_%m"))
save.path <- paste0("./outputs/script_", data.versao, ".txt")
writeLines(script_sql, save.path)
cat(paste0("\033[0;", 36, "m", "\nDone with all scripts!\nCheck the folder 'output' for the result","\033[0m"))
# cat("\nDone with all scripts!\nCheck the folder 'output' for the result")
