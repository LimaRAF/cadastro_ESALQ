#' Get List of Names for DB Update
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' 
#' @details
#' Creation date: 28 Fev 2024
#' 

cat("\nRunning script:", script, "...", "\n")

# LOADING THE MOST UP-TO-DATE VERSION -------------------------------
## Getting all objects in the workspace before the script starts
obj.to.keep <- ls()

## Loading the file
db <- readRDS("./data/raw-data/cadastro_arvores_esalq.RDS")

# db <- as.data.frame(readxl::read_excel("./data/raw-data/species_book_indexes.xlsx"))
# toto <- plantR::fixSpecies(db, "ESPÉCIE") 
# db$Determinacao <- toto$scientificName.new

## Getting only the IDs with confirmed identifications
db1 <- db[!is.na(db$Determinacao), c("NumeroPlaca", 
                                     "Determinacao", 
                                     "Autoria")]

# NAME CHECKING WITH FLORA DO BRASIL -------------------------------
## Checking all new species names (with Flora do Brasil)
name.check <- plantR::formatTax(tax = db1, tax.name = "Determinacao", 
                                author.name = "Autoria", sug.dist = 0.9,
                                clean.indet = TRUE, drop = "")

# Algum nome que na planilha é um possível problema? Corrigir a coluna determinacao da planilha
cols2inspect <- c("Determinacao","suggestedName", "tax.notes")
# check_these <- !name.check$tax.notes %in% c("name accepted", NA, "not found")
# if (any(check_these))
#   unique(name.check[check_these, cols2inspect])

# Algum nome com grafia errada? Corrigir a coluna determinacao da planilha
check_these <- grepl("misspelled", name.check$tax.notes)
if (any(check_these)) {
  print("Nomes com problemas de grafia na planilha (corrigir):\n")
  unique(name.check[check_these, cols2inspect])
  stopifnot(all(!check_these)) # all check_these must be FALSE
}

# Inspecionando o resultado final
cols2inspect <- c(cols2inspect, "id", "match_type")
# check_these <- name.check$multiple_match %in% TRUE
# if (any(check_these)) {
#     unique(name.check[check_these, cols2inspect])
# }

# check_these <- !is.na(name.check$fuzzy_dist_name) & 
#                   name.check$fuzzy_dist_name > 0.1
# if (any(check_these)) {
#     unique(name.check[check_these, cols2inspect])
# }

#unique(name.check[, cols2inspect])


# NAME CHECKING WITH WORLD FLORA ONLINE -------------------------------
# Lista de nomes nao encontrados na Flora do Brasil online
check_these <- name.check$tax.notes %in% c("not found")
if (any(check_these)) {
  names1 <- name.check[name.check$tax.notes %in% c("not found"), ]
  # unique(names1[,cols2inspect])
  name.check1 <- plantR::formatTax(tax = names1[,c(1:3)], tax.name = "Determinacao", 
                                   author.name = "Autoria", sug.dist = 0.9,
                                   clean.indet = TRUE, db = plantRdata::wfoNames, 
                                   drop = "", )
  name.check1$family.new[name.check1$Determinacao %in% "Acrocarpus fraxinifolius"] <- 
    "Fabaceae"
  
  cols2rm <- c(names(db1), "scientificName.new", 
               "scientificNameAuthorship.new", "scientificNameStatus")
  cols2rep <- names(name.check)[!names(name.check) %in% cols2rm]
  name.check[check_these, cols2rep] <- 
    name.check1[, cols2rep]
}

# BINDING BACK INTO THE FULL DB -------------------------------
cols2keep <- names(name.check)[!names(name.check) %in% names(db1)]
db2 <- dplyr::left_join(db1, 
                        unique(name.check[c(cols2keep, "Determinacao")]), 
                        by = "Determinacao")
names(db2)[which(names(db2) %in% "suggestedName")] <- 
  "nome.cientifico.correto"

## Saving the result
saveRDS(db2, "./data/derived-data/cadastro_arvores_esalq_nome_cientifico.RDS")
# writexl::write_xlsx(db2, "./data/derived-data/species_book_indexes_checked.xlsx")

## Removing all objects created in the script
all.objs <- ls()
rm.objs <- all.objs[!all.objs %in% obj.to.keep]
rm(list = rm.objs)
cat("Done with script:", script, "\n")