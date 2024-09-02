#' Obtaining and Adding Common Names to the DB
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' 
#' @details
#' Creation date: 28 Fev 2024
#'
#'

cat(paste0("\033[0;", 31, "m", "\nRunning script: ", script, "...","\033[0m","\n"))
# cat("\nRunning script:", script, "...", "\n")

## Getting all objects in the workspace before the script starts
obj.to.keep <- ls()

## Getting the most up-to-date full db
db3 <- readRDS("./data/derived-data/cadastro_arvores_esalq_nome_cientifico.RDS")

## Getting all names from the ESALQ Tree database
db <- as.data.frame(readxl::read_excel("./data/raw-data/Tabela_mestre_190315.xlsx"))
especies <- sort(unique(db$Gênero))
all.new <- sort(unique(db3$nome.cientifico.correto[!db3$nome.cientifico.correto %in% especies]))
new.spp <- c("Acrocarpus fraxinifolius", "Alchornea cf. glandulosa", 
             "Alchornea glandulosa", "Aleurites moluccanus", 
             "Callistemon salignus", "Cedrela odorata", 
             "Ceiba glaziovii", "Chrysophyllum cainito", 
             "Cinnamomum verum", "Cochlospermum vitifolium", 
             "Copaifera langsdorffii", "Cordia myxa", 
             "Cupressus funebris", "Erythrina velutina", 
             "Erythrina verna", "Ficus eximia", 
             "Galesia integrifolia", "Handroanthus cf. vellosoi", 
             "Inga laurina", "Moquilea tomentosa", 
             "Myroxylon peruiferum", "Parmentiera sp.", 
             "Syzygium cf samarangense", "Terminalia catappa",
             "Toona ciliata", "Trichilia clausenii", 
             "Zeyheria tuberculosa")
especies <- sort(unique(c(especies, all.new, new.spp)))
tmp <- flora::get.taxa(especies, drop = c(""), suggest.names = TRUE, 
                       suggestion.distance = 0.9, vernacular = TRUE)

### minor fixes
tmp$vernacular.name <- 
  gsub("pao \\/ pau \\/ pao", "Pau", tmp$vernacular.name, ignore.case = TRUE)
tmp$vernacular.name <- 
  gsub("pao \\/ pau", "Pau", tmp$vernacular.name, ignore.case = TRUE)
tmp$vernacular.name <- 
  gsub("cumixá/cumichá", "Cumixá", tmp$vernacular.name, ignore.case = TRUE)
tmp$vernacular.name <- 
  gsub("Caixeta/Caixeto", "Caixeta", tmp$vernacular.name, ignore.case = TRUE)
tmp$vernacular.name <- 
  gsub("norte/AM", "AM", tmp$vernacular.name, ignore.case = TRUE)
tmp$vernacular.name <- 
  gsub("Bahia/Rio de Janeiro", "BA, RJ", tmp$vernacular.name, ignore.case = TRUE)


## Breaking the info
tutu <- strsplit(tmp$vernacular.name, "\\|")
tutu <- lapply(tutu, function(x) gsub("\\s+", " ", x, perl = TRUE))
tutu <- lapply(tutu, function(x) gsub("^ | $", "", x, perl = TRUE))
tutu <- lapply(tutu, function(x) strsplit(x, '\\/'))
empty.vec <- c(rep(NA, 3), TRUE)
nomes <- c("name", "language", "region")
names(empty.vec) <- c(nomes, "decisao")
for (i in seq_along(tutu)) {
  #cat(i, "\n")
  if (all(is.na(tutu[[i]])))  { 
    tutu[[i]] <- empty.vec 
  } else {
    tmp1 <- lapply(tutu[[i]], function(x) setNames(x, nomes))
    tmp2 <- as.data.frame(dplyr::bind_rows(tmp1))
    tmp2$decisao <- FALSE
    if (any(grepl("SP|São Paulo", tmp2$region, ignore.case = TRUE))) {
      tmp2$decisao[grep("SP|São Paulo", tmp2$region, ignore.case = TRUE)] <- TRUE
      tutu[[i]] <- tmp2
      next
    }
    
    if (any(grepl("sudeste", tmp2$region, ignore.case = TRUE))) {
      tmp2$decisao[grep("sudeste", tmp2$region, ignore.case = TRUE)] <- TRUE
      tutu[[i]] <- tmp2
      next
    }
    
    if (!any(grepl("sudeste", tmp2$region, ignore.case = TRUE)) &
        !any(grepl("SP|São Paulo", tmp2$region, ignore.case = TRUE))) {
      tmp2$decisao[1] <- TRUE    
      tutu[[i]] <- tmp2
    }
  }
}
names(tutu) <- especies
tutu1 <- as.data.frame(dplyr::bind_rows(tutu, .id = "Gênero"))

# Filtering the data frame to get only one vernacular name per species
tutu2 <- tutu1[tutu1$decisao, ]
toto <- tutu2$Gênero[duplicated(tutu2$Gênero)]
tutu2.1 <- tutu2[tutu2$Gênero %in% toto, ]
especies.1 <- c("Allophyllus edulis", "Astronium graveolens", 
                "Cassia fistula", "Cecropia pachystachya",
                "Euterpe edulis", "Hovenia dulcis",
                "Joannesia princeps", "Ocotea puberula",
                "Rhamnidium elaeocarpum", "Sapindus saponaria",
                "Alchornea glandulosa subsp. iricurana",
                "Allophylus edulis", "Inga laurina")
names(especies.1) <- c("fruto-de-pombo", "Guaritá", 
                       "Cássia-imperial", "embaúba-branca",
                       "Juçara", "Uva-do-Japão",
                       "Boleira", "canela-babosa",
                       "Cafezinho", "saboneteira",
                       "tapiá", "fruto-de-pombo",
                       "ingá-mirim")
tutu2.1 <- tutu2.1[!duplicated(tutu2.1$Gênero), ]
for(i in seq_along(especies.1)) {
  tutu2.1$name[tutu2.1$Gênero %in% especies.1[i]] <- names(especies.1)[1] 
}
tutu3 <- rbind.data.frame(tutu2[!tutu2$Gênero %in% toto, ],
                          tutu2.1)
tutu3 <- tutu3[order(tutu3$Gênero), ]
tutu3$name <- stringr::str_to_title(tutu3$name)

## Previous vernacular names
bd.names <- unique(db[, c("Gênero", "Nome")])
new.spp.df <- data.frame(Gênero = new.spp, Nome = NA)
bd.names <- unique(rbind.data.frame(bd.names, new.spp.df))
bd.names1 <- dplyr::left_join(bd.names, tutu3, by = "Gênero")
names(tmp)[which(names(tmp) %in% "original.search")] <- "Gênero"
tmp1 <- tmp[, c("Gênero", "search.str", "notes")]

bd.names2 <- dplyr::left_join(bd.names1, tmp1)
replace_these <- is.na(bd.names2$Nome)
bd.names2$Nome[replace_these] <- bd.names2$name[replace_these] 

## Getting missing vernacular names from file
miss.names <- readRDS("./data/raw-data/nomes_comuns.rds")
replace_these <- is.na(bd.names2$Nome)
miss.spp <- bd.names2$Gênero[replace_these]

stopifnot(length(miss.spp[!miss.spp %in% miss.names[[1]]]) == 0) # should be empty

toto <- dplyr::left_join(bd.names2, miss.names, by = "Gênero")
bd.names2$Nome[replace_these] <- toto$name.y[replace_these] 
replace_these <- is.na(bd.names2$Nome)
if (any(replace_these))  
  bd.names2$Nome[replace_these] <- bd.names2$name[replace_these] 
names(bd.names2)[which(names(bd.names2) %in% "search.str")] <- 
  "nome.cientifico.correto"

## Saving common names in the full DB --------------------------------
replace_these <- !is.na(bd.names2$nome.cientifico.correto)
if (any(replace_these)) 
  bd.names2$nome.cientifico.correto[replace_these] <- 
    bd.names2$Gênero[replace_these]
bd.names3 <- bd.names2[!duplicated(bd.names2$nome.cientifico.correto),]

## Loading the file
db4 <- dplyr::left_join(db3, 
                        bd.names3[c("nome.cientifico.correto", "Nome", "name")],
                        by = "nome.cientifico.correto")

check_these <- is.na(db4$Nome) 
if (any(check_these)) {
  db_miss <- unique(db[db$Gênero %in% unique(db4$Determinacao[check_these]), 
                       c("Gênero", "Nome")])
  names(db_miss)[1] <- "nome.cientifico.correto"
  tmp <- db4[check_these, ]
  tmp1 <- dplyr::left_join(tmp, db_miss, by = "nome.cientifico.correto")  
  db4$Nome[check_these] <- tmp1$Nome.y
}

check_these <- is.na(db4$Nome) 
if (any(check_these)) {
  db_miss <- unique(db4[check_these, c("Determinacao", "Nome", "name")])
  names(miss.names)[1] <- "Determinacao"
  toto <- dplyr::left_join(db4, miss.names, by = "Determinacao")
  replace_these <- is.na(toto$Nome.x) & !is.na(toto$name.y)
  toto[replace_these, "Nome.x"] <- toto[replace_these,  "name.y"]
  db4$Nome[check_these] <- toto$Nome.x[check_these]
  
  names(miss.names)[1] <- "nome.cientifico.correto"
  miss.names1 <- miss.names[!duplicated(miss.names$nome.cientifico.correto), ]
  toto <- dplyr::left_join(db4, miss.names1[,c("nome.cientifico.correto", "name")], 
                           by = "nome.cientifico.correto")
  replace_these <- is.na(toto$Nome) & !is.na(toto$name.y)
  toto[replace_these, "Nome"] <- toto[replace_these,  "name.y"]
  db4$Nome[check_these] <- toto$Nome[check_these]
}

check_these <- is.na(db4$Nome)
if (any(check_these)) {
  cat("\nCommon names to add in the google drive file:\n") 
  # https://docs.google.com/spreadsheets/d/15aHE9bzSx42GffEuu8pDUq-1Y8NuE_-uZxgUThZ4mT4/edit?usp=sharing
  tmp <- unique(db4[check_these, c("nome.cientifico.correto", "Nome", "name")])
  print(tmp <- tmp[order(tmp$nome.cientifico.correto), ])
  stopifnot(all(!check_these)) # all check_these must be FALSE
}

## Final edits
db4$Nome[db4$nome.cientifico.correto %in% "Araucaria columnaris"] <- 
  "Araucária-colunar"
db4$Nome[db4$nome.cientifico.correto %in% "Cecropia pachystachya"] <- 
  "Embaúba-branca"

check_these <- grepl('[0-9]', db4$Nome)
if (any(check_these)) {
  cat("\nWeird common names:\n") 
  tmp <- unique(db4[check_these, c("nome.cientifico.correto", "Nome")])
  (tmp <- tmp[order(tmp$nome.cientifico.correto), ])
}

## Saving
db5 <- db4[, -dim(db4)[2]]
names(db5)[dim(db5)[2]] <- "nome.popular"
saveRDS(db5, "./data/derived-data/cadastro_arvores_esalq_nome_cientifico_popular.RDS")

## Removing all objects created in the script
all.objs <- ls()
rm.objs <- all.objs[!all.objs %in% obj.to.keep]
rm(list = rm.objs)
cat(paste0("\033[0;", 36, "m", "Done with script: ", script, "...","\033[0m","\n"))
# cat("Done with script:", script, "\n")
