#' Get List of Names for DB Update
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' 
#' @details
#' Creation date: 28 Fev 2024
#' 

# LOADING THE MOST UP-TO-DATE VERSION -------------------------------
## Getting all objects in the workspace before the script starts
obj.to.keep <- ls()

## Loading the file
db <- readRDS("./data/raw-data/cadastro_arvores_esalq.RDS")

## Getting only the IDs with confirmed identifications
db1 <- db[!is.na(db$Determinacao), ]

# NAME CHECKING WITH FLORA DO BRASIL -------------------------------
## Checking all new species names (with Flora do Brasil)
name.check <- flora::get.taxa(unique(db1$Determinacao), 
                             suggestion.distance = 0.91)
table(name.check$notes)

# Algum nome com grafia errada? Corrigir a coluna determinacao da planilha
check_these <- !name.check$notes %in% c("", NA, "not found")
if (any(check_these)) 
  name.check[check_these, ]

# Lista de nomes nao encontrados na Flora do Brasil online
names1 <- name.check[name.check$notes %in% c("not found"), ]

# NAME CHECKING WITH WORLD FLORA ONLINE -------------------------------
path.tax <- here::here(list("data", "raw-data", "WFO_Backbone"))
# WorldFlora::WFO.download(save.dir = path.tax)
path.tax1 <- file.path(path.tax, "classification.csv")
WorldFlora::WFO.remember(WFO.file = path.tax1,
                         WFO.data = "WFO.data", WFO.pos = 1)

## Exact matching
wfo <- WorldFlora::WFO.match(
  spec.data = names1$original.search,
  Fuzzy = 0, WFO.data = WFO.data, counter = 100, Fuzzy.max = 10, 
  verbose = F, no.dates = T)
wfo.one <- WorldFlora::WFO.one(wfo)
tt <- match(names1$original.search, wfo.one$spec.name.ORIG)
wfo.final <- cbind.data.frame(names1, wfo.one[tt,])
wfo.final$match.status <- NA
wfo.final$match.status[wfo.final$Matched] <- "exact"

## Fuzzy matching
check_these <- !wfo.final$Matched
wfo3 <- WorldFlora::WFO.match(
  spec.data = names1$original.search[check_these],
  Fuzzy = 0.1, WFO.data = WFO.data, counter = 25, Fuzzy.max = 7, 
  verbose = F, no.dates = T)
wfo3.one <- WorldFlora::WFO.one(wfo3)

tt1 <- match(names1$original.search[check_these], wfo3.one$spec.name.ORIG)
wfo3.final <- cbind.data.frame(names1[check_these, ], wfo3.one[tt1,])
wfo3.final$match.status <- NA
wfo3.final$match.status[wfo3.final$Matched] <- "fuzzy"

col.match <- 
  unique(match(colnames(wfo3.final), colnames(wfo.final), nomatch = 0))
wfo.final[check_these, col.match] <- wfo3.final[, col.match]
check_these <- !wfo.final$Matched
wfo.final$match.status[check_these] <- "no_match"

names1$name_macthed_wfo <- wfo.final$scientificName
names1$macth_status_wfo <- wfo.final$match.status
names1$wfo_id <- wfo.final$taxonID


# NAME CHECKING WITH WORLD CHECKLIST OF VASCULAR PLANTS ---------------
matches_wcvp <- rWCVP::wcvp_match_names(names1,
                            name_col = "original.search",
                            author_col = NULL,
                            fuzzy = TRUE,
                            progress_bar = TRUE)

if (any(matches_wcvp$multiple_matches)) {
  tmp2 <- matches_wcvp[matches_wcvp$multiple_matches, ]
  tmp2 <- tmp2[order(tmp2$match_edit_distance, match(tmp2$original.search, unique(tmp2$original.search))), ]
  ## CHECAR SE ESTÁ FUNCIONANDO QUANDO HOUVER MAIS DE 1 NOME DUPLICADO
  matches_wcvp[matches_wcvp$multiple_matches, ] <- tmp2
  matches_wcvp1 <- matches_wcvp[!duplicated(matches_wcvp$original.search), ]
} else { matches_wcvp1 <- matches_wcvp }

names1$name_macthed_wcvp <- matches_wcvp1$wcvp_name
names1$macth_status_wcvp <- matches_wcvp1$match_type
names1$wcvp_id <- matches_wcvp1$wcvp_id

replace_these <- is.na(names1$name_macthed_wfo) & !is.na(names1$name_macthed_wcvp)
if (any(replace_these)) {
  names1[replace_these, ]
}

replace_these <- names1$name_macthed_wfo == names1$name_macthed_wcvp & 
                  !is.na(names1$name_macthed_wcvp)
if (any(replace_these)) {
  names1[replace_these, "scientific.name"] <- paste(matches_wcvp1[replace_these, "wcvp_name"], 
                                                    matches_wcvp1[replace_these, "wcvp_authors"])
    names1[replace_these, "taxon.rank"] <- matches_wcvp1[replace_these, "wcvp_rank"]
    names1[replace_these, "taxon.status"] <- matches_wcvp1[replace_these, "wcvp_status"]
}

replace_these <- grepl("uzzy", names1$macth_status_wcvp)
if (any(replace_these)) {
    names1[replace_these, "search.str"] <- 
      names1[replace_these, "name_macthed_wfo"]
    names1[!replace_these, "search.str"] <- 
      names1[!replace_these, "original.search"]
} else {  
  names1[["search.str"]] <- names1[["original.search"]]
}


# REPLACING NAMES NOT FOUND IN FLORA DO BRASIL ---------------------------
cols.fbo <- c("id", "notes", "search.str")
cols.wfo <- c("wfo_id", "macth_status_wcvp", "search.str")
name.check[name.check$notes %in% c("not found"), cols.fbo] <- 
  names1[, cols.wfo]

# BINDING BACK INTO THE FULL DB -------------------------------
names(name.check)[which(names(name.check) %in% "original.search")] <- "Determinacao"
db2 <- dplyr::left_join(db1, name.check[c("search.str", "Determinacao")], 
                        by = "Determinacao")

names(db2)[which(names(db2) %in% "search.str")] <- "nome.cientifico.correto"

## Saving the result
saveRDS(db2, "./data/derived-data/cadastro_arvores_esalq_nome_cientifico.RDS")

## Removing all objects created in the script
all.objs <- ls()
rm.objs <- all.objs[!all.objs %in% obj.to.keep]
rm(list = rm.objs)

