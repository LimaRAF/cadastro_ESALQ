#' Combine and Check the ESALQ Tree Species List
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' 
#' @details
#' Creation date: 21 Mar 2024
#' 
# LOADING ALL SOURCES OF NAMES  -------------------------------------
## Loading the files
cadastro <- readRDS("./data/raw-data/cadastro_arvores_esalq.RDS")
lista <- readRDS("./data/raw-data/listas_arvores_esalq.RDS")
esa <- as.data.frame(readxl::read_excel("./data/raw-data/speciesLink-20240325095930-0019682.xlsx"))

## Merging the files
cadastro1 <- data.frame(Nome = unique(c(cadastro$suggestedName, cadastro$NomeCientifico, 
                                        cadastro$Determinacao)))
cadastro1$Fonte <- "cadastro"
lista1 <- lista[,c("Nome", "Fonte")]
nomes <- dplyr::bind_rows(cadastro1, lista1)

# from speciesLink
esa1 <- esa[, c("scientificname", "scientificnameauthor", "life.form"), drop = FALSE]
esa1$Fonte <- "speciesLink"
esa2 <- esa1[!grepl("Erva|Subarbusto|Liana|Bambu", esa1$life.form, perl = TRUE) |
               grepl("Arvore|Palmeira|Arbusto", esa1$life.form, perl = TRUE),
             c("scientificname", "scientificnameauthor", "Fonte")]
names(esa2)[1] <- "Nome"
sort(unique(esa2$Nome[!esa2$Nome %in% nomes$Nome]))
nomes <- dplyr::bind_rows(nomes, unique(esa2))
nomes <- nomes[!nomes$Nome %in% c("", " ", "NA", NA), ]

# NAME CHECKING WITH FLORA DO BRASIL -------------------------------
## Preparing the names
nomes1 <- plantR::fixSpecies(nomes, "Nome") 

## Checking all new species names (with Flora do Brasil)
name.check <- flora::get.taxa(unique(nomes1$scientificName.new), replace.synonyms = FALSE,
                             suggestion.distance = 0.91, drop = c(""))

# Algum nome com grafia errada? Corrigir a coluna determinacao da planilha
rep_these <- grepl("miss", name.check$notes)
name.check$original.search[rep_these] <- 
  name.check$search.str[rep_these]

# NAME CHECKING WITH WORLD CHECKLIST OF VASCULAR PLANTS ---------------
matches_wcvp <- rWCVP::wcvp_match_names(name.check,
                            name_col = "original.search",
                            author_col = NULL,
                            fuzzy = TRUE,
                            progress_bar = TRUE)

#### ROTINA PRECISA SER ESTUDADA E REVISTA ####
## Varios fuzzy simpes não sendo resolvidos
## left_join perdendo algumas espécies
## Tlavez usar flora, wfo e depois o wcvp...
## ver rotina aqui: https://matildabrown.github.io/rWCVP/articles/redlist-name-matching.html

if (any(matches_wcvp$multiple_matches)) {
  tmp2 <- matches_wcvp[matches_wcvp$multiple_matches, ]
  tmp2.1 <- tmp2[tmp2$match_similarity >= 0.89 , ]
  tmp2.1 <- tmp2.1[!tmp2.1$wcvp_status %in% c("Illegitimate", "Unplaced", "Invalid"), ]
  tmp2.1 <- tmp2[order(tmp2.1$match_edit_distance, 
                     match(tmp2.1$original.search, unique(tmp2.1$original.search))), ]
  # synonym_codes <- c("Synonym", "Orthographic", "Artificial Hybrid", "Unplaced")
  # synonyms <-
  #   tmp2 %>%
  #   filter(wcvp_status %in% synonym_codes | ! sum(wcvp_status %in% synonym_codes))
  ## CHECAR SE ESTÁ FUNCIONANDO QUANDO HOUVER MAIS DE 1 NOME DUPLICADO
  tmp3 <- tmp2.1[!duplicated(tmp2.1$original.search), ]
  tmp4 <- dplyr::left_join(tmp2, tmp3, by = "original.search", suffix = c(".x", ""))
  tmp4.1 <- tmp4[, colnames(matches_wcvp)]
  matches_wcvp[matches_wcvp$multiple_matches, ] <- tmp4.1
  # matches_wcvp1 <- matches_wcvp[!duplicated(matches_wcvp$original.search), ]
}

matches_wcvp1 <- matches_wcvp[!duplicated(matches_wcvp$original.search),]
matches_wcvp2 <- matches_wcvp1[!is.na(matches_wcvp1$match_similarity) & 
                                 matches_wcvp1$match_similarity > 0.79, ]
tmp <- dplyr::left_join(name.check, matches_wcvp2,
                        by = "original.search")

name.check$name_macthed_wcvp <- tmp$wcvp_name
name.check$authors_wcvp <- tmp$wcvp_authors
name.check$taxon.rank_wcvp <- tmp$wcvp_rank 
name.check$taxon.status_wcvp <- tmp$wcvp_status
name.check$macth_status_wcvp <- tmp$match_type
name.check$wcvp_id <- tmp$wcvp_id
name.check$wcvp_id_accepted <- tmp$wcvp_accepted_id

# BINDING BACK INTO THE FULL DB -------------------------------
names(name.check)[which(names(name.check) %in% "original.search")] <- "Nome"
name.check1 <- name.check[!duplicated(name.check$Nome), ]
col.names <- c("name_macthed_wcvp", "authors_wcvp",
               "taxon.rank_wcvp", "taxon.status_wcvp",
               "macth_status_wcvp" ,"wcvp_id", 
               "wcvp_id_accepted" )
db2 <- dplyr::left_join(nomes1, 
                        name.check1[c(col.names, "Nome")], 
                        by = "Nome")
name.check2 <- name.check1
names(name.check2)[which(names(name.check2) %in% "Nome")] <- "scientificName.new"
db2.1 <- dplyr::left_join(nomes1, 
                         name.check2[c(col.names, "scientificName.new")], 
                         by = "scientificName.new")
rep_these <- is.na(db2$name_macthed_wcvp) & !is.na(db2.1$name_macthed_wcvp)
db2[rep_these, col.names] <- db2.1[rep_these, col.names]

## Saving the result
saveRDS(db2, "./data/derived-data/lista_arvores_esalq_nome_cientifico_wcvp.RDS")
#writexl::write_xlsx(db2, "./data/derived-data/lista_arvores_esalq_nome_cientifico_wcvp.xlsx")


# GETTING THE FINAL LIST OF VALID NAMES FOLLOWING THE WCVP ---------------------
esalq <- as.data.frame(readxl::read_xlsx("./data/derived-data/lista_arvores_esalq_nome_cientifico_wcvp.xlsx"))

## Checking for missing important info
esalq1 <- esalq[(is.na(esalq$authors_wcvp) | is.na(esalq$wcvp_id)) & 
                          !esalq$scientificNameStatus %in% "indet" &
                          !esalq$name_macthed_wcvp %in% c("", " ", "NA", NA), 
                        c("Ordem", "name_macthed_wcvp", "authors_wcvp")]
if (dim(esalq1)[1] > 0) {
  check_info <- rWCVP::wcvp_match_names(esalq1,
                                        name_col = "name_macthed_wcvp",
                                        author_col = "authors_wcvp",
                                        fuzzy = FALSE,
                                        progress_bar = TRUE)
  tmp1 <- dplyr::left_join(esalq, check_info, by = "Ordem")
  writexl::write_xlsx(tmp1, "./data/derived-data/check_info.xlsx")
}

## Getting valid names
library(rWCVP)
library(tidyverse) # data wrangling
library(rWCVPdata) #version 0.4.1
esalq1 <- esalq[!esalq$scientificNameStatus %in% "indet" &
                          !esalq$name_macthed_wcvp %in% c("", " ", "NA", NA), 
                        c("Ordem", "name_macthed_wcvp", "authors_wcvp")]
valid_wcvp_names <- as.data.frame(rWCVP::wcvp_match_names(esalq1,
                                                name_col = "name_macthed_wcvp",
                                                author_col = "authors_wcvp",
                                                fuzzy = FALSE,
                                                progress_bar = TRUE))

path <- here::here("data", "raw-data", "wcvp")
if (!dir.exists(path)) 
  dir.create(path)

# load global checklist
wcvp_global <- wcvp_checklist()

# select a subset of columns
wcvp_global <- wcvp_global %>% 
  select(plant_name_id, taxon_name, taxon_authors,
         taxon_rank, taxon_status, accepted_name,
         family, genus, geographic_area, region, 
         lifeform_description, ipni_id, powo_id)

# pull only the unique ids and names of species
accepted_names <- wcvp_global %>% 
  select(plant_name_id, taxon_name, taxon_authors) %>% 
  distinct()

all_names <- rWCVPdata::wcvp_names %>% 
  filter(accepted_plant_name_id %in% accepted_names$plant_name_id)

# Select important ids and names of species
valid_wcvp_names1 <- valid_wcvp_names %>% 
  select(Ordem, wcvp_id, wcvp_name, wcvp_authors, wcvp_status, wcvp_accepted_id)


## Getting the accepted names
tmp <- wcvp_global[, c("plant_name_id", "accepted_name")]
tmp <- tmp[!duplicated(tmp$plant_name_id), ]
names(tmp)[1] <- "wcvp_accepted_id"
esalq2 <- dplyr::left_join(valid_wcvp_names1, tmp, by = "wcvp_accepted_id")
names(esalq2) <- c("Ordem", "wcvp_id", "wcvp_name", "wcvp_authors", "wcvp_status", "plant_name_id", "accepted_name")
esalq2 <- esalq2[, c("Ordem", "plant_name_id", "accepted_name")]
tmp1 <- wcvp_global[!duplicated(wcvp_global$plant_name_id), ]
esalq3 <- dplyr::left_join(esalq2, tmp1, by = "plant_name_id", suffix = c("",".accept"))

## Saving accepted names back into the original file
esalq4 <- esalq3[, c("Ordem", "plant_name_id", "accepted_name", "taxon_authors", "taxon_rank", "taxon_status", "powo_id")]
names(esalq4) <- c("Ordem", "accepted_wcvp_id", "accepted_name", "accepted_name_authors", "accepted_taxon_rank", "accepted_taxon_status", "accepted_powo_id")
esalq5 <- dplyr::left_join(esalq, esalq4, by = "Ordem")

## Checking infra-specific names
# infra_names <- unique(esalq5[!is.na(esalq5$accepted_taxon_rank) & 
#                                !esalq5$accepted_taxon_rank %in% "Species", c("accepted_name", "accepted_name_authors")])
# infra_names$Nome <- gsub(" var\\..*| subsp\\..*", "", infra_names$accepted_name, perl = TRUE)
# infra_names <- infra_names[!infra_names$Nome %in% esalq5$accepted_name,]
# infra_wcvp_names <- as.data.frame(rWCVP::wcvp_match_names(infra_names,
#                                                           name_col = "Nome",
#                                                           author_col = NULL,
#                                                           fuzzy = FALSE,
#                                                           progress_bar = TRUE))
# # Add those to the input file
# infra_wcvp_names1 <- infra_wcvp_names[, c("accepted_name", "Nome", "match_type", "wcvp_id", 
#                                           "wcvp_authors", "wcvp_rank", "wcvp_status",
#                                           "wcvp_accepted_id")] 
# writexl::write_xlsx(infra_wcvp_names1, "./data/derived-data/infraspecific_to_add.xlsx")

## Aggregatting per source
tmp <- esalq5[, c("Fonte", "accepted_name")]
tmp$Fonte <- gsub("_.*", "", tmp$Fonte, perl = TRUE)
tmp1 <- aggregate(tmp$Fonte, list(tmp$accepted_name), function(x) paste0(unique(x), collapse = "|"))
names(tmp1) <- c("accepted_name", "sources")
final_list <- unique(esalq5[, c("accepted_wcvp_id", "accepted_name", "accepted_name_authors", "accepted_taxon_rank", "accepted_taxon_status", "accepted_powo_id")])
final_list <- dplyr::left_join(final_list, tmp1, by = "accepted_name")
final_list <- final_list[!is.na(final_list$accepted_name), ]
final_list <- final_list[order(final_list$accepted_name), ]

## Removing infra-specific names
final_list1 <- final_list[final_list$accepted_taxon_rank %in% "Species", ]

## Addin full species name
final_list1$species <- .build.name(final_list1, 
                                   col.names = c("accepted_name", "accepted_name_authors"))

## Adding the check status column
final_list1$check_status <- NA
rep_these <- stringr::str_count(final_list1$sources, "\\|") >= 3
final_list1$check_status[rep_these] <- "low_priority"

rep_these <- final_list1$sources %in% c("barbin", "cadastro", "cadastro|barbin")
final_list1$check_status[rep_these] <- "high_priority"

rep_these <- final_list1$sources %in% c("nathalia", "nathalia|barbin", "cadastro|nathalia",
                                        "nathalia|goldenberg|barbin", "goldenberg|barbin",
                                        "cadastro|nathalia|barbin")
final_list1$check_status[rep_these] <- "medium_priority"

rep_these <- grepl("speciesLink", final_list1$sources)
final_list1$check_status[rep_these] <- "low_priority"

# confirmed species from the taxonomic ID revision
cadastro <- readRDS("./data/raw-data/cadastro_arvores_esalq.RDS")
check_spp <- sort(unique(c(final_list1$accepted_name[final_list1$accepted_name %in% cadastro$Determinacao],
               esalq5$Nome[esalq5$Nome %in% cadastro$Determinacao],
               esalq5$scientificName.new[esalq5$scientificName.new %in% cadastro$Determinacao],
               esalq5$name_macthed_wcvp[esalq5$name_macthed_wcvp %in% cadastro$Determinacao])))
rep_these <- final_list1$accepted_name %in% check_spp
final_list1$check_status[rep_these] <- "confirmed"

# confirmed species from speciesLink
esa <- as.data.frame(readxl::read_excel("./data/raw-data/speciesLink-20240325095930-0019682.xlsx"))
names(esa)[which(names(esa) %in% "catalognumber")] <- "catalogNumber"
names(esa)[which(names(esa) %in% "collectioncode")] <- "collectionCode"
names(esa)[which(names(esa) %in% "latitude")] <- "decimalLatitude"
names(esa)[which(names(esa) %in% "longitude")] <- "decimalLongitude"
names(esa)[which(names(esa) %in% "identifiedby")] <- "identifiedBy"
names(esa)[which(names(esa) %in% "institutioncode")] <- "institutionCode"
names(esa)[which(names(esa) %in% "collector")] <- "recordedBy"
names(esa)[which(names(esa) %in% "collectornumber")] <- "recordNumber"
names(esa)[which(names(esa) %in% "scientificname")] <- "scientificName"
names(esa)[which(names(esa) %in% "scientificnameauthor")] <- "scientificNameAuthorship"
names(esa)[which(names(esa) %in% "stateprovince")] <- "stateProvince"
names(esa)[which(names(esa) %in% "typestatus")] <- "typeStatus"
names(esa)[which(names(esa) %in% "yearidentified")] <- "yearIdentified"
names(esa)[which(names(esa) %in% "monthidentified")] <- "monthIdentified"
names(esa)[which(names(esa) %in% "dayidentified")] <- "dayIdentified"
esa$year <- esa$yearcollected

occs <- plantR::formatDwc(splink_data = esa)
occs <- plantR::formatOcc(occs)
occs <- plantR::validateTax(occs, 
                            miss.taxonomist = c("Souza, V.C.", "Rodrigues, R.R.", 
                                                "Ivanauskas, N.M.", "Lorenzi, H.",
                                                "Colletta, G.D."))
ok_names <- unique(occs$scientificName[occs$tax.check %in% "high"])
check_spp <- sort(unique(c(final_list1$accepted_name[final_list1$accepted_name %in% ok_names],
                           esalq5$Nome[esalq5$Nome %in% ok_names],
                           esalq5$scientificName.new[esalq5$scientificName.new %in% ok_names],
                           esalq5$name_macthed_wcvp[esalq5$name_macthed_wcvp %in% ok_names])))
rep_these <- final_list1$accepted_name %in% check_spp
final_list1$check_status[rep_these] <- "confirmed"
sort(table(final_list1$sources[is.na(final_list1$check_status)]))


## Getting extra info
tmp <- wcvp_global[, c("plant_name_id", "family", "genus", "lifeform_description", "geographic_area")]
tmp <- tmp[!duplicated(tmp$plant_name_id), ]
names(tmp)[1] <- "accepted_wcvp_id"
final_list2 <- dplyr::left_join(final_list1, tmp, by = "accepted_wcvp_id")

## Already in the microproject
micro <- readRDS("./data/raw-data/esalq_PN_microproject.RDS")
micro$accepted_name <- sapply(micro$species, flora::remove.authors)
miss.micro <- micro[!micro$accepted_name %in% final_list2$accepted_name, ]
miss_names <- as.data.frame(rWCVP::wcvp_match_names(miss.micro,
                                                          name_col = "accepted_name",
                                                          author_col = NULL,
                                                          fuzzy = FALSE,
                                                          progress_bar = TRUE))
miss_names <- miss_names[!miss_names$wcvp_status %in% "Illegitimate", ]

tmp <- wcvp_global[, c("plant_name_id", "accepted_name")]
names(tmp)[1] <- "wcvp_accepted_id"
tmp <- tmp[!duplicated(tmp$wcvp_accepted_id), ]
miss_names1 <- dplyr::left_join(miss_names, tmp, by = "wcvp_accepted_id")
micro$accepted_name[!micro$accepted_name %in% final_list2$accepted_name] <-
  miss_names1$accepted_name.y
final_list2$in_microproject <- final_list2$accepted_name %in% micro$accepted_name
micro[!micro$accepted_name %in% final_list2$accepted_name, ]

## Organizing
col_order <- c("check_status", "in_microproject",
               "family", "genus", "species", "accepted_name", "accepted_name_authors", 
               "accepted_taxon_rank", "accepted_taxon_status", 
               "accepted_wcvp_id", "accepted_powo_id", 
               "lifeform_description", "geographic_area", "sources")
final_list3 <- final_list2[, col_order]
final_list3 <- final_list3[order(final_list3$family, final_list3$genus, final_list3$species), ]


## LAst minute, non tree, shrubs removal
rm_spp <- c("Alcea rosea", "Arundo donax", "Dichorisandra thyrsiflora", 
"Typha latifolia", "Commelina rufipes", "Pontederia cordata", "Yucca flaccida", 
"Bignonia magnifica","Clerodendrum thomsoniae","Mucuna pruriens","Myriopus paniculatus",
"Paullinia elegans","Serjania orbicularis","Cleome spinosa","Heliotropium transalpinum",
"Euphorbia milii", "Muehlenbeckia platyclada", "Phoradendron rubrum","Paullinia cupana")
final_list4 <- final_list3[!final_list3$accepted_name %in% rm_spp, ]

## Saving 
# write.csv(final_list4, "./data/derived-data/esalq_tree_shrubs_list_wcvp.csv", 
#           row.names = FALSE, fileEncoding = "UTF-8")
writexl::write_xlsx(final_list4, "./data/derived-data/esalq_tree_shrubs_list_wcvp.xlsx", format_headers = FALSE)
