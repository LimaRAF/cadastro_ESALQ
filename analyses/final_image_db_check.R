#### FINAL CHECK - vcs IMDAGE DATABASE ####

## LOAD DB
db <- as.data.frame(readxl::read_excel('info_imagens_final_corrected.xlsx'))

## GET NAMES WITHOUT AUTHOR
spp <- plantR::fixSpecies(db, tax.name = "Plantspecies_full", FALSE, TRUE)
# removing infra-specific names
replace_these <- grepl("var\\.|subsp\\.|f\\.", spp$scientificName.new)
if(any(replace_these))
  spp$scientificName.new[replace_these] <- 
    sapply(strsplit(spp$scientificName.new[replace_these], " "), function(x) paste(head(x, 2), collapse = " "))
#gettin problematic names
check_these <- stringr::str_count(spp$scientificName.new, " ") > 1
if(any(check_these)) {
  spp$scientificName.new[check_these] <- 
    gsub(" Brasilia$| Canastra$", "", spp$scientificName.new[check_these], perl = TRUE)
  spp$scientificName.new[check_these] <- 
    gsub(" Panero$| Souza$| Bruniera$| Andrea$| van der$| \\(H\\.Rob\\.\\)$|", "", spp$scientificName.new[check_these], perl = TRUE)
  spp$scientificName.new[check_these] <- 
    gsub(" Celastraceae$| Myrtaceae$", "", spp$scientificName.new[check_these], perl = TRUE)
  spp$scientificName.new[check_these] <- 
    gsub(" cones$| salvadorensis$| coerulea$| ×trifasciata$| 'black velvet'$", "", spp$scientificName.new[check_these], perl = TRUE)
}
sort(unique(spp$scientificName.new[stringr::str_count(spp$scientificName.new, " ") > 1]))
write.csv(spp, "tmp.csv")

## GET LOCALITY/VOUCHER INFO FROM FILE NAME
nomes <- db$PlantspeciesOriginal
nomes1 <- strsplit(nomes, " ")
nomes2 <- rep(NA, length(nomes1))
replace_these <- lengths(nomes1) > 2
if(any(replace_these))
  nomes2[replace_these] <- 
    stringr::str_trim(sapply(nomes1[replace_these], function(x) paste(x[3:length(x)], collapse = " ")))
write.csv(cbind.data.frame(db$ordem, db$PlantspeciesOriginal, nomes2), "tmp.csv")


## FIND DUPLICATED OBSID
toto <- aggregate(db$Plantspecies, list(db$ObsId), unique)
check_these <- lengths(toto$x) > 1
toto1 <- aggregate(db$Date, list(db$ObsId), unique)
check_these1 <- lengths(toto$x) > 1
dup.ids <- sort(unique(toto$Group.1[check_these], toto$Group.1[check_these1]))
new.obs.ids <- db$ObsId
db$ObsId_new <- db$ObsId
db$check_ID <- db$ObsId %in% dup.ids
max.id <- max(db$ObsId) 
for (i in 1:length(dup.ids)) {
  id.i <- dup.ids[i]
  rep_these <- new.obs.ids %in% id.i
  data.i <- db[rep_these, c("ObsId", "Plantspecies", "Date")]
  rep.ids <- as.factor(apply(data.i, 1, paste, collapse = "_"))
  for (j in 1:length(levels(rep.ids))) {
    if (j == 1)
      levels(rep.ids)[j] <- gsub("_.*", "", levels(rep.ids)[j]) 
    if (j > 1)
      levels(rep.ids)[j] <- gsub("_.*", paste0("_", j - 1), levels(rep.ids)[j]) 
  }
  new.obs.ids[rep_these] <- as.character(rep.ids)
}
replace_these <- grepl("_", new.obs.ids, perl = TRUE)
#db$ObsId[replace_these] <- new.obs.ids[replace_these]
new.obs.ids1 <- as.factor(new.obs.ids[replace_these])
levels(new.obs.ids1) <- order(unique(new.obs.ids[replace_these]))
db$ObsId_new[replace_these] <- 
  as.numeric(as.character(new.obs.ids1)) + max.id
#Checking
toto <- aggregate(db$Plantspecies, list(db$ObsId_new), unique)
any(lengths(toto$x) > 1) # FALSE is good!
toto1 <- aggregate(db$Date, list(db$ObsId), unique)
any(lengths(toto$x)) > 1  # FALSE is good!
write.csv(cbind.data.frame(db$ordem, db$ObsId, db$ObsId_new, db$check_ID), "ObsId_check.csv")

## Getting Voucher info from older file
# db_old <- as.data.frame(readxl::read_excel('infos_imagens com correções de VCS.xlsx'))
# toto <- dplyr::left_join(db, db_old[, c("ordem", "voucher")])
# write.csv(cbind.data.frame(toto$ordem, toto$ObsId, toto$voucher), "voucher_check.csv")


## CHECKING PROBLEMATIC COORDINATES
db$decimalLatitude <- db$Lat
db$decimalLongitude <- db$Long

locais <- strsplit(gsub(" \\(cultivated\\)", "", db$LocalityName), ",")
locais <- lapply(locais, function(x) gsub("^ | $", "", x, perl = TRUE))
db$country <- sapply(locais, tail, 1)
db$country <- gsub("\\.$", "", db$country)
toto <- lapply(locais, function(x) x[length(x) - 1])
toto[!sapply(toto, function(x) length(x) > 0)] <- NA
db$stateProvince <- unlist(toto)
toto <- lapply(locais, function(x) x[length(x) - 2])
toto[!sapply(toto, function(x) length(x) > 0)] <- NA
db$municipality <- unlist(toto)
db$locality <- NA
rep_these <- lengths(locais) > 3
db$locality[rep_these] <- sapply(locais[rep_these], function(x) x[length(x) - 3])

db1 <- plantR::formatLoc(db, 
                         loc.levels = c("country", "stateProvince", "municipality"))
db1 <- plantR::formatCoord(db1)
db1 <- plantR::validateLoc(db1)
db1$remarks <- NA
db1 <- plantR::validateCoord(db1)
summaryFlags(db1)

write.csv(db1[,c("ordem", "LocalityName", "Lat", "Long", "country",
                 "stateProvince", "municipality", "locality",
                 "loc", "loc.correct", "coord.check",
                 "loc.check", "geo.check", "out.check")], "coord_check.csv")

