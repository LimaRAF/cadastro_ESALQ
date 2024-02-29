#####################################
### Getting file info from images ###
#####################################

# Instalando os pacotes necessários
if (!require("data.table")) 
  install.packages("data.table")

if (!require("dplyr")) 
  install.packages("dplyr")

if (!require("exifr")) 
  install.packages("exifr")

if (!require("remotes")) 
  install.packages("remotes")

if (!require("stringr")) 
  install.packages("stringr")

if (!require("jpeg")) 
  install.packages("jpeg")

if (!require("tiff")) 
  install.packages("tiff")

if (!require("plantR")) 
  remotes::install_github("LimaRAF/plantR")



# -------------------------------------------------------------------------
# ANTES DE COMEÇAR
## 1- Usar apenas um caminho para o pacote de fotos de cada autor/fotógrafo
## 2- Os formatos de fotos aceitos são: JPG et PNG
## A organização ideal das pastas e subpastas é a seguinte:
##  ¦ - (raiz) fotos sem dados de localidade; nome das espécies são os nomes dos arquivos
##  ¦¦ - Localidade (p.ex. Ibitipoca)
##  ¦¦¦ - Especie (não precisa subdividir pastas por familia)
##  ¦¦¦¦ - Observacoes (i.e. indivíduos/specimens - não repetir nome das observações)

## Caso haja apenas uma observação por espécie, não precisa de subpastas de Observações
## Os seguintes formatos tb são aceitos:
##  ¦ - (raiz)
##  ¦¦ - Localidade (nome das espécies são os nomes dos arquivos)

##  ¦ - (raiz)
##  ¦¦ - Especie (para fotos que se sabe o nome mas não o local)

# -------------------------------------------------------------------------
# PASSO 1 - FORNEÇA O CAMINHO PARA A PASTA
## Substitua aqui o caminho para a pasta contendo as fotos no seu computador
## Obs.: Evitar nomes de arquivos e pastas com acentos, para evitar erros de encoding
## Obs.: Nomes de arquivos com problemas de encoding serão ignorados
path <- "C:/Users/renat/Desktop/Fotos Plantas"
path <- "D:/BotSist2021"

# -------------------------------------------------------------------------
# PASSO 2 - INFORMAÇÕES NECESSÁRIAS
## quem é o autor das fotos?
autor <- "Vinicius Castro Souza" # coloque aqui o nome de que tirous as fotos

## abreviacao e nome de coletor (se for o caso)
coletor <- "VCS" # coloque aqui como o nome do coletor esta abreviado
names(coletor) <- "V.C.Souza"

## quais são seus dados de utilizados do Pl@ntNet
user.pn <- "vcsouza@usp.br"
email.pn <- "vcsouza@usp.br"

## qual a licensa que será atribuída para as fotos
### opções: ‘under copyright’, 'cc-by-nc', 'cc-by-nc-sa', 'public', 'cc-by-sa', 'cc-by', etc.
### mais detalhes aqui: https://certificates.creativecommons.org/cccertedu/chapter/3-3-license-types/#:~:text=The%20Attribution%2DNonCommercial%2DNoDerivatives%20license,give%20credit%20to%20the%20creator.
### Default se vazio: "cc-by-sa"
licensa <- "cc-by-nc-sa"

## qual é o formato dos arquivos pasta por pasta?
### Aqui precisa especificar pasta a pasta o que o nome do arquivo quer dizer e
### o que o nome da pasta quer dizer. Se não há pastas, colocar tudo sob 'raiz'
#folder é familia e o arquivo é a especie + voucher
pastas <- list.files(path)
pastas <- pastas[!grepl("\\.", pastas)]
lista.pastas <- as.list(rep("FileNameEhEspecie", length(pastas)))
names(lista.pastas) <- pastas

formatos <- append(list(raiz = "FileNameEhEspecie"),
                 lista.pastas)
# formatos <- list(raiz = "FileNameEhEspecie",
#                  Ibitipoca = c("FileNameEhEspecie", "FolderNameEhLocalidade"),
#                 Jurupara = c("FileNameEhEspecie", "FolderNameEhLocalidade"),
#                 # `OESTE DA BAHIA` = c("FolderNameEhLocalidade", "SubFolderNameEhEspecie"),
#                 serra = c("FileNameEhEspecie", "FolderNameEhLocalidade"))

## qual é a localidade de cada pasta (se for o caso)?
# coordenadas em graus decimais na projeção WGS-84
# locais <- list(Ibitipoca = c(loc = "Parque Estadual do Ibitipoca, Lima Duarte, Minas Gerais, Brazil", 
#                              lat = "-21.672726", long = "-43.877147"),
#                Jurupara = c(loc = "Parque Estadual Jurupará, Ibiúna, São Paulo, Brazil", 
#                             lat = "-23.972166", long = "-47.234979"),
#                # `OESTE DA BAHIA`= c(loc = "Bahia, Brazil"),
#                serra = c(loc = "Cond. Alpes da Canmtareira, Mairiporã, São Paulo, Brazil", 
#                          lat = "-23.411751", long = "-46.645462"))
locais <- NULL

# -------------------------------------------------------------------------
# PASSO 3 - EXTRAINDO AS INFORMAÇÕES DAS FOTOS (NÃO PRECISA SUBSTITUIR NADA)
## Aqui iremos listar todos os arquivos contendo as imagens na pasta especificada acima
files <- list.files(path, recursive = TRUE, full.names = TRUE)
sort(table(tolower(gsub(".*\\.", "", files, perl = TRUE)))) ## formatos de arquivos na pasta

## Algum arquivo tif, nef ou dng Se sim converter e salvar em uma pasta à parte
outros <- files[grepl("\\.TIF$|\\.NEF$|\\.DNG$", files, 
                    ignore.case = TRUE, perl = TRUE)]
outros <- outros[!grepl("\\/DSCN[0-9]", outros)]
# pasta.temp <- paste0(path, "/ZZ_outros_formatos_renato/")
# outros.new <- gsub(path, "D:/BotSist2021/ZZ_outros_formatos_renato", outros)

# file.copy(from = head(outros,2), to = head(outros.new,2), 
#           overwrite = TRUE, recursive = FALSE, 
#           copy.mode = TRUE, copy.date = TRUE)

## Algum arquivo tif? Se sim converter e salvar em uma pasta à parte
## VCS: já fez a conversão nas suas pastas
# tifs <- files[grepl("\\.TIF$", files, ignore.case = TRUE, perl = TRUE)]
# tifs <- tifs[!grepl("\\/DSCN[0-9]", tifs)]
# pasta.temp <- paste0(path, "/ZZ_convertidas_renato/")
# 
# if (length(tifs) > 0) {
#   for (i in 1:length(tifs)) {
#     nome.arq <- tifs[i]
#     img <- try(tiff::readTIFF(nome.arq, native=TRUE), TRUE)
#     if (class(img) == "try-error") {
#       next
#     } else {
#       nome.arq.new <- NULL
#       nome.arq.new <- gsub("TIF$|tif$", "jpg", gsub(".*\\/", "", nome.arq))
#       nome.arq.new <- paste0(pasta.temp, nome.arq.new)
#       jpeg::writeJPEG(img, target = nome.arq.new, quality = 1)
#     }
#   }
# }


## E aqui armanezar apenas os arquivos como fotos (.JPG no caso)
files <- files[grepl("\\.JPG$|\\.JPEG$|\\.PNG$", files, ignore.case = TRUE, perl = TRUE)]

## Se for necessário remover pelo nome, os arquivos indesejáveis
ignorar_esses <- "vert\\.|corte\\.|page\\.|page[0-9]\\."
files <- files[!grepl(ignorar_esses, files, perl = TRUE)]
# ignorar_esses1 <- "OESTE DA BAHIA"
# files <- files[!grepl(ignorar_esses1, files, perl = TRUE)]

## Resolvendo possíveis problemas de ecnoding nos nomes das pastas
files1 <- plantR:::fixEncoding(files)

## Removendo os acentos nos nomes das pastas
tmp <- plantR:::rmLatin(files1)
# file.copy(from = files1[files1 != tmp], to = tmp[files1 != tmp],
#           overwrite = TRUE, recursive = FALSE,
#           copy.mode = TRUE, copy.date = TRUE)
files1[files1 != tmp] <- tmp[files1 != tmp]
files1 <- unique(files1)

## Removendo pastas com problemas
# files1 <- files1[!grepl("rvores", files1)]
files1 <- files1[!grepl("Briofitas", files1)]
files1 <- files1[!grepl("Liquenes", files1)]
files1 <- files1[!grepl("populacao", files1)]
files1 <- files1[!grepl("Orquidario", files1)]
files1 <- files1[!grepl("Feijoes", files1)]
files1 <- files1[!grepl("Psidium nao Campomanesia det MS VCS39945", files1)]

## Aqui iremos armazenar as pastas e subpastas e os nomes de cada imagem
files2 <- gsub(paste0(path,"/"), "", files1, perl = TRUE)
nomes <- gsub(".*/", "", files2, perl = TRUE)
caminhos <- mapply(function(x, y) { gsub(x, "", y, fixed = TRUE) },
              nomes, files2)
# caminhos[caminhos %in% ""] <- "raiz_da_pasta"
caminhos.split <- strsplit(caminhos, "\\/", perl = TRUE)
caminhos.split1 <- 
  as.data.frame(data.table::setDT(data.table::transpose(caminhos.split, fill = NA)))
names(caminhos.split1)[1:dim(caminhos.split1)[2]] <- 
  paste0("subpasta", 1:dim(caminhos.split1)[2])

## Aqui extraimos as informações (em loop para evitar erros)
### Obs: Se houver erros, eles serão printados nesse passo. Nesse caso,
### você pode renomear/excluir os arquivos com problema e começar de novo.
infos <- as.data.frame((exifr::read_exif(files1, recursive = FALSE)))
saveRDS(infos, "fotos_VCS.rds")
infos <- readRDS("fotos_VCS.rds")

if (!any(grepl("^GPS", colnames(infos), perl = TRUE)))
  infos[,c("GPSLatitude", "GPSLongitude", "GPSHPositioningError", "GPSAltitude")] <- NA_character_

## Aqui criamos a planilha que recebrá as informações
dados_imagens <- data.frame(ObsId = NA_character_,
                            FileName = infos$FileName, # obrigatório
                            Plantspecies = "undetermined", # obrigatório
                            Plantspecies_epithet = NA_character_,
                            Plantgenus = NA_character_,
                            PlantFamily = NA_character_,
                            Date = infos$CreateDate,
                            OrganType = NA_character_, #Pierre: si tu n'as pas l'info, nous la génèrerons automatiquement.
                            LocalityName = NA_character_,
                            Long = infos$GPSLongitude,
                            Lat = infos$GPSLatitude,
                            Author = autor,
                            Licence = licensa,
                            pn.user = user.pn,
                            pn.email = email.pn,
                            FileSize = infos$FileSize,
                            FileType = infos$FileType,
                            XResolution = infos$XResolution,
                            YResolution = infos$YResolution, 
                            ResolutionUnit = infos$ResolutionUnit ,
                            Flash = infos$Flash,
                            ISO = infos$ISO, 
                            ImageWidth = infos$ImageWidth, 
                            ImageHeight = infos$ImageHeight,
                            GPSHPositioningError = infos$GPSHPositioningError,
                            GPSAltitude = infos$GPSAltitude,
                            SourceFile = infos$SourceFile)

## Aqui combinamos tudo, se for o caso
if (length(nomes) > dim(infos)[1]) {
  files3 <- data.frame(SourceFile = files1)
  dados_imagens1 <- dplyr::left_join(files3, dados_imagens)
  replace_these <- !files1 %in% dados_imagens$SourceFile
  dados_imagens1[replace_these, c("FileName")] <- "ProbEncodingProblem"
} else {
  dados_imagens1 <- dados_imagens
}

# -------------------------------------------------------------------------
# PASSO 4 - EXTRAINDO AS INFORMAÇÕES DAS LOCALIDADES E ESPÉCIES

## Substituindo as informações
for (i in seq_len(length(formatos))) {
  
  # Replacing info
  pasta <- names(formatos)[i]
  formato.i <- formatos[[i]]
  
  if (pasta %in% "raiz") {
    replace_these <- is.na(caminhos.split1$subpasta1)
    
    if (formato.i %in% "FileNameEhEspecie")
      dados_imagens1$Plantspecies[replace_these] <-
        dados_imagens1$FileName[replace_these]
    
  } else {
    replace_these <- caminhos.split1$subpasta1 %in% pasta
    formato.i.spp <- formato.i[grepl("Especie", formato.i)]
    if (!is.null(locais))
      locais.i <- locais[[pasta]]
    
    if (formato.i.spp %in% "FileNameEhEspecie")
      dados_imagens1$Plantspecies[replace_these] <- 
        dados_imagens1$FileName[replace_these]
    
    if (formato.i.spp %in% "SubFolderNameEhEspecie")
      dados_imagens1$Plantspecies[replace_these] <- 
        caminhos.split1$subpasta2[replace_these]
    
    if (grepl("Localidade", formato.i)) {
      formato.i.loc <- formato.i[grepl("Localidade", formato.i)]
      
      if (formato.i.loc %in% "FolderNameEhLocalidade")
        dados_imagens1$LocalityName[replace_these] <-
          caminhos.split1$subpasta1[replace_these]
    }
    
    if (!is.null(locais)) {
      if (length(locais) > 0) {
        locais.i.loc <- locais.i["loc"]
        if (!is.na(locais.i.loc))
          dados_imagens1$LocalityName <- 
            gsub(pasta, locais.i.loc, dados_imagens1$LocalityName, perl = TRUE)
        
        locais.i.lat <- locais.i["lat"]
        if (!is.na(locais.i.loc))
          dados_imagens1$Lat[replace_these] <- locais.i.lat
        
        locais.i.long <- locais.i["long"]
        if (!is.na(locais.i.loc))
          dados_imagens1$Long[replace_these] <- locais.i.long
      }
    }
  }
}

# -------------------------------------------------------------------------
# PASSO 5 - EDITANDO AS INFORMAÇÕES

## Editando as informações taxonômicas
#Antoine: nom complet de l'espèce (ex: Acer campestre L.)
spp <- dados_imagens1$Plantspecies
spp <- gsub("\\s+", " ", spp, perl = TRUE)
spp <- gsub("^ | $", "", spp, perl = TRUE)
spp <- gsub("\\.jpg$", "", spp, ignore.case = TRUE, perl = TRUE)
spp <- gsub("\\([0-9][0-9]\\)$", "", spp, ignore.case = TRUE, perl = TRUE)
spp <- gsub("\\([0-9]\\)$", "", spp, ignore.case = TRUE, perl = TRUE)
spp <- gsub("\\[[0-9][0-9]\\]$", "", spp, ignore.case = TRUE, perl = TRUE)
spp <- gsub("\\[[0-9]\\]$", "", spp, ignore.case = TRUE, perl = TRUE)
spp <- gsub("^ | $", "", spp, perl = TRUE)
spp <- gsub(" Kew$", "", spp, perl = TRUE)
get_these <- "DSC|IMG|image"
replace_these <- !grepl(" ", spp, perl = TRUE) & 
                  grepl("[0-9][0-9]$", spp, perl = TRUE) & 
                  grepl(get_these, spp, perl = TRUE, ignore.case = TRUE)
spp[replace_these] <- "undetermined"
spp[!grepl("[a-z]", spp, perl = TRUE, ignore.case = TRUE)] <- "undetermined"
dados_imagens1$PlantspeciesOriginal <- spp
# Fixing name format
spp1 <- plantR::fixSpecies(spp, rm.indet = TRUE)
check_these <- spp1$scientificNameStatus %in% "family_as_genus" & 
                stringr::str_count(spp1$scientificName, " ") == 1
spp1$scientificName.new[check_these] <- 
  gsub(" .*", "", spp1$scientificName[check_these], perl = TRUE)

# Checking for typos, synonyms and valid names
spp2 <- plantR::prepSpecies(spp1, db = "bfo", sug.dist = 0.85, 
                            use.authors = FALSE, drop.cols = c("tmp.ordem", "author", 
                                                               "full_sp", "id"))
spp2$scientificNameFull <- plantR:::fixEncoding(spp2$scientificNameFull) 

replace_these <- !is.na(spp2$suggestedName) & !spp2$scientificName %in% "undetermined"
spp2$scientificName.new[replace_these] <-
  # spp2$suggestedName[replace_these]
  spp2$scientificNameFull[replace_these]
dados_imagens1$Plantspecies <- spp2$scientificName.new

replace_these <- dados_imagens1$PlantspeciesOriginal %in% "undetermined"
dados_imagens1$Plantspecies[replace_these] <- "undetermined"

replace_these <- grepl("NANA", dados_imagens1$Plantspecies)
dados_imagens1$Plantspecies[replace_these] <- "undetermined"

replace_these <- is.na(dados_imagens1$Plantspecies)
dados_imagens1$Plantspecies[replace_these] <- 
  dados_imagens1$PlantspeciesOriginal[replace_these]

replace_these <- !is.na(spp2$family)
dados_imagens1$PlantFamily[replace_these] <- 
  spp2$family[replace_these]

replace_these <- grepl(" ", dados_imagens1$Plantspecies)
split.nomes <- strsplit(dados_imagens1$Plantspecies, " ")
dados_imagens1$Plantgenus[replace_these] <-
  as.character(sapply(split.nomes[replace_these], head, 1))
dados_imagens1$Plantspecies_epithet[replace_these] <-
  as.character(sapply(split.nomes[replace_these], function(x) x[2]))

check_these <- !is.na(dados_imagens1$Plantspecies_epithet) & 
                  (nchar(dados_imagens1$Plantspecies_epithet) < 4 |
                     grepl("\\d", dados_imagens1$Plantspecies_epithet))
dados_imagens1$Plantspecies_epithet[check_these] <- NA_character_

check_these <- !is.na(dados_imagens1$Plantgenus) & 
  (nchar(dados_imagens1$Plantgenus) < 4 |
     grepl("\\d", dados_imagens1$Plantgenus))
dados_imagens1$Plantgenus[check_these] <- NA_character_

replace_these <- spp2$scientificNameStatus %in% "family_as_genus"
dados_imagens1$Plantgenus[replace_these] <- NA_character_
dados_imagens1$Plantspecies_epithet[replace_these] <- NA_character_

replace_these <- stringr::str_count(dados_imagens1$Plantspecies, " ") == 1 &
                  grepl("\\.|\\(", dados_imagens1$Plantspecies_epithet, perl = TRUE)
dados_imagens1$Plantspecies_epithet[replace_these] <- NA_character_

#### CHECK HERE ####
# Il y a eu des soucis sur l'encodage de certains caractères dans le fichier CSV. Par exemple "Rudgea jasminoides (Cham.) MÃ¼ll.Arg." au lieu de "Rudgea jasminoides (Cham.) Müll.Arg.". Pour les prochains imports, vous serait-il possible d'encoder les fichiers CSV en UTF-8 ?
# Les observations sont importées dans le projet "Brésil" et des fois les noms complets d'espèces ne correspondent pas tout à fait aux noms de ce projet, par exemple "Cyrtopodium flavum Link & Otto ex Rchb.f." dans votre fichier et "Cyrtopodium flavum (Nees) Link & Otto ex Rchb." dans notre référentiel. Si vous le souhaitez, vous pouvez aligner vos noms d'espèce avec ceux du projet : https://identify.plantnet.org/fr/brazil/species?illustratedOnly=false Cela permet notamment de rendre les noms d'espèces cliquables sur le site pour accéder à la fiche espèce correspondante. Vous pouvez proposer un nouveau nom en votant depuis la page de détail d'une observation.
dados_imagens1$Plantspecies_utf8 <- enc2utf8(dados_imagens1$Plantspecies)

## Editando as informações de data
#Pierre: soit un timestamp UNIX, soit un format homogène pour l'ensemble du fichier
dia <- dados_imagens1$Date
dados_imagens1$DateExif <- dados_imagens1$Date
dia.POSIXct <- as.POSIXct(dia, format = "%Y:%m:%d %H:%M:%S",
                          tz = "America/Sao_Paulo")
dia <- as.Date(dia, format = "%Y:%m:%d")
dia <- as.character(dia)
check_these <- is.na(dia)
if (any(check_these)) {
  # infos[check_these, names(infos)[grepl("Date", names(infos))]]
  dia[check_these] <- 
    as.character(as.Date(infos$FileModifyDate[check_these], format = "%Y:%m:%d"))
}
dados_imagens1$Date <- dia

#### CHECK HERE ####
#Les observations sans date. Dans ce cas, elles sont automatiquement assignées au 01.01.1970 à 00:00:00
check_these <- is.na(dia)
table(check_these) # All FALSE?

## Definindo o Obs.ID de cada imagem
#Pierre: - L'obsID est idéalement construit à partir du regroupement des photos prise 
#par le même photographe, de la même espèce, le même jour (et sur un pas de temps suffisamment court).
#C'est cela est trop risqué, compliqué de construire un tel id, nous créerons une obs. par image.
intervalo <- cut(dia.POSIXct, include.lowest = TRUE,
                  breaks = seq(min(dia.POSIXct, na.rm = TRUE),
                              max(dia.POSIXct, na.rm = TRUE),
                              by = 3600))
intervalo <- as.character(intervalo)
taxon <- dados_imagens1$Plantspecies
same_OBsID <- paste(taxon, intervalo, sep = "_")
dados_imagens1$ObsId <- as.double(as.factor(same_OBsID))

## Editando as informações de localidade
table(dados_imagens1$LocalityName)

# -------------------------------------------------------------------------
# PASSO 6 - EXTRAINDO INFORMAÇÕES AUXILIARES
nomes1 <- dados_imagens1$PlantspeciesOriginal
split.nomes1 <- strsplit(nomes1, " |_", perl = TRUE)
split.nomes1 <- lapply(split.nomes1, function(x) x[which(x %in% coletor) + 1])
split.nomes1[lengths(split.nomes1) == 0] <- ""
if (all(unlist(split.nomes1) %in% "")) {
  dados_imagens1$InfoExtra <- NA_character_
} else {
  split.nomes2 <- sapply(split.nomes1, function(x) paste (names(coletor), x))
  replace_these <- grepl(" $", split.nomes2)
  split.nomes2[replace_these] <- NA_character_
  
  replace_these <- is.na(split.nomes2) & !dados_imagens1$Plantspecies %in% "undetermined"
  split.nomes2[replace_these] <- paste(dados_imagens1$Plantspecies[replace_these], 
                                       dados_imagens1$Date[replace_these], 
                                       sep = "_")
  dados_imagens1$InfoExtra <- split.nomes2
}

# -------------------------------------------------------------------------
# PASSO 7 - OBTENDO OS NUMEROS DE COLETA
patt <- "[A-Z][A-Z][0-9][0-9]|[A-Z][A-Z] [0-9][0-9]"
check_these <- grepl(patt, dados_imagens1$PlantspeciesOriginal, perl = TRUE)
vouchers <- dados_imagens1$PlantspeciesOriginal[check_these]
vouchers <- gsub(".*?([A-Z][A-Z][A-Z] [0-9])", "\\1", vouchers, perl = TRUE)
vouchers <- gsub(".*?([A-Z][A-Z][A-Z][0-9])", "\\1", vouchers, perl = TRUE)
check_these1 <- !grepl("^[A-Z][A-Z][A-Z][0-9]|[A-Z][A-Z][A-Z] [0-9]", vouchers, perl = TRUE)
vouchers[check_these1] <- gsub(".*?([A-Z][A-Z] [0-9])", "\\1", vouchers[check_these1], perl = TRUE)
vouchers[check_these1] <- gsub(".*?([A-Z][A-Z][0-9])", "\\1", vouchers[check_these1], perl = TRUE)
vouchers <- gsub("([A-Z][A-Z]) ([0-9][0-9])", "\\1\\2", vouchers, perl = TRUE)
dados_imagens1$voucher <- NA
dados_imagens1$voucher[check_these] <- vouchers
  
# -------------------------------------------------------------------------
# PASSO 8 - SALVANDO OS RESULTADOS E ZIPANDO A PASTA PARA ENVIO
csv.file <- paste0(path,'/infos_imagens.csv')
write.csv(dados_imagens1, csv.file, row.names = FALSE, fileEncoding = "UTF-8")
xlsx.file <- paste0(path,'/infos_imagens.xlsx')
writexl::write_xlsx(dados_imagens1, xlsx.file)

# Se você precisar editar manualmente o arquivo criado, faça antes de gerar o .zip
# csv.file1 <- csv.file
csv.file1 <- readxl::read_excel(paste0(path,'/infos_imagens com correções de VCS.xlsx'))
files2zip <- c(csv.file1, dados_imagens1$SourceFile)
zip(zipfile = path, files = files2zip)

#copying to a folder first
all.folders <- regmatches(files2zip,gregexpr(".*(?<=/)",files2zip,perl=TRUE))
all.folders <- unique(sapply(all.folders, head, 1))
for(i in seq_along(all.folders)) {
# for(i in 1:5) {
  path.i <- all.folders[i]
  path.i <- gsub("D:/BotSist2021/", "E:/VCS_PlantNet/", path.i)
  dir.create(path = path.i, recursive = TRUE)
} 
  
file.copy(dados_imagens1$SourceFile, 
          to = gsub("D:/BotSist2021","E:/VCS_PlantNet",dados_imagens1$SourceFile),
          copy.date = TRUE, overwrite = TRUE)





