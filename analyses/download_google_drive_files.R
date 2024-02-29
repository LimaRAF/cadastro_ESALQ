#' Downloading Files from the Google Drive
#' 
#' @description 
#' This is a simple script to access and download the most up-to-date
#' version of the information related to the ESALQ Trees database
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' 
#' @details
#' Creation date: 28 Fev 2024
#' 
# PREPARING CREDENTIAL FOR DOWNLOAD  ------------------------------------
## Getting all objects in the workspace before the script starts
obj.to.keep <- ls()

## Set authentication token to be stored in a folder called `.secrets`
options(gargle_oauth_cache = ".secrets")

## Authenticate manually
googlesheets4::gs4_auth()

## If successful, the previous step stores a token file. Check that a file that
## stores a token exists with:
# googlesheets4::gs4_auth(cache = ".secrets",
#                         email = "raflima@usp.br")

# LATEST VERSION OF THE GD SPREADSHEET  ---------------------------------
link <- "https://docs.google.com/spreadsheets/d/1XJg-lW03M4T5UheuIci7OVUdv1kEzW5fBdDnXokm1OQ/edit?usp=sharing"
planilha <- as.data.frame(googlesheets4::read_sheet(link))
saveRDS(planilha, "./data/raw-data/cadastro_arvores_esalq.RDS")

# LATEST VERSION OF THE COMMON NAMES  ---------------------------------
link <- "https://docs.google.com/spreadsheets/d/15aHE9bzSx42GffEuu8pDUq-1Y8NuE_-uZxgUThZ4mT4/edit?usp=sharing"
comum <- as.data.frame(googlesheets4::read_sheet(link))
saveRDS(comum, "./data/raw-data/nomes_comuns.RDS")


## Removing all objects created in the script
all.objs <- ls()
rm.objs <- all.objs[!all.objs %in% obj.to.keep]
rm(list = rm.objs)
