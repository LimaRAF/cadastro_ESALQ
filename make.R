#' cadastro_ESALQ: A Research Compendium
#' 
#' @description 
#' A paragraph providing a full description of the project and describing each 
#' step of the workflow.
#' 
#' @author Renato Lima \email{raflima@usp.br}
#' 
#' @details
#' Creation date: 28 Fev 2024




## Install Dependencies (listed in DESCRIPTION) --------------------
# rcompendium::add_dependencies("R/")
# rcompendium::add_dependencies("analyses/")
devtools::install_deps(upgrade = "never")
remotes::install_github('matildabrown/rWCVPdata')


## Load Project Addins (R Functions and Packages) ------------------
devtools::load_all(here::here())


## Global Variables ------------------------------------------------
# You can list global variables here (or in a separate R script)


## Run Project --------------------------------------------------
source(here::here("analyses", "download_google_drive_files.R"))
source(here::here("analyses", "get_species_scientific_names.R"))
source(here::here("analyses", "get_species_common_names.R"))
source(here::here("analyses", "build_sql_code.R"))

