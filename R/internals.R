#' 
#' @title Remove Unwanted Spaces
#' 
#' @param x a character or vector
#'
#' @return the character `x` without trailing or double spaces
#'  
#' @keywords internal
#'
#' @noRd
#' 
.squish <- function (x) {
  x <- gsub("\\s\\s+", " ", as.character(x), perl = TRUE)
  # x <- gsub("  ", " ", x, perl = TRUE)
  x <- gsub("^ | $", "", x, perl = TRUE)
  return(x)
}

#' 
#' @title Build Organism Name
#' 
#' @description Combine diffent table columns with species name information (i.e.
#'   genus, epiteth, infra-epiteth) into a single organism name
#' 
#' @param x the data frame with the taxonomic information
#' @param col.names the name of the columns containing the information to be
#'   combined in the desired order
#'
#' @return a vector with the combined information
#' 
#'  
#' @keywords internal
#' 
#' @noRd
#' 
.build.name <- function(x, col.names = c("Genus_original", "Species_original"))
{
  
  if (any(!col.names %in% colnames(x)))
    stop("One or more names in 'col.names' were not found in 'x'")
  
  # cols <- names(x)[names(x) %in% col.names]
  cols <- names(x)[match(col.names, names(x), nomatch = 0)]
  
  if (length(cols) > 1) {
    
    #organismName <- apply(x[, cols], 1, paste, collapse = " ")
    organismName <- do.call(paste, x[, cols])
    organismName <- gsub(" NA$", "", organismName, perl = TRUE)
    organismName <- .squish(organismName)
    return(organismName)
  } else {
    warning("Less than two columns found; skipping...")
  }    
}

#' 
#' 
#' @title Standardize Species Annotators
#' 
#' @description Standardize the annotation of name modificators (e.g. cf., aff., etc)
#' 
#' @param x a vector
#' @param annotation the annotation desired to be standardized (i.e. 'cf',
#'   'aff', 'var', 'subsp')
#' @param paste logical. Should the cf. or var. be binded in the front of the
#'   name? Defaults to TRUE.
#' @param fix.authors logical. Should the function try to remove authors from
#'   var. or subsp? Defaults to TRUE.
#'
#' @return the standardized vector `x`
#'  
#' @keywords internal
#' 
#' @noRd
#' 
.fix_annotation <- function(x, annotation = NULL, 
                            paste = TRUE, fix.authors = TRUE) {
  
  if (is.null(annotation))
    return(x)
  
  x <- .squish(x)
  
  if (annotation %in% "cf") {
    x1 <- gsub("^cf ", "cf. ", x, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^cf, ", "cf., ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" cf ", " cf. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" cf$", " cf.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("(^cf\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
    x1 <- gsub("( cf\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
    x1 <- gsub("(cf\\. )([A-Z])", "\\1\\L\\2", x1, perl = TRUE)
    x1 <- gsub("(cf\\.)([A-Z])", "\\1 \\L\\2", x1, perl = TRUE)
    
    if (paste) {
      check_these <- grepl(" cf\\.$| cf\\. ", x1, perl = TRUE)
      check_these[is.na(check_these)] <- FALSE
      if (any(check_these))
        x1[check_these] <- 
        paste0("cf. ", 
               gsub(" cf\\.$", "", 
                    gsub(" cf\\. ", " ", x1[check_these], perl = TRUE),
                    perl = TRUE))
    }
    
    x1 <- gsub("^cf$", "cf.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^Cf\\.$", "cf.", x1, perl = TRUE)
    x1 <- gsub(" Cf\\. ", " cf. ", x1, perl = TRUE)
    x1 <- gsub("^cf\\. ", "cf. ", x1, perl = TRUE, ignore.case = TRUE)
    
    return(x1)
  }
  
  
  if (annotation %in% "aff") {
    x1 <- gsub("^aff ", "aff. ", x, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^aff, ", "aff., ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" aff ", " aff. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" af\\. ", " aff. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" aff$", " aff.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("(^aff\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
    x1 <- gsub("( aff\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
    x1 <- gsub("(aff\\. )([A-Z])", "\\1\\L\\2", x1, perl = TRUE)
    x1 <- gsub("(aff\\.)([A-Z])", "\\1 \\L\\2", x1, perl = TRUE)
    
    if (paste) {
      check_these <- grepl(" aff\\.$| aff\\. ", x1, perl = TRUE) & 
        !grepl(" aff\\. [A-Z]", x1, perl = TRUE) & 
        !grepl("nov\\.", x1, perl = TRUE)
      check_these[is.na(check_these)] <- FALSE
      if (any(check_these))
        x1[check_these] <- 
        paste0("aff. ", 
               gsub(" aff\\.$", "", 
                    gsub(" aff\\. ", " ", x1[check_these], perl = TRUE),
                    perl = TRUE))
    }    
    
    x1 <- gsub("^aff$", "aff.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^Aff\\.$", "aff.", x1, perl = TRUE)
    x1 <- gsub(" Aff\\. ", " aff. ", x1, perl = TRUE)
    x1 <- gsub("^aff\\. ", "aff. ", x1, perl = TRUE, ignore.case = TRUE)
    
    return(x1)
  }
  
  if (annotation %in% "var") {
    x1 <- gsub("^var ", "var. ", x, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" var ", " var. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" var$", " var.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("(^var\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
    x1 <- gsub("( var\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
    x1 <- gsub("([a-z])(var\\.)([a-z])", "\\1 \\2 \\3", x1, perl = TRUE)
    x1 <- gsub("(var\\. )([A-Z])", "\\1\\L\\2", x1, perl = TRUE)
    x1 <- gsub("^var$", "var.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^Var\\.$", "var.", x1, perl = TRUE)
    x1 <- gsub(" Var\\. ", " var. ", x1, perl = TRUE)
    if (fix.authors)
      x1 <- .fix_authors(x1)
    
    return(x1)
  }
  
  if (annotation %in% "subsp") {
    x1 <- gsub("^subsp |^spp |^ssp ", "subsp. ", x, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^spp\\. |^ssp\\. ", "subsp. ", x, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" subsp | spp | ssp ", " subsp. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" subsp$| spp$| ssp$", " subsp.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" spp\\. ", " subsp. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" ssp\\. ", " subsp. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub(" sub\\. ", " subsp. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^spp\\. ", "subsp. ", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^ssp\\.", "subsp.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("(^subsp\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
    x1 <- gsub("( subsp\\.)([a-z])", "\\1 \\2", x1, perl = TRUE)
    x1 <- gsub("([a-z])(subsp\\.)([a-z])", "\\1 \\2 \\3", x1, perl = TRUE)
    x1 <- gsub("^subsp$|^spp$", "subsp.", x1, perl = TRUE, ignore.case = TRUE)
    x1 <- gsub("^Subsp\\.$", "subsp.", x1, perl = TRUE)
    x1 <- gsub(" Subsp\\. ", " subsp. ", x1, perl = TRUE)
    x1 <- gsub("(subsp\\. )([A-Z])", "\\1\\L\\2", x1, perl = TRUE)
    x1 <- gsub("(subsp\\.)([A-Z])", "\\1\\ \\L\\2", x1, perl = TRUE)
    x1 <- gsub("^subspd", "subsp. d", x1, perl = TRUE)
    x1 <- gsub("^subspp", "subsp. p", x1, perl = TRUE)
    x1 <- gsub("^subsps", "subsp. s", x1, perl = TRUE)
    x1 <- gsub("^sspr", "subsp. r", x1, perl = TRUE)
    if (fix.authors)
      x1 <- .fix_authors(x1)
    
    return(x1)
  }
} 

