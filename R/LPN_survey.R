#' @export
get_landholders <- function(object, with_names = TRUE) {
  UseMethod("get_landholders", object)
}

#' @export
get_landholders.default <- function(object, with_names = TRUE) {
  warning(warn_msg$method_not_implemented)
  return(NULL)  
}

#' Landholders listing
#' 
#' \code{get_landholders} retrieves all landholders in the survey object.
#' 
#' This method extracts the listing of unique landholder names or identifiers 
#' from the raw hyper-table in the survey object.
#' 
#' @param object A \code{\link{survey}} object of landholder.plot.neighbour
#'   subclass.
#'   
#' @param with_names A logical flag to get landholder names if TRUE, or 
#'   landholder identifiers otherwise.
#'   
#' @return A character vector of unique names or identifiers.
#' 
#' @examples
#' get_landholders(tagarina_croplands)
#' get_landholders(tagarina_croplands, with_names = FALSE)
#' 
#' @export
get_landholders.landholder.plot.neighbour <- 
  function(object, with_names = TRUE) {
    colname <- ifelse(with_names, "Landholder", "LandholderId")
    records <- get_records(object)
    return(levels(factor(records[[colname]])))
}

#' @export
nlandholders <- function(object) {
  UseMethod("nlandholders", object)
}

#' @export
nlandholders.default <- function(object) {
  warning(warn_msg$method_not_implemented)
  return(NULL)  
}

#' Landholders count
#' 
#' \code{nlandholders} gets the number of landholders in the survey object.
#' 
#' This method extracts the number of unique landholder names from the raw
#' hyper-table in the survey object.
#' 
#' @param object A \code{\link{survey}} object of landholder.plot.neighbour 
#'   subclass.
#'   
#' @return A non negative integer of length 1.
#'   
#' @examples
#' nlandholders(tagarina_croplands)
#' 
#' @export
nlandholders.landholder.plot.neighbour <- function(object) {
  landholders <- get_landholders(object)
  return(length(landholders))
}

#' @export
get_agglevel_names <- function(object, level) {
  UseMethod("get_agglevel_names", object)
}

#' @export
get_agglevel_names.default <- function(object, level) {
  warning(warn_msg$method_not_implemented)
  return(NULL)
}

#' Aggregation level names
#' 
#' \code{nlandholders} gets all place names which refer to a particular level of
#' spatial aggregation.
#' 
#' This method extracts a collection of distinct place names from the raw
#' hyper-table in the survey object.
#' 
#' @param object A \code{\link{survey}} object of landholder.plot.neighbour 
#'   subclass.
#'
#' @param level A positive integer of length 1.
#' 
#' @return A character vector of unique names.
#'   
#' @examples
#' get_agglevel_names(tagarina_croplands, 1)
#' 
#' @export
get_agglevel_names.landholder.plot.neighbour <- function(object, level) {
  records <- get_records(object)
  levelname <- paste0("Level_", level)
  query <- records[grep(levelname, records$Variable),]
  return(levels(factor(query$Value)))
}

#' @export
nplots <- function(object) {
  UseMethod("nplots", object)
}

#' @export
nplots.default <- function(object) {
  warning(warn_msg$method_not_implemented)
  return(NULL)  
}

#' @export
nagglevels <- function(object) {
  UseMethod("nagglevels", object)
}

#' @export
nagglevels.default <- function(object) {
  warning(warn_msg$method_not_implemented)
  return(NULL)
}

#' Aggregation levels count
#' 
#' \code{nlandholders} gets the number of levels land lots can be aggregated in.
#' 
#' This method extracts the number of land lot spatial aggregation levels from
#' the metadata in the survey object's header.
#' 
#' @param object A \code{\link{survey}} object of landholder.plot.neighbour 
#'   subclass.
#'   
#' @return A non negative integer of length 1.
#'   
#' @examples
#' nagglevels(tagarina_croplands)
#' 
#' @export
nagglevels.landholder.plot.neighbour <- function(object) {
  length(object$header$metadata$aggregations)
}

#' Plot count
#' 
#' \code{nlandholders} gets the number of land lots in the survey object.
#' 
#' This method extracts the number of unique land lots from the raw 
#' hyper-table in the survey object.
#' 
#' @param object A \code{\link{survey}} object of landholder.plot.neighbour 
#'   subclass.
#'   
#' @return A non negative integer of length 1.
#'   
#' @examples
#' nplots(tagarina_croplands)
#' 
#' @export
nplots.landholder.plot.neighbour <- function(object) {
  records <- get_records(object)
  return(nlevels(as.factor(records$Plot)))
}
