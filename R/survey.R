#'Surveys
#'
#'\code{survey} is a constructor function that generates an instance of a survey
#'object from an structured text document that collects spatially related
#'observations.
#'
#'A survey is a collection of spatially related observations or records that 
#'conform to a structure or schema (e.g. landholder-plot-neighbour) for which 
#'the settle package provides a suitable reader. A survey object is useful to 
#'explore spatial semantics within the collection in order to spatially 
#'aggreggate data, plot neighbouring items or identify and locate clusters.
#'
#'@param filename Path to a structured text document in one of the supported 
#'  formats.
#'@return A survey object or an error in case path is invalid or format/schema 
#'  is not supported.
#'  
#'@section Supported text formats: Currently the only supported format is YAML.
#'  
#'@section Supported schemas: The only implemented reader is LPNschema_reader, 
#'  which can parse documents with a landholder-plot-neighbour schema.
#'  
#'@export
survey <- function(filename) {
  if (!is.character(filename)) {
    stop(err_msg$path_not_valid)
  } else if (!file.exists(filename)) {
    stop(err_msg$file_missing)
  } else if (!is_supported_format(filename)) {
    stop(err_msg$format_not_supported)
  } else {
    document <- load_file(filename)
    survey_header <- load_header(document)
    if (!schema_exists(survey_header)) {
      stop(err_msg$schema_is_missing)
    } else {
      reader_function <- get_reader_function(survey_header)
      if (is.null(reader_function)) {
        stop(err_msg$schema_not_supported)
      } else {
        survey_data <- read_document(reader_function, document)
        new_survey = list(header = survey_header, body = survey_data)
        # survey objects get automatically subclassed using the reader name
        # selected from the readers list
        class(new_survey) <- 
          append(class(new_survey),
                 c("survey", get_reader_name(survey_header)))
        return(new_survey)
      }
    }  
  }
}

#' @export
get_records <- function(object) {
  UseMethod("get_records", object)
}

#' @export
get_records.default <- function(object) {
  warning(warn_msg$method_not_implemented)
  return(NULL)
}

#' Survey data retrieval
#' 
#' \code{get_records} retrieves all records in the survey object provided.
#' 
#' This is the accessor method for the raw hyper-table that holds all records in
#' a survey object.
#' 
#' @param object A \code{\link{survey}} object.
#' 
#' @return A data frame organized as hyper-table or column-wide store.
#' 
#' @examples
#' get_records(tagarina_croplands)
#' 
#' @export
get_records.survey <- function(object) {
  return(object$body$hypertable)
}

#' @export
get_recordcount <- function(object) {
  UseMethod("get_recordcount", object)
}

#' @export
get_recordcount.default <- function(object) {
  warning(warn_msg$method_not_implemented)
  return(NULL)
}

#' Survey record count
#' 
#' \code{get_records} gets number of records in the survey object provided.
#' 
#' This method counts the total number of records in a survey object's
#' hyper-table.
#' 
#' @inheritParams get_records.survey
#'   
#' @return An integer of length 1 or NULL.
#'   
#' @examples
#' get_recordcount(tagarina_croplands)
#' 
#' @export
get_recordcount.survey <- function(object) {
  return(nrow(get_records(object)))
}

#' @export
get_mapcatalogue <- function(object) {
  UseMethod("get_mapcatalogue", object)
}

#' @export
get_mapcatalogue.default <- function(object) {
  warning(warn_msg$method_not_implemented)
  return(NULL)
}

#' @export
get_mapcatalogue.survey <- function(object) {
  mapnames <- get_mapnames(object)
  featuretypes <- get_featuretypes(object)
  coordsystems <- get_EPSG_codes(object)
  return(data.frame(mapname = mapnames, 
                    featuretype = featuretypes, 
                    coordsystem = coordsystems))
}

# Format checking function intended for internal use
is_supported_format <- function(filename) {
  return(tolower(tools::file_ext(filename)) %in% names(loaders))
}

# File loading function intended for internal use
load_file <- function(filename) {
  key <- tolower(tools::file_ext(filename))
  f <- loaders[[key]]
  return(f(filename))
}

# Header loading function intended for internal use
load_header <- function(document) {
  meta <- list(structure = document$Structure,
               title = document$Title,
               description = document$Description,
               landmetrics = document$Landmetrics,
               aggregations = document$Aggregations)
  geom <- NULL
  if (is.vector(document$Spatial_data)) {
    geom <- lapply(document$Spatial_data, function(x) {
      rgdal::readOGR(dsn = x,  layer = rgdal::ogrListLayers(x))})
    geom <- stats::setNames(geom, basename(document$Spatial_data))    
  }
  header <- list(metadata = meta, geometries = geom)
  return(header)
}

# Schema checking function intended for internal use
schema_exists <- function(header) {
  schema_name <- header$metadata$structure
  return(!is.null(schema_name) &&
           !anyNA(schema_name) &&
           nchar(trimws(schema_name)) > 0)
}

# Readers list accessor intended for internal use
get_reader_function <- function(header) {
  reader_name <- get_reader_name(header)
  if (!is.null(reader_name)) {
    return(readers[[reader_name]])
  } else {
    return(NULL)
  }
}

# Readers list accessor intended for internal use
get_reader_name <- function(header) {
  schema_name <- header$metadata$structure
  reader_name <- stdvarname(schema_name)
  if (reader_name %in% names(readers)) {
    return(reader_name)
  } else {
    return(NULL)
  }
}

# Document reading function intended for internal use
read_document <- function(reader_function, document) {
  return(reader_function(document))
}

# R var name generator intended for internal use
stdvarname <- function(varname) {
  return(tolower(make.names(varname, allow_ = FALSE)))
}

# Geometries list accessor intended for internal use
get_geoms <- function(survey) {
  return(survey$header$geometries)
}

# Geometry source names list accessor intended for internal use
get_mapnames <- function(survey) {
  geoms <- get_geoms(survey)
  return(names(geoms))
}

# Geometry type list accessor intended for internal use
get_featuretypes <- function(survey) {
  geoms <- get_geoms(survey)
  return(vapply(geoms, class, character(1), USE.NAMES = FALSE))
}

# PROJ4 string list accessor intended for internal use
get_proj4_strings <- function(survey) {
  geoms <- get_geoms(survey)
  return(vapply(geoms, function(g) g@proj4string@projargs, 
                character(1), USE.NAMES = FALSE))
}

# PROJ4 strings to EPSG codes translation intended for internal use
get_EPSG_codes <- function(survey) {
  srcatalogue <- rgdal::make_EPSG()
  to_EPSG_code <- function(proj4) {
    query <- srcatalogue[which(srcatalogue$prj4 == proj4), ]
    n <- nrow(query)
    # return last EPSG code found
    return(query[n,]$code)
  }
  coordsystems <- get_proj4_strings(survey)
  return(vapply(coordsystems, to_EPSG_code, integer(1), USE.NAMES = FALSE))
}