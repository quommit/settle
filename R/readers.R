# Supported document reading functions
# 
# Use lowercase and dot notation for naming readers. Do not use underscores (_) 
# since schema names in source document get standardized as R var names using 
# make.names(names, allow_ = FALSE) Good: readers$new.reader.name Bad: 
# readers$new_reader_name
# 
# This reader catalog is the extension point for both, new document reading
# functions and new S3 methods. The reader name (that is, the standardized
# schema name) is used to subclass the survey object which has been created with
# that particular document reading function. Thus, S3 methods dealing with new
# document schemas should just use the reader name as the class those
# methods belong to.
readers <- list()
readers$landholder.plot.neighbour <- function(document) {
  return(LPNschema_reader(document))
}
