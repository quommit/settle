# Supported file loading functions
loaders <- list()
loaders$yml <- function(filename) {
  return(yaml::yaml.load_file(filename))
}
