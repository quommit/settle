# Landholder-plot-neighbour schema reader
LPNschema_reader <- function(document) {
  if (!(is.list(document))) {
    stop("Input data must be a list.")
  }
  landholders <- flatten(document)
  landholders_transposed <- transpose(landholders)
  landholders_normalized <- normalize(landholders_transposed)
  data <- list(created = Sys.time(), user = Sys.info()[["user"]],
               os = Sys.info()[["sysname"]], hypertable = landholders_normalized)
  return(data)
}

# private methods
flatten <- function(document) {
  landholders <- lapply(document$Landholders, function(x){ unlist(x)})
  landholders <- plyr::rbind.fill(
    lapply(landholders, function(x) do.call("data.frame", as.list(x))))
  return(landholders)
}

transpose <- function(landholders) {
  landholders <- reshape2::melt(landholders, id = c("Landholder"))
  landholders <- landholders[order(landholders$Landholder),]
  return(landholders)
}

normalize <- function(landholders) {
  landholders <- landholders[stats::complete.cases(landholders),]
  landholders <- data.frame(landholders$Landholder, landholders$variable, 
                            do.call(
                              "rbind", 
                              strsplit(as.character(landholders$variable), 
                                       ".", 
                                       fixed = TRUE)),
                            landholders$value)
  levels(landholders$X4) <- c(levels(landholders$X4), 0)
  landholders$X4[landholders$X4 == "Plots"] <- 0
  landholders$X4 <- paste(landholders$landholders.Landholder, 
                          landholders$X4, sep = '-')
  levels(landholders$X3) <- c(levels(landholders$X3), "Neighbour")
  landholders$X3[grepl("Neigh", landholders$X3)] <- "Neighbour"
  levels(landholders$X3) <- c(levels(landholders$X3), "Natural")
  landholders$X3[grepl("Natur", landholders$X3)] <- "Natural"
  levels(landholders$X3) <- c(levels(landholders$X3), "Anthropic")
  landholders$X3[grepl("Anthro", landholders$X3)] <- "Anthropic"
  levels(landholders$X3) <- c(levels(landholders$X3), "Administrative")
  landholders$X3[grepl("Admin", landholders$X3)] <- "Administrative"
  landholders<-data.frame(landholders$landholders.Landholder, landholders$X2, 
                          landholders$X3, landholders$X4, 
                          landholders$landholders.value)
  names(landholders)[1] <- "Landholder"
  names(landholders)[2] <- "VarCategory"
  names(landholders)[3] <- "Variable"
  names(landholders)[4] <- "Plot"
  names(landholders)[5] <- "Value"
  LandholderId <- as.numeric(landholders$Landholder)
  landholders <- cbind(LandholderId, landholders)
  return(landholders)
}
