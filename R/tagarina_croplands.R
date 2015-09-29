#' Excerpt from Sella's XVIII century croplands inventory
#' 
#' Sella is a small village to the north of Alicante province, in southeastern 
#' Spain. Carried out in 1726, Sella's croplands inventory is an example of the 
#' so called 'capbreus', the ancient land registries that were periodically 
#' undertaken in settlements within the Crown of Aragon beginning in late Middle
#' Ages and all along the early modern period. These data are an excerpt of the 
#' Capbreu of Sella that includes all records concerning plots located within 
#' the Valley of Tagarina, a traditional farming area to the north-east of the 
#' village.
#' 
#' @author Pablo Gimenez Font (for the transcription and YAML source document)
#'   
#' @format A survey object that encapsulates a data frame which is accessible 
#'   through get_records.survey method. The Capbreu of Sella has been ported to a YAML 
#'   structured text document which conveys the Landholder-Plot-Neighbour (LPN) 
#'   schema and can be read using settle's LPNschema_reader. The output is a 
#'   data frame organized as a hyper-table or wide-column store which holds 460 
#'   records and 6 fields: \describe{ \item{LandholderId}{Peasant's integer 
#'   identifier} \item{Landholder}{Peasant's name transcription} 
#'   \item{VarCategory}{Column family (Aggregations, Landmetrics or Limits)} 
#'   \item{Variable}{Column name whose value is stored in the Value field (Area 
#'   = Plot area; Level_N = Place name according to level N of spatial 
#'   aggregation; Neighbour = Neighbouring peasant's name; Natural = 
#'   Topographical feature this plot limits with; Anthropic = Road or irrigation
#'   infrastructure this plot limits with; Administrative = Neighbouring 
#'   settlement's name)} \item{Plot}{Plot identifier} \item{Value}{Corresponding
#'   value of the column which this record refers to} }
#'   
#' @source Province of Alicante Historical Archive
"tagarina_croplands"