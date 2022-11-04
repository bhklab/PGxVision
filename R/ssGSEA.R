#' Return a data.frame containing the results of single-sample gene set enrichment analyis
#' on a patient sample. 
#'
#' @param patientDf `data.frame` Single patient sample gene expression table, where 
#' rownames are genes and only one column is present for the expression of each gene
#' @param geneSetsJSON `character` This is the path to the JSON file for the gene set collection. 
#' The JSON file must contain two objects. A `name` which correspnods to a character
#' value containing the name of the gene set collection, and a `genesets` object which 
#' contains all the gene sets for that collection. 
#'
#' @return `data.frame` ssGSEA output Data.Frame with rownames corresponding to gene sets 
#' and one column containing the ssGSEA estimates (between -1 to 1) for each gene set
#'
#' @import GSVA
#' @import jsonlite
#' @export

performSSGSEA <- function(patientDf, geneSetsJSON) {
  jsonData <- fromJSON(geneSetsJSON)
  geneSetName <- jsonData$name
  geneSetDescriptions <- jsonData$descriptions
  geneSetList <- jsonData$genesets
  
  estimate <- gsva(expr=data.matrix(patientDf), 
                   gset.idx.list = geneSetList, 
                   method='ssgsea', 
                   min.sz=1,
                   verbose=FALSE)
  
  # Also get a column of the actual list of genes in every gene set
  genes <- vapply(rownames(estimate), \(x) paste0(jsonData[["genesets"]][[x]], collapse=", "), character(1))
  
  return( list(results = cbind(estimate, genes), name = geneSetName, descriptions = geneSetDescriptions)) 
}