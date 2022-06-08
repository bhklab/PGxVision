#' Return data frame of relevant biomarkers based on patient's differential gene expression
#' based on genes with expression >90th percentile and <10th percentile.
#'
#' @param patientDF `data.frame` Patient sample gene expression table, where
#' rownames are genes and only one column is present for the expression of each gene
#' @param referenceDF `data.frame` where rows are features (i.e. genes), columns
#'   are samples, and values are a measurement on each feature for each sample.
#'   Rownames must match rownames in `patientDF`
#' @param biomarkerDF `data.frame` This is the PharcoDB dataframe, containing all biomarkers
#'   Every row is a biomarker association. Gene names in `patientDF` and
#'
#' @return `data.frame` Data frame containing rows with all biomarkers (from PharmacoDB)
#' relevant to the patient's overexpressed and underexpressed genes. Each row is a
#' biomarker association.
#'
#' @export
retrieveDiffExpressedGenes <- function(patientDF, referenceDF, biomarkerDF) {

  ecdf_list <- apply(as.matrix(referenceDF), MARGIN=1, FUN=ecdf)
  percentiles <- Map(function(x, y) x(y), ecdf_list, patientDF[rownames(referenceDF), ])

  overexpressed <- percentiles[percentiles >= 0.9]
  underexpressed <- percentiles[percentiles <= 0.1]

  #TODO: automatically detect whether the clinician is uploading a sample that uses simple gene IDs or
  #ENSEMBL IDs

  subsetOver = biomarkerDF[( biomarkerDF$estimate > 0 & biomarkerDF$gene_symbol %in% names(overexpressed)), ]
  subsetUnder = biomarkerDF[(biomarkerDF$estimate < 0 & biomarkerDF$gene_symbol %in% names(underexpressed)), ]

  subsetOver$percentile <- overexpressed[subsetOver[["gene_symbol"]]]
  subsetUnder$percentile <- underexpressed[subsetUnder[['gene_symbol']]]

  return(rbind(subsetOver, subsetUnder))
}
