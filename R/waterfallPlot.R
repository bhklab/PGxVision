#' Build a waterfall plot of drug response
#'
#' Use a waterfall plot to visualize drug sensitivity in decreasing order (
#' left -> right corresponds to most sensitive -> least sensitive). The
#' waterfall plot can also be colored by a third parameter, such as statistical
#' significance (p-value, fdr, etc.) There are many common uses of the
#' waterfall plot, such as:
#' * Find the most effective drugs for a particular tumour: plot different
#'   compounds along the x-axis, and the response of the tumor on the y-axis
#' * Find the tumour subtypes that respond best to a drug: plot different
#'   subtypes along the x-axis, and the response of each tumor to the compound
#'   of interest on the y-axis
#' @md
#'
#' @param drugResponseDf A data.frame of drug sensitivity measurements in
#' different tumours, subtypes, or replicates
#' @param xAxisCol The name of the column in biomarkerDf that will be mapped
#' on the x-axis. Since this is a discrete axis, drugResponseDf$xAxisCol should
#' be a vector of type "character".
#' @param drugSensitivityCol The name of the column in biomarkerDf that has the
#' drug sensitivity metrics; will be mapped on the y-axis. Since this is a
#' continuous axis, drugResponseDf$drugSensitivityCol should be a vector of
#' type "numeric".
#' @param colorCol (optional) The name of the column that will determine the
#' color of the bars (such as statistical significance). Since this is a
#' continuous axis, drugResponseDf$colorCol should be a vector of type
#' "numeric". If no column is provided, it will default to the
#' drugSensitivityCol.
#' @param xLabel (optional) The label for the x-axis. Defaults to `<xAxisCol>`.
#' @param yLabel (optional) The label for the y-axis. Defaults to
#' `<drugSensitivityCol>`.
#' @param title (optional) The title for the plot. Defaults to
#' '`<drugSensitivityCol>` vs. `<xAxisCol>`'
#'
#' @return A ggplot2 plot object mapping the drug sensitivity metrics against
#' the xAxisCol, sorted in descending order by drug sensitivity and colored by
#' colorCol
#'
#' @examples
#' data(BRCA.PDXE.paxlitaxel.response)
#' buildWaterfallPlot(BRCA.PDXE.paxlitaxel.response,
#'                    xAxisCol="tumour", drugSensitivityCol="angle",
#'                    colorCol="ODC1", xLabel="Tumour",
#'                    yLabel="Angle Between Treatment and Control",
#'                    title="Paclitaxel Response in BRCA Tumours")
#'
#' @importFrom checkmate assertDataFrame assertNames assertString testString
#' assertCharacter assertNumeric
#' @importFrom ggplot2 ggplot geom_bar scale_fill_continuous theme aes
#' theme_classic ggtitle element_text ylab xlab
#' @export
buildWaterfallPlot <- function(drugResponseDf, xAxisCol, drugSensitivityCol,
                               colorCol=NULL, xLabel=NULL, yLabel=NULL,
                               title=NULL) {
  # Assign axis labels and title, if user does not provide any
  if (is.null(xLabel)) {
    xLabel <- xAxisCol
    message(paste0("You have not provided a custom label for the x-axis.\n",
                   "The label will default to xAxisCol."))
  }
  if (is.null(yLabel)) {
    yLabel <- drugSensitivityCol
    message(paste0("You have not provided a custom label for the y-axis.\n",
                   "The label will default to drugSensitivityCol."))
  }
  if (is.null(title)) {
    title <- paste0(yLabel, " vs. ", xLabel)
    message(paste0("You have not provided a custom title for your plot.\n",
                   "The title will default to 'yLabel vs. xLabel'."))
  }

  # Check user inputs
  checkmate::assertDataFrame(drugResponseDf)
  checkmate::assertString(xAxisCol)
  checkmate::assertString(drugSensitivityCol)
  checkmate::assertString(xLabel)
  checkmate::assertString(yLabel)
  checkmate::assertString(title)

  requiredCols <- c(xAxisCol, drugSensitivityCol)
  if (checkmate::testString(colorCol)) {
    requiredCols[2] <- colorCol
  } else {
    message(paste0("You have not selected a color scheme for your waterfall ",
                   "plot.\nBars will be colored based on drug sensitivity."))
  }
  # Check that the dataframe actually has the data cols specified by the user
  checkmate::assertNames(colnames(drugResponseDf), must.include=requiredCols)
  # Check that the columns specified by the user have the correct types
  checkmate::assertCharacter(drugResponseDf[, xAxisCol])
  checkmate::assertNumeric(drugResponseDf[, drugSensitivityCol])
  if (!is.null(colorCol)) {
    checkmate::assertNumeric(drugResponseDf[, colorCol])
  }

  # Order the x-axis data points based on their drug sensitivity
  sortedDf <- drugResponseDf[order(drugResponseDf[,drugSensitivityCol],
                                decreasing=TRUE), ]
  sortedXAxis <- sortedDf[, xAxisCol]

  # Set fill column and legend title based on whether colorCol was provided
  if (is.null(colorCol)) {
    fill <- sortedDf[, drugSensitivityCol]
    legendName <- drugSensitivityCol
  } else {
    fill <- sortedDf[, colorCol]
    legendName <- colorCol
  }

  # Build the waterfall (sorted bar) plot
  plot <- ggplot2::ggplot(sortedDf, ggplot2::aes(
    x=factor(sortedXAxis, levels=sortedXAxis),
    y=sortedDf[, drugSensitivityCol], fill=fill))

  plot <- plot + ggplot2::geom_bar(stat="identity") +
          ggplot2::scale_fill_continuous(type="viridis", name=legendName)

  # Add title and axes labels
  plot <- plot + ggplot2::theme_classic() + ggplot2::ggtitle(title) +
    ggplot2::ylab(yLabel) + ggplot2::xlab(xLabel) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5,
                                                       hjust=1))

  return(plot)
}

# [END]
