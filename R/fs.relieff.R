#' Test U Manna-Whitneya (U-test) for feature selection
#'
#' @details
#' In case of FDR control it is recommended to use Benjamini-Hochberg-Yekutieli
#' p-value adjustment
#'
#' @param x input data where columns are variables and rows are observations (all numeric)
#' @param y decision variable as a boolean vector of length equal to number of observations
#' @param params method as accepted by \code{\link[stats]{p.adjust}} (\code{"BY"}
#' is recommended for FDR, see Details)
#' @return A \code{\link{data.frame}} with selected features and p.value
#'
#' @examples
#'
#' \dontrun{
#'
#' decisions <- data$class
#' data$class <- NULL
#'
#' fs.utest(data, decisions, params = list(adjust = 'holm', alpha = 0.05))
#' }
#'
#' @importFrom stats wilcox.test p.adjust
#' 
#' @export
fs.relieff <- function(x, y, params = list(feature.number = 100)){
  result <- fs.relief(x, y)
  stats <- as.data.frame(result$stats)
  var.names <- row.names(stats)
  scores <- stats[,1]
  var.imp <- as.data.frame(cbind(var.names, scores))
  names(var.imp) <- c('name', 'score')
  var.imp <- var.imp[order(var.imp$score, decreasing=T),][1:params$feature.number,]
  return(var.imp)
}