#' Build MultiDimensional Feature Selector from IGs
#'
#' @details
#' Build MultiDimensional Feature Selector from IGs
#'
#' @param x input data where columns are variables and rows are observations (all numeric)
#' @param y decision variable as a boolean vector of length equal to number of observations
#' @param params method as accepted by \code{\link[stats]{p.adjust}} (\code{"BY"}
#' is recommended for FDR, see Details)
#' @return A \code{\link{data.frame}} with selected features and p.value
#'
#'
#' @examples
#' \dontrun{
#'
#' decisions <- data$class
#' data$class <- NULL
#'
#' fs.mdfs.1D(data, decisions, params = list(adjust = 'holm', alpha = 0.05))
#' }
#'
#'
#' @import FCBF
#' @importFrom stats p.adjust
#' @export
fs.fcbf <- function(x, y, params = list(su = 0.25)){
  if (!is.data.frame(x)) data = as.data.frame(x)
  xf <- discretize_exprs(x[,1:params$feature.number])
  result <- fcbf(xf,
                y,
                minimum_su = params$su,
                n_genes_selected_in_first_step = NULL,
                verbose = FALSE,
                samples_in_rows = TRUE,
                balance_classes = FALSE)
vars <- result$index
numvars <- as.numeric(vars)
var.imp <- data.frame(name = t(df[1, numvars]), score = result$SU)
rownames <- rownames(var.imp)
rownames(var.imp) <- as.numeric(result$index)
return(var.imp)
}
