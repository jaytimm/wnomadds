#' Extract cut line coordinates
#'
#' A simple function for extracting cutting line coordinates and roll call polarity from an object of class `nomObj` via the `wnominate` package.
#'
#' @name wnm_get_cutlines
#' @param x A nomObj via wnominate::wnominate output
#' @param rollcall_obj A rollcall object from pscl package
#' @param arrow_length A numeric value specifying arrow length for subsequent visualization
#' @return A data frame



#Modified from wnominate package
#' @export
#' @rdname wnm_get_cutlines
wnm_get_cutlines <- function(x,
                             arrow_length = 0.05,
                             rollcall_obj,...) {

  dims <- c(1,2)
  row.names(x$rollcalls) <- dimnames(rollcall_obj$votes)[[2]]

  constrained <- ((abs(x$rollcalls[,"spread1D"]) > 0.0 | abs(x$rollcalls[,"spread2D"]) > 0.0)
                  & (x$rollcalls[,"midpoint1D"]**2 + x$rollcalls[,"midpoint2D"]**2) < .95)

  cutlineData <- cbind(x$rollcalls[constrained,paste("midpoint",dims[1],"D",sep="")],
                       x$rollcalls[constrained,paste("spread",dims[1],"D",sep="")],
                       x$rollcalls[constrained,paste("midpoint",dims[2],"D",sep="")],
                       x$rollcalls[constrained,paste("spread",dims[2],"D",sep="")])
  cutlineData <- na.omit(cutlineData)
  ns <- na.omit(row.names(x$rollcalls)[constrained])


  cuts <- apply(cutlineData, 1, add.cutline,
                weight=x$weights[dims[2]]/x$weights[dims[1]])
  names(cuts) <- ns

  cuts <- data.table::rbindlist(cuts, id = 'Bill_Code')

  cuts <- cuts[complete.cases(cuts),]
  cuts$id <- rep(1:2, length(unique(cuts$Bill_Code)))
  cuts <- data.table::melt(cuts, c('Bill_Code','id'), c('x','y'),
                           variable.name="variable", value.name="value")
  cuts <- data.table::dcast (cuts,
                             Bill_Code ~ paste0(variable,"_",id),
                             value.var = "value")
  cuts <- cuts[, c('Bill_Code', 'x_1','y_1', 'x_2', 'y_2')]

  cuts$pols <- get_polarity(x, rollcall_obj, cuts)
  fin_cuts <- get_arrows (cuts, arrow_length = arrow_length)
  subset(fin_cuts, select = -c(pols))
}

