#' Build/extract cutline data
#'
#' Functions extract cutting line data for usage in 'tidier' frameworks.
#'
#' @name get_wnominate_underlying
#' @param x A nomObj via wnominate::wnominate output
#' @param rollcall_obj A rollcall object from pscl package
#' @return A data frame



#Modified from wnominate package
#' @export
#' @rdname wnm_get_cutlines
wnm_get_cutlines <- function(x,
                             dims=c(1,2),
                             arrow_length = 0.05,
                             vote_id = NULL,
                             rollcall_obj,...) {

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

  cuts <- data.table::rbindlist(cuts, id = 'Bill_Code') ##Rename

  cuts <- cuts[complete.cases(cuts),]
  cuts$id <- rep(1:2, length(unique(cuts$Bill_Code))) ##!
  cuts <- data.table::melt(cuts, c('Bill_Code','id'), c('x','y'), ##!
                           variable.name="variable", value.name="value")
  cuts <- data.table::dcast (cuts,
                             Bill_Code ~ paste0(variable,"_",id),  #SEEMS unique to nmsl53 data set -- ?
                             value.var = "value")
  cuts <- cuts[, c('Bill_Code', 'x_1','y_1', 'x_2', 'y_2')]

  cuts$pols <- get_polarity(x, rollcall_obj, cuts)
  fin_cuts <- get_arrows (cuts, arrow_length = arrow_length)
  subset(fin_cuts, select = -c(pols))
}

