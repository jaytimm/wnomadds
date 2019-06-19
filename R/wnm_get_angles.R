#' Extract cut line angles from an object of class `nomObject`
#'
#' A simple function for extracting cut line angles from an object of class `nomObject`.
#'
#' @name wnm_get_angles
#' @param x A nomObj via wnominate::wnominate output
#' @return A data frame


#Modified from wnominate package
#' @export
#' @rdname wnm_get_angles
wnm_get_angles <- function(x) {

  dims <- c(1,2)
  weight <- x$weight[dims[2]]/x$weight[dims[1]]

  contrained <- ((abs(x$rollcalls[,paste("spread",dims[1],"D",sep="")]) > 0.0 |
                    abs(x$rollcalls[,paste("spread",dims[2],"D",sep="")]) > 0.0)
                 & (x$rollcalls[,paste("midpoint",dims[1],"D",sep="")]**2 +
                      x$rollcalls[,paste("midpoint",dims[2],"D",sep="")]**2) < .95)

  cutvector1 <- na.omit(x$rollcalls[contrained,paste("spread",dims[2],"D",sep="")]*weight/
                          sqrt(x$rollcalls[contrained,paste("spread",dims[1],"D",sep="")]^2
                               + weight^2*x$rollcalls[contrained,paste("spread",dims[2],"D",sep="")]^2))
  cutvector2 <- -1*na.omit(x$rollcalls[contrained,paste("spread",dims[1],"D",sep="")]/
                             sqrt(x$rollcalls[contrained,paste("spread",dims[1],"D",sep="")]^2
                                  + weight^2*x$rollcalls[contrained,paste("spread",dims[2],"D",sep="")]^2))
  cutvector1[cutvector2<0] <- -cutvector1[cutvector2<0]
  cutvector2[cutvector2<0] <- -cutvector2[cutvector2<0]

  data.frame(Bill_Code=ns <- na.omit(row.names(x$rollcalls)[contrained]), ##!
             angle=atan2(cutvector2,cutvector1)*180/pi, stringsAsFactors = FALSE)
}
