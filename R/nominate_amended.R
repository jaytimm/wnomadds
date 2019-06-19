#' Some internal functions for extracting cut lines, cut line angles, and roll call polarity from an object of class `nomObj` via the `wnominate` package.
#'
#'
#' @name nominate_amended
#' @return A data frame


# Modified from wnominate package
add.cutline <- function(cutData,
                        weight) {

  slope <- -cutData[2]/(cutData[4]*weight)
  if (is.na(slope)) {
    x <- c(cutData[1],cutData[1])
    y <- c(sqrt(1-cutData[1]^2),-sqrt(1-cutData[1]^2))
    slope <- NA
    intercept <- NA
  }
  else {
    intercept <- -slope*cutData[1]+cutData[3]
    x <- c( (-slope*intercept + sqrt( (slope*intercept)^2 -
                                        (1+slope*slope)*(intercept*intercept-1)))/(1+slope*slope),
            (-slope*intercept - sqrt( (slope*intercept)^2 -
                                        (1+slope*slope)*(intercept*intercept-1)))/(1+slope*slope) )
    if (is.na(x[1])) {
      warning("Couldn't solve for points on the unit circle!\n")
      x<-NA
      y<-NA
      slope<-NA
      intercept<-NA
    }
    else {
      y <- intercept + slope*x
      y[y < -1] <- -sqrt(1-x[y<1]^2)
      y[y >  1] <-  sqrt(1-x[y>1]^2)
    }
  }
  data.frame(x=x,y=y) #Outputs x1,y1/ x2,y2 coords for single cutline.
}


get_arrows <- function(cuts,
                       arrow_length = arrow_length){

  arrows <- data.frame()
  for (i in 1:nrow(cuts)) {

    v <- c(cuts$x_2[i]- cuts$x_1[i], cuts$y_2[i] - cuts$y_1[i]) # calculate vector

    if (cuts$pol[i] == -1) {vnp <- c( v[2], -v[1] )} else {
      vnp <- c( -v[2], v[1] ) }

    arrows <- rbind(arrows,
                    data.frame(x_1a = cuts$x_1[i] + arrow_length*vnp[1],
                               y_1a = cuts$y_1[i] + arrow_length*vnp[2],
                               x_2a = cuts$x_2[i] + arrow_length*vnp[1],
                               y_2a = cuts$y_2[i] + arrow_length*vnp[2])
    )
  }
  cbind (cuts, arrows)

}



get_polarity <- function (x,
                          rollcall_obj,
                          cuts) {

  pol <- vector()

  x1 <- x$rollcalls[cuts$Bill_Code,]
  leg1 <-  x$legislators$coord1D
  x2 <- rollcall_obj$votes[,cuts$Bill_Code]


  for (i in 1:nrow(cuts)) {
    q <- data.frame(x= c(cuts$x_1[i],cuts$x_2[i]), y = c(cuts$y_1[i],cuts$y_2[i]))
    q1 <- lm(q$y ~ q$x)
    ln <- q1$coefficients[2]*leg1 + q1$coefficients[1]
    ln <- data.frame(cbind(ln, vote=as.integer(x2[,i])))
    ln$pol <- ln$ln < 0

    q2 <- data.frame(table(ln$vote,ln$pol), stringsAsFactors = FALSE)
    q2 <- subset(q2, Var1 == 1)
    q2 <- q2[which.max(q2$Freq),]

    if ( q2$Var2=='TRUE') {
      pol[i] <- -1} else {pol[i] <- 1}
  }

  pol}
