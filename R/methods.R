#' Build/extract cutline data
#'
#' Functions extract cutting line data for usage in 'tidier' frameworks.
#'
#' @name get_cut_data
#' @param x A nomObj via wnominate::wnominate output
#' @param rollcall_obj An rollcall object from pscl package
#' @return A data frame


#Modified from wnominate package
add.cutline <- function(cutData,weight) {

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


#Modified from wnominate package
#' @export
#' @rdname get_cut_data
wnom_adds_get_angles <- function(x, dims=c(1,2),...) {

    weight<-x$weight[dims[2]]/x$weight[dims[1]]

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

    data.frame(Bill_ID=ns <- na.omit(row.names(x$rollcalls)[constrained]),
               angle=atan2(cutvector2,cutvector1)*180/pi, stringsAsFactors = FALSE)
}


#i <- 8

get_arrows <- function(cuts, arrow_length = 0.05){

  arrows <- data.frame()
  for (i in 1:nrow(cuts)) {

  v <- c(cuts$x_2[i]- cuts$x_1[i], cuts$y_2[i] - cuts$y_1[i]) # calculate vector
  #v <- v/sqrt((v[1]**2 + v[2]**2)) # normalize vector
  #vnp <- c( -v[2], v[1] ) # perpendicular unit vector

  if (cuts$pol[i] == -1) {vnp <- c( v[2], -v[1] )} else {
    vnp <- c( -v[2], v[1] ) } #Flips results

  #if (v[2]/v[1]>=0 && cuts$pol[i] == -1) vnp <- c( v[2], -v[1] )
  #if (v[2]/v[1]>=0 && cuts$pol[i] == 1) vnp <- c( -v[2], v[1] )
  #if (v[2]/v[1]<0 && cuts$pol[i] == -1) vnp <- c( -v[2], v[1] )
  #if (v[2]/v[1]<0 && cuts$pol[i] == 1) vnp <- c( v[2], -v[1] )

  #Also, simply adding polarity*arrow_length worked, just not perpendicular
  arrows <- rbind(arrows,
                  data.frame(x_1a = cuts$x_1[i] + arrow_length*vnp[1],
                             y_1a = cuts$y_1[i] + arrow_length*vnp[2],
                             x_2a = cuts$x_2[i] + arrow_length*vnp[1],
                             y_2a = cuts$y_2[i] + arrow_length*vnp[2])
                  )
  }
  cbind (cuts, arrows)

}


#


#x <- resultd2
#rollcall_obj <- datRC
#i <- 15
#Modified from NOMINATE code
get_polarity <- function (x, rollcall_obj, cuts) {

  pol <- vector()

  x1 <- x$rollcalls[cuts$Bill_ID,]
  leg1 <-  x$legislators$coord1D
  x2 <- rollcall_obj$votes[,cuts$Bill_ID]


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


#Modified from wnominate package
#' @export
#' @rdname get_cut_data
wnom_adds_get_cutlines <- function(x,
          dims=c(1,2), add_arrows = TRUE, rollcall_obj,...) {

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

  cuts <- data.table::rbindlist(cuts, id = 'Bill_ID')
  cuts <- cuts[complete.cases(cuts),]
  cuts$id <- rep(1:2, length(unique(cuts$Bill_ID)))
  cuts <- data.table::melt(cuts, c('Bill_ID','id'), c('x','y'),
              variable.name="variable", value.name="value")
  cuts <- data.table::dcast (cuts, Bill_ID ~ paste0(variable,"_",id), value.var = "value")

  if (add_arrows) {
    cuts$pols <- get_polarity(x, rollcall_obj, cuts)
    fin_cuts <- get_arrows (cuts, arrow_length = 0.05)
    fin_cuts } else { cuts}
}
