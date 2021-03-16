#' @title Descriptive statistics
#' @description This function is useful to see the basic statistical information of a variable.
#' @param x Numeric. A vector containing the data set.
#' @param dec Numeric. A value defined to set the number of decimals in the returns.
#' @param op Logical. If TRUE, returns plot, histogram, boxplot and qqnorm of the data set.
#' @return If op=FALSE (defined by default), the function returns a matrix (1 row, 9 columns) with values of 
#' the mean, the standart deviadtion, the minumun, the median, the maximum, the variance,
#' the variation coefficient, the first quartil value and the third quartil value of the data set.
#' Whereas if op=TRUE , the function returns four graphics: 1) A plot with X axis the number
#' of samples and Y axis the amount of values included in the data set, 2) The density histogram of data set
#' with a red line representing the distribution shape of the data, 3) A classical boxplot of the data,
#' and 4) the qqnorm plot including the qqline used to check the normal disribution in a data set through
#' visual inspection.
#' @details This function shows statistical values including several visual tools.
#' @importFrom stats density median qqnorm qqline sd var
#' @importFrom graphics boxplot hist lines par plot
#' @examples
#' #Example 1
#' data=c(rnorm(1000,30,10))
#' DeStatWH( x=data ,dec=2 )
#' #Example 2
#' DeStatWH( x=data , op=TRUE )
#' @export
DeStatWH=function(x,dec=NULL,op=FALSE){
  v=as.numeric(x)
  if(is.null(dec)){ dec=3 }
  if(op==FALSE){
    m=matrix(0,1,9)
    colnames(m)=c("Mean","SD","Min","Median","Max","variance","CV","1st Qu.","3rd Qu.")
    rownames(m)="values"
    m[1,1] =round(  mean(x),dec  )
    m[1,2] =round(  sd(x),dec  )
    m[1,3] =round(  min(x),dec  )
    m[1,5] =round(  median(x),dec  )
    m[1,4] =round(  max(x),dec  )
    m[1,6] =round(  var(x),dec  )
    m[1,7] =round(  sd(x)/mean(x),dec  )
    m[1,8] =round(  summary(x)[2],dec  )
    m[1,9] =round(  summary(x)[5],dec  )
    return(m)
  }else{
    oldpar = par(mfrow=c(2,2) , no.readonly = TRUE)
    plot(  c(1:length(x)),  x,  ylab="Y"  ,xlab="samples"  ,main="Plot"  )
    hist(x,main="Histogram",xlab="in red: the distribution of data",freq=FALSE,col="lightblue1")
    lines(density(x),col="red",lwd=2)
    boxplot(x,main="boxplot",ylab="Min, 1st Qu., Mean, 3rd Qu., Max",xlab="only vector")
    qqnorm(x)
    qqline(x, col = "red", lwd = 2)
    on.exit(par(oldpar))
  }
}