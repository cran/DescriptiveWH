#' @title Arithmetic, Harmonic, Geometric and Weighted Mean
#' @description Function used to compute different kind of mean in variables.
#' @param x Numeric. A vector of the data set.
#' @param meth Character. To compute the arithmetic (meth="a"), harmonic ("h"), geometric ("g") or weighted ("w") mean. Only for
#' weighted mean option, an additional column including the weight of values is required.
#' @param narm Logical. Optional argument used to compute the mean removing NA values.
#' @param dec Numeric. A value to define the number of decimals in the returns.
#' @return This function returns a numerical value representing the mean depending on used method.
#' @details This function shows the mean as a numerical value from the data set. For the case of weighted mean, an additional column
#' must be included in the dataset containing the assigned weigths of each value. The weighted mean is computed taking into account the
#' total percentage of weights, that is, for cases that the sumed of wights is 100% or different (i.e. 90%, 120%, etc).
#' @importFrom stats sd
#' @examples
#' #Example 1
#' data1=c(7,6,NA,8,NA)
#' meanWH(x=data1,meth="a",narm=TRUE)
#' #Example 2
#' data2=rnorm(10,50,3)
#' meanWH(x=data2,meth="a",dec=2)
#' #Example 3
#' data3=rnorm(100,50,11)
#' meanWH(x=data3,meth="g")
#' #Example 4
#' data4=rnorm(10,20,4)
#' meanWH(data4,meth="h")
#' #Example 5  (total weight is closed to 100%)
#' df1=data.frame(value=c(9,8,7,8),weight=c(0.5,0.2,0.2,0.1))
#' meanWH(df1,meth="w")
#' #Example 6  (total weight is not closed 100%)
#' df2=data.frame(value=c(9,8,7,8),weight=c(0.5,0.2,0.2,0.2))
#' meanWH(df2,meth="w")
#' @export
meanWH=function(x,meth=NULL,narm=FALSE,dec=NULL){
 if( narm==TRUE ){ v=as.numeric(x[!is.na(x)]) }else{ v=x }
 if( is.null(dec)==FALSE ){ v=round(v,dec) }
  if(meth=="a"){
    arimean=sum(v)/length(v)
    v=arimean
  }
  if(meth=="g"){
    geomean=exp(mean(log(v)))
    v=geomean
  }
  if(meth=="w"){
    if( sum(v[,2])==1 ){
    v[,"Mult"]=v[,1]*v[,2]
    weimean=sum(v[,"Mult"])
    v=weimean
  }else{
    v[,"Mult"]=v[,1]*v[,2]
    weimean=sum(v[,"Mult"])
    weight_sum=sum(v[,2])
    weimean=weimean/weight_sum
    v=weimean
  }
 }
  if(meth=="h"){
    n=length(v)
    harmean=sum(1/v)
    harmean=n/harmean
    v=harmean
  }
v
}