#' @title Distribution of records
#' @description This function is useful to see the frequency of data and descriptive statistics
#' @param x Numeric. A vector containing the data set.
#' @param dec Numeric. A value defined to set the number of decimals in the returns.
#' @return List including the table with the absolute frequency of data, also the mean, median, mode and range of data.
#' @details This function shows statistical descriptive values of a vector.
#' @importFrom stats density median sd var
#' @examples
#' #Example 1
#' dat=c(1,5,2,4,5,1,3,5,3,5)
#' distrWH(x=dat,dec=3)
#' @export
distrWH=function(x,dec=FALSE){
if( dec==TRUE ){ decimals=dec }
  N=length(x)
  Values=sort(unique(x))
  df=data.frame(Values,Abs_Freq=0)
  cl=1:dim(df)[1]
   for (i in cl){
    Abs_Freq=length(x[x==df[i,"Values"]])
    df[i,2]=Abs_Freq
   }
   df
Mean=(sum(df[,"Values"]*df[,"Abs_Freq"]))/sum(df[,"Abs_Freq"])
n_mo=max(df[,"Abs_Freq"])
mo=df[df[,"Abs_Freq"]==n_mo,]
Mode=mo[,1]
Median=median(x)
Range=range(x)
Return=list(table=df,Mean=Mean,Median=Median,Mode=Mode,Range=Range)
Return
}