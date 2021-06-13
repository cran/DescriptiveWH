#' @title Distribution of frequency
#' @description Function used to compute the absolute and relative frecuency of a categorical variable.
#' @param x Numeric or Character. A vector of the data set.
#' @param op Logical. Optional argument to see the data structure in graphical representation.
#' @return A data frame is returned by default (op=FALSE), including the absolute and relative frequeincies of the data set.
#' In addition, cumulative fequencies are included in this table. When "op=TRUE", the function returns only the absolute and relative
#' frequencies in a graphical representation through histogram.
#' @details This function shows a table including the frequency of values from a categorical vector. This vector can be formed by numeric,
#' character or mixed values, that is, numerical and character values. The table also includes the number of categories in which data
#' is distributed. The absolute and relative frecuencies can be showed using histogram.
#' @importFrom stats sd
#' @importFrom grDevices rainbow
#' @importFrom graphics boxplot hist lines par plot barplot
#' @examples
#' #Example 1
#' data1=c("A","A","A","A",2,3,2,3,"B","B","B")   
#' freqWH(data1)
#' #Example 2
#' data2=c("A","A","B","A","C")
#' freqWH(data2)
#' #Example 3
#' data3=c(5,5,3,1,5,2,3,6,9,6,6)
#' freqWH(data3)
#' #Example 4
#' data4=c("A1","A2","B3","A2","C")
#' freqWH(data4)
#' @export
freqWH=function(x,op=FALSE){
  N=length(x)
  Values=unique(x)
  df=data.frame(Class=1:length(Values),Values,Abs_Freq=0,Rel_Freq=0,Cum_Abs_Freq=0,Cum_Rel_Freq=0)
  cl=1:dim(df)[1]
   for (i in cl){
    Abs_Freq=length(x[x==df[i,"Values"]])
    Rel_Freq=round( (Abs_Freq/N)*100 ,2)
    df[i,3:4]=c(Abs_Freq,Rel_Freq)
    Cum_Abs_Freq=sum( df[,"Abs_Freq"] )
    Cum_Rel_Freq=round(   ((sum(df[,"Abs_Freq"]))/N)*100  ,2)
    df[i,5:6]=c(Cum_Abs_Freq,Cum_Rel_Freq)
   }
  df
   if(op==TRUE){
   if(is.numeric(x)==TRUE){
    RR=dim(df)[1]
    CC=dim(df)[2]
    oldpar = par(mfrow=c(2,1) , no.readonly = TRUE)
    m=t(as.matrix(df[,2:3],RR,2))
    colnames(m)=m[1,]
    m=m[-1,]
    barplot(  m , main = "Absolute frequency", col = rainbow(RR))
    m=t(as.matrix(df[,c(2,4)],RR,2))
    colnames(m)=m[1,]
    m=m[-1,]
    barplot(  m , main = "Relative frequency (%)", col = rainbow(RR))
    on.exit(par(oldpar))
   }else{
    RR=dim(df)[1]
    CC=dim(df)[2]
    df[,"rec"]=1:RR
    sel=as.character(df[,2])
    oldpar = par(mfrow=c(2,1) , no.readonly = TRUE)
    m=t(as.matrix(df[,c(5,3)],RR,2))
    colnames(m)=m[1,]
    m=m[-1,]
    barplot(  m , main = "Absolute frequency", col = rainbow(RR), names.arg = sel )
    m=t(as.matrix(df[,c(5,4)],RR,2))
    colnames(m)=m[1,]
    m=m[-1,]
    barplot(  m , main = "Relative frequency (%)", col = rainbow(RR), names.arg = sel )
    on.exit(par(oldpar))
   }
   }else{ df }
}