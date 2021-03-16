#' @title Outliers detection
#' @description The function used to detect atypical values in a data set
#' @param x Numeric. A vector of the data set.
#' @param lower Numeric. A value representing the lower limit of outliers.
#' @param upper Numeric. A value representing the upper limit of outliers.
#' @param dec Numeric. A value to define the decimal numbers
#' @return A list including three elements: 1) A data frame including the mean, the standart deviation, and
#' the upper and lower limits of outliers, 2) A numeric vector with data considered no outliers, and 2) A numeric
#' vector including the putative outliers.
#' @details This function shows a list with the putative outliers and other without those.
#' If you do not type lower and upper values, by default the value is assumed as the standard deviation
#' of "x" multipied two.
#' @importFrom stats sd
#' @examples
#' #Example 1
#' data=rnorm(100,30,15)
#' outlierWH( x=data ,lower=15 ,upper=35 )
#' #Example 2
#' outlierWH( x=data )
#' #Example 3
#' outlierWH( x=data ,lower=15 )
#' @export
outlierWH=function(x,lower=NULL,upper=NULL,dec=NULL){
   if(is.null(dec)==TRUE){ dec=3 }
   if(is.numeric(x)==TRUE){   v=x
   if(is.numeric(lower)==TRUE && is.numeric(upper)==TRUE){
    Mean = mean(v)
    SD = sd(v)
    Lower_limit = Mean-lower
    Upper_limit = Mean+upper
    Outlier_interval = round(data.frame(Mean,SD,Lower_limit,Upper_limit),dec)
   }
   if(is.null(lower)==TRUE && is.null(upper)==TRUE){
    Mean = mean(v)
    SD = sd(v)
    Lower_limit = Mean-(sd(v)*2)
    Upper_limit = Mean+(sd(v)*2)
    Outlier_interval = round(data.frame(Mean,SD,Lower_limit,Upper_limit),dec)
   }
   if(is.numeric(lower)==TRUE && is.null(upper)==TRUE){
    Mean = mean(v)
    SD = sd(v)
    Lower_limit = Mean-lower
    Upper_limit = Mean+(sd(v)*2)
    Outlier_interval = round(data.frame(Mean,SD,Lower_limit,Upper_limit),dec)
   }
   if(is.null(lower)==TRUE && is.numeric(upper)==TRUE){
    Mean = mean(v)
    SD = sd(v)
    Lower_limit = Mean-(sd(v)*2)
    Upper_limit = Mean+upper
    Outlier_interval = round(data.frame(Mean,SD,Lower_limit,Upper_limit),dec)
   }
    outliers_list=c(v[v<Lower_limit] , v[v>Upper_limit])
    no_outliers_list=v[v>Lower_limit]
    no_outliers_list=no_outliers_list[no_outliers_list<Upper_limit]
    outliers_list=round(sort(outliers_list),dec)
    no_outliers_list=round(sort(no_outliers_list),dec)
    L=list(Outlier_interval=Outlier_interval,no_outliers=no_outliers_list,outliers=outliers_list)
    L
  }else{ print("data must be numeric class") }
}