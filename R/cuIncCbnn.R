#' cuIncCbnn
#'
#'This function calculates the cumulative incidence using a cbnn model.
#'
#' @param fit (list) output from fitHazard function.
#' @param times (numeric) vector of time points at which you wish you calculate the cumulative incidence.
#' @param x_test (matrix) matrix containing the data you wish to predict on.
#' @return (matrix) a matrix representing the cumulative incidence of each individual.
#'
#' @examples
#' library(cbnn)
#' library(casebase)
#' library(magrittr)
#' data<-casebase::ERSPC
#' data$ScrArm<-as.numeric(data$ScrArm)-1
#' eventVar<-"DeadOfPrCa"
#' timeVar<-"Follow.Up.Time"
#' features<-"ScrArm"
#' nnInput<-keras::layer_input(shape=length(features))
#' nnOutput<-nnInput %>% keras::layer_dense(units=1, use_bias = TRUE)
#' cbnnPrep<-prepCbnn(features, nnInput, nnOutput, data, offset=NA,timeVar,
#' eventVar, ratio=10, compRisk=FALSE)
#' fit<-fitHazard(cbnnPrep,epochs=1,batchSize=500)
#' times<-seq(min(data$Follow.Up.Time),max(data$Follow.Up.Time),length.out=10)
#' x_test<-as.matrix(data[sample(nrow(data),100),])
#' cumulativeIncidenceCurves<-cuIncCbnn(fit, times=times,x_test=x_test)
#' @export



cuIncCbnn<-function(fit, times=times,x_test=x_test){
  tempOffset<-as.matrix(data.frame(offset=rep(0,nrow(x_test))))
  x_test<-as.matrix(x_test[,c(fit$features),drop=F])
  timeColumn<-which(fit$timeVar== colnames(x_test))
  results<- matrix(data=NA,nrow=length(times),ncol= nrow(x_test)+1)
  results[,1]<-times
  for(i in 1:length(times)){
    x_test[,timeColumn]<-rep(times[i],nrow(x_test))
    results[i,-1]=fit$network%>% predict(list(x_test,tempOffset),verbose=0)
  }
  for (i in 2:ncol(results)){
    results[,i]=1-exp(-1*cumsum((results[,i]/(1-results[,i]))*(diff(times)[1])))
  }
  return(results)
}

