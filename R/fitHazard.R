#' fitHazard
#'
#' This function trains the model on casebase sampled data. It adds the loss as
#' it changes over time, list of training data used as input and finally output
#' training data to the cbnn prep.
#'
#' @param cbnnPrep (list) output from prepcbnnPrep function.
#' @param epochs (numeric) number of epochs to learn through.
#' @param batchSize (numeric) Number of samples to use for each batch.
#' @param earlyStoppingCallbacks (list) expecting parameters for
#' keras::callback_early_stopping as a list
#' @param valData (list) A list of Lists. List[[1]][[1]] is the feature matrix,
#' list[[1]][[2]] is the prediction,list[[2]] is the offset column. This is optional.
#' @return (list) cbnnPrep: list from prepcbnnPrep passed in as argument,\cr
#' resultOfFit: loss and metric over epochs,\cr
#' xTrain: casebase sampled data that is used in learning phase,\cr
#' yTrain: event feature used to assess what was learned in learning phase.\cr
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
#'
#' @export
fitHazard<-function(cbnnPrep,epochs=20000,batchSize=500,earlyStoppingCallbacks=NULL,valData=NULL){


  offset<-as.matrix(cbnnPrep$offset)

  xTrain<-cbnnPrep$casebaseData[,-c(which(colnames(cbnnPrep$casebaseData) %in% cbnnPrep$eventVar))]

  xTrain<-as.matrix(cbnnPrep$casebaseData[,c(cbnnPrep$features)])
  yTrain<-as.matrix(cbnnPrep$casebaseData[,c(which(colnames(cbnnPrep$casebaseData) %in% cbnnPrep$eventVar))])
  xTensor<-list(xTrain, offset)


    resultOfFit<-cbnnPrep$network %>% keras::fit(
      x = xTensor,
      y = yTrain,
      epochs = epochs,
      batch_size = batchSize,
      validation_data=valData,
      shuffle=T,
      callbacks=earlyStoppingCallbacks,verbose=0)

  cbnnPrep[[length(cbnnPrep)+1]]<-resultOfFit
  names(cbnnPrep)[length(cbnnPrep)]<-"resultOfFit"
  cbnnPrep[[length(cbnnPrep)+1]]<-xTrain
  names(cbnnPrep)[length(cbnnPrep)]<-"xTrain"
  cbnnPrep[[length(cbnnPrep)+1]]<-yTrain
  names(cbnnPrep)[length(cbnnPrep)]<-"yTrain"

  return(cbnnPrep)

}
