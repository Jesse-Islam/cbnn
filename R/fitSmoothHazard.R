#' fitHazard
#'
#' This function trains the model on casebase sampled data. It adds the loss as
#' it changes over time, list of training data used as input and finally output
#' training data to the cbnn prep.
#'
#' @param cbnnPrep (list) output from prepcbnnPrep function.
#' @param epochs (numeric) number of epochs to learn through.
#' @param batchSize (numeric) Number of samples to use for each batch.
#' @param verbose (numeric) tells keras::fit whether to be verbose during the
#' learning process. 0=silent, 1=partial, 2=full.
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
#' eventVar<-"DeadOfPrCa"
#' timeVar<-"Follow.Up.Time"
#' features<-"ScrArm"
#' nnInput<-keras::layer_input(shape=length(features))
#' nnOutput<-nnInput %>% keras::layer_dense(units=1, use_bias = TRUE)
#' cbnnPrep<-prepCbnn(features, nnInput, nnOutput, data, offset=NA,timeVar,
#' eventVar, ratio=10, compRisk=FALSE)
#' fitHazard(cbnnPrep,epochs=1,batchSize=500,verbose=0)
#'
#' @export
fitHazard<-function(cbnnPrep,epochs=20000,batchSize=500,verbose=0){


  offset<-as.matrix(cbnnPrep$offset)

  xTrain<-cbnnPrep$casebaseData[,-c(which(colnames(cbnnPrep$casebaseData) %in% cbnnPrep$eventVar))]

  xTrain<-as.matrix(cbnnPrep$casebaseData[,c(cbnnPrep$features)])
  yTrain<-as.matrix(cbnnPrep$casebaseData[,c(which(colnames(cbnnPrep$casebaseData) %in% cbnnPrep$eventVar))])
  xTensor<-list(xTrain, offset)

  resultOfFit<-cbnnPrep$network %>% keras::fit(x = xTensor,
    y = yTrain,
    epochs = epochs,#30000,
    batchSize = batchSize,#54540,
    verbose=verbose,callbacks=list(keras::callback_early_stopping(
      monitor = "loss",
      min_delta = 10^-6,
      patience = 1000,
      verbose = 1,
      mode = c("auto"),
      baseline =NULL,#lossCutOff,
      restore_best_weights = F
    )))

  cbnnPrep[[length(cbnnPrep)+1]]<-resultOfFit
  names(cbnnPrep)[length(cbnnPrep)]<-"resultOfFit"
  cbnnPrep[[length(cbnnPrep)+1]]<-xTrain
  names(cbnnPrep)[length(cbnnPrep)]<-"xTrain"
  cbnnPrep[[length(cbnnPrep)+1]]<-yTrain
  names(cbnnPrep)[length(cbnnPrep)]<-"yTrain"

  return(cbnnPrep)

}
