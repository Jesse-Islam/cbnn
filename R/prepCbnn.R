#' prepCbnn
#'
#' This function attaches the required components to make survival outcomes
#' possible from a binary classification network. Namely, it adds an offset term
#' and passes the result through a Sigmoid activation function.
#' This offset term is an offset in the statistical sense. As such, the
#' network will add the offset term before passing through a sigmoid.
#'
#' @param features (list) Input features to be used in model.
#' @param nnInput (keras layer) Input layer for data to enter network.
#' @param nnOutput (keras layer) Final keras layer of user designed network. Expected final layer is a single node.
#' @param data (data.frame) Dataset to be used. can either be pre-casebase sampled with the sampleCaseBase function or the original dataset.
#' @param offset (vector) Column of offset values generated from sampleCaseBase.
#' @param timeVar (string) Survival time feature over which to eventually predict.
#' @param eventVar (string) The event Feature in the data-set.
#' @param ratio (numeric) Number of base-series samples per case-series samples. Default=100.
#' @param compRisk (boolean) states if modeling competing risks or single event.
#' @return (list) network: keras neural network model, \cr
#'  casebaseData: case-base sampled data,\cr
#'  offset: offset defined by case-base sampling,\cr
#'  timeVar: time variable (user defined),\cr
#'  eventVar: event variable (user defined),\cr
#'  features: feature of interest with user defined order.
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
#'
#' @export
prepCbnn<-function(features, nnInput, nnOutput, data,offset=NA, timeVar,eventVar, ratio=100, compRisk=FALSE){
  if(class(data)[1]=="cbData"){
    data<-data
  }else{
    data<-casebase::sampleCaseBase(data,time=timeVar,event=eventVar,ratio,compRisk)
  }
  offset<-data[,ncol(data),drop=F]
  data<-data[,-ncol(data),drop=F]

  offsetInput<- keras::layer_input(shape = c(1), name = 'offsetInput')
  mainOutput <- keras::layer_add(c(nnOutput,offsetInput)) %>%
    keras::layer_activation(activation="sigmoid")

  model<-keras::keras_model(
    inputs = c(nnInput, offsetInput),
    outputs = c(mainOutput)
  )

  model%>% keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(decay=10^-9), #optimizer_rmsprop(lr = 0.001)
    metrics = c('binary_accuracy')
  )
  features<-features[which(!(features %in% eventVar))]
  return(list(network=model,
              casebaseData=data,
              offset=offset,
              timeVar=timeVar,
              eventVar=eventVar,
              features=features)
         )

}
