% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prepCbnn.R
\name{prepCbnn}
\alias{prepCbnn}
\title{prepCbnn}
\usage{
prepCbnn(
  features,
  nnInput,
  nnOutput,
  data,
  offset = NA,
  timeVar,
  eventVar,
  ratio = 100,
  compRisk = FALSE,
  kOptim = keras::optimizer_adam()
)
}
\arguments{
\item{features}{(list) Input features to be used in model.}

\item{nnInput}{(keras layer) Input layer for data to enter network.}

\item{nnOutput}{(keras layer) Final keras layer of user designed network. Expected final layer is a single node.}

\item{data}{(data.frame) Dataset to be used. can either be pre-casebase sampled with the sampleCaseBase function or the original dataset.}

\item{offset}{(vector) Column of offset values generated from sampleCaseBase.}

\item{timeVar}{(string) Survival time feature over which to eventually predict.}

\item{eventVar}{(string) The event Feature in the data-set.}

\item{ratio}{(numeric) Number of base-series samples per case-series samples. Default=100.}

\item{compRisk}{(boolean) states if modeling competing risks or single event.}

\item{kOptim}{("keras.optimizers.adam.Adam") Optimizer of choice from keras. Default set to optimizer_adam()}
}
\value{
(list) network: keras neural network model, \cr
 casebaseData: case-base sampled data,\cr
 offset: offset defined by case-base sampling,\cr
 timeVar: time variable (user defined),\cr
 eventVar: event variable (user defined),\cr
 features: feature of interest with user defined order.
}
\description{
This function attaches the required components to make survival outcomes
possible from a binary classification network. Namely, it adds an offset term
and passes the result through a Sigmoid activation function.
This offset term is an offset in the statistical sense. As such, the
network will add the offset term before passing through a sigmoid.
}
\examples{
library(cbnn)
library(casebase)
library(magrittr)
data<-casebase::ERSPC
data$ScrArm<-as.numeric(data$ScrArm)-1
eventVar<-"DeadOfPrCa"
timeVar<-"Follow.Up.Time"
features<-"ScrArm"
nnInput<-keras::layer_input(shape=length(features))
nnOutput<-nnInput \%>\% keras::layer_dense(units=1, use_bias = TRUE)
cbnnPrep<-prepCbnn(features, nnInput, nnOutput, data, offset=NA,timeVar,
eventVar, ratio=10, compRisk=FALSE)

}
