## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
#install.packages(cbnn)
library(cbnn)
library(keras)
##########################################
### Required packages for data simulation
##########################################
library(flexsurv)
library(simsurv)
library(splines)
#Splines is needed here too to permit a flexible baseline hazard
############################################
### Required packages to run linear Cox
############################################
library(survival)
############################################
### Required packages to run linear casebase
############################################
library(casebase)
############################################
### Required package for IPA and AUC_{IPCW}
############################################
library(riskRegression)
############################################
### visualization packages
############################################
library(ggplot2)


##########################
###Hyperparameters
##########################

min_delta = 0 
epo=2000
patience <- 10
iteration<-100
numSamp<-5000


#note that these weren't the best hyperparameters, just a set that works ok. proper hyperparameter optimization procedures are ideally done for your specific use case.
lr=c(0.001)
drpt=c(0.01)
layer1=c(50)
layer2=c(10)
bsize=c(100)
actv=c("relu")



## ----simPrep1-----------------------------------------------------------------

##############################
###set up complex simulation
##############################

# Should the user have specific scenarios they want to simulate, examples can be found at 
# https://cran.r-project.org/web/packages/simsurv/vignettes/simsurv_usage.html#example-3-simulating-under-a-weibull-model-with-time-dependent-effects

# Define a function returning the log cum hazard at time t
logcumhaz <- function(t, x, betas, knots) {
  
  # Obtain the basis terms for the spline-based log
  # cumulative hazard (evaluated at time t)
  basis <- flexsurv::basis(knots, log(t))
  
  # Evaluate the log cumulative hazard under the
  # Royston and Parmar specification
  res <-betas[["gamma0"]] * basis[[1]] + 
    betas[["gamma1"]] * basis[[2]] +
    betas[["gamma2"]] * basis[[3]] +
    betas[["gamma3"]] * basis[[4]] +
    betas[["gamma4"]] * basis[[5]] +
    betas[["z1"]] * x[["z1"]]+
    betas[["z2"]] * x[["z2"]]+
    betas[["z3"]] * x[["z3"]]+
    betas[["term1"]] * x[["z1"]]*t+
    betas[["term3"]] * x[["z2"]]*(x[["z3"]])
  #
  # Return the log cumulative hazard at time t
  res
}

## -----------------------------------------------------------------------------
# get a real fit on breast cancer survival to generate our flexible baseline.
true_mod <- flexsurv::flexsurvspline(Surv(rectime, censrec) ~ 1, data = simsurv::brcancer, k = 3) #from flexsurv

#########################
### Define coefficients
#########################
coefficients<-true_mod$coefficients
coefficients<-c(coefficients,z1=-5,z2=-1,z3=1,term1=0.001,term3=-1) #terms are interactions

## -----------------------------------------------------------------------------

#########################
###Simulate data
#########################
cov<-data.frame(id=1:numSamp,
                z1=c(rep(1,numSamp/2),rep(0,numSamp/2)),
                z2=c(rnorm(numSamp/2,1,0.5),rnorm(numSamp/2,0,0.5)),
                z3=c(rnorm(numSamp,1,0.5)))

dat <- simsurv(betas = coefficients,      # "true" parameter values
               x = cov,                   # covariate data for {numSamp} individuals
               knots = true_mod$knots,    # knot locations for splines
               logcumhazard = logcumhaz,  # definition of log(cumulative hazard) above
               maxt = 10000,              # want to prevent right censoring.
               interval = c(0,100000000)) # interval for root finding
colnames(dat)<-c("id","time","status")

#Introduce random sampling
samp<-sample(seq(1,nrow(dat),by=1), floor(nrow(dat)*(0.1))) #random censoring
dat$status[samp]<-0

# Merge the simulated event times onto covariate data frame
data <- merge(cov, dat)
data<-data[,-1]


#split the data into training, validation and test sets.
spec = c(train = .70,validate=.15, test = .15)
g = sample(cut(
  seq(nrow(data)), 
  nrow(data)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(data, g)






## -----------------------------------------------------------------------------

#normalizing function for convenience
normalizer<-function(data,means,sds,maxTime){
  normalized<-as.data.frame(data)
  for (i in 1:ncol(data)){
    if(length(unique(data[,i]))>1){
      normalized[,i]<-(data[,i]-means[i])/sds[i]
    }
  }
  normalized$status<-data$status
  
  normalized$time<-data$time/maxTime
  return(normalized)
}



train<-res$train
val<-res$validate
fullTest<-res$test
#get means, standard deviations (maxtime is kept for plotting results later)
sds<-sapply(train, function(x) (sd(x)))
means<-sapply(train, function(x) (mean(x)))
maxTime<-max(train$time)
timeVar<-'time'
statusVar<-'status'

train<-normalizer(train,means,sds,maxTime)
val<-normalizer(res$validate,means,sds,maxTime)
fullTest<-normalizer(fullTest,means,sds,maxTime)
test<-fullTest[,!(colnames(fullTest) %in% statusVar)] #remove status column, only include features, 



#for the linear models (casebase and Cox), we train on all of the training and validation data.
linModelTrain<-as.data.frame(rbind(train,val))

# we prepare the times of interest
times<-seq(from=min(test$time),
           to=max(test$time),
           length.out = 100
)

times<-head(times, -1)
times<-tail(times, -1)

## -----------------------------------------------------------------------------

# If you want to generate your own matrices for comparison, simply add the tunnel class to a sample(rows) by time (columns) cumulative incidence matrix
# i.e class(cumulativeIncidenceMatrix)<-c("tunnel",class(cumulativeIncidenceMatrix))
predictRisk.tunnel <- function(object, newdata, times, cause, ...){
  class(object)<-class(object)[-1]
  return(object)
}



## -----------------------------------------------------------------------------

###########################
###casebase optimal model
###########################
final_mod_cb_glm <- fitSmoothHazard(status~bs(time)-time+z1+z2+z3+z1:time+ z2:z3,#+z1:z3
                                    data = linModelTrain,
                                    time = "time",
                                    event="status",
                                    ratio = 100
)

summary(final_mod_cb_glm)
tglmAbsRisk<-absoluteRisk(final_mod_cb_glm,time=times,newdata=test,type="CI")
tglmAbsRisk<-as.data.frame(tglmAbsRisk)
rownames(tglmAbsRisk)<-tglmAbsRisk$time
trueGlmProper<- t(tglmAbsRisk[-1,-1])
class(trueGlmProper)<-c("tunnel",class(trueGlmProper))



## -----------------------------------------------------------------------------
#########################
###casebase+splines
##########################
mod_cb_glm <- fitSmoothHazard(status~bs(time)+.-time,
                              data = linModelTrain,
                              time = "time",
                              event="status",
                              ratio = 100
)

glmAbsRisk<-as.data.frame(absoluteRisk(mod_cb_glm,time=times,newdata=test,type="CI"))
rownames(glmAbsRisk)<-glmAbsRisk$time
glmProper<- t(glmAbsRisk[-1,-1])
class(glmProper)<-c("tunnel",class(glmProper))


## -----------------------------------------------------------------------------
############################
###coxph
############################
cox<-coxph(Surv(time, status) ~ ., data = linModelTrain,x=T)

## -----------------------------------------------------------------------------

#############################
###CBNN
#############################
traincb<-sampleCaseBase(data = as.data.frame(train),time="time",event="status",ratio=100)
valcb<-sampleCaseBase(data = as.data.frame(val),time="time",event="status",ratio=100)

  valcb<-list(list(as.matrix(valcb[,-c(ncol(valcb)-1,ncol(valcb))]),
                 as.matrix(valcb[,ncol(valcb),drop=F])),
            as.matrix(valcb[,ncol(valcb)-1,drop=F]))
  
  valcb[[1]][[2]]<-rep(traincb$offset[1],nrow(valcb[[1]][[1]]))   


## -----------------------------------------------------------------------------

covars_input<-keras::layer_input(shape=ncol(train)-1,
                                 name = 'main_input')

covars_output<-covars_input%>%
  layer_dense(units=layer1,use_bias = T,activation = actv)%>%
  layer_dropout(drpt)%>%
  layer_dense(units=layer2,use_bias = T,activation = actv)%>%
  layer_dropout(drpt)%>%
  layer_dense(units=1,use_bias = T)
cbnnPrepared<-prepCbnn(features=colnames(train)[-ncol(train)],
               nnInput = covars_input,
               nnOutput = covars_output,
               data = traincb,
               offset=traincb$offset,
               timeVar = "time",
               eventVar= "status",
               compRisk=FALSE,
               kOptim=optimizer_adam(learning_rate = lr,  weight_decay=10^-7)
)


## ----message=FALSE------------------------------------------------------------

callBacks<-keras::callback_early_stopping(
  monitor = "val_loss",
  min_delta = min_delta,
  patience = patience,
  verbose = 0,
  mode = c("auto"),
  baseline =NULL,
  restore_best_weights = T
)


fitCBNN<-fitHazard(cbnnPrepared,
               epochs=epo,
               batchSize=bsize,
               earlyStoppingCallbacks = callBacks,
               valData=valcb)


## ----message=FALSE------------------------------------------------------------

cbnnPredictionsCI<-cuIncCbnn(fitCBNN,
              times=times,
              x_test=test
)


rownames(cbnnPredictionsCI)<-cbnnPredictionsCI[,1]
cbnnPredictionsCI<- t(cbnnPredictionsCI[,-1])
class(cbnnPredictionsCI)<-c("tunnel",class(cbnnPredictionsCI))


## -----------------------------------------------------------------------------
############################
### Brier Score
############################  
brierFinalResults <- Score(list("Cox_Lin" = cox,'CB_Logi'=glmProper,
                                'CBNN_Poly'=cbnnPredictionsCI,'Optimal'=trueGlmProper),
                           data =fullTest, 
                           formula = Hist(time, status != 0) ~ 1, summary = c("risks","IPA","ibs"), 
                           se.fit = FALSE, metrics = c("auc", "brier"), contrasts = FALSE, times = times)
#brierFinalResults$Brier$score contains the brier score derived metrics like IPA.
#brierFinalResults$AUC$score contains the AUC metric.


## ----fig.width=8--------------------------------------------------------------

ggplot(data=brierFinalResults$Brier$score, aes(x=times*maxTime,y=IPA,col=model))+geom_line()
ggplot(data=brierFinalResults$AUC$score, aes(x=times*maxTime,y=AUC,col=model))+geom_line()



