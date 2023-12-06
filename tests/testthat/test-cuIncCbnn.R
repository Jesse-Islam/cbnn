data<-casebase::ERSPC
data$ScrArm<-as.numeric(data$ScrArm)-1
eventVar<-"DeadOfPrCa"
timeVar<-"Follow.Up.Time"
features<-"ScrArm"
nnInput<-keras::layer_input(shape=length(features))
nnOutput<-nnInput %>% keras::layer_dense(units=1, use_bias = TRUE)
cbnnPrep<-prepCbnn(features, nnInput, nnOutput, data, offset=NA,timeVar,
                   eventVar, ratio=10, compRisk=FALSE)
fit<-fitHazard(cbnnPrep,epochs=1,batchSize=500)

test_that("try fitting...", {
  times<-seq(min(data$Follow.Up.Time),max(data$Follow.Up.Time),length.out=10)
  x_test<-as.matrix(data[sample(nrow(data),10),])
  expect_error(cumulativeIncidenceCurves<-cuIncCbnn(fit, times=times,x_test=x_test),NA)
})
#> canFit 🌈
