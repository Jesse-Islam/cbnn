data<-casebase::ERSPC
data$ScrArm<-as.numeric(data$ScrArm)-1
eventVar<-"DeadOfPrCa"
timeVar<-"Follow.Up.Time"
features<-"ScrArm"
nnInput<-keras::layer_input(shape=length(features))
nnOutput<-nnInput %>% keras::layer_dense(units=1, use_bias = TRUE)
cbnnPrep<-prepCbnn(features, nnInput, nnOutput, data, offset=NA,timeVar,
                   eventVar, ratio=100, compRisk=FALSE)




test_that("try fitting...", {
  expect_error(fit<-fitHazard(cbnnPrep,epochs=10,batchSize=500),NA)
})
#> canFit 🌈
