

data<-casebase::ERSPC
eventVar<-"DeadOfPrCa"
timeVar<-"Follow.Up.Time"
features<-"ScrArm"
nnInput<-keras::layer_input(shape=length(features))

nnOutput<-nnInput %>% keras::layer_dense(units=1,
                                         use_bias = TRUE)

cbnnPrep<-prepCbnn(features, nnInput, nnOutput, data, offset=NA,
                   timeVar,eventVar, ratio=10, compRisk=FALSE)

test_that("try fitting...", {
  expect_error(cbnn::fitHazard(cbnnPrep,epochs=1,batchSize=500,verbose=0),NA)
})
#> canFit 🌈
