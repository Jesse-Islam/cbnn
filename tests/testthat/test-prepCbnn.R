

data<-casebase::ERSPC
eventVar<-"DeadOfPrCa"
timeVar<-"Follow.Up.Time"
features<-"ScrArm"

preSampled<-sampleCaseBase(
  data,
  timeVar,
  eventVar,
  ratio = 10,
  comprisk = FALSE
)
nnInput<-keras::layer_input(shape=length(features))

nnOutput<-nnInput %>% keras::layer_dense(units=1,
                                         use_bias = TRUE)

test_that("prepCBNN", {
  expect_error(prepCbnn(features, nnInput, nnOutput, data, offset=NA,
                        timeVar,eventVar, ratio=100, compRisk=FALSE),NA)
  expect_error(prepCbnn(features, nnInput, nnOutput, preSampled,offset=NA,
                        timeVar,eventVar, ratio=100, compRisk=FALSE),NA)
})
#> cbnnPrepped 🎊

