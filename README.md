# cbnn
R package for case-base neural networks.

This package is based on keras. As its R implementation has python-based dependencies, its important to follow https://tensorflow.rstudio.com/install/ and correctly install keras. Then, install the cbnn package 


```
#follow https://tensorflow.rstudio.com/install/
#install.packages('devtools')
devtools::install_github("Jesse-Islam/cbnn")
```

You can then run the following code to make sure everything installed correctly. If there are any problems, feel free to contact me on this repository and open an issue.



```
#load packages
library(cbnn)
library(casebase)
library(magrittr)

#process data
data<-casebase::ERSPC
data$ScrArm<-as.numeric(data$ScrArm)-1
eventVar<-"DeadOfPrCa"
timeVar<-"Follow.Up.Time"
features<-"ScrArm"

#prepare CBNN model for fitting
nnInput<-keras::layer_input(shape=length(features))
nnOutput<-nnInput %>% keras::layer_dense(units=1, use_bias = TRUE)
cbnnPrep<-prepCbnn(features, nnInput, nnOutput, data, offset=NA,timeVar,
eventVar, ratio=10, compRisk=FALSE)

#Fit model
fit<-fitHazard(cbnnPrep,epochs=1,batchSize=500)

#calculate cumulative incidence curves
times<-seq(min(data$Follow.Up.Time),max(data$Follow.Up.Time),length.out=10)
x_test<-as.matrix(data[sample(nrow(data),100),])
cumulativeIncidenceCurves<-cuIncCbnn(fit, times=times,x_test=x_test)
```


I recommend going through the vignettes next, as it uses the complex simulation from the manuscript ( https://github.com/Jesse-Islam/cbnnManuscript ) as an example to better understand the implementation of CBNN on tabular data. For convenience, here is a link to the Time-varying-interactions-and-flexible-baseline-hazard vignette: https://github.com/Jesse-Islam/cbnn/tree/main/doc/Time-varying-interactions-and-flexible-baseline-hazard.html


If you're interested in reproducing the results from the paper, the exact scripts used to generate the figures are available in https://github.com/Jesse-Islam/cbnnManuscript/tree/main/analyses.

Please cite these papers if you use this method in your work:

```
   Islam, J., Turgeon, M., Sladek, R., & Bhatnagar, S. (2023).
   Case-Base Neural Networks: survival analysis with time-varying,
   higher-order interactions. arXiv preprint arXiv:2301.06535.
   
   Bhatnagar S, Turgeon M, Islam J, Saarela O, Hanley J (2020).
   _casebase: Fitting Flexible Smooth-in-Time Hazards and Risk Functions
   via Logistic and Multinomial Regression_. R package version 0.9.0,
   <https://CRAN.R-project.org/package=casebase>.
   
   Hanley, James A., and Olli S. Miettinen. Fitting smooth-in-time
   prognostic risk functions via logistic regression. International
   Journal of Biostatistics 5.1 (2009): 1125-1125.
   
   Saarela, Olli. A case-base sampling method for estimating recurrent
   event intensities. Lifetime data analysis 22.4 (2016): 589-605.
``` 
