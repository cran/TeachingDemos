"run.power.examp" <-
function(){
  slider( power.refresh,
         c('Sample Size','Standard Deviation','True Difference',
           'Alpha level'),
         c(1,0.25,-1,0.01),
         c(100,5,3,0.99),
         c(1,0.25,0.1,0.01),
         c(1,1,1,0.05),
         title="Power Demo") }

