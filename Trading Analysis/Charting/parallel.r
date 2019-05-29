# Script to run source programs in parallel
# Script has to be called in the same folder as the other source files
# source("Z:/Strategy Interns/Strategy.Intern.3/R/ParallelR/Parallel/trendline_functions_falsebreakout.r")

run_parallel <-function(fileList){
  if(Sys.info()["sysname"]=="Windows"){
    library(doParallel)
    library(foreach)
    numberofCores <- max(1, detectCores()-1)
    cl<-makeCluster(numberofCores)
    registerDoParallel(cl)
  }else{
    library(doMC)
    registerDoMC(numberOfCores)
  }
  returnComputation <-
    foreach(x=fileList) %dopar%{
      source(x)
    }
  
  #returnComputation = data.frame(returnComputation)
  
  
  if(Sys.info()["sysname"]=="Windows") stopCluster(cl)
  
  return (returnComputation)
}

# Test files
fileList<-list("Trendline.R", 
               "Trendline para.R")


t1 = Sys.time()
output <- run_parallel(fileList)
t2 = Sys.time()
t2-t1

