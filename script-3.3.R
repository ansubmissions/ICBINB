source("./configuration.R")

save(true.clust, file=paste0("./results-script-3.3/true.clust.RData"))
save(s, file=paste0("./results-script-3.3/s.RData"))
save(lst, file=paste0("./results-script-3.3/lst.RData"))
save(z0, file=paste0("./results-script-3.3/z0.RData"))
save(zl.list, file=paste0("./results-script-3.3/zl.list.RData"))

# Running DGMM

iteration = 1
while(iteration <= 20){

    deepMFA.res.init.random <- deepMFA::deep.mfa(z0, k, r, num.layers, nb.iter, init="random", true.clust=true.clust)
    save(deepMFA.res.init.random, file=paste0("./results-script-3.3/deepMFA.res.init.random-",iteration,".RData"))

    iteration <- iteration+1
  
} # end while




#### Getting the results

# ARI

for(iteration in 1:20){

  load(paste0("./results-script-1-3.3/true.clust.RData"))
  load(paste0("./results-script-1-3.3/s.RData"))
  

  load(paste0("./results-script-1-3.3/deepMFA.res.init.random-",iteration,".RData"))
  ari <- mclust::adjustedRandIndex(true.clust, max.col(deepMFA.res.init.random$ps.y))
  ari.init.random <- c(ari.init.random,ari)

}


par(mfrow=c(1,1))
boxplot(
    data.frame(
      "Random"=ari.init.random
  ),
  xlab='random initialisation',  
  ylab='ARI',
  ylim=c(0,1)
)

# BIC

bic.init.random <- c()

for(iteration in 1:20){

  load(paste0("./results-script-3.3/true.clust.RData"))
  load(paste0("./results-script-3.3/s.RData"))
  

  load(paste0("./results-script-3.3/deepMFA.res.init.random-",iteration,".RData"))
  bic <- deepMFA.res.init.random$bic
  bic.init.random <- c(bic.init.random,bic)
 

}


# get BIC values over iterations DGMM

for(idx in 1:20){
  load(paste0("./results-script-1-hard-2/deepMFA.res.init.random-",idx,".RData"))
  plot(deepMFA.res.init.random$bics, xlab="Iterations", ylab="BIC value")
  Sys.sleep(1.5)
}


