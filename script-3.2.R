source("./configuration-2.R")

save(true.clust, file=paste0("./results-script-3.2/true.clust.RData"))
save(s, file=paste0("./results-script-3.2/s.RData"))
save(lst, file=paste0("./results-script-3.2/lst.RData"))
save(z0, file=paste0("./results-script-3.2/z0.RData"))
save(zl.list, file=paste0("./results-script-3.2/zl.list.RData"))


iteration = 1
while(iteration <= 20){

    for(iter.k1 in 2:6){
      new.k <- c(iter.k1,k[-1])
      deepMFA.res.init.random <- deepMFA::deep.mfa(z0, new.k, r, num.layers, nb.iter, init="random", true.clust=true.clust)
     save(deepMFA.res.init.random, file=paste0("./results-script-3.2/deepMFA.res.init.random-",
       iteration,
        "-",iter.k1,
       ".RData"))
    }
    
    library(mclust)
    mclust.options(hcUse = "SVD")
    mclust.res.first <- Mclust(z0, G=2:6, verbose=FALSE, model="VVV")
    save(mclust.res.first, file=paste0("./results-script-3.2/mclust.res.first-",iteration,".RData"))


    iteration <- iteration+1
  
} # end while


### Getting the results:

# BIC

bic.init.random.2 <- c()
bic.init.random.3 <- c()
bic.init.random.4 <- c()
bic.init.random.5 <- c()
bic.init.random.6 <- c()
bic.mclust.first <- c()


bics.dgmm <- matrix(0, nrow=5, ncol=20)
idx <- 0
for(iteration in 1:20){
  idx <- idx +1
  load(paste0("./results-script-3.2/true.clust.RData"))
  load(paste0("./results-script-3.2/s.RData"))
  
  iter.k1 = 2
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    bics.dgmm[1,idx] <- bic <-  deepMFA.res.init.random$bic
    bic.init.random.2 <- c(bic.init.random.2,bic)
  iter.k1 = 3
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    bics.dgmm[2,idx] <- bic <-  deepMFA.res.init.random$bic
    bic.init.random.3 <- c(bic.init.random.3,bic)
  iter.k1 = 4
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    bics.dgmm[3,idx] <- bic <-  deepMFA.res.init.random$bic
    bic.init.random.4 <- c(bic.init.random.4,bic)
  iter.k1 = 5
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    bics.dgmm[4,idx] <- bic <-  deepMFA.res.init.random$bic
    bic.init.random.5 <- c(bic.init.random.5,bic)
  iter.k1 = 6
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    bics.dgmm[5,idx] <- bic <-  deepMFA.res.init.random$bic
    bic.init.random.6 <- c(bic.init.random.6,bic)
  


  load(paste0("./results-script-3.2/mclust.res.first-",iteration,".RData"))
  bic <- -mclust.res.first$bic
  bic.mclust.first <- c(bic.mclust.first,bic)

}

# ARI

ari.init.random.2 <- c()
ari.init.random.3 <- c()
ari.init.random.4 <- c()
ari.init.random.5 <- c()
ari.init.random.6 <- c()
ari.mclust.first <- c()

aris.dgmm <- matrix(0, nrow=5, ncol=20)


for(iteration in 1:20){
  load(paste0("./results-script-3.2/true.clust.RData"))
  load(paste0("./results-script-3.2/s.RData"))
  
  iter.k1 = 2
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    aris.dgmm[1,iteration] <- ari <- mclust::adjustedRandIndex(s[,1], deepMFA.res.init.random$s[,1])
    ari.init.random.2 <- c(ari.init.random.2,ari)
  iter.k1 = 3
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    aris.dgmm[2,iteration] <- ari <- mclust::adjustedRandIndex(s[,1], deepMFA.res.init.random$s[,1])
    ari.init.random.3 <- c(ari.init.random.3,ari)
  iter.k1 = 4
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    aris.dgmm[3,iteration] <- ari <- mclust::adjustedRandIndex(s[,1], deepMFA.res.init.random$s[,1])
    ari.init.random.4 <- c(ari.init.random.4,ari)
  iter.k1 = 5
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    aris.dgmm[4,iteration] <- ari <- mclust::adjustedRandIndex(s[,1], deepMFA.res.init.random$s[,1])
    ari.init.random.5 <- c(ari.init.random.5,ari)
  iter.k1 = 6
    load(paste0("./results-script-3.2/deepMFA.res.init.random-",
      iteration,"-",iter.k1,".RData"))
    aris.dgmm[5,iteration] <- ari <- mclust::adjustedRandIndex(s[,1], deepMFA.res.init.random$s[,1])
    ari.init.random.6 <- c(ari.init.random.6,ari)
  

  load(paste0("./results-script-3.2/mclust.res.first-",iteration,".RData"))
  ari <- mclust::adjustedRandIndex(s[,1], mclust.res.first$cl)
  ari.mclust.first <- c(ari.mclust.first,ari)

}



idxs <- apply(bics.dgmm,2,which.min)

aris <- c()
for(it in 1:20){
  aris <- c(aris,aris.dgmm[idxs[it],it])
}

par(mfrow=c(1,1))
boxplot(
    data.frame(
      "DGMM first layer"=aris,
      "GMM" = ari.mclust.first
    ),
  ylab="ARI",
  ylim=c(0,1)
)
