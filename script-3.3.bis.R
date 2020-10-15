source("./configuration-2.R")

save(true.clust, file=paste0("./results-script-3.3.bis/true.clust.RData"))
save(s, file=paste0("./results-script-3.3.bis/s.RData"))
save(lst, file=paste0("./results-script-3.3.bis/lst.RData"))
save(z0, file=paste0("./results-script-3.3.bis/z0.RData"))
save(zl.list, file=paste0("./results-script-3.3.bis/zl.list.RData"))


# 4 blocks of code that can be run separately after running code above:

# block 1
set.seed(1)
iteration = 1
while(iteration <= 25){

    deepMFA.res.init.random <- deepMFA::deep.mfa(z0, k, r, num.layers, nb.iter, init="random", true.clust=true.clust)
    save(deepMFA.res.init.random, file=paste0("./results-script-3.3.bis/deepMFA.res.init.random-",iteration,".RData"))

    iteration <- iteration+1
  
} # end while

# block 2
set.seed(2)
iteration = 26
while(iteration <= 50){

    deepMFA.res.init.random <- deepMFA::deep.mfa(z0, k, r, num.layers, nb.iter, init="random", true.clust=true.clust)
    save(deepMFA.res.init.random, file=paste0("./results-script-3.3.bis/deepMFA.res.init.random-",iteration,".RData"))

    iteration <- iteration+1
  
} # end while

# block 3
set.seed(3)
iteration = 51
while(iteration <= 75){

    deepMFA.res.init.random <- deepMFA::deep.mfa(z0, k, r, num.layers, nb.iter, init="random", true.clust=true.clust)
    save(deepMFA.res.init.random, file=paste0("./results-script-3.3.bis/deepMFA.res.init.random-",iteration,".RData"))

    iteration <- iteration+1
  
} # end while


# block 4
set.seed(4)
iteration = 76
while(iteration <= 100){

    deepMFA.res.init.random <- deepMFA::deep.mfa(z0, k, r, num.layers, nb.iter, init="random", true.clust=true.clust)
    save(deepMFA.res.init.random, file=paste0("./results-script-3.3.bis/deepMFA.res.init.random-",iteration,".RData"))

    iteration <- iteration+1
  
} # end while




#### Getting the results for the 100 mutual ARI:

num <- 100
aris.mat <- matrix(0, nrow=num,ncol=num)
diag(aris.mat)=1
for(iteration1 in 1:num){
  print(iteration1)
  load(paste0("./results-script-3.3.bis/deepMFA.res.init.random-",iteration1,".RData"))
  m <- max.col(deepMFA.res.init.random$ps.y)
  for(iteration2 in 1:iteration1){
    load(paste0("./results-script-3.3.bis/deepMFA.res.init.random-",iteration2,".RData"))
    ari <- mclust::adjustedRandIndex(m, max.col(deepMFA.res.init.random$ps.y))
    aris.mat[iteration1,iteration2] <- ari
    aris.mat[iteration2,iteration1] <- ari
  }
}

save(aris.mat, file="./results-script-3.3.bis/aris.mat.RData")



load("./results-script-3.3.bis/aris.mat.RData")
par(mfrow=c(1,2))
image(aris.mat, main='', col = gray(seq(1.0, 0.0, -0.08)))
hist(aris.mat, 10, main='',col = gray(seq(1.0, 0.0, -0.08)),xlab = "ARI values", xlim=c(0,1))




