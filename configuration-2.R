# This script is used to simulate data and try deepMFA 
  
  set.seed(1)


  k <- c(3,3,2) # num clusters per layers
  r <- c(11,6,2)
  num.obs <- 4000
  p <- 17 # (num.col)
  num.layers <- 3
  nb.iter <- 200
  

  eta <- vector("list", length = 2)
  Lambda <- vector("list", length = 2)
  psi <- vector("list", length = 2)
 
  eta[[1]] <- matrix(0, nrow = k[1], ncol=p)
  eta[[1]][1,] <- matrix(rnorm(p, mean=0, sd=20), nrow = 1)
  eta[[1]][2,] <- matrix(rnorm(p, mean=0, sd=20), nrow = 1)
  eta[[1]][3,] <- matrix(rnorm(p, mean=0, sd=20), nrow = 1)

  eta[[2]] <- matrix(0, nrow = k[2], ncol=r[1])
  eta[[2]][1,] <- matrix(rnorm(r[1], mean=0, sd=10), nrow = 1)
  eta[[2]][2,] <- matrix(rnorm(r[1], mean=0, sd=10), nrow = 1)
  eta[[2]][3,] <- matrix(rnorm(r[1], mean=0, sd=10), nrow = 1)

  eta[[3]] <- matrix(0, nrow = k[3], ncol=r[2])
  eta[[3]][1,] <- matrix(rnorm(r[2], mean=0, sd=10), nrow = 1)
  eta[[3]][2,] <- matrix(rnorm(r[2], mean=0, sd=10), nrow = 1)


  psi[[1]] <- matrix(0, nrow = k[1], ncol=p)
  psi[[1]][1,] <- matrix(abs(rnorm(p, mean=30, sd=1)), nrow = 1)
  psi[[1]][2,] <- matrix(abs(rnorm(p, mean=3, sd=5)), nrow = 1)
  psi[[1]][3,] <- matrix(abs(rnorm(p, mean=2, sd=3)), nrow = 1)

  psi[[2]] <- matrix(0, nrow = k[2], ncol=r[1])
  psi[[2]][1,] <- matrix(abs(rnorm(r[1], mean=3, sd=2)), nrow = 1)
  psi[[2]][2,] <- matrix(abs(rnorm(r[1], mean=5, sd=2)), nrow = 1)
  psi[[2]][3,] <- matrix(abs(rnorm(r[1], mean=10, sd=2)), nrow = 1)

  psi[[3]] <- matrix(0, nrow = k[3], ncol=r[2])
  psi[[3]][1,] <- matrix(abs(rnorm(r[2], mean=3, sd=2)), nrow = 1)
  psi[[3]][2,] <- matrix(abs(rnorm(r[2], mean=5, sd=2)), nrow = 1)


  Lambda[[1]] <- array(0, dim=c(k[1],p,r[1]))
  Lambda[[1]][1,,] <- matrix(rnorm(p*r[1], mean=1, sd=15), nrow = p)
  Lambda[[1]][2,,] <- matrix(rnorm(p*r[1], mean=-1, sd=15), nrow = p)
  Lambda[[1]][3,,] <- matrix(rnorm(p*r[1], mean=1, sd=15), nrow = p)
  nb.zeros <- ceiling(0.2*length(Lambda[[1]]))
  sample.zeros <- sample(1:length(Lambda[[1]]), nb.zeros)
  Lambda[[1]][sample.zeros] <- 0
  

  Lambda[[2]] <- array(0,  dim=c(k[2],r[1],r[2]))
  Lambda[[2]][1,,] <- matrix(rnorm(r[1]*r[2], mean=2, sd=2), nrow = r[1])
  Lambda[[2]][2,,] <- matrix(rnorm(r[1]*r[2], mean=0, sd=1), nrow = r[1])
  Lambda[[2]][3,,] <- matrix(rnorm(r[1]*r[2], mean=3, sd=2), nrow = r[1])
  nb.zeros <- ceiling(0.5*length(Lambda[[2]]))
  sample.zeros <- sample(1:length(Lambda[[2]]), nb.zeros)
  Lambda[[2]][sample.zeros] <- 0

  Lambda[[3]] <- array(0,  dim=c(k[3],r[2],r[3]))
  Lambda[[3]][1,,] <- matrix(rnorm(r[2]*r[3], mean=-10, sd=2), nrow = r[2])
  Lambda[[3]][2,,] <- matrix(rnorm(r[2]*r[3], mean=10, sd=1), nrow = r[2])
  nb.zeros <- ceiling(0.2*length(Lambda[[3]]))
  sample.zeros <- sample(1:length(Lambda[[3]]), nb.zeros)
  Lambda[[3]][sample.zeros] <- 0


 
  z3 = mvtnorm::rmvnorm(n = num.obs, mean=rep(0,r[3]), sigma = diag(r[3]))

  # layers3
  sample.layers3 <- sample(1:k[3], num.obs, replace=T)
  idx.layers3.clust1 <- which(sample.layers3==1)
  idx.layers3.clust2 <- which(sample.layers3==2)
  u3.1 = mvtnorm::rmvnorm(n = length(idx.layers3.clust1), 
                          mean=rep(0,r[2]), sigma = diag(psi[[3]][1,]))
  u3.2 = mvtnorm::rmvnorm(n = length(idx.layers3.clust2), 
                          mean=rep(0,r[2]), sigma = diag(psi[[3]][2,]))


  z2 <- matrix(0, nrow= num.obs, ncol=r[2])


  z2.1 = matrix(eta[[3]][1,], nrow=length(idx.layers3.clust1),
                              ncol=r[2], byrow= T) + 
            t(Lambda[[3]][1,,]%*%t(z3[idx.layers3.clust1,])) + u3.1
  z2.2 = matrix(eta[[3]][2,], nrow=length(idx.layers3.clust2),
                              ncol=r[2], byrow= T) + 
            t(Lambda[[3]][2,,]%*%t(z3[idx.layers3.clust2,])) + u3.2



  z2[idx.layers3.clust1,] <- z2.1
  z2[idx.layers3.clust2,] <- z2.2


  # layers2
  sample.layers2 <- sample(1:k[2], num.obs, replace=T)
  idx.layers2.clust1 <- which(sample.layers2==1)
  idx.layers2.clust2 <- which(sample.layers2==2)
  idx.layers2.clust3 <- which(sample.layers2==3)
  u2.1 = mvtnorm::rmvnorm(n = length(idx.layers2.clust1), 
                          mean=rep(0,r[1]), sigma = diag(psi[[2]][1,]))
  u2.2 = mvtnorm::rmvnorm(n = length(idx.layers2.clust2), 
                          mean=rep(0,r[1]), sigma = diag(psi[[2]][2,]))
  u2.3 = mvtnorm::rmvnorm(n = length(idx.layers2.clust3), 
                          mean=rep(0,r[1]), sigma = diag(psi[[2]][3,]))
 


  z1 <- matrix(0, nrow= num.obs, ncol=r[1])


  z1.1 = matrix(eta[[2]][1,], nrow=length(idx.layers2.clust1),
                              ncol=r[1], byrow= T) + 
            t(Lambda[[2]][1,,]%*%t(z2[idx.layers2.clust1,])) + u2.1
  z1.2 = matrix(eta[[2]][2,], nrow=length(idx.layers2.clust2),
                              ncol=r[1], byrow= T) + 
            t(Lambda[[2]][2,,]%*%t(z2[idx.layers2.clust2,])) + u2.2
  z1.3 = matrix(eta[[2]][3,], nrow=length(idx.layers2.clust3), 
                              ncol=r[1], byrow= T) + 
            t(Lambda[[2]][3,,]%*%t(z2[idx.layers2.clust3,])) + u2.3


  z1[idx.layers2.clust1,] <- z1.1
  z1[idx.layers2.clust2,] <- z1.2
  z1[idx.layers2.clust3,] <- z1.3




  # layers1
  sample.layers1 <- sample(1:k[1], num.obs, replace=T)
  idx.layers1.clust1 <- which(sample.layers1==1)
  idx.layers1.clust2 <- which(sample.layers1==2)
  idx.layers1.clust3 <- which(sample.layers1==3)
  u1.1 = mvtnorm::rmvnorm(n = length(idx.layers1.clust1), 
                          mean=rep(0,p), sigma = diag(psi[[1]][1,]))
  u1.2 = mvtnorm::rmvnorm(n = length(idx.layers1.clust2), 
                          mean=rep(0,p), sigma = diag(psi[[1]][2,]))
  u1.3 = mvtnorm::rmvnorm(n = length(idx.layers1.clust3), 
                          mean=rep(0,p), sigma = diag(psi[[1]][3,]))


  z0 <- matrix(0, nrow= num.obs, ncol=p)


  z0.1 = matrix(eta[[1]][1,], nrow=length(idx.layers1.clust1),
                ncol=p, byrow= T) + 
    t(Lambda[[1]][1,,]%*%t(z1[idx.layers1.clust1,])) + u1.1
  z0.2 = matrix(eta[[1]][2,], nrow=length(idx.layers1.clust2),
                ncol=p, byrow= T) + 
    t(Lambda[[1]][2,,]%*%t(z1[idx.layers1.clust2,])) + u1.2
  z0.3 = matrix(eta[[1]][3,], nrow=length(idx.layers1.clust3), 
                ncol=p, byrow= T) + 
    t(Lambda[[1]][3,,]%*%t(z1[idx.layers1.clust3,])) + u1.3


  z0[idx.layers1.clust1,] <- z0.1
  z0[idx.layers1.clust2,] <- z0.2
  z0[idx.layers1.clust3,] <- z0.3

  ################ FIND TRUE CLUSTS AND PARAMS ####################

  k.comb <- apply(t(k), 2, function(x) 1 : x)
  if (is.list(k.comb)) {
    k.comb <- expand.grid(k.comb)
  }
  if (is.matrix(k.comb)) {
    k.comb <- expand.grid(split(t(k.comb), 1 : ncol(k.comb)))
  }
  if (prod(k) == 1) {
    k.comb <- matrix(k.comb, nrow =1)
  }
  s <- matrix(0, nrow = num.obs, ncol=num.layers)
  s[idx.layers1.clust1, 1] <- 1
  s[idx.layers1.clust2, 1] <- 2
  s[idx.layers1.clust3, 1] <- 3
  s[idx.layers2.clust1, 2] <- 1
  s[idx.layers2.clust2, 2] <- 2
  s[idx.layers2.clust3, 2] <- 3
  s[idx.layers3.clust1, 3] <- 1
  s[idx.layers3.clust2, 3] <- 2

  true.clust <- matrix(0, nrow= num.obs, ncol=1)
  for(i in 1:num.obs){
    mat <- matrix(0, nrow= num.obs, ncol=nrow(k.comb))
    
    mat[i,] = apply(k.comb, 1, function(x){
        all(x==s[i,])
      })
    true.clust[i,] <- which(1==mat[i,])
    
  }

  R <- c(p,r)
  psi.inv <- list()
  pis <- list()
  psis <- list()
  for (l in 1 : num.layers) {
    pis[[l]] <- rep(1/k[l], k[l])
    psi.inv[[l]] <- array(0, dim=c(k[l],R[l],R[l]))
    psis[[l]] <- array(0, dim=c(k[l],R[l],R[l]))
    eta[[l]] <- t(eta[[l]])
    for(ki in 1:k[l]){
      psi.inv[[l]][ki,,] <- diag(1/psi[[l]][ki,])
      psis[[l]][ki,,] <- diag(psi[[l]][ki,])
    }
  }


  lst <- list(pis = list(), Lambda = list(), eta = list(), psi = list(), psi.inv = list())
    
    for (l in 1 : num.layers) {
      
      lst$pis[l] <- list(pis[[l]])
      lst$Lambda[l] <- list(Lambda[[l]])
      lst$eta[l] <- list(eta[[l]])
      lst$psi[l] <- list(psis[[l]])
      lst$psi.inv[l] <- list(psi.inv[[l]])
  }
  lst1 <- lst



  lst2 <- list(pis = list(), Lambda = list(), eta = list(), psi = list(), psi.inv = list())
    
  for (l in 1 : num.layers) {
      
      lst2$pis[l] <- list(pis[[l]])
      lst2$Lambda[l] <- list(Lambda[[l]] + rnorm(length(Lambda[[l]]),0,0.5) )
      lst2$eta[l] <- list(eta[[l]] + rnorm(length(eta[[l]]),0,0.5) )
      for(ki in 1:k[l]){
        psis[[l]][ki,,] <-  psis[[l]][ki,,] + diag( rnorm(dim(psis[[l]][ki,,])[1],0,0.5) )
        psi.inv[[l]][ki,,] <-  psi.inv[[l]][ki,,] + diag( rnorm(dim(psi.inv[[l]][ki,,])[1],0,0.5) )
      }
      lst2$psi[l] <- list(psis[[l]])
      lst2$psi.inv[l] <- list(psi.inv[[l]])
      
  }

  lst3 <- list(pis = list(), Lambda = list(), eta = list(), psi = list(), psi.inv = list())
    
  for (l in 1 : num.layers) {
      
      lst3$pis[l] <- list(pis[[l]])
      lst3$Lambda[l] <- list(Lambda[[l]] + rnorm(length(Lambda[[l]]),0,1) )
      lst3$eta[l] <- list(eta[[l]] + rnorm(length(eta[[l]]),0,1) )
      for(ki in 1:k[l]){
        psis[[l]][ki,,] <-  psis[[l]][ki,,] + diag( rnorm(dim(psis[[l]][ki,,])[1],0,1) )
        psi.inv[[l]][ki,,] <-  psi.inv[[l]][ki,,] + diag( rnorm(dim(psi.inv[[l]][ki,,])[1],0,1) )
      }
      lst3$psi[l] <- list(psis[[l]])
      lst3$psi.inv[l] <- list(psi.inv[[l]])
      
  }

  lst4 <- list(pis = list(), Lambda = list(), eta = list(), psi = list(), psi.inv = list())
    
  for (l in 1 : num.layers) {
      
      lst4$pis[l] <- list(pis[[l]])
      lst4$Lambda[l] <- list(Lambda[[l]] + rnorm(length(Lambda[[l]]),0,2.5) )
      lst4$eta[l] <- list(eta[[l]] + rnorm(length(eta[[l]]),0,2.5) )
      for(ki in 1:k[l]){
        psis[[l]][ki,,] <-  psis[[l]][ki,,] + diag( rnorm(dim(psis[[l]][ki,,])[1],0,2.5) )
        psi.inv[[l]][ki,,] <-  psi.inv[[l]][ki,,] + diag( rnorm(dim(psi.inv[[l]][ki,,])[1],0,2.5) )
      }
      lst4$psi[l] <- list(psis[[l]])
      lst4$psi.inv[l] <- list(psi.inv[[l]])
      
  }



zl.list <- list()
zl.list[[1]] <- z1
zl.list[[2]] <- z2
zl.list[[3]] <- z3
