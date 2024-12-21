library(MASS)

unconstrained.unpenalized <- function(Y, M, A, C = NULL){
  n <- nrow(M)
  p <- ncol(M)
  if(is.null(C)){
    k <- 0
    C <- matrix(1, nrow = n)
  } else {
    k <- ncol(C)
    C <- cbind(1,C)
  }
  X <- cbind(A,C)
  tX <- t(X)
  
  alpha.hat <- t(solve(tX%*%X)%*%tX%*%M)
  alpha.a.hat <- alpha.hat[,1]
  alpha.c.hat <- alpha.hat[,2:(k+2)]
  Sigma.m.hat <- matrix(0, nrow = p, ncol = p)
  for(i in 1:n){
    Sigma.m.hat <- Sigma.m.hat + matrix(M[i,] - alpha.hat%*%X[i,], ncol = 1)%*%matrix(M[i,] - alpha.hat%*%X[i,], nrow = 1)
  }
  Sigma.m.hat <- Sigma.m.hat/n
  
  X <- cbind(X, M)
  tX <- t(X)
  beta.hat <- as.vector(solve(tX%*%X)%*%tX%*%Y)
  beta.a.hat <- beta.hat[1]
  beta.c.hat <- beta.hat[2:(k+2)]
  beta.m.hat <- beta.hat[(k+3):length(beta.hat)]
  sigma.e.sq.hat <- sum((Y - X%*%matrix(beta.hat, ncol = 1))^2)/n
  
  return(list("alpha.a.hat" = alpha.a.hat, "alpha.c.hat" = alpha.c.hat, "Sigma.m.hat" = Sigma.m.hat,
              "beta.a.hat" = beta.a.hat, "beta.c.hat" = beta.c.hat, "beta.m.hat" = beta.m.hat,
              "sigma.e.sq.hat" = sigma.e.sq.hat))
}

constrained.unpenalized <- function(Y, M, A, C = NULL, T.hat.external, err.tol.out = 1e-08, err.tol.med = 1e-08, max.itr = 10000){
  n <- nrow(M)
  p <- ncol(M)
  if(is.null(C)){
    k <- 0
    C <- matrix(1, nrow = n)
  } else {
    k <- ncol(C)
    C <- cbind(1,C)
  }
  X <- cbind(A,C)
  tX <- t(X)
  
  tC <- t(C)
  CtC.inv <- solve(tC%*%C)
  CtC.inv.tC <- CtC.inv%*%tC
  
  sum.sq.exp <- sum(A^2)
  
  #Calculate Initial Values Using Unconstrained Method
  alpha.hat.init <- t(solve(tX%*%X)%*%tX%*%M)
  alpha.a.hat.init <- alpha.hat.init[,1]
  alpha.c.hat.init <- alpha.hat.init[,2:(k+2)]
  Sigma.m.hat.init <- matrix(0, nrow = p, ncol = p)
  for(i in 1:n){
    Sigma.m.hat.init <- Sigma.m.hat.init + matrix(M[i,] - alpha.hat.init%*%X[i,], ncol = 1)%*%matrix(M[i,] - alpha.hat.init%*%X[i,], nrow = 1)
  }
  Sigma.m.hat.init <- Sigma.m.hat.init/n
  
  X <- cbind(X, M)
  tX <- t(X)
  beta.hat.init <- as.vector(solve(tX%*%X)%*%tX%*%Y)
  beta.a.hat.init <- beta.hat.init[1]
  beta.c.hat.init <- beta.hat.init[2:(k+2)]
  beta.m.hat.init <- beta.hat.init[(k+3):length(beta.hat.init)]
  sigma.e.sq.hat.init <- sum((Y - X%*%matrix(beta.hat.init, ncol = 1))^2)/n
  
  #Constrained coordinate descent algorithm
  beta.m.prev <- beta.m.hat.init
  beta.c.prev <- beta.c.hat.init
  alpha.c.prev <- alpha.c.hat.init
  alpha.a.prev <- alpha.a.hat.init
  sigma.e.sq.prev <- sigma.e.sq.hat.init
  Sigma.m.prev <- Sigma.m.hat.init
  beta.m.new <- rep(NA,p)
  beta.c.new <- rep(NA,k+1)
  alpha.c.new <- matrix(NA, nrow = p, ncol = k+1)
  alpha.a.new <- rep(NA,p)
  sigma.e.sq.new <- NA
  Sigma.m.new <- matrix(NA, nrow = p, ncol = p)
  delta.alpha <- 1
  delta.Sigma.m <- 1
  delta.beta <- 1
  delta.sigma.e.sq <- 1
  cnt <- 1
  while (((((delta.alpha > err.tol.med) | (delta.Sigma.m > err.tol.med)) | (delta.beta > err.tol.out)) | (delta.sigma.e.sq > err.tol.out)) & (cnt <= max.itr)) {
    alpha.c.new <- t(CtC.inv.tC%*%(M - matrix(A, ncol = 1)%*%matrix(alpha.a.prev, nrow = 1)))
    alpha.tmp.vec <- rep(0, p)
    for(i in 1:n){
      alpha.tmp.vec <- alpha.tmp.vec + A[i]*(solve(Sigma.m.prev)%*%matrix(M[i,] - as.vector(alpha.c.new%*%matrix(C[i,], ncol = 1)), ncol = 1) - (1/sigma.e.sq.prev)*(Y[i] - T.hat.external*A[i] - sum(M[i,]*beta.m.prev) - sum(C[i,]*beta.c.prev))*matrix(beta.m.prev, ncol = 1))
    }
    alpha.a.new <- as.vector((1/sum.sq.exp)*solve(solve(Sigma.m.prev) + (1/sigma.e.sq.prev)*matrix(beta.m.prev, ncol = 1)%*%matrix(beta.m.prev, nrow = 1))%*%matrix(alpha.tmp.vec, ncol = 1))
    Sigma.m.tmp <- matrix(0, nrow = p, ncol = p)
    for(i in 1:n){
      Sigma.m.tmp <- Sigma.m.tmp + matrix(M[i,] - A[i]*alpha.a.new - as.vector(alpha.c.new%*%matrix(C[i,], ncol = 1)), ncol = 1)%*%matrix(M[i,] - A[i]*alpha.a.new - as.vector(alpha.c.new%*%matrix(C[i,], ncol = 1)), nrow = 1)
    }
    Sigma.m.new <- Sigma.m.tmp/n
    
    beta.c.new <- as.vector(CtC.inv.tC%*%(Y - (T.hat.external - sum(alpha.a.new*beta.m.prev))*A - M%*%matrix(beta.m.prev, ncol = 1)))
    beta.tmp.vec <- rep(0, p)
    resid.med <- matrix(0, nrow = p, ncol = p)
    cor.mod <- rep(0, p)
    for(i in 1:n){
      resid.med <- resid.med + matrix(M[i,] - A[i]*alpha.a.new, ncol = 1)%*%matrix(M[i,] - A[i]*alpha.a.new, nrow = 1)
      cor.mod <- cor.mod + (Y[i] - A[i]*T.hat.external - sum(C[i,]*beta.c.new))*matrix(M[i,] - A[i]*alpha.a.new, ncol = 1)
    }
    beta.m.new <- as.vector(solve(resid.med)%*%cor.mod)
    sigma.e.sq.new <- sum((Y - (T.hat.external - sum(alpha.a.new*beta.m.new))*A - M%*%matrix(beta.m.new, ncol = 1) - C%*%matrix(beta.c.new, ncol = 1))^2)/n
    
    #Check convergence (average error tolerance per regression coefficient)
    delta.beta <- (sum((beta.m.new - beta.m.prev)^2) + sum((beta.c.new - beta.c.prev)^2))/(p+k+1)
    delta.alpha <- (sum((alpha.a.new - alpha.a.prev)^2) + sum((alpha.c.new - alpha.c.prev)^2))/(p*(k+1)+p)
    delta.sigma.e.sq <- abs(sigma.e.sq.new - sigma.e.sq.prev)
    delta.Sigma.m <- sum(abs(Sigma.m.new - Sigma.m.prev))/p^2
    
    #Update
    beta.m.prev <- beta.m.new
    beta.c.prev <- beta.c.new
    sigma.e.sq.prev <- sigma.e.sq.new
    alpha.c.prev <- alpha.c.new
    alpha.a.prev <- alpha.a.new
    Sigma.m.prev <- Sigma.m.new
    
    cnt <- cnt + 1
  }
  
  if(cnt > max.itr){
    converged <- 0
  } else {
    converged <- 1
  }
  
  return(list("alpha.a.hat" = alpha.a.new, "alpha.c.hat" = alpha.c.new, "Sigma.m.hat" = Sigma.m.new,
              "beta.m.hat" = beta.m.new, "beta.c.hat" = beta.c.new, "sigma.e.sq.hat" = sigma.e.sq.new,
              "converged" = converged))
}

rand.eff.coord.desc.unpenalized <- function(Y, M, A, C = NULL, first.moment, second.moment, err.tol.out = 1e-08, err.tol.med = 1e-08, max.itr = 10000){
  init.vals <- unconstrained.unpenalized(Y = Y, M = M, A = A, C = C)
  alpha.a.prev <- init.vals$alpha.a.hat
  alpha.c.prev <- init.vals$alpha.c.hat
  Sigma.m.prev <- init.vals$Sigma.m.hat
  Sigma.m.inv.prev <- solve(Sigma.m.prev)
  beta.c.prev <- init.vals$beta.c.hat
  beta.m.prev <- init.vals$beta.m.hat
  sigma.e.sq.prev <- init.vals$sigma.e.sq.hat
  
  n <- nrow(M)
  p <- ncol(M)
  if(is.null(C)){
    k <- 0
    C <- matrix(1, nrow = n)
  } else {
    k <- ncol(C)
    C <- cbind(1,C)
  }
  X <- cbind(A,C)
  tX <- t(X)
  
  AtA <- sum(A^2)
  CtC.inv.tC <- solve(t(C)%*%C)%*%t(C)
  
  beta.m.new <- rep(NA,p)
  beta.c.new <- rep(NA,k+1)
  alpha.c.new <- matrix(NA, nrow = p, ncol = k+1)
  alpha.a.new <- rep(NA,p)
  sigma.e.sq.new <- NA
  Sigma.m.new <- matrix(NA, nrow = p, ncol = p)
  Sigma.m.inv.new <- matrix(NA, nrow = p, ncol = p)
  delta.alpha <- 1
  delta.Sigma.m <- 1
  delta.beta <- 1
  delta.sigma.e.sq <- 1
  cnt <- 1
  
  #Coordinate Descent Algorithm
  while (((((delta.alpha > err.tol.med) | (delta.Sigma.m > err.tol.med)) | (delta.beta > err.tol.out)) | (delta.sigma.e.sq > err.tol.out)) & (cnt <= max.itr)){
    
    #Update model parameters in mediator model
    alpha.c.new <- t(CtC.inv.tC%*%(M - matrix(A, ncol = 1)%*%matrix(alpha.a.prev, nrow = 1)))
    
    alpha.tmp.vec <- rep(0, p)
    
    for(i in 1:n){
      alpha.tmp.vec <- alpha.tmp.vec + A[i]*(Sigma.m.inv.prev%*%matrix(M[i,] - as.vector(alpha.c.new%*%matrix(C[i,], ncol = 1)), ncol = 1))
    }
    alpha.tmp.vec <- alpha.tmp.vec - (1/sigma.e.sq.prev)*sum(A*(Y - M%*%matrix(beta.m.prev, ncol = 1) - C%*%matrix(beta.c.prev, ncol = 1) - first.moment*A))*matrix(beta.m.prev, ncol = 1)
    alpha.a.new <- as.vector((1/AtA)*solve(Sigma.m.inv.prev + (1/sigma.e.sq.prev)*matrix(beta.m.prev, ncol = 1)%*%matrix(beta.m.prev, nrow = 1))%*%matrix(alpha.tmp.vec, ncol = 1))
    
    Sigma.m.tmp <- matrix(0, nrow = p, ncol = p)
    for(i in 1:n){
      Sigma.m.tmp <- Sigma.m.tmp + matrix(M[i,] - A[i]*alpha.a.new - as.vector(alpha.c.new%*%matrix(C[i,], ncol = 1)), ncol = 1)%*%matrix(M[i,] - A[i]*alpha.a.new - as.vector(alpha.c.new%*%matrix(C[i,], ncol = 1)), nrow = 1)
    }
    Sigma.m.new <- Sigma.m.tmp/n
    Sigma.m.inv.new <- solve(Sigma.m.new)
    
    #Update model parameters in outcome model
    beta.c.new <- CtC.inv.tC%*%(Y - M%*%matrix(beta.m.prev, ncol = 1) - (first.moment - sum(alpha.a.new*beta.m.prev))*A)
    beta.m.new <- solve(t(M - A%*%matrix(alpha.a.new, nrow = 1))%*%(M - A%*%matrix(alpha.a.new, nrow = 1)))%*%t(M - A%*%matrix(alpha.a.new, nrow = 1))%*%(Y - C%*%matrix(beta.c.new, ncol = 1) - first.moment*A)
    sigma.e.sq.tmp.vec <- Y - C%*%matrix(beta.c.new, ncol = 1) - M%*%matrix(beta.m.new, ncol = 1) + sum(alpha.a.new*beta.m.new)*A
    sigma.e.sq.new <- (sum(sigma.e.sq.tmp.vec^2) - 2*first.moment*sum(A*sigma.e.sq.tmp.vec) + AtA*second.moment)/n
    
    #Check convergence (average error tolerance)
    delta.beta <- (sum((beta.m.new - beta.m.prev)^2) + sum((beta.c.new - beta.c.prev)^2))/(p+k+1)
    delta.alpha <- (sum((alpha.a.new - alpha.a.prev)^2) + sum((alpha.c.new - alpha.c.prev)^2))/(p*(k+1)+p)
    delta.sigma.e.sq <- abs(sigma.e.sq.new - sigma.e.sq.prev)
    delta.Sigma.m <- sum(abs(Sigma.m.new - Sigma.m.prev))/p^2
    
    #Update
    beta.m.prev <- beta.m.new
    beta.c.prev <- beta.c.new
    sigma.e.sq.prev <- sigma.e.sq.new
    alpha.c.prev <- alpha.c.new
    alpha.a.prev <- alpha.a.new
    Sigma.m.prev <- Sigma.m.new
    Sigma.m.inv.prev <- Sigma.m.inv.new
    
    cnt <- cnt + 1
  }
  
  if(cnt > max.itr){
    converged <- 0
  } else {
    converged <- 1
  }
  
  return(list("alpha.a.hat" = alpha.a.new, "alpha.c.hat" = alpha.c.new, "Sigma.m.hat" = Sigma.m.new,
              "beta.m.hat" = beta.m.new, "beta.c.hat" = beta.c.new, "sigma.e.sq.hat" = sigma.e.sq.new,
              "converged" = converged))
}

rand.eff.unpenalized <- function(Y, M, A, C = NULL, rand.eff.mean, rand.eff.var, T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external, err.tol.out = 1e-08, err.tol.med = 1e-08, max.itr = 10000){
  init.vals <- unconstrained.unpenalized(Y = Y, M = M, A = A, C = C)
  alpha.a.prev <- init.vals$alpha.a.hat
  alpha.c.prev <- init.vals$alpha.c.hat
  Sigma.m.prev <- init.vals$Sigma.m.hat
  Sigma.m.inv.prev <- solve(Sigma.m.prev)
  beta.c.prev <- init.vals$beta.c.hat
  beta.m.prev <- init.vals$beta.m.hat
  sigma.e.sq.prev <- init.vals$sigma.e.sq.hat
  
  n <- nrow(M)
  p <- ncol(M)
  if(is.null(C)){
    k <- 0
    C <- matrix(1, nrow = n)
  } else {
    k <- ncol(C)
    C <- cbind(1,C)
  }
  
  AtA <- sum(A^2)
  
  beta.m.new <- rep(NA,p)
  beta.c.new <- rep(NA,k+1)
  alpha.c.new <- matrix(NA, nrow = p, ncol = k+1)
  alpha.a.new <- rep(NA,p)
  sigma.e.sq.new <- NA
  Sigma.m.new <- matrix(NA, nrow = p, ncol = p)
  Sigma.m.inv.new <- matrix(NA, nrow = p, ncol = p)
  delta.alpha <- 1
  delta.Sigma.m <- 1
  delta.beta <- 1
  delta.sigma.e.sq <- 1
  cnt <- 1
  
  #EM Algorithm
  while (((((delta.alpha > err.tol.med) | (delta.Sigma.m > err.tol.med)) | (delta.beta > err.tol.out)) | (delta.sigma.e.sq > err.tol.out)) & (cnt <= max.itr)){
    #E-Step
    first.moment <- (AtA/sigma.e.sq.prev + 1/rand.eff.var)^(-1)*((sum(A*(Y - C%*%matrix(beta.c.prev, ncol = 1) - M%*%matrix(beta.m.prev, ncol = 1) + A*sum(alpha.a.prev*beta.m.prev)))/sigma.e.sq.prev) + (rand.eff.mean/rand.eff.var))
    second.moment <- (AtA/sigma.e.sq.prev + 1/rand.eff.var)^(-1) + (first.moment)^2
    
    #M-Step (Coordinate Descent)
    if(k == 0){
      cd.out <- rand.eff.coord.desc.unpenalized(Y = Y, M = M, A = A, C = NULL, first.moment = first.moment, second.moment = second.moment)
    } else if(k == 1) {
      cd.out <- rand.eff.coord.desc.unpenalized(Y = Y, M = M, A = A, C = matrix(C[,-1], ncol = 1), first.moment = first.moment, second.moment = second.moment)
    } else if(k > 1){
      cd.out <- rand.eff.coord.desc.unpenalized(Y = Y, M = M, A = A, C = C[,-1], first.moment = first.moment, second.moment = second.moment)
    }
    alpha.c.new <- cd.out$alpha.c.hat
    alpha.a.new <- cd.out$alpha.a.hat
    Sigma.m.new <- cd.out$Sigma.m.hat
    Sigma.m.inv.new <- solve(Sigma.m.new)
    beta.c.new <- cd.out$beta.c.hat
    beta.m.new <- cd.out$beta.m.hat
    sigma.e.sq.new <- cd.out$sigma.e.sq.hat
    
    #Check convergence (average error tolerance)
    delta.beta <- (sum((beta.m.new - beta.m.prev)^2) + sum((beta.c.new - beta.c.prev)^2))/(p+k+1)
    delta.alpha <- (sum((alpha.a.new - alpha.a.prev)^2) + sum((alpha.c.new - alpha.c.prev)^2))/(p*(k+1)+p)
    delta.sigma.e.sq <- abs(sigma.e.sq.new - sigma.e.sq.prev)
    delta.Sigma.m <- sum(abs(Sigma.m.new - Sigma.m.prev))/p^2
    
    #Update
    beta.m.prev <- beta.m.new
    beta.c.prev <- beta.c.new
    sigma.e.sq.prev <- sigma.e.sq.new
    alpha.c.prev <- alpha.c.new
    alpha.a.prev <- alpha.a.new
    Sigma.m.prev <- Sigma.m.new
    
    cnt <- cnt + 1
  }
  
  first.moment <- (AtA/sigma.e.sq.new + 1/rand.eff.var)^(-1)*((sum(A*(Y - C%*%matrix(beta.c.new, ncol = 1) - M%*%matrix(beta.m.new, ncol = 1) + A*sum(alpha.a.new*beta.m.new)))/sigma.e.sq.new) + (rand.eff.mean/rand.eff.var))
  beta.a.new <- first.moment - sum(alpha.a.new*beta.m.new)
  
  if(cnt > max.itr){
    converged <- 0
  } else {
    converged <- 1
  }
  
  return(list("alpha.a.hat" = alpha.a.new, "alpha.c.hat" = alpha.c.new, "Sigma.m.hat" = Sigma.m.new,
              "beta.m.hat" = beta.m.new, "beta.c.hat" = beta.c.new, "sigma.e.sq.hat" = sigma.e.sq.new,
              "beta.a.hat" = beta.a.new, "post.var.beta.a.hat" = (AtA/sigma.e.sq.new + 1/rand.eff.var)^(-1),
              "converged" = converged))
}

choose_s2 <- function(Y, M, A, C, T.hat.internal, var.T.hat.internal, T.hat.external, var.T.hat.external, method = "minimum_mse"){
  if(method == "minimum_mse"){
    #AtA <- sum(A^2)
    #out.mod <- lm(Y ~ M + A + C)
    #sigma.e.sq.hat <- (summary(out.mod)$sigma)^2
    #f_mse <- function(log.s2){
    #  est.var <- (AtA/sigma.e.sq.hat + 1/(exp(log.s2)*var.T.hat.external))^(-2)*(AtA/sigma.e.sq.hat + 1/((exp(log.s2))^2*var.T.hat.external))
    #  est.bias <- (AtA/sigma.e.sq.hat + 1/(exp(log.s2)*var.T.hat.external))^(-1)*(AtA*T.hat.internal/sigma.e.sq.hat + T.hat.external/(exp(log.s2)*var.T.hat.external)) - T.hat.internal
    #  est.var + (est.bias)^2
    #}
    #optimal.log.s2 <- optim(par = 0, fn = f_mse, lower = log(0.01), upper = log(3*(T.hat.internal-T.hat.external)^2/(var.T.hat.external)), method = "Brent")
    #optimal.s2 <- exp(optimal.log.s2$par)
    optimal.s2 <- (T.hat.internal - T.hat.external)^2/var.T.hat.external
  } else if(method == "eb"){
    optimal.s2 <- max(0,(T.hat.internal - T.hat.external)^2 - var.T.hat.internal)/var.T.hat.external
  } else if(method == "aic"){
    s2.grid <- exp(seq(from = log(0.01), log(3/(var(A)*var.T.hat.external)), length.out = 50))
    aic <- rep(NA, length(s2.grid))
    for(s2.idx in 1:length(s2.grid)){
      re.var <- s2.grid[s2.idx]*var.T.hat.external
      fit.soft <- rand.eff.unpenalized(Y = Y, M = M, A = A, C = C, rand.eff.mean = T.hat.external, rand.eff.var = re.var)
      aic[s2.idx] <- calc.aic(Y = Y, M = M, A = A, C = C,
                              alpha.a = fit.soft$alpha.a.hat, alpha.c = fit.soft$alpha.c.hat,
                              Sigma.m = fit.soft$Sigma.m.hat, Sigma.m.inv = solve(fit.soft$Sigma.m.hat),
                              beta.c = fit.soft$beta.c.hat, beta.m = fit.soft$beta.m.hat, beta.a = fit.soft$beta.a.hat,
                              sigma.e.sq = fit.soft$sigma.e.sq.hat, rand.eff.mean = T.hat.external, rand.eff.var = re.var)
    }
    optimal.s2 <- s2.grid[which.min(aic)]
  } else if(method == "cv"){
    #s2.grid <- exp(seq(from = log(0.01), log(3/(var(A)*var.T.hat.external)), length.out = 50))
    #cv.err <- rep(NA, length(s2.grid))
    #n.fold <- 5
    #fold.id <- sample(rep(1:n.fold, each = n/n.fold), size = n, replace = FALSE)
    #for(s2.idx in 1:length(s2.grid)){
    #  re.var <- s2.grid[s2.idx]*var.T.hat.external
    #  cv.err.tmp <- rep(0, n.fold)
    #  for(fld in 1:n.fold){
    #    Y.train <- Y[fold.id != fld]
    #    M.train <- M[fold.id != fld,]
    #    A.train <- A[fold.id != fld]
    #    C.train <- C[fold.id != fld,]
    #    
    #    Y.test <- Y[fold.id == fld]
    #    M.test <- M[fold.id == fld,]
    #    A.test <- A[fold.id == fld]
    #    C.test <- C[fold.id == fld,]
    #    
    #    fit.soft <- rand.eff.unpenalized(Y = Y.train, M = M.train, A = A.train, C = C.train, rand.eff.mean = T.hat.external, rand.eff.var = re.var)
    #    
    #    Y.test.hat <- cbind(1,C.test)%*%fit.soft$beta.c.hat + M.test%*%fit.soft$beta.m.hat + A.test*fit.soft$beta.a.hat
    #    M.test.hat <- cbind(1,C.test)%*%t(fit.soft$alpha.c.hat) + matrix(A.test, ncol = 1)%*%matrix(fit.soft$alpha.a.hat, nrow = 1)
    #    
    #    Sigma.m.inv.hat.test <- solve(fit.soft$Sigma.m.hat)
    #    cv.err.tmp[fld] <- 0
    #    for(i in 1:nrow(M.test)){
    #      cv.err.tmp[fld] <- cv.err.tmp[fld] + matrix(M.test[i,] - M.test.hat[i,], nrow = 1)%*%Sigma.m.inv.hat.test%*%matrix(M.test[i,] - M.test.hat[i,], ncol = 1)
    #    }
    #    cv.err.tmp[fld] <- cv.err.tmp[fld] + sum((Y.test-Y.test.hat)^2)/fit.soft$sigma.e.sq.hat
    #  }
    #  cv.err[s2.idx] <- mean(cv.err.tmp)
    #}
    #optimal.s2 <- s2.grid[which.min(cv.err)]
    
    s2.grid <- exp(seq(from = log(0.01), log(5/(var(A)*var.T.hat.external)), length.out = 25))
    cv.err <- rep(NA, length(s2.grid))
    n.split <- 10
    frac.train <- 0.8
    for(s2.idx in 1:length(s2.grid)){
      set.seed(20221201)
      print(s2.idx)
      print(s2.grid[s2.idx])
      re.var <- s2.grid[s2.idx]*var.T.hat.external
      cv.err.tmp <- rep(0, n.split)
      for(fld in 1:n.split){
        fold.id <- sample(1:n, size = frac.train*n, replace = FALSE)
        
        Y.train <- Y[fold.id]
        M.train <- M[fold.id,]
        A.train <- A[fold.id]
        C.train <- C[fold.id,]
        
        Y.test <- Y[!(1:n %in% fold.id)]
        M.test <- M[!(1:n %in% fold.id),]
        A.test <- A[!(1:n %in% fold.id)]
        C.test <- C[!(1:n %in% fold.id),]
                
        fit.soft <- rand.eff.unpenalized(Y = Y.train, M = M.train, A = A.train, C = C.train, rand.eff.mean = T.hat.external, rand.eff.var = re.var)
                
        Y.test.hat <- cbind(1,C.test)%*%fit.soft$beta.c.hat + M.test%*%fit.soft$beta.m.hat + A.test*fit.soft$beta.a.hat
        M.test.hat <- cbind(1,C.test)%*%t(fit.soft$alpha.c.hat) + matrix(A.test, ncol = 1)%*%matrix(fit.soft$alpha.a.hat, nrow = 1)
        
        Sigma.m.inv.hat.test <- solve(fit.soft$Sigma.m.hat)
        cv.err.tmp[fld] <- 0
        for(i in 1:nrow(M.test)){
          cv.err.tmp[fld] <- cv.err.tmp[fld] + matrix(M.test[i,] - M.test.hat[i,], nrow = 1)%*%Sigma.m.inv.hat.test%*%matrix(M.test[i,] - M.test.hat[i,], ncol = 1)
        }
        cv.err.tmp[fld] <- cv.err.tmp[fld] + sum((Y.test-Y.test.hat)^2)/fit.soft$sigma.e.sq.hat
      }
      cv.err[s2.idx] <- mean(cv.err.tmp)
    }
    optimal.s2 <- s2.grid[which.min(cv.err)]
  }
  return(optimal.s2)
}

soft.threshold <- function(z, theta){
  if(abs(z) - theta > 0){
    val <- sign(z)*(abs(z) - theta)
  } else if(abs(z) - theta <= 0){
    val <- 0
  }
  return(val)
}

unconstrained.penalized <- function(Y, M, A, C, lambda.alpha, lambda.beta, gamma.alpha = 2, gamma.beta = 2, err.tol.out = 1e-08, err.tol.med = 1e-08, max.itr = 10000){
  n <- nrow(M)
  p <- ncol(M)
  if(is.null(C)){
    k <- 0
    C <- matrix(1, nrow = n)
  } else {
    k <- ncol(C)
    C <- cbind(1,C)
  }
  
  X <- cbind(A,C)
  tX <- t(X)
  XtX.inv <- solve(tX%*%X)
  XtX.inv.tX <- XtX.inv%*%tX
  tC <- t(C)
  CtC.inv <- solve(tC%*%C)
  CtC.inv.tC <- CtC.inv%*%tC
  
  unpenalized_mle <- unconstrained.unpenalized(Y = Y, M = M, A = A, C = C[,-1])
  sum.sq.exp <- sum(A^2)
  adpt.w.beta.m <- 1/abs(unpenalized_mle$beta.m.hat)^(gamma.beta)
  adpt.w.alpha.a <- 1/abs(unpenalized_mle$alpha.a.hat)^(gamma.alpha)
  sq.m <- apply(M, 2, function(x){sum(x^2)})
  sigma.e.sq.hat <- unpenalized_mle$sigma.e.sq.hat
  Sigma.m.hat <- unpenalized_mle$Sigma.m.hat
  Sigma.m.hat.inv <- solve(Sigma.m.hat)
  penalty.term.beta.m <- (lambda.beta*sigma.e.sq.hat*adpt.w.beta.m)/sq.m
  penalty.term.alpha.a <- (lambda.alpha*adpt.w.alpha.a)/(sum.sq.exp*diag(Sigma.m.hat.inv))
  
  beta.m.prev <- unpenalized_mle$beta.m.hat
  beta.a.prev <- unpenalized_mle$beta.a.hat
  beta.c.prev <- unpenalized_mle$beta.c.hat
  beta.o.prev <- c(beta.a.prev, beta.c.prev)
  alpha.c.prev <- unpenalized_mle$alpha.c.hat
  alpha.a.prev <- unpenalized_mle$alpha.a.hat
  beta.m.new <- rep(NA,p)
  beta.a.new <- NA
  beta.c.new <- rep(NA,k+1)
  alpha.c.new <- matrix(NA, nrow = p, ncol = k+1)
  alpha.a.new <- rep(NA,p)
  delta.alpha <- 1
  delta.beta <- 1
  cnt <- 1
  while (((delta.alpha > err.tol.med) | (delta.beta > err.tol.out)) & (cnt <= max.itr)) {
    alpha.c.new <- t(CtC.inv.tC%*%(M - matrix(A, ncol = 1)%*%matrix(alpha.a.prev, nrow = 1)))
    med.resid.tmp <- rep(0, p)
    for(i in 1:n){
      med.resid.tmp <- med.resid.tmp + as.vector(A[i]*((matrix(M[i,], nrow = 1) - t(alpha.c.new%*%matrix(C[i,], ncol = 1)))%*%Sigma.m.hat.inv))
    }
    Y.star <- Y - M%*%matrix(beta.m.prev, ncol = 1)
    beta.o.new <- as.vector(XtX.inv.tX%*%Y.star)
    beta.a.new <- beta.o.new[1]
    beta.c.new <- beta.o.new[2:(k+2)]
    for(j in 1:p){
      if(j == 1){
        beta.tmp.vec <- beta.m.prev[2:p]
        alpha.tmp.vec <- alpha.a.prev[2:p]
      } else if(j %in% 2:(p-1)){
        beta.tmp.vec <- c(beta.m.new[1:(j-1)],beta.m.prev[(j+1):p])
        alpha.tmp.vec <- c(alpha.a.new[1:(j-1)],alpha.a.prev[(j+1):p])
      } else if(j == p){
        beta.tmp.vec <- beta.m.new[1:(p-1)]
        alpha.tmp.vec <- alpha.a.new[1:(p-1)]
      }
      tmp.cor <- sum(M[,j]*as.vector(Y - X%*%matrix(beta.o.new, ncol = 1) - M[,-j]%*%matrix(beta.tmp.vec, ncol = 1)))
      beta.m.new[j] <- soft.threshold(tmp.cor/sq.m[j], penalty.term.beta.m[j])
      alpha.a.new[j] <- soft.threshold(med.resid.tmp[j]/(sum.sq.exp*Sigma.m.hat.inv[j,j]) - sum(alpha.tmp.vec*Sigma.m.hat.inv[-j,j])/(Sigma.m.hat.inv[j,j]), penalty.term.alpha.a[j])
    }
    
    #Check convergence (average error tolerance per regression coefficient)
    delta.beta <- (sum((beta.m.new - beta.m.prev)^2) + sum((beta.c.new - beta.c.prev)^2) + (beta.a.new - beta.a.prev)^2)/(p+k+1+1)
    delta.alpha <- (sum((alpha.a.new - alpha.a.prev)^2) + sum((alpha.c.new - alpha.c.prev)^2))/(p*(k+1)+p)
    
    #Update
    beta.o.prev <- beta.o.new
    beta.a.prev <- beta.a.new
    beta.m.prev <- beta.m.new
    beta.c.prev <- beta.c.new
    alpha.c.prev <- alpha.c.new
    alpha.a.prev <- alpha.a.new
    
    cnt <- cnt + 1
  }
  
  if(cnt > max.itr){
    converged <- 0
  } else {
    converged <- 1
  }
  
  return(list("alpha.a.hat" = alpha.a.new, "alpha.c.hat" = alpha.c.new, "Sigma.m.hat" = Sigma.m.hat,
              "beta.m.hat" = beta.m.new, "beta.a.hat" = beta.a.new, "beta.c.hat" = beta.c.new, "sigma.e.sq.hat" = sigma.e.sq.hat,
              "lambda.alpha" = lambda.alpha, "lambda.beta" = lambda.beta,
              "gamma.alpha" = gamma.alpha, "gamma.beta" = gamma.beta, "converged" = converged))
}

constrained.penalized <- function(Y, M, A, C, T.hat.external, lambda.alpha, lambda.beta, gamma.alpha = 2, gamma.beta = 2, err.tol.out = 1e-08, err.tol.med = 1e-08, max.itr = 10000){
  n <- nrow(M)
  p <- ncol(M)
  if(is.null(C)){
    k <- 0
    C <- matrix(1, nrow = n)
  } else {
    k <- ncol(C)
    C <- cbind(1,C)
  }

  X <- cbind(A,C)
  tX <- t(X)
  XtX.inv <- solve(tX%*%X)
  XtX.inv.tX <- XtX.inv%*%tX
  tC <- t(C)
  CtC.inv <- solve(tC%*%C)
  CtC.inv.tC <- CtC.inv%*%tC
  
  unpenalized_mle <- unconstrained.unpenalized(Y = Y, M = M, A = A, C = C[,-1])
  sum.sq.exp <- sum(A^2)
  adpt.w.beta.m <- 1/abs(unpenalized_mle$beta.m.hat)^(gamma.beta)
  adpt.w.alpha.a <- 1/abs(unpenalized_mle$alpha.a.hat)^(gamma.alpha)
  sigma.e.sq.hat <- unpenalized_mle$sigma.e.sq.hat
  Sigma.m.hat <- unpenalized_mle$Sigma.m.hat
  Sigma.m.hat.inv <- solve(Sigma.m.hat)
  penalty.term.beta.m <- lambda.beta*adpt.w.beta.m
  penalty.term.alpha.a <- lambda.alpha*adpt.w.alpha.a
  
  beta.m.prev <- unpenalized_mle$beta.m.hat
  beta.c.prev <- unpenalized_mle$beta.c.hat
  alpha.c.prev <- unpenalized_mle$alpha.c.hat
  alpha.a.prev <- unpenalized_mle$alpha.a.hat
  beta.m.new <- rep(NA,p)
  beta.c.new <- rep(NA,k+1)
  alpha.c.new <- matrix(NA, nrow = p, ncol = k+1)
  alpha.a.new <- rep(NA,p)
  delta.alpha <- 1
  delta.beta <- 1
  cnt <- 1
  while (((delta.alpha > err.tol.med) | (delta.beta > err.tol.out)) & (cnt <= max.itr)) {
    alpha.c.new <- t(CtC.inv.tC%*%(M - matrix(A, ncol = 1)%*%matrix(alpha.a.prev, nrow = 1)))
    med.resid.tmp <- rep(0, p)
    for(i in 1:n){
      med.resid.tmp <- med.resid.tmp + as.vector(A[i]*((matrix(M[i,], nrow = 1) - t(alpha.c.new%*%matrix(C[i,], ncol = 1)))%*%Sigma.m.hat.inv))
    }
    Y.star <- Y - M%*%matrix(beta.m.prev, ncol = 1) - A*(T.hat.external - sum(alpha.a.prev*beta.m.prev))
    beta.c.new <- as.vector(CtC.inv.tC%*%Y.star)
    for(j in 1:p){
      if(j == 1){
        beta.tmp.vec <- beta.m.prev[2:p]
        alpha.tmp.vec <- alpha.a.prev[2:p]
      } else if(j %in% 2:(p-1)){
        beta.tmp.vec <- c(beta.m.new[1:(j-1)],beta.m.prev[(j+1):p])
        alpha.tmp.vec <- c(alpha.a.new[1:(j-1)],alpha.a.prev[(j+1):p])
      } else if(j == p){
        beta.tmp.vec <- beta.m.new[1:(p-1)]
        alpha.tmp.vec <- alpha.a.new[1:(p-1)]
      }
      beta.m.new[j] <- soft.threshold((1/sigma.e.sq.hat)*sum((M[,j] - A*alpha.a.prev[j])*(Y - (T.hat.external - sum(alpha.tmp.vec*beta.tmp.vec))*A - M[,-j]%*%matrix(beta.tmp.vec, ncol = 1) - C%*%matrix(beta.c.new, ncol = 1))), penalty.term.beta.m[j])/((1/sigma.e.sq.hat)*sum((M[,j] - A*alpha.a.prev[j])^2))
      if(j < p){
        const.alpha.a.update <- sum(A*(Y - M%*%matrix(c(beta.m.new[1:j],beta.m.prev[(j+1):p]), ncol = 1) - (T.hat.external - sum(alpha.tmp.vec*beta.tmp.vec))*A - C%*%matrix(beta.c.new, ncol = 1)))
      } else if(j == p){
        const.alpha.a.update <- sum(A*(Y - M%*%matrix(beta.m.new[1:p], ncol = 1) - (T.hat.external - sum(alpha.tmp.vec*beta.tmp.vec))*A - C%*%matrix(beta.c.new, ncol = 1)))
      }
      alpha.a.new[j] <- soft.threshold(med.resid.tmp[j] - sum.sq.exp*sum(alpha.tmp.vec*Sigma.m.hat.inv[-j,j]) - (beta.m.new[j]/sigma.e.sq.hat)*const.alpha.a.update, penalty.term.alpha.a[j])/(sum.sq.exp*Sigma.m.hat.inv[j,j] + sum.sq.exp*beta.m.new[j]^2/sigma.e.sq.hat)
    }
    
    #Check convergence (average error tolerance per regression coefficient)
    delta.beta <- (sum((beta.m.new - beta.m.prev)^2) + sum((beta.c.new - beta.c.prev)^2))/(p+k+1)
    delta.alpha <- (sum((alpha.a.new - alpha.a.prev)^2) + sum((alpha.c.new - alpha.c.prev)^2))/(p*(k+1)+p)
    
    #Update
    beta.m.prev <- beta.m.new
    beta.c.prev <- beta.c.new
    alpha.c.prev <- alpha.c.new
    alpha.a.prev <- alpha.a.new
    
    cnt <- cnt + 1
  }
  
  if(cnt > max.itr){
    converged <- 0
  } else {
    converged <- 1
  }
  
  return(list("alpha.a.hat" = alpha.a.new, "alpha.c.hat" = alpha.c.new, "Sigma.m.hat" = Sigma.m.hat,
              "beta.m.hat" = beta.m.new, "beta.c.hat" = beta.c.new, "sigma.e.sq.hat" = sigma.e.sq.hat,
              "T.hat.external" = T.hat.external,
              "lambda.alpha" = lambda.alpha, "lambda.beta" = lambda.beta,
              "gamma.alpha" = gamma.alpha, "gamma.beta" = gamma.beta, "converged" = converged))
}

soft.penalized <- function(Y, M, A, C, T.hat.external, var.T.hat.external, s2, lambda.alpha, lambda.beta, gamma.alpha = 2, gamma.beta = 2, err.tol.out = 1e-08, err.tol.med = 1e-08, max.itr = 10000){
  n <- nrow(M)
  p <- ncol(M)
  if(is.null(C)){
    k <- 0
    C <- matrix(1, nrow = n)
  } else {
    k <- ncol(C)
    C <- cbind(1,C)
  }

  X <- cbind(A,C)
  tX <- t(X)
  XtX.inv <- solve(tX%*%X)
  XtX.inv.tX <- XtX.inv%*%tX
  tC <- t(C)
  CtC.inv <- solve(tC%*%C)
  CtC.inv.tC <- CtC.inv%*%tC
  
  unpenalized_mle <- unconstrained.unpenalized(Y = Y, M = M, A = A, C = C[,-1])
  sum.sq.exp <- sum(A^2)
  adpt.w.beta.m <- 1/abs(unpenalized_mle$beta.m.hat)^(gamma.beta)
  adpt.w.alpha.a <- 1/abs(unpenalized_mle$alpha.a.hat)^(gamma.alpha)
  sigma.e.sq.hat <- unpenalized_mle$sigma.e.sq.hat
  Sigma.m.hat <- unpenalized_mle$Sigma.m.hat
  Sigma.m.hat.inv <- solve(Sigma.m.hat)
  penalty.term.beta.m <- lambda.beta*adpt.w.beta.m
  penalty.term.alpha.a <- lambda.alpha*adpt.w.alpha.a
  
  beta.m.prev <- unpenalized_mle$beta.m.hat
  beta.c.prev <- unpenalized_mle$beta.c.hat
  alpha.c.prev <- unpenalized_mle$alpha.c.hat
  alpha.a.prev <- unpenalized_mle$alpha.a.hat
  theta.a.I.prev <- T.hat.external
  beta.m.new <- rep(NA,p)
  beta.c.new <- rep(NA,k+1)
  alpha.c.new <- matrix(NA, nrow = p, ncol = k+1)
  alpha.a.new <- rep(NA,p)
  theta.a.I.new <- NA
  delta.alpha <- 1
  delta.beta <- 1
  
  cnt <- 1
  while (((delta.alpha > err.tol.med) | (delta.beta > err.tol.out)) & (cnt <= max.itr)) {
    alpha.c.new <- t(CtC.inv.tC%*%(M - matrix(A, ncol = 1)%*%matrix(alpha.a.prev, nrow = 1)))
    med.resid.tmp <- rep(0, p)
    for(i in 1:n){
      med.resid.tmp <- med.resid.tmp + as.vector(A[i]*((matrix(M[i,], nrow = 1) - t(alpha.c.new%*%matrix(C[i,], ncol = 1)))%*%Sigma.m.hat.inv))
    }
    Y.star <- Y - M%*%matrix(beta.m.prev, ncol = 1) - A*(theta.a.I.prev - sum(alpha.a.prev*beta.m.prev))
    beta.c.new <- as.vector(CtC.inv.tC%*%Y.star)
    for(j in 1:p){
      if(j == 1){
        beta.tmp.vec <- beta.m.prev[2:p]
        alpha.tmp.vec <- alpha.a.prev[2:p]
      } else if(j %in% 2:(p-1)){
        beta.tmp.vec <- c(beta.m.new[1:(j-1)],beta.m.prev[(j+1):p])
        alpha.tmp.vec <- c(alpha.a.new[1:(j-1)],alpha.a.prev[(j+1):p])
      } else if(j == p){
        beta.tmp.vec <- beta.m.new[1:(p-1)]
        alpha.tmp.vec <- alpha.a.new[1:(p-1)]
      }
      beta.m.new[j] <- soft.threshold((1/sigma.e.sq.hat)*sum((M[,j] - A*alpha.a.prev[j])*(Y - (theta.a.I.prev - sum(alpha.tmp.vec*beta.tmp.vec))*A - M[,-j]%*%matrix(beta.tmp.vec, ncol = 1) - C%*%matrix(beta.c.new, ncol = 1))), penalty.term.beta.m[j])/((1/sigma.e.sq.hat)*sum((M[,j] - A*alpha.a.prev[j])^2))
      if(j < p){
        const.alpha.a.update <- sum(A*(Y - M%*%matrix(c(beta.m.new[1:j],beta.m.prev[(j+1):p]), ncol = 1) - (theta.a.I.prev - sum(alpha.tmp.vec*beta.tmp.vec))*A - C%*%matrix(beta.c.new, ncol = 1)))
      } else if(j == p){
        const.alpha.a.update <- sum(A*(Y - M%*%matrix(beta.m.new[1:p], ncol = 1) - (theta.a.I.prev - sum(alpha.tmp.vec*beta.tmp.vec))*A - C%*%matrix(beta.c.new, ncol = 1)))
      }
      alpha.a.new[j] <- soft.threshold(med.resid.tmp[j] - sum.sq.exp*sum(alpha.tmp.vec*Sigma.m.hat.inv[-j,j]) - (beta.m.new[j]/sigma.e.sq.hat)*const.alpha.a.update, penalty.term.alpha.a[j])/(sum.sq.exp*Sigma.m.hat.inv[j,j] + sum.sq.exp*beta.m.new[j]^2/sigma.e.sq.hat)
    }
    theta.a.I.new <- (sum(A^2)/sigma.e.sq.hat + 1/(s2*var.T.hat.external))^(-1)*(sum(A*(Y - M%*%matrix(beta.m.new, ncol = 1) - C%*%matrix(beta.c.new, ncol = 1) + sum(alpha.a.new*beta.m.new)*A))/sigma.e.sq.hat + T.hat.external/(s2*var.T.hat.external))
    
    #Check convergence (average error tolerance per regression coefficient)
    delta.beta <- (sum((beta.m.new - beta.m.prev)^2) + sum((beta.c.new - beta.c.prev)^2) + (theta.a.I.new-theta.a.I.prev)^2)/(p+k+1+1)
    delta.alpha <- (sum((alpha.a.new - alpha.a.prev)^2) + sum((alpha.c.new - alpha.c.prev)^2))/(p*(k+1)+p)
    
    #Update
    beta.m.prev <- beta.m.new
    beta.c.prev <- beta.c.new
    alpha.c.prev <- alpha.c.new
    alpha.a.prev <- alpha.a.new
    theta.a.I.prev <- theta.a.I.new
    
    cnt <- cnt + 1
  }
  
  if(cnt > max.itr){
    converged <- 0
  } else {
    converged <- 1
  }
  
  return(list("alpha.a.hat" = alpha.a.new, "alpha.c.hat" = alpha.c.new, "Sigma.m.hat" = Sigma.m.hat,
              "beta.m.hat" = beta.m.new, "beta.c.hat" = beta.c.new, "sigma.e.sq.hat" = sigma.e.sq.hat,
              "T.hat.external" = T.hat.external, "beta.a.hat" = theta.a.I.new - sum(alpha.a.new*beta.m.new),
              "lambda.alpha" = lambda.alpha, "lambda.beta" = lambda.beta,
              "gamma.alpha" = gamma.alpha, "gamma.beta" = gamma.beta, "converged" = converged))
}

sim.data <- function(n, p.con, p.mediators, rho.con.exp, rho.mediators,
                     r2.mediator, r2.outcome, total.effect.internal,
                     n.external.ratio, is.same.external, total.effect.external){

  #Generate internal data
  mediator.model.regressor.cov <- matrix(rho.con.exp, nrow = 1+p.con, ncol = 1+p.con)
  diag(mediator.model.regressor.cov) <- 1
  mediator.model.regressors <- mvrnorm(n = n, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
  colnames(mediator.model.regressors) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
  alpha.c <- matrix(0.1, nrow = p.con, ncol = p.mediators)
  alpha.a.single <- 0.6
  alpha.a <- c(rep(alpha.a.single, 10), rep(0, 40))
  mediator.cor <- matrix(rho.mediators, nrow = p.mediators, ncol = p.mediators)
  mediator.cor[1:5,1:5] <- 0.3
  mediator.cor[6:10,6:10] <- 0.3
  mediator.cor[11:15,11:15] <- 0.3
  mediator.cor[16:50,16:50] <- 0.3
  diag(mediator.cor) <- 1
  #sigma.sq.mediator <- as.numeric(((1-r2.mediator)/r2.mediator)*matrix(c(alpha.a.single,rep(0.1, p.con)), nrow = 1)%*%mediator.model.regressor.cov%*%matrix(c(alpha.a.single,rep(0.1, p.con)), ncol = 1))
  sigma.sq.mediator <- ((1-r2.mediator)/r2.mediator)*(alpha.a.single*mediator.model.regressor.cov[1,1]*alpha.a.single)
  mediator.cov <- diag(sqrt(sigma.sq.mediator), p.mediators)%*%mediator.cor%*%diag(sqrt(sigma.sq.mediator), p.mediators)
  M.internal <- matrix(mediator.model.regressors[, colnames(mediator.model.regressors) == "A"], ncol = 1)%*%matrix(alpha.a, nrow = 1) +
    mediator.model.regressors[, colnames(mediator.model.regressors) != "A"]%*%alpha.c +
    mvrnorm(n = n, rep(0,p.mediators), Sigma = mediator.cov)
  A.internal <- as.vector(mediator.model.regressors[, colnames(mediator.model.regressors) == "A"])
  C.internal <- mediator.model.regressors[, colnames(mediator.model.regressors) != "A"]

  beta.c <- rep(0.1, p.con)
  beta.m.single <- 0.1
  beta.m <- c(rep(beta.m.single, 5), rep(0, 5), rep(beta.m.single, 5), rep(0, 35))
  beta.a <- total.effect.internal - sum(alpha.a*beta.m)
  if(beta.a < 0){
    stop('Total effect is negative!')
  }
  outcome.cov <- matrix(0, nrow = p.mediators + 1 + p.con, ncol = p.mediators + 1 + p.con)
  outcome.cov[1:p.mediators, 1:p.mediators] <- mediator.cov + t(as.matrix(rbind(alpha.a, alpha.c)))%*%mediator.model.regressor.cov%*%as.matrix(rbind(alpha.a, alpha.c))
  outcome.cov[(1+p.mediators):(p.mediators+1+p.con), (1+p.mediators):(p.mediators+1+p.con)] <- mediator.model.regressor.cov
  outcome.cov[1:p.mediators, (1+p.mediators):(p.mediators+1+p.con)] <- t(as.matrix(rbind(alpha.a, alpha.c)))%*%mediator.model.regressor.cov
  outcome.cov[(1+p.mediators):(p.mediators+1+p.con), 1:p.mediators] <- t(outcome.cov[1:p.mediators, (1+p.mediators):(p.mediators+1+p.con)])
  #Can show this using law of total variance and reconstructing joint multivariate normal distribution from conditional and marginal
  #sigma.e.sq <- ((1-r2.outcome)/r2.outcome)*(matrix(c(beta.m,beta.a,beta.c), nrow = 1)%*%outcome.cov%*%matrix(c(beta.m,beta.a,beta.c), ncol = 1))
  sigma.e.sq <- ((1-r2.outcome)/r2.outcome)*(matrix(beta.m, nrow = 1)%*%outcome.cov[1:p.mediators, 1:p.mediators]%*%matrix(beta.m, ncol = 1))
  Y.internal <- as.vector(cbind(M.internal,mediator.model.regressors)%*%matrix(c(beta.m,beta.a,beta.c), ncol = 1)) + rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq))
  
  int.mod <- lm(Y.internal ~ A.internal + C.internal)
  T.hat.internal <- coef(int.mod)[names(coef(int.mod)) == "A.internal"]
  var.T.hat.internal <- vcov(int.mod)[row.names(vcov(int.mod)) == "A.internal", colnames(vcov(int.mod)) == "A.internal"]
  
  #Generate external data
  if(is.same.external == TRUE){
    n.external <- n.external.ratio*n
    mediator.model.regressors.ext <- mvrnorm(n = n.external, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
    colnames(mediator.model.regressors.ext) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
    sigma.ext.sq <- sigma.e.sq + as.numeric(matrix(beta.m, nrow = 1)%*%mediator.cov%*%matrix(beta.m, ncol = 1))
    total.effect.external <- total.effect.internal
    external.con.coeffs <- (alpha.c%*%matrix(beta.m, ncol = 1))
    A.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) == "A"]
    C.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) != "A"]
    Y.external <- A.external*total.effect.external + as.vector(C.external%*%external.con.coeffs) + rnorm(n = n.external, 0, sqrt(sigma.ext.sq))
    ext.mod <- lm(Y.external ~ A.external + C.external)
    T.hat.external <- coef(ext.mod)[names(coef(ext.mod)) == "A.external"]
    var.T.hat.external <- vcov(ext.mod)[row.names(vcov(ext.mod)) == "A.external", colnames(vcov(ext.mod)) == "A.external"]
  } else {
    n.external <- n.external.ratio*n
    mediator.model.regressors.ext <- mvrnorm(n = n.external, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
    colnames(mediator.model.regressors.ext) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
    sigma.ext.sq <- sigma.e.sq + as.numeric(matrix(beta.m, nrow = 1)%*%mediator.cov%*%matrix(beta.m, ncol = 1))
    external.con.coeffs <- (alpha.c%*%matrix(beta.m, ncol = 1))
    A.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) == "A"]
    C.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) != "A"]
    Y.external <- A.external*total.effect.external + as.vector(C.external%*%external.con.coeffs) + rnorm(n = n.external, 0, sqrt(sigma.ext.sq))
    ext.mod <- lm(Y.external ~ A.external + C.external)
    T.hat.external <- coef(ext.mod)[names(coef(ext.mod)) == "A.external"]
    var.T.hat.external <- vcov(ext.mod)[row.names(vcov(ext.mod)) == "A.external", colnames(vcov(ext.mod)) == "A.external"]
  }

  return(list("Y" = Y.internal, "M" = M.internal, "A" = A.internal, "C" = C.internal,
              "R2.m" = summary(lm(M.internal[,1] ~ A.internal + C.internal))$r.squared,
              "R2.o" = summary(lm(Y.internal ~ M.internal + A.internal + C.internal))$r.squared,
              "alpha.a" = alpha.a, "alpha.c" = alpha.c,
              "beta.a" = beta.a, "beta.m" = beta.m, "beta.c" = beta.c,
              "sigma.e.sq" = sigma.e.sq, "Sigma.m" = mediator.cov,
              "n.external" = n.external, "T.hat.external" = T.hat.external,
              "var.T.hat.external" = var.T.hat.external,
              "T.hat.internal" = T.hat.internal,
              "var.T.hat.internal" = var.T.hat.internal,
              "R2.ext" = summary(ext.mod)$r.squared))
}

sim.data.no.ie <- function(n, p.con, p.mediators, rho.con.exp, rho.mediators,
                     	   r2.mediator, r2.outcome, total.effect.internal,
                     	   n.external.ratio, is.same.external, total.effect.external){

  #Generate internal data
  mediator.model.regressor.cov <- matrix(rho.con.exp, nrow = 1+p.con, ncol = 1+p.con)
  diag(mediator.model.regressor.cov) <- 1
  mediator.model.regressors <- mvrnorm(n = n, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
  colnames(mediator.model.regressors) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
  alpha.c <- matrix(0.1, nrow = p.con, ncol = p.mediators)
  alpha.a.single <- 0.6
  alpha.a <- c(rep(alpha.a.single, 10), rep(0, 40))
  mediator.cor <- matrix(rho.mediators, nrow = p.mediators, ncol = p.mediators)
  mediator.cor[1:5,1:5] <- 0.3
  mediator.cor[6:10,6:10] <- 0.3
  mediator.cor[11:15,11:15] <- 0.3
  mediator.cor[16:50,16:50] <- 0.3
  diag(mediator.cor) <- 1
  #sigma.sq.mediator <- as.numeric(((1-r2.mediator)/r2.mediator)*matrix(c(alpha.a.single,rep(0.1, p.con)), nrow = 1)%*%mediator.model.regressor.cov%*%matrix(c(alpha.a.single,rep(0.1, p.con)), ncol = 1))
  sigma.sq.mediator <- ((1-r2.mediator)/r2.mediator)*(alpha.a.single*mediator.model.regressor.cov[1,1]*alpha.a.single)
  mediator.cov <- diag(sqrt(sigma.sq.mediator), p.mediators)%*%mediator.cor%*%diag(sqrt(sigma.sq.mediator), p.mediators)
  M.internal <- matrix(mediator.model.regressors[, colnames(mediator.model.regressors) == "A"], ncol = 1)%*%matrix(alpha.a, nrow = 1) +
    mediator.model.regressors[, colnames(mediator.model.regressors) != "A"]%*%alpha.c +
    mvrnorm(n = n, rep(0,p.mediators), Sigma = mediator.cov)
  A.internal <- as.vector(mediator.model.regressors[, colnames(mediator.model.regressors) == "A"])
  C.internal <- mediator.model.regressors[, colnames(mediator.model.regressors) != "A"]

  beta.c <- rep(0.1, p.con)
  beta.m.single <- 0
  beta.m <- c(rep(beta.m.single, 5), rep(0, 5), rep(beta.m.single, 5), rep(0, 35))
  beta.a <- total.effect.internal - sum(alpha.a*beta.m)
  if(beta.a < 0){
    stop('Total effect is negative!')
  }
  outcome.cov <- matrix(0, nrow = p.mediators + 1 + p.con, ncol = p.mediators + 1 + p.con)
  outcome.cov[1:p.mediators, 1:p.mediators] <- mediator.cov + t(as.matrix(rbind(alpha.a, alpha.c)))%*%mediator.model.regressor.cov%*%as.matrix(rbind(alpha.a, alpha.c))
  outcome.cov[(1+p.mediators):(p.mediators+1+p.con), (1+p.mediators):(p.mediators+1+p.con)] <- mediator.model.regressor.cov
  outcome.cov[1:p.mediators, (1+p.mediators):(p.mediators+1+p.con)] <- t(as.matrix(rbind(alpha.a, alpha.c)))%*%mediator.model.regressor.cov
  outcome.cov[(1+p.mediators):(p.mediators+1+p.con), 1:p.mediators] <- t(outcome.cov[1:p.mediators, (1+p.mediators):(p.mediators+1+p.con)])
  #Can show this using law of total variance and reconstructing joint multivariate normal distribution from conditional and marginal
  sigma.e.sq <- ((1-r2.outcome)/r2.outcome)*(matrix(c(beta.m,beta.a,beta.c), nrow = 1)%*%outcome.cov%*%matrix(c(beta.m,beta.a,beta.c), ncol = 1))
  #sigma.e.sq <- ((1-r2.outcome)/r2.outcome)*(matrix(beta.m, nrow = 1)%*%outcome.cov[1:p.mediators, 1:p.mediators]%*%matrix(beta.m, ncol = 1))
  Y.internal <- as.vector(cbind(M.internal,mediator.model.regressors)%*%matrix(c(beta.m,beta.a,beta.c), ncol = 1)) + rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq))
  
  int.mod <- lm(Y.internal ~ A.internal + C.internal)
  T.hat.internal <- coef(int.mod)[names(coef(int.mod)) == "A.internal"]
  var.T.hat.internal <- vcov(int.mod)[row.names(vcov(int.mod)) == "A.internal", colnames(vcov(int.mod)) == "A.internal"]
  
  #Generate external data
  if(is.same.external == TRUE){
    n.external <- n.external.ratio*n
    mediator.model.regressors.ext <- mvrnorm(n = n.external, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
    colnames(mediator.model.regressors.ext) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
    sigma.ext.sq <- sigma.e.sq + as.numeric(matrix(beta.m, nrow = 1)%*%mediator.cov%*%matrix(beta.m, ncol = 1))
    total.effect.external <- total.effect.internal
    external.con.coeffs <- (alpha.c%*%matrix(beta.m, ncol = 1))
    A.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) == "A"]
    C.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) != "A"]
    Y.external <- A.external*total.effect.external + as.vector(C.external%*%external.con.coeffs) + rnorm(n = n.external, 0, sqrt(sigma.ext.sq))
    ext.mod <- lm(Y.external ~ A.external + C.external)
    T.hat.external <- coef(ext.mod)[names(coef(ext.mod)) == "A.external"]
    var.T.hat.external <- vcov(ext.mod)[row.names(vcov(ext.mod)) == "A.external", colnames(vcov(ext.mod)) == "A.external"]
  } else {
    n.external <- n.external.ratio*n
    mediator.model.regressors.ext <- mvrnorm(n = n.external, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
    colnames(mediator.model.regressors.ext) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
    sigma.ext.sq <- sigma.e.sq + as.numeric(matrix(beta.m, nrow = 1)%*%mediator.cov%*%matrix(beta.m, ncol = 1))
    external.con.coeffs <- (alpha.c%*%matrix(beta.m, ncol = 1))
    A.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) == "A"]
    C.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) != "A"]
    Y.external <- A.external*total.effect.external + as.vector(C.external%*%external.con.coeffs) + rnorm(n = n.external, 0, sqrt(sigma.ext.sq))
    ext.mod <- lm(Y.external ~ A.external + C.external)
    T.hat.external <- coef(ext.mod)[names(coef(ext.mod)) == "A.external"]
    var.T.hat.external <- vcov(ext.mod)[row.names(vcov(ext.mod)) == "A.external", colnames(vcov(ext.mod)) == "A.external"]
  }

  return(list("Y" = Y.internal, "M" = M.internal, "A" = A.internal, "C" = C.internal,
              "R2.m" = summary(lm(M.internal[,1] ~ A.internal + C.internal))$r.squared,
              "R2.o" = summary(lm(Y.internal ~ M.internal + A.internal + C.internal))$r.squared,
              "alpha.a" = alpha.a, "alpha.c" = alpha.c,
              "beta.a" = beta.a, "beta.m" = beta.m, "beta.c" = beta.c,
              "sigma.e.sq" = sigma.e.sq, "Sigma.m" = mediator.cov,
              "n.external" = n.external, "T.hat.external" = T.hat.external,
              "var.T.hat.external" = var.T.hat.external,
              "T.hat.internal" = T.hat.internal,
              "var.T.hat.internal" = var.T.hat.internal,
              "R2.ext" = summary(ext.mod)$r.squared))
}

sim.data.null.ie <- function(n, p.con, p.mediators, rho.con.exp, rho.mediators,
                     	     r2.mediator, r2.outcome, total.effect.internal,
                     	     n.external.ratio, is.same.external, total.effect.external){

  #Generate internal data
  mediator.model.regressor.cov <- matrix(rho.con.exp, nrow = 1+p.con, ncol = 1+p.con)
  diag(mediator.model.regressor.cov) <- 1
  mediator.model.regressors <- mvrnorm(n = n, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
  colnames(mediator.model.regressors) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
  alpha.c.single <- 0.1
  alpha.c <- matrix(alpha.c.single, nrow = p.con, ncol = p.mediators)
  alpha.a.single <- 0
  alpha.a <- c(rep(alpha.a.single, 10), rep(0, 40))
  mediator.cor <- matrix(rho.mediators, nrow = p.mediators, ncol = p.mediators)
  mediator.cor[1:5,1:5] <- 0.3
  mediator.cor[6:10,6:10] <- 0.3
  mediator.cor[11:15,11:15] <- 0.3
  mediator.cor[16:50,16:50] <- 0.3
  diag(mediator.cor) <- 1
  #sigma.sq.mediator <- as.numeric(((1-r2.mediator)/r2.mediator)*matrix(c(alpha.a.single,rep(0.1, p.con)), nrow = 1)%*%mediator.model.regressor.cov%*%matrix(c(alpha.a.single,rep(0.1, p.con)), ncol = 1))
  #sigma.sq.mediator <- ((1-r2.mediator)/r2.mediator)*(alpha.a.single*mediator.model.regressor.cov[1,1]*alpha.a.single)
  sigma.sq.mediator <- ((1-r2.mediator)/r2.mediator)*(matrix(c(0,rep(alpha.c.single, p.con)), nrow = 1)%*%mediator.model.regressor.cov%*%matrix(c(0,rep(alpha.c.single, p.con)), ncol = 1))
  sigma.sq.mediator <- as.numeric(sigma.sq.mediator)
  mediator.cov <- diag(sqrt(sigma.sq.mediator), p.mediators)%*%mediator.cor%*%diag(sqrt(sigma.sq.mediator), p.mediators)
  M.internal <- matrix(mediator.model.regressors[, colnames(mediator.model.regressors) == "A"], ncol = 1)%*%matrix(alpha.a, nrow = 1) +
    mediator.model.regressors[, colnames(mediator.model.regressors) != "A"]%*%alpha.c +
    mvrnorm(n = n, rep(0,p.mediators), Sigma = mediator.cov)
  A.internal <- as.vector(mediator.model.regressors[, colnames(mediator.model.regressors) == "A"])
  C.internal <- mediator.model.regressors[, colnames(mediator.model.regressors) != "A"]

  beta.c <- rep(0.1, p.con)
  beta.m.single <- 0
  beta.m <- c(rep(beta.m.single, 5), rep(0, 5), rep(beta.m.single, 5), rep(0, 35))
  beta.a <- total.effect.internal - sum(alpha.a*beta.m)
  if(beta.a < 0){
    stop('Total effect is negative!')
  }
  outcome.cov <- matrix(0, nrow = p.mediators + 1 + p.con, ncol = p.mediators + 1 + p.con)
  outcome.cov[1:p.mediators, 1:p.mediators] <- mediator.cov + t(as.matrix(rbind(alpha.a, alpha.c)))%*%mediator.model.regressor.cov%*%as.matrix(rbind(alpha.a, alpha.c))
  outcome.cov[(1+p.mediators):(p.mediators+1+p.con), (1+p.mediators):(p.mediators+1+p.con)] <- mediator.model.regressor.cov
  outcome.cov[1:p.mediators, (1+p.mediators):(p.mediators+1+p.con)] <- t(as.matrix(rbind(alpha.a, alpha.c)))%*%mediator.model.regressor.cov
  outcome.cov[(1+p.mediators):(p.mediators+1+p.con), 1:p.mediators] <- t(outcome.cov[1:p.mediators, (1+p.mediators):(p.mediators+1+p.con)])
  #Can show this using law of total variance and reconstructing joint multivariate normal distribution from conditional and marginal
  sigma.e.sq <- ((1-r2.outcome)/r2.outcome)*(matrix(c(beta.m,beta.a,beta.c), nrow = 1)%*%outcome.cov%*%matrix(c(beta.m,beta.a,beta.c), ncol = 1))
  #sigma.e.sq <- ((1-r2.outcome)/r2.outcome)*(matrix(beta.m, nrow = 1)%*%outcome.cov[1:p.mediators, 1:p.mediators]%*%matrix(beta.m, ncol = 1))
  Y.internal <- as.vector(cbind(M.internal,mediator.model.regressors)%*%matrix(c(beta.m,beta.a,beta.c), ncol = 1)) + rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq))
  
  int.mod <- lm(Y.internal ~ A.internal + C.internal)
  T.hat.internal <- coef(int.mod)[names(coef(int.mod)) == "A.internal"]
  var.T.hat.internal <- vcov(int.mod)[row.names(vcov(int.mod)) == "A.internal", colnames(vcov(int.mod)) == "A.internal"]
  
  #Generate external data
  if(is.same.external == TRUE){
    n.external <- n.external.ratio*n
    mediator.model.regressors.ext <- mvrnorm(n = n.external, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
    colnames(mediator.model.regressors.ext) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
    sigma.ext.sq <- sigma.e.sq + as.numeric(matrix(beta.m, nrow = 1)%*%mediator.cov%*%matrix(beta.m, ncol = 1))
    total.effect.external <- total.effect.internal
    external.con.coeffs <- (alpha.c%*%matrix(beta.m, ncol = 1))
    A.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) == "A"]
    C.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) != "A"]
    Y.external <- A.external*total.effect.external + as.vector(C.external%*%external.con.coeffs) + rnorm(n = n.external, 0, sqrt(sigma.ext.sq))
    ext.mod <- lm(Y.external ~ A.external + C.external)
    T.hat.external <- coef(ext.mod)[names(coef(ext.mod)) == "A.external"]
    var.T.hat.external <- vcov(ext.mod)[row.names(vcov(ext.mod)) == "A.external", colnames(vcov(ext.mod)) == "A.external"]
  } else {
    n.external <- n.external.ratio*n
    mediator.model.regressors.ext <- mvrnorm(n = n.external, mu = rep(0,1+p.con), Sigma = mediator.model.regressor.cov)
    colnames(mediator.model.regressors.ext) <- c("A","C1.m","C2.m","C3.m","C4.m","C5.m")
    sigma.ext.sq <- sigma.e.sq + as.numeric(matrix(beta.m, nrow = 1)%*%mediator.cov%*%matrix(beta.m, ncol = 1))
    external.con.coeffs <- (alpha.c%*%matrix(beta.m, ncol = 1))
    A.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) == "A"]
    C.external <- mediator.model.regressors.ext[, colnames(mediator.model.regressors.ext) != "A"]
    Y.external <- A.external*total.effect.external + as.vector(C.external%*%external.con.coeffs) + rnorm(n = n.external, 0, sqrt(sigma.ext.sq))
    ext.mod <- lm(Y.external ~ A.external + C.external)
    T.hat.external <- coef(ext.mod)[names(coef(ext.mod)) == "A.external"]
    var.T.hat.external <- vcov(ext.mod)[row.names(vcov(ext.mod)) == "A.external", colnames(vcov(ext.mod)) == "A.external"]
  }

  return(list("Y" = Y.internal, "M" = M.internal, "A" = A.internal, "C" = C.internal,
              "R2.m" = summary(lm(M.internal[,1] ~ A.internal + C.internal))$r.squared,
              "R2.o" = summary(lm(Y.internal ~ M.internal + A.internal + C.internal))$r.squared,
              "alpha.a" = alpha.a, "alpha.c" = alpha.c,
              "beta.a" = beta.a, "beta.m" = beta.m, "beta.c" = beta.c,
              "sigma.e.sq" = sigma.e.sq, "Sigma.m" = mediator.cov,
              "n.external" = n.external, "T.hat.external" = T.hat.external,
              "var.T.hat.external" = var.T.hat.external,
              "T.hat.internal" = T.hat.internal,
              "var.T.hat.internal" = var.T.hat.internal,
              "R2.ext" = summary(ext.mod)$r.squared))
}

calc.bic <- function(Y, M, A, C, T.hat = NULL, alpha.c.hat, alpha.a.hat, beta.a.hat = NULL, beta.c.hat, beta.m.hat, sigma.e.sq.hat, Sigma.m.hat.inv, rand.eff.var = NULL){
  n <- length(Y)
  if(!is.null(beta.a.hat)){
    Y.hat <- Y - M%*%matrix(beta.m.hat, ncol = 1) - A*beta.a.hat - cbind(1,C)%*%matrix(beta.c.hat, ncol = 1)
    M.hat <- matrix(A, ncol = 1)%*%matrix(alpha.a.hat, nrow = 1) - cbind(1,C)%*%t(alpha.c.hat)
    num.nonzero <- nrow(alpha.c.hat)*ncol(alpha.c.hat) + sum(as.numeric(alpha.a.hat != 0)) + length(beta.c.hat) + 1 + sum(as.numeric(beta.m.hat != 0))
    if(!is.null(rand.eff.var)){
      df <- num.nonzero - (1 - (sum(A^2)/sigma.e.sq.hat)/((sum(A^2)/sigma.e.sq.hat)+1/rand.eff.var)) #Adjust dfs for ridge penalty on total effect model
    } else if(is.null(rand.eff.var)){
      df <- num.nonzero
    }
  } else if(is.null(beta.a.hat)){
    Y.hat <- Y - M%*%matrix(beta.m.hat, ncol = 1) - A*(T.hat - sum(alpha.a.hat*beta.m.hat)) - cbind(1,C)%*%matrix(beta.c.hat, ncol = 1)
    M.hat <- matrix(A, ncol = 1)%*%matrix(alpha.a.hat, nrow = 1) - cbind(1,C)%*%t(alpha.c.hat)
    num.nonzero <- nrow(alpha.c.hat)*ncol(alpha.c.hat) + sum(as.numeric(alpha.a.hat != 0)) + length(beta.c.hat) + sum(as.numeric(beta.m.hat != 0))
    df <- num.nonzero
  }
  med.contribution <- 0
  for(i in 1:n){
    med.contribution <- med.contribution + matrix(M[i,] - M.hat[i,], nrow = 1)%*%Sigma.m.hat.inv%*%matrix(M[i,] - M.hat[i,], ncol = 1)
  }
  
  return(sum((Y - Y.hat)^2)/sigma.e.sq.hat + med.contribution + log(n)*df)
}

tune.bic <- function(Y, M, A, C, T.hat.external, var.T.hat.external, s2 = NULL, method = "robust", nlambda = 30,
                     gamma.alpha = 2, gamma.beta = 2, err.tol.out = 1e-08, err.tol.med = 1e-08, max.itr = 10000){
  
  unpenalized_mle <- unconstrained.unpenalized(Y = Y, M = M, A = A, C = C)
  adpt.w.beta.m <- 1/abs(unpenalized_mle$beta.m.hat)^(gamma.beta)
  adpt.w.alpha.a <- 1/abs(unpenalized_mle$alpha.a.hat)^(gamma.alpha)
  
  lambda.beta.max <- max(abs(colSums(apply(M, 2, function(x){x*Y}))))/mean(adpt.w.beta.m)
  lambda.alpha.max <- max(abs(colSums(apply(M, 2, function(x){x*A}))))/mean(adpt.w.alpha.a)
  lambda.alpha.grid <- seq(from = 0, to = lambda.alpha.max, length.out = nlambda)
  lambda.beta.grid <- seq(from = 0, to = lambda.beta.max, length.out = nlambda)
  
  lambda.grid <- expand.grid(lambda.alpha.grid, lambda.beta.grid)
  bic.store <- rep(NA, nrow(lambda.grid))
  if(method == "robust"){
    for(l in 1:nrow(lambda.grid)){
      fit.bic <- soft.penalized(Y = Y, M = M, A = A, C = C, T.hat.external = T.hat.external,
                                  var.T.hat.external = var.T.hat.external, s2 = s2,
                                  lambda.alpha = lambda.grid[l,1], lambda.beta = lambda.grid[l,2])
      bic.store[l] <- calc.bic(Y = Y, M = M, A = A, C = C, T.hat = T.hat.external, alpha.c.hat = fit.bic$alpha.c.hat,
                               alpha.a.hat = fit.bic$alpha.a.hat, beta.a.hat = fit.bic$beta.a.hat,
                               beta.c.hat = fit.bic$beta.c.hat, beta.m.hat = fit.bic$beta.m.hat,
                               sigma.e.sq.hat = fit.bic$sigma.e.sq.hat, Sigma.m.hat.inv = solve(fit.bic$Sigma.m.hat),
                               rand.eff.var = s2*var.T.hat.external)
    }
  } else if(method == "unconstrained"){
    for(l in 1:nrow(lambda.grid)){
      fit.bic <- unconstrained.penalized(Y = Y, M = M, A = A, C = C,
                                         lambda.alpha = lambda.grid[l,1], lambda.beta = lambda.grid[l,2])
      bic.store[l] <- calc.bic(Y = Y, M = M, A = A, C = C, T.hat = NULL, alpha.c.hat = fit.bic$alpha.c.hat,
                               alpha.a.hat = fit.bic$alpha.a.hat, beta.a.hat = fit.bic$beta.a.hat,
                               beta.c.hat = fit.bic$beta.c.hat, beta.m.hat = fit.bic$beta.m.hat,
                               sigma.e.sq.hat = fit.bic$sigma.e.sq.hat, Sigma.m.hat.inv = solve(fit.bic$Sigma.m.hat),
                               rand.eff.var = NULL)
    }
  } else if(method == "hard"){
    for(l in 1:nrow(lambda.grid)){
      fit.bic <- constrained.penalized(Y = Y, M = M, A = A, C = C, T.hat.external = T.hat.external,
                                       lambda.alpha = lambda.grid[l,1], lambda.beta = lambda.grid[l,2])
      bic.store[l] <- calc.bic(Y = Y, M = M, A = A, C = C, T.hat = T.hat.external, alpha.c.hat = fit.bic$alpha.c.hat,
                               alpha.a.hat = fit.bic$alpha.a.hat, beta.a.hat = NULL,
                               beta.c.hat = fit.bic$beta.c.hat, beta.m.hat = fit.bic$beta.m.hat,
                               sigma.e.sq.hat = fit.bic$sigma.e.sq.hat, Sigma.m.hat.inv = solve(fit.bic$Sigma.m.hat),
                               rand.eff.var = NULL)
    }
  }
  lambda.hat.bic <- lambda.grid[which.min(bic.store),]
  return(list("grid" = lambda.grid, "bic" = bic.store, "lambda.alpha.hat.bic" = lambda.hat.bic[1], "lambda.beta.hat.bic" = lambda.hat.bic[2]))
}

