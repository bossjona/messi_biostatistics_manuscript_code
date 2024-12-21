setwd("/filepath/")

source("implementations.R")

#Get the arrayid from slurm
arrayid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
arrayid <- ifelse(is.na(arrayid), 1, arrayid)

print(arrayid)

#############################################
## Simulate Data
#############################################

setwd("/filepath/simulations_incorrect_random/n2000_next200000/high_r2_mediator_high_r2_outcome/")

#Simulate mediation data
set.seed(20220222 + arrayid)
n <- 2000
p.con <- 5
p.mediators <- 50
rho.con.exp <- 0.2
rho.mediators <- 0.2
r2.mediator <- 0.2
total.effect.internal <- 1
r2.outcome <- 0.8
n.external.ratio <- 100
is.same.external <- FALSE
total.effect.external <- ifelse(runif(1)<0.8, total.effect.internal, rnorm(n = 1, mean = total.effect.internal, sd = sqrt(0.2)))
sim.dat <- sim.data(n = n, p.con = p.con, p.mediators = p.mediators, rho.con.exp = rho.con.exp,
                    rho.mediators = rho.mediators, r2.mediator = r2.mediator, r2.outcome = r2.outcome,
                    total.effect.internal = total.effect.internal,
                    n.external.ratio = n.external.ratio, is.same.external = is.same.external,
                    total.effect.external = total.effect.external)

r2.m.true <- sim.dat$R2.m
r2.o.true <- sim.dat$R2.o
alpha.a.true <- sim.dat$alpha.a
alpha.c.true <- sim.dat$alpha.c
Sigma.m.true <- sim.dat$Sigma.m
beta.m.true <- sim.dat$beta.m
beta.a.true <- sim.dat$beta.a
beta.c.true <- sim.dat$beta.c
sigma.e.sq.true <- sim.dat$sigma.e.sq
T.hat.internal <- sim.dat$T.hat.internal
var.T.hat.internal <- sim.dat$var.T.hat.internal
T.hat.external <- sim.dat$T.hat.external
var.T.hat.external <- sim.dat$var.T.hat.external
R2.ext.true <- sim.dat$R2.ext
AtA <- sum((lm(sim.dat$A ~ sim.dat$C)$residuals)^2)

#############################################
## Unconstrained - Unpenalized
#############################################

final.fit <- unconstrained.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C)

alpha.a.hat.u.no.pen <- final.fit$alpha.a.hat
alpha.c.hat.u.no.pen <- final.fit$alpha.c.hat
beta.m.hat.u.no.pen <- final.fit$beta.m.hat
beta.a.hat.u.no.pen <- final.fit$beta.a.hat
beta.c.hat.u.no.pen <- final.fit$beta.c.hat

Sigma.m.hat.u <- final.fit$Sigma.m.hat
sigma.e.sq.hat.u <- final.fit$sigma.e.sq.hat

print("Unconstrained - Unpenalized Done")

#############################################
## Hard Constraint (Oracle) - Unpenalized
#############################################

plug.in.est <- beta.a.true + sum(alpha.a.true*beta.m.true)

final.fit <- constrained.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, T.hat.external = plug.in.est)

alpha.a.hat.o.no.pen <- final.fit$alpha.a.hat
alpha.c.hat.o.no.pen <- final.fit$alpha.c.hat
beta.m.hat.o.no.pen <- final.fit$beta.m.hat
beta.a.hat.o.no.pen <- plug.in.est - sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
beta.c.hat.o.no.pen <- final.fit$beta.c.hat

Sigma.m.hat.o <- final.fit$Sigma.m.hat
sigma.e.sq.hat.o <- final.fit$sigma.e.sq.hat

print("Hard Constraint (Oracle) - Unpenalized Done")

#############################################
## Hard Constraint (External Estimate) - Unpenalized
#############################################

plug.in.est <- T.hat.external

final.fit <- constrained.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, T.hat.external = plug.in.est)

alpha.a.hat.h.ext.no.pen <- final.fit$alpha.a.hat
alpha.c.hat.h.ext.no.pen <- final.fit$alpha.c.hat
beta.m.hat.h.ext.no.pen <- final.fit$beta.m.hat
beta.a.hat.h.ext.no.pen <- plug.in.est - sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
beta.c.hat.h.ext.no.pen <- final.fit$beta.c.hat

Sigma.m.hat.h.ext <- final.fit$Sigma.m.hat
sigma.e.sq.hat.h.ext <- final.fit$sigma.e.sq.hat

print("Hard Constraint (External Estimate) - Unpenalized Done")

#############################################
## Soft Constraint (Optimal MSE) - Unpenalized
#############################################

s2.hat <- choose_s2(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, T.hat.internal = T.hat.internal, var.T.hat.internal = var.T.hat.internal, T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external, method = "minimum_mse")
final.fit <- rand.eff.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.hat*var.T.hat.external,
                                  T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)
s2.hat.sc.opt.mse.no.pen <- s2.hat

alpha.a.hat.sc.opt.mse.no.pen <- final.fit$alpha.a.hat
alpha.c.hat.sc.opt.mse.no.pen <- final.fit$alpha.c.hat
beta.m.hat.sc.opt.mse.no.pen <- final.fit$beta.m.hat
beta.a.hat.sc.opt.mse.no.pen <- final.fit$beta.a.hat
beta.c.hat.sc.opt.mse.no.pen <- final.fit$beta.c.hat

Sigma.m.hat.sc.opt.mse <- final.fit$Sigma.m.hat
sigma.e.sq.hat.sc.opt.mse <- final.fit$sigma.e.sq.hat

n.boot <- 200
s2.boot <- rep(NA, n.boot)
nie.boot <- rep(NA, n.boot)
nde.boot <- rep(NA, n.boot)
te.boot <- rep(NA, n.boot)
save.boot <- rep(NA, n.boot)
for(r in 1:n.boot){
  print(r)
  epsilon.y <- rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq.hat.sc.opt.mse))
  gen.M <- cbind(1,sim.dat$C)%*%t(alpha.c.hat.sc.opt.mse.no.pen) + matrix(sim.dat$A, ncol = 1)%*%matrix(alpha.a.hat.sc.opt.mse.no.pen, nrow = 1) + mvrnorm(n = n, mu = rep(0, p.mediators), Sigma = Sigma.m.hat.sc.opt.mse)
  gen.Y <- sim.dat$A*beta.a.hat.sc.opt.mse.no.pen + as.vector(cbind(1, sim.dat$C)%*%beta.c.hat.sc.opt.mse.no.pen) + as.vector(gen.M%*%beta.m.hat.sc.opt.mse.no.pen) + epsilon.y
  boot.mod <- lm(gen.Y ~ sim.dat$A + sim.dat$C)
  T.hat.internal.boot <- coef(boot.mod)[names(coef(boot.mod)) == "sim.dat$A"]
  var.T.hat.internal.boot <- vcov(boot.mod)[names(coef(boot.mod)) == "sim.dat$A", names(coef(boot.mod)) == "sim.dat$A"]
  s2.hat.boot <- choose_s2(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C, T.hat.internal = T.hat.internal.boot, var.T.hat.internal = var.T.hat.internal.boot, T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external, method = "minimum_mse")
  final.fit <- rand.eff.unpenalized(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.hat.boot*var.T.hat.external,
                                    T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)
  
  s2.boot[r] <- s2.hat.boot
  nde.boot[r] <- final.fit$beta.a.hat
  nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
  te.boot[r] <- nde.boot[r] + nie.boot[r]
}

delta.nde <- quantile(nde.boot - beta.a.hat.sc.opt.mse.no.pen, probs = c(0.025,0.975))
nde.ci95.param.boot.quantile.sc.opt.mse.no.pen <- c(beta.a.hat.sc.opt.mse.no.pen - delta.nde[2], beta.a.hat.sc.opt.mse.no.pen - delta.nde[1])
nde.ci95.param.boot.asym.norm.sc.opt.mse.no.pen <- c(beta.a.hat.sc.opt.mse.no.pen - 1.96*sqrt(var(nde.boot)), beta.a.hat.sc.opt.mse.no.pen + 1.96*sqrt(var(nde.boot)))

delta.nie <- quantile(nie.boot - sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen), probs = c(0.025,0.975))
nie.ci95.param.boot.quantile.sc.opt.mse.no.pen <- c(sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen) - delta.nie[2], sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen) - delta.nie[1])
nie.ci95.param.boot.asym.norm.sc.opt.mse.no.pen <- c(sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen) - 1.96*sqrt(var(nie.boot)), sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen) + 1.96*sqrt(var(nie.boot)))

delta.te <- quantile(te.boot - (beta.a.hat.sc.opt.mse.no.pen + sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen)), probs = c(0.025,0.975))
te.ci95.param.boot.quantile.sc.opt.mse.no.pen <- c((beta.a.hat.sc.opt.mse.no.pen + sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen)) - delta.te[2], (beta.a.hat.sc.opt.mse.no.pen + sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen)) - delta.te[1])
te.ci95.param.boot.asym.norm.sc.opt.mse.no.pen <- c((beta.a.hat.sc.opt.mse.no.pen + sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen)) - 1.96*sqrt(var(te.boot)), (beta.a.hat.sc.opt.mse.no.pen + sum(alpha.a.hat.sc.opt.mse.no.pen*beta.m.hat.sc.opt.mse.no.pen)) + 1.96*sqrt(var(te.boot)))

print("Soft Constraint (Optimal MSE) - Unpenalized Done")

#############################################
## Soft Constraint (EB) - Unpenalized
#############################################

s2.hat <- choose_s2(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, T.hat.internal = T.hat.internal, var.T.hat.internal = var.T.hat.internal, T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external, method = "eb")
if(s2.hat != 0){
  final.fit <- rand.eff.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.hat*var.T.hat.external,
                                  T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)
  beta.a.hat.sc.eb.no.pen <- final.fit$beta.a.hat
} else if(s2.hat == 0){
  s2.threshold <- 0.000001
  final.fit <- rand.eff.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.threshold*var.T.hat.external,
                                  T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)
  beta.a.hat.sc.eb.no.pen <- final.fit$beta.a.hat
}

s2.hat.sc.eb.no.pen <- s2.hat

alpha.a.hat.sc.eb.no.pen <- final.fit$alpha.a.hat
alpha.c.hat.sc.eb.no.pen <- final.fit$alpha.c.hat
beta.m.hat.sc.eb.no.pen <- final.fit$beta.m.hat
beta.c.hat.sc.eb.no.pen <- final.fit$beta.c.hat

Sigma.m.hat.sc.eb <- final.fit$Sigma.m.hat
sigma.e.sq.hat.sc.eb <- final.fit$sigma.e.sq.hat

n.boot <- 200
s2.boot <- rep(NA, n.boot)
nie.boot <- rep(NA, n.boot)
nde.boot <- rep(NA, n.boot)
te.boot <- rep(NA, n.boot)
save.boot <- rep(NA, n.boot)
for(r in 1:n.boot){
  print(r)
  epsilon.y <- rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq.hat.sc.eb))
  gen.M <- cbind(1,sim.dat$C)%*%t(alpha.c.hat.sc.eb.no.pen) + matrix(sim.dat$A, ncol = 1)%*%matrix(alpha.a.hat.sc.eb.no.pen, nrow = 1) + mvrnorm(n = n, mu = rep(0, p.mediators), Sigma = Sigma.m.hat.sc.eb)
  gen.Y <- sim.dat$A*beta.a.hat.sc.eb.no.pen + as.vector(cbind(1, sim.dat$C)%*%beta.c.hat.sc.eb.no.pen) + as.vector(gen.M%*%beta.m.hat.sc.eb.no.pen) + epsilon.y
  boot.mod <- lm(gen.Y ~ sim.dat$A + sim.dat$C)
  T.hat.internal.boot <- coef(boot.mod)[names(coef(boot.mod)) == "sim.dat$A"]
  var.T.hat.internal.boot <- vcov(boot.mod)[names(coef(boot.mod)) == "sim.dat$A", names(coef(boot.mod)) == "sim.dat$A"]
  s2.hat.boot <- choose_s2(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C, T.hat.internal = T.hat.internal.boot, var.T.hat.internal = var.T.hat.internal.boot, T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external, method = "eb")
  if(s2.hat.boot != 0){
    final.fit <- rand.eff.unpenalized(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.hat.boot*var.T.hat.external,
                                    T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)
    nde.boot[r] <- final.fit$beta.a.hat
  } else if(s2.hat.boot == 0){
    #final.fit <- constrained.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, T.hat.external = T.hat.external)
    #nde.boot[r] <- T.hat.external - sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
    
    s2.threshold <- 0.000001
    final.fit <- rand.eff.unpenalized(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.threshold*var.T.hat.external,
                                    T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)
    nde.boot[r] <- final.fit$beta.a.hat
  }
  
  s2.boot[r] <- s2.hat.boot
  nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
  te.boot[r] <- nde.boot[r] + nie.boot[r]
}

delta.nde <- quantile(nde.boot - beta.a.hat.sc.eb.no.pen, probs = c(0.025,0.975))
nde.ci95.param.boot.quantile.sc.eb.no.pen <- c(beta.a.hat.sc.eb.no.pen - delta.nde[2], beta.a.hat.sc.eb.no.pen - delta.nde[1])
nde.ci95.param.boot.asym.norm.sc.eb.no.pen <- c(beta.a.hat.sc.eb.no.pen - 1.96*sqrt(var(nde.boot)), beta.a.hat.sc.eb.no.pen + 1.96*sqrt(var(nde.boot)))

delta.nie <- quantile(nie.boot - sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen), probs = c(0.025,0.975))
nie.ci95.param.boot.quantile.sc.eb.no.pen <- c(sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen) - delta.nie[2], sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen) - delta.nie[1])
nie.ci95.param.boot.asym.norm.sc.eb.no.pen <- c(sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen) - 1.96*sqrt(var(nie.boot)), sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen) + 1.96*sqrt(var(nie.boot)))

delta.te <- quantile(te.boot - (beta.a.hat.sc.eb.no.pen + sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen)), probs = c(0.025,0.975))
te.ci95.param.boot.quantile.sc.eb.no.pen <- c((beta.a.hat.sc.eb.no.pen + sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen)) - delta.te[2], (beta.a.hat.sc.eb.no.pen + sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen)) - delta.te[1])
te.ci95.param.boot.asym.norm.sc.eb.no.pen <- c((beta.a.hat.sc.eb.no.pen + sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen)) - 1.96*sqrt(var(te.boot)), (beta.a.hat.sc.eb.no.pen + sum(alpha.a.hat.sc.eb.no.pen*beta.m.hat.sc.eb.no.pen)) + 1.96*sqrt(var(te.boot)))

print("Soft Constraint (EB) - Unpenalized Done")

#############################################
## Soft Constraint (Discrete) - Unpenalized
#############################################

temp1 <- ((sigma.e.sq.hat.u + matrix(beta.m.hat.u.no.pen, nrow = 1)%*%Sigma.m.hat.u%*%matrix(beta.m.hat.u.no.pen, ncol = 1))/(AtA/n))/n
temp2 <- (T.hat.internal - T.hat.external)^2 - temp1
if(temp1 < temp2){
  final.fit <- unconstrained.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C)
  
  alpha.a.hat.d.no.pen <- final.fit$alpha.a.hat
  alpha.c.hat.d.no.pen <- final.fit$alpha.c.hat
  beta.m.hat.d.no.pen <- final.fit$beta.m.hat
  beta.a.hat.d.no.pen <- final.fit$beta.a.hat
  beta.c.hat.d.no.pen <- final.fit$beta.c.hat
  
  Sigma.m.hat.d <- final.fit$Sigma.m.hat
  sigma.e.sq.hat.d <- final.fit$sigma.e.sq.hat
  
  choose_eb <- "indicator_zero"
} else if(temp1 >= temp2){
  plug.in.est <- T.hat.external
  
  final.fit <- constrained.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, T.hat.external = plug.in.est)
  
  alpha.a.hat.d.no.pen <- final.fit$alpha.a.hat
  alpha.c.hat.d.no.pen <- final.fit$alpha.c.hat
  beta.m.hat.d.no.pen <- final.fit$beta.m.hat
  beta.a.hat.d.no.pen <- plug.in.est - sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
  beta.c.hat.d.no.pen <- final.fit$beta.c.hat
  
  Sigma.m.hat.d <- final.fit$Sigma.m.hat
  sigma.e.sq.hat.d <- final.fit$sigma.e.sq.hat
  
  choose_eb <- "indicator_nonzero"
}

n.boot <- 200
nie.boot <- rep(NA, n.boot)
nde.boot <- rep(NA, n.boot)
te.boot <- rep(NA, n.boot)
save.boot <- rep(NA, n.boot)
for(r in 1:n.boot){
  print(r)
  
  epsilon.y <- rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq.hat.d))
  gen.M <- cbind(1,sim.dat$C)%*%t(alpha.c.hat.d.no.pen) + matrix(sim.dat$A, ncol = 1)%*%matrix(alpha.a.hat.d.no.pen, nrow = 1) + mvrnorm(n = n, mu = rep(0, p.mediators), Sigma = Sigma.m.hat.d)
  gen.Y <- sim.dat$A*beta.a.hat.d.no.pen + as.vector(cbind(1, sim.dat$C)%*%beta.c.hat.d.no.pen) + as.vector(gen.M%*%beta.m.hat.d.no.pen) + epsilon.y
  boot.mod <- lm(gen.Y ~ sim.dat$A + sim.dat$C)
  T.hat.internal.boot <- coef(boot.mod)[names(coef(boot.mod)) == "sim.dat$A"]
  var.T.hat.internal.boot <- vcov(boot.mod)[names(coef(boot.mod)) == "sim.dat$A", names(coef(boot.mod)) == "sim.dat$A"]
  
  init.fit.boot <- unconstrained.unpenalized(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C)
  beta.m.hat.u.no.pen.boot <- init.fit.boot$beta.m.hat
  Sigma.m.hat.u.boot <- init.fit.boot$Sigma.m.hat
  sigma.e.sq.hat.u.boot <- init.fit.boot$sigma.e.sq.hat
  
  temp1 <- ((sigma.e.sq.hat.u.boot + matrix(beta.m.hat.u.no.pen.boot, nrow = 1)%*%Sigma.m.hat.u.boot%*%matrix(beta.m.hat.u.no.pen.boot, ncol = 1))/(AtA/n))/n
  temp2 <- max(0, (T.hat.internal.boot - T.hat.external)^2 - (var.T.hat.external + temp1)) + var.T.hat.external
  if(temp1 < temp2){
    final.fit <- unconstrained.unpenalized(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C)
    nde.boot[r] <- final.fit$beta.a.hat
    nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
    te.boot[r] <- nde.boot[r] + nie.boot[r]
  } else if(temp1 >= temp2){
    plug.in.est <- T.hat.external
    final.fit <- constrained.unpenalized(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C, T.hat.external = plug.in.est)
    nde.boot[r] <- plug.in.est - sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
    nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
    te.boot[r] <- plug.in.est
  }
}

delta.nde <- quantile(nde.boot - beta.a.hat.d.no.pen, probs = c(0.025,0.975))
nde.ci95.param.boot.quantile.d.no.pen <- c(beta.a.hat.d.no.pen - delta.nde[2], beta.a.hat.d.no.pen - delta.nde[1])
nde.ci95.param.boot.asym.norm.d.no.pen <- c(beta.a.hat.d.no.pen - 1.96*sqrt(var(nde.boot)), beta.a.hat.d.no.pen + 1.96*sqrt(var(nde.boot)))

delta.nie <- quantile(nie.boot - sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen), probs = c(0.025,0.975))
nie.ci95.param.boot.quantile.d.no.pen <- c(sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen) - delta.nie[2], sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen) - delta.nie[1])
nie.ci95.param.boot.asym.norm.d.no.pen <- c(sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen) - 1.96*sqrt(var(nie.boot)), sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen) + 1.96*sqrt(var(nie.boot)))

delta.te <- quantile(te.boot - (beta.a.hat.d.no.pen + sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen)), probs = c(0.025,0.975))
te.ci95.param.boot.quantile.d.no.pen <- c((beta.a.hat.d.no.pen + sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen)) - delta.te[2], (beta.a.hat.d.no.pen + sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen)) - delta.te[1])
te.ci95.param.boot.asym.norm.d.no.pen <- c((beta.a.hat.d.no.pen + sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen)) - 1.96*sqrt(var(te.boot)), (beta.a.hat.d.no.pen + sum(alpha.a.hat.d.no.pen*beta.m.hat.d.no.pen)) + 1.96*sqrt(var(te.boot)))

print("Soft Constraint (Discrete) - Unpenalized Done")

#############################################
## Soft Constraint (s^2 = 0.0001) - Unpenalized
#############################################

s2.fixed <- 0.0001

final.fit <- rand.eff.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.fixed*var.T.hat.external,
                                  T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)

alpha.a.hat.sc.0.no.pen <- final.fit$alpha.a.hat
alpha.c.hat.sc.0.no.pen <- final.fit$alpha.c.hat
beta.m.hat.sc.0.no.pen <- final.fit$beta.m.hat
beta.a.hat.sc.0.no.pen <- final.fit$beta.a.hat
beta.c.hat.sc.0.no.pen <- final.fit$beta.c.hat

Sigma.m.hat.sc.0 <- final.fit$Sigma.m.hat
sigma.e.sq.hat.sc.0 <- final.fit$sigma.e.sq.hat

n.boot <- 200
nie.boot <- rep(NA, n.boot)
nde.boot <- rep(NA, n.boot)
te.boot <- rep(NA, n.boot)
save.boot <- rep(NA, n.boot)
for(r in 1:n.boot){
  print(r)
  epsilon.y <- rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq.hat.sc.0))
  gen.M <- cbind(1,sim.dat$C)%*%t(alpha.c.hat.sc.0.no.pen) + matrix(sim.dat$A, ncol = 1)%*%matrix(alpha.a.hat.sc.0.no.pen, nrow = 1) + mvrnorm(n = n, mu = rep(0, p.mediators), Sigma = Sigma.m.hat.sc.0)
  gen.Y <- sim.dat$A*beta.a.hat.sc.0.no.pen + as.vector(cbind(1, sim.dat$C)%*%beta.c.hat.sc.0.no.pen) + as.vector(gen.M%*%beta.m.hat.sc.0.no.pen) + epsilon.y
  boot.mod <- lm(gen.Y ~ sim.dat$A + sim.dat$C)
  T.hat.internal.boot <- coef(boot.mod)[names(coef(boot.mod)) == "sim.dat$A"]
  final.fit <- rand.eff.unpenalized(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.fixed*var.T.hat.external,
                                    T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)
  
  nde.boot[r] <- final.fit$beta.a.hat
  nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
  te.boot[r] <- nde.boot[r] + nie.boot[r]
}

delta.nde <- quantile(nde.boot - beta.a.hat.sc.0.no.pen, probs = c(0.025,0.975))
nde.ci95.param.boot.quantile.sc.0.no.pen <- c(beta.a.hat.sc.0.no.pen - delta.nde[2], beta.a.hat.sc.0.no.pen - delta.nde[1])
nde.ci95.param.boot.asym.norm.sc.0.no.pen <- c(beta.a.hat.sc.0.no.pen - 1.96*sqrt(var(nde.boot)), beta.a.hat.sc.0.no.pen + 1.96*sqrt(var(nde.boot)))

delta.nie <- quantile(nie.boot - sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen), probs = c(0.025,0.975))
nie.ci95.param.boot.quantile.sc.0.no.pen <- c(sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen) - delta.nie[2], sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen) - delta.nie[1])
nie.ci95.param.boot.asym.norm.sc.0.no.pen <- c(sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen) - 1.96*sqrt(var(nie.boot)), sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen) + 1.96*sqrt(var(nie.boot)))

delta.te <- quantile(te.boot - (beta.a.hat.sc.0.no.pen + sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen)), probs = c(0.025,0.975))
te.ci95.param.boot.quantile.sc.0.no.pen <- c((beta.a.hat.sc.0.no.pen + sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen)) - delta.te[2], (beta.a.hat.sc.0.no.pen + sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen)) - delta.te[1])
te.ci95.param.boot.asym.norm.sc.0.no.pen <- c((beta.a.hat.sc.0.no.pen + sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen)) - 1.96*sqrt(var(te.boot)), (beta.a.hat.sc.0.no.pen + sum(alpha.a.hat.sc.0.no.pen*beta.m.hat.sc.0.no.pen)) + 1.96*sqrt(var(te.boot)))

print("Soft Constraint (s^2 = 0.0001) - Unpenalized Done")

#############################################
## Soft Constraint (s^2 = 10000000000) - Unpenalized
#############################################

s2.fixed <- 10000000000

final.fit <- rand.eff.unpenalized(Y = sim.dat$Y, M = sim.dat$M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.fixed*var.T.hat.external,
                                  T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)

alpha.a.hat.sc.inf.no.pen <- final.fit$alpha.a.hat
alpha.c.hat.sc.inf.no.pen <- final.fit$alpha.c.hat
beta.m.hat.sc.inf.no.pen <- final.fit$beta.m.hat
beta.a.hat.sc.inf.no.pen <- final.fit$beta.a.hat
beta.c.hat.sc.inf.no.pen <- final.fit$beta.c.hat

Sigma.m.hat.sc.inf <- final.fit$Sigma.m.hat
sigma.e.sq.hat.sc.inf <- final.fit$sigma.e.sq.hat

n.boot <- 200
nie.boot <- rep(NA, n.boot)
nde.boot <- rep(NA, n.boot)
te.boot <- rep(NA, n.boot)
save.boot <- rep(NA, n.boot)
for(r in 1:n.boot){
  print(r)
  epsilon.y <- rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq.hat.sc.inf))
  gen.M <- cbind(1,sim.dat$C)%*%t(alpha.c.hat.sc.inf.no.pen) + matrix(sim.dat$A, ncol = 1)%*%matrix(alpha.a.hat.sc.inf.no.pen, nrow = 1) + mvrnorm(n = n, mu = rep(0, p.mediators), Sigma = Sigma.m.hat.sc.inf)
  gen.Y <- sim.dat$A*beta.a.hat.sc.inf.no.pen + as.vector(cbind(1, sim.dat$C)%*%beta.c.hat.sc.inf.no.pen) + as.vector(gen.M%*%beta.m.hat.sc.inf.no.pen) + epsilon.y
  boot.mod <- lm(gen.Y ~ sim.dat$A + sim.dat$C)
  T.hat.internal.boot <- coef(boot.mod)[names(coef(boot.mod)) == "sim.dat$A"]
  final.fit <- rand.eff.unpenalized(Y = gen.Y, M = gen.M, A = sim.dat$A, C = sim.dat$C, rand.eff.mean = T.hat.external, rand.eff.var = s2.fixed*var.T.hat.external,
                                    T.hat.external = T.hat.external, var.T.hat.external = var.T.hat.external)
  
  nde.boot[r] <- final.fit$beta.a.hat
  nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
  te.boot[r] <- nde.boot[r] + nie.boot[r]
}

delta.nde <- quantile(nde.boot - beta.a.hat.sc.inf.no.pen, probs = c(0.025,0.975))
nde.ci95.param.boot.quantile.sc.inf.no.pen <- c(beta.a.hat.sc.inf.no.pen - delta.nde[2], beta.a.hat.sc.inf.no.pen - delta.nde[1])
nde.ci95.param.boot.asym.norm.sc.inf.no.pen <- c(beta.a.hat.sc.inf.no.pen - 1.96*sqrt(var(nde.boot)), beta.a.hat.sc.inf.no.pen + 1.96*sqrt(var(nde.boot)))

delta.nie <- quantile(nie.boot - sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen), probs = c(0.025,0.975))
nie.ci95.param.boot.quantile.sc.inf.no.pen <- c(sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen) - delta.nie[2], sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen) - delta.nie[1])
nie.ci95.param.boot.asym.norm.sc.inf.no.pen <- c(sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen) - 1.96*sqrt(var(nie.boot)), sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen) + 1.96*sqrt(var(nie.boot)))

delta.te <- quantile(te.boot - (beta.a.hat.sc.inf.no.pen + sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen)), probs = c(0.025,0.975))
te.ci95.param.boot.quantile.sc.inf.no.pen <- c((beta.a.hat.sc.inf.no.pen + sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen)) - delta.te[2], (beta.a.hat.sc.inf.no.pen + sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen)) - delta.te[1])
te.ci95.param.boot.asym.norm.sc.inf.no.pen <- c((beta.a.hat.sc.inf.no.pen + sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen)) - 1.96*sqrt(var(te.boot)), (beta.a.hat.sc.inf.no.pen + sum(alpha.a.hat.sc.inf.no.pen*beta.m.hat.sc.inf.no.pen)) + 1.96*sqrt(var(te.boot)))

print("Soft Constraint (s^2 = 10000000000) - Unpenalized Done")

#############################################
## Save Output
#############################################

setwd("/filepath/simulations_incorrect_random/n2000_next200000/high_r2_mediator_high_r2_outcome/intermediate_results/")

#Simulation Parameters

write.csv(r2.m.true, paste0("r2.m.true.", arrayid, ".csv"))
write.csv(r2.o.true, paste0("r2.o.true.", arrayid, ".csv"))
write.csv(alpha.a.true, paste0("alpha.a.true.", arrayid, ".csv"))
write.csv(Sigma.m.true, paste0("Sigma.m.true.", arrayid, ".csv"))
write.csv(beta.m.true, paste0("beta.m.true.", arrayid, ".csv"))
write.csv(beta.a.true, paste0("beta.a.true.", arrayid, ".csv"))
write.csv(sigma.e.sq.true, paste0("sigma.e.sq.true.", arrayid, ".csv"))
write.csv(T.hat.internal, paste0("T.hat.internal.", arrayid, ".csv"))
write.csv(var.T.hat.internal, paste0("var.T.hat.internal.", arrayid, ".csv"))
write.csv(T.hat.external, paste0("T.hat.external.", arrayid, ".csv"))
write.csv(var.T.hat.external, paste0("var.T.hat.external.", arrayid, ".csv"))
write.csv(R2.ext.true, paste0("R2.ext.true.", arrayid, ".csv"))
write.csv(n, paste0("n.", arrayid, ".csv"))
write.csv(AtA, paste0("AtA.", arrayid, ".csv"))

#Unconstrained

write.csv(alpha.a.hat.u.no.pen, paste0("alpha.a.hat.u.no.pen.", arrayid, ".csv"))
write.csv(alpha.c.hat.u.no.pen, paste0("alpha.c.hat.u.no.pen.", arrayid, ".csv"))
write.csv(beta.m.hat.u.no.pen, paste0("beta.m.hat.u.no.pen.", arrayid, ".csv"))
write.csv(beta.a.hat.u.no.pen, paste0("beta.a.hat.u.no.pen.", arrayid, ".csv"))
write.csv(beta.c.hat.u.no.pen, paste0("beta.c.hat.u.no.pen.", arrayid, ".csv"))

write.csv(Sigma.m.hat.u, paste0("Sigma.m.hat.u.", arrayid, ".csv"))
write.csv(sigma.e.sq.hat.u, paste0("sigma.e.sq.hat.u.", arrayid, ".csv"))

#Oracle

write.csv(alpha.a.hat.o.no.pen, paste0("alpha.a.hat.o.no.pen.", arrayid, ".csv"))
write.csv(alpha.c.hat.o.no.pen, paste0("alpha.c.hat.o.no.pen.", arrayid, ".csv"))
write.csv(beta.m.hat.o.no.pen, paste0("beta.m.hat.o.no.pen.", arrayid, ".csv"))
write.csv(beta.a.hat.o.no.pen, paste0("beta.a.hat.o.no.pen.", arrayid, ".csv"))
write.csv(beta.c.hat.o.no.pen, paste0("beta.c.hat.o.no.pen.", arrayid, ".csv"))

write.csv(Sigma.m.hat.o, paste0("Sigma.m.hat.o.", arrayid, ".csv"))
write.csv(sigma.e.sq.hat.o, paste0("sigma.e.sq.hat.o.", arrayid, ".csv"))

#Hard Constraint (External Estimate)

write.csv(alpha.a.hat.h.ext.no.pen, paste0("alpha.a.hat.h.ext.no.pen.", arrayid, ".csv"))
write.csv(alpha.c.hat.h.ext.no.pen, paste0("alpha.c.hat.h.ext.no.pen.", arrayid, ".csv"))
write.csv(beta.m.hat.h.ext.no.pen, paste0("beta.m.hat.h.ext.no.pen.", arrayid, ".csv"))
write.csv(beta.a.hat.h.ext.no.pen, paste0("beta.a.hat.h.ext.no.pen.", arrayid, ".csv"))
write.csv(beta.c.hat.h.ext.no.pen, paste0("beta.c.hat.h.ext.no.pen.", arrayid, ".csv"))

write.csv(Sigma.m.hat.h.ext, paste0("Sigma.m.hat.h.ext.", arrayid, ".csv"))
write.csv(sigma.e.sq.hat.h.ext, paste0("sigma.e.sq.hat.h.ext.", arrayid, ".csv"))

#Soft Constraint (Optimal MSE)

write.csv(alpha.a.hat.sc.opt.mse.no.pen, paste0("alpha.a.hat.sc.opt.mse.", arrayid, ".csv"))
write.csv(alpha.c.hat.sc.opt.mse.no.pen, paste0("alpha.c.hat.sc.opt.mse.", arrayid, ".csv"))
write.csv(beta.m.hat.sc.opt.mse.no.pen, paste0("beta.m.hat.sc.opt.mse.", arrayid, ".csv"))
write.csv(beta.a.hat.sc.opt.mse.no.pen, paste0("beta.a.hat.sc.opt.mse.", arrayid, ".csv"))
write.csv(beta.c.hat.sc.opt.mse.no.pen, paste0("beta.c.hat.sc.opt.mse.", arrayid, ".csv"))

write.csv(Sigma.m.hat.sc.opt.mse, paste0("Sigma.m.hat.sc.opt.mse.", arrayid, ".csv"))
write.csv(sigma.e.sq.hat.sc.opt.mse, paste0("sigma.e.sq.hat.sc.opt.mse.", arrayid, ".csv"))

write.csv(s2.hat.sc.opt.mse.no.pen, paste0("s2.hat.sc.opt.mse.no.pen.", arrayid, ".csv"))

write.csv(nde.ci95.param.boot.quantile.sc.opt.mse.no.pen, paste0("nde.ci95.param.boot.quantile.sc.opt.mse.", arrayid, ".csv"))
write.csv(nde.ci95.param.boot.asym.norm.sc.opt.mse.no.pen, paste0("nde.ci95.param.boot.asym.norm.sc.opt.mse.", arrayid, ".csv"))

write.csv(nie.ci95.param.boot.quantile.sc.opt.mse.no.pen, paste0("nie.ci95.param.boot.quantile.sc.opt.mse.", arrayid, ".csv"))
write.csv(nie.ci95.param.boot.asym.norm.sc.opt.mse.no.pen, paste0("nie.ci95.param.boot.asym.norm.sc.opt.mse.", arrayid, ".csv"))

write.csv(te.ci95.param.boot.quantile.sc.opt.mse.no.pen, paste0("te.ci95.param.boot.quantile.sc.opt.mse.", arrayid, ".csv"))
write.csv(te.ci95.param.boot.asym.norm.sc.opt.mse.no.pen, paste0("te.ci95.param.boot.asym.norm.sc.opt.mse.", arrayid, ".csv"))

#Soft Constraint (EB)

write.csv(alpha.a.hat.sc.eb.no.pen, paste0("alpha.a.hat.sc.eb.no.pen.", arrayid, ".csv"))
write.csv(alpha.c.hat.sc.eb.no.pen, paste0("alpha.c.hat.sc.eb.no.pen.", arrayid, ".csv"))
write.csv(beta.m.hat.sc.eb.no.pen, paste0("beta.m.hat.sc.eb.no.pen.", arrayid, ".csv"))
write.csv(beta.a.hat.sc.eb.no.pen, paste0("beta.a.hat.sc.eb.no.pen.", arrayid, ".csv"))
write.csv(beta.c.hat.sc.eb.no.pen, paste0("beta.c.hat.sc.eb.no.pen.", arrayid, ".csv"))

write.csv(Sigma.m.hat.sc.eb, paste0("Sigma.m.hat.sc.eb.", arrayid, ".csv"))
write.csv(sigma.e.sq.hat.sc.eb, paste0("sigma.e.sq.hat.sc.eb.", arrayid, ".csv"))

write.csv(s2.hat.sc.eb.no.pen, paste0("s2.hat.sc.eb.no.pen.", arrayid, ".csv"))

write.csv(nde.ci95.param.boot.quantile.sc.eb.no.pen, paste0("nde.ci95.param.boot.quantile.sc.eb.", arrayid, ".csv"))
write.csv(nde.ci95.param.boot.asym.norm.sc.eb.no.pen, paste0("nde.ci95.param.boot.asym.norm.sc.eb.", arrayid, ".csv"))

write.csv(nie.ci95.param.boot.quantile.sc.eb.no.pen, paste0("nie.ci95.param.boot.quantile.sc.eb.", arrayid, ".csv"))
write.csv(nie.ci95.param.boot.asym.norm.sc.eb.no.pen, paste0("nie.ci95.param.boot.asym.norm.sc.eb.", arrayid, ".csv"))

write.csv(te.ci95.param.boot.quantile.sc.eb.no.pen, paste0("te.ci95.param.boot.quantile.sc.eb.", arrayid, ".csv"))
write.csv(te.ci95.param.boot.asym.norm.sc.eb.no.pen, paste0("te.ci95.param.boot.asym.norm.sc.eb.", arrayid, ".csv"))

#Soft Constraint (Discrete)

write.csv(alpha.a.hat.d.no.pen, paste0("alpha.a.hat.d.no.pen.", arrayid, ".csv"))
write.csv(alpha.c.hat.d.no.pen, paste0("alpha.c.hat.d.no.pen.", arrayid, ".csv"))
write.csv(beta.m.hat.d.no.pen, paste0("beta.m.hat.d.no.pen.", arrayid, ".csv"))
write.csv(beta.a.hat.d.no.pen, paste0("beta.a.hat.d.no.pen.", arrayid, ".csv"))
write.csv(beta.c.hat.d.no.pen, paste0("beta.c.hat.d.no.pen.", arrayid, ".csv"))

write.csv(Sigma.m.hat.d, paste0("Sigma.m.hat.d.", arrayid, ".csv"))
write.csv(sigma.e.sq.hat.d, paste0("sigma.e.sq.hat.d.", arrayid, ".csv"))

write.csv(choose_eb, paste0("hard.or.unconstrained.d.", arrayid, ".csv"))

write.csv(nde.ci95.param.boot.quantile.d.no.pen, paste0("nde.ci95.param.boot.quantile.d.", arrayid, ".csv"))
write.csv(nde.ci95.param.boot.asym.norm.d.no.pen, paste0("nde.ci95.param.boot.asym.norm.d.", arrayid, ".csv"))

write.csv(nie.ci95.param.boot.quantile.d.no.pen, paste0("nie.ci95.param.boot.quantile.d.", arrayid, ".csv"))
write.csv(nie.ci95.param.boot.asym.norm.d.no.pen, paste0("nie.ci95.param.boot.asym.norm.d.", arrayid, ".csv"))

write.csv(te.ci95.param.boot.quantile.d.no.pen, paste0("te.ci95.param.boot.quantile.d.", arrayid, ".csv"))
write.csv(te.ci95.param.boot.asym.norm.d.no.pen, paste0("te.ci95.param.boot.asym.norm.d.", arrayid, ".csv"))

#Soft Constraint (s^2 = 0.0001)

write.csv(alpha.a.hat.sc.0.no.pen, paste0("alpha.a.hat.sc.0.no.pen.", arrayid, ".csv"))
write.csv(alpha.c.hat.sc.0.no.pen, paste0("alpha.c.hat.sc.0.no.pen.", arrayid, ".csv"))
write.csv(beta.m.hat.sc.0.no.pen, paste0("beta.m.hat.sc.0.no.pen.", arrayid, ".csv"))
write.csv(beta.a.hat.sc.0.no.pen, paste0("beta.a.hat.sc.0.no.pen.", arrayid, ".csv"))
write.csv(beta.c.hat.sc.0.no.pen, paste0("beta.c.hat.sc.0.no.pen.", arrayid, ".csv"))

write.csv(Sigma.m.hat.sc.0, paste0("Sigma.m.hat.sc.0.", arrayid, ".csv"))
write.csv(sigma.e.sq.hat.sc.0, paste0("sigma.e.sq.hat.sc.0.", arrayid, ".csv"))

write.csv(nde.ci95.param.boot.quantile.sc.0.no.pen, paste0("nde.ci95.param.boot.quantile.sc.0.", arrayid, ".csv"))
write.csv(nde.ci95.param.boot.asym.norm.sc.0.no.pen, paste0("nde.ci95.param.boot.asym.norm.sc.0.", arrayid, ".csv"))

write.csv(nie.ci95.param.boot.quantile.sc.0.no.pen, paste0("nie.ci95.param.boot.quantile.sc.0.", arrayid, ".csv"))
write.csv(nie.ci95.param.boot.asym.norm.sc.0.no.pen, paste0("nie.ci95.param.boot.asym.norm.sc.0.", arrayid, ".csv"))

write.csv(te.ci95.param.boot.quantile.sc.0.no.pen, paste0("te.ci95.param.boot.quantile.sc.0.", arrayid, ".csv"))
write.csv(te.ci95.param.boot.asym.norm.sc.0.no.pen, paste0("te.ci95.param.boot.asym.norm.sc.0.", arrayid, ".csv"))

#Soft Constraint (s^2 = 10000000000)

write.csv(alpha.a.hat.sc.inf.no.pen, paste0("alpha.a.hat.sc.inf.no.pen.", arrayid, ".csv"))
write.csv(alpha.c.hat.sc.inf.no.pen, paste0("alpha.c.hat.sc.inf.no.pen.", arrayid, ".csv"))
write.csv(beta.m.hat.sc.inf.no.pen, paste0("beta.m.hat.sc.inf.no.pen.", arrayid, ".csv"))
write.csv(beta.a.hat.sc.inf.no.pen, paste0("beta.a.hat.sc.inf.no.pen.", arrayid, ".csv"))
write.csv(beta.c.hat.sc.inf.no.pen, paste0("beta.c.hat.sc.inf.no.pen.", arrayid, ".csv"))

write.csv(Sigma.m.hat.sc.inf, paste0("Sigma.m.hat.sc.inf.", arrayid, ".csv"))
write.csv(sigma.e.sq.hat.sc.inf, paste0("sigma.e.sq.hat.sc.inf.", arrayid, ".csv"))

write.csv(nde.ci95.param.boot.quantile.sc.inf.no.pen, paste0("nde.ci95.param.boot.quantile.sc.inf.", arrayid, ".csv"))
write.csv(nde.ci95.param.boot.asym.norm.sc.inf.no.pen, paste0("nde.ci95.param.boot.asym.norm.sc.inf.", arrayid, ".csv"))

write.csv(nie.ci95.param.boot.quantile.sc.inf.no.pen, paste0("nie.ci95.param.boot.quantile.sc.inf.", arrayid, ".csv"))
write.csv(nie.ci95.param.boot.asym.norm.sc.inf.no.pen, paste0("nie.ci95.param.boot.asym.norm.sc.inf.", arrayid, ".csv"))

write.csv(te.ci95.param.boot.quantile.sc.inf.no.pen, paste0("te.ci95.param.boot.quantile.sc.inf.", arrayid, ".csv"))
write.csv(te.ci95.param.boot.asym.norm.sc.inf.no.pen, paste0("te.ci95.param.boot.asym.norm.sc.inf.", arrayid, ".csv"))
