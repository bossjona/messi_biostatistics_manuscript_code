setwd("/filepath/simulations_incorrect_random/n200_next20000/high_r2_mediator_low_r2_outcome/intermediate_results/")

#Create empty lists and vectors to populate

n <- read.csv(paste0("n.", 1, ".csv"))[,-1]

#Unconstrained
beta.m.u.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.u.no.pen.est.vec <- rep(NA, 2000)
alpha.a.u.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
nie.u.no.pen.est.vec <- rep(NA, 2000)
asym.vars.u.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 2)
nie.u.no.pen.signif.vec <- rep(NA, 2000)
nde.u.no.pen.signif.vec <- rep(NA, 2000)
sigma.e.sq.hat.u.vec <- rep(NA, 2000)

#Oracle
beta.m.o.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.o.no.pen.est.vec <- rep(NA, 2000)
alpha.a.o.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
nie.o.no.pen.est.vec <- rep(NA, 2000)
asym.vars.o.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 2)
nie.o.no.pen.signif.vec <- rep(NA, 2000)
nde.o.no.pen.signif.vec <- rep(NA, 2000)
sigma.e.sq.hat.o.vec <- rep(NA, 2000)

#Hard Constraint (External Estimate)
beta.m.h.ext.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.h.ext.no.pen.est.vec <- rep(NA, 2000)
alpha.a.h.ext.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
nie.h.ext.no.pen.est.vec <- rep(NA, 2000)
asym.vars.h.ext.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 2)
nie.h.ext.no.pen.signif.vec <- rep(NA, 2000)
nde.h.ext.no.pen.signif.vec <- rep(NA, 2000)
sigma.e.sq.hat.h.ext.vec <- rep(NA, 2000)

#Soft Constraint (Optimal MSE)
beta.m.hat.sc.opt.mse.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.hat.sc.opt.mse.no.pen.vec <- rep(NA, 2000)
alpha.a.hat.sc.opt.mse.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 50)
nie.sc.opt.mse.no.pen.est.vec <- rep(NA, 2000)
asym.vars.sc.opt.mse.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 2)

te.sc.opt.mse.no.pen.est.vec <- rep(NA, 2000)

nie.sc.opt.mse.no.pen.signif.vec <- rep(NA, 2000)
nie.sc.opt.mse.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
nde.sc.opt.mse.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
te.sc.opt.mse.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
nie.sc.opt.mse.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
nde.sc.opt.mse.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
te.sc.opt.mse.pb.quant.no.pen.signif.vec <- rep(NA, 2000)

sigma.e.sq.hat.sc.opt.mse.vec <- rep(NA, 2000)
s2.hat.sc.opt.mse.no.pen.est.vec <- rep(NA, 2000)

#Soft Constraint (EB)
beta.m.hat.sc.eb.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.hat.sc.eb.no.pen.vec <- rep(NA, 2000)
alpha.a.hat.sc.eb.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 50)
nie.sc.eb.no.pen.est.vec <- rep(NA, 2000)
asym.vars.sc.eb.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 2)

te.sc.eb.no.pen.est.vec <- rep(NA, 2000)

nie.sc.eb.no.pen.signif.vec <- rep(NA, 2000)

nie.sc.eb.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
nde.sc.eb.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
te.sc.eb.pb.quant.no.pen.signif.vec <- rep(NA, 2000)

nie.sc.eb.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
nde.sc.eb.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
te.sc.eb.pb.asym.no.pen.signif.vec <- rep(NA, 2000)

sigma.e.sq.hat.sc.eb.vec <- rep(NA, 2000)
s2.hat.sc.eb.no.pen.est.vec <- rep(NA, 2000)

#Soft Constraint (Discrete)
beta.m.hat.d.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.hat.d.no.pen.vec <- rep(NA, 2000)
alpha.a.hat.d.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 50)
nie.d.no.pen.est.vec <- rep(NA, 2000)

te.d.no.pen.est.vec <- rep(NA, 2000)

nie.d.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
nde.d.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
te.d.pb.quant.no.pen.signif.vec <- rep(NA, 2000)

nie.d.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
nde.d.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
te.d.pb.asym.no.pen.signif.vec <- rep(NA, 2000)

sigma.e.sq.hat.d.vec <- rep(NA, 2000)
hard.or.unconstrained.d.vec <- rep(NA, 2000)

asym.vars.d.pb.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 2)

#Soft Constraint (s^2 = 0.0001)
beta.m.sc.0.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.sc.0.no.pen.est.vec <- rep(NA, 2000)
alpha.a.sc.0.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
nie.sc.0.no.pen.est.vec <- rep(NA, 2000)
asym.vars.sc.0.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 2)
nie.sc.0.no.pen.signif.vec <- rep(NA, 2000)
sigma.e.sq.hat.sc.0.vec <- rep(NA, 2000)

te.sc.0.no.pen.est.vec <- rep(NA, 2000)

nie.sc.0.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
nde.sc.0.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
te.sc.0.pb.quant.no.pen.signif.vec <- rep(NA, 2000)

nie.sc.0.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
nde.sc.0.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
te.sc.0.pb.asym.no.pen.signif.vec <- rep(NA, 2000)

#Soft Constraint (s^2 = 10000000000)
beta.m.sc.inf.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.sc.inf.no.pen.est.vec <- rep(NA, 2000)
alpha.a.sc.inf.no.pen.est.mat <- matrix(NA, nrow = 2000, ncol = 50)
nie.sc.inf.no.pen.est.vec <- rep(NA, 2000)
asym.vars.sc.inf.no.pen.mat <- matrix(NA, nrow = 2000, ncol = 2)
nie.sc.inf.no.pen.signif.vec <- rep(NA, 2000)
sigma.e.sq.hat.sc.inf.vec <- rep(NA, 2000)

te.sc.inf.no.pen.est.vec <- rep(NA, 2000)

nie.sc.inf.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
nde.sc.inf.pb.quant.no.pen.signif.vec <- rep(NA, 2000)
te.sc.inf.pb.quant.no.pen.signif.vec <- rep(NA, 2000)

nie.sc.inf.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
nde.sc.inf.pb.asym.no.pen.signif.vec <- rep(NA, 2000)
te.sc.inf.pb.asym.no.pen.signif.vec <- rep(NA, 2000)

AtA.vec <- rep(NA, 2000)
beta.m.true <- matrix(NA, nrow = 2000, ncol = 50)
beta.a.true <- rep(NA, 2000)
alpha.a.true <- matrix(NA, nrow = 2000, ncol = 50)
sigma.e.sq.true.vec <- rep(NA, 2000)
r2.m.vec <- rep(NA, 2000)
r2.o.vec <- rep(NA, 2000)
r2.t.ext.vec <- rep(NA, 2000)
T.hat.internal.vec <- rep(NA, 2000)
T.hat.external.vec <- rep(NA, 2000)
var.T.hat.internal.vec <- rep(NA, 2000)
var.T.hat.external.vec <- rep(NA, 2000)

finished.itr <- 1:2000
#Read in data from all 2000 jobs and put it in an array
for(arrayid in finished.itr){
  
  #True
  beta.m.true[arrayid,] <- read.csv(paste0("beta.m.true.", arrayid, ".csv"))[,-1]
  beta.a.true[arrayid] <- as.numeric(read.csv(paste0("beta.a.true.", arrayid, ".csv"))[,-1])
  alpha.a.true[arrayid,] <- read.csv(paste0("alpha.a.true.", arrayid, ".csv"))[,-1]
  nie.true <- sum(alpha.a.true[arrayid,]*beta.m.true[arrayid,])
  nde.true <- beta.a.true[arrayid]
  sigma.e.sq.true <- as.numeric(read.csv(paste0("sigma.e.sq.true.", arrayid, ".csv"))[,-1])
  sigma.e.sq.true.vec[arrayid] <- sigma.e.sq.true
  Sigma.m.true <- as.matrix(read.csv(paste0("Sigma.m.true.", arrayid, ".csv"))[,-1])
  Sigma.m.inv.true <- solve(Sigma.m.true)
  
  #Total Effect Information
  T.hat.internal <- as.numeric(read.csv(paste0("T.hat.internal.", arrayid, ".csv"))[,-1])
  T.hat.external <- as.numeric(read.csv(paste0("T.hat.external.", arrayid, ".csv"))[,-1])
  var.T.hat.internal <- as.numeric(read.csv(paste0("var.T.hat.internal.", arrayid, ".csv"))[,-1])
  var.T.hat.external <- as.numeric(read.csv(paste0("var.T.hat.external.", arrayid, ".csv"))[,-1])
  
  T.hat.internal.vec[arrayid] <- T.hat.internal
  T.hat.external.vec[arrayid] <- T.hat.external
  var.T.hat.internal.vec[arrayid] <- var.T.hat.internal
  var.T.hat.external.vec[arrayid] <- var.T.hat.external
  
  #Unconstrained - Unpenalized
  beta.m.u.no.pen.est.mat[arrayid,] <- read.csv(paste0("beta.m.hat.u.no.pen.", arrayid, ".csv"))[,-1]
  beta.a.u.no.pen.est.vec[arrayid] <- read.csv(paste0("beta.a.hat.u.no.pen.", arrayid, ".csv"))[,-1]
  alpha.a.u.no.pen.est.mat[arrayid,] <- read.csv(paste0("alpha.a.hat.u.no.pen.", arrayid, ".csv"))[,-1]
  nie.u.no.pen.est.vec[arrayid] <- sum(alpha.a.u.no.pen.est.mat[arrayid,]*beta.m.u.no.pen.est.mat[arrayid,])
    
  sigma.e.sq.hat <- as.numeric(read.csv(paste0("sigma.e.sq.hat.u.", arrayid, ".csv"))[,-1])
  sigma.e.sq.hat.u.vec[arrayid] <- sigma.e.sq.hat
  Sigma.m.hat <- as.matrix(read.csv(paste0("Sigma.m.hat.u.", arrayid, ".csv"))[,-1])
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  
  sigma.e.sq.hat.save <- sigma.e.sq.hat
  Sigma.m.hat.save <- Sigma.m.hat
  Sigma.m.inv.hat.save <- Sigma.m.inv.hat
  AtA.vec[arrayid] <- as.numeric(read.csv(paste0("AtA.", arrayid, ".csv"))[,-1])
  sigma.a.sq.hat <- AtA.vec[arrayid]/n
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.u.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.u.no.pen.est.mat[arrayid,], ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.u.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.u.no.pen.est.mat[arrayid,], ncol = 1))
  nie.var <- (quad.form.beta.hat/sigma.a.sq.hat) + sigma.e.sq.hat*quad.form.alpha.hat
  nde.var <- (sigma.e.sq.hat/sigma.a.sq.hat) + sigma.e.sq.hat*quad.form.alpha.hat
  
  asym.vars.u.no.pen.mat[arrayid,] <- c(nie.var, nde.var)
  nie.u.no.pen.signif.vec[arrayid] <- ifelse((nie.u.no.pen.est.vec[arrayid] - 1.96*sqrt(nie.var)/sqrt(n) <= nie.true) == (nie.u.no.pen.est.vec[arrayid] + 1.96*sqrt(nie.var)/sqrt(n) >= nie.true), 1, 0)
  nde.u.no.pen.signif.vec[arrayid] <- ifelse((beta.a.u.no.pen.est.vec[arrayid] - 1.96*sqrt(nde.var)/sqrt(n) <= nde.true) == (beta.a.u.no.pen.est.vec[arrayid] + 1.96*sqrt(nde.var)/sqrt(n) >= nde.true), 1, 0)
  
  #Oracle - Unpenalized
  beta.m.o.no.pen.est.mat[arrayid,] <- read.csv(paste0("beta.m.hat.o.no.pen.", arrayid, ".csv"))[,-1]
  beta.a.o.no.pen.est.vec[arrayid] <- read.csv(paste0("beta.a.hat.o.no.pen.", arrayid, ".csv"))[,-1]
  alpha.a.o.no.pen.est.mat[arrayid,] <- read.csv(paste0("alpha.a.hat.o.no.pen.", arrayid, ".csv"))[,-1]
  nie.o.no.pen.est.vec[arrayid] <- sum(alpha.a.o.no.pen.est.mat[arrayid,]*beta.m.o.no.pen.est.mat[arrayid,])
  
  sigma.e.sq.hat <- as.numeric(read.csv(paste0("sigma.e.sq.hat.o.", arrayid, ".csv"))[,-1])
  sigma.e.sq.hat.o.vec[arrayid] <- sigma.e.sq.hat
  Sigma.m.hat <- as.matrix(read.csv(paste0("Sigma.m.hat.o.", arrayid, ".csv"))[,-1])
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.o.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.o.no.pen.est.mat[arrayid,], ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.o.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.o.no.pen.est.mat[arrayid,], ncol = 1))
  nie.var <- (quad.form.beta.hat/sigma.a.sq.hat)*(sigma.e.sq.hat/(sigma.e.sq.hat + quad.form.beta.hat)) + sigma.e.sq.hat*quad.form.alpha.hat
  nde.var <- nie.var
  
  asym.vars.o.no.pen.mat[arrayid,] <- c(nie.var, nde.var)
  nie.o.no.pen.signif.vec[arrayid] <- ifelse((nie.o.no.pen.est.vec[arrayid] - 1.96*sqrt(nie.var)/sqrt(n) <= nie.true) == (nie.o.no.pen.est.vec[arrayid] + 1.96*sqrt(nie.var)/sqrt(n) >= nie.true), 1, 0)
  nde.o.no.pen.signif.vec[arrayid] <- ifelse((beta.a.o.no.pen.est.vec[arrayid] - 1.96*sqrt(nde.var)/sqrt(n) <= nde.true) == (beta.a.o.no.pen.est.vec[arrayid] + 1.96*sqrt(nde.var)/sqrt(n) >= nde.true), 1, 0)
  
  #Hard Constraint (External Estimate) - Unpenalized
  beta.m.h.ext.no.pen.est.mat[arrayid,] <- read.csv(paste0("beta.m.hat.h.ext.no.pen.", arrayid, ".csv"))[,-1]
  beta.a.h.ext.no.pen.est.vec[arrayid] <- read.csv(paste0("beta.a.hat.h.ext.no.pen.", arrayid, ".csv"))[,-1]
  alpha.a.h.ext.no.pen.est.mat[arrayid,] <- read.csv(paste0("alpha.a.hat.h.ext.no.pen.", arrayid, ".csv"))[,-1]
  nie.h.ext.no.pen.est.vec[arrayid] <- sum(alpha.a.h.ext.no.pen.est.mat[arrayid,]*beta.m.h.ext.no.pen.est.mat[arrayid,])
  
  sigma.e.sq.hat <- as.numeric(read.csv(paste0("sigma.e.sq.hat.h.ext.", arrayid, ".csv"))[,-1])
  sigma.e.sq.hat.h.ext.vec[arrayid] <- sigma.e.sq.hat
  Sigma.m.hat <- as.matrix(read.csv(paste0("Sigma.m.hat.h.ext.", arrayid, ".csv"))[,-1])
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.h.ext.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.h.ext.no.pen.est.mat[arrayid,], ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.h.ext.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.h.ext.no.pen.est.mat[arrayid,], ncol = 1))
  nie.var <- (quad.form.beta.hat/sigma.a.sq.hat)*(sigma.e.sq.hat/(sigma.e.sq.hat + quad.form.beta.hat)) + sigma.e.sq.hat*quad.form.alpha.hat
  nde.var <- nie.var
  
  asym.vars.h.ext.no.pen.mat[arrayid,] <- c(nie.var, nde.var)
  nie.h.ext.no.pen.signif.vec[arrayid] <- ifelse((nie.h.ext.no.pen.est.vec[arrayid] - 1.96*sqrt(nie.var)/sqrt(n) <= nie.true) == (nie.h.ext.no.pen.est.vec[arrayid] + 1.96*sqrt(nie.var)/sqrt(n) >= nie.true), 1, 0)
  nde.h.ext.no.pen.signif.vec[arrayid] <- ifelse((beta.a.h.ext.no.pen.est.vec[arrayid] - 1.96*sqrt(nde.var)/sqrt(n) <= nde.true) == (beta.a.h.ext.no.pen.est.vec[arrayid] + 1.96*sqrt(nde.var)/sqrt(n) >= nde.true), 1, 0)
  
  #Soft Constraint (Optimal MSE) - Unpenalized
  beta.m.hat.sc.opt.mse.no.pen.mat[arrayid,] <- read.csv(paste0("beta.m.hat.sc.opt.mse.", arrayid, ".csv"))[,-1]
  beta.a.hat.sc.opt.mse.no.pen.vec[arrayid] <- read.csv(paste0("beta.a.hat.sc.opt.mse.", arrayid, ".csv"))[,-1]
  alpha.a.hat.sc.opt.mse.no.pen.mat[arrayid,] <- read.csv(paste0("alpha.a.hat.sc.opt.mse.", arrayid, ".csv"))[,-1]
  nie.sc.opt.mse.no.pen.est.vec[arrayid] <- sum(alpha.a.hat.sc.opt.mse.no.pen.mat[arrayid,]*beta.m.hat.sc.opt.mse.no.pen.mat[arrayid,])
  
  sigma.e.sq.hat.sc.opt.mse.vec[arrayid] <- read.csv(paste0("sigma.e.sq.hat.sc.opt.mse.", arrayid, ".csv"))[,-1]
  s2.hat.sc.opt.mse.no.pen.est.vec[arrayid] <- read.csv(paste0("s2.hat.sc.opt.mse.no.pen.", arrayid, ".csv"))[,-1]
  
  te.sc.opt.mse.no.pen.est.vec[arrayid] <- beta.a.hat.sc.opt.mse.no.pen.vec[arrayid] + nie.sc.opt.mse.no.pen.est.vec[arrayid]
  
  nie.sc.opt.mse.pb.quant <- read.csv(paste0("nie.ci95.param.boot.quantile.sc.opt.mse.", arrayid, ".csv"))[,-1]
  nde.sc.opt.mse.pb.quant <- read.csv(paste0("nde.ci95.param.boot.quantile.sc.opt.mse.", arrayid, ".csv"))[,-1]
  te.sc.opt.mse.pb.quant <- read.csv(paste0("te.ci95.param.boot.quantile.sc.opt.mse.", arrayid, ".csv"))[,-1]
  
  nie.sc.opt.mse.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.opt.mse.pb.quant[1] <= nie.true) == (nie.sc.opt.mse.pb.quant[2] >= nie.true), 1, 0)
  nde.sc.opt.mse.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nde.sc.opt.mse.pb.quant[1] <= nde.true) == (nde.sc.opt.mse.pb.quant[2] >= nde.true), 1, 0)
  te.sc.opt.mse.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((te.sc.opt.mse.pb.quant[1] <= nie.true + nde.true) == (te.sc.opt.mse.pb.quant[2] >= nie.true + nde.true), 1, 0)
  
  nie.sc.opt.mse.pb.asym <- read.csv(paste0("nie.ci95.param.boot.asym.norm.sc.opt.mse.", arrayid, ".csv"))[,-1]
  nde.sc.opt.mse.pb.asym <- read.csv(paste0("nde.ci95.param.boot.asym.norm.sc.opt.mse.", arrayid, ".csv"))[,-1]
  te.sc.opt.mse.pb.asym <- read.csv(paste0("te.ci95.param.boot.asym.norm.sc.opt.mse.", arrayid, ".csv"))[,-1]
  
  nie.sc.opt.mse.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.opt.mse.pb.asym[1] <= nie.true) == (nie.sc.opt.mse.pb.asym[2] >= nie.true), 1, 0)
  nde.sc.opt.mse.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nde.sc.opt.mse.pb.asym[1] <= nde.true) == (nde.sc.opt.mse.pb.asym[2] >= nde.true), 1, 0)
  te.sc.opt.mse.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((te.sc.opt.mse.pb.asym[1] <= nie.true + nde.true) == (te.sc.opt.mse.pb.asym[2] >= nie.true + nde.true), 1, 0)
  
  Sigma.m.hat <- as.matrix(read.csv(paste0("Sigma.m.hat.sc.opt.mse.", arrayid, ".csv"))[,-1])
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  quad.form.beta.hat <- as.numeric(matrix(beta.m.hat.sc.opt.mse.no.pen.mat[arrayid,], nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.hat.sc.opt.mse.no.pen.mat[arrayid,], ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.hat.sc.opt.mse.no.pen.mat[arrayid,], nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.hat.sc.opt.mse.no.pen.mat[arrayid,], ncol = 1))
  soft.const <- (1/(n*s2.hat.sc.opt.mse.no.pen.est.vec[arrayid]*var.T.hat.external))*((sigma.a.sq.hat/sigma.e.sq.hat.sc.opt.mse.vec[arrayid])+(1/(n*s2.hat.sc.opt.mse.no.pen.est.vec[arrayid]*var.T.hat.external)))^(-1)
  nie.var <- (quad.form.beta.hat/sigma.a.sq.hat)*(1+(soft.const*quad.form.beta.hat/sigma.e.sq.hat.sc.opt.mse.vec[arrayid]))^(-1) + sigma.e.sq.hat.sc.opt.mse.vec[arrayid]*quad.form.alpha.hat
  
  asym.vars.sc.opt.mse.no.pen.mat[arrayid,] <- c(nie.var, n*((nde.sc.opt.mse.pb.asym[2] - nde.sc.opt.mse.pb.asym[1])/(2*1.96))^2)
  
  nie.sc.opt.mse.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.opt.mse.no.pen.est.vec[arrayid] - 1.96*sqrt(nie.var)/sqrt(n) <= nie.true) == (nie.sc.opt.mse.no.pen.est.vec[arrayid] + 1.96*sqrt(nie.var)/sqrt(n) >= nie.true), 1, 0)
  
  #Soft Constraint (Discrete) - Unpenalized
  
  beta.m.hat.d.no.pen.mat[arrayid,] <- read.csv(paste0("beta.m.hat.d.no.pen.", arrayid, ".csv"))[,-1]
  beta.a.hat.d.no.pen.vec[arrayid] <- read.csv(paste0("beta.a.hat.d.no.pen.", arrayid, ".csv"))[,-1]
  alpha.a.hat.d.no.pen.mat[arrayid,] <- read.csv(paste0("alpha.a.hat.d.no.pen.", arrayid, ".csv"))[,-1]
  nie.d.no.pen.est.vec[arrayid] <- sum(alpha.a.hat.d.no.pen.mat[arrayid,]*beta.m.hat.d.no.pen.mat[arrayid,])
  
  te.d.no.pen.est.vec[arrayid] <- beta.a.hat.d.no.pen.vec[arrayid] + nie.d.no.pen.est.vec[arrayid]
  
  nie.d.pb.quant <- read.csv(paste0("nie.ci95.param.boot.quantile.d.", arrayid, ".csv"))[,-1]
  nde.d.pb.quant <- read.csv(paste0("nde.ci95.param.boot.quantile.d.", arrayid, ".csv"))[,-1]
  te.d.pb.quant <- read.csv(paste0("te.ci95.param.boot.quantile.d.", arrayid, ".csv"))[,-1]
  
  nie.d.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nie.d.pb.quant[1] <= nie.true) == (nie.d.pb.quant[2] >= nie.true), 1, 0)
  nde.d.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nde.d.pb.quant[1] <= nde.true) == (nde.d.pb.quant[2] >= nde.true), 1, 0)
  te.d.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((te.d.pb.quant[1] <= nie.true + nde.true) == (te.d.pb.quant[2] >= nie.true + nde.true), 1, 0)
  
  nie.d.pb.asym <- read.csv(paste0("nie.ci95.param.boot.asym.norm.d.", arrayid, ".csv"))[,-1]
  nde.d.pb.asym <- read.csv(paste0("nde.ci95.param.boot.asym.norm.d.", arrayid, ".csv"))[,-1]
  te.d.pb.asym <- read.csv(paste0("te.ci95.param.boot.asym.norm.d.", arrayid, ".csv"))[,-1]
  
  nie.d.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nie.d.pb.asym[1] <= nie.true) == (nie.d.pb.asym[2] >= nie.true), 1, 0)
  nde.d.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nde.d.pb.asym[1] <= nde.true) == (nde.d.pb.asym[2] >= nde.true), 1, 0)
  te.d.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((te.d.pb.asym[1] <= nie.true + nde.true) == (te.d.pb.asym[2] >= nie.true + nde.true), 1, 0)
  
  asym.vars.d.pb.no.pen.mat[arrayid,] <- c(n*((nie.d.pb.asym[2] - nie.d.pb.asym[1])/(2*1.96))^2, n*((nde.d.pb.asym[2] - nde.d.pb.asym[1])/(2*1.96))^2)
  sigma.e.sq.hat.d.vec[arrayid] <- as.numeric(read.csv(paste0("sigma.e.sq.hat.d.", arrayid, ".csv"))[,-1])
  hard.or.unconstrained.d.vec[arrayid] <- read.csv(paste0("hard.or.unconstrained.d.", arrayid, ".csv"))[,-1]
  
  #Soft Constraint (EB) - Unpenalized
  
  beta.m.hat.sc.eb.no.pen.mat[arrayid,] <- read.csv(paste0("beta.m.hat.sc.eb.no.pen.", arrayid, ".csv"))[,-1]
  beta.a.hat.sc.eb.no.pen.vec[arrayid] <- read.csv(paste0("beta.a.hat.sc.eb.no.pen.", arrayid, ".csv"))[,-1]
  alpha.a.hat.sc.eb.no.pen.mat[arrayid,] <- read.csv(paste0("alpha.a.hat.sc.eb.no.pen.", arrayid, ".csv"))[,-1]
  nie.sc.eb.no.pen.est.vec[arrayid] <- sum(alpha.a.hat.sc.eb.no.pen.mat[arrayid,]*beta.m.hat.sc.eb.no.pen.mat[arrayid,])
  
  sigma.e.sq.hat <- as.numeric(read.csv(paste0("sigma.e.sq.hat.sc.eb.", arrayid, ".csv"))[,-1])
  sigma.e.sq.hat.sc.eb.vec[arrayid] <- sigma.e.sq.hat
  Sigma.m.hat <- as.matrix(read.csv(paste0("Sigma.m.hat.sc.eb.", arrayid, ".csv"))[,-1])
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  
  s2.hat.sc.eb.no.pen.est.vec[arrayid] <- read.csv(paste0("s2.hat.sc.eb.no.pen.", arrayid, ".csv"))[,-1]
  te.sc.eb.no.pen.est.vec[arrayid] <- beta.a.hat.sc.eb.no.pen.vec[arrayid] + nie.sc.eb.no.pen.est.vec[arrayid]
  
  s2.hat <- s2.hat.sc.eb.no.pen.est.vec[arrayid]
  
  nie.sc.eb.pb.quant <- read.csv(paste0("nie.ci95.param.boot.quantile.sc.eb.", arrayid, ".csv"))[,-1]
  nde.sc.eb.pb.quant <- read.csv(paste0("nde.ci95.param.boot.quantile.sc.eb.", arrayid, ".csv"))[,-1]
  te.sc.eb.pb.quant <- read.csv(paste0("te.ci95.param.boot.quantile.sc.eb.", arrayid, ".csv"))[,-1]
  
  nie.sc.eb.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.eb.pb.quant[1] <= nie.true) == (nie.sc.eb.pb.quant[2] >= nie.true), 1, 0)
  nde.sc.eb.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nde.sc.eb.pb.quant[1] <= nde.true) == (nde.sc.eb.pb.quant[2] >= nde.true), 1, 0)
  te.sc.eb.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((te.sc.eb.pb.quant[1] <= nie.true + nde.true) == (te.sc.eb.pb.quant[2] >= nie.true + nde.true), 1, 0)
  
  nie.sc.eb.pb.asym <- read.csv(paste0("nie.ci95.param.boot.asym.norm.sc.eb.", arrayid, ".csv"))[,-1]
  nde.sc.eb.pb.asym <- read.csv(paste0("nde.ci95.param.boot.asym.norm.sc.eb.", arrayid, ".csv"))[,-1]
  te.sc.eb.pb.asym <- read.csv(paste0("te.ci95.param.boot.asym.norm.sc.eb.", arrayid, ".csv"))[,-1]
  
  nie.sc.eb.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.eb.pb.asym[1] <= nie.true) == (nie.sc.eb.pb.asym[2] >= nie.true), 1, 0)
  nde.sc.eb.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nde.sc.eb.pb.asym[1] <= nde.true) == (nde.sc.eb.pb.asym[2] >= nde.true), 1, 0)
  te.sc.eb.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((te.sc.eb.pb.asym[1] <= nie.true + nde.true) == (te.sc.eb.pb.asym[2] >= nie.true + nde.true), 1, 0)
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.hat.sc.eb.no.pen.mat[arrayid,], nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.hat.sc.eb.no.pen.mat[arrayid,], ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.hat.sc.eb.no.pen.mat[arrayid,], nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.hat.sc.eb.no.pen.mat[arrayid,], ncol = 1))
  #if(s2.hat != 0){
  #  soft.const <- (1/(n*s2.hat.sc.eb.no.pen.est.vec[arrayid]*var.T.hat.external))*((sigma.a.sq.hat/sigma.e.sq.hat.sc.eb.vec[arrayid])+(1/(n*s2.hat.sc.eb.no.pen.est.vec[arrayid]*var.T.hat.external)))^(-1)
  #  nie.var <- (quad.form.beta.hat/sigma.a.sq.hat)*(1+(soft.const*quad.form.beta.hat/sigma.e.sq.hat.sc.eb.vec[arrayid]))^(-1) + sigma.e.sq.hat.sc.eb.vec[arrayid]*quad.form.alpha.hat
  #} else if(s2.hat == 0){
  #  s2.threshold <- 0.000001
  #  soft.const <- (1/(n*s2.threshold*var.T.hat.external))*((sigma.a.sq.hat/sigma.e.sq.hat.sc.eb.vec[arrayid])+(1/(n*s2.threshold*var.T.hat.external)))^(-1)
  #  nie.var <- (quad.form.beta.hat/sigma.a.sq.hat)*(1+(soft.const*quad.form.beta.hat/sigma.e.sq.hat.sc.eb.vec[arrayid]))^(-1) + sigma.e.sq.hat.sc.eb.vec[arrayid]*quad.form.alpha.hat
  #}
  if(hard.or.unconstrained.d.vec[arrayid] == "indicator_zero"){
    nie.var <- (quad.form.beta.hat/sigma.a.sq.hat) + sigma.e.sq.hat*quad.form.alpha.hat
  } else if(hard.or.unconstrained.d.vec[arrayid] == "indicator_nonzero"){
    gen.chi.sq <- rchisq(10000, df = 1)
    nu.a.I <- n*var.T.hat.internal
    gen.chi.sq[gen.chi.sq <= 1] <- 0
    chi.sq.term <- mean((1+(sigma.a.sq.hat/sigma.e.sq.hat)*nu.a.I*gen.chi.sq)^(-1))
    nie.var <- (quad.form.beta.hat/sigma.a.sq.hat)*(1+(chi.sq.term*quad.form.beta.hat/sigma.e.sq.hat))^(-1) + sigma.e.sq.hat*quad.form.alpha.hat
  }
  
  asym.vars.sc.eb.no.pen.mat[arrayid,] <- c(nie.var, n*((nde.sc.eb.pb.asym[2] - nde.sc.eb.pb.asym[1])/(2*1.96))^2)
  nie.sc.eb.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.eb.no.pen.est.vec[arrayid] - 1.96*sqrt(nie.var)/sqrt(n) <= nie.true) == (nie.sc.eb.no.pen.est.vec[arrayid] + 1.96*sqrt(nie.var)/sqrt(n) >= nie.true), 1, 0)
  
  #Soft Constraint (s^2 = 0.0001) - Unpenalized
  
  s2.fixed <- 0.0001
  
  beta.m.sc.0.no.pen.est.mat[arrayid,] <- read.csv(paste0("beta.m.hat.sc.0.no.pen.", arrayid, ".csv"))[,-1]
  beta.a.sc.0.no.pen.est.vec[arrayid] <- read.csv(paste0("beta.a.hat.sc.0.no.pen.", arrayid, ".csv"))[,-1]
  alpha.a.sc.0.no.pen.est.mat[arrayid,] <- read.csv(paste0("alpha.a.hat.sc.0.no.pen.", arrayid, ".csv"))[,-1]
  nie.sc.0.no.pen.est.vec[arrayid] <- sum(alpha.a.sc.0.no.pen.est.mat[arrayid,]*beta.m.sc.0.no.pen.est.mat[arrayid,])
  
  sigma.e.sq.hat <- as.numeric(read.csv(paste0("sigma.e.sq.hat.sc.0.", arrayid, ".csv"))[,-1])
  sigma.e.sq.hat.sc.0.vec[arrayid] <- sigma.e.sq.hat
  Sigma.m.hat <- as.matrix(read.csv(paste0("Sigma.m.hat.sc.0.", arrayid, ".csv"))[,-1])
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  
  nie.sc.0.pb.quant <- read.csv(paste0("nie.ci95.param.boot.quantile.sc.0.", arrayid, ".csv"))[,-1]
  nde.sc.0.pb.quant <- read.csv(paste0("nde.ci95.param.boot.quantile.sc.0.", arrayid, ".csv"))[,-1]
  te.sc.0.pb.quant <- read.csv(paste0("te.ci95.param.boot.quantile.sc.0.", arrayid, ".csv"))[,-1]
  
  nie.sc.0.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.0.pb.quant[1] <= nie.true) == (nie.sc.0.pb.quant[2] >= nie.true), 1, 0)
  nde.sc.0.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nde.sc.0.pb.quant[1] <= nde.true) == (nde.sc.0.pb.quant[2] >= nde.true), 1, 0)
  te.sc.0.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((te.sc.0.pb.quant[1] <= nie.true + nde.true) == (te.sc.0.pb.quant[2] >= nie.true + nde.true), 1, 0)
  
  nie.sc.0.pb.asym <- read.csv(paste0("nie.ci95.param.boot.asym.norm.sc.0.", arrayid, ".csv"))[,-1]
  nde.sc.0.pb.asym <- read.csv(paste0("nde.ci95.param.boot.asym.norm.sc.0.", arrayid, ".csv"))[,-1]
  te.sc.0.pb.asym <- read.csv(paste0("te.ci95.param.boot.asym.norm.sc.0.", arrayid, ".csv"))[,-1]
  
  nie.sc.0.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.0.pb.asym[1] <= nie.true) == (nie.sc.0.pb.asym[2] >= nie.true), 1, 0)
  nde.sc.0.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nde.sc.0.pb.asym[1] <= nde.true) == (nde.sc.0.pb.asym[2] >= nde.true), 1, 0)
  te.sc.0.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((te.sc.0.pb.asym[1] <= nie.true + nde.true) == (te.sc.0.pb.asym[2] >= nie.true + nde.true), 1, 0)
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.sc.0.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.sc.0.no.pen.est.mat[arrayid,], ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.sc.0.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.sc.0.no.pen.est.mat[arrayid,], ncol = 1))
  nie.var <- (quad.form.beta.hat/sigma.a.sq.hat)*(1+(1/(n*s2.fixed*var.T.hat.external))*(1/(n*s2.fixed*var.T.hat.external)+(sigma.a.sq.hat/sigma.e.sq.hat))^(-1)*(1/sigma.e.sq.hat)*quad.form.beta.hat)^(-1) + sigma.e.sq.hat*quad.form.alpha.hat
  soft.const <- (1/(n*s2.fixed*var.T.hat.external))*((sigma.a.sq.hat/sigma.e.sq.hat)+(1/(n*s2.fixed*var.T.hat.external)))^(-1)
  
  asym.vars.sc.0.no.pen.mat[arrayid,] <- c(nie.var, n*((nde.sc.0.pb.asym[2] - nde.sc.0.pb.asym[1])/(2*1.96))^2)
  nie.sc.0.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.0.no.pen.est.vec[arrayid] - 1.96*sqrt(nie.var)/sqrt(n) <= nie.true) == (nie.sc.0.no.pen.est.vec[arrayid] + 1.96*sqrt(nie.var)/sqrt(n) >= nie.true), 1, 0)
  
  #Soft Constraint (s^2 = 10000000000) - Unpenalized
  
  s2.fixed <- 10000000000
  
  beta.m.sc.inf.no.pen.est.mat[arrayid,] <- read.csv(paste0("beta.m.hat.sc.inf.no.pen.", arrayid, ".csv"))[,-1]
  beta.a.sc.inf.no.pen.est.vec[arrayid] <- read.csv(paste0("beta.a.hat.sc.inf.no.pen.", arrayid, ".csv"))[,-1]
  alpha.a.sc.inf.no.pen.est.mat[arrayid,] <- read.csv(paste0("alpha.a.hat.sc.inf.no.pen.", arrayid, ".csv"))[,-1]
  nie.sc.inf.no.pen.est.vec[arrayid] <- sum(alpha.a.sc.inf.no.pen.est.mat[arrayid,]*beta.m.sc.inf.no.pen.est.mat[arrayid,])
  
  sigma.e.sq.hat <- as.numeric(read.csv(paste0("sigma.e.sq.hat.sc.inf.", arrayid, ".csv"))[,-1])
  sigma.e.sq.hat.sc.inf.vec[arrayid] <- sigma.e.sq.hat
  Sigma.m.hat <- as.matrix(read.csv(paste0("Sigma.m.hat.sc.inf.", arrayid, ".csv"))[,-1])
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  
  nie.sc.inf.pb.quant <- read.csv(paste0("nie.ci95.param.boot.quantile.sc.inf.", arrayid, ".csv"))[,-1]
  nde.sc.inf.pb.quant <- read.csv(paste0("nde.ci95.param.boot.quantile.sc.inf.", arrayid, ".csv"))[,-1]
  te.sc.inf.pb.quant <- read.csv(paste0("te.ci95.param.boot.quantile.sc.inf.", arrayid, ".csv"))[,-1]
  
  nie.sc.inf.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.inf.pb.quant[1] <= nie.true) == (nie.sc.inf.pb.quant[2] >= nie.true), 1, 0)
  nde.sc.inf.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((nde.sc.inf.pb.quant[1] <= nde.true) == (nde.sc.inf.pb.quant[2] >= nde.true), 1, 0)
  te.sc.inf.pb.quant.no.pen.signif.vec[arrayid] <- ifelse((te.sc.inf.pb.quant[1] <= nie.true + nde.true) == (te.sc.inf.pb.quant[2] >= nie.true + nde.true), 1, 0)
  
  nie.sc.inf.pb.asym <- read.csv(paste0("nie.ci95.param.boot.asym.norm.sc.inf.", arrayid, ".csv"))[,-1]
  nde.sc.inf.pb.asym <- read.csv(paste0("nde.ci95.param.boot.asym.norm.sc.inf.", arrayid, ".csv"))[,-1]
  te.sc.inf.pb.asym <- read.csv(paste0("te.ci95.param.boot.asym.norm.sc.inf.", arrayid, ".csv"))[,-1]
  
  nie.sc.inf.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.inf.pb.asym[1] <= nie.true) == (nie.sc.inf.pb.asym[2] >= nie.true), 1, 0)
  nde.sc.inf.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((nde.sc.inf.pb.asym[1] <= nde.true) == (nde.sc.inf.pb.asym[2] >= nde.true), 1, 0)
  te.sc.inf.pb.asym.no.pen.signif.vec[arrayid] <- ifelse((te.sc.inf.pb.asym[1] <= nie.true + nde.true) == (te.sc.inf.pb.asym[2] >= nie.true + nde.true), 1, 0)
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.sc.inf.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.sc.inf.no.pen.est.mat[arrayid,], ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.sc.inf.no.pen.est.mat[arrayid,], nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.sc.inf.no.pen.est.mat[arrayid,], ncol = 1))
  nie.var <- (quad.form.beta.hat/sigma.a.sq.hat)*(1+(1/(n*s2.fixed*var.T.hat.external))*(1/(n*s2.fixed*var.T.hat.external)+(sigma.a.sq.hat/sigma.e.sq.hat))^(-1)*(1/sigma.e.sq.hat)*quad.form.beta.hat)^(-1) + sigma.e.sq.hat*quad.form.alpha.hat
  soft.const <- (1/(n*s2.fixed*var.T.hat.external))*((sigma.a.sq.hat/sigma.e.sq.hat)+(1/(n*s2.fixed*var.T.hat.external)))^(-1)
  
  asym.vars.sc.inf.no.pen.mat[arrayid,] <- c(nie.var, n*((nde.sc.inf.pb.asym[2] - nde.sc.inf.pb.asym[1])/(2*1.96))^2)
  nie.sc.inf.no.pen.signif.vec[arrayid] <- ifelse((nie.sc.inf.no.pen.est.vec[arrayid] - 1.96*sqrt(nie.var)/sqrt(n) <= nie.true) == (nie.sc.inf.no.pen.est.vec[arrayid] + 1.96*sqrt(nie.var)/sqrt(n) >= nie.true), 1, 0)
  
  #Other Simulation Details
  r2.t.ext.vec[arrayid] <- read.csv(paste0("R2.ext.true.", arrayid, ".csv"))[,-1]
  r2.m.vec[arrayid] <- read.csv(paste0("r2.m.true.", arrayid, ".csv"))[,-1]
  r2.o.vec[arrayid] <- read.csv(paste0("r2.o.true.", arrayid, ".csv"))[,-1]
}

#Convert array to matrix and write matrix to csv file

##Estimates

#Unconstrained
write.csv(beta.m.u.no.pen.est.mat, file = "../results/beta.m.u.no.pen.est.csv")
write.csv(beta.a.u.no.pen.est.vec, file = "../results/nde.u.no.pen.est.csv")
write.csv(alpha.a.u.no.pen.est.mat, file = "../results/alpha.a.u.no.pen.est.csv")
write.csv(nie.u.no.pen.est.vec, file = "../results/nie.u.no.pen.est.csv")
write.csv(sigma.e.sq.hat.u.vec, file = "../results/sigma.e.sq.u.no.pen.est.csv")

write.csv(asym.vars.u.no.pen.mat, file = "../results/vars.u.no.pen.csv")
write.csv(nie.u.no.pen.signif.vec, file = "../results/nie.u.no.pen.coverage.csv")
write.csv(nde.u.no.pen.signif.vec, file = "../results/nde.u.no.pen.coverage.csv")

#Oracle
write.csv(beta.m.o.no.pen.est.mat, file = "../results/beta.m.o.no.pen.est.csv")
write.csv(beta.a.o.no.pen.est.vec, file = "../results/nde.o.no.pen.est.csv")
write.csv(alpha.a.o.no.pen.est.mat, file = "../results/alpha.a.o.no.pen.est.csv")
write.csv(nie.o.no.pen.est.vec, file = "../results/nie.o.no.pen.est.csv")
write.csv(sigma.e.sq.hat.o.vec, file = "../results/sigma.e.sq.o.no.pen.est.csv")

write.csv(asym.vars.o.no.pen.mat, file = "../results/vars.o.no.pen.csv")
write.csv(nie.o.no.pen.signif.vec, file = "../results/nie.o.no.pen.coverage.csv")
write.csv(nde.o.no.pen.signif.vec, file = "../results/nde.o.no.pen.coverage.csv")

#Hard Constraint (External Estimate)
write.csv(beta.m.h.ext.no.pen.est.mat, file = "../results/beta.m.h.ext.no.pen.est.csv")
write.csv(beta.a.h.ext.no.pen.est.vec, file = "../results/nde.h.ext.no.pen.est.csv")
write.csv(alpha.a.h.ext.no.pen.est.mat, file = "../results/alpha.a.h.ext.no.pen.est.csv")
write.csv(nie.h.ext.no.pen.est.vec, file = "../results/nie.h.ext.no.pen.est.csv")
write.csv(sigma.e.sq.hat.h.ext.vec, file = "../results/sigma.e.sq.h.ext.no.pen.est.csv")

write.csv(asym.vars.h.ext.no.pen.mat, file = "../results/vars.h.ext.no.pen.csv")
write.csv(nie.h.ext.no.pen.signif.vec, file = "../results/nie.h.ext.no.pen.coverage.csv")
write.csv(nde.h.ext.no.pen.signif.vec, file = "../results/nde.h.ext.no.pen.coverage.csv")

#Soft Constraint (Optimal MSE)
write.csv(beta.m.hat.sc.opt.mse.no.pen.mat, file = "../results/beta.m.sc.opt.mse.no.pen.est.csv")
write.csv(beta.a.hat.sc.opt.mse.no.pen.vec, file = "../results/nde.sc.opt.mse.no.pen.est.csv")
write.csv(alpha.a.hat.sc.opt.mse.no.pen.mat, file = "../results/alpha.a.sc.opt.mse.no.pen.est.csv")
write.csv(nie.sc.opt.mse.no.pen.est.vec, file = "../results/nie.sc.opt.mse.no.pen.est.csv")
write.csv(sigma.e.sq.hat.sc.opt.mse.vec, file = "../results/sigma.e.sq.sc.opt.mse.no.pen.est.csv")

write.csv(s2.hat.sc.opt.mse.no.pen.est.vec, file = "../results/s2.hat.sc.opt.mse.no.pen.est.csv")

write.csv(asym.vars.sc.opt.mse.no.pen.mat, file = "../results/vars.sc.opt.mse.no.pen.csv")
write.csv(nie.sc.opt.mse.no.pen.signif.vec, file = "../results/nie.sc.opt.mse.no.pen.coverage.csv")

write.csv(nie.sc.opt.mse.pb.quant.no.pen.signif.vec, file = "../results/nie.sc.opt.mse.pb.quant.no.pen.coverage.csv")
write.csv(nde.sc.opt.mse.pb.quant.no.pen.signif.vec, file = "../results/nde.sc.opt.mse.pb.quant.no.pen.coverage.csv")
write.csv(te.sc.opt.mse.pb.quant.no.pen.signif.vec, file = "../results/te.sc.opt.mse.pb.quant.no.pen.coverage.csv")

write.csv(nie.sc.opt.mse.pb.asym.no.pen.signif.vec, file = "../results/nie.sc.opt.mse.pb.asym.no.pen.coverage.csv")
write.csv(nde.sc.opt.mse.pb.asym.no.pen.signif.vec, file = "../results/nde.sc.opt.mse.pb.asym.no.pen.coverage.csv")
write.csv(te.sc.opt.mse.pb.asym.no.pen.signif.vec, file = "../results/te.sc.opt.mse.pb.asym.no.pen.coverage.csv")

#Soft Constraint (EB)
write.csv(beta.m.hat.sc.eb.no.pen.mat, file = "../results/beta.m.sc.eb.no.pen.est.csv")
write.csv(beta.a.hat.sc.eb.no.pen.vec, file = "../results/nde.sc.eb.no.pen.est.csv")
write.csv(alpha.a.hat.sc.eb.no.pen.mat, file = "../results/alpha.a.sc.eb.no.pen.est.csv")
write.csv(nie.sc.eb.no.pen.est.vec, file = "../results/nie.sc.eb.no.pen.est.csv")
write.csv(sigma.e.sq.hat.sc.eb.vec, file = "../results/sigma.e.sq.sc.eb.no.pen.est.csv")

write.csv(s2.hat.sc.eb.no.pen.est.vec, file = "../results/s2.hat.sc.eb.no.pen.est.csv")

write.csv(asym.vars.sc.eb.no.pen.mat, file = "../results/vars.sc.eb.no.pen.csv")
write.csv(nie.sc.eb.no.pen.signif.vec, file = "../results/nie.sc.eb.no.pen.coverage.csv")

write.csv(nie.sc.eb.pb.quant.no.pen.signif.vec, file = "../results/nie.sc.eb.pb.quant.no.pen.coverage.csv")
write.csv(nde.sc.eb.pb.quant.no.pen.signif.vec, file = "../results/nde.sc.eb.pb.quant.no.pen.coverage.csv")
write.csv(te.sc.eb.pb.quant.no.pen.signif.vec, file = "../results/te.sc.eb.pb.quant.no.pen.coverage.csv")

write.csv(nie.sc.eb.pb.asym.no.pen.signif.vec, file = "../results/nie.sc.eb.pb.asym.no.pen.coverage.csv")
write.csv(nde.sc.eb.pb.asym.no.pen.signif.vec, file = "../results/nde.sc.eb.pb.asym.no.pen.coverage.csv")
write.csv(te.sc.eb.pb.asym.no.pen.signif.vec, file = "../results/te.sc.eb.pb.asym.no.pen.coverage.csv")

#Soft Constraint (Discrete)

write.csv(beta.m.hat.d.no.pen.mat, file = "../results/beta.m.d.no.pen.est.csv")
write.csv(beta.a.hat.d.no.pen.vec, file = "../results/nde.d.no.pen.est.csv")
write.csv(alpha.a.hat.d.no.pen.mat, file = "../results/alpha.a.d.no.pen.est.csv")
write.csv(nie.d.no.pen.est.vec, file = "../results/nie.d.no.pen.est.csv")
write.csv(sigma.e.sq.hat.d.vec, file = "../results/sigma.e.sq.d.no.pen.est.csv")

write.csv(nie.d.pb.quant.no.pen.signif.vec, file = "../results/nie.d.pb.quant.no.pen.coverage.csv")
write.csv(nde.d.pb.quant.no.pen.signif.vec, file = "../results/nde.d.pb.quant.no.pen.coverage.csv")
write.csv(te.d.pb.quant.no.pen.signif.vec, file = "../results/te.d.pb.quant.no.pen.coverage.csv")

write.csv(nie.d.pb.asym.no.pen.signif.vec, file = "../results/nie.d.pb.asym.no.pen.coverage.csv")
write.csv(nde.d.pb.asym.no.pen.signif.vec, file = "../results/nde.d.pb.asym.no.pen.coverage.csv")
write.csv(te.d.pb.asym.no.pen.signif.vec, file = "../results/te.d.pb.asym.no.pen.coverage.csv")

write.csv(hard.or.unconstrained.d.vec, file = "../results/hard.or.unconstrained.d.csv")

write.csv(asym.vars.d.pb.no.pen.mat, file = "../results/vars.d.no.pen.csv")

#Soft Constraint (s^2 = 0.0001)
write.csv(beta.m.sc.0.no.pen.est.mat, file = "../results/beta.m.sc.0.no.pen.est.csv")
write.csv(beta.a.sc.0.no.pen.est.vec, file = "../results/nde.sc.0.no.pen.est.csv")
write.csv(alpha.a.sc.0.no.pen.est.mat, file = "../results/alpha.a.sc.0.no.pen.est.csv")
write.csv(nie.sc.0.no.pen.est.vec, file = "../results/nie.sc.0.no.pen.est.csv")
write.csv(sigma.e.sq.hat.sc.0.vec, file = "../results/sigma.e.sq.sc.0.no.pen.est.csv")

write.csv(asym.vars.sc.0.no.pen.mat, file = "../results/vars.sc.0.no.pen.csv")
write.csv(nie.sc.0.no.pen.signif.vec, file = "../results/nie.sc.0.no.pen.coverage.csv")

write.csv(nie.sc.0.pb.quant.no.pen.signif.vec, file = "../results/nie.sc.0.pb.quant.no.pen.coverage.csv")
write.csv(nde.sc.0.pb.quant.no.pen.signif.vec, file = "../results/nde.sc.0.pb.quant.no.pen.coverage.csv")
write.csv(te.sc.0.pb.quant.no.pen.signif.vec, file = "../results/te.sc.0.pb.quant.no.pen.coverage.csv")

write.csv(nie.sc.0.pb.asym.no.pen.signif.vec, file = "../results/nie.sc.0.pb.asym.no.pen.coverage.csv")
write.csv(nde.sc.0.pb.asym.no.pen.signif.vec, file = "../results/nde.sc.0.pb.asym.no.pen.coverage.csv")
write.csv(te.sc.0.pb.asym.no.pen.signif.vec, file = "../results/te.sc.0.pb.asym.no.pen.coverage.csv")

#Soft Constraint (s^2 = 10000000000)
write.csv(beta.m.sc.inf.no.pen.est.mat, file = "../results/beta.m.sc.inf.no.pen.est.csv")
write.csv(beta.a.sc.inf.no.pen.est.vec, file = "../results/nde.sc.inf.no.pen.est.csv")
write.csv(alpha.a.sc.inf.no.pen.est.mat, file = "../results/alpha.a.sc.inf.no.pen.est.csv")
write.csv(nie.sc.inf.no.pen.est.vec, file = "../results/nie.sc.inf.no.pen.est.csv")
write.csv(sigma.e.sq.hat.sc.inf.vec, file = "../results/sigma.e.sq.sc.inf.no.pen.est.csv")

write.csv(asym.vars.sc.inf.no.pen.mat, file = "../results/vars.sc.inf.no.pen.csv")
write.csv(nie.sc.inf.no.pen.signif.vec, file = "../results/nie.sc.inf.no.pen.coverage.csv")

write.csv(nie.sc.inf.pb.quant.no.pen.signif.vec, file = "../results/nie.sc.inf.pb.quant.no.pen.coverage.csv")
write.csv(nde.sc.inf.pb.quant.no.pen.signif.vec, file = "../results/nde.sc.inf.pb.quant.no.pen.coverage.csv")
write.csv(te.sc.inf.pb.quant.no.pen.signif.vec, file = "../results/te.sc.inf.pb.quant.no.pen.coverage.csv")

write.csv(nie.sc.inf.pb.asym.no.pen.signif.vec, file = "../results/nie.sc.inf.pb.asym.no.pen.coverage.csv")
write.csv(nde.sc.inf.pb.asym.no.pen.signif.vec, file = "../results/nde.sc.inf.pb.asym.no.pen.coverage.csv")
write.csv(te.sc.inf.pb.asym.no.pen.signif.vec, file = "../results/te.sc.inf.pb.asym.no.pen.coverage.csv")

#Other Output
write.csv(AtA.vec, "../results/AtA.csv")
write.csv(r2.t.ext.vec, "../results/r2.t.ext.csv")
write.csv(r2.m.vec, "../results/r2.m.csv")
write.csv(r2.o.vec, "../results/r2.o.csv")
write.csv(beta.m.true, "../results/beta.m.true.csv")
write.csv(beta.a.true, "../results/beta.a.true.csv")
write.csv(alpha.a.true, "../results/alpha.a.true.csv")
write.csv(sigma.e.sq.true.vec, "../results/sigma.e.sq.true.csv")
write.csv(n, "../results/n.csv")
write.csv(T.hat.internal.vec, "../results/T.hat.internal.csv")
write.csv(T.hat.external.vec, "../results/T.hat.external.csv")
write.csv(var.T.hat.internal.vec, "../results/var.T.hat.internal.csv")
write.csv(var.T.hat.external.vec, "../results/var.T.hat.external.csv")
