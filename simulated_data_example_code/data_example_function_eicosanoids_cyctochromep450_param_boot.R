library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(MASS)
library(corrplot)

setwd("/filepath/")

external.info <- read.csv("Supplemental Study-Boss-No PROTECT.csv", stringsAsFactors = FALSE)
names(external.info)[names(external.info) == "X"] <- "name"
external.info$name <- toupper(external.info$name)

analysis.fn.eicosanoids <- function(phthalate_visit, metabolite.nm){
  source("implementations.R")
  
  d2 <- read.csv("protect.all.exposures.simulate.csv", stringsAsFactors = FALSE)
  
  eico.info <- read.csv("eic.names.csv", stringsAsFactors = FALSE)
  eico.names <- eico.info$Variable.Name[eico.info$Pathway.Group == "Cytochrome p450 Pathway"]
  
  p.wald.test <- rep(NA, 3)
  
  #Check with Amber: X14_15_ETE (old variable name), X14_15_EET (new variable name)
  names(d2)[names(d2) == "X14_15_EET"] <- "X14_15_ETE"
  
  #Grab mediator at visit 3
  eico.df <- d2 %>% filter(visit == 3) %>% dplyr::select(studyid, all_of(eico.names))
  
  #For now just grab visit
  other.df <- d2 %>% filter(visit == phthalate_visit) %>% dplyr::select(studyid, FINALGA_BEST, isage, edu_cat, preBMI,
                                                                        sg, metabolite.nm)
  
  #Merge datasets
  mediator.model.df <- merge(other.df, eico.df, by = "studyid")
  mediator.model.df <- na.omit(mediator.model.df)
  
  #Subject 2699 has LTE4 = 0, Prior to log-transformation reassign value close to 0
  #mediator.model.df[mediator.model.df$studyid == subset(mediator.model.df, LTE4 == 0)$studyid,]$LTE4 <- 0.0025
  
  get.standardize <- read.csv("phth.pool.Jonathan.csv", stringsAsFactors = FALSE)
  get.standardize.order <- get.standardize[,1]
  get.standardize.iqr <- get.standardize$iqr
  
  C <- model.matrix(~ isage + factor(edu_cat) + preBMI, data = mediator.model.df)
  C <- C[, colnames(C) != "(Intercept)"]
  C[, colnames(C) == "isage"] <- C[, colnames(C) == "isage"]/5
  A <- log(mediator.model.df[[metabolite.nm]])
  A <- A + log((median(mediator.model.df$sg)-1)/(mediator.model.df$sg-1)) #Specific Gravity Adjustment
  A <- (A - mean(A))/(get.standardize.iqr[toupper(get.standardize.order) == metabolite.nm])
  M <- as.matrix(mediator.model.df[, names(mediator.model.df) %in% eico.names])
  M <- scale(log(M))
  Y <- mediator.model.df$FINALGA_BEST
  n <- length(Y)
  sigma.A.sq <- var(lm(A ~ C)$residuals)
  
  int.mod <- lm(Y ~ A + C)
  internal.te.est <- coef(int.mod)[names(coef(int.mod)) == "A"]
  var.internal.te.est <- vcov(int.mod)[row.names(vcov(int.mod)) == "A", colnames(vcov(int.mod)) == "A"]
  
  external.te.est <- external.info$Est[external.info$name == metabolite.nm]
  var.external.te.est <- ((external.info$u95[external.info$name == metabolite.nm] - external.info$l95[external.info$name == metabolite.nm])/(2*1.96))^2
  
  # M.cor <- cor(M)
  # pdf("cytochromep450_correlations_protect.pdf", onefile = FALSE, height = 12, width = 12)
  # corrplot.mixed(M.cor, tl.col = "black", upper = "color")
  # dev.off()
  
  #Unconstrained
  
  final.fit <- unconstrained.unpenalized(Y = Y, M = M, A = A, C = C)
  
  alpha.a.hat <- final.fit$alpha.a.hat
  alpha.c.hat <- final.fit$alpha.c.hat
  beta.m.hat <- final.fit$beta.m.hat
  beta.a.hat <- final.fit$beta.a.hat
  beta.c.hat <- final.fit$beta.c.hat
  
  nie.est <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
  nde.est <- beta.a.hat
  te.est <- nde.est + nie.est
  
  Sigma.m.hat <- final.fit$Sigma.m.hat
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  sigma.e.sq.hat <- final.fit$sigma.e.sq.hat
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.hat, nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.hat, ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.hat, nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.hat, ncol = 1))
  nie.var <- (quad.form.beta.hat/sigma.A.sq) + sigma.e.sq.hat*quad.form.alpha.hat
  nde.var <- (sigma.e.sq.hat/sigma.A.sq) + sigma.e.sq.hat*quad.form.alpha.hat
  te.var <- (sigma.e.sq.hat + quad.form.beta.hat)/sigma.A.sq
  
  r2o <- quad.form.beta.hat/(sigma.e.sq.hat + quad.form.beta.hat)
  r2m <- (alpha.a.hat^2*sigma.A.sq)/(diag(Sigma.m.hat) + alpha.a.hat^2*sigma.A.sq)
  
  asym.var.mat <- rbind(cbind(Sigma.m.hat/sigma.A.sq, matrix(0, nrow = nrow(Sigma.m.hat), ncol = ncol(Sigma.m.hat))),
                        cbind(matrix(0, nrow = nrow(Sigma.m.hat), ncol = ncol(Sigma.m.hat)), sigma.e.sq.hat*Sigma.m.inv.hat))
  wald.test.stat <- n*as.numeric(matrix(c(alpha.a.hat, beta.m.hat), nrow = 1)%*%solve(asym.var.mat)%*%matrix(c(alpha.a.hat, beta.m.hat), ncol = 1))
  
  p.wald.test[1] <- pchisq(wald.test.stat, df = 2*ncol(M), ncp = 0, lower.tail = FALSE)
  
  nie.ci95 <- c(nie.est - 1.96*sqrt(nie.var)/sqrt(n), nie.est + 1.96*sqrt(nie.var)/sqrt(n))
  nde.ci95 <- c(nde.est - 1.96*sqrt(nde.var)/sqrt(n), nde.est + 1.96*sqrt(nde.var)/sqrt(n))
  te.ci95 <- c(te.est - 1.96*sqrt(te.var)/sqrt(n), te.est + 1.96*sqrt(te.var)/sqrt(n))
  
  unconst.summary <- data.frame("met.nm" = metabolite.nm,
                                "n" = length(Y),
                                "param" = c("NIE","NDE","TE"),
                                "est" = c(nie.est,nde.est,te.est),
                                "lcl95" = c(nie.ci95[1],nde.ci95[1],te.ci95[1]),
                                "ucl95" = c(nie.ci95[2],nde.ci95[2],te.ci95[2]))
  
  print("Unconstrained - Done")
  
  #Discrete Selection
  
  temp1 <- ((sigma.e.sq.hat + matrix(beta.m.hat, nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.hat, ncol = 1))/sigma.A.sq)/n
  temp2 <- (internal.te.est - external.te.est)^2 - temp1
  if(temp1 < temp2){
    final.fit <- unconstrained.unpenalized(Y = Y, M = M, A = A, C = C)
    
    alpha.a.hat <- final.fit$alpha.a.hat
    alpha.c.hat <- final.fit$alpha.c.hat
    beta.m.hat <- final.fit$beta.m.hat
    beta.a.hat <- final.fit$beta.a.hat
    beta.c.hat <- final.fit$beta.c.hat
    
    Sigma.m.hat <- final.fit$Sigma.m.hat
    Sigma.m.inv.hat <- solve(Sigma.m.hat)
    sigma.e.sq.hat <- final.fit$sigma.e.sq.hat
    
    choose_eb <- "indicator_zero"
  } else if(temp1 >= temp2){
    plug.in.est <- external.te.est
    
    final.fit <- constrained.unpenalized(Y = Y, M = M, A = A, C = C, T.hat.external = plug.in.est)
    
    alpha.a.hat <- final.fit$alpha.a.hat
    alpha.c.hat <- final.fit$alpha.c.hat
    beta.m.hat <- final.fit$beta.m.hat
    beta.a.hat <- plug.in.est - sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
    beta.c.hat <- final.fit$beta.c.hat
    
    Sigma.m.hat <- final.fit$Sigma.m.hat
    Sigma.m.inv.hat <- solve(Sigma.m.hat)
    sigma.e.sq.hat <- final.fit$sigma.e.sq.hat
    
    choose_eb <- "indicator_nonzero"
  }
  
  n.boot <- 1000
  nie.boot <- rep(NA, n.boot)
  nde.boot <- rep(NA, n.boot)
  te.boot <- rep(NA, n.boot)
  save.boot <- rep(NA, n.boot)
  for(r in 1:n.boot){
    print(r)
    
    epsilon.y <- rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq.hat))
    gen.M <- cbind(1,C)%*%t(alpha.c.hat) + matrix(A, ncol = 1)%*%matrix(alpha.a.hat, nrow = 1) + mvrnorm(n = n, mu = rep(0, ncol(M)), Sigma = Sigma.m.hat)
    gen.Y <- A*beta.a.hat + as.vector(cbind(1, C)%*%beta.c.hat) + as.vector(gen.M%*%beta.m.hat) + epsilon.y
    boot.mod <- lm(gen.Y ~ A + C)
    T.hat.internal.boot <- coef(boot.mod)[names(coef(boot.mod)) == "A"]
    var.T.hat.internal.boot <- vcov(boot.mod)[names(coef(boot.mod)) == "A", names(coef(boot.mod)) == "A"]
    
    init.fit.boot <- unconstrained.unpenalized(Y = gen.Y, M = gen.M, A = A, C = C)
    beta.m.hat.u.no.pen.boot <- init.fit.boot$beta.m.hat
    Sigma.m.hat.u.boot <- init.fit.boot$Sigma.m.hat
    sigma.e.sq.hat.u.boot <- init.fit.boot$sigma.e.sq.hat
    
    temp1 <- ((sigma.e.sq.hat.u.boot + matrix(beta.m.hat.u.no.pen.boot, nrow = 1)%*%Sigma.m.hat.u.boot%*%matrix(beta.m.hat.u.no.pen.boot, ncol = 1))/sigma.A.sq)/n
    temp2 <- max(0, (T.hat.internal.boot - external.te.est)^2 - (var.external.te.est + temp1)) + var.external.te.est
    if(temp1 < temp2){
      final.fit <- unconstrained.unpenalized(Y = gen.Y, M = gen.M, A = A, C = C)
      nde.boot[r] <- final.fit$beta.a.hat
      nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
      te.boot[r] <- nde.boot[r] + nie.boot[r]
    } else if(temp1 >= temp2){
      plug.in.est <- external.te.est
      final.fit <- constrained.unpenalized(Y = gen.Y, M = gen.M, A = A, C = C, T.hat.external = plug.in.est)
      nde.boot[r] <- plug.in.est - sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
      nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
      te.boot[r] <- plug.in.est
    }
  }
  
  nde.est <- beta.a.hat
  delta.nde <- quantile(nde.boot - nde.est, probs = c(0.025,0.975))
  nde.ci95 <- c(nde.est - delta.nde[2], nde.est - delta.nde[1])
  
  nie.est <- sum(alpha.a.hat*beta.m.hat)
  delta.nie <- quantile(nie.boot - nie.est, probs = c(0.025,0.975))
  nie.ci95 <- c(nie.est - delta.nie[2], nie.est - delta.nie[1])
  
  te.est <- nde.est + nie.est
  delta.te <- quantile(te.boot - te.est, probs = c(0.025,0.975))
  te.ci95 <- c(te.est - delta.te[2], te.est - delta.te[1])
  
  soft.discrete.summary <- data.frame("met.nm" = metabolite.nm,
                                      "n" = length(Y),
                                      "param" = c("NIE","NDE","TE"),
                                      "est" = c(nie.est,nde.est,te.est),
                                      "lcl95" = c(nie.ci95[1],nde.ci95[1],te.ci95[1]),
                                      "ucl95" = c(nie.ci95[2],nde.ci95[2],te.ci95[2]))
  
  print("Discrete Selection - Done")
  
  #MESSI (EB)
  
  s2.hat <- choose_s2(Y = Y, M = M, A = A, C = C, T.hat.internal = internal.te.est, var.T.hat.internal = var.internal.te.est, T.hat.external = external.te.est,
                      var.T.hat.external = var.external.te.est, method = "eb")
  if(s2.hat != 0){
    final.fit <- rand.eff.unpenalized(Y = Y, M = M, A = A, C = C, rand.eff.mean = external.te.est, rand.eff.var = s2.hat*var.external.te.est,
                                      T.hat.external = external.te.est, var.T.hat.external = var.external.te.est)
  } else if(s2.hat == 0){
    s2.threshold <- 0.000001
    final.fit <- rand.eff.unpenalized(Y = Y, M = M, A = A, C = C, rand.eff.mean = external.te.est, rand.eff.var = s2.threshold*var.external.te.est,
                                      T.hat.external = external.te.est, var.T.hat.external = var.external.te.est)
  }
  
  alpha.a.hat <- final.fit$alpha.a.hat
  alpha.c.hat <- final.fit$alpha.c.hat
  beta.m.hat <- final.fit$beta.m.hat
  beta.a.hat <- final.fit$beta.a.hat
  beta.c.hat <- final.fit$beta.c.hat
  
  nie.est <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
  nde.est <- beta.a.hat
  te.est <- nde.est + nie.est
  
  Sigma.m.hat <- final.fit$Sigma.m.hat
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  sigma.e.sq.hat <- final.fit$sigma.e.sq.hat
  
  nde.bias.hat <- (sum((A)^2)/final.fit$sigma.e.sq.hat + 1/(s2.hat*var.external.te.est))^(-1)*(internal.te.est*sum((A)^2)/final.fit$sigma.e.sq.hat + external.te.est/(s2.hat*var.external.te.est)) - internal.te.est
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.hat, nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.hat, ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.hat, nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.hat, ncol = 1))
  if(choose_eb == "indicator_zero"){
    nie.var.asym <- (quad.form.beta.hat/sigma.A.sq) + sigma.e.sq.hat*quad.form.alpha.hat
  } else if(choose_eb == "indicator_nonzero"){
    gen.chi.sq <- rchisq(10000, df = 1)
    nu.a.I <- n*var.internal.te.est
    gen.chi.sq[gen.chi.sq <= 1] <- 0
    chi.sq.term <- mean((1+(sigma.A.sq/sigma.e.sq.hat)*nu.a.I*gen.chi.sq)^(-1))
    nie.var.asym <- (quad.form.beta.hat/sigma.A.sq)*(1+(chi.sq.term*quad.form.beta.hat/sigma.e.sq.hat))^(-1) + sigma.e.sq.hat*quad.form.alpha.hat
  }
  
  n.boot <- 1000
  s2.boot <- rep(NA, n.boot)
  nie.boot <- rep(NA, n.boot)
  nde.boot <- rep(NA, n.boot)
  te.boot <- rep(NA, n.boot)
  save.boot <- rep(NA, n.boot)
  set.seed(20230118)
  for(r in 1:n.boot){
    print(r)
    epsilon.y <- rnorm(n = n, mean = 0, sd = sqrt(sigma.e.sq.hat))
    gen.M <- cbind(1,C)%*%t(alpha.c.hat) + matrix(A, ncol = 1)%*%matrix(alpha.a.hat, nrow = 1) + mvrnorm(n = n, mu = rep(0, ncol(M)), Sigma = Sigma.m.hat)
    gen.Y <- A*beta.a.hat + as.vector(cbind(1, C)%*%beta.c.hat) + as.vector(gen.M%*%beta.m.hat) + epsilon.y
    boot.mod <- lm(gen.Y ~ A + C)
    T.hat.internal.boot <- coef(boot.mod)[names(coef(boot.mod)) == "A"]
    var.T.hat.internal.boot <- vcov(boot.mod)[names(coef(boot.mod)) == "A", names(coef(boot.mod)) == "A"]
    s2.hat.boot <- choose_s2(Y = gen.Y, M = gen.M, A = A, C = C, T.hat.internal = T.hat.internal.boot, var.T.hat.internal = var.T.hat.internal.boot, T.hat.external = external.te.est, var.T.hat.external = var.external.te.est, method = "eb")
    if(s2.hat.boot != 0){
      final.fit <- rand.eff.unpenalized(Y = gen.Y, M = gen.M, A = A, C = C, rand.eff.mean = external.te.est, rand.eff.var = s2.hat.boot*var.external.te.est,
                                        T.hat.external = external.te.est, var.T.hat.external = var.external.te.est)
    } else if(s2.hat.boot == 0){
      s2.threshold <- 0.000001
      final.fit <- rand.eff.unpenalized(Y = gen.Y, M = gen.M, A = A, C = C, rand.eff.mean = external.te.est, rand.eff.var = s2.threshold*var.external.te.est,
                                        T.hat.external = external.te.est, var.T.hat.external = var.external.te.est)
    }
    s2.boot[r] <- s2.hat.boot
    nde.boot[r] <- final.fit$beta.a.hat
    nie.boot[r] <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
    te.boot[r] <- nde.boot[r] + nie.boot[r]
    
    save.boot[r] <- (sum(A^2)/(n*final.fit$sigma.e.sq.hat) + (1/(n*s2.hat.boot*external.te.est)))^(-1)*((sum(A^2)*sum(alpha.a.hat*(beta.m.hat - final.fit$beta.m.hat))/(n*final.fit$sigma.e.sq.hat)) + (sum(A*epsilon.y)/(n*final.fit$sigma.e.sq.hat)) + (sum(alpha.a.hat*beta.m.hat) - sum(final.fit$alpha.a.hat*final.fit$beta.m.hat))/(n*s2.hat.boot*external.te.est))
  }
  
  nie.ci95 <- c(nie.est - 1.96*sqrt(nie.var.asym/n), nie.est + 1.96*sqrt(nie.var.asym/n))
  
  delta.te <- quantile(te.boot - te.est, probs = c(0.025,0.975))
  te.ci95 <- c(te.est - delta.te[2], te.est - delta.te[1])
  
  delta.nde <- quantile(nde.boot - nde.est, probs = c(0.025,0.975))
  nde.ci95 <- c(nde.est - delta.nde[2], nde.est - delta.nde[1])
  
  soft.summary <- data.frame("met.nm" = metabolite.nm,
                             "n" = length(Y),
                             "param" = c("NIE","NDE","TE"),
                             "est" = c(nie.est,nde.est,te.est),
                             "lcl95" = c(nie.ci95[1],nde.ci95[1],te.ci95[1]),
                             "ucl95" = c(nie.ci95[2],nde.ci95[2],te.ci95[2]))
  
  print("MESSI (EB) - Done")
  
  #Hard Constraint
  
  external.te.est <- external.info$Est[external.info$name == metabolite.nm]
  var.external.te.est <- ((external.info$u95[external.info$name == metabolite.nm] - external.info$l95[external.info$name == metabolite.nm])/(2*1.96))^2
  
  final.fit <- constrained.unpenalized(Y = Y, M = M, A = A, C = C, T.hat.external = external.te.est)
  
  alpha.a.hat <- final.fit$alpha.a.hat
  alpha.c.hat <- final.fit$alpha.c.hat
  beta.m.hat <- final.fit$beta.m.hat
  beta.a.hat <- final.fit$beta.a.hat
  beta.c.hat <- final.fit$beta.c.hat
  
  nie.est <- sum(final.fit$alpha.a.hat*final.fit$beta.m.hat)
  nde.est <- external.te.est - nie.est
  te.est <- external.te.est
  
  Sigma.m.hat <- final.fit$Sigma.m.hat
  Sigma.m.inv.hat <- solve(Sigma.m.hat)
  sigma.e.sq.hat <- final.fit$sigma.e.sq.hat
  
  quad.form.beta.hat <- as.numeric(matrix(beta.m.hat, nrow = 1)%*%Sigma.m.hat%*%matrix(beta.m.hat, ncol = 1))
  quad.form.alpha.hat <- as.numeric(matrix(alpha.a.hat, nrow = 1)%*%Sigma.m.inv.hat%*%matrix(alpha.a.hat, ncol = 1))
  nie.var <- (quad.form.beta.hat/sigma.A.sq)*(sigma.e.sq.hat/(sigma.e.sq.hat+quad.form.beta.hat)) + sigma.e.sq.hat*quad.form.alpha.hat
  nde.var <- (sigma.e.sq.hat/sigma.A.sq)*(quad.form.beta.hat/(sigma.e.sq.hat+quad.form.beta.hat)) + sigma.e.sq.hat*quad.form.alpha.hat
  te.var <- 0
  
  asym.var.mat <- rbind(cbind(solve(Sigma.m.inv.hat+(matrix(beta.m.hat, ncol = 1)%*%matrix(beta.m.hat, nrow = 1))/sigma.e.sq.hat)/sigma.A.sq, matrix(0, nrow = nrow(Sigma.m.hat), ncol = ncol(Sigma.m.hat))),
                        cbind(matrix(0, nrow = nrow(Sigma.m.hat), ncol = ncol(Sigma.m.hat)), sigma.e.sq.hat*Sigma.m.inv.hat))
  wald.test.stat <- n*as.numeric(matrix(c(alpha.a.hat, beta.m.hat), nrow = 1)%*%solve(asym.var.mat)%*%matrix(c(alpha.a.hat, beta.m.hat), ncol = 1))
  
  p.wald.test[3] <- pchisq(wald.test.stat, df = 2*ncol(M), ncp = 0, lower.tail = FALSE)
  
  nie.ci95 <- c(nie.est - 1.96*sqrt(nie.var)/sqrt(n), nie.est + 1.96*sqrt(nie.var)/sqrt(n))
  nde.ci95 <- c(nde.est - 1.96*sqrt(nde.var)/sqrt(n), nde.est + 1.96*sqrt(nde.var)/sqrt(n))
  te.ci95 <- c(te.est, te.est)
  
  const.summary <- data.frame("met.nm" = metabolite.nm,
                              "n" = length(Y),
                              "param" = c("NIE","NDE","TE"),
                              "est" = c(nie.est,nde.est,te.est),
                              "lcl95" = c(nie.ci95[1],nde.ci95[1],te.ci95[1]),
                              "ucl95" = c(nie.ci95[2],nde.ci95[2],te.ci95[2]))
  
  print("Hard Constraint - Done")
  
  return(list(unconst.summary, soft.summary, soft.discrete.summary, const.summary, r2o, p.wald.test))
}

###################################################################################
## Visit 1
###################################################################################

set.seed(20221130)

#MBP at Visit 1
phthalate_visit <- 1
metabolite.nm <- "MBP"
mbp.v1.out <- analysis.fn.eicosanoids(phthalate_visit = phthalate_visit, metabolite.nm = metabolite.nm)

#MIBP at Visit 1
phthalate_visit <- 1
metabolite.nm <- "MIBP"
mibp.v1.out <- analysis.fn.eicosanoids(phthalate_visit = phthalate_visit, metabolite.nm = metabolite.nm)

#MBzP at Visit 1
phthalate_visit <- 1
metabolite.nm <- "MBZP"
mbzp.v1.out <- analysis.fn.eicosanoids(phthalate_visit = phthalate_visit, metabolite.nm = metabolite.nm)

###################################################################################
## Visit 2
###################################################################################

set.seed(20221130)

#MBP at Visit 2
phthalate_visit <- 2
metabolite.nm <- "MBP"
mbp.v2.out <- analysis.fn.eicosanoids(phthalate_visit = phthalate_visit, metabolite.nm = metabolite.nm)

#MIBP at Visit 2
phthalate_visit <- 2
metabolite.nm <- "MIBP"
mibp.v2.out <- analysis.fn.eicosanoids(phthalate_visit = phthalate_visit, metabolite.nm = metabolite.nm)

#MBzP at Visit 2
phthalate_visit <- 2
metabolite.nm <- "MBZP"
mbzp.v2.out <- analysis.fn.eicosanoids(phthalate_visit = phthalate_visit, metabolite.nm = metabolite.nm)

###################################################################################
## Summarize Output
###################################################################################

save.image("protect_output_cytochromep450_param_boot.Rdata")

library(ggplot2)

setwd("/filepath/")

load("protect_output_cytochromep450_param_boot.Rdata")

#Plot Results

mbp.v1.out[[1]]$analysis <- "Unconstrained"
mbp.v1.out[[2]]$analysis <- "MESSI-EB"
mbp.v1.out[[3]]$analysis <- "Discrete Selection"
mbp.v1.out[[4]]$analysis <- "Hard Constraint"

mibp.v1.out[[1]]$analysis <- "Unconstrained"
mibp.v1.out[[2]]$analysis <- "MESSI-EB"
mibp.v1.out[[3]]$analysis <- "Discrete Selection"
mibp.v1.out[[4]]$analysis <- "Hard Constraint"

mbzp.v1.out[[1]]$analysis <- "Unconstrained"
mbzp.v1.out[[2]]$analysis <- "MESSI-EB"
mbzp.v1.out[[3]]$analysis <- "Discrete Selection"
mbzp.v1.out[[4]]$analysis <- "Hard Constraint"

mbp.v2.out[[1]]$analysis <- "Unconstrained"
mbp.v2.out[[2]]$analysis <- "MESSI-EB"
mbp.v2.out[[3]]$analysis <- "Discrete Selection"
mbp.v2.out[[4]]$analysis <- "Hard Constraint"

mibp.v2.out[[1]]$analysis <- "Unconstrained"
mibp.v2.out[[2]]$analysis <- "MESSI-EB"
mibp.v2.out[[3]]$analysis <- "Discrete Selection"
mibp.v2.out[[4]]$analysis <- "Hard Constraint"

mbzp.v2.out[[1]]$analysis <- "Unconstrained"
mbzp.v2.out[[2]]$analysis <- "MESSI-EB"
mbzp.v2.out[[3]]$analysis <- "Discrete Selection"
mbzp.v2.out[[4]]$analysis <- "Hard Constraint"

mbp.v1.out.df <- rbind(mbp.v1.out[[1]], mbp.v1.out[[2]], mbp.v1.out[[3]], mbp.v1.out[[4]])
mibp.v1.out.df <- rbind(mibp.v1.out[[1]], mibp.v1.out[[2]], mibp.v1.out[[3]], mibp.v1.out[[4]])
mbzp.v1.out.df <- rbind(mbzp.v1.out[[1]], mbzp.v1.out[[2]], mbzp.v1.out[[3]], mbzp.v1.out[[4]])

mbp.v2.out.df <- rbind(mbp.v2.out[[1]], mbp.v2.out[[2]], mbp.v2.out[[3]], mbp.v2.out[[4]])
mibp.v2.out.df <- rbind(mibp.v2.out[[1]], mibp.v2.out[[2]], mibp.v2.out[[3]], mibp.v2.out[[4]])
mbzp.v2.out.df <- rbind(mbzp.v2.out[[1]], mbzp.v2.out[[2]], mbzp.v2.out[[3]], mbzp.v2.out[[4]])

mbp.v1.out.df$label <- paste0("MBP", " V", 1)
mibp.v1.out.df$label <- paste0("MiBP", " V", 1)
mbzp.v1.out.df$label <- paste0("MBzP", " V", 1)

mbp.v2.out.df$label <- paste0("MBP", " V", 2)
mibp.v2.out.df$label <- paste0("MiBP", " V", 2)
mbzp.v2.out.df$label <- paste0("MBzP", " V", 2)

ggplot.df <- rbind(mbp.v1.out.df, mibp.v1.out.df, mbzp.v1.out.df, mbp.v2.out.df, mibp.v2.out.df, mbzp.v2.out.df)
ggplot.df$param[ggplot.df$param == "NDE"] <- "DE"
ggplot.df$param[ggplot.df$param == "NIE"] <- "IE"
ggplot.df$plot.nm <- paste0(ggplot.df$analysis, " ", ggplot.df$param)
ggplot.df$color.sig <- "black"
ggplot.df$color.sig[sign(ggplot.df$lcl95) == sign(ggplot.df$ucl95)] <- "darkred"

ggplot.df$plot.nm <- factor(ggplot.df$plot.nm, levels = c("Unconstrained TE", "MESSI-EB TE", "Discrete Selection TE", "Hard Constraint TE",
                                                          "Unconstrained DE", "MESSI-EB DE", "Discrete Selection DE", "Hard Constraint DE",
                                                          "Unconstrained IE", "MESSI-EB IE", "Discrete Selection IE", "Hard Constraint IE"))

ann_box <- data.frame(xmin = c(-0.6,-0.6), xmax = c(0.2,0.2), ymin = rep("Unconstrained IE", 2), ymax = rep("Hard Constraint IE", 2), label = c("MBzP V1", "MBzP V2"))

pdf("protect_results_cytochromep450_param_boot_simulate.pdf", onefile = FALSE, height = 8, width = 10)
ggplot(ggplot.df, aes(x = est, y = plot.nm, xmin = lcl95, xmax = ucl95, color = factor(color.sig))) +
  geom_pointrange(shape = 16, fill = "black") +
  geom_vline(xintercept = 0, linetype = 3) + facet_grid(~ label) + theme_bw() +
  scale_color_manual(values = c("black","red2")) +
  scale_x_continuous(breaks = c(-0.9, -0.6, -0.3, 0, 0.3), limits = c(-0.9,0.3)) +
  theme(axis.title.y = element_blank(), axis.title.x = element_blank(), plot.title = element_text(hjust = 0.5),
        legend.position = "none", panel.spacing.x = unit(8, "mm"))
dev.off()

###################################################################################
## Descriptive Statistics - Visit 1 Phthalates and Visit 3 Eicosanoids
###################################################################################

#Visit 1 and Visit 3
phthalate_visit <- 1
metabolite.nm <- c("MBP", "MIBP", "MBZP")
d2 <- read.csv("protect.all.exposures.simulate.csv", stringsAsFactors = FALSE)

eico.info <- read.csv("eic.names.csv", stringsAsFactors = FALSE)
eico.names <- eico.info$Variable.Name[eico.info$Pathway.Group == "Cytochrome p450 Pathway"]

#Check with Amber: X14_15_ETE (old variable name), X14_15_EET (new variable name)
names(d2)[names(d2) == "X14_15_EET"] <- "X14_15_ETE"

#Grab mediator at visit 3
eico.df <- d2 %>% filter(visit == 3) %>% dplyr::select(studyid, all_of(eico.names))
eico.df <- na.omit(eico.df)

#For now just grab visit
other.df <- d2 %>% filter(visit == phthalate_visit) %>% dplyr::select(studyid, FINALGA_BEST, isage, edu_cat, preBMI,
                                                                      sg, all_of(metabolite.nm))

#Merge datasets
mediator.model.df <- merge(other.df, eico.df, by = "studyid")
mediator.model.df <- mediator.model.df %>% filter(!is.na(FINALGA_BEST), !is.na(isage), !is.na(edu_cat),
                                                  !is.na(preBMI), !is.na(sg))
mediator.model.df <- mediator.model.df %>% filter((!is.na(MBP) | !is.na(MIBP)) | !is.na(MBZP))

mediator.model.df$preterm <- as.numeric(mediator.model.df$FINALGA_BEST < 37)

#Descriptive Statistics Table

mediator.model.df %>% summarize(mean(isage), sd(isage))
mediator.model.df %>% group_by(preterm) %>% summarize(mean(isage), sd(isage))
round(t.test(isage ~ preterm, data = mediator.model.df)$p.value, digits = 3)

mediator.model.df %>% summarize(mean(preBMI), sd(preBMI))
mediator.model.df %>% group_by(preterm) %>% summarize(mean(preBMI), sd(preBMI))
round(t.test(preBMI ~ preterm, data = mediator.model.df)$p.value, digits = 3)

mediator.model.df %>% group_by(edu_cat) %>% tally() %>% mutate("perc" = round(100*n/sum(n), digits = 1))
mediator.model.df %>% group_by(preterm, edu_cat) %>% tally() %>% mutate("perc" = round(100*n/sum(n), digits = 1))
round(chisq.test(table(mediator.model.df$preterm, mediator.model.df$edu_cat))$p.value, digits = 3)

###################################################################################
## Descriptive Statistics - Visit 2 Phthalates and Visit 3 Eicosanoids
###################################################################################

#Visit 2 and Visit 3
phthalate_visit <- 2
metabolite.nm <- c("MBP", "MIBP", "MBZP")
d2 <- read.csv("protect.all.exposures.simulate.csv", stringsAsFactors = FALSE)

eico.info <- read.csv("eic.names.csv", stringsAsFactors = FALSE)
eico.names <- eico.info$Variable.Name[eico.info$Pathway.Group == "Cytochrome p450 Pathway"]

#Check with Amber: X14_15_ETE (old variable name), X14_15_EET (new variable name)
names(d2)[names(d2) == "X14_15_EET"] <- "X14_15_ETE"

#Grab mediator at visit 3
eico.df <- d2 %>% filter(visit == 3) %>% dplyr::select(studyid, all_of(eico.names))
eico.df <- na.omit(eico.df)

#For now just grab visit
other.df <- d2 %>% filter(visit == phthalate_visit) %>% dplyr::select(studyid, FINALGA_BEST, isage, edu_cat, preBMI,
                                                                      sg, all_of(metabolite.nm))

#Merge datasets
mediator.model.df <- merge(other.df, eico.df, by = "studyid")
mediator.model.df <- mediator.model.df %>% filter(!is.na(FINALGA_BEST), !is.na(isage), !is.na(edu_cat),
                                                  !is.na(preBMI), !is.na(sg))
mediator.model.df <- mediator.model.df %>% filter((!is.na(MBP) | !is.na(MIBP)) | !is.na(MBZP))

mediator.model.df$preterm <- as.numeric(mediator.model.df$FINALGA_BEST < 37)

#Descriptive Statistics Table

mediator.model.df %>% summarize(mean(isage), sd(isage))
mediator.model.df %>% group_by(preterm) %>% summarize(mean(isage), sd(isage))
round(t.test(isage ~ preterm, data = mediator.model.df)$p.value, digits = 3)

mediator.model.df %>% summarize(mean(preBMI), sd(preBMI))
mediator.model.df %>% group_by(preterm) %>% summarize(mean(preBMI), sd(preBMI))
round(t.test(preBMI ~ preterm, data = mediator.model.df)$p.value, digits = 3)

mediator.model.df %>% group_by(edu_cat) %>% tally() %>% mutate("perc" = round(100*n/sum(n), digits = 1))
mediator.model.df %>% group_by(preterm, edu_cat) %>% tally() %>% mutate("perc" = round(100*n/sum(n), digits = 1))
round(chisq.test(table(mediator.model.df$preterm, mediator.model.df$edu_cat))$p.value, digits = 3)
