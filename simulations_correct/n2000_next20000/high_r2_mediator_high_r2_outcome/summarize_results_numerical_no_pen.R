setwd("/filepath/simulations_correct/n2000_next20000/high_r2_mediator_high_r2_outcome/results/")

n <- read.csv("n.csv")[,-1]

nie.true <- apply(read.csv("alpha.a.true.csv")[,-1]*read.csv("beta.m.true.csv")[,-1], 1, sum)
nie.est.sc.opt.mse <- read.csv("nie.sc.opt.mse.no.pen.est.csv")[,-1]
nie.est.h.ext <- read.csv("nie.h.ext.no.pen.est.csv")[,-1]
nie.est.o <- read.csv("nie.o.no.pen.est.csv")[,-1]
nie.est.sc.eb <- read.csv("nie.sc.eb.no.pen.est.csv")[,-1]
nie.est.u <- read.csv("nie.u.no.pen.est.csv")[,-1]
nie.est.sc.0 <- read.csv("nie.sc.0.no.pen.est.csv")[,-1]
nie.est.sc.inf <- read.csv("nie.sc.inf.no.pen.est.csv")[,-1]
nie.est.d <- read.csv("nie.d.no.pen.est.csv")[,-1]

nie.cov.sc.opt.mse.asym.norm <- read.csv("nie.sc.opt.mse.no.pen.coverage.csv")[,-1]
nie.cov.h.ext <- read.csv("nie.h.ext.no.pen.coverage.csv")[,-1]
nie.cov.o <- read.csv("nie.o.no.pen.coverage.csv")[,-1]
nie.cov.sc.eb <- read.csv("nie.sc.eb.no.pen.coverage.csv")[,-1]
nie.cov.sc.eb.pb.quant <- read.csv("nie.sc.eb.pb.quant.no.pen.coverage.csv")[,-1]
nie.cov.sc.eb.pb.asym <- read.csv("nie.sc.eb.pb.asym.no.pen.coverage.csv")[,-1]
nie.cov.u <- read.csv("nie.u.no.pen.coverage.csv")[,-1]
nie.cov.sc.0 <- read.csv("nie.sc.0.no.pen.coverage.csv")[,-1]
nie.cov.sc.inf <- read.csv("nie.sc.inf.no.pen.coverage.csv")[,-1]
nie.cov.sc.opt.mse.pb.quant <- read.csv("nie.sc.opt.mse.pb.quant.no.pen.coverage.csv")[,-1]
nie.cov.sc.opt.mse.pb.asym <- read.csv("nie.sc.opt.mse.pb.asym.no.pen.coverage.csv")[,-1]
nie.cov.d.pb.quant <- read.csv("nie.d.pb.quant.no.pen.coverage.csv")[,-1]
nie.cov.d.pb.asym <- read.csv("nie.d.pb.asym.no.pen.coverage.csv")[,-1]

nde.true <- read.csv("beta.a.true.csv")[,-1]
nde.est.sc.opt.mse <- read.csv("nde.sc.opt.mse.no.pen.est.csv")[,-1]
nde.est.h.ext <- read.csv("nde.h.ext.no.pen.est.csv")[,-1]
nde.est.o <- read.csv("nde.o.no.pen.est.csv")[,-1]
nde.est.sc.eb <- read.csv("nde.sc.eb.no.pen.est.csv")[,-1]
nde.est.u <- read.csv("nde.u.no.pen.est.csv")[,-1]
nde.est.sc.0 <- read.csv("nde.sc.0.no.pen.est.csv")[,-1]
nde.est.sc.inf <- read.csv("nde.sc.inf.no.pen.est.csv")[,-1]
nde.est.d <- read.csv("nde.d.no.pen.est.csv")[,-1]

nde.cov.h.ext <- read.csv("nde.h.ext.no.pen.coverage.csv")[,-1]
nde.cov.o <- read.csv("nde.o.no.pen.coverage.csv")[,-1]
nde.cov.sc.eb.pb.quant <- read.csv("nde.sc.eb.pb.quant.no.pen.coverage.csv")[,-1]
nde.cov.sc.eb.pb.asym <- read.csv("nde.sc.eb.pb.asym.no.pen.coverage.csv")[,-1]
nde.cov.u <- read.csv("nde.u.no.pen.coverage.csv")[,-1]
nde.cov.sc.0.pb.quant <- read.csv("nde.sc.0.pb.quant.no.pen.coverage.csv")[,-1]
nde.cov.sc.0.pb.asym <- read.csv("nde.sc.0.pb.asym.no.pen.coverage.csv")[,-1]
nde.cov.sc.inf.pb.quant <- read.csv("nde.sc.inf.pb.quant.no.pen.coverage.csv")[,-1]
nde.cov.sc.inf.pb.asym <- read.csv("nde.sc.inf.pb.asym.no.pen.coverage.csv")[,-1]
nde.cov.sc.opt.mse.pb.quant <- read.csv("nde.sc.opt.mse.pb.quant.no.pen.coverage.csv")[,-1]
nde.cov.sc.opt.mse.pb.asym <- read.csv("nde.sc.opt.mse.pb.asym.no.pen.coverage.csv")[,-1]
nde.cov.d.pb.quant <- read.csv("nde.d.pb.quant.no.pen.coverage.csv")[,-1]
nde.cov.d.pb.asym <- read.csv("nde.d.pb.asym.no.pen.coverage.csv")[,-1]

vars.sc.opt.mse <- read.csv("vars.sc.opt.mse.no.pen.csv")[,-1]
vars.h.ext <- read.csv("vars.h.ext.no.pen.csv")[,-1]
vars.o <- read.csv("vars.o.no.pen.csv")[,-1]
vars.sc.eb <- read.csv("vars.sc.eb.no.pen.csv")[,-1]
vars.u <- read.csv("vars.u.no.pen.csv")[,-1]
vars.sc.0 <- read.csv("vars.sc.0.no.pen.csv")[,-1]
vars.sc.inf <- read.csv("vars.sc.inf.no.pen.csv")[,-1]
vars.sc.d <- read.csv("vars.d.no.pen.csv")[,-1]

#Calculate Bias

#Bias NIE
bias.nie.sc.opt.mse <- mean(nie.est.sc.opt.mse) - nie.true[1]
bias.nie.h.ext <- mean(nie.est.h.ext) - nie.true[1]
bias.nie.o <- mean(nie.est.o) - nie.true[1]
bias.nie.sc.eb <- mean(nie.est.sc.eb) - nie.true[1]
bias.nie.u <- mean(nie.est.u) - nie.true[1]
bias.nie.sc.0 <- mean(nie.est.sc.0) - nie.true[1]
bias.nie.sc.inf <- mean(nie.est.sc.inf) - nie.true[1]
bias.nie.d <- mean(nie.est.d) - nie.true[1]

#Bias NDE
bias.nde.sc.opt.mse <- mean(nde.est.sc.opt.mse) - nde.true[1]
bias.nde.h.ext <- mean(nde.est.h.ext) - nde.true[1]
bias.nde.o <- mean(nde.est.o) - nde.true[1]
bias.nde.sc.eb <- mean(nde.est.sc.eb) - nde.true[1]
bias.nde.u <- mean(nde.est.u) - nde.true[1]
bias.nde.sc.0 <- mean(nde.est.sc.0) - nde.true[1]
bias.nde.sc.inf <- mean(nde.est.sc.inf) - nde.true[1]
bias.nde.d <- mean(nde.est.d) - nde.true[1]

#Calculate MSE

#MSE NIE
mse.nie.sc.opt.mse <- mean((nie.est.sc.opt.mse - nie.true)^2)
mse.nie.h.ext <- mean((nie.est.h.ext - nie.true)^2)
mse.nie.o <- mean((nie.est.o - nie.true)^2)
mse.nie.sc.eb <- mean((nie.est.sc.eb - nie.true)^2)
mse.nie.u <- mean((nie.est.u - nie.true)^2)
mse.nie.sc.0 <- mean((nie.est.sc.0 - nie.true)^2)
mse.nie.sc.inf <- mean((nie.est.sc.inf - nie.true)^2)
mse.nie.d <- mean((nie.est.d - nie.true)^2)

#MSE NDE
mse.nde.sc.opt.mse <- mean((nde.est.sc.opt.mse - nde.true)^2)
mse.nde.h.ext <- mean((nde.est.h.ext - nde.true)^2)
mse.nde.o <- mean((nde.est.o - nde.true)^2)
mse.nde.sc.eb <- mean((nde.est.sc.eb - nde.true)^2)
mse.nde.u <- mean((nde.est.u - nde.true)^2)
mse.nde.sc.0 <- mean((nde.est.sc.0 - nde.true)^2)
mse.nde.sc.inf <- mean((nde.est.sc.inf - nde.true)^2)
mse.nde.d <- mean((nde.est.d - nde.true)^2)

#Calculate Empirical SD

#Empirical SD NIE
e.sd.nie.sc.opt.mse <- sd(nie.est.sc.opt.mse)
e.sd.nie.h.ext <- sd(nie.est.h.ext)
e.sd.nie.o <- sd(nie.est.o)
e.sd.nie.sc.eb <- sd(nie.est.sc.eb)
e.sd.nie.u <- sd(nie.est.u)
e.sd.nie.sc.0 <- sd(nie.est.sc.0)
e.sd.nie.sc.inf <- sd(nie.est.sc.inf)
e.sd.nie.d <- sd(nie.est.d)

#Empirical SD NDE
e.sd.nde.sc.opt.mse <- sd(nde.est.sc.opt.mse)
e.sd.nde.h.ext <- sd(nde.est.h.ext)
e.sd.nde.o <- sd(nde.est.o)
e.sd.nde.sc.eb <- sd(nde.est.sc.eb)
e.sd.nde.u <- sd(nde.est.u)
e.sd.nde.sc.0 <- sd(nde.est.sc.0)
e.sd.nde.sc.inf <- sd(nde.est.sc.inf)
e.sd.nde.d <- sd(nde.est.d)

#Calculate Average SE (First number is NIE, second number is NDE)
avg.se.h.ext <- apply(sqrt(vars.h.ext/n), 2, mean)
avg.se.o <- apply(sqrt(vars.o/n), 2, mean)
avg.se.sc.eb <- apply(sqrt(vars.sc.eb/n), 2, mean)
avg.se.u <- apply(sqrt(vars.u/n), 2, mean)
avg.se.sc.0 <- apply(sqrt(vars.sc.0/n), 2, mean)
avg.se.sc.inf <- apply(sqrt(vars.sc.inf/n), 2, mean)
avg.se.d <- apply(sqrt(vars.sc.d/n), 2, mean)

#Estimated Coverage Probability

#Coverage Probability NIE
cp.nie.sc.opt.mse.asym.norm <- mean(nie.cov.sc.opt.mse.asym.norm)
cp.nie.h.ext <- mean(nie.cov.h.ext)
cp.nie.o <- mean(nie.cov.o)
cp.nie.sc.eb <- mean(nie.cov.sc.eb)
cp.nie.sc.eb.pb.quant <- mean(nie.cov.sc.eb.pb.quant)
cp.nie.sc.eb.pb.asym <- mean(nie.cov.sc.eb.pb.asym)
cp.nie.u <- mean(nie.cov.u)
cp.nie.sc.0 <- mean(nie.cov.sc.0)
cp.nie.sc.inf <- mean(nie.cov.sc.inf)
cp.nie.sc.opt.mse.pb.quant <- mean(nie.cov.sc.opt.mse.pb.quant)
cp.nie.sc.opt.mse.pb.asym <- mean(nie.cov.sc.opt.mse.pb.asym)
cp.nie.d.pb.quant <- mean(nie.cov.d.pb.quant)
cp.nie.d.pb.asym <- mean(nie.cov.d.pb.asym)

#Coverage Probability NDE
cp.nde.h.ext <- mean(nde.cov.h.ext)
cp.nde.o <- mean(nde.cov.o)
cp.nde.sc.eb.pb.quant <- mean(nde.cov.sc.eb.pb.quant)
cp.nde.sc.eb.pb.asym <- mean(nde.cov.sc.eb.pb.asym)
cp.nde.u <- mean(nde.cov.u)
cp.nde.sc.0.pb.quant <- mean(nde.cov.sc.0.pb.quant)
cp.nde.sc.0.pb.asym <- mean(nde.cov.sc.0.pb.asym)
cp.nde.sc.inf.pb.quant <- mean(nde.cov.sc.inf.pb.quant)
cp.nde.sc.inf.pb.asym <- mean(nde.cov.sc.inf.pb.asym)
cp.nde.sc.opt.mse.pb.quant <- mean(nde.cov.sc.opt.mse.pb.quant)
cp.nde.sc.opt.mse.pb.asym <- mean(nde.cov.sc.opt.mse.pb.asym)
cp.nde.d.pb.quant <- mean(nde.cov.d.pb.quant)
cp.nde.d.pb.asym <- mean(nde.cov.d.pb.asym)

setwd("/filepath/simulations_correct/n2000_next20000/high_r2_mediator_high_r2_outcome/")

#Output Simulation Summary
method.lbl.nie <- c("Unconstrained", "Soft Constraint (EB) Asym Norm", "Soft Constraint (EB) PB Quantile", "Soft Constraint (EB) PB Asym",
                    "Soft Constraint (s^2 = 0.0001)", "Soft Constraint (s^2 = 10000000000)",
                    "Hard Constraint (External)", "Hard Constraint (Oracle)",
                    "Soft Constraint (Discrete) PB Quantile", "Soft Constraint (Discrete) PB Asym")

bias.nie <- c(bias.nie.u, bias.nie.sc.eb, bias.nie.sc.eb, bias.nie.sc.eb, bias.nie.sc.0, bias.nie.sc.inf, bias.nie.h.ext, bias.nie.o, bias.nie.d, bias.nie.d)
mse.nie <- c(mse.nie.u, mse.nie.sc.eb, mse.nie.sc.eb, mse.nie.sc.eb, mse.nie.sc.0, mse.nie.sc.inf, mse.nie.h.ext, mse.nie.o, mse.nie.d, mse.nie.d)
e.sd.nie <- c(e.sd.nie.u, e.sd.nie.sc.eb, e.sd.nie.sc.eb, e.sd.nie.sc.eb, e.sd.nie.sc.0, e.sd.nie.sc.inf, e.sd.nie.h.ext, e.sd.nie.o, e.sd.nie.d, e.sd.nie.d)
avg.se.nie <- c(avg.se.u[1], avg.se.sc.eb[1], avg.se.sc.eb[1], avg.se.sc.eb[1], avg.se.sc.0[1], avg.se.sc.inf[1], avg.se.h.ext[1], avg.se.o[1], avg.se.d[1], avg.se.d[1])
cp.nie <- c(cp.nie.u, cp.nie.sc.eb, cp.nie.sc.eb.pb.quant, cp.nie.sc.eb.pb.asym, cp.nie.sc.0, cp.nie.sc.inf, cp.nie.h.ext, cp.nie.o, cp.nie.d.pb.quant, cp.nie.d.pb.asym)
nie.summary <- data.frame("method" = method.lbl.nie, "bias" = bias.nie, "mse" = mse.nie, "emp_sd" = e.sd.nie, "avg_se" = avg.se.nie, "cp" = cp.nie)

write.csv(nie.summary, "nie_summary_no_pen.csv")

method.lbl.nde <- c("Unconstrained", "Soft Constraint (EB) PB Quantile", "Soft Constraint (EB) PB Asym",
                    "Soft Constraint (s^2 = 0.0001) PB Quantile", "Soft Constraint (s^2 = 0.0001) PB Asym",
                    "Soft Constraint (s^2 = 10000000000) PB Quantile", "Soft Constraint (s^2 = 10000000000) PB Asym",
                    "Hard Constraint (External)", "Hard Constraint (Oracle)",
                    "Soft Constraint (Discrete) PB Quantile", "Soft Constraint (Discrete) PB Asym")

bias.nde <- c(bias.nde.u, bias.nde.sc.eb, bias.nde.sc.eb, bias.nde.sc.0, bias.nde.sc.0, bias.nde.sc.inf, bias.nde.sc.inf, bias.nde.h.ext, bias.nde.o, bias.nde.d, bias.nde.d)
mse.nde <- c(mse.nde.u, mse.nde.sc.eb, mse.nde.sc.eb, mse.nde.sc.0, mse.nde.sc.0, mse.nde.sc.inf, mse.nde.sc.inf, mse.nde.h.ext, mse.nde.o, mse.nde.d, mse.nde.d)
e.sd.nde <- c(e.sd.nde.u, e.sd.nde.sc.eb, e.sd.nde.sc.eb, e.sd.nde.sc.0, e.sd.nde.sc.0, e.sd.nde.sc.inf, e.sd.nde.sc.inf, e.sd.nde.h.ext, e.sd.nde.o, e.sd.nde.d, e.sd.nde.d)
avg.se.nde <- c(avg.se.u[2], avg.se.sc.eb[2], avg.se.sc.eb[2], avg.se.sc.0[2], avg.se.sc.0[2],
                avg.se.sc.inf[2], avg.se.sc.inf[2], avg.se.h.ext[2], avg.se.o[2], avg.se.d[2], avg.se.d[2])
cp.nde <- c(cp.nde.u, cp.nde.sc.eb.pb.quant, cp.nde.sc.eb.pb.asym, cp.nde.sc.0.pb.quant, cp.nde.sc.0.pb.asym, cp.nde.sc.inf.pb.quant, cp.nde.sc.inf.pb.asym,
            cp.nde.h.ext, cp.nde.o, cp.nde.d.pb.quant, cp.nde.d.pb.asym)
nde.summary <- data.frame("method" = method.lbl.nde, "bias" = bias.nde, "mse" = mse.nde, "emp_sd" = e.sd.nde, "avg_se" = avg.se.nde, "cp" = cp.nde)

write.csv(nde.summary, "nde_summary_no_pen.csv")
