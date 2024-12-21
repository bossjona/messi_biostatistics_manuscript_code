library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(grid)

setwd("/filepath/simulations_incorrect_random/n200_next2000/high_r2_mediator_high_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 2000, R^2 = 0.2, R^2 = 0.8"

p1.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next20000/high_r2_mediator_high_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 20000, R^2 = 0.2, R^2 = 0.8"

p2.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next200000/high_r2_mediator_high_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 200000, R^2 = 0.2, R^2 = 0.8"

p3.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next2000/high_r2_mediator_low_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 2000, R^2 = 0.2, R^2 = 0.2"

p4.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next20000/high_r2_mediator_low_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 20000, R^2 = 0.2, R^2 = 0.2"

p5.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next200000/high_r2_mediator_low_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 200000, R^2 = 0.2, R^2 = 0.2"

p6.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next2000/high_r2_mediator_medium_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 2000, R^2 = 0.2, R^2 = 0.5"

p7.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next20000/high_r2_mediator_medium_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 20000, R^2 = 0.2, R^2 = 0.5"

p8.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next200000/high_r2_mediator_medium_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 200000, R^2 = 0.2, R^2 = 0.5"

p9.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next2000/low_r2_mediator_high_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 2000, R^2 = 0.05, R^2 = 0.8"

p10.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next20000/low_r2_mediator_high_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 20000, R^2 = 0.05, R^2 = 0.8"

p11.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next200000/low_r2_mediator_high_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 200000, R^2 = 0.05, R^2 = 0.8"

p12.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next2000/low_r2_mediator_low_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 2000, R^2 = 0.05, R^2 = 0.2"

p13.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next20000/low_r2_mediator_low_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 20000, R^2 = 0.05, R^2 = 0.2"

p14.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next200000/low_r2_mediator_low_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 200000, R^2 = 0.05, R^2 = 0.2"

p15.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next2000/low_r2_mediator_medium_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
                sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 2000, R^2 = 0.05, R^2 = 0.5"

p16.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next20000/low_r2_mediator_medium_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 20000, R^2 = 0.05, R^2 = 0.5"

p17.df <- summary.df

setwd("/filepath/simulations_incorrect_random/n200_next200000/low_r2_mediator_medium_r2_outcome/results/")

n.sim.itr <- length(read.csv("nie.u.no.pen.est.csv")[,-1])

nie.true <- sum(read.csv("alpha.a.true.csv")[1,-1]*read.csv("beta.m.true.csv")[1,-1])

rel.sq.err <- c(sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.h.ext.no.pen.est.csv")[,-1] - nie.true)^2)),
		sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.sc.eb.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.d.no.pen.est.csv")[,-1] - nie.true)^2)),
               sqrt(mean((read.csv("nie.u.no.pen.est.csv")[,-1] - nie.true)^2)/mean((read.csv("nie.o.no.pen.est.csv")[,-1] - nie.true)^2)))

lbl.vec <- c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)")

summary.df <- data.frame("rel.sq.err" = rel.sq.err, "Method" = lbl.vec)
summary.df$Method <- factor(summary.df$Method, levels = c("Unconstrained", "Hard Constraint", "MESSI-EB", "Discrete Selection", "Hard Constraint (Oracle)"))
summary.df$setting <- "n_ext = 200000, R^2 = 0.05, R^2 = 0.5"

p18.df <- summary.df

setwd("/filepath/simulations_incorrect_random/")

plot.df <- rbind(p1.df, p2.df, p3.df, p7.df, p8.df, p9.df, p4.df, p5.df, p6.df,
		 p10.df, p11.df, p12.df, p16.df, p17.df, p18.df, p13.df, p14.df, p15.df)

plot.df$setting <- factor(plot.df$setting, levels = c("n_ext = 2000, R^2 = 0.2, R^2 = 0.8",
						      "n_ext = 20000, R^2 = 0.2, R^2 = 0.8",
                                                     "n_ext = 200000, R^2 = 0.2, R^2 = 0.8",
						      "n_ext = 2000, R^2 = 0.2, R^2 = 0.5",
						      "n_ext = 20000, R^2 = 0.2, R^2 = 0.5",
                                                     "n_ext = 200000, R^2 = 0.2, R^2 = 0.5",
                                                     "n_ext = 2000, R^2 = 0.2, R^2 = 0.2",
						      "n_ext = 20000, R^2 = 0.2, R^2 = 0.2",
                                                     "n_ext = 200000, R^2 = 0.2, R^2 = 0.2",
                                                     "n_ext = 2000, R^2 = 0.05, R^2 = 0.8",
						      "n_ext = 20000, R^2 = 0.05, R^2 = 0.8",
                                                     "n_ext = 200000, R^2 = 0.05, R^2 = 0.8",
						      "n_ext = 2000, R^2 = 0.05, R^2 = 0.5",
						      "n_ext = 20000, R^2 = 0.05, R^2 = 0.5",
                                                     "n_ext = 200000, R^2 = 0.05, R^2 = 0.5",
                                                     "n_ext = 2000, R^2 = 0.05, R^2 = 0.2",
						      "n_ext = 20000, R^2 = 0.05, R^2 = 0.2",
                                                     "n_ext = 200000, R^2 = 0.05, R^2 = 0.2"))

levels(plot.df$setting) <- c(expression(paste(n[E], " = 2000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.8")),
			     expression(paste(n[E], " = 20000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.8")),
			     expression(paste(n[E], " = 200000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.8")),
			     expression(paste(n[E], " = 2000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.5")),
			     expression(paste(n[E], " = 20000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.5")),
			     expression(paste(n[E], " = 200000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.5")),
                            expression(paste(n[E], " = 2000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.2")),
			     expression(paste(n[E], " = 20000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.2")),
			     expression(paste(n[E], " = 200000, ", R["A|C"]^2, " = 0.2, ", R["M|A,C"]^2, " = 0.2")),
			     expression(paste(n[E], " = 2000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.8")),
			     expression(paste(n[E], " = 20000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.8")),
			     expression(paste(n[E], " = 200000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.8")),
			     expression(paste(n[E], " = 2000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.5")),
			     expression(paste(n[E], " = 20000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.5")),
			     expression(paste(n[E], " = 200000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.5")),
                            expression(paste(n[E], " = 2000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.2")),
			     expression(paste(n[E], " = 20000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.2")),
			     expression(paste(n[E], " = 200000, ", R["A|C"]^2, " = 0.05, ", R["M|A,C"]^2, " = 0.2")))

horiz.lines <- subset(plot.df, Method == "Hard Constraint (Oracle)")

rel.sq.plot <- ggplot(data = plot.df, aes(x = Method, y = rel.sq.err)) + geom_point(size = 3) +
                      geom_hline(yintercept = 1, linetype = "solid") +
                      ggtitle("IE Estimation, Random Simulation Settings (n = 200)") +
                      xlab("") + ylab("Ratio of RIMSE of Unconstrained Estimator over RIMSE") +
                      theme_bw() +
                      theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=1, hjust=1), plot.title = element_text(hjust = 0.5, size = 12)) +
                      facet_wrap(~setting, ncol = 3, labeller = label_parsed)  +
                      coord_cartesian(ylim = c(0, 2)) +
		      geom_hline(aes(yintercept = rel.sq.err), data = horiz.lines, color = "black", linetype = "dashed")

t_shift <- scales::trans_new("shift",
                             transform = function(x) {x-1},
                             inverse = function(x) {x+1})

rel.sq.plot <- ggplot(data = plot.df, aes(x = Method, y = rel.sq.err, fill = Method)) + geom_bar(stat="identity", color="black") +
                      geom_hline(yintercept = 1, linetype = "solid") +
                      ggtitle("IE Estimation, Random Simulation Settings (n = 200)") +
                      xlab("") + ylab("Ratio of RIMSE of Unconstrained Estimator over RIMSE") +
                      theme_bw() +
                      theme(legend.position = "none", axis.text.x = element_text(angle=45, vjust=1, hjust=1), plot.title = element_text(hjust = 0.5, size = 12)) +
                      facet_wrap(~setting, ncol = 3, labeller = label_parsed)  +
                      scale_y_continuous(trans = t_shift, limits = c(0, 2)) +
                      scale_fill_manual(values = c("gray100", "gray75", "gray50", "gray25", "gray0")) +
		      geom_hline(aes(yintercept = rel.sq.err), data = horiz.lines, color = "black", linetype = "dashed")

pdf("rmse_results_n200_nie_unpenalized_incorrect_random.pdf", onefile = FALSE, width = 8, height = 10)
print(rel.sq.plot)
dev.off()
