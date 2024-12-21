library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(grid)
library(xtable)

setwd("/filepath/simulations_incorrect_fixed/n200_next2000/high_r2_mediator_high_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))

print(sim.out)

sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p1.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next20000/high_r2_mediator_high_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p2.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next200000/high_r2_mediator_high_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p3.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next2000/high_r2_mediator_low_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p4.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next20000/high_r2_mediator_low_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p5.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next200000/high_r2_mediator_low_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p6.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next2000/high_r2_mediator_medium_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p7.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next20000/high_r2_mediator_medium_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p8.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next200000/high_r2_mediator_medium_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p9.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next2000/low_r2_mediator_high_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p10.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next20000/low_r2_mediator_high_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p11.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next200000/low_r2_mediator_high_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p12.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next2000/low_r2_mediator_low_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p13.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next20000/low_r2_mediator_low_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p14.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next200000/low_r2_mediator_low_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p15.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next2000/low_r2_mediator_medium_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p16.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next20000/low_r2_mediator_medium_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p17.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/n200_next200000/low_r2_mediator_medium_r2_outcome/")

sim.out <- read.csv("nie_summary_no_pen.csv")[,-1]
sim.out$bias <- sim.out$bias
sim.out$mse <- sqrt(sim.out$mse)
names(sim.out)[names(sim.out) == "mse"] <- "rmse"
sim.out$emp_sd <- sim.out$emp_sd
sim.out$avg_se <- sim.out$avg_se
sim.out$cp <- sim.out$cp
sim.out <- sim.out %>% filter(method %in% c("Unconstrained", "Hard Constraint (External)", "Soft Constraint (EB) Asym Norm",
                                            "Soft Constraint (Discrete) PB Quantile", "Hard Constraint (Oracle)"))
sim.out$method <- c("Unconstrained", "Soft Constraint (EB)", "Hard Constraint (External)", "Hard Constraint (Oracle)", "Soft Constraint (Discrete)")
sim.out <- sim.out[c(which(sim.out$method == "Unconstrained"), which(sim.out$method == "Hard Constraint (External)"),
                     which(sim.out$method == "Soft Constraint (EB)"), which(sim.out$method == "Soft Constraint (Discrete)"),
                     which(sim.out$method == "Hard Constraint (Oracle)")),]

p18.df <- sim.out

setwd("/filepath/simulations_incorrect_fixed/")

ltx.tbl <- rbind(cbind(p1.df, p2.df[,-1], p3.df[,-1]), cbind(p7.df, p8.df[,-1], p9.df[,-1]), cbind(p4.df, p5.df[,-1], p6.df[,-1]),
                 cbind(p10.df, p11.df[,-1], p12.df[,-1]), cbind(p16.df, p17.df[,-1], p18.df[,-1]), cbind(p13.df, p14.df[,-1], p15.df[,-1]))

print(xtable(ltx.tbl, type = "latex", digits = 3), file = "nie_200_incorrect_fixed.tex")
