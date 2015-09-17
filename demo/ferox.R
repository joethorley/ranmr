# load required packages
library(ggplot2)
library(foreach)
library(doParallel)
library(magrittr)
library(dplyr)
library(jaggernaut)

# create figures and results folders in the working directory
dir.create("figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)

# load required datasets
data(scotland)
data(rannoch)
data(power_station)
data(ferox)

map_scotland(scotland, rannoch)
ggsave("figures/scotland.eps", width = 2.63, height = 3)

map_rannoch(rannoch, power_station) %>%
  plot_mr(ferox, xcol = "Easting",  ycol = "Northing", gp = .)
ggsave("figures/rannoch.eps", width = 7.5, height = 2)

plot_mr(ferox, xlab = "Fork Length (mm)", ylab = "Wet Mass (kg)")
ggsave("figures/mass.eps", width = 2.63, height = 2.63)

plot_mr(ferox, xcol = "Age",  ycol = "Length",
                    xlab = "Scale Age (yr)",  ylab = "Fork Length (mm)")
ggsave("figures/age.eps", width = 2.63, height = 2.63)

summarise_mr(ferox)
saveRDS(summarise_mr(ferox), file = "results/data.rds")

tabulate_mr(ferox)
saveRDS(tabulate_mr(ferox), file = "results/table.rds")

# set number of genuine and pseudoindividuals to be 1000
levels(ferox$Fish) %<>% c(paste0("Pseudo", 1:(1000 - nlevels(ferox$Fish))))

analysis <- analyse_mr(ferox, niters = 10^5)
summary(analysis)

saveRDS(analysis, file = "results/analysis.rds")
saveRDS(coef(analysis), file = "results/coef.rds")

plot_adundance(analysis)
ggsave("figures/abundance.eps", width = 2.63, height = 2.63)

ppc <- predictive_check(analysis)
ppc
saveRDS(ppc, file = "results/ppc.rds")
