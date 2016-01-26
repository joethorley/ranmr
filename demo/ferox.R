# for additional information on a function type: ?function_name

# ensure required packages are loaded
library(ggplot2)
library(foreach)
library(doParallel)
library(sp)
library(magrittr)
library(dplyr)
library(jaggernaut)
library(ranmrdata)
library(ranmr)

# create results/plots, results/pdfs and results/rds directories to store results
create_dirs()

# load ferox mark-recapture dataset
ferox <- ferox()

# plot and save length-mass data
plot_mr(ferox, xlab = "Fork Length (mm)", ylab = "Wet Mass (kg)")
save_plot("mass")

# plot and save age-length data
plot_mr(ferox, xcol = "Age",  ycol = "Length",
                    xlab = "Scale Age (yr)",  ylab = "Fork Length (mm)")
save_plot("age")

save_kml(ferox, "ferox.kml")

# print and save summary and table of ferox data
summarise_mr(ferox)
save_rds(summarise_mr(ferox), "data")
tabulate_mr(ferox)
save_rds(tabulate_mr(ferox), "table")

# set number of genuine and pseudoindividuals to be 1000
levels(ferox$Fish) %<>% c(paste0("Pseudo", 1:(1000 - nlevels(ferox$Fish))))

# print JAGS model code
cat(mr_model_code())

# perform mark-recapture analysis and save results
analysis <- analyse_mr(ferox)
summary(analysis)

save_rds(analysis, "analysis")
save_rds(coef(analysis), "coef")

# save pdf of analysis traceplots
save_pdf(analysis, "traceplots")

# plot and save abundance estimates by year
plot_abundance(analysis)
save_plot("abundance")

# perform posterior predictive check
ppc <- predictive_check(analysis)
print(ppc)
save_rds(ppc, "ppc")
