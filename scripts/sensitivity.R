#################################################################
##                  Loading packages and data                  ##
#################################################################

#loading packages
library(tidyverse)
library(nodeIdentifyR)
library(IsingFit)
library(bootnet)
library(NIRApost)
library(flextable)
library(gtsummary)

#loading data
data_combined_recoded <- read.csv("./data/data_combined_sensitivity.csv") %>% 
  select(-id)

#################################################################
##                     Fitting Ising model                     ##
#################################################################

set.seed(42)

groups <- list(
  Physical = c(1:8, 12, 13),
  Cognitive = c(9, 10, 11))

#fitting Ising model
net <- estimateNetwork(data_combined_recoded,
                       default = "IsingFit",
                       tuning = 0.25, rule = "AND")

plot(net, layout = "spring", theme = "colorblind",
     groups = groups, legend = TRUE, legend.cex = 2,
     filename = "panicnet_sensitivity", filetype = "png", width = 20, height = 20)

#extracting edge weight matrix and threshold parameter vector
weights <- net$results$weiadj
thresholds <- net$results$thresholds

# #moderation effect test (prerequisite for NIRA)
# sig_moder <- runMgmmAnalysis(
#   data        = as.matrix(data_combined_recoded),
#   plotResults = FALSE,
#   rule        = "AND",
#   lambdaGam   = 0.25,
#   nB          = 100)
# saveRDS(sig_moder, file = "./data/sig_moder_sensitivity.RDS")

sig_moder <- readRDS(file = "./data/sig_moder_sensitivity.RDS")

#list must be empty for NIRA to proceed
print(sig_moder$significant_moderators)

#################################################################
##                  Fitting simulation models                  ##
#################################################################

#alleviating intervention
set.seed(42)
sim_alleviating<- simulateResponses(edge_weights = weights, thresholds = thresholds, 
                                    perturbation_type = "alleviating", 
                                    amount_of_SDs_perturbation = 2)

scores_alleviating <- calculateSumScores(sim_alleviating) %>% 
  prepareDFforPlottingAndANOVA() %>% 
  permutationNIRAtest(method = "bonferroni")

plotMeanNIRA(scores_alleviating, "alleviating") %>% 
  ggsave(filename = "alleviating_plot_sensitivity.png", width = 6, height = 4)

#aggravating intervention
set.seed(42)
sim_aggravating <- simulateResponses(edge_weights = weights, thresholds = thresholds, 
                                     perturbation_type = "aggravating", 
                                     amount_of_SDs_perturbation = 2)

scores_aggravating <- calculateSumScores(sim_aggravating) %>% 
  prepareDFforPlottingAndANOVA() %>% 
  permutationNIRAtest(method = "bonferroni")

plotMeanNIRA(scores_aggravating, "aggravating") %>% 
  ggsave(filename = "aggravating_plot_sensitivity.png", width = 6, height = 4)

#################################################################
##              Checking model stability/accuracy              ##
#################################################################

#stability of alleviating intervention sims
set.seed(42)
# stability_alleviating <- stabilityNIRAtest(edge_weights = weights, thresholds = thresholds,
#                                            perturbation_type = "alleviating",
#                                            amount_of_SDs_perturbation = 2,
#                                            nReps = 1000, parallel = TRUE)
# saveRDS(stability_alleviating, file = "./data/stability_alleviating_sensitivity.RDS")

stability_alleviating <- readRDS(file = "./data/stability_alleviating_sensitivity.RDS")

stability_alleviating_rank <- findMaxN(stability_alleviating, n = 13)
print(stability_alleviating_rank)
plotMaxN(stability_alleviating_rank, low = 1, high = 3) %>% 
  ggsave(filename = "stability_alleviating_rank_sensitivity.png", width = 6, height = 4)

#stability of aggravating intervention sims
set.seed(42)
# stability_aggravating <- stabilityNIRAtest(edge_weights = weights, thresholds = thresholds,
#                                            perturbation_type = "aggravating",
#                                            amount_of_SDs_perturbation = 2,
#                                            nReps = 1000, parallel = TRUE)
# saveRDS(stability_aggravating, file = "./data/stability_aggravating_sensitivity.RDS")

stability_aggravating <- readRDS(file = "./data/stability_aggravating_sensitivity.RDS")

stability_aggravating_rank <- findMaxN(stability_aggravating, n = 13)
print(stability_aggravating_rank)
plotMaxN(stability_aggravating_rank, low = 1, high = 3)  %>% 
  ggsave(filename = "stability_aggravating_rank_sensitivity.png", width = 6, height = 4)

