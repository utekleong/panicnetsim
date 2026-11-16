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
data_combined_recoded <- read.csv("./data/data_combined.csv") %>% 
  select(-id)

symptomnames <- read.csv("./data/symptom_names.csv")

##################################################################
##                          Prevalance                          ##
##################################################################

prevalence <- data.frame(node_label = names(data_combined_recoded),
                               count = colSums(data_combined_recoded, na.rm = TRUE),
                               percent = round(colMeans(data_combined_recoded, na.rm = TRUE) * 100, 1))
rownames(prevalence) <- NULL
prevalence <- right_join(symptomnames, prevalence, by = "node_label")
prevalence <-  prevalence[order(prevalence$percent, decreasing = TRUE),]

prevalence_table <- prevalence %>%
  select(-node_label) %>% 
  flextable() %>% 
  set_header_labels(symptom = "Symptom",
                    count = "n",
                    percent = "Prevalence (%)") %>% 
  autofit()
#print(prevalence_table, preview = "docx")

##################################################################
##                         Demographics                         ##
##################################################################

#loading data
demo_step <- read.csv("./data/demo_step.csv") %>% 
  select(code,age, gender, Marital, Race, Education, Income) %>% 
  rename(id = code,
         marital_status = Marital,
         ethnicity = Race,
         education = Education,
         income = Income) %>% 
  mutate(across(gender:income, as.factor)) %>% 
  mutate(income = na_if(income, "0"))

demo_stept <- read.csv("./data/demo_stept.csv") %>% 
  select(id, age, gender, marital_status, ethnicity, education, income) %>% 
  mutate(income = na_if(income, ""))
demo_stept <- demo_stept[-c(1,2),]
demo_stept <- mutate(demo_stept, across(gender:income, as.factor))
demo_stept$age <- as.numeric(demo_stept$age)

demo_cdrpilot <- read.csv("./data/demo_cdrpilot.csv") %>% 
  mutate(across(gender:income, as.factor)) %>%
  mutate(income = na_if(income, "missing"))

demo_cdrvalid <- read.csv("./data/demo_cdrvalid.csv") %>% 
  mutate(across(gender:income, as.factor)) %>%
  mutate(income = na_if(income, "Missing"))
demo_cdrvalid$income <- as.character(demo_cdrvalid$income) %>% 
  as.numeric()
demo_cdrvalid <- demo_cdrvalid %>% 
  mutate(income = case_when(
    income < 1000 ~ "<$1,000",
    income < 3000 ~ "$1,000-$2999",
    income < 5000 ~ "$3,000-$4999",
    income < 7000 ~ "$5,000-$6999",
    income < 10000 ~ "$7,000-$9999",
    income >= 10000 ~ ">=$10,000"
  ))

#combining datasets
sampleid <- read.csv("./data/data_combined.csv") %>% 
  select(id)

demo_combined <- bind_rows(demo_step, demo_stept, demo_cdrpilot, demo_cdrvalid) %>% 
  filter(id %in% sampleid$id) %>% 
  mutate(
    marital_status = fct_collapse(
      marital_status,
      "Married/Living with partner" = c("Married/living with partner", "Married/Living with partner"),
      "Divorced/Separated/Widowed"= c("Divorced/Separated", "Divorced/separated/widowed", "Widowed")
    ),
    ethnicity = fct_collapse(
      ethnicity,
      Other = c("Other", "More than one race/ethnicity")
    ),
    education = fct_collapse(
      education,
      "Secondary or below" = c("Secondary or below", "Primary or below", "Secondary/Vocational"),
      "Tertiary" = c("Tertiary", "University/postgraduate", "Pre-university/Polytechnic", "University/Postgraduate")
    ),
    income = fct_collapse(
      income,
      "<$1,000" = c("< S$1,000", "<$1,000"),
      "$1,000-$2999" = c("$1,000-$2999", "S$1,000 - S$2,999"),
      "$3,000-$4999" = c("$3,000-$4,999", "$3,000-$4999", "S$3,000 - S$4,999"),
      "$5,000-$6999" = c("$5,000-$6,999", "$5,000-$6999", "S$5,000 - S$6,999"),
      "$7,000-$9999" = c("$7,000-$9,999", "$7,000-$9999", "S$7,000 - S$9,999"),
      ">=$10,000" = c(">= S$10,000", ">=$10,000", "$10000 and above"),
      "Unable to classify" = c("$2000 and below", "$2001 - $5000", "$5001 - $8000", "$8001 - $10000")
    ),
    income = fct_na_value_to_level(income, level = "Missing")
  )

#reordering levels
demo_combined <- demo_combined %>% 
  mutate(
    marital_status = fct_relevel(
      marital_status,
      "Single",
      "Married/Living with partner",
      "Divorced/Separated/Widowed"
  ),
    ethnicity = fct_relevel(
      ethnicity,
      "Chinese",
      "Malay",
      "Indian",
      "Eurasian",
      "Other"
  ),
    income = fct_relevel(
      income,
      "<$1,000",
      "$1,000-$2999",
      "$3,000-$4999",
      "$5,000-$6999",
      "$7,000-$9999",
      ">=$10,000",
      "Unable to classify",
      "Missing"
    )
  )

#building table
demo_table <- demo_combined %>% 
  select(-id) %>% 
  tbl_summary(label = list(age = "Age, Mean(SD); Range",
                           gender = "Gender, n(%)",
                           marital_status = "Marital Status, n(%)",
                           ethnicity = "Ethnicity, n(%)",
                           education = "Education, n(%)",
                           income = "Income, n(%)"),
              statistic = age ~ "{mean} ({sd}); {min}–{max}") %>% 
  remove_footnote_header() %>% 
  as_flex_table()
#print(demo_table, preview = "docx")
    
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
     filename = "panicnet", filetype = "png", width = 20, height = 20)

#extracting edge weight matrix and threshold parameter vector
weights <- net$results$weiadj
thresholds <- net$results$thresholds

#moderation effect test (prerequisite for NIRA)
# sig_moder <- runMgmmAnalysis(
#   data        = as.matrix(data_combined_recoded),
#   plotResults = FALSE,
#   rule        = "AND",
#   lambdaGam   = 0.25,
#   nB          = 100)
# saveRDS(sig_moder, file = "./data/sig_moder.RDS")

sig_moder <- readRDS(file = "./data/sig_moder.RDS")

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
  ggsave(filename = "alleviating_plot.png", width = 6, height = 4)

#aggravating intervention
set.seed(42)
sim_aggravating <- simulateResponses(edge_weights = weights, thresholds = thresholds, 
                                     perturbation_type = "aggravating", 
                                     amount_of_SDs_perturbation = 2)

scores_aggravating <- calculateSumScores(sim_aggravating) %>% 
  prepareDFforPlottingAndANOVA() %>% 
  permutationNIRAtest(method = "bonferroni")

plotMeanNIRA(scores_aggravating, "aggravating") %>% 
  ggsave(filename = "aggravating_plot.png", width = 6, height = 4)

#stat tables
alleviating_table <- scores_alleviating$stat %>% 
  select(mean_other, sd_other, ciLower_other, ciUpper_other, cohen_d, p.adjust) %>%
  round(digits = 2) %>% 
  rownames_to_column(var = "node_label") %>% 
  left_join(symptomnames, by = "node_label") %>% 
  relocate(symptom, .after = "node_label") %>% 
  select(-node_label) %>%
  mutate(ci = paste0("[",ciLower_other, ",", ciUpper_other, "]"), .after = sd_other, .keep = "unused") %>%
  mutate(p.adjust = case_when(
    p.adjust <= 0.001 ~ "<.001",
    p.adjust <= 0.01 ~ "<.01",
    p.adjust <= 0.05 ~ "<.05",
    TRUE ~ as.character(p.adjust))) %>%
  arrange(mean_other) %>% 
  flextable() %>%
  set_header_labels(symptom = "Mean symptom sum score",
                    mean_other = "Mean",
                    sd_other = "SD",
                    ci = "95% CI",
                    cohen_d = "Cohen's d",
                    p.adjust = "Adjusted p") %>% 
  autofit()
#print(alleviating_table, preview = "docx")
  
aggravating_table <- scores_aggravating$stat  %>% 
  select(mean_other, sd_other, ciLower_other, ciUpper_other, cohen_d, p.adjust) %>%
  round(digits = 2) %>% 
  rownames_to_column(var = "node_label") %>% 
  left_join(symptomnames, by = "node_label") %>% 
  relocate(symptom, .after = "node_label") %>% 
  select(-node_label) %>%
  mutate(ci = paste0("[",ciLower_other, ",", ciUpper_other, "]"), .after = sd_other, .keep = "unused") %>%
  mutate(p.adjust = case_when(
    p.adjust <= 0.001 ~ "<.001",
    p.adjust <= 0.01 ~ "<.01",
    p.adjust <= 0.05 ~ "<.05",
    TRUE ~ as.character(p.adjust))) %>%
  arrange(desc(mean_other)) %>% 
  flextable() %>%
  set_header_labels(symptom = "Symptom",
                    mean_other = "Mean symptom sum score",
                    sd_other = "SD",
                    ci = "95% CI",
                    cohen_d = "Cohen's d",
                    p.adjust = "Adjusted p") %>% 
  autofit()
#print(aggravating_table, preview = "docx")

#################################################################
##              Checking model stability/accuracy              ##
#################################################################

#stability of alleviating intervention sims
set.seed(42)
# stability_alleviating <- stabilityNIRAtest(edge_weights = weights, thresholds = thresholds, 
#                                            perturbation_type = "alleviating", 
#                                            amount_of_SDs_perturbation = 2,
#                                            nReps = 1000, parallel = TRUE)
#saveRDS(stability_alleviating, file = "./data/stability_alleviating.RDS")

stability_alleviating <- readRDS(file = "./data/stability_alleviating.RDS")

stability_alleviating_rank <- findMaxN(stability_alleviating, n = 13)
print(stability_alleviating_rank)
plotMaxN(stability_alleviating_rank, low = 1, high = 3) %>% 
  ggsave(filename = "stability_alleviating_rank.png", width = 6, height = 4)

#stability of aggravating intervention sims
set.seed(42)
# stability_aggravating <- stabilityNIRAtest(edge_weights = weights, thresholds = thresholds,
#                                            perturbation_type = "aggravating",
#                                            amount_of_SDs_perturbation = 2,
#                                            nReps = 1000, parallel = TRUE)
# saveRDS(stability_aggravating, file = "./data/stability_aggravating.RDS")

stability_aggravating <- readRDS(file = "./data/stability_aggravating.RDS")

stability_aggravating_rank <- findMaxN(stability_aggravating, n = 13)
print(stability_aggravating_rank)
plotMaxN(stability_aggravating_rank, low = 1, high = 3)  %>% 
  ggsave(filename = "stability_aggravating_rank.png", width = 6, height = 4)

#edge weight stability
set.seed(42)
nonparboot <- bootnet(net, nBoots = 1000)
plot(nonparboot, labels = TRUE, order = "sample") %>% 
  ggsave(filename = "nonparboot.png", width = 6, height = 10)
