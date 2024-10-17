library(boot)
library(tidyverse)
library(mltools)
library(future)
library(future.apply)
library(glue)
library(gridExtra)
library(Cairo)

set.seed(0)
recompute <- TRUE # set to TRUE to recompute cached inputs needed to generate figures 1 and 2.

if (!file.exists('../data/gib_gbs_clean_inputs_normalized.csv')) {
  stop('Input data ../data/gib_gbs_clean_inputs_normalized.csv does not exist. Is your working directory set to [repository root]/code?')
}

source('./figure_helpers.R')

d <- read_csv('../data/gib_gbs_clean_inputs_normalized.csv')

train.size <- .8
indices <- sample(1:nrow(d), size = floor(train.size * nrow(d)))
d.train <- d[indices, ]
d.test <- d[-indices, ]

write_csv(d.train, '../data/gib_gbs_clean_inputs_normalized_train.csv')
write_csv(d.test, '../data/gib_gbs_clean_inputs_normalized_test.csv')

d <- d.train

plan(multisession)

## PREPARE FIGURE DATA

# This step generates the data needed to produce figures 1 and 2. It takes ~5-10 minutes to run on a standard personal laptop.
# Once computed, the results are cached so that figures can be quickly regenerated
if (file.exists('../data/by_gbs_figure_data.csv') && !recompute) {
  d1 <- read_csv('../data/by_gbs_figure_data.csv')
} else {
  score_list <- split(d, list(d$total_score))
  by_gbs <- future_lapply(score_list, process_group, conf.level = 1 - .05 / 23, future.seed = TRUE)
  d1 <- bind_rows(by_gbs, .id = "group_id")
  
  write_csv(d1, '../data/by_gbs_figure_data.csv')
}


if (file.exists('../data/by_group_figure_data_uncorrected.csv') && !recompute) {
  d2 <- read_csv('../data/by_group_figure_data_uncorrected.csv')
} else {
  d_list <- split(d, list(d$BUN, d$HGB, d$SBP, d$PULSE, d$cardiac_failure, d$hepatic_disease, d$melena, d$syncope, d$SEX_C))
  result_list <- future_lapply(d_list, process_group, future.seed = TRUE)
  d2 <- bind_rows(result_list, .id = "group_id")
  
  write_csv(d2, '../data/by_group_figure_data_uncorrected.csv')
}


if (file.exists('../data/by_group_figure_data_corrected.csv') && !recompute) {
  d3 <- read_csv('../data/by_group_figure_data_corrected.csv')
} else {
  d_list <- split(d, list(d$BUN, d$HGB, d$SBP, d$PULSE, d$cardiac_failure, d$hepatic_disease, d$melena, d$syncope, d$SEX_C))
  result_list <- future_lapply(d_list, function(g) process_group(g, conf.level = 1 - .05 / 669), future.seed = TRUE )
  d3 <- bind_rows(result_list, .id = "group_id")
  
  write_csv(d3, '../data/by_group_figure_data_corrected.csv')
}

## GENERATE FIGURES
p0.0 <- plot_scatter(d2, d3, combine.data = FALSE)
ggsave('../figures/observationally_identical_uncorrected.pdf', p0.0, width = 12, height = 5, device = cairo_pdf)
ggsave('../figures/observationally_identical_uncorrected_compact.pdf', p0.0, width = 8, height = 5, device = cairo_pdf)
p0 <- plot_scatter(d2, d3, combine.data = TRUE)
ggsave('../figures/observationally_identical_corrected.pdf', p0, width = 12, height = 5, device = cairo_pdf)

p1 <- d1 %>%
  mutate(GBS = total_score) %>%
  ggplot(aes(x = factor(total_score), group = GBS)) +
  
  geom_boxplot(aes(ymin = ci.lower, lower = ci.lower, ymax = ci.upper, upper = ci.upper, middle = coef),
               color = "#00000030", fill = "#FFCC33", width = .9, stat = "identity") +
  geom_point(position = position_dodge(width = .9),
             aes(y = coef), shape = 21, size = 4, fill = "#FFCC33") +
  theme_minimal(base_size = 20) +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  ylab("Matthew's Correlation Coefficient") +
  xlab("Glasgow-Blatchford Score") +
  ggtitle("")

ggsave('../figures/by_gbs.pdf', p1, width = 12, height = 5, device = cairo_pdf)
ggsave('../figures/by_gbs_compact.pdf', p1, width = 8, height = 5, device = cairo_pdf)


d2 <- read_csv('../data/mcc_by_pred_test.csv')

p2 <- d2 %>%
  mutate(reindexed = 1:7) %>%
  ggplot(aes(x = factor(reindexed), group = factor(index))) +
  geom_boxplot(aes(ymin = lower, lower = lower, ymax = upper, upper = upper, middle = mcc_score, fill = index / 10),
               color = "#00000030", width = .9, stat = "identity") +
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red", 
                       midpoint = median(d2$index) / 10, name = "Algorithm Predicted Risk") +
  geom_point(position = position_dodge(width = .9), 
             aes(y = mcc_score), shape = 21, size = 4) +
  theme_minimal(base_size = 20) +
  ylab('Matthew\'s Correlation Coefficient') +
  xlab('Subset Index') + 
  ggtitle('')

ggsave('../figures/indistinguishable_subset_risk.pdf', p2, width = 12, height = 5, device = cairo_pdf)



