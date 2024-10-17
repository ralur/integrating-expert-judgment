
compute_mcc <- function(data, indices) {
  X <- data$X[indices]
  Y <- data$Y[indices]
  
  mcc(X, Y)
}


bootstrap_mcc <- function(y, y_hat, bootstrap.replicates, conf.level) {
  flag <- sd(y) == 0 || sd(y_hat) == 0
  if (is.na(flag) || flag) { # if either vector is constant (e.g., all zeroes), the MCC is zero
    return(list(coef = 0, ci.lower = 0, ci.upper = 0))
  }
  data <- data.frame(X = y, Y = y_hat)
  point_est <- compute_mcc(data, 1:nrow(data))
  bootstrap <- boot(data, compute_mcc, R = bootstrap.replicates)
  ci <- boot.ci(bootstrap, conf = conf.level, type = "bca")$bca[4:5]
  
  list(coef = point_est, ci.lower = ci[1], ci.upper = ci[2])
}


safe_cor <- function(y, y_hat) {
  
  if (length(y) == 1 || sd(y) == 0 || length(y_hat) == 1 || sd(y_hat) == 0) {
    return(0)
  } else {
    return(cor(y, y_hat))
  }
}

process_group <- function(data, bootstrap.replicates = 1000, conf.level = .95) {
  if (nrow(data) == 0){
    return(list())
  }
  
  bootstrap_results <- bootstrap_mcc(data$y, data$y_hat, bootstrap.replicates, conf.level)
  
  list(
    n = nrow(data),
    coef = bootstrap_results$coef,
    ci.lower = bootstrap_results$ci.lower,
    ci.upper = bootstrap_results$ci.upper,
    y = mean(data$y),
    y_hat = mean(data$y_hat),
    total_score = mean(data$total_score),
    s = sd(data$total_score)
  )
}

plot_scatter <- function(data.uncorrected, data.corrected, combine.data = TRUE) {
  
  if (combine.data) {
    data <- rbind(data.uncorrected %>% mutate(correction = 'no correction'), data.corrected %>% mutate(correction = 'Bonferroni corrected')) %>%
      mutate(correction = factor(correction, levels = c('no correction', 'Bonferroni corrected')))
  }
  else {
    data <- data.uncorrected %>%
      mutate(correction = 'no correction')  
  }
  
  
  data %>%
    mutate(stat.sig = ifelse(ci.lower > 0 | ci.upper < 0, 'yes', 'no')) %>%
    mutate(stat.sig = ifelse(is.na(ci.lower) | is.na(ci.upper), 'no', stat.sig)) %>%
    mutate(point.size = ifelse(stat.sig == 'yes', 5, 2)) %>%
    drop_na %>%
    ggplot(aes(log2(n), coef, color = total_score, size = factor(stat.sig, labels = c("Not Significant", "Significant (5% level)")))) +
    geom_point() +
    scale_color_gradient(low = "green", high = "red") +  # specifies color gradient from green to red
    scale_size_manual(values = c(2, 6)) +
    theme_minimal() +
    labs(
      color = "GBS",
      size = 'Significance'
    ) +
    xlab(expression(log[2](group ~ size))) +
    ylab('Matthew\'s Correlation Coefficient') +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.text = element_text(size = 15),
          legend.key.size = unit(1.2, "cm"),
          axis.title.x = element_text(size = 18),
          legend.title = element_text(size = 20),
          axis.title.y = element_text(size = 18),
          strip.text = element_text(size = 20)) + facet_wrap(~correction)
}