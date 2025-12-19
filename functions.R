####################################
########## Plot Functions ##########
####################################



##### Scree-plot function #####

get_screeplot <- function(outcome, outcomename) {
  plot(1:length(outcome$values), outcome$values,
       type = "b",
       ylab = "Eigenvalue", xlab = "Factor", main = paste("Scree plot —", outcomename)
  )
}


##### Define function for coefplots ##### 

get_coefplot <- function(dataframe, colnumber = 3) {
  ggplot(dataframe, aes(y = Sample, x = Estimate, color = Model)) +
    geom_point(position = position_dodge(width = 0.6), size = 2.5) +
    geom_errorbarh(
      aes(xmin = Estimate - 1.96 * SE, xmax = Estimate + 1.96 * SE),
      height = 0.25,
      position = position_dodge(width = 0.6)
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("orange2", "orchid4")) +
    labs(
      title = "Estimated RD Effects Across Outcomes",
      subtitle = "Bias-Adjusted Estimates",
      x = "Estimated RD Effect",
      y = NULL,
      color = "Model"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom"
    ) +
    facet_wrap(~Outcome, ncol = colnumber)
}


##### Define function for discontinuity plots #####
get_discontinuityplot <- function(dataframe, subset, primary_only = TRUE, colnumber = 3) { # arguments:
  # dataframe referring to the specific subset,
  # a string identifying the subset, and column numbers
  # primary outcomes or include traditionalism too
  # number of columns
  df_long <- dataframe %>%
    filter(age <= 60) %>%
    pivot_longer(
      cols = c(media_index),
      names_to = "Outcome",
      values_to = "Value"
    ) %>%
    filter(!is.na(Value)) %>%
    mutate(
      Outcome = recode(
        Outcome,
        media_index = "Media Attitudes",
      ),
      Outcome = factor(
        Outcome,
        levels = c(
          "Media Attitudes"
        )
      )
    )
  
  # NOT APPLICABLE TO THIS PROJECT Check whether only primary variables should be kept
  # if (primary_only) {
  #   df_long <- df_long %>%
  #     filter(Outcome == "Populism" | Outcome == "Authoritarianism" | Outcome == "Nativism")
  # }
  
  # Now do the plotting
  ggplot(df_long, aes(x = age, y = Value, color = factor(treatment))) +
    geom_point(alpha = 0.5) +
    geom_smooth(
      aes(group = factor(treatment)),
      method = "lm",
      formula = y ~ poly(x, 1),
      se = TRUE,
      color = "black",
      alpha = 0.75
    ) +
    geom_vline(xintercept = 26, linetype = "dashed", color = "black") +
    scale_color_brewer(
      palette = "Dark2",
      name = "2012 Voting Eligibility",
      labels = c("Not Eligible (<26)", "Eligible (≥26)")
    ) +
    labs(
      x = "Age in 2020",
      y = NULL,
      title = paste("Discontinuities in Media Attitudes Among", subset),
      subtitle = paste(subset)
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    facet_wrap(~Outcome, scales = "free_y", ncol = colnumber)
}


####################################
########### RDD Functions ##########
####################################


#####  Function that extracts the core values as a table to easily look at them ##### 
extract_rdd_summary <- function(rd_object, model_label = "Model") {
  # Extract core values
  coef <- rd_object$coef[2]
  se <- rd_object$se[2]
  pval <- rd_object$pv[2]
  p <- rd_object$p
  bw_type <- rd_object$bwselect
  bw_value <- rd_object$bws[1]
  n_below <- rd_object$N_h[1]
  n_above <- rd_object$N_h[2]
  
  # # Significance stars
  # stars <- case_when(
  #   pval < 0.001 ~ "***",
  #   pval < 0.01 ~ "**",
  #   pval < 0.05 ~ "*",
  #   TRUE ~ ""
  # )
  
  # Output as a table
  tibble(
    `Model` = model_label,
    # `Estimate` = paste0(round(coef, 3), stars),
    `Estimate` = round(coef, 3),
    `SE` = round(se, 3),
    `Bandwidth Type` = ifelse(bw_type == "mserd", "MSE-optimal", bw_type),
    `Bandwidth (h)` = round(bw_value, 2),
    `N` = n_below + n_above
  )
}


#####  Function that utilizes the function above and the existing rd_robust function to run the models ##### 
run_rdd_models <- function(data, index_var, controls, sample_label) {
  # Extract outcome + running variable
  y <- data[[index_var]]
  x <- data$age
  
  # --- Simple RDD model ---
  rdd_simple <- rdrobust(
    y = y,
    x = x,
    c = 26,
    all = TRUE
  )
  summary_simple <- extract_rdd_summary(
    rdd_simple,
    model_label = "Without Controls"
  )
  
  # --- Controls RDD model ---
  covs <- data[, controls, drop = FALSE]
  
  rdd_controls <- rdrobust(
    y = y,
    x = x,
    c = 26,
    covs = covs,
    all = TRUE
  )
  summary_controls <- extract_rdd_summary(
    rdd_controls,
    model_label = "With Controls"
  )
  
  # Add sample label and return combined results
  bind_rows(summary_simple, summary_controls) %>%
    dplyr::mutate(Sample = sample_label, .before = 1)
}
