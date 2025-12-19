

get_robustnesssplots <- function(dataframe, outcome, estimate_type = 1, include_covariates = FALSE) {

  # if (include_covariates) {
  #   covariates <- dataframe[, c("gender", "education", "income"), drop = FALSE]
  #   control_type <- "(With Controls)"
  # } else {
  #   covariates <- NULL
  #   control_type <- "(Without Controls)"
  # }
  #
  # if (outcome == "nativism_index") {
  #   outcome_name <- "Nativist Attitudes"
  # } else if (outcome == "authoritarianism index"){
  #   outcome_name <- "Authoritarian Attitudes"
  # } else {
  #   outcome_name <- "Populist Attitudes"
  # }
  #
  # if (estimate_type == 1) {
  #   estimate_name <- "Conventional Estimates"
  # } else if (estimate_type == 2){
  #   estimate_name <- "Bias-Adjusted Estimates"
  # } else {
  #   estimate_name <- "Bias-Adjusted Estimates with Robust Standard Errors"
  # }

  # Get the name of the dataframe to determine subset
  df_name <- deparse(substitute(dataframe))

  subset_name <- switch(
    df_name,
    df = "Full Sample",
    df_independents = "Independents",
    df_partisans = "Partisans",
    df_democrats = "Democrats",
    df_republicans = "Republicans",
    stop("Unknown dataframe. Expected: df, df_independents, df_partisans, df_democrats, or df_republicans")
  )


  dataframe <- dataframe[complete.cases(dataframe[[outcome]], df$age), ]

  # Remove NAs from outcome and age variables
  if (include_covariates) {
    dataframe <- dataframe[complete.cases(dataframe[[outcome]], dataframe$age,
                                          dataframe$gender, dataframe$education, dataframe$income), ]
  } else {
    dataframe <- dataframe[complete.cases(dataframe[[outcome]], dataframe$age), ]
  }


  if (include_covariates) {
    covariates <- dataframe[, c("gender", "education", "income"), drop = FALSE]
    control_type <- "(With Controls)"
  } else {
    covariates <- NULL
    control_type <- "(Without Controls)"
  }

  outcome_name <- switch(
    as.character(outcome),
    "media_index" = "Media Freedom Attitudes,",
    stop("Unknown outcome variable")
  )

  estimate_name <- switch(
    as.character(estimate_type),
    "1" = "Conventional Estimates",
    "2" = "Bias-Adjusted Estimates",
    "3" = "Bias-Adjusted Estimates with Robust Standard Errors",
    stop("estimate_type must be 1, 2, or 3")
  )

  # subset_name <- switch(
  #   as.character(dataframe),
  #   df = "Full Sample",
  #   df_independents = "Independents",
  #   df_partisans = "Partisans",
  #   df_democrats = "Democrats",
  #   df_republicans = "Republicans",
  #   stop("Unknown outcome variable")
  # )
  #

  #######  Bandwidth Robustness Checks #######

  # Set vector of bandwidths (e.g., 2 to 12)
  bandwidths <- seq(4, 16, by = 1)

  # Store results
  results_bandwidths <- map_dfr(bandwidths, function(h) {
    r <- rdrobust(y = dataframe[[outcome]], x = dataframe$age, c = 26, h = h, p = 1, covs = covariates)

    tibble(
      bandwidth = h,
      estimate = r$coef[estimate_type],
      se = r$se[estimate_type]
    )
  })

  # Plot
  graph_bandwidths <- ggplot(results_bandwidths, aes(x = bandwidth, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.15) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Bandwidth",
      y = "LATE",
      title = "Sensitivity of RD Estimate to Bandwidth Choice",
      subtitle = "(1st-Order Polynomial, Cutoff = 26)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )


  ## Cutoff Robustness Checks ##

  # Set vector of cutoffs
  cutoffs <- seq(24, 28, by = 1)

  # Store results
  results_cutoffs <- map_dfr(cutoffs, function(cutoff) {
    r <- rdrobust(y = dataframe[[outcome]], x = dataframe$age, c = cutoff, p = 1, covs = covariates)

    tibble(
      cutoff = cutoff,
      estimate = r$coef[estimate_type],
      se = r$se[estimate_type]
    )
  })


  # Plot
  graph_cutoffs <- ggplot(results_cutoffs, aes(x = cutoff, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.15) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Cutoff",
      y = "LATE",
      title = "Sensitivity of RD Estimate to Cutoff Choice",
      subtitle = "(MSE Bandwidth, 1st-Order Polynomial)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )


  ## Polynomial Robustness Checks ##

  # Set vector of cutoffs
  polynomials <- seq(0, 3, by = 1)

  # Store results
  results_polynomials <- map_dfr(polynomials, function(polynomial) {
    r <- rdrobust(y = dataframe[[outcome]], x = dataframe$age, c = 26, p = polynomial, covs = covariates)

    tibble(
      polynomial = polynomial,
      estimate = r$coef[estimate_type],
      se = r$se[estimate_type]
    )
  })


  # Plot
  graph_polynomials <- ggplot(results_polynomials, aes(x = polynomial, y = estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = estimate - 1.96 * se, ymax = estimate + 1.96 * se), width = 0.15) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Polynomial Degree",
      y = "LATE",
      title = "Sensitivity of RD Estimate to Polynomial Degree",
      subtitle = "(MSE Bandwidth, Cutoff = 26)"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )


  (graph_bandwidths | graph_cutoffs | graph_polynomials) +
    plot_annotation(
      title = paste(
        "Robustness Checks:",
        outcome_name,
        estimate_name,
        "Among",
        subset_name,
        control_type
      ),
      theme = theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    )
}

df_set <- list(df, df_independents, df_partisans, df_democrats, df_republicans)

df_set <- as.vector(df_set)


dataframes <- list(
  "Full Sample" = df,
  "Independents" = df_independents,
  "Republicans" = df_republicans
)

outcomes <- c("media_index")
estimate_types <- c(1, 2, 3)
covariate_options <- c(FALSE, TRUE)

# Create all combinations
combinations <- expand.grid(
  subset_name = names(dataframes),
  outcome = outcomes,
  estimate_type = estimate_types,
  include_covariates = covariate_options,
  stringsAsFactors = FALSE
)

# Run all combinations
library(purrr)

# For Quarto/RMarkdown, explicitly print plots
walk(1:nrow(combinations), function(i) {
  subset_name <- combinations$subset_name[i]
  outcome <- combinations$outcome[i]
  estimate_type <- combinations$estimate_type[i]
  include_covariates <- combinations$include_covariates[i]
  
  # Get the actual dataframe
  df_to_use <- dataframes[[subset_name]]
  
  # Create and print the plot
  p <- get_robustnesssplots(
    dataframe = df_to_use,
    outcome = outcome,
    estimate_type = estimate_type,
    include_covariates = include_covariates,
    subset_name = subset_name
  )
  
  # Explicitly print for Quarto/RMarkdown
  print(p)
  
  # Add spacing between plots
  cat("\n\n")
})


dataframes <- list(
  "Full Sample" = df,
  "Independents" = df_independents,
  "Republicans" = df_republicans
)

