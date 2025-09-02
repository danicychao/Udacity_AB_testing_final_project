# --------------------------------------------------------
# AB Testing Script
#
# Purpose:
#   This script analyzes an A/B testing experiment,
#   covering from launch preparation to result analysis.
#   It includes:
#     - baseline metric calculations
#     - sample size estimation
#     - sanity checks
#     - hypothesis testing functions
# 
# Input data:
#   - Final_Project_Results_Control.csv
#   - Final_Project_Results_Experiment.csv
#   - Final_Project_Baseline_Values.csv
# --------------------------------------------------------

# ---- Load libraries ----
library(pwr)
library(here)

# ---- Define functions ----

#' Estimate sample size
#' 
#' @param base Baseline conversion rate
#' @param d_min Minimum meaningful change
#' @param sig_level Significance level
#' @param stat_power The sensitivity
#' 
#' @return Required sample size per variation (e.g., experiment group)
AB_sample_size <- function(base, d_min, sig_level, stat_power){
  variant <- base + d_min
  es <- ES.h(base, variant)
  sample_size <- pwr.2p.test(h=es, n= , sig.level=sig_level, power=stat_power)$n
  return(sample_size)
}

#' Read control/experiment group data
#' 
#' @param df Data frame of control/experiment group data
group_sample <- function(df) {
  pageviews <- df$Pageviews[1:23]
  clicks <- df$Clicks[1:23]
  enrollments <- df$Enrollments[1:23]
  payments <- df$Payments[1:23]
  
  pageviews_total <- sum(pageviews)
  clicks_total <- sum(clicks)
  enrollments_total <- sum(enrollments)
  payments_total <- sum(payments)
  
  list(
    pageviews = pageviews,
    pageviews_total = pageviews_total,
    clicks = clicks,
    clicks_total = clicks_total,
    enrollments = enrollments,
    enrollments_total = enrollments_total,
    payments = payments,
    payments_total = payments_total
  )
}

#' Check if samples are evenly split between control and experiment groups
#' 
#' @param control Number of samples in control group
#' @param experiment Number of samples in experiment group
#' @param zscore z-score corresponding to desired confidence level (default = 1.96 for 95% CL)
#' 
#' @return List of even splitting check result, observed metric, and confidence interval
Bernoulli_invariant_check <- function(control, experiment, zscore = 1.96){
  total <- control + experiment
  metric = experiment / total
  se <- sqrt(0.5*0.5/total)
  l_bound <- 0.5 - zscore*se
  u_bound <- 0.5 + zscore*se
  
  decision <- if (metric < l_bound || metric > u_bound) {
    "Check before proceeding!"
  } 
  else {
    "You can proceed."
  }
  
  return (list(
    decision = decision,
    metric = metric,
    confidence_interval = c(l_bound, u_bound)
  ))
}

#' Check if rates are equivalent between control and experiment groups
#' 
#' @param control_x Number of conversions in control group (e.g. clicks, enrollments, payments)
#' @param experiment_x Number of conversions in experiment group
#' @param control_n Number of samples in control group (e.g. page views, clicks, enrollments)
#' @param experiment_n Number of samples in experiment group
#' @param zscore z-score corresponding to desired confidence level (default = 1.96 for 95% CL)
#' 
#' @return List of sanity check result, observed metric, and confidence interval
rate_invariant_check <- function(control_x, experiment_x, control_n, experiment_n, zscore = 1.96){
  mean_pool <- (control_x+experiment_x)/(control_n+experiment_n)
  sd_pool <- sqrt(mean_pool*(1-mean_pool)*(1/control_n+1/experiment_n))
  
  mean_diff <-  experiment_x/experiment_n - control_x/control_n
  
  l_bound <- -zscore*sd_pool
  u_bound <- zscore*sd_pool
  
  decision <- if (mean_diff < l_bound || mean_diff > u_bound){
    "Check your experiment setups!"
  }
  else {
    "You can proceed."
  }
  
  return (list(
    decision = decision,
    metric = mean_diff,
    confidence_interval = c(l_bound, u_bound)
  ))
}

#' Test hypothesis function
#' 
#' @param control_x Number of conversions in control group (e.g. clicks, enrollments, payments)
#' @param experiment_x Number of conversions in experiment group
#' @param control_n Number of samples in control group (e.g. page views, clicks, enrollments)
#' @param experiment_n Number of samples in experiment group
#' @param diff_prac Practical difference for business
#' @param z_score z-score corresponding to test level
#' 
#' @return List of AB test result, 
#'         observed difference between control and experiment groups,
#'         confidence interval,
#'         and minimum meaningful difference for business  
AB_test <- function(control_x, experiment_x, control_n, experiment_n, diff_prac, z_score){
  mean_pool <- (control_x+experiment_x)/(control_n+experiment_n)
  sd_pool <- sqrt(mean_pool*(1-mean_pool)*(1/control_n+1/experiment_n))
  
  mean_diff <- experiment_x/experiment_n - control_x/control_n
  
  l_bound <- mean_diff - z_score * sd_pool
  u_bound <- mean_diff + z_score * sd_pool
  
  result <- if (l_bound > 0){
    if (abs(diff_prac) < l_bound){
      "The Exp. rate is higher for the business."
    }
    else {
      "The Exp. rate is statistically higher, but there is no difference for the business."
    }
  }
  else if (u_bound < 0){
    if (diff_prac < abs(u_bound)){
      "The Exp. rate is lower for the business."
    }
    else {
      "The Exp. rate is statistically lower, but there is no difference for the business."
    }
  }
  else {
    "There is no statistical difference between Con. and Exp.."
  }
  
  return(list(
    mean_diff           = mean_diff,
    confidence_interval = c(l_bound, u_bound),
    practical_diff      = abs(diff_prac),
    result            = result
  ))
}

#' Sign test function
#' 
#' @param control_x Number of conversions in control group (e.g. clicks, enrollments, payments)
#' @param experiment_x Number of conversions in experiment group
#' @param control_n Number of samples in control group (e.g. page views, clicks, enrollments)
#' @param experiment_n Number of samples in experiment group
#' 
#' @return List of sign test result,
#'         portion of days that experiment group has higher rate than control group,
#'         observed value of test statistics,
#'         and p-value
sign_test <- function(control_x, experiment_x, control_n, experiment_n){
  control_days <- control_x/control_n
  experiment_days <- experiment_x/experiment_n
  days_total <- length(control_days)
  
  ratio_diff <- sum(experiment_days > control_days)/days_total
  ratio_sd <- sqrt(ratio_diff*(1-ratio_diff)/days_total)
  
  test_statistics <- abs(ratio_diff - 0.5)/sqrt(ratio_diff * (1 - ratio_diff) / days_total)
  p_value <- 2*(1-pnorm(test_statistics))
  
  result <- if (p_value < 0.05){
    if (ratio_diff > 0.5){
      "The Exp. rate is higher in sign test."
    }
    else {
      "The Exp. rate is lower in sign test."
    }
  }
  else {
    "No difference between Con. and Exp. in sign test."
  }

  return(list(
    ratio_diff           = ratio_diff,
    test_statistics      = test_statistics,
    p_value              = p_value,
    result               = result
  ))
}

# ---- Start execution ----

# ---- Calculate baseline metric ----
# ---- Baseline values from Final_Project_Baseline_Values.csv ----
baseline_views <- 40000
baseline_clicks <- 3200
baseline_CTP <- baseline_clicks/baseline_views
baseline_CTP_sd <- sqrt(baseline_CTP*(1-baseline_CTP)/baseline_views)
baseline_enrollment <- 660
baseline_pay_prob <- 0.53
baseline_pay_prob_sd <- sqrt(baseline_pay_prob*(1-baseline_pay_prob)/baseline_enrollment)
baseline_gross_conversion <- baseline_enrollment/baseline_clicks
baseline_gross_conversion_sd <- sqrt(baseline_gross_conversion*(1-baseline_gross_conversion)/baseline_clicks)
baseline_net_conversion <- 0.1093125
baseline_net_conversion_sd <- sqrt(baseline_net_conversion*(1-baseline_net_conversion)/baseline_clicks)

# ---- Estimate required sample size ----
AB_sample_size(baseline_pay_prob, 0.01, 0.05, 0.8) # Required "enrollments"
AB_sample_size(baseline_gross_conversion, 0.01, 0.05, 0.8) # Required pageviews
AB_sample_size(baseline_net_conversion, 0.0075, 0.05, 0.8) # Required pageviews

# ---- Load control/experiment data ----
control_group <- group_sample(read.csv(here("Final_Project_Results_Control.csv")))
experiment_group <- group_sample(read.csv(here("Final_Project_Results_Experiment.csv")))

# ---- Sanity checks of pageviews and click-through-rate ----
Bernoulli_invariant_check(control_group$pageviews_total, experiment_group$pageviews_total) # Check pageview
rate_invariant_check(control_group$clicks_total, experiment_group$clicks_total, 
                     control_group$pageviews_total, experiment_group$pageviews_total) # Check CTR

### Extra sanity checks with baseline values
rate_invariant_check(baseline_clicks, control_group$clicks_total, baseline_views, control_group$pageviews_total) 
rate_invariant_check(baseline_clicks, experiment_group$clicks_total, baseline_views, experiment_group$pageviews_total) 


# ---- Hypothesis testing ----

### Effect size test
### zscore = 1.96 for 5% confindence level

### practical difference, d_min=0.01, for gross conversion
AB_test(control_group$enrollments_total, experiment_group$enrollments_total, 
        control_group$clicks_total, experiment_group$clicks_total, -0.01, 1.96) 

### practical difference, d_min=0.0075, for net conversion
AB_test(control_group$payments_total, experiment_group$payments_total, 
        control_group$clicks_total, experiment_group$clicks_total, 0.0075, 1.96)

### Sign test
sign_test(control_group$enrollments, experiment_group$enrollments, control_group$clicks, experiment_group$clicks)
sign_test(control_group$payments, experiment_group$payments, control_group$clicks, experiment_group$clicks)



