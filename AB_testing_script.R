N_view <- 40000
N_click <- 3200
CTP <- N_click/N_view
CTP_sd <- sqrt(CTP*(1-CTP)/N_view)
N_enrollment <- 660
Prob_pay <- 0.53
Pay_sd <- sqrt(Prob_pay*(1-Prob_pay)/N_enrollment)
Gr_convert <- N_enrollment/N_click
Gr_convert_sd <- sqrt(Gr_convert*(1-Gr_convert)/N_click)
Net_convert <- 0.1093125
Net_convert_sd <- sqrt(Net_convert*(1-Net_convert)/N_click)

library(pwr)
AB_sample_size <- function(base, d_min, sig_level, stat_power){
  variant <- base + d_min
  es <- ES.h(base, variant)
  sample_size <- pwr.2p.test(h=es, n= , sig.level=sig_level, power=stat_power)$n
  return(sample_size)
}

AB_sample_size(Prob_pay, 0.01, 0.05, 0.8)
AB_sample_size(Gr_convert, -0.01, 0.05, 0.8)
AB_sample_size(Net_convert, 0.0075, 0.05, 0.8)

control_group <- read.csv("/Users/danichao/Documents/AB_testing/Final Project Results - Control.csv")
control_pageviews <- control_group$Pageviews[1:23]
control_pageviews_total <- sum(control_pageviews)
control_clicks <- control_group$Clicks[1:23]
control_clicks_total <- sum(control_clicks)
control_CTR <- control_clicks_total/control_pageviews_total
control_enroll <- control_group$Enrollments[1:23]
control_enroll_total <- sum(control_enroll)
control_pay <- control_group$Payments[1:23]
control_pay_total <- sum(control_pay)
control_Gr <- control_enroll_total/control_clicks_total
control_Gr_sd <- sqrt(control_Gr*(1-control_Gr)/control_clicks_total)
control_Net_convert <- control_pay_total/control_clicks_total

experiment_group <- read.csv("/Users/danichao/Documents/AB_testing/Final Project Results - Experiment.csv")
experiment_pageviews <- experiment_group$Pageviews[1:23]
experiment_pageviews_total <- sum(experiment_pageviews)
experiment_clicks <- experiment_group$Clicks[1:23]
experiment_clicks_total <- sum(experiment_clicks)
experiment_CTR <- experiment_clicks_total/experiment_pageviews_total
experiment_enroll <- experiment_group$Enrollments[1:23]
experiment_enroll_total <- sum(experiment_enroll)
experiment_pay <- experiment_group$Payments[1:23]
experiment_pay_total <- sum(experiment_pay)
experiment_Gr <- control_enroll_total/control_clicks_total
experiment_Gr_sd <- sqrt(experiment_Gr*(1-experiment_Gr)/experiment_clicks_total)
experiment_Net_convert <- experiment_pay_total/experiment_clicks_total

Pageview_invariant_check <- function(control, experiment){
  metric = experiment/(control+experiment)
  l_bound <- 0.5-1.96*sqrt(0.5*0.5/(control+experiment))
  u_bound <- 0.5+1.96*sqrt(0.5*0.5/(control+experiment))
  
  if (metric < l_bound){
    print("Check before proceed!")
  }
  else if (metric > u_bound){
    print("Check before proceed!")
  }
  else {
    "You can proceed."
  }
}

Pageview_invariant_check(control_pageviews_total, experiment_pageviews_total)

CTR_invariant_check <- function(control_x, experiment_x, control_n, experiment_n){
  mean_pool <- (control_x+experiment_x)/(control_n+experiment_n)
  sd_pool <- sqrt(mean_pool*(1-mean_pool)*(1/control_n+1/experiment_n))
  
  mean_diff <-  experiment_x/experiment_n - control_x/control_n
  
  l_bound <- -1.96*sd_pool
  u_bound <- 1.96*sd_pool
  
  if (mean_diff < l_bound){
    sprintf("CTR is not even! Check your CTR!")
  }
  else if (mean_diff > u_bound){
    sprintf("CTR is not even! Check your CTR!")
  }
  else {
    "You can proceed."
  }
}

CTR_invariant_check(N_click, control_clicks_total, N_view, control_pageviews_total)
CTR_invariant_check(N_click, experiment_clicks_total, N_view, experiment_pageviews_total)
CTR_invariant_check(control_clicks_total, experiment_clicks_total, control_pageviews_total, experiment_pageviews_total)

AB_test <- function(control_x, experiment_x, control_n, experiment_n, diff_prac, z_score){
  mean_pool <- (control_x+experiment_x)/(control_n+experiment_n)
  sd_pool <- sqrt(mean_pool*(1-mean_pool)*(1/control_n+1/experiment_n))
  
  mean_diff <- experiment_x/experiment_n - control_x/control_n
  
  l_bound <- mean_diff - z_score*sd_pool
  u_bound <- mean_diff + z_score*sd_pool
  print(c(l_bound, u_bound))
  
  if (l_bound > 0){
    if (diff_prac < l_bound){
      sprintf("The Exp. rate is higher for the business.")
    }
    else {
      sprintf("The Exp. rate is statistically higher, but there is no difference for the business.")
    }
  }
  else if (u_bound < 0){
    if (diff_prac > u_bound){
      sprintf("The Exp. rate is lower for the business.")
    }
    else {
      sprintf("The Exp. rate is statistically lower, but there is no difference for the business.")
    }
  }
  else {
    "There is no statistical difference between Con. and Exp.."
  }
  
}
AB_test(control_enroll_total, experiment_enroll_total, control_clicks_total, experiment_clicks_total, -0.01, 1.96)
AB_test(control_pay_total, experiment_pay_total, control_clicks_total, experiment_clicks_total, 0.0075, 1.96)

TheAB_test_alt <- function(control_x, experiment_x, control_n, experiment_n, diff_prac, z_score){
  mean_pool <- (control_x+experiment_x)/(control_n+experiment_n)
  sd_pool <- sqrt(mean_pool*(1-mean_pool)*(1/control_n+1/experiment_n))
  
  mean_diff <- experiment_x/experiment_n - control_x/control_n
  
  l_bound <- - z_score*sd_pool
  u_bound <-   z_score*sd_pool
  print(c(l_bound, u_bound))
  
  if (mean_diff < l_bound){
    if (mean_diff < diff_prac){
      sprintf("The Exp. rate is lower for the business.")
  }
    else {
      sprintf("The Exp. rate is statistically lower, but there is no difference for the business.")
    }
  }  
  else if (mean_diff > u_bound){
    if (mean_diff > diff_prac){
      sprintf("The Exp. rate is higher for the business.")
    }
    else {
      sprintf("The Exp. rate is statistically higher, but there is no difference for the business.")
    }
  }  
  else {
    "There is no statistical difference."
  }
}
  
control_gc_days <- control_enroll/control_clicks
experiment_gc_days <- experiment_enroll/experiment_clicks

control_pay_days <- control_pay/control_clicks
experiment_pay_days <- experiment_pay/experiment_clicks

ratio_diff <- sum(control_gc_days > experiment_gc_days)/length(control_gc_days)
ratio_sd <- sqrt(ratio_diff*(1-ratio_diff)/length(control_gc_days))
ratio_diff-1.96*ratio_sd
