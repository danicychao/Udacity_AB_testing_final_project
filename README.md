# Udacity AB Testing Final Project

This repo contains my R codes and report for the [Udacity A/B Testing Project](https://www.udacity.com/course/ab-testing--ud257), showcasing both my statistical knowledge and R programming skills. 
The full detailed analysis is [here](https://github.com/danicychao/Udacity_AB_testing_final_project/blob/main/AB_Testing_Final_Report.pdf), including the flowchart of the A/B experiment, metric selection and calculation, and hypothesis testing.

## Introduction

Udacity wants to improve the overall student experience. They come up with a new design, "free trial screener". Before they officially implement the free trial screener in the website, 
they want to test the free trial screener to see if it can really help them to improve student experience. Therefore, they launch this A/B testing experiment on the free trial screener
and collect the feedback from students. In this project, I analyse the A/B test results with R and further provide data-driven recommendation.

Refer to [here](https://docs.google.com/document/u/1/d/1aCquhIqsUApgsxQ8-SQBAigFDcfWVVohLEXcV6jWbdI/pub) for more detail.

## Notes

- Although retention is the most direct metric, it requires too many page views and will make the experiment take too long. Instead, I use gross conversion and net conversion as the evaluation metrics.

- Unlike the Bonferroni test, we want both the hypothesis tests on gross conversion and net conversion to pass. In other words, the Bonferroni is an "OR" condition, but our scenario here is an "AND" condition.

- I use the default `ES.h` function and `pwr.2p.test` function from the `pwr` package in R to estimate required sample size for sufficient statistica power. The estimated sample size is slightly different from the estimate of the famous [Evan Miller's A/B test sample size calculator](https://www.evanmiller.org/ab-testing/sample-size.html). The discrepancy mainly results from the different methods estimating the variance of control and experiment groups, which are both unknown. In this project, difference is around 1-2% in the sample size, practically neglectable.

## Summary


