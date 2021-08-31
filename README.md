# stratified

## Goal
This repository presents a way to analyze and present case-crossover data in R. 

## Data disclaimer

**My goal was not to report study results, but to present how to analyze data! Thus any outcome presented in this repository should not be considered as providing any knowledge on the subject of life stress triggering self-poisoning.** Data used to present the analysis were simulated. 

## Scope of the analysis

I used condidtional logistic regression (CLG) to determine if exposure (in this example stressful life event) triggers the outcome (in this example intentional self-poisoning). 

To verify if the effect is more or less pronounced in various groups (eg. women vs men or depressed vs non-depressed patients) I used stratified analysis. Statistical sigifficance of the differences between strata was verified by including interaction term of the characteristic in question and exposure in the CLG model.
