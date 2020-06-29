---
jupyter:
  jupytext:
    formats: ipynb,md
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.2'
      jupytext_version: 1.4.2
  kernelspec:
    display_name: R
    language: R
    name: ir
---

# Stratified analysis of case-crossover or case-control data

```R
#load packages
library(tidyverse)
library(survival)
library(cowplot)
```

## About the data  

The analysis below is ran on data from case-crossover study. Case-crossover design is similar to case-control design, but there are no control patients. Instead each patient at different time serves as his or her own control. See [M. Maclure and M. A. Mittleman](https://www.annualreviews.org/doi/abs/10.1146/annurev.publhealth.21.1.193) for reference.

I used conditional logistic regression (CLG) from package [survival](https://cran.r-project.org/web/packages/survival/index.html) to test the hypotesis that exposure to a stressful life event can trigger self-poisoning.

>**Important note**  
>*This analysis is based on simulated data. These are not results of an actual study and no conclusions about the effect of stressful life events on triggering self-poisonig should be drawn from them. I created this notebook only to demonstrate how similar data can be analysed and presented in R. However, sample structure and frequencies of occurrence of stressful life events are based on data from actual, smaller sample of patienst who self-poisoned (n = 124).*
>*Here you can find a refference on life events triggering suicide attempts:*
>1. Bagge, C. L., Glenn, C. R., & Lee, H.-J. (2013). Quantifying the impact of recent negative life events on suicide attempts. Journal of Abnormal Psychology, 122(2), 359–368. [https://doi.org/10.1037/a0030371](https://doi.org/10.1037/a0030371)
>2. Liu, B.-P., Zhang, J., Chu, J., Qiu, H.-M., Jia, C.-X., & Hennessy, D. A. (2019). Negative life events as triggers on suicide attempt in rural China: a case-crossover study. Psychiatry Research, 276, 100–106. [https://doi.org/10.1016/j.psychres.2019.04.008](https://doi.org/10.1016/j.psychres.2019.04.008)
>3. Conner, K. R., Houston, R. J., Swogger, M. T., Conwell, Y., You, S., He, H., … Duberstein, P. R. (2012). Stressful life events and suicidal behavior in adults with alcohol use disorders: Role of event severity, timing, and type. Drug and Alcohol Dependence, 120(1–3), 155–161. [https://doi.org/10.1016/j.drugalcdep.2011.07.013](https://doi.org/10.1016/j.drugalcdep.2011.07.013)

There are three groups of variables in the dataset:

1. Variables essential to run conditional logistic regression:

  - **exposure** `TRUE` value indicates that an exposure to a stressful life event occured in a given time window
  - **outcome** `TRUE` value indicates that an outcome, i.e. self-poisoning occured in the time window
  - **id** indentifies a patient and is used to identify strata in `clogit` call used to run CLG

2. **day** is a helper variable identifing case and control days (case day is a day when a patient self-poisoned, for each case day `outcome == TRUE`)
3. Grouping variables are used to compare the effect of stressful life events in groups having varying characteristics:

- **women** `TRUE` indicates that a patient is a woman
- **psychiatric_history** `TRUE` indicates that a patient has a history of psychiatric consultation
- **depression** `TRUE` indicates that a patient was ever diagnosed with depression
- **attempts** `TRUE` indicates that a patient made at least one suicide attempt before index self-poisoning

Let's take a look at the dataset:

```R
#load dataset
events_data <- read_tsv("dataset_.tsv")
glimpse(events_data)
```

## Patient's charcteristics

There are 299 unique patients in the dataset.

The code below creates a data frame with information about characteristics of each individual patient (in `events_data` each patient appears eight times). It does it by keeping only the one occurence of each id in the dataset, in this case only the records where `day == "case_day"`.

```R
patients_data <- events_data %>%
filter(day == "case_day") %>%
select(-day, -outcome, -exposure)
glimpse(patients_data)
```

Let's take a look how many patients with given characteristics are in the dataset:

```R
n_total = nrow(patients_data)
count(patients_data, women) %>% mutate(proportion = n / n_total)
count(patients_data, psychiatric_history) %>% mutate(proportion = n / n_total)
count(patients_data, depression) %>% mutate(proportion = n / n_total)
count(patients_data, attempts) %>% mutate(proportion = n / n_total)
```

## Exposures on each day

Now lets take a look at the number of exposures on each day.

The line below runs a script that sets the theme for plots.

```R
source("plots_theme.R", local = TRUE)
```

We start with creating a data frame containing only rows, where `exposure == TURE` and counting exposures on each day. The code recreates variable `outcome`to use it when drawing a plot.

```R
exposures_per_day <- events_data %>%
filter(exposure) %>%
group_by(day) %>%
summarise(count = n()) %>%
mutate(outcome = day == "case_day")
```

The plot below shows number of exposures to stressful life events on each day. `geom_text` was used to display number of exposures above each bar.

```R
ggplot(data = exposures_per_day, aes(x = day)) +
  geom_bar(aes(weight = count, fill = outcome), show.legend = TRUE) +
  geom_text(aes(y = count, label = count, fontface ="bold"),
    nudge_y = 2,
    size = 3.1,
    hjust = "middle",
    color = "gray20") +
  labs(
    x = "Day",
    y = "Number of patients exposed to any stressful event",
    title = "Patients exposed to any stresful event on case and control days"
  ) +
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5)) +
  scale_fill_manual(values = c("#21908CFF", "#440154FF"))
```

## Compare numbers of self-poisonings on exposed and unexposed days

Since we are going to compare the odds of self-poisoning on days with and without exposure several times, it is usefull to create a function drawing this plot.

In the function below using grouping variable is optional.

The function takes the following arguments:

- `dtst` a data frame with variables outcome and exposure, should also contain grouping variable if it is provided within the function call
- `gv` optional grouping variable
- `gv_label_false` and `gv_label_true` are optional variables used when `gv` is provided to label its levels, default to "FALSE" and "TRUE" respectively.

The function:

1. Checks if `gv` was provided and groups `dtst` by `gv` (if provided), outcome and exposure
2. Creates a data frame for a plot, with the counts in each group. Since counts are often used in tables summarising data from case-control and case-crossover studies, it also creates character variable `weight_string` to display patient counts on the bars (to my best knowledge displaying it nicely cannot be achieved without creating additional variable)
3. Draws the plot
4. If `gv` is provided it uses it for `facet_wrap`.

```R
create_outcome_plot <- function(dtst, gv, gv_label_false = "FALSE", gv_label_true = "TRUE") {
  #group by grouping variable (when provided) and exposure and outcome
  if(!missing(gv)){
    gv <- enquo(gv)
    outcome_plot_data <- dtst %>%
    mutate(group = !!gv) %>%
    group_by(group, exposure, outcome)
    }else{
    outcome_plot_data <- dtst %>%
    group_by(exposure, outcome)
    }

  #data for the plot  
  outcome_plot_data <- outcome_plot_data %>%
  summarise(weight = n()) %>%
  mutate(weight_string = paste0("n = ", weight)) %>%
  arrange(desc(exposure), desc(outcome))

  #plot call
  outcome_plot <- ggplot(data = outcome_plot_data, aes(x = exposure, y = weight)) +
  geom_col(aes(fill = outcome), position = "fill", width = 0.8) +
  geom_text(aes(label = weight_string),
    position = position_fill(vjust = 0.5),
    size = 3,
    color = "gray90",
    fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("#21908CFF", "#440154FF"), labels = c("no self-poisoning", "self-poisoning")) +
  scale_x_discrete(labels = c("no", "yes")) +
  labs(
    y = "proportion of patients",
    x = "exposure to stressful life event",
    fill = element_blank())

  if(!missing(gv)){
    mk_lbls <- function(string){
      if_else(string == "TRUE", gv_label_true, gv_label_false)
    }
    outcome_plot <- outcome_plot +
    facet_wrap(~ group, labeller = labeller(group = mk_lbls), ncol = 1)
    }else{}

  return(outcome_plot)
}
```

After all this code it takes only a function call and a title to display the plot.

```R
(outcome_plot_all <- create_outcome_plot(events_data) +
  labs(title = "Self-poisonings on days with and without stressful events"))
```

## Fitting a model

To fit conditional logistic regression model I use `survival::clogit`. Since the logic behind using conditional logistic regression is to take into account inter-patient variablility, patient `id` is provided as strata argument. *Please note that these are not the same strata that are used in the stratified analysis below*.

```R
#all data model
all <- clogit(outcome ~ exposure + strata(id),
  data = events_data)
summary(all)
```

OR is exp(coef), so in this case the odds of self-poisoning is 6.79 times higher after exposure to a stressful life event.

## Comparing raw data in groups

Now we can easily compare raw data in groups using plotting function created before. Since I want to display the plots on a grid, I don't need title, legend and x axis title and text on each plot. To get rid of them let's create a function `plots_for_grid`, which slightly modifies the result of `create_outcome_plot` (it takes plot as an argument).

```R
plots_for_grid <- function(plot) {
  new_plot <- plot +
    theme(legend.position = "none",
          plot.margin = margin(l = 20, r = 10, t = 0, b = 0),
          axis.title.x = element_blank(),
          axis.text.x = element_blank()
         ) +
    labs(title = element_blank())
  return(new_plot)
}
```

Now we can create a plot for each grouping variable...

```R
plot_women <- plots_for_grid(create_outcome_plot(events_data,
  women,
  gv_label_false = "men",
  gv_label_true = "women"))

plot_psychiatric_history <- plots_for_grid(create_outcome_plot(events_data,
  psychiatric_history,
  gv_label_false = "patients who didn't receive previous psychiatric treatment",
  gv_label_true = "patients who received previous psychiatric treatment"))

plot_depression <- plots_for_grid(create_outcome_plot(events_data,
  depression,
  gv_label_false = "patients not diagnosed with depression",
  gv_label_true = "patients diagnosed with depression"))

plot_attempts <- plots_for_grid(create_outcome_plot(events_data,
  attempts,
  gv_label_false = "patients who didn't make previous suicide attempts",
  gv_label_true = "patients who made previous suicide attempts"))
```

...and display it in a grid. Variable `legend` is extracted by `cowplot::get_legend` from outcome plot for total number of patients (group plots do not have legends). Title for the whole grid is created by `cowplot::draw_label`.

```R
legend <- get_legend(outcome_plot_all)
title <- ggdraw() +
  draw_label(
    "Odds and number of self-poisonings in groups",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 12)
```

Title and legend are aligned in a grid along with plots. Bottom plot is modified to have x axis (other plots do not have it, so it can be displayed only once at the bottom). To preserve bars width in rel_heights parameter it takes some more space.

```R
options(repr.plot.width = 6, repr.plot.height = 11)
plot_grid(title,
  legend,
  plot_women,
  plot_psychiatric_history,
  plot_depression,
  plot_attempts +
    theme(axis.text.x = element_text(),
      plot.margin = margin(l = 20, r = 10, t = 0, b = 20),
      axis.title.x = element_text()),
  ncol = 1,
  rel_heights = c(0.2, 0.1, 1.5, 1.5, 1.5, 2.2)
  )
```

## Stratified ORs

### Goal

The goal of stratified analysis presented here is to compare ORs and verify if they differ significantly between groups. To achieve this statistical significance of rate ratio (RR) is verified with conditional logistic regression with interaction between exposure and grouping variable. Grouping variable identifies different strata. Here we have e.g. women compared to man (`women == TRUE` vs `women == FALSE`) or patients diagnosed with depression compared to patients without this diagnosis (`depression == TRUE` vs `depression == FALSE`). *Please note, that this strata here should not be confused with strata in `survival::clogit` call used to run CLG!*

Since `survival::clogit` do not require adding both variables of the interaction term as separate terms, the call is: `outcome ~ exposure*grouping_variable + strata(id)`.

### Example

As an example, to compare ORs between sexes we have:

```R
(interaction <- clogit(outcome ~ exposure*women + strata(id), data = events_data))
```

`exp(coef)` in the first row below the call is OR for self-poisoning after exposure **in the group for which grouping variable is `FALSE`**, in this example men. Second row has only NAs, because patients were their own controls, so the level of grouping variable was constant during all case and control times. `exp(coef)` in the third row is the coefficient of the interaction term and it translates to the rate ratio (RR) between `women == FALSE` and `women == TRUE` group. So we know, that the OR of self-poisoning after exposure to a stressful life event is 1.52 times higher among women than among men.

Just to check, OR among men:

```R
(men <- clogit(outcome ~ exposure + strata(id),
    data = events_data %>% filter(!women)))
```

OR among women:

```R
(women <- clogit(outcome ~ exposure + strata(id),
    data = events_data %>% filter(women)))
```

The code below extracts `exp(coef)` from the summary of the model `men` and `women`, then extracts RR from the model with interaction and does the algebra necessary to show that `exp(coef)` is indeed RR.

```R
(m <- summary(men)$coefficients[2])
(w <- summary(women)$coefficients[2])
i <- summary(interaction)$coefficients[3, 2]
m * i
```

In this example RR is not statistically significant, so the data do not let us assume that the effect of stressful life events on triggering self poisoning is indeed more pronounced among women.

### Function to extract ORs and RR from the models

The function below extracts ORs for both levels of the grouping variable along with their confidence intervals and RR along with its p value.

It takes dataset and grouping variable as its arguments (grouping variable name is passed as a string). The dataset should have outcome, exposure and id variables and grouping variable should have two levels encoded as `TRUE` and `FALSE`.

The function:

1. Fits models for `FALSE` and `TRUE` groups to get ORs in this groups
2. Fits the mode with interaction term
3. Creates data frame with OR for both levels of the grouping variable and RR
4. Adds info about RR significance and stars to the data frame

*Actually only one model with interaction term could be fitted, but since the dataset is not so big, all three models are fitted for function code clarity.*

```R
ORs_in_groups <- function(dataset, grouping_var) {
  false_group <- clogit(outcome ~ exposure + strata(id),
    data = dataset %>% filter(!dataset[[grouping_var]]))
  true_group <- clogit(outcome ~ exposure + strata(id),
    data = dataset %>% filter(dataset[[grouping_var]]))

  full_model <- clogit(outcome ~ exposure*dataset[[grouping_var]] + strata(id),
  data = dataset)

  ORs <- data.frame(
    grouping_var = grouping_var,
    grouping_var_level = c(FALSE, TRUE),
    OR = c(
      summary(false_group)$conf.int[ , "exp(coef)"],
      summary(true_group)$conf.int[ , "exp(coef)"]
      ),
    OR_lower_95 = c(
      summary(false_group)$conf.int[ , "lower .95"],
      summary(true_group)$conf.int[ , "lower .95"]
      ),
    OR_upper_95 = c(
      summary(false_group)$conf.int[ , "upper .95"],
      summary(true_group)$conf.int[ , "upper .95"]
      ),
    RR = rep(summary(full_model)$coefficients[3, "exp(coef)"], 2),
    RR_p_value = rep(summary(full_model)$coefficients[3, "Pr(>|z|)"], 2)
   )

  ORs <- ORs %>%
  mutate (RR_sig = case_when(
    RR_p_value < 0.1 & RR_p_value >= 0.05 ~ paste0("ns, p = ", round(RR_p_value, digits = 3)),
    RR_p_value < 0.05 & RR_p_value >= 0.01 ~ "p < 0.05",
    RR_p_value < 0.01 & RR_p_value >= 0.001 ~ "p < 0.01",
    RR_p_value < 0.001 ~ "p < 0.001",
    TRUE ~ "ns"
    )) %>%
  mutate(stars = case_when(
    RR_p_value < 0.1 & RR_p_value >= 0.05 ~ "+",
    RR_p_value < 0.05 & RR_p_value >= 0.01 ~ "*",
    RR_p_value < 0.01 & RR_p_value >= 0.001 ~ "**",
    RR_p_value < 0.001 ~ "***",
    TRUE ~ ""))

  return(ORs)
}
```

Results for each of the for grouping variables can be easily combined into one data frame.

```R
women_ORs <- ORs_in_groups(events_data, "women")
psychiatric_history_ORs <- ORs_in_groups(events_data, "psychiatric_history")
depression_ORs <- ORs_in_groups(events_data, "depression")
attempts_ORs <- ORs_in_groups(events_data, "attempts")
(stratified_ORs <- rbind(women_ORs, psychiatric_history_ORs, depression_ORs, attempts_ORs))
```

### Plotting ORs

The data frame with ORs can be easily plotted. The adjustments below are optional and provide nice labels for grouping variables with information about RR and its significance level.

```R
stratified_ORs$grouping_var <- recode(stratified_ORs$grouping_var,
  "women" = "women",
  "psychiatric_history" = "psychiatric treatment history",
  "depression" = "depression diagnosis",
  "attempts" = "previous suicide attempts")

stratified_ORs <- stratified_ORs %>%
mutate(grouping_var_rr = paste0(grouping_var,
  ": RR = ",
  round(RR, digits = 2),
  stars))
```

The data frame is plotted with `ggplot::geom_point()`, where points represent ORs and `ggpolot:error_bar()`, where bars represent confidence intervals.

As you can see, overlapping error bars translate to non-significant RR.

```R
options(repr.plot.width = 6, repr.plot.height = 7)

ggplot(data = stratified_ORs, aes(x = OR, y = grouping_var_level)) +
  geom_point() +
  geom_errorbar(aes(xmin = OR_lower_95, xmax = OR_upper_95), width = 0.2) +
  geom_vline(xintercept = 1, color = "red") +
  scale_x_continuous(breaks = seq(0, 12.5, by = 1)) +
  facet_wrap(~ grouping_var_rr, ncol = 1, as.table = FALSE) +
  theme(axis.title.y = element_blank()) +
  labs(title = "Comparison of ORs in strata",
    caption = "RR significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, + p < 0.1\n
    Red line indicates OR = 1.0")
```

I hope you enjoyed stratified ORs! :)
