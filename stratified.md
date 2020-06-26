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

## Important note

*This analysis is based on simulated data. These are not results of an actual study and no conclusions about the effect of stressful life events on triggering self-poisonig should be drawn from them. I created this notebook only to demonstrate how similar data can be analysed and presented in R. However, sample structure and frequencies of occurrence of stressful life events are based on data from actual, smaller sample of patienst who self-poisoned (n = 124).*  

```R
#packages
library(tidyverse)
library(survival)
library(plotly)
```

```R
#plots theme
my_theme <- theme_classic() +
 theme(
    plot.margin = margin(30, 10, 30, 20),
    plot.title = element_text(face = "bold.italic", size = 12, hjust = 0, vjust = 10, color = "gray20"),
    plot.title.position = "plot",
    axis.title = element_text(face = "italic", color = "gray20"),
    axis.title.x = element_text(vjust = -5),
    axis.title.y = element_text(vjust = 5),
    axis.text = element_text(color = "gray20"),
    legend.position = "top",
    legend.margin = margin(c(5,5,10,0)),
    legend.title = element_text(face = "italic"),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    plot.caption.position = "panel",
    plot.caption = element_text(size = 8, face = "italic", color = "grey30", vjust = -10),
    strip.text = element_text(face = "bold")
  )
theme_set(my_theme)
```

```R
#dataset
events_data <- read_tsv("dataset_.tsv")
```

```R
exposures_per_day <- events_data %>%
filter(exposure) %>%
group_by(day) %>%
summarise(count = n())

exposures_plot <- ggplot(data = exposures_per_day, aes(x = day)) +
  geom_bar(aes(weight = count, fill = ""), show.legend = FALSE) +
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
  scale_fill_manual(values = "#FDE725FF")

exposures_plot
```

```R
create_outcome_plot <- function(dtst, gv, gv_label_false, gv_label_true) {
  if(!missing(gv)){
    gv <- enquo(gv)
    outcome_plot_data <- dtst %>%
    mutate(group = !!gv) %>%
    group_by(group, exposure, outcome)
    }else{
    outcome_plot_data <- dtst %>%
    group_by(exposure, outcome)
    }

  outcome_plot_data <- outcome_plot_data %>%
  summarise(weight = n()) %>%
  mutate(weight_string = paste0("n = ", weight)) %>%
  arrange(desc(exposure), desc(outcome))

  outcome_plot <- ggplot(data = outcome_plot_data, aes(x = exposure, y = weight)) +
  geom_col(aes(fill = outcome), position = "fill", width = 0.65) +
  geom_text(aes(label = weight_string),
    position = position_fill(vjust = 0.5),
    size = 3,
    color = "gray90",
    fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("#21908CFF", "#440154FF")) +
  labs(
    y = "proportion of patients",
    x = "exposure to stressful life event",
    fill = "self-poisoning")

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

```R
outcome_plot_all <- create_outcome_plot(events_data) +
  labs(title = "Self-poisonings on days with and without stressful events")
outcome_plot_all
```

```R
#all data model
all <- clogit(outcome ~ exposure + strata(id),
  data = events_data)
summary(all)
```

```R
#plot_women
plot_women <- create_outcome_plot(events_data, women,
  gv_label_false = "men",
  gv_label_true = "women") +
labs(title = "Number of self-poisonings: comparison by sex")
plot_women
```

```R
#plot_psychiatric_history
plot_psychiatric_history <- create_outcome_plot(events_data, psychiatric_history,
  gv_label_false = "patients who didn't receive previous psychiatric treatment",
  gv_label_true = "patients who received previous psychiatric treatment") +
labs(title = "Number of self-poisonings: comparison by psychiatric history")
plot_psychiatric_history
```

```R
#plot_depression
plot_depression <- create_outcome_plot(events_data, depression,
  gv_label_false = "patients not diagnosed with depression",
  gv_label_true = "patients diagnosed with depression") +
labs(title = "Number of self-poisonings: comparison by depression diagnosis")
plot_depression
```

```R
#plot_attempts
plot_attempts <- create_outcome_plot(events_data, attempts,
  gv_label_false = "patients who didn't make previous suicide attempts",
  gv_label_true = "patients who made previous suicide attempts") +
labs(title = "Number of self-poisonings: comparison by previous suicide attempt status")
plot_attempts
```

```R
#function which returns OR with confidence intervals for specified goups
#also returns RR with p value
#takes dataset with so specified vars as an argument:
  #outcome var is named outcome,
  #exposure var is named exposure
  #clogit strata var is named id
#gouping_var is passed as a string

ORs_in_groups <- function(dataset, grouping_var) {
  false_group <- clogit(outcome ~ exposure + strata(id),
    data = dataset %>% filter(!dataset[[grouping_var]]))
  true_group <- clogit(outcome ~ exposure + strata(id),
    data = dataset %>% filter(dataset[[grouping_var]]))
  full_model <- clogit(outcome ~ exposure*dataset[[grouping_var]] + strata(id),
  data = dataset)
  ORs <- data.frame(
    group = paste0(grouping_var, c(":FALSE", ":TRUE")),
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
    RR_p_value < 0.05 & RR_p_value >= 0.01 ~ "* p < 0.05",
    RR_p_value < 0.01 & RR_p_value >= 0.001 ~ "** p < 0.01",
    RR_p_value < 0.001 ~ "*** p < 0.001",
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

```R
women_ORs <- ORs_in_groups(events_data, "women")
psychiatric_history_ORs <- ORs_in_groups(events_data, "psychiatric_history")
depression_ORs <- ORs_in_groups(events_data, "depression")
attempts_ORs <- ORs_in_groups(events_data, "attempts")
stratified_ORs <- rbind(women_ORs, psychiatric_history_ORs, depression_ORs, attempts_ORs)
stratified_ORs$grouping_var <- substring(stratified_ORs$group, 1, regexpr(":", stratified_ORs$group) - 1)
stratified_ORs$grouping_var <- recode(stratified_ORs$grouping_var,
  "women" = "women",
  "psychiatric_history" = "psychiatric treatment history",
  "depression" = "depression diagnosis",
  "attempts" = "previous suicide attempts")
stratified_ORs$grouping_var_level <- substring(stratified_ORs$group, regexpr(":", stratified_ORs$group) + 1)
stratified_ORs <- stratified_ORs %>%
mutate(grouping_var_rr = paste0(grouping_var,
  ": RR = ",
  round(RR, digits = 2),
  stars))
```

```R
ggplot(data = stratified_ORs, aes(x = OR, y = grouping_var_level)) +
  geom_point() +
  geom_errorbar(aes(xmin = OR_lower_95, xmax = OR_upper_95), width = 0.2) +
  geom_vline(xintercept = 1, color = "red") +
  scale_x_continuous(breaks = seq(0, 12.5, by = 1)) +
  facet_wrap(~ grouping_var_rr, ncol = 1, as.table = FALSE) +
  theme(axis.title.y = element_blank()) +
  labs(title = "Comparison of ORs in strata",
    caption = "RR significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, + p < 0.1")
```
