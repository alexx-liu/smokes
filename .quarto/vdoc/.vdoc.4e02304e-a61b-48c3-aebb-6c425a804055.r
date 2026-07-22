#
#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(primer.tutorials)
library(tidymodels)
library(broom)
library(marginaleffects)
#
#
#
smokes |> 
  mutate(age = 5 * (age %/% 5)) |> 
  group_by(age, sex) |> 
  summarize(prop = mean(smoke == "Yes"), .groups = "drop") |> 
  ggplot(aes(x = age, y = prop)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    facet_wrap(~ sex) +
    scale_y_continuous(labels = scales::percent_format())
#
#
#
#| cache: true
fit_smokes <- logistic_reg(engine = "glm") |> 
  fit(smoke ~ age + sex, data = smokes)
#
#
#
library(easystats)
check_predictions(extract_fit_engine(fit_smokes))
#
#
#
tidy(fit_smokes, conf.int = TRUE)
#
#
#
library(gt)

tidy(fit_smokes, conf.int = TRUE) |> 
  gt() |> 
  fmt_number(columns = c(estimate, conf.low, conf.high), decimals = 3) |> 
  cols_label(
    term = "Term",
    estimate = "Estimate",
    conf.low = "Lower CI",
    conf.high = "Upper CI"
  ) |> 
  tab_header(title = "Logistic Regression: smoke ~ age + sex")
#
#
#
predictions(extract_fit_engine(fit_smokes),
            newdata = data.frame(age = c(30, 70, 30, 70),
                                 sex = c("Female", "Female", "Male", "Male")))
#
#
#
plot_predictions(extract_fit_engine(fit_smokes), condition = c("age", "sex"), draw = FALSE)
#
#
#
avg_comparisons(extract_fit_engine(fit_smokes), variables = "sex")
#
#
#
plot_predictions(extract_fit_engine(fit_smokes), condition = c("age", "sex"), draw = FALSE) |> 
  ggplot(aes(x = age, y = estimate, color = sex, fill = sex)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Age and Sex Predict Ever-Smoker Status",
      subtitle = "Men are consistently more likely than women to have ever smoked, across all ages",
      x = "Age",
      y = "Predicted Probability of Ever-Smoker Status",
      color = "Sex",
      fill = "Sex",
      caption = "Source: NHANES 2009–2012, via primer.tutorials::smokes"
    ) +
    theme_minimal()
#
#
#
#
#
#
#
