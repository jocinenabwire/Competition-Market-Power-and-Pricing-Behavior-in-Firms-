############################################################
# Competition, Market Power, and Pricing Behaviour in Firms
# Reproducible R script for GitHub
############################################################

############################################################
# 0. Clean environment
############################################################

rm(list = ls())

############################################################
# 1. Load packages
############################################################

# Run install.packages() only once if needed:
# install.packages(c(
#   "tidyverse", "haven", "here", "janitor", "labelled",
#   "modelsummary", "fixest", "sandwich", "lmtest",
#   "GGally", "officer", "flextable", "broom", "car",
#   "marginaleffects", "scales"
# ))

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(labelled)
library(modelsummary)
library(fixest)
library(sandwich)
library(lmtest)
library(GGally)
library(officer)
library(flextable)
library(broom)
library(car)
library(marginaleffects)
library(scales)

############################################################
# 2. Create results folder
############################################################

dir.create(here("results"), showWarnings = FALSE)

############################################################
# 3. Import raw data
############################################################

# Put the .dta file in the main GitHub folder.
# If the file name changes, change only this line.

raw_data <- read_dta(here("New_Comprehensive_April_01_2026.dta")) %>%
  clean_names()

############################################################
# 4. Helper functions
############################################################

clean_negative_codes <- function(x) {
  ifelse(x < 0, NA, x)
}

winsorise <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs = probs, na.rm = TRUE)
  x <- ifelse(x < q[1], q[1], x)
  x <- ifelse(x > q[2], q[2], x)
  return(x)
}

############################################################
# 5. Construct variables
############################################################

data <- raw_data %>%
  mutate(
    firm_id    = idstd,
    country_id = as.factor(country),
    region_id  = as.factor(region),
    year       = clean_negative_codes(a14y),
    weight     = wt,
    
    sector_main   = as.factor(sector_ms),
    sector_strata = as.factor(stra_sector),
    industry_isic = clean_negative_codes(isic_v4),
    industry_fe   = as.factor(industry_isic),
    
    employees    = clean_negative_codes(size_num),
    ln_employees = log(employees + 1),
    
    size_cat = factor(
      size,
      levels = c(1, 2, 3),
      labels = c("Small", "Medium", "Large")
    ),
    
    multi_establishment = case_when(
      clean_negative_codes(a7) == 1 ~ 1,
      clean_negative_codes(a7) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    manager_experience = clean_negative_codes(b7),
    manager_experience = ifelse(manager_experience > 80, NA, manager_experience),
    manager_experience = winsorise(manager_experience),
    
    female_manager = case_when(
      "b7a" %in% names(.) & clean_negative_codes(b7a) == 1 ~ 1,
      "b7a" %in% names(.) & clean_negative_codes(b7a) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    year_registered = clean_negative_codes(b6b),
    firm_age = ifelse(!is.na(year) & !is.na(year_registered),
                      year - year_registered, NA),
    firm_age = ifelse(firm_age < 0 | firm_age > 150, NA, firm_age),
    firm_age = winsorise(firm_age),
    
    ########################################################
    # Competition variables
    ########################################################
    
    competitors_cat = case_when(
      clean_negative_codes(e2) == 1 ~ "No competitors",
      clean_negative_codes(e2) == 2 ~ "One competitor",
      clean_negative_codes(e2) == 3 ~ "Two to five competitors",
      clean_negative_codes(e2) == 4 ~ "More than five competitors",
      TRUE ~ NA_character_
    ),
    
    competitors_cat = factor(
      competitors_cat,
      levels = c(
        "No competitors",
        "One competitor",
        "Two to five competitors",
        "More than five competitors"
      )
    ),
    
    competition_intensity = case_when(
      clean_negative_codes(e2) == 1 ~ 0,
      clean_negative_codes(e2) == 2 ~ 1,
      clean_negative_codes(e2) == 3 ~ 2,
      clean_negative_codes(e2) == 4 ~ 3,
      TRUE ~ NA_real_
    ),
    
    high_competition = case_when(
      competition_intensity >= 2 ~ 1,
      competition_intensity < 2  ~ 0,
      TRUE ~ NA_real_
    ),
    
    informal_competition = case_when(
      clean_negative_codes(e11) == 1 ~ 1,
      clean_negative_codes(e11) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    ########################################################
    # Market power
    ########################################################
    
    market_power = case_when(
      clean_negative_codes(e33) == 1 ~ 1,
      clean_negative_codes(e33) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    ########################################################
    # Pricing behaviour
    ########################################################
    
    price_increase = case_when(
      clean_negative_codes(e4) == 1 ~ 1,
      clean_negative_codes(e4) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    country_year = interaction(country_id, year, drop = TRUE)
  )

############################################################
# 6. Additional robustness variables, if available
############################################################

data <- data %>%
  mutate(
    competition_increased = case_when(
      "e32" %in% names(.) & clean_negative_codes(e32) == 1 ~ 1,
      "e32" %in% names(.) & clean_negative_codes(e32) %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    
    foreign_technology = case_when(
      "e6" %in% names(.) & clean_negative_codes(e6) == 1 ~ 1,
      "e6" %in% names(.) & clean_negative_codes(e6) == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

############################################################
# 7. Final analysis sample
############################################################

analysis <- data %>%
  filter(
    !is.na(market_power),
    !is.na(price_increase),
    !is.na(competition_intensity),
    !is.na(informal_competition),
    !is.na(ln_employees),
    !is.na(manager_experience),
    !is.na(firm_age),
    !is.na(size_cat),
    !is.na(industry_fe),
    !is.na(country_year),
    !is.na(weight),
    weight > 0
  )

write_csv(analysis, here("results", "clean_analysis_data.csv"))

############################################################
# 8. Descriptive statistics
############################################################

desc_continuous <- analysis %>%
  select(
    competition_intensity,
    market_power,
    price_increase,
    informal_competition,
    ln_employees,
    firm_age,
    manager_experience
  )

datasummary_skim(
  desc_continuous,
  output = here("results", "table_01_descriptive_statistics.docx")
)

############################################################
# 9. Distribution tables
############################################################

tab_competitors <- analysis %>%
  count(competitors_cat) %>%
  mutate(share = 100 * n / sum(n))

tab_market_power <- analysis %>%
  count(market_power) %>%
  mutate(
    market_power = ifelse(market_power == 1, "Yes", "No"),
    share = 100 * n / sum(n)
  )

tab_price <- analysis %>%
  count(price_increase) %>%
  mutate(
    price_increase = ifelse(price_increase == 1, "Price increased", "No price increase"),
    share = 100 * n / sum(n)
  )

tab_size <- analysis %>%
  count(size_cat) %>%
  mutate(share = 100 * n / sum(n))

doc <- read_docx()

doc <- body_add_par(doc, "Table A1. Distribution by number of competitors", style = "heading 1")
doc <- body_add_flextable(doc, autofit(flextable(tab_competitors)))

doc <- body_add_par(doc, "Table A2. Market power", style = "heading 1")
doc <- body_add_flextable(doc, autofit(flextable(tab_market_power)))

doc <- body_add_par(doc, "Table A3. Pricing behaviour", style = "heading 1")
doc <- body_add_flextable(doc, autofit(flextable(tab_price)))

doc <- body_add_par(doc, "Table A4. Firm size", style = "heading 1")
doc <- body_add_flextable(doc, autofit(flextable(tab_size)))

print(doc, target = here("results", "table_02_distribution_tables.docx"))

############################################################
# 10. Graphs
############################################################

fig_market_power <- analysis %>%
  group_by(competitors_cat) %>%
  summarise(
    mean_market_power = mean(market_power, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = competitors_cat, y = mean_market_power)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Number of competitors",
    y = "Share of firms with market power",
    title = "Market power by competitive environment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(
  here("results", "fig_01_market_power_by_competition.png"),
  fig_market_power,
  width = 7,
  height = 4,
  dpi = 300
)

fig_price_market_power <- analysis %>%
  group_by(market_power) %>%
  summarise(
    mean_price_increase = mean(price_increase, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(market_power = ifelse(market_power == 1, "Market power", "No market power")) %>%
  ggplot(aes(x = market_power, y = mean_price_increase)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "",
    y = "Share of firms increasing prices",
    title = "Pricing behaviour by market power"
  ) +
  theme_minimal()

ggsave(
  here("results", "fig_02_price_increase_by_market_power.png"),
  fig_price_market_power,
  width = 6,
  height = 4,
  dpi = 300
)

fig_price_competition <- analysis %>%
  group_by(competitors_cat) %>%
  summarise(
    mean_price_increase = mean(price_increase, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = competitors_cat, y = mean_price_increase)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Number of competitors",
    y = "Share of firms increasing prices",
    title = "Price increases by competitive environment"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(
  here("results", "fig_03_price_increase_by_competition.png"),
  fig_price_competition,
  width = 7,
  height = 4,
  dpi = 300
)

corr_data <- analysis %>%
  select(
    competition_intensity,
    informal_competition,
    market_power,
    price_increase,
    ln_employees,
    firm_age,
    manager_experience
  )

fig_corr <- ggcorr(corr_data, label = TRUE)

ggsave(
  here("results", "fig_04_correlation_matrix.png"),
  fig_corr,
  width = 7,
  height = 6,
  dpi = 300
)

############################################################
# 11. Econometric models
############################################################

############################################################
# 11.1 Competition and market power
############################################################

mp_lpm <- feols(
  market_power ~ competition_intensity + informal_competition +
    ln_employees + firm_age + manager_experience +
    i(size_cat) + multi_establishment |
    country_year + industry_fe,
  data = analysis,
  weights = ~ weight,
  cluster = ~ country_id
)

mp_probit <- glm(
  market_power ~ competition_intensity + informal_competition +
    ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment +
    country_year + industry_fe,
  data = analysis,
  weights = weight,
  family = binomial(link = "probit")
)

mp_logit <- glm(
  market_power ~ competition_intensity + informal_competition +
    ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment +
    country_year + industry_fe,
  data = analysis,
  weights = weight,
  family = binomial(link = "logit")
)

mp_probit_vcov <- vcovCL(mp_probit, cluster = analysis$country_id)
mp_logit_vcov  <- vcovCL(mp_logit, cluster = analysis$country_id)

modelsummary(
  list(
    "LPM" = mp_lpm,
    "Probit" = mp_probit,
    "Logit" = mp_logit
  ),
  vcov = list(
    NULL,
    mp_probit_vcov,
    mp_logit_vcov
  ),
  output = here("results", "table_03_competition_market_power_lpm_probit_logit.docx"),
  stars = TRUE,
  coef_omit = "country_year|industry_fe",
  gof_omit = "IC|Log|RMSE",
  notes = "LPM, Probit and Logit models. Survey weights applied. Probit and Logit standard errors clustered at country level. Country-year and industry fixed effects included but omitted from the table."
)

############################################################
# 11.2 Competition, market power and price increases
############################################################

price_lpm <- feols(
  price_increase ~ competition_intensity + informal_competition +
    market_power + ln_employees + firm_age + manager_experience +
    i(size_cat) + multi_establishment |
    country_year + industry_fe,
  data = analysis,
  weights = ~ weight,
  cluster = ~ country_id
)

price_probit <- glm(
  price_increase ~ competition_intensity + informal_competition +
    market_power + ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment +
    country_year + industry_fe,
  data = analysis,
  weights = weight,
  family = binomial(link = "probit")
)

price_logit <- glm(
  price_increase ~ competition_intensity + informal_competition +
    market_power + ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment +
    country_year + industry_fe,
  data = analysis,
  weights = weight,
  family = binomial(link = "logit")
)

price_probit_vcov <- vcovCL(price_probit, cluster = analysis$country_id)
price_logit_vcov  <- vcovCL(price_logit, cluster = analysis$country_id)

modelsummary(
  list(
    "LPM" = price_lpm,
    "Probit" = price_probit,
    "Logit" = price_logit
  ),
  vcov = list(
    NULL,
    price_probit_vcov,
    price_logit_vcov
  ),
  output = here("results", "table_04_price_behaviour_lpm_probit_logit.docx"),
  stars = TRUE,
  coef_omit = "country_year|industry_fe",
  gof_omit = "IC|Log|RMSE",
  notes = "LPM, Probit and Logit models. Survey weights applied. Probit and Logit standard errors clustered at country level. Country-year and industry fixed effects included but omitted from the table."
)

############################################################
# 12. Average marginal effects
############################################################

mp_probit_ame <- avg_slopes(
  mp_probit,
  vcov = mp_probit_vcov
)

mp_logit_ame <- avg_slopes(
  mp_logit,
  vcov = mp_logit_vcov
)

price_probit_ame <- avg_slopes(
  price_probit,
  vcov = price_probit_vcov
)

price_logit_ame <- avg_slopes(
  price_logit,
  vcov = price_logit_vcov
)

modelsummary(
  list(
    "Market power: Probit AME" = mp_probit_ame,
    "Market power: Logit AME" = mp_logit_ame,
    "Price increase: Probit AME" = price_probit_ame,
    "Price increase: Logit AME" = price_logit_ame
  ),
  output = here("results", "table_05_average_marginal_effects.docx"),
  stars = TRUE,
  notes = "Average marginal effects from Probit and Logit models."
)

############################################################
# 13. Robustness checks for Probit and Logit
############################################################

mp_probit_robust <- glm(
  market_power ~ competition_intensity + informal_competition +
    ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment +
    region_id + factor(year) + industry_fe,
  data = analysis,
  weights = weight,
  family = binomial(link = "probit")
)

mp_logit_robust <- glm(
  market_power ~ competition_intensity + informal_competition +
    ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment +
    region_id + factor(year) + industry_fe,
  data = analysis,
  weights = weight,
  family = binomial(link = "logit")
)

price_probit_robust <- glm(
  price_increase ~ competition_intensity + informal_competition +
    market_power + ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment +
    region_id + factor(year) + industry_fe,
  data = analysis,
  weights = weight,
  family = binomial(link = "probit")
)

price_logit_robust <- glm(
  price_increase ~ competition_intensity + informal_competition +
    market_power + ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment +
    region_id + factor(year) + industry_fe,
  data = analysis,
  weights = weight,
  family = binomial(link = "logit")
)

mp_probit_robust_vcov    <- vcovCL(mp_probit_robust, cluster = analysis$country_id)
mp_logit_robust_vcov     <- vcovCL(mp_logit_robust, cluster = analysis$country_id)
price_probit_robust_vcov <- vcovCL(price_probit_robust, cluster = analysis$country_id)
price_logit_robust_vcov  <- vcovCL(price_logit_robust, cluster = analysis$country_id)

modelsummary(
  list(
    "MP Probit robust" = mp_probit_robust,
    "MP Logit robust" = mp_logit_robust,
    "Price Probit robust" = price_probit_robust,
    "Price Logit robust" = price_logit_robust
  ),
  vcov = list(
    mp_probit_robust_vcov,
    mp_logit_robust_vcov,
    price_probit_robust_vcov,
    price_logit_robust_vcov
  ),
  output = here("results", "table_06_probit_logit_robustness.docx"),
  stars = TRUE,
  coef_omit = "region_id|factor\\(year\\)|industry_fe",
  gof_omit = "IC|Log|RMSE",
  notes = "Robustness specifications for Probit and Logit using region, year and industry fixed effects."
)

############################################################
# 14. Predicted probabilities
############################################################

pred_data <- analysis %>%
  group_by(competitors_cat) %>%
  summarise(
    competition_intensity = mean(competition_intensity, na.rm = TRUE),
    informal_competition = mean(informal_competition, na.rm = TRUE),
    market_power = mean(market_power, na.rm = TRUE),
    ln_employees = mean(ln_employees, na.rm = TRUE),
    firm_age = mean(firm_age, na.rm = TRUE),
    manager_experience = mean(manager_experience, na.rm = TRUE),
    size_cat = names(sort(table(analysis$size_cat), decreasing = TRUE))[1],
    multi_establishment = mean(multi_establishment, na.rm = TRUE),
    country_year = names(sort(table(analysis$country_year), decreasing = TRUE))[1],
    industry_fe = names(sort(table(analysis$industry_fe), decreasing = TRUE))[1],
    .groups = "drop"
  )

pred_data$size_cat <- factor(pred_data$size_cat, levels = levels(analysis$size_cat))
pred_data$country_year <- factor(pred_data$country_year, levels = levels(analysis$country_year))
pred_data$industry_fe <- factor(pred_data$industry_fe, levels = levels(analysis$industry_fe))

pred_data$pred_market_power_probit <- predict(
  mp_probit,
  newdata = pred_data,
  type = "response"
)

pred_data$pred_price_increase_probit <- predict(
  price_probit,
  newdata = pred_data,
  type = "response"
)

write_csv(
  pred_data,
  here("results", "predicted_probabilities_by_competition.csv")
)

fig_pred_mp <- ggplot(
  pred_data,
  aes(x = competitors_cat, y = pred_market_power_probit)
) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Number of competitors",
    y = "Predicted probability of market power",
    title = "Predicted market power by competition: Probit"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(
  here("results", "fig_05_predicted_market_power_probit.png"),
  fig_pred_mp,
  width = 7,
  height = 4,
  dpi = 300
)

fig_pred_price <- ggplot(
  pred_data,
  aes(x = competitors_cat, y = pred_price_increase_probit)
) +
  geom_col() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Number of competitors",
    y = "Predicted probability of price increase",
    title = "Predicted price increase by competition: Probit"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(
  here("results", "fig_06_predicted_price_increase_probit.png"),
  fig_pred_price,
  width = 7,
  height = 4,
  dpi = 300
)

############################################################
# 15. Diagnostics
############################################################

diagnostic_lm <- lm(
  price_increase ~ competition_intensity + informal_competition +
    market_power + ln_employees + firm_age + manager_experience +
    size_cat + multi_establishment,
  data = analysis
)

vif_results <- car::vif(diagnostic_lm)

capture.output(
  vif_results,
  file = here("results", "diagnostic_vif_results.txt")
)

analysis$residual_price_model <- resid(price_lpm)

fig_residuals <- ggplot(
  analysis,
  aes(x = fitted(price_lpm), y = residual_price_model)
) +
  geom_point(alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residual plot: pricing behaviour LPM"
  ) +
  theme_minimal()

ggsave(
  here("results", "fig_07_residual_plot_price_model.png"),
  fig_residuals,
  width = 7,
  height = 4,
  dpi = 300
)

############################################################
# 16. Export summary notes
############################################################

summary_text <- paste0(
  "Competition, Market Power, and Pricing Behaviour in Firms\n\n",
  "Number of observations in final analysis sample: ", nrow(analysis), "\n",
  "Number of countries/economies: ", n_distinct(analysis$country_id), "\n",
  "Number of country-year cells: ", n_distinct(analysis$country_year), "\n\n",
  "Models estimated:\n",
  "1. Linear Probability Model (LPM)\n",
  "2. Probit model\n",
  "3. Logit model\n\n",
  "Main outcomes:\n",
  "1. market_power: firm reports ability to increase prices more than competitors without losing customers.\n",
  "2. price_increase: firm reports price increase in the last fiscal year.\n\n",
  "Main competition measures:\n",
  "1. competition_intensity from e2.\n",
  "2. informal_competition from e11.\n\n",
  "Identification warning:\n",
  "Competition, market power and pricing behaviour may be endogenous. ",
  "More productive firms may choose different markets, survive under stronger competition, ",
  "or have unobserved quality that affects both market power and pricing. ",
  "Therefore, the results should be interpreted as conditional associations, not strict causal effects.\n"
)

writeLines(
  summary_text,
  here("results", "econometric_summary_notes.txt")
)

############################################################
# END OF SCRIPT
############################################################