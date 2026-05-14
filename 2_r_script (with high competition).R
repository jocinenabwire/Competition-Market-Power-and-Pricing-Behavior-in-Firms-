############################################################
# High Competition and Firm Performance
# Evidence from Enterprise Surveys
# Main outcome: log sales
# Results folder: results2
############################################################

rm(list = ls())

############################################################
# 1. Load packages
############################################################

# install.packages(c(
#   "tidyverse", "haven", "here", "janitor", "modelsummary",
#   "fixest", "sandwich", "lmtest", "GGally", "officer",
#   "flextable", "car", "scales"
# ))

library(tidyverse)
library(haven)
library(here)
library(janitor)
library(modelsummary)
library(fixest)
library(sandwich)
library(lmtest)
library(GGally)
library(officer)
library(flextable)
library(car)
library(scales)

############################################################
# 2. Create results folder and import data
############################################################

dir.create(here("results2"), showWarnings = FALSE)

raw_data <- read_dta(
  here("data_raw", "New_Comprehensive_April_01_2026.dta")
) %>%
  clean_names()

############################################################
# 3. Helper functions
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
# 4. Construct variables
############################################################

data <- raw_data %>%
  mutate(
    ########################################################
    # Identifiers
    ########################################################
    
    firm_id    = idstd,
    country_id = as.factor(country),
    region_id  = as.factor(region),
    year       = clean_negative_codes(a14y),
    weight     = wt,
    country_year = interaction(country_id, year, drop = TRUE),
    
    ########################################################
    # Sector controls
    ########################################################
    
    sector_main   = as.factor(sector_ms),
    sector_strata = as.factor(stra_sector),
    industry_isic = clean_negative_codes(isic_v4),
    industry_fe   = as.factor(industry_isic),
    
    ########################################################
    # Firm scale and structure
    ########################################################
    
    employees = clean_negative_codes(size_num),
    employees = ifelse(employees <= 0, NA, employees),
    ln_employees = log(employees),
    
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
    
    ########################################################
    # Firm age and management
    ########################################################
    
    year_registered = clean_negative_codes(b6b),
    
    firm_age = ifelse(
      !is.na(year) & !is.na(year_registered),
      year - year_registered,
      NA
    ),
    firm_age = ifelse(firm_age < 0 | firm_age > 150, NA, firm_age),
    firm_age = winsorise(firm_age),
    
    manager_experience = clean_negative_codes(b7),
    manager_experience = ifelse(manager_experience > 80, NA, manager_experience),
    manager_experience = winsorise(manager_experience),
    
    ########################################################
    # Main outcome: sales
    ########################################################
    
    sales_total = clean_negative_codes(d2),
    sales_total = ifelse(sales_total <= 0, NA, sales_total),
    sales_total = winsorise(sales_total),
    ln_sales = log(sales_total),
    
    sales_per_worker = sales_total / employees,
    sales_per_worker = ifelse(sales_per_worker <= 0, NA, sales_per_worker),
    sales_per_worker = winsorise(sales_per_worker),
    ln_sales_per_worker = log(sales_per_worker),
    
    ########################################################
    # Alternative outcomes
    ########################################################
    
    export_share = clean_negative_codes(d3c),
    export_share = ifelse(export_share < 0 | export_share > 100, NA, export_share),
    
    exporter = case_when(
      !is.na(export_share) & export_share > 0 ~ 1,
      !is.na(export_share) & export_share == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    product_innovation = case_when(
      clean_negative_codes(h1) == 1 ~ 1,
      clean_negative_codes(h1) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    process_innovation = case_when(
      clean_negative_codes(h5) == 1 ~ 1,
      clean_negative_codes(h5) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    any_innovation = case_when(
      product_innovation == 1 | process_innovation == 1 ~ 1,
      product_innovation == 0 & process_innovation == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    capacity_utilization = clean_negative_codes(f1),
    capacity_utilization = ifelse(
      capacity_utilization < 0 | capacity_utilization > 100,
      NA,
      capacity_utilization
    ),
    
    price_increase = case_when(
      clean_negative_codes(e4) == 1 ~ 1,
      clean_negative_codes(e4) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
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
    
    high_competition = case_when(
      clean_negative_codes(e2) == 4 ~ 1,
      clean_negative_codes(e2) %in% c(1, 2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    
    high_competition_label = case_when(
      high_competition == 1 ~ "High competition: more than 5 competitors",
      high_competition == 0 ~ "Low competition: 0 to 5 competitors",
      TRUE ~ NA_character_
    ),
    
    high_competition_label = factor(
      high_competition_label,
      levels = c(
        "Low competition: 0 to 5 competitors",
        "High competition: more than 5 competitors"
      )
    ),
    
    competition_intensity = case_when(
      clean_negative_codes(e2) == 1 ~ 0,
      clean_negative_codes(e2) == 2 ~ 1,
      clean_negative_codes(e2) == 3 ~ 2,
      clean_negative_codes(e2) == 4 ~ 3,
      TRUE ~ NA_real_
    ),
    
    informal_competition = case_when(
      clean_negative_codes(e11) == 1 ~ 1,
      clean_negative_codes(e11) == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    informal_obstacle = clean_negative_codes(e30),
    informal_obstacle = ifelse(informal_obstacle > 4, NA, informal_obstacle)
  )

############################################################
# 5. Analysis samples
############################################################

analysis_sales <- data %>%
  filter(
    !is.na(ln_sales),
    !is.na(high_competition),
    !is.na(ln_employees),
    !is.na(country_id),
    !is.na(sector_strata),
    !is.na(weight),
    weight > 0
  )

analysis_sales_full <- data %>%
  filter(
    !is.na(ln_sales),
    !is.na(high_competition),
    !is.na(ln_employees),
    !is.na(multi_establishment),
    !is.na(firm_age),
    !is.na(manager_experience),
    !is.na(country_id),
    !is.na(sector_strata),
    !is.na(weight),
    weight > 0
  )

analysis_productivity <- data %>%
  filter(
    !is.na(ln_sales_per_worker),
    !is.na(high_competition),
    !is.na(ln_employees),
    !is.na(country_id),
    !is.na(sector_strata),
    !is.na(weight),
    weight > 0
  )

analysis_export <- data %>%
  filter(
    !is.na(exporter),
    !is.na(high_competition),
    !is.na(ln_employees),
    !is.na(country_id),
    !is.na(sector_strata),
    !is.na(weight),
    weight > 0
  )

analysis_innovation <- data %>%
  filter(
    !is.na(any_innovation),
    !is.na(high_competition),
    !is.na(ln_employees),
    !is.na(country_id),
    !is.na(sector_strata),
    !is.na(weight),
    weight > 0
  )

analysis_capacity <- data %>%
  filter(
    !is.na(capacity_utilization),
    !is.na(high_competition),
    !is.na(ln_employees),
    !is.na(country_id),
    !is.na(sector_strata),
    !is.na(weight),
    weight > 0
  )

analysis_price <- data %>%
  filter(
    !is.na(price_increase),
    !is.na(high_competition),
    !is.na(ln_employees),
    !is.na(country_id),
    !is.na(sector_strata),
    !is.na(weight),
    weight > 0
  )

analysis_components <- data %>%
  filter(
    !is.na(ln_sales),
    !is.na(competition_intensity),
    !is.na(informal_competition),
    !is.na(informal_obstacle),
    !is.na(ln_employees),
    !is.na(multi_establishment),
    !is.na(firm_age),
    !is.na(manager_experience),
    !is.na(country_id),
    !is.na(sector_strata),
    !is.na(weight),
    weight > 0
  )

cat("Sales sample:", nrow(analysis_sales), "\n")
cat("Full-control sales sample:", nrow(analysis_sales_full), "\n")
cat("Productivity sample:", nrow(analysis_productivity), "\n")
cat("Export sample:", nrow(analysis_export), "\n")
cat("Innovation sample:", nrow(analysis_innovation), "\n")
cat("Capacity utilization sample:", nrow(analysis_capacity), "\n")
cat("Price increase sample:", nrow(analysis_price), "\n")
cat("Component robustness sample:", nrow(analysis_components), "\n")

write_csv(analysis_sales, here("results2", "clean_analysis_sales_high_competition.csv"))
write_csv(analysis_sales_full, here("results2", "clean_analysis_sales_full_high_competition.csv"))

############################################################
# 6. Descriptive statistics
############################################################

desc_continuous <- analysis_sales %>%
  select(
    ln_sales,
    ln_employees,
    high_competition,
    competition_intensity,
    informal_competition,
    informal_obstacle,
    firm_age,
    manager_experience
  )

datasummary_skim(
  desc_continuous,
  output = here("results2", "table_01_descriptive_statistics.docx")
)

############################################################
# 7. Distribution tables
############################################################

tab_high_comp <- analysis_sales %>%
  count(high_competition_label) %>%
  mutate(share = 100 * n / sum(n))

tab_competitors <- analysis_sales %>%
  count(competitors_cat) %>%
  mutate(share = 100 * n / sum(n))

tab_informal <- analysis_sales %>%
  count(informal_competition) %>%
  mutate(
    informal_competition = ifelse(
      informal_competition == 1,
      "Competes with informal firms",
      "Does not compete with informal firms"
    ),
    share = 100 * n / sum(n)
  )

tab_size <- analysis_sales %>%
  count(size_cat) %>%
  mutate(share = 100 * n / sum(n))

doc <- read_docx()

doc <- body_add_par(doc, "Table A1. High competition status", style = "heading 1")
doc <- body_add_flextable(doc, autofit(flextable(tab_high_comp)))

doc <- body_add_par(doc, "Table A2. Distribution by number of competitors", style = "heading 1")
doc <- body_add_flextable(doc, autofit(flextable(tab_competitors)))

doc <- body_add_par(doc, "Table A3. Informal competition", style = "heading 1")
doc <- body_add_flextable(doc, autofit(flextable(tab_informal)))

doc <- body_add_par(doc, "Table A4. Firm size", style = "heading 1")
doc <- body_add_flextable(doc, autofit(flextable(tab_size)))

print(doc, target = here("results2", "table_02_distribution_tables.docx"))

############################################################
# 8. Graphs
############################################################

fig_sales_high_comp <- analysis_sales %>%
  group_by(high_competition_label) %>%
  summarise(
    mean_ln_sales = mean(ln_sales, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = high_competition_label, y = mean_ln_sales)) +
  geom_col() +
  labs(
    x = "",
    y = "Average log sales",
    title = "Average firm sales by competition intensity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(
  here("results2", "fig_01_sales_by_high_competition.png"),
  fig_sales_high_comp,
  width = 7,
  height = 4,
  dpi = 300
)

fig_box_sales_high_comp <- ggplot(
  analysis_sales,
  aes(x = high_competition_label, y = ln_sales)
) +
  geom_boxplot() +
  labs(
    x = "",
    y = "Log sales",
    title = "Distribution of firm sales by competition intensity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(
  here("results2", "fig_02_box_sales_by_high_competition.png"),
  fig_box_sales_high_comp,
  width = 7,
  height = 4,
  dpi = 300
)

fig_productivity_high_comp <- analysis_productivity %>%
  group_by(high_competition_label) %>%
  summarise(
    mean_ln_sales_per_worker = mean(ln_sales_per_worker, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = high_competition_label, y = mean_ln_sales_per_worker)) +
  geom_col() +
  labs(
    x = "",
    y = "Average log sales per worker",
    title = "Average productivity by competition intensity"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(
  here("results2", "fig_03_productivity_by_high_competition.png"),
  fig_productivity_high_comp,
  width = 7,
  height = 4,
  dpi = 300
)

############################################################
# 9. Main sales models
############################################################

sales_main_1 <- feols(
  ln_sales ~ high_competition +
    i(sector_strata) |
    country_id,
  data = analysis_sales,
  weights = ~ weight,
  cluster = ~ country_id
)

sales_main_2 <- feols(
  ln_sales ~ high_competition +
    ln_employees +
    i(sector_strata) |
    country_id,
  data = analysis_sales,
  weights = ~ weight,
  cluster = ~ country_id
)

sales_main_3 <- feols(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id,
  data = analysis_sales_full,
  weights = ~ weight,
  cluster = ~ country_id
)

sales_main_4 <- feols(
  ln_sales ~ high_competition + informal_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id,
  data = analysis_sales_full %>% filter(!is.na(informal_competition)),
  weights = ~ weight,
  cluster = ~ country_id
)

modelsummary(
  list(
    "Baseline" = sales_main_1,
    "Employment control" = sales_main_2,
    "Full controls" = sales_main_3,
    "Add informal competition" = sales_main_4
  ),
  output = here("results2", "table_03_main_sales_high_competition.docx"),
  stars = TRUE,
  coef_omit = "sector_strata|country_id",
  gof_omit = "IC|Log|RMSE",
  notes = "Outcome: log sales. High competition equals one if the firm reports more than five competitors and zero otherwise. Firm size categories are excluded; log employment controls for firm scale. Main models use country fixed effects and sector controls. Survey weights applied. Standard errors clustered at country level."
)

############################################################
# 10. Alternative outcomes
############################################################

productivity_model <- feols(
  ln_sales_per_worker ~ high_competition +
    ln_employees + multi_establishment +
    i(sector_strata) |
    country_id,
  data = analysis_productivity,
  weights = ~ weight,
  cluster = ~ country_id
)

capacity_model <- feols(
  capacity_utilization ~ high_competition +
    ln_employees + multi_establishment +
    i(sector_strata) |
    country_id,
  data = analysis_capacity,
  weights = ~ weight,
  cluster = ~ country_id
)

export_lpm <- feols(
  exporter ~ high_competition +
    ln_employees + multi_establishment +
    i(sector_strata) |
    country_id,
  data = analysis_export,
  weights = ~ weight,
  cluster = ~ country_id
)

innovation_lpm <- feols(
  any_innovation ~ high_competition +
    ln_employees + multi_establishment +
    i(sector_strata) |
    country_id,
  data = analysis_innovation,
  weights = ~ weight,
  cluster = ~ country_id
)

price_lpm <- feols(
  price_increase ~ high_competition +
    ln_employees + multi_establishment +
    i(sector_strata) |
    country_id,
  data = analysis_price,
  weights = ~ weight,
  cluster = ~ country_id
)

modelsummary(
  list(
    "Productivity" = productivity_model,
    "Capacity utilization" = capacity_model,
    "Export participation" = export_lpm,
    "Innovation" = innovation_lpm,
    "Price increase" = price_lpm
  ),
  output = here("results2", "table_04_alternative_outcomes_high_competition.docx"),
  stars = TRUE,
  coef_omit = "sector_strata|country_id",
  gof_omit = "IC|Log|RMSE",
  notes = "Alternative outcomes. Export participation, innovation and price increase are estimated as Linear Probability Models. Firm size categories are excluded; log employment controls for scale."
)

############################################################
# 11. Robustness checks
############################################################

sales_components <- feols(
  ln_sales ~ competition_intensity + informal_competition + informal_obstacle +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id,
  data = analysis_components,
  weights = ~ weight,
  cluster = ~ country_id
)

sales_country_year <- feols(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id^year,
  data = analysis_sales_full,
  weights = ~ weight,
  cluster = ~ country_id
)

sales_strict_fe <- feols(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience |
    country_id^year + industry_fe,
  data = analysis_sales_full,
  weights = ~ weight,
  cluster = ~ country_id
)

sales_unweighted <- feols(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id,
  data = analysis_sales_full,
  cluster = ~ country_id
)

modelsummary(
  list(
    "Components" = sales_components,
    "Country-year FE" = sales_country_year,
    "Strict FE" = sales_strict_fe,
    "Unweighted" = sales_unweighted
  ),
  output = here("results2", "table_05_robustness_checks_high_competition.docx"),
  stars = TRUE,
  coef_omit = "sector_strata|country_id|year|industry_fe",
  gof_omit = "IC|Log|RMSE",
  notes = "Robustness checks. Components model uses the original competition variables. Strict FE includes country-year and industry fixed effects."
)

############################################################
# 12. Heterogeneity analysis
############################################################

sales_manufacturing <- feols(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id,
  data = analysis_sales_full %>% filter(sector_main == "Manufacturing"),
  weights = ~ weight,
  cluster = ~ country_id
)

sales_services <- feols(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id,
  data = analysis_sales_full %>% filter(sector_main == "Services"),
  weights = ~ weight,
  cluster = ~ country_id
)

sales_small <- feols(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id,
  data = analysis_sales_full %>% filter(size_cat == "Small"),
  weights = ~ weight,
  cluster = ~ country_id
)

sales_medium_large <- feols(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience +
    i(sector_strata) |
    country_id,
  data = analysis_sales_full %>% filter(size_cat != "Small"),
  weights = ~ weight,
  cluster = ~ country_id
)

modelsummary(
  list(
    "Manufacturing" = sales_manufacturing,
    "Services" = sales_services,
    "Small firms" = sales_small,
    "Medium/Large firms" = sales_medium_large
  ),
  output = here("results2", "table_06_heterogeneity_results_high_competition.docx"),
  stars = TRUE,
  coef_omit = "sector_strata|country_id",
  gof_omit = "IC|Log|RMSE",
  notes = "Heterogeneity analysis by sector and firm size. Outcome: log sales."
)

############################################################
# 13. Diagnostics
############################################################

diagnostic_lm <- lm(
  ln_sales ~ high_competition +
    ln_employees + multi_establishment +
    firm_age + manager_experience + sector_strata,
  data = analysis_sales_full
)

vif_results <- car::vif(diagnostic_lm)

capture.output(
  vif_results,
  file = here("results2", "diagnostic_vif_results.txt")
)

analysis_sales_full$residual_sales_model <- resid(sales_main_3)

fig_residuals <- ggplot(
  analysis_sales_full,
  aes(x = fitted(sales_main_3), y = residual_sales_model)
) +
  geom_point(alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residual plot: log sales model"
  ) +
  theme_minimal()

ggsave(
  here("results2", "fig_04_residual_plot_sales_model.png"),
  fig_residuals,
  width = 7,
  height = 4,
  dpi = 300
)

############################################################
# 14. Export summary notes
############################################################

summary_text <- paste0(
  "High Competition and Firm Performance\n\n",
  "Main idea:\n",
  "The main competition variable is a binary indicator equal to one if the firm reports more than five competitors, ",
  "and zero if the firm reports zero to five competitors.\n\n",
  "Sample sizes:\n",
  "Sales sample: ", nrow(analysis_sales), "\n",
  "Full-control sales sample: ", nrow(analysis_sales_full), "\n",
  "Productivity sample: ", nrow(analysis_productivity), "\n",
  "Export participation sample: ", nrow(analysis_export), "\n",
  "Innovation sample: ", nrow(analysis_innovation), "\n",
  "Capacity utilization sample: ", nrow(analysis_capacity), "\n",
  "Price increase sample: ", nrow(analysis_price), "\n\n",
  "Empirical strategy:\n",
  "The main outcome is log sales. The main specification uses country fixed effects and sector controls, ",
  "because competition is partly sectoral. More demanding fixed effects are treated as robustness checks. ",
  "Firm size categories are excluded from regressions because log employment controls for firm scale.\n\n",
  "Identification warning:\n",
  "Competition is not randomly assigned. More productive firms may select into more competitive markets, ",
  "and unobserved product quality may affect both competition exposure and firm performance. ",
  "Therefore, results should be interpreted as conditional associations rather than strict causal effects.\n"
)

writeLines(
  summary_text,
  here("results2", "econometric_summary_notes.txt")
)

############################################################
# END OF SCRIPT
############################################################