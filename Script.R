
## Economic inequality and authoritarian support
## Project description: To investigate the relationship between economic inequality and aggregate support for authoritarian leadership and how economic inequality mediates the relationship between relative income status and authoritarian support in democracies
## Data sources: World Values Survey, World Development Indicators, Standardized World Income Inequality Database, World Inequality Database, Varieties of Democracy (updated as of 2025)


## 1. Load packages
library(readr)
library(tidyverse)
library(countrycode)
library(haven)
library(fuzzyjoin)
library(zoo)
library(ggplot2)
library(patchwork)
library(lme4)
library(fixest)
library(modelsummary)
library(gt)
library(ggeffects)
library(effects)


## 2. Load and clean data (WVS, SWIID, WID, WDI, and V-Dem)

# Load WVS data
df_raw <- read.csv("Data/WVS_Time_Series_1981-2022_csv_v5_0.csv")

# Data cleaning
df <- df_raw %>%
  dplyr::select(COUNTRY_ALPHA,S020,S002VS,E114,A165,E033,E069_11,F034,X001,X003R,X025R,X028,X047_WVS)

# Renaming variables
df <- df %>%
  rename(
    iso3c = COUNTRY_ALPHA, # country
    year = S020, # survey year
    wave = S002VS, # survey wave
    income = X047_WVS, # 1 = bottom income, 10 = top income
    auth_leader = E114, # 1 = very good, 4 = very bad
    gender = X001, # 1 = male, 2 = female
    age = X003R, # 1 = youngest age cohort, 6 = oldest age cohort
    education = X025R, # 1 = primary education, 2 = secondary education, 3 = university education
    religiosity = F034, # 1 = religious, 2 = not religious, 3 = atheist
    employment = X028, # 1 = full time, 2 = part time, 3 = self-employed, 4 = retired, 5 = stay at home, 6 = student, 7 = unemployed, 8 = other
    political_ideology = E033, # 1 = left, 10 = right
    social_trust = A165, # 1 = most people can be trusted, 2 = need to be very careful
    government_confidence = E069_11, # 1 = a great deal, 4 = none at all
  )

# Exclude first two waves of WVS as the leader question is not asked
df <- df %>%
  filter(year > 1994)

# Clean out missing values
df <- df %>%
  mutate(
    income = ifelse(income < 0, NA, income),
    auth_leader = ifelse(auth_leader < 0, NA, auth_leader),
    gender = ifelse(gender < 0, NA, gender),
    age = ifelse(age < 0, NA, age),
    education = ifelse(education < 0, NA, education),
    religiosity = ifelse(religiosity < 0, NA, religiosity),
    employment = ifelse(employment < 0, NA, employment),
    political_ideology = ifelse(political_ideology < 0, NA, political_ideology),
    social_trust = ifelse(social_trust < 0, NA, social_trust),
    government_confidence = ifelse(government_confidence < 0, NA, government_confidence),
    political_importance = ifelse(political_importance < 0, NA, political_importance),
    community_security = ifelse(community_security < 0, NA, community_security),
    redistribution_attitude = ifelse(redistribution_attitude < 0, NA, redistribution_attitude),
  )

# Data check
summary(df$income)
summary(df$auth_leader)
summary(df$gender)
summary(df$age)
summary(df$education)
summary(df$religiosity)
summary(df$employment)
summary(df$political_ideology)
summary(df$social_trust)
summary(df$government_confidence)

# Convert response to strong leader question as binary variable (1 = support for authoritarian leader)
df <- df %>%
  mutate(
    auth_leader_binary = case_when(
      auth_leader %in% c(1,2) ~ 1,
      auth_leader %in% c(3,4) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Convert income scale (1-10) into three income categories (B40, M30, T30)
df <- df %>%
  mutate(
    income_t = case_when(
      income %in% c(1,2,3,4) ~ 1,
      income %in% c(5,6,7) ~ 2,
      income %in% c(8,9,10) ~ 3
    )
  )

df <- df %>%
  mutate(income_t = factor(income_t))


# Convert demographic control variables (reverse code for ease of interpretation)
df <- df %>%
  mutate(
    gender = case_when(
      gender %in% c(1) ~ 0,
      gender %in% c(2) ~ 1,
      TRUE ~ NA_real_
    )
  )

df <- df %>%
  mutate(
    religiosity = case_when(
      religiosity %in% c(1) ~ 3,
      religiosity %in% c(2) ~ 2,
      religiosity %in% c(3) ~ 1,
      TRUE ~ NA_real_
    )
  )

df <- df %>%
  mutate(
    unemployed = case_when(
      employment %in% c(7) ~ 1,
      employment %in% c(1,2,3,4,5,6,8) ~ 0,
      TRUE ~ NA_real_
    )
  )

df <- df %>%
  mutate(
    social_trust = case_when(
      social_trust %in% c(1) ~ 1,
      social_trust %in% c(2) ~ 0,
      TRUE ~ NA_real_
    )
  )

df <- df %>%
  mutate(
    government_confidence = case_when(
      government_confidence %in% c(4) ~ 1,
      government_confidence %in% c(3) ~ 2,
      government_confidence %in% c(2) ~ 3,
      government_confidence %in% c(1) ~ 4,
      TRUE ~ NA_real_
    )
  )


# Load World Development Indicators (WDI) data for macroeconomic control variables
library(WDI)

df_wdi <- WDI(
  country = unique(df$iso3c),
  indicator = c("NY.GDP.PCAP.PP.KD", "NY.GDP.PCAP.KD.ZG","NY.GDP.MKTP.KD.ZG","SI.POV.GINI"),
  start = min(df$year),
  end = max(df$year)
)

df_wdi <- df_wdi %>%
  rename(
    gdp_per_capita = NY.GDP.PCAP.PP.KD,
    gdp_growth = NY.GDP.MKTP.KD.ZG,
    gini_coefficient_wdi = SI.POV.GINI
  )


# Load SWIID data for economic inequality (post-tax Gini coefficient)
load("Data/swiid_inequality.rda")

df_swiid <- swiid_summary %>%
  dplyr::select(country, year, gini_disp)

df_swiid$iso3c <- countrycode(df_swiid$country, origin = "country.name", destination = "iso3c")

df_swiid <- df_swiid %>%
  mutate(gini_coefficient_swiid_disp = gini_disp/100)


# 2.4 Load WID data for economic inequality (post-tax Gini coefficient, retrieved with STATA WID command and imported into R)
df_wid <- read_dta("Data/wid_inequality.dta")

df_wid <- df_wid %>%
  filter(age == 992) %>%
  filter(pop == "j")

df_wid <- df_wid %>%
  dplyr::select(country, year, value) %>%
  rename(
    gini_coefficient_wid_post = value
  )

df_wid$iso3c <- countrycode(df_wid$country, origin = "iso2c", destination = "iso3c")


# Varieties of Democracy data
df_regime <- read.csv("Data/regime_score.csv")

df_regime <- df_regime %>%
  rename(
    country = Entity,
    year = Year,
    iso3c = Code,
    regime_score = Political.regime
  )


## 3. Merge data

# Merge WDI, WID, SWIID, and V-Dem data
df_macro <- df_wdi %>%
  left_join(df_wid, by = c("iso3c","year"))

df_macro <- df_macro %>%
  left_join(df_swiid, by = c("iso3c","year"))

df_macro_regime <- df_macro %>%
  left_join(df_regime, by = c("iso3c","year"))

df_macro_regime <- df_macro_regime %>%
  dplyr::select(iso3c, year, gdp_per_capita, gdp_growth, gini_coefficient_swiid_disp, gini_coefficient_wid_post, gini_coefficient_wdi, regime_score)


# Filter for consolidated democracies that consistently have a V-Dem regime score greater than 1 since the start of WVS Wave 3 (closed autocracy = 0, electoral autocracy = 1, electoral democracy = 2, liberal democracy = 3)
df_macro_regime <- df_macro_regime %>%
  filter(year > 1994) %>%
  group_by(iso3c) %>%
  mutate(consolidated = all(regime_score > 1)) %>%
  ungroup()


# Merge with WVS data
df_merge <- df %>%
  left_join(df_macro_regime, by = c("iso3c","year"))


# Scale macroeconomic variables
df_merge <- df_merge %>%
  mutate(country_wave = interaction(iso3c, wave))

df_merge <- df_merge %>%
  mutate(
    log_gdp_per_capita = log(gdp_per_capita),
    log_gdp_per_capita_c = scale(log_gdp_per_capita)[,1],
    gdp_growth_c = scale(gdp_growth)[,1],
    gini_coefficient_swiid_disp_c = scale(gini_coefficient_swiid_disp)[,1],
    gini_coefficient_wid_post_c = scale(gini_coefficient_wid_post)[,1],
    gini_coefficient_wdi_c = scale(gini_coefficient_wdi)[,1]
    )

# Create the continuous income variable
df_merge <- df_merge %>%
  group_by(country_wave) %>%
  mutate(
    income_mean = mean(income, na.rm = TRUE),
    income_c = income - income_mean
  ) %>%
  ungroup()


# Filter for democracies (every country-wave with a V-Dem regime score greater than 1)
df_democracy <- df_merge %>%
  filter(regime_score > 1)


# Renaming
df_democracy <- df_democracy %>%
  rename(country = iso3c)


## 4. Descriptive statistics

# Scatterplot of economic inequality and authoritarian support with country-wave observation units
df_main <- df_democracy %>%
  group_by(country_wave) %>%
  summarise(mean_support = mean(auth_leader_binary, na.rm = TRUE),
            mean_swiid = mean(gini_coefficient_swiid_disp, na.rm = TRUE),
            mean_wid = mean(gini_coefficient_wid_post, na.rm = TRUE))

ggplot(df_main, aes(x = mean_swiid, y = mean_support)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal()

ggplot(df_main, aes(x = mean_wid, y = mean_support)) +
  geom_point(alpha = 0.6, size = 2) +
  theme_minimal()


# Bar plot of income status and authoritarian support with pooled data from low-inequality democracies and high-inequality democracies
df_low <- df_democracy %>%
  filter(gini_coefficient_wid_post < mean(gini_coefficient_wid_post, na.rm = TRUE)) %>%
  mutate(income = factor(income))

df_high <- df_democracy %>%
  filter(gini_coefficient_wid_post > mean(gini_coefficient_wid_post, na.rm = TRUE)) %>%
  mutate(income = factor(income))

df_low <- df_low %>%
  group_by(income) %>%
  summarise(mean_support = mean(auth_leader_binary, na.rm = TRUE)) %>%
  filter(!is.na(income))

df_high <- df_high %>%
  group_by(income) %>%
  summarise(mean_support = mean(auth_leader_binary, na.rm = TRUE)) %>%
  filter(!is.na(income))

graph_1 <- ggplot(data = df_low, aes(x = income, y = mean_support, fill = mean_support)) +
  geom_col() +
  scale_fill_gradient(low = "grey70", high = "grey50") +
  ylim(0, 0.55) +
  labs(x = "Income decile",
       y = "Percentage of authoritarian support",
       title = "Low-inequality democracies (pooled)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "none")

graph_2 <- ggplot(data = df_high, aes(x = income, y = mean_support, fill = mean_support)) +
  geom_col() +
  scale_fill_gradient(low = "grey70", high = "grey50") +
  ylim(0, 0.55) +
  labs(x = "Income decile",
       y = "Percentage of authoritarian support",
       title = "High-inequality democracies (pooled)") +
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        plot.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.position = "none")

(graph_1 + graph_2) +
  plot_annotation(title = "Figure 1. Income Status and Authoritarian Support",
                  theme = theme(plot.title = element_text(family = "serif")))


## 5. Multilevel logistic regression

# Main models

# Main effect of economic inequality on authoritarian support (random-intercept model with country effects and country-wave effects)
model_1 <- glmer(auth_leader_binary ~ income_t + gini_coefficient_swiid_disp_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1|country), family = binomial, data = df_democracy)

summary(model_1)

model_2 <- glmer(auth_leader_binary ~ income_t + gini_coefficient_wid_post_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1|country), family = binomial, data = df_democracy)

summary(model_2)


# Interaction effect of economic inequality and income status on authoritarian support (random-slope model with country effects and country-wave effects)
model_3 <- glmer(auth_leader_binary ~ income_t * gini_coefficient_swiid_disp_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_t|country), family = binomial, data = df_democracy)

summary(model_3)


model_4 <- glmer(auth_leader_binary ~ income_t * gini_coefficient_wid_post_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_t|country), family = binomial, data = df_democracy)

summary(model_4)


# Robustness check models

# Consolidated democracies
df_consolidated <- df_merge %>%
  filter(consolidated == TRUE)

df_consolidated <- df_consolidated %>%
  rename(country = iso3c)

model_5 <- glmer(auth_leader_binary ~ income_t + gini_coefficient_swiid_disp_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_t|country), family = binomial, data = df_consolidated)

summary(model_5)


model_6 <- glmer(auth_leader_binary ~ income_t + gini_coefficient_wid_post_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_t|country), family = binomial, data = df_consolidated)

summary(model_6)


model_7 <- glmer(auth_leader_binary ~ income_t * gini_coefficient_swiid_disp_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_t|country), family = binomial, data = df_consolidated)

summary(model_7)


model_8 <- glmer(auth_leader_binary ~ income_t * gini_coefficient_wid_post_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_t|country), family = binomial, data = df_consolidated)

summary(model_8)


# Income as continuous variable
model_9 <- glmer(auth_leader_binary ~ income_c * gini_coefficient_swiid_disp_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_c|country), family = binomial, data = df_democracy)

summary(model_9)


model_10 <- glmer(auth_leader_binary ~ income_c * gini_coefficient_wid_post_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_c|country), family = binomial, data = df_democracy)

summary(model_10)


# Income as continuous variable and additional control variables (political ideology, social trust, government confidence)
model_11 <- glmer(auth_leader_binary ~ income_c * gini_coefficient_swiid_disp_c + log_gdp_per_capita_c + gdp_growth_c + social_trust + government_confidence + political_ideology + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_c|country), family = binomial, data = df_democracy)

summary(model_11)


model_12 <- glmer(auth_leader_binary ~ income_c * gini_coefficient_wid_post_c + log_gdp_per_capita_c + gdp_growth_c + social_trust + government_confidence + political_ideology + gender + age + education + unemployed + religiosity + (1|country:wave) + (1 + income_c|country), family = binomial, data = df_democracy)

summary(model_12)


# FE models
model_13 <- feglm(auth_leader_binary ~ income_c + gini_coefficient_swiid_disp_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity | country + wave, family = binomial, data = df_democracy)

summary(model_13)


model_14 <- feglm(auth_leader_binary ~ income_c + gini_coefficient_wid_post_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity | country + wave, family = binomial, data = df_democracy)

summary(model_14)


model_15 <- feglm(auth_leader_binary ~ income_c * gini_coefficient_swiid_disp_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity | country + wave, family = binomial, data = df_democracy)

summary(model_15)


model_16 <- feglm(auth_leader_binary ~ income_c * gini_coefficient_wid_post_c + log_gdp_per_capita_c + gdp_growth_c + gender + age + education + unemployed + religiosity | country + wave, family = binomial, data = df_democracy)

summary(model_16)


## 6. Summary tables

# Countries in the main models

# Survey waves in the regression
df_democracy_waves <- model.frame(model_2)

df_democracy_waves <- df_democracy_waves %>%
  group_by(country) %>%
  summarise(waves = paste(sort(unique(wave)), collapse = ","))

# Survey waves in the WVS
df_waves <- df_merge %>%
  group_by(iso3c) %>%
  summarise(all_waves = paste(sort(unique(wave)), collapse = ","),
            min_regime_score = min(regime_score)) %>%
  rename(country = iso3c)

# Merge data
df_democracy_waves <- df_democracy_waves %>%
  left_join(df_waves, by = "country")

df_democracy_waves$country <- countrycode(df_democracy_waves$country, origin = "iso3c", destination = "country.name")

df_democracy_waves <- df_democracy_waves %>%
  mutate(waves_converted = str_split(waves, pattern = ",")) %>%
  mutate(all_waves_converted = str_split(all_waves, pattern = ","))

# Add explanation for excluded waves (regime transition or missing variables)
df_democracy_waves <- df_democracy_waves %>%
  mutate(waves_difference = map2_lgl(waves_converted, all_waves_converted, ~! setequal(.x, .y)))

df_democracy_waves <- df_democracy_waves %>%
  mutate(
    waves_excluded = case_when(
      waves_difference == FALSE ~ "",
      min_regime_score < 2 ~ "Regime change",
      TRUE ~ "Missing variables"
    )
  )

df_democracy_waves <- df_democracy_waves %>%
  select(country, waves, all_waves, waves_excluded)


# Countries in the consolidated democracies models

# Survey waves in the regression
df_consolidated_waves <- model.frame(model_6)

df_consolidated_waves <- df_consolidated_waves %>%
  group_by(country) %>%
  summarise(waves = paste(sort(unique(wave)), collapse = ","))

# Merge data
df_consolidated_waves <- df_consolidated_waves %>%
  left_join(df_waves, by = "country")

df_consolidated_waves$country <- countrycode(df_consolidated_waves$country, origin = "iso3c", destination = "country.name")

# Add explanation for excluded waves (consolidated democracies are those that did not experience any regime change thus all excluded waves are due to missing variables)
df_consolidated_waves <- df_consolidated_waves %>%
  mutate(waves_converted = str_split(waves, pattern = ",")) %>%
  mutate(all_waves_converted = str_split(all_waves, pattern = ","))

df_consolidated_waves <- df_consolidated_waves %>%
  mutate(waves_difference = map2_lgl(waves_converted, all_waves_converted, ~! setequal(.x, .y)))

df_consolidated_waves <- df_consolidated_waves %>%
  mutate(
    waves_excluded = case_when(
      waves_difference == FALSE ~ "",
      min_regime_score < 2 ~ "Regime change",
      TRUE ~ "Missing variables"
    )
  )

df_consolidated_waves <- df_consolidated_waves %>%
  select(country, waves, all_waves, waves_excluded)


# Output table for the main models
main_models <- list(
  "Model 1: SWIID" = model_1,
  "Model 2: WID" = model_2,
  "Model 3: SWIID" = model_3,
  "Model 4: WID" = model_4
)

count_1 <- model.frame(model_1)
count_2 <- model.frame(model_2)

count_1 <- as.character(n_distinct(count_1$country))
count_2 <- as.character(n_distinct(count_2$country))


country_row <- data.frame(
  term = c("Countries"),
  model_1 = count_1,
  model_2 = count_2,
  model_3 = count_1,
  model_4 = count_2
)

map_1 <- c(
  "gini_coefficient_swiid_disp_c" = "Economic inequality",
  "gini_coefficient_wid_post_c" = "Economic inequality",
  "income_t2:gini_coefficient_swiid_disp_c" = "Economic inequality x Middle income (Base: Low income)",
  "income_t3:gini_coefficient_swiid_disp_c" = "Economic inequality x High income (Base: Low income)",
  "income_t2:gini_coefficient_wid_post_c" = "Economic inequality x Middle income (Base: Low income)",
  "income_t3:gini_coefficient_wid_post_c" = "Economic inequality x High income (Base: Low income)",
  "income_t2" = "Middle income",
  "income_t3" = "High income",
  "gender" = "Gender (Female = 1)",
  "age" = "Age",
  "unemployed" = "Unemployed",
  "education" = "Education",
  "religiosity" = "Religiosity",
  "log_gdp_per_capita_c" = "GDP per capita (logged)",
  "gdp_growth_c" = "GDP growth"
)

main <- modelsummary(main_models,
             estimate = "{estimate}{stars} ({std.error})",
             statistic = NULL,
             output = "gt",
             coef_map = map_1,
             gof_omit = "IC|Log|F|AIC|BIC|R2|RMSE",
             add_rows = country_row,
             title = "Figure 2: Multilevel Models on Economic Inequality and the Socioeconomic Base of Authoritarian Support in Democracies") |>
  gt::opt_table_font(font = "Times New Roman") |>
  gt::tab_options(
    table.font.size = gt::px(12),
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    data_row.padding = gt::px(4),
    table.width = gt::pct(90)
  )

gtsave(main, "main_models.docx")


# Predicted probabilities plot for the main models
auth_support_predict_wid <- ggeffect(model_4, terms = c("gini_coefficient_wid_post_c [-2.70,2.02]", "income_t"), bias_correction = TRUE)

auth_support_predict_wid <- auth_support_predict_wid %>%
  mutate(
    income_category = factor(
      group,
      levels = c(1,2,3),
      labels = c("Low income", "Middle income", "High income"),
      ordered = TRUE
    )
  ) %>%
  filter(!is.na(income_category))

ggplot(auth_support_predict_wid, aes(x = x, y = predicted, color = income_category)) +
  geom_line(size = 1) +
  labs(x = "Gini coefficient from WID (centered)",
       y = "Probability of authoritarian support",
       title = "Figure 3. Predicted Probability of Authoritarian Support",
       color = "Income tier") +
  scale_color_brewer(palette = "Paired") +
  theme_minimal() +
  theme(
    text = element_text(family = "serif"),
    plot.title = element_text(size = 12),
    axis.title = element_text(size = 12)
  )


# Output table for robustness check models with consolidated democracies
supplementary_models_1 <- list(
  "Model 5: SWIID" = model_5,
  "Model 6: WID" = model_6,
  "Model 7: SWIID" = model_7,
  "Model 8: WID" = model_8
)

count_3 <- model.frame(model_5)
count_4 <- model.frame(model_6)

count_3 <- as.character(n_distinct(count_3$country))
count_4 <- as.character(n_distinct(count_4$country))


country_row <- data.frame(
  term = c("Countries"),
  model_5 = count_3,
  model_6 = count_4,
  model_7 = count_3,
  model_8 = count_4
)



map_2 <- c(
  "gini_coefficient_swiid_disp_c" = "Economic inequality",
  "gini_coefficient_wid_post_c" = "Economic inequality",
  "income_t2:gini_coefficient_swiid_disp_c" = "Economic inequality x Middle income",
  "income_t3:gini_coefficient_swiid_disp_c" = "Economic inequality x High income",
  "income_t2:gini_coefficient_wid_post_c" = "Economic inequality x Middle income",
  "income_t3:gini_coefficient_wid_post_c" = "Economic inequality x High income",
  "income_t2" = "Middle income",
  "income_t3" = "High income",
  "gender" = "Gender (Female = 1)",
  "age" = "Age",
  "unemployed" = "Unemployed",
  "education" = "Education",
  "religiosity" = "Religiosity",
  "log_gdp_per_capita_c" = "GDP per capita (logged)",
  "gdp_growth_c" = "GDP growth"
)

supplementary_1 <- modelsummary(supplementary_models_1,
             estimate = "{estimate}{stars} ({std.error})",
             statistic = NULL,
             output = "gt",
             coef_map = map_2,
             gof_omit = "IC|Log|F|AIC|BIC|R2|RMSE",
             add_rows = country_row,
             title = "Figure 4: Multilevel Models with Consolidated Democracies") |>
  gt::opt_table_font(font = "Times New Roman") |>
  gt::tab_options(
    table.font.size = gt::px(12),
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    data_row.padding = gt::px(4),
    table.width = gt::pct(90)
  )

gtsave(supplementary_1, "consolidated_democracies_models.docx")


# Output table for robustness check models with continuous income
supplementary_models_2 <- list(
  "Model 9: SWIID" = model_9,
  "Model 10: WID" = model_10,
  "Model 11: SWIID" = model_11,
  "Model 12: WID" = model_12
)

count_5 <- model.frame(model_9)
count_6 <- model.frame(model_10)
count_7 <- model.frame(model_11)
count_8 <- model.frame(model_12)

count_5 <- as.character(n_distinct(count_5$country))
count_6 <- as.character(n_distinct(count_6$country))
count_7 <- as.character(n_distinct(count_7$country))
count_8 <- as.character(n_distinct(count_8$country))

country_row <- data.frame(
  term = c("Countries"),
  model_9 = count_5,
  model_10 = count_6,
  model_11 = count_7,
  model_12 = count_8
)

map_3 <- c(
  "gini_coefficient_swiid_disp_c" = "Economic inequality",
  "gini_coefficient_wid_post_c" = "Economic inequality",
  "income_c" = "Income",
  "income_c:gini_coefficient_swiid_disp_c" = "Economic inequality x Income",
  "income_c:gini_coefficient_wid_post_c" = "Economic inequality x Income",
  "gender" = "Gender (Female = 1)",
  "age" = "Age",
  "unemployed" = "Unemployed",
  "education" = "Education",
  "religiosity" = "Religiosity",
  "social_trust" = "Social trust",
  "government_confidence" = "Government confidence",
  "political_ideology" = "Political ideology (Right = 10)",
  "log_gdp_per_capita_c" = "GDP per capita (logged)",
  "gdp_growth_c" = "GDP growth"
)

supplementary_2 <- modelsummary(supplementary_models_2,
             estimate = "{estimate}{stars} ({std.error})",
             statistic = NULL,
             output = "gt",
             coef_map = map_3,
             gof_omit = "IC|Log|F|AIC|BIC|R2|RMSE",
             add_rows = country_row,
             title = "Figure 5: Multilevel Models with Income as Continuous Variable") |>
  gt::opt_table_font(font = "Times New Roman") |>
  gt::tab_options(
    table.font.size = gt::px(12),
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    data_row.padding = gt::px(4),
    table.width = gt::pct(90)
  )

gtsave(supplementary_2, "continuous_income_models.docx")


