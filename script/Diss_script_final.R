#install.packages( c("parallelly","globals","listenv","future","future.apply","recipes","tidyverse","slider","lubridate","fixest","did","marginaleffects"),  dependencies = TRUE)

# ==============================================================================
#                       Prep: Libraries & Functions
# ==============================================================================
library(parallelly)
library(future)
library(future.apply)
library(recipes)
library(tidyverse)
library(slider)
library(lubridate)
library(fixest) 
library(did)
library(marginaleffects)
library(ggplot2)

set.seed(123)

# Because there were some issues with dates in TSV files being character I tried
# to standardize the process
# CY - Cyprus removed entirely due to data issues

# -------------------------
#  MONTHLY FUNCTION HELPERS
# -------------------------


# Date helper function
as_month_date <- function(x) {
  x <- as.character(x)
  out <- ifelse(grepl("^\\d{4}-\\d{2}$", x), paste0(x, "-01"),
                ifelse(grepl("^\\d{6}$", x), paste0(substr(x,1,4), "-",
                substr(x,5,6), "-01"), NA_character_))
  as.Date(out)
}

# Pivot helper function
pivot_monthly <- function(df, id_cols) {
  df %>%
    pivot_longer(cols = -all_of(id_cols), names_to = "date_chr", values_to = "value") %>%
    mutate(date = as_month_date(date_chr)) %>%
    select(-date_chr)%>%
    filter(country != "CY")
}

# -------------------------
# YEARLY FUNCTION HELPERS
# -------------------------

as_year_date <- function(x) {
  yr <- str_extract(as.character(x), "\\d{4}")  
  as.Date(paste0(yr, "-01-01"))
}

# Pivot helper function
pivot_long_year <- function(df, id_cols) {
  df %>%
    pivot_longer(cols = matches("^\\d{4}$"),
                 names_to = "year", values_to = "value") %>%
    mutate(date = as_year_date(year)) %>%
    select(-year)%>%
    filter(country != "CY")
}

# ==============================================================================
#                           MONTHLY DATA
# ==============================================================================

# -------------------------
#       RENEWABLES
#     Percentage (PC)
# -------------------------
raw_PC <- read_tsv("estat_nrg_cb_pem_filtered_PC.tsv", show_col_types = FALSE)

cleaned_PC <- raw_PC %>%
  separate(col = 1, into = c("freq", "siec", "unit", "country"), sep = ",") %>%
  filter(siec %in% c("RA100","RA110","RA120","RA200","RA300","RA310","RA320","RA400","RA410","RA420","RA500_5160"))

long_PC <- pivot_monthly(cleaned_PC, id_cols = c("freq","siec","unit","country")) %>%
  mutate(value = as.numeric(na_if(value, ":")))

renew_PC <- long_PC %>%
  pivot_wider(names_from = siec, values_from = value, names_prefix = "nrg_cb_pem_") %>%
  select(-freq, -unit) %>%
  rename(
    Hydro_Total_PC      = nrg_cb_pem_RA100,
    Hydro_Pure_PC       = nrg_cb_pem_RA110,
    Hydro_Mixed_PC      = nrg_cb_pem_RA120,
    Geothermal_PC       = nrg_cb_pem_RA200,
    Wind_Total_PC       = nrg_cb_pem_RA300,
    Wind_Onshore_PC     = nrg_cb_pem_RA310,
    Wind_Offshore_PC    = nrg_cb_pem_RA320,
    Solar_Total_PC      = nrg_cb_pem_RA400,
    Solar_Thermal_PC    = nrg_cb_pem_RA410,
    Solar_PV_PC         = nrg_cb_pem_RA420,
    Other_Renewables_PC = nrg_cb_pem_RA500_5160
  ) %>%
  mutate(Total_Renewables_PC = rowSums(across(c(Hydro_Total_PC, Geothermal_PC, Wind_Total_PC, Solar_Total_PC, Other_Renewables_PC)), na.rm = TRUE))

# -------------------------
#         RENEWABLES
#            (GWh)
# -------------------------
raw_GWH <- read_tsv("estat_nrg_cb_pem_filtered_GWH.tsv", show_col_types = FALSE)

cleaned_GWH <- raw_GWH %>%
  separate(col = 1, into = c("freq", "siec", "unit", "country"), sep = ",") %>%
  filter(siec %in% c("RA100","RA110","RA120","RA200",
                     "RA300","RA310","RA320","RA400","RA410","RA420","RA500_5160"))

long_GWH <- pivot_monthly(cleaned_GWH, id_cols = c("freq","siec","unit","country")) %>%
  mutate(value = as.numeric(na_if(value, ":")))

renew_GWH <- long_GWH %>%
  pivot_wider(names_from = siec, values_from = value, names_prefix = "nrg_cb_pem_") %>%
  select(-freq, -unit) %>%
  rename(
    Hydro_Total_GWH      = nrg_cb_pem_RA100,
    Hydro_Pure_GWH       = nrg_cb_pem_RA110,
    Hydro_Mixed_GWH      = nrg_cb_pem_RA120,
    Geothermal_GWH       = nrg_cb_pem_RA200,
    Wind_Total_GWH       = nrg_cb_pem_RA300,
    Wind_Onshore_GWH     = nrg_cb_pem_RA310,
    Wind_Offshore_GWH    = nrg_cb_pem_RA320,
    Solar_Total_GWH      = nrg_cb_pem_RA400,
    Solar_Thermal_GWH    = nrg_cb_pem_RA410,
    Solar_PV_GWH         = nrg_cb_pem_RA420,
    Other_Renewables_GWH = nrg_cb_pem_RA500_5160
  ) %>%
  mutate(Total_Renewables_GWH = rowSums(across(c(Hydro_Total_GWH, Geothermal_GWH,
                                                 Wind_Total_GWH, Solar_Total_GWH,
                                                 Other_Renewables_GWH)), na.rm = TRUE))

# -------------------------
#    GAS IMPORTS TOTAL
#       (NG + LNG)
# -------------------------
raw_total_gas <- read_tsv("estat_nrg_ti_gasm_filtered_Total.tsv", col_types = cols(.default = "c"), show_col_types = FALSE)

cleaned_total_gas <- raw_total_gas %>%
  separate(col = 1, into = c("freq","siec","partner","unit","country"), sep = ",") %>%
  filter(siec %in% c("G3000","G3200"))

# Monthly column names to YYYY-MM
date_cols_total <- setdiff(names(cleaned_total_gas), c("siec","partner","country","freq","unit"))
new_names_total <- vapply(date_cols_total, function(x){
  if (str_detect(x, "^\\\
\\d{6}$")) format(as.Date(paste0(x, "01"), "%Y%m%d"), "%Y-%m") else x
}, character(1))
names(cleaned_total_gas)[match(date_cols_total, names(cleaned_total_gas))] <- new_names_total

long_total <- pivot_monthly(cleaned_total_gas, id_cols = c("freq","siec","partner","unit","country")) %>%
  mutate(value = as.numeric(na_if(value, ":")))

final_total <- long_total %>%
  select(-partner, -unit, -freq) %>%
  pivot_wider(names_from = siec, values_from = value, names_prefix = "nrg_cb_pem_") %>%
  rename(NG_mcm_total = nrg_cb_pem_G3000,
         LNG_mcm_total = nrg_cb_pem_G3200) %>%
  mutate(agg_total_imports = rowSums(across(c(NG_mcm_total, LNG_mcm_total)), na.rm = TRUE))

# -------------------------
#    GAS IMPORTS RUSSIA
#       (NG + LNG)
# -------------------------
raw_ru_gas <- read_tsv("estat_nrg_ti_gasm_filtered_Russia.tsv", col_types = cols(.default = "c"), show_col_types = FALSE)

cleaned_ru_gas <- raw_ru_gas %>%
  separate(col = 1, into = c("freq","siec","partner","unit","country"), sep = ",") %>%
  filter(siec %in% c("G3000","G3200"))

date_cols_ru <- setdiff(names(cleaned_ru_gas), c("siec","partner","country","freq","unit"))
new_names_ru <- vapply(date_cols_ru, function(x){
  if (str_detect(x, "^\\d{6}$")) format(as.Date(paste0(x, "01"), "%Y%m%d"), "%Y-%m") else x
}, character(1))
names(cleaned_ru_gas)[match(date_cols_ru, names(cleaned_ru_gas))] <- new_names_ru

long_ru <- pivot_monthly(cleaned_ru_gas, id_cols = c("freq","siec","partner","unit","country")) %>%
  mutate(value = as.numeric(na_if(value, ":")))

final_ru <- long_ru %>%
  select(-partner, -unit, -freq) %>%
  pivot_wider(names_from = siec, values_from = value, names_prefix = "nrg_cb_pem_") %>%
  rename(NG_mcm_RU = nrg_cb_pem_G3000,
         LNG_mcm_RU = nrg_cb_pem_G3200) %>%
  mutate(agg_RU_imports = rowSums(across(c(NG_mcm_RU, LNG_mcm_RU)), na.rm = TRUE))

# -------------------------
#          GAS  
#      CONSUMPTION
# -------------------------
raw_gas_cons <- read_tsv("estat_nrg_cb_gasm_filtered_cons.tsv", col_types = cols(.default = "c"), show_col_types = FALSE)

cleaned_gas_cons <- raw_gas_cons %>%
  separate(col = 1, into = c("freq","nrg_bal","siec","unit","country"), sep = ",") %>%
  select(-freq, -unit, -nrg_bal)

long_gas_cons <- pivot_monthly(cleaned_gas_cons, id_cols = c("siec","country")) %>%
  mutate(gas_consumption_mcm = as.numeric(na_if(value, ":"))) %>%
  select(country, date, gas_consumption_mcm)

# -------------------------
#          COAL
#       CONSUMPTION
# -------------------------
raw_coal <- read_tsv("estat_nrg_cb_sffm_filtered.tsv", col_types = cols(.default = "c"), show_col_types = FALSE)

cleaned_coal <- raw_coal %>%
  separate(col = 1, into = c("freq","nrg_bal","siec","unit","country"), sep = ",") %>%
  select(-freq, -unit, -nrg_bal)

long_coal <- pivot_monthly(cleaned_coal, id_cols = c("siec","country")) %>%
  mutate(coal_consumption_tht = as.numeric(na_if(value, ":"))) %>%
  select(country, date, coal_consumption_tht)

# -------------------------
#           GAS 
#      STOCKS/Storage
# -------------------------
raw_stocks <- read_tsv("estat_nrg_stk_gasm_filtered.tsv", col_types = cols(.default = "c"), show_col_types = FALSE)

cleaned_stocks <- raw_stocks %>%
  separate(col = 1, into = c("freq","stk_flow","siec","unit","country"), sep = ",") %>%
  select(-freq, -unit, -stk_flow)

long_stocks <- pivot_monthly(cleaned_stocks, id_cols = c("siec","country")) %>%
  mutate(gas_stock_mcm = as.numeric(na_if(value, ":"))) %>%
  select(country, date, gas_stock_mcm)

# -------------------------
#       MERGE DATA
# -------------------------
merged_data <- renew_PC %>%
  full_join(renew_GWH,  by = c("country","date")) %>%
  full_join(final_total, by = c("country","date")) %>%
  full_join(final_ru,    by = c("country","date")) %>%
  full_join(long_gas_cons, by = c("country","date")) %>%
  full_join(long_coal,     by = c("country","date")) %>%
  full_join(long_stocks,   by = c("country","date")) %>%
  arrange(country, date)

# Russian import dependency (%)
merged_data <- merged_data %>%
  mutate(
    Russia_dependency_pct = case_when(
      is.na(agg_total_imports) | agg_total_imports == 0 ~ NA_real_,
      TRUE ~ (agg_RU_imports / agg_total_imports) * 100
    ),
    Russia_dependency_pct = round(pmin(Russia_dependency_pct, 100), 2),
  )

# -------------------------
#    C&S Treatment flagging 
#  >10% for 3 cons. months
# -------------------------
# Parameters
τ         <- 0.10                         # Threshold
K         <- 3                            # consecutive months
min_total <- 1                            # minimum total imports
min_start <- as.Date("2022-03-01")        # earliest treatment

# Pre-war dependency share (up to 02-2022)
pre_cut <- min_start
prewar_avg <- merged_data %>%
  filter(date < pre_cut) %>%
  mutate(
    pre_ru_share = if_else(!is.na(agg_total_imports) & agg_total_imports >= min_total,
                           agg_RU_imports / agg_total_imports, NA_real_)
  ) %>%
  group_by(country) %>%
  summarise(pre_dep_share = mean(pre_ru_share, na.rm = TRUE), .groups = "drop")


# Treatment based on dependency
merged_data <- merged_data %>%
  left_join(prewar_avg, by = "country") %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    ru_share = if_else(!is.na(agg_total_imports) & agg_total_imports >= min_total, # Current month’s RU share
                       agg_RU_imports / agg_total_imports, NA_real_),
    low      = !is.na(ru_share) & (ru_share <= τ), # Flag months where RU share is ≤ τ
    nonmiss_k = slide_int(!is.na(ru_share), ~ sum(.x), .before = K - 1, .after = 0, .complete = TRUE), # accounts for missing data K-month window
    all_low_k = slide_lgl(low, ~ all(.x), .before = K - 1, .after = 0, .complete = TRUE), # Are all K months low?
    eligible = !is.na(pre_dep_share) & (pre_dep_share > τ), # Accounts for countries with no or negligible amounts of imports pre war
    cutoff_hit = eligible & (nonmiss_k == K) & all_low_k & (date >= min_start), # Cutoff hits only if eligible, meets K-month low condition, and date is after min_start
    first_treat_date = if (any(cutoff_hit, na.rm = TRUE)) min(date[cutoff_hit]) else as.Date(NA) # First month of sustained dependency cut-off
  ) %>%
  ungroup() %>%
  mutate(
    treat = if_else(!is.na(first_treat_date) & date >= first_treat_date, 1L, 0L),# Treatment indicator... once treated, always treated
    # Group and time identifiers for C&S
    tname = year(date) * 100L + month(date),
    gname = if_else(is.na(first_treat_date), 0L,
                    year(first_treat_date) * 100L + month(first_treat_date)),
    event_time = if_else(is.na(first_treat_date), NA_integer_,
                         (year(date) - year(first_treat_date)) * 12L +
                           (month(date) - month(first_treat_date)))
  )
# -------------------------
# Export SINGLE merged CSV
# -------------------------
write_csv(merged_data, "merged_monthly_energy_dataset.csv")

# ==============================================================================
#                           YEARLY DATA
# ==============================================================================

# -------------------------
#        POPULATION 
# -------------------------
raw_pop <- read_tsv("estat_demo_pjan_filtered.tsv",
                    col_types = cols(.default = "c"))

pop_data <- raw_pop %>%
  separate(col = 1,
           into = c("freq", "unit", "age", "sex", "country"),
           sep = ",") %>%
  filter(age == "TOTAL", sex == "T") %>%
  select(-freq, -unit, -age, -sex) %>%
  pivot_long_year(id_cols = c("country")) %>%
  mutate(population = as.numeric(na_if(str_replace_all(value, "[a-z]", ""), ":"))) %>%
  select(country, date, population)

# -------------------------
#           GDP
#        PER CAPITA (PPS)
# -------------------------
raw_gdp <- read_tsv("estat_nama_10_pc_filtered.tsv",
                    col_types = cols(.default = "c"))

gdp_data <- raw_gdp %>%
  separate(col = 1,
           into = c("freq", "unit", "na_item", "country"),
           sep = ",") %>%
  filter(na_item == "B1GQ") %>%
  select(-freq, -unit, -na_item) %>%
  pivot_long_year(id_cols = c("country")) %>%
  mutate(gdp_pc_pps = as.numeric(na_if(str_replace_all(value, "[a-z]", ""), ":"))) %>%
  select(country, date, gdp_pc_pps)

# -------------------------
#       FOSSIL FUEL 
#    SHARE OF ENERGY MIX
# -------------------------
raw_energy <- read_tsv("estat_nrg_bal_c_filtered.tsv",
                       col_types = cols(.default = "c"))

cleaned_mix <- raw_energy %>%
  separate(col = 1,
           into = c("freq", "nrg_bal", "siec", "unit", "country"),
           sep = ",") %>%
  filter(siec %in% c("C0000X0350-0370", "O4000XBIO",
                     "G3000", "TOTAL")) %>%
  select(-freq, -unit, -nrg_bal)

long_mix <- cleaned_mix %>%
  pivot_long_year(id_cols = c("country", "siec")) %>%
  mutate(siec = recode(siec,
                       "C0000X0350-0370" = "COAL",
                       "O4000XBIO"      = "OIL",
                       "G3000"          = "GAS"))

wide_mix_share <- long_mix %>%
  pivot_wider(names_from = siec, values_from = value) %>%
  mutate(across(c(COAL, GAS, OIL, TOTAL), as.numeric),
         fossil_share = 100 * (COAL + GAS + OIL) / TOTAL) %>%
  select(country, date, COAL, GAS, OIL, fossil_share)

# -------------------------
#    ENERGY CONSUMPTION 
#       PER CAPITA 
#    (MWh&GWh/person)
# -------------------------
energy_cons <- raw_energy %>%
  separate(col = 1,
           into = c("freq", "unit", "siec",
                    "nrg_bal", "country"),
           sep = ",") %>%
  filter(siec == "TOTAL") %>%
  select(-freq, -unit, -nrg_bal)

long_energy_total <- energy_cons %>%
  pivot_long_year(id_cols = c("country")) %>%
  mutate(total_energy_gwh = as.numeric(na_if(value, ":"))) %>%
  select(country, date, total_energy_gwh)

# Divide by population
energy_per_capita <- long_energy_total %>%
  left_join(pop_data, by = c("country", "date")) %>%
  mutate(energy_per_capita_gwh = total_energy_gwh / population,
         energy_per_capita_mwh = energy_per_capita_gwh * 1000) %>%
  select(country, date, energy_per_capita_gwh, energy_per_capita_mwh)


# -------------------------
#        MERGE DATA
# -------------------------
merged_annual_data <- pop_data %>%
  full_join(gdp_data,        by = c("country", "date")) %>%
  full_join(wide_mix_share,  by = c("country", "date")) %>%
  full_join(energy_per_capita, by = c("country", "date")) %>%
  arrange(country, date)

# Annual CSV
write_csv(merged_annual_data, "merged_annual_data.csv")

#-------------------------------------------------------------------------------
#                          DiD Pipeline
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                          prep data
#-------------------------------------------------------------------------------

year_month_merged <- read_csv("merged_monthly_energy_dataset.csv") %>%
  mutate(year = year(date)) %>%
  left_join(
    read_csv("merged_annual_data.csv") %>%
      mutate(year = year(date)) %>%
      select(-date),
    by = c("country", "year")
  ) %>%
  mutate(
    log_Renewables_PC = log1p(Total_Renewables_PC),
    log_Renewables_Gwh = log1p(Total_Renewables_GWH),
    log_gdp_pc_pps    = log1p(gdp_pc_pps),
    log_energy_pc_mwh = log1p(energy_per_capita_mwh)
  ) %>%
  mutate(
    post_t = if_else(tname >= 202203, 1, 0),
    pre_dep_share = pre_dep_share * 100
  )

cont_did_data <- year_month_merged %>%
  select(country, date, tname, post_t, pre_dep_share,
         log_Renewables_PC, log_Renewables_Gwh, Total_Renewables_PC, Total_Renewables_GWH,
         fossil_share, log_energy_pc_mwh, log_gdp_pc_pps,
         gas_stock_mcm, coal_consumption_tht, gas_consumption_mcm, population)

#-------------------------------------------------------------------------------
#          Continuous DiD models (pre/post)
#-------------------------------------------------------------------------------

cont_mod_PC <- cont_did_data %>%
  feols(Total_Renewables_PC ~ pre_dep_share * post_t | country + date,
        cluster = ~ country, .)

cont_mod_PC_2 <- cont_did_data %>%
  feols(Total_Renewables_PC ~ pre_dep_share * post_t +
          gas_stock_mcm + fossil_share + log_energy_pc_mwh + log_gdp_pc_pps + population |
          country + date,
        cluster = ~ country, .)

cont_mod_Gwh <- cont_did_data %>%
  feols(Total_Renewables_GWH ~ pre_dep_share * post_t | country + date,
        cluster = ~ country, .)

cont_mod_Gwh_2 <- cont_did_data %>%
  feols(Total_Renewables_GWH ~ pre_dep_share * post_t +
          gas_stock_mcm + fossil_share + log_energy_pc_mwh + log_gdp_pc_pps + population |
          country + date,
        cluster = ~ country, .)

# Log transformed robustness (Robustness)

#cont_mod_PC <- cont_did_data %>% feols(log_Renewables_PC ~ pre_dep_share * post_t | country + date, cluster = ~ country, .)

#cont_mod_PC_2 <- cont_did_data %>% feols(log_Renewables_PC ~ pre_dep_share * post_t + gas_stock_mcm + fossil_share + log_energy_pc_mwh + log_gdp_pc_pps + population | country + date, cluster = ~ country, .)

#cont_mod_Gwh <- cont_did_data %>% feols(log_Renewables_Gwh ~ pre_dep_share * post_t | country + date, cluster = ~ country, .)

#cont_mod_Gwh_2 <- cont_did_data %>% feols(log_Renewables_Gwh ~ pre_dep_share * post_t + gas_stock_mcm + fossil_share + log_energy_pc_mwh + log_gdp_pc_pps + population | country + date, cluster = ~ country, .)

#Additional outcomes as robustness checks
#cont_mod_gas <- cont_did_data %>%
#  feols(gas_consumption_mcm ~ pre_dep_share * post_t | country + date,
#        cluster = ~ country, .)

#cont_mod_coal <- cont_did_data %>%
#  feols(coal_consumption_tht ~ pre_dep_share * post_t | country + date,
#        cluster = ~ country, .)

#-----------------------------Tables and Figures--------------------------------

term <- "pre_dep_share:post_t"

ct1 <- summary(cont_mod_PC)$coeftable
e1  <- ct1[term,"Estimate"]; 
s1 <- ct1[term,"Std. Error"];
p1 <- ct1[term, grep("^Pr", colnames(ct1))]

ct2 <- summary(cont_mod_PC_2)$coeftable
e2  <- ct2[term,"Estimate"]; 
s2 <- ct2[term,"Std. Error"]; 
p2 <- ct2[term, grep("^Pr", colnames(ct2))]

ct3 <- summary(cont_mod_Gwh)$coeftable
e3  <- ct3[term,"Estimate"]; 
s3 <- ct3[term,"Std. Error"]; 
p3 <- ct3[term, grep("^Pr", colnames(ct3))]

ct4 <- summary(cont_mod_Gwh_2)$coeftable
e4  <- ct4[term,"Estimate"]; 
s4 <- ct4[term,"Std. Error"]; 
p4 <- ct4[term, grep("^Pr", colnames(ct4))]

specs <- c("TWFE PC (no controls)",
           "TWFE PC (controls)",
           "TWFE GWh (no controls)",
           "TWFE GWh (controls)")
specs_PC <- c("TWFE PC (no controls)",
           "TWFE PC (controls)")
specs_Gwh <-  c("TWFE GWh (no controls)",
                "TWFE GWh (controls)")

table_dy <- tibble(
  spec     = specs,
  term     = term,
  estimate = c(e1, e2, e3, e4),
  se       = c(s1, s2, s3, s4),
  ci_low   = estimate - 1.96 * se,
  ci_high  = estimate + 1.96 * se,
  p_value  = c(p1, p2, p3, p4)
)

table_dy_PC <- tibble(
  spec     = specs_PC,
  term     = term,
  estimate = c(e1, e2),
  se       = c(s1, s2),
  ci_low   = estimate - 1.96 * se,
  ci_high  = estimate + 1.96 * se,
  p_value  = c(p1, p2)
)

table_dy_Gwh <- tibble(
  spec     = specs_Gwh,
  term     = term,
  estimate = c(e3, e4),
  se       = c(s3, s4),
  ci_low   = estimate - 1.96 * se,
  ci_high  = estimate + 1.96 * se,
  p_value  = c(p3, p4)
)

table_dy
table_dy_PC
table_dy_Gwh

F_cont_PC <- ggplot(
  table_dy_PC,
  aes(x = estimate, y = factor(spec, levels = rev(specs_PC)))
) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high)) +
  labs(
    title = "Effect of Pre-war Dependence in the Post Period",
    subtitle = "Coefficient on Pre. Dep. × post_t",
    x = "Change in renewables share (pp)",
    y = NULL,
    caption = "Post period begins Mar-2022.\n Pre dependence share = 2019–2021 mean Russian share of total gas imports.\n Points = estimates; lines = 95% CIs."
  ) +
  theme_minimal()

F_cont_Gwh <- ggplot(
  table_dy_Gwh,
  aes(x = estimate, y = factor(spec, levels = rev(specs_Gwh)))
) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_pointrange(aes(xmin = ci_low, xmax = ci_high)) +
  labs(
    title = "Effect of Pre-war Dependence in the Post Period",
    subtitle = "Coefficient on Pre. Dep. × post_t",
    x = "Change in renewables (GWh/month)",
    y = NULL,
    caption = "Post period begins Mar-2022.\n  Pre dependence share = 2019–2021 mean Russian share of total gas imports.\n Points = estimates; lines = 95% CIs"
  ) +
  theme_minimal()

F_cont_PC 
F_cont_Gwh 

#-------------------------------------------------------------------------------
#              DiD dynamic (event‑time) models
#-------------------------------------------------------------------------------

cont_did_data <- cont_did_data %>%
  mutate(
    rel_month = (year(date) - 2022L) * 12L +
      (month(date) - 3L)
  )

#Dynamic model for renewables share (PC)
dy_mod_PC <- cont_did_data %>%
  feols(Total_Renewables_PC ~ i(rel_month, pre_dep_share, ref = -1) |
          country + date, .)

#Dynamic model for renewables level (GWh)
dy_mod_Gwh <- cont_did_data %>%
  feols(Total_Renewables_GWH ~ i(rel_month, pre_dep_share, ref = -1) |
          country + date, .)

#Predictions for dynamic model
prs_PC <- avg_predictions(dy_mod_PC, variables = c("pre_dep_share", "rel_month"))
prs_Gwh <- avg_predictions(dy_mod_Gwh, variables = c("pre_dep_share", "rel_month"))

summary(prs_PC)
summary(prs_Gwh)
#-----------------------------Tables and Figures--------------------------------

F_dy_PC <- prs_PC %>% 
  mutate(months = as.numeric(as.character(rel_month)),
         pre_dep_share = as.factor(round(pre_dep_share,2))) %>% 
  ggplot(aes(x = months, y = estimate, ymin = conf.low, ymax = conf.high,
             colour = pre_dep_share, fill = pre_dep_share)) +
  geom_ribbon(alpha = .25, colour = NA) +
  geom_line() +
  geom_vline(xintercept = -1, linetype = "dashed") +
  labs(
    title = "Dynamic DiD: Renewables Share (PC)",
    x = "Months relative to Mar-2022 (k)",
    y = "Predicted renewables share (pp)",
    colour = "Pre-war Dep.", fill = "Pre-war Dep."
  ) +
  theme_minimal()

F_dy_Gwh <- prs_Gwh %>% 
  mutate(months = as.numeric(as.character(rel_month)),
         pre_dep_share = as.factor(round(pre_dep_share,2))) %>% 
  ggplot(aes(x = months, y = estimate, ymin = conf.low, ymax = conf.high,
             colour = pre_dep_share, fill = pre_dep_share)) +
  geom_ribbon(alpha = .25, colour = NA) +
  geom_line() +
  geom_vline(xintercept = -1, linetype = "dashed") +
  labs(
    title = "Dynamic DiD: Renewables Level (Gwh)",
    x = "Months relative to Mar-2022 (k)",
    y = "Predicted renewables Level (Gwh)",
    colour = "Pre-war dep.", fill = "Pre-war dep."
    )+
  theme_minimal()

F_dy_PC
F_dy_Gwh

#-------------------------------------------------------------------------------
#             Categorical exposure models
#-------------------------------------------------------------------------------

cont_did_data <- cont_did_data %>%
  mutate(
    dep_cat = cut(pre_dep_share, c(-Inf, 10, 40, Inf),
                  labels = c("Low", "Medium", "High"))
  )

cat_mod_PC <- cont_did_data %>%
  feols(Total_Renewables_PC ~ i(rel_month, dep_cat, ref = -1) |
          country, .)

cat_mod_Gwh <- cont_did_data %>%
  feols(Total_Renewables_GWH ~ i(rel_month, dep_cat, ref = -1) |
          country, .)

#categorical pred
cat_prs_PC <- avg_predictions(cat_mod_PC, variables = c("dep_cat", "rel_month"))
cat_prs_Gwh <- avg_predictions(cat_mod_Gwh, variables = c("dep_cat", "rel_month"))

F_cat_PC <- cat_prs_PC %>% 
  mutate(months = as.numeric(as.character(rel_month))) %>% 
  ggplot(aes(x = months, y = estimate, ymin = conf.low, ymax = conf.high,
             colour = dep_cat, fill = dep_cat)) +
  geom_ribbon(alpha = .25, colour = NA) +
  geom_line() +
  geom_vline(xintercept = -1, linetype = "dashed") +
  labs(
    title = "Categorical DiD: Renewables Share (PC)",
    x = "Months relative to Mar-2022 (k)",
    y = "Predicted renewables share (%)",
    colour = "Pre-war dependence",
    fill   = "Pre-war dependence"
  ) +
  theme_minimal()

F_cat_Gwh <- cat_prs_Gwh %>% 
  mutate(months = as.numeric(as.character(rel_month))) %>% 
  ggplot(aes(x = months, y = estimate, ymin = conf.low, ymax = conf.high,
             colour = dep_cat, fill = dep_cat)) +
  geom_ribbon(alpha = .25, colour = NA) +
  geom_line() +
  geom_vline(xintercept = -1, linetype = "dashed") +
  labs(
    title = "Categorical DiD: Renewables Level (GWh)",
    x = "Months relative to Mar-2022 (k)",
    y = "Predicted renewables (GWh)",
    colour = "Pre-war dependence",
    fill   = "Pre-war dependence"
  ) +
  theme_minimal()

F_cat_PC
F_cat_Gwh


#-------------------------------------------------------------------------------
#              Callaway & Sant’Anna (C&S) DiD estimators
#-------------------------------------------------------------------------------

analysis_data_cs <- year_month_merged %>%
  mutate(idname = as.numeric(factor(country, levels = unique(country))))

analysis_data_cs <-analysis_data_cs %>%
  select(idname, country, date, treat, gname, tname, pre_dep_share,
         Total_Renewables_PC, Total_Renewables_GWH,
         log_gdp_pc_pps) 

# Convert monthly to quarterly and create new treatment variables
analysis_data_cs <- analysis_data_cs %>%
  mutate(
    year            = tname %/% 100L,
    month           = tname %%  100L,
    quarter         = ((month - 1L) %/% 3L) + 1L,
    quarter_index   = as.integer(year - min(year)) * 4 + quarter)%>%
  group_by(idname) %>%
  mutate(
    trt_year    = gname %/% 100L,
    trt_month   = gname %% 100L,
    trt_quarter = ((trt_month - 1L) %/% 3L) + 1L,
    first_trt_quarter = ifelse(
      gname == 0L, 0L,
      quarter_index[tname == gname]),
  )%>%
  ungroup()%>%
  # Waves due to tiny cohorts and some naming issues
  # Mental note:
  #   1st half 2022 (Q2    2022) -> wave 1
  #   2nd half 2022 (Q3–Q4 2022) -> wave 2
  #   1st half 2023 (Q1–Q2 2023) -> wave 3
  mutate(
    pooled_wave = case_when(
      first_trt_quarter == 0L                ~ 0L,  # never treated
      first_trt_quarter %in% c(18L)          ~ 18L,  # wave 1
      first_trt_quarter %in% c(19L, 20L)     ~ 20L,  # wave 2
      first_trt_quarter %in% c(21L, 22L)     ~ 21L,  # wave 3
      TRUE                                         ~ first_trt_quarter
    ))
ref_q <- 8

wave_summary <- analysis_data_cs %>%
  group_by(pooled_wave) %>%
  summarise(
    first_trt_quarter = ifelse(unique(pooled_wave) == 0L, NA_integer_,
                               min(first_trt_quarter[first_trt_quarter > 0])),
    num_countries = n_distinct(idname),
    avg_prewar_dep = mean(pre_dep_share[year < 2022], na.rm = TRUE),
    pre_quarters = first_trt_quarter - ref_q,
    post_quarters = mean(max(quarter_index) - first_trt_quarter, na.rm = TRUE)
  ) %>%
  arrange(first_trt_quarter)

# Outcome and control to quarter level
analysis_data_cs <- analysis_data_cs %>%
  group_by(idname, country, quarter_index, gname, pooled_wave) %>%
  summarise(
    renewables       = mean(Total_Renewables_PC, na.rm = TRUE),
    renewables_Gwh   = mean(Total_Renewables_GWH, na.rm = TRUE),
    log_gdp          = mean(log_gdp_pc_pps,      na.rm = TRUE),
    .groups          = "drop"
  ) %>%
  filter(is.finite(renewables))

# Keep units with a single cohort and drop units treated at entry 
analysis_data_cs <- analysis_data_cs %>%
  group_by(idname) %>% filter(n_distinct(pooled_wave) == 1L) %>% ungroup() %>%
  group_by(idname) %>%
  mutate(first_obs_q = min(quarter_index), gval = unique(pooled_wave)) %>%
  ungroup() %>%
  filter(!(gval > 0L & gval <= first_obs_q)) %>%
  select(-first_obs_q, -gval)

#-------------------------------------------------------------------------------
#                      WAR SPECIFICATION (2022Q1-2024Q4)
#-------------------------------------------------------------------------------

# War window (2020Q1–2024Q4)
war_data <- analysis_data_cs %>%
  filter(quarter_index >= 9L & quarter_index <= 28L)

# 

# Renewables % - nevertreated 
att_war_never <- att_gt(
  yname   = "renewables",
  tname   = "quarter_index",
  idname  = "idname",
  gname   = "pooled_wave",
  xformla = ~ log_gdp,
  data    = war_data,
  control_group = "nevertreated",
  anticipation  = 0,
  bstrap   = TRUE,
  clustervars = "idname"
)
summary(att_war_never)

# Renewables % - nyt  (robustness)
att_war_nyt <- att_gt(
  yname   = "renewables",
  tname   = "quarter_index",
  idname  = "idname",
  gname   = "pooled_wave",
  xformla = ~ log_gdp,
  data    = war_data,
  control_group = "notyettreated",
  anticipation  = 0,
  bstrap   = TRUE,
  clustervars = "idname"
)
summary(att_war_nyt)

# Renewables Gwh - nevertreated 
att_war_never_Gwh <- att_gt(
  yname   = "renewables_Gwh",
  tname   = "quarter_index",
  idname  = "idname",
  gname   = "pooled_wave",
  xformla = ~ log_gdp,
  data    = war_data,
  control_group = "nevertreated",
  anticipation  = 0,
  bstrap   = TRUE,
  clustervars = "idname"
)
summary(att_war_never_Gwh)

# Renewables Gwh - nyt (robustness)
att_war_nyt_Gwh <- att_gt(
  yname   = "renewables_Gwh",
  tname   = "quarter_index",
  idname  = "idname",
  gname   = "pooled_wave",
  xformla = ~ log_gdp,
  data    = war_data,
  control_group = "notyettreated",
  anticipation  = 0,
  bstrap   = TRUE,
  clustervars = "idname"
)
summary(att_war_nyt_Gwh)

# Event-study curves
es_war_never <- aggte(att_war_never, type = "dynamic")
es_war_nyt   <- aggte(att_war_nyt,   type = "dynamic")
es_war_never_Gwh <- aggte(att_war_never_Gwh, type = "dynamic")
es_war_nyt_Gwh   <- aggte(att_war_nyt_Gwh,   type = "dynamic")

#---------------------------------PLOTS-----------------------------------------

# Plot % nevertreated
never_war_pct_df <- data.frame(
  event_time = es_war_never$egt,
  att        = es_war_never$att.egt,
  se         = es_war_never$se.egt
) %>%
  mutate(
    ci_lower = att - qnorm(0.975) * se,
    ci_upper = att + qnorm(0.975) * se
  )

ggdid(es_war_never)+
  theme_minimal() +
  labs(
    title = "Event-Study: Treatment Effects on Renewables (%)",
    subtitle = "War - Never-treated",
    x = "Quarters relative to treatment",
    y = "ATT estimate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )


# Plot % nyt
nyt_war_pct_df <- data.frame(
  event_time = es_war_nyt$egt,
  att        = es_war_nyt$att.egt,
  se         = es_war_nyt$se.egt
) %>%
  mutate(
    ci_lower = att - qnorm(0.975) * se,
    ci_upper = att + qnorm(0.975) * se
  )

ggdid(es_war_nyt)+
  theme_minimal() +
  labs(
    title = "Event-Study: Treatment Effects on Renewables (%)",
    subtitle = "War - Not-yet-treated",
    x = "Quarters relative to treatment",
    y = "ATT estimate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Plot Gwh nevertreated
never_war_gwh_df <- data.frame(
  event_time = es_war_never_Gwh$egt,
  att        = es_war_never_Gwh$att.egt,
  se         = es_war_never_Gwh$se.egt
) %>%
  mutate(
    ci_lower = att - qnorm(0.975) * se,
    ci_upper = att + qnorm(0.975) * se
  )

ggdid(es_war_never_Gwh)+
  theme_minimal() +
  labs(
    title = "Event-Study: Treatment Effects on Renewables (Gwh)",
    subtitle = "War - Never-treated",
    x = "Quarters relative to treatment",
    y = "ATT estimate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Plot Gwh nyt
nyt_war_gwh_df <- data.frame(
  event_time = es_war_nyt_Gwh$egt,
  att        = es_war_nyt_Gwh$att.egt,
  se         = es_war_nyt_Gwh$se.egt
) %>%
  mutate(
    ci_lower = att - qnorm(0.975) * se,
    ci_upper = att + qnorm(0.975) * se
  )

ggdid(es_war_nyt_Gwh)+
  theme_minimal() +
  labs(
    title = "Event-Study: Treatment Effects on Renewables (Gwh)",
    subtitle = "War - Not-yet-treated",
    x = "Quarters relative to treatment",
    y = "ATT estimate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )



ov_never <-  aggte(att_war_never, type = "simple")
ov_nyt   <-  aggte(att_war_nyt,   type = "simple")


t6_cs <- data.frame(
  Model   = c("C&S ATT (never-treated)", "C&S ATT (not-yet-treated)"),
  Outcome = c("Renewables (%)",          "Renewables (%)"),
  Est     = c(ov_never$overall.att,      ov_nyt$overall.att),
  SE      = c(ov_never$overall.se,       ov_nyt$overall.se)
)
t6_cs$CI_low  <- t6_cs$Est - 1.96 * t6_cs$SE
t6_cs$CI_high <- t6_cs$Est + 1.96 * t6_cs$SE

print(t6_cs)


ov_never_Gwh <-  aggte(att_war_never_Gwh, type = "simple")
ov_nyt_Gwh   <-  aggte(att_war_nyt_Gwh,   type = "simple")

t6_cs_gwh <- data.frame(
  Model = c("C&S ATT (never-treated) [GWh]", "C&S ATT (not-yet-treated) [GWh]"),
  Est   = c(ov_never_Gwh$overall.att,        ov_nyt_Gwh$overall.att),
  SE    = c(ov_never_Gwh$overall.se,         ov_nyt_Gwh$overall.se)
)
t6_cs_gwh$CI_low  <- t6_cs_gwh$Est - 1.96 * t6_cs_gwh$SE
t6_cs_gwh$CI_high <- t6_cs_gwh$Est + 1.96 * t6_cs_gwh$SE
print(t6_cs_gwh)

get_row <- function(model, spec, outcome) {
  ct <- summary(model)$coeftable
  est <- ct[term, "Estimate"]
  se <- ct[term, "Std. Error"]
  p <- ct[term, grep("^Pr", colnames(ct))]
  tibble(
    spec = spec,
    outcome = outcome,
    term = term,
    estimate = est,
    se = se,
    ci_low = est - 1.96 * se,
    ci_high = est + 1.96 * se,
    p_value = as.numeric(p)
  )
}


T6a <- bind_rows(
  get_row(cont_mod_PC, "TWFE PC (no controls)", "Renewables (%)"),
  get_row(cont_mod_PC_2, "TWFE PC (controls)", "Renewables (%)"),
  get_row(cont_mod_Gwh, "TWFE GWh (no controls)", "Renewables (GWh)"),
  get_row(cont_mod_Gwh_2, "TWFE GWh (controls)", "Renewables (GWh)")
) %>%
  mutate(across(c(estimate, se, ci_low, ci_high), ~round(.x, 3)))

print(T6a)


# Wave att_gt
# NEVERTREATED
w1_dat_never_pct <- war_data[war_data$pooled_wave %in% c(0L, 18L), ]
w1_att_never_pct <-  att_gt(yname = "renewables",
                            tname = "quarter_index",
                            idname = "idname",
                            gname = "pooled_wave",
                            xformla = ~ log_gdp,
                            data = w1_dat_never_pct,
                            control_group = "nevertreated",
                            anticipation = 0,
                            bstrap = TRUE,
                            clustervars = "idname")
w1_es_never_pct  <-  aggte(w1_att_never_pct, type = "dynamic")
w1_df_never_pct  <- data.frame(e = w1_es_never_pct$egt, att = w1_es_never_pct$att.egt, se = w1_es_never_pct$se.egt, Wave = "W1")

w2_dat_never_pct <- war_data[war_data$pooled_wave %in% c(0L, 20L), ]
w2_att_never_pct <-  att_gt(yname = "renewables",
                            tname = "quarter_index",
                            idname = "idname",
                            gname = "pooled_wave",
                            xformla = ~ log_gdp,
                            data = w2_dat_never_pct,
                            control_group = "nevertreated",
                            anticipation = 0, bstrap = TRUE,
                            clustervars = "idname")
w2_es_never_pct  <-  aggte(w2_att_never_pct, type = "dynamic")
w2_df_never_pct  <- data.frame(e = w2_es_never_pct$egt, att = w2_es_never_pct$att.egt, se = w2_es_never_pct$se.egt, Wave = "W2")

w3_dat_never_pct <- war_data[war_data$pooled_wave %in% c(0L, 21L), ]
w3_att_never_pct <-  att_gt(yname = "renewables",
                            tname = "quarter_index",
                            idname = "idname",
                            gname = "pooled_wave",
                            xformla = ~ log_gdp,
                            data = w3_dat_never_pct,
                            control_group = "nevertreated",
                            anticipation = 0,
                            bstrap = TRUE,
                            clustervars = "idname")
w3_es_never_pct  <-  aggte(w3_att_never_pct, type = "dynamic")
w3_df_never_pct  <- data.frame(e = w3_es_never_pct$egt, att = w3_es_never_pct$att.egt, se = w3_es_never_pct$se.egt, Wave = "W3")

# Combine / CI
w_never_pct <- rbind(w1_df_never_pct, w2_df_never_pct, w3_df_never_pct)
w_never_pct$lo <- w_never_pct$att - 1.96 * w_never_pct$se
w_never_pct$hi <- w_never_pct$att + 1.96 * w_never_pct$se

# NOT-YET-TREATED
w1_dat_nyt_pct <- war_data[war_data$pooled_wave %in% c(0L, 18L), ]
w1_att_nyt_pct <-  att_gt(yname = "renewables",
                          tname = "quarter_index",
                          idname = "idname",
                          gname = "pooled_wave",
                          xformla = ~ log_gdp,
                          data = w1_dat_nyt_pct,
                          control_group = "notyettreated",
                          anticipation = 0, bstrap = TRUE,
                          clustervars = "idname")
w1_es_nyt_pct  <-  aggte(w1_att_nyt_pct, type = "dynamic")
w1_df_nyt_pct  <- data.frame(e = w1_es_nyt_pct$egt, att = w1_es_nyt_pct$att.egt, se = w1_es_nyt_pct$se.egt, Wave = "W1")

w2_dat_nyt_pct <- war_data[war_data$pooled_wave %in% c(0L, 20L), ]
w2_att_nyt_pct <-  att_gt(yname = "renewables",
                          tname = "quarter_index",
                          idname = "idname",
                          gname = "pooled_wave",
                          xformla = ~ log_gdp,
                          data = w2_dat_nyt_pct,
                          control_group = "notyettreated",
                          anticipation = 0,
                          bstrap = TRUE,
                          clustervars = "idname")
w2_es_nyt_pct  <-  aggte(w2_att_nyt_pct, type = "dynamic")
w2_df_nyt_pct  <- data.frame(e = w2_es_nyt_pct$egt, att = w2_es_nyt_pct$att.egt, se = w2_es_nyt_pct$se.egt, Wave = "W2")

w3_dat_nyt_pct <- war_data[war_data$pooled_wave %in% c(0L, 21L), ]
w3_att_nyt_pct <-  att_gt(yname = "renewables",
                          tname = "quarter_index",
                          idname = "idname",
                          gname = "pooled_wave",
                          xformla = ~ log_gdp,
                          data = w3_dat_nyt_pct,
                          control_group = "notyettreated",
                          anticipation = 0, bstrap = TRUE,
                          clustervars = "idname")
w3_es_nyt_pct  <-  aggte(w3_att_nyt_pct, type = "dynamic")
w3_df_nyt_pct  <- data.frame(e = w3_es_nyt_pct$egt, att = w3_es_nyt_pct$att.egt, se = w3_es_nyt_pct$se.egt, Wave = "W3")


w_nyt_pct <- rbind(w1_df_nyt_pct, w2_df_nyt_pct, w3_df_nyt_pct)
w_nyt_pct$lo <- w_nyt_pct$att - 1.96 * w_nyt_pct$se
w_nyt_pct$hi <- w_nyt_pct$att + 1.96 * w_nyt_pct$se

# NEVERTREATED GWH
g1_dat_never_gwh <- war_data[war_data$pooled_wave %in% c(0L, 18L), ]
g1_att_never_gwh <-  att_gt(yname = "renewables_Gwh",
                            tname = "quarter_index",
                            idname = "idname",
                            gname = "pooled_wave",
                            xformla = ~ log_gdp,
                            data = g1_dat_never_gwh,
                            control_group = "nevertreated",
                            anticipation = 0,
                            bstrap = TRUE,
                            clustervars = "idname")
g1_es_never_gwh  <-  aggte(g1_att_never_gwh, type = "dynamic")
g1_df_never_gwh  <- data.frame(e = g1_es_never_gwh$egt, att = g1_es_never_gwh$att.egt, se = g1_es_never_gwh$se.egt, Wave = "W1")

g2_dat_never_gwh <- war_data[war_data$pooled_wave %in% c(0L, 20L), ]
g2_att_never_gwh <-  att_gt(yname = "renewables_Gwh",
                            tname = "quarter_index",
                            idname = "idname",
                            gname = "pooled_wave",
                            xformla = ~ log_gdp,
                            data = g2_dat_never_gwh,
                            control_group = "nevertreated",
                            anticipation = 0,
                            bstrap = TRUE,
                            clustervars = "idname")
g2_es_never_gwh  <-  aggte(g2_att_never_gwh, type = "dynamic")
g2_df_never_gwh  <- data.frame(e = g2_es_never_gwh$egt, att = g2_es_never_gwh$att.egt, se = g2_es_never_gwh$se.egt, Wave = "W2")

g3_dat_never_gwh <- war_data[war_data$pooled_wave %in% c(0L, 21L), ]
g3_att_never_gwh <-  att_gt(yname = "renewables_Gwh",
                            tname = "quarter_index",
                            idname = "idname",
                            gname = "pooled_wave",
                            xformla = ~ log_gdp,
                            data = g3_dat_never_gwh,
                            control_group = "nevertreated",
                            anticipation = 0,
                            bstrap = TRUE,
                            clustervars = "idname")
g3_es_never_gwh  <-  aggte(g3_att_never_gwh, type = "dynamic")
g3_df_never_gwh  <- data.frame(e = g3_es_never_gwh$egt, att = g3_es_never_gwh$att.egt, se = g3_es_never_gwh$se.egt, Wave = "W3")


w_never_gwh <- rbind(g1_df_never_gwh, g2_df_never_gwh, g3_df_never_gwh)
w_never_gwh$lo <- w_never_gwh$att - 1.96 * w_never_gwh$se
w_never_gwh$hi <- w_never_gwh$att + 1.96 * w_never_gwh$se

# NOT-YET-TREATED GWH
g1_dat_nyt_gwh <- war_data[war_data$pooled_wave %in% c(0L, 18L), ]
g1_att_nyt_gwh <-  att_gt(yname = "renewables_Gwh",
                          tname = "quarter_index",
                          idname = "idname",
                          gname = "pooled_wave",
                          xformla = ~ log_gdp,
                          data = g1_dat_nyt_gwh,
                          control_group = "notyettreated",
                          anticipation = 0,
                          bstrap = TRUE,
                          clustervars = "idname")
g1_es_nyt_gwh  <-  aggte(g1_att_nyt_gwh, type = "dynamic")
g1_df_nyt_gwh  <- data.frame(e = g1_es_nyt_gwh$egt, att = g1_es_nyt_gwh$att.egt, se = g1_es_nyt_gwh$se.egt, Wave = "W1")

g2_dat_nyt_gwh <- war_data[war_data$pooled_wave %in% c(0L, 20L), ]
g2_att_nyt_gwh <-  att_gt(yname = "renewables_Gwh",
                          tname = "quarter_index",
                          idname = "idname",
                          gname = "pooled_wave",
                          xformla = ~ log_gdp,
                          data = g2_dat_nyt_gwh,
                          control_group = "notyettreated",
                          anticipation = 0,
                          bstrap = TRUE,
                          clustervars = "idname")
g2_es_nyt_gwh  <-  aggte(g2_att_nyt_gwh, type = "dynamic")
g2_df_nyt_gwh  <- data.frame(e = g2_es_nyt_gwh$egt, att = g2_es_nyt_gwh$att.egt, se = g2_es_nyt_gwh$se.egt, Wave = "W2")


g3_dat_nyt_gwh <- war_data[war_data$pooled_wave %in% c(0L, 21L), ]
g3_att_nyt_gwh <-  att_gt(yname = "renewables_Gwh",
                          tname = "quarter_index",
                          idname = "idname",
                          gname = "pooled_wave",
                          xformla = ~ log_gdp,
                          data = g3_dat_nyt_gwh,
                          ontrol_group = "notyettreated",
                          anticipation = 0,
                          bstrap = TRUE,
                          clustervars = "idname")
g3_es_nyt_gwh  <-  aggte(g3_att_nyt_gwh, type = "dynamic")
g3_df_nyt_gwh  <- data.frame(e = g3_es_nyt_gwh$egt, att = g3_es_nyt_gwh$att.egt, se = g3_es_nyt_gwh$se.egt, Wave = "W3")


w_nyt_gwh <- rbind(g1_df_nyt_gwh, g2_df_nyt_gwh, g3_df_nyt_gwh)
w_nyt_gwh$lo <- w_nyt_gwh$att - 1.96 * w_nyt_gwh$se
w_nyt_gwh$hi <- w_nyt_gwh$att + 1.96 * w_nyt_gwh$se

#plot

p_w_never_pct <- ggplot(w_never_pct, aes(x = e, y = att, color = Wave)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
  geom_line() + geom_point() +
  labs(x = "Quarters relative to first treatment", y = "ATT on renewables (%)",
       title = "Event-Time by Wave — % (Never-treated)") +
  theme_minimal()

p_w_never_pct

p_w_nyt_pct <- ggplot(w_nyt_pct, aes(x = e, y = att, color = Wave)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
  geom_line() + geom_point() +
  labs(x = "Quarters relative to first treatment", y = "ATT on renewables (%)",
       title = "Event-Time by Wave — % (Not-yet-treated)") +
  theme_minimal()

p_w_nyt_pct

p_w_never_gwh <- ggplot(w_never_gwh, aes(x = e, y = att, color = Wave)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
  geom_line() + geom_point() +
  labs(x = "Quarters relative to first treatment", y = "ATT on renewables (GWh)",
       title = "Event-Time by Wave — GWh (Never-treated)") +
  theme_minimal()

p_w_never_gwh

p_w_nyt_gwh <- ggplot(w_nyt_gwh, aes(x = e, y = att, color = Wave)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0) +
  geom_line() + geom_point() +
  labs(x = "Quarters relative to first treatment", y = "ATT on renewables (GWh)",
       title = "Event-Time by Wave — GWh (Not-yet-treated)") +
  theme_minimal()

p_w_nyt_gwh


t_w1_never_pct <- aggte(w1_att_never_pct, type = "simple")
t_w2_never_pct <- aggte(w2_att_never_pct, type = "simple")
t_w3_never_pct <- aggte(w3_att_never_pct, type = "simple")


t_w1_nyt_pct <- aggte(w1_att_nyt_pct, type = "simple")
t_w2_nyt_pct <- aggte(w2_att_nyt_pct, type = "simple")
t_w3_nyt_pct <- aggte(w3_att_nyt_pct, type = "simple")


t7_pct <- data.frame(
  Wave        = c("W1","W2","W3"),
  ATT_never   = c(t_w1_never_pct$overall.att, t_w2_never_pct$overall.att, t_w3_never_pct$overall.att),
  SE_never    = c(t_w1_never_pct$overall.se,  t_w2_never_pct$overall.se,  t_w3_never_pct$overall.se),
  ATT_nyt     = c(t_w1_nyt_pct$overall.att,   t_w2_nyt_pct$overall.att,   t_w3_nyt_pct$overall.att),
  SE_nyt      = c(t_w1_nyt_pct$overall.se,    t_w2_nyt_pct$overall.se,    t_w3_nyt_pct$overall.se)
)

t7_pct


t_w1_never_gwh <- aggte(g1_att_never_gwh, type = "simple")
t_w2_never_gwh <- aggte(g2_att_never_gwh, type = "simple")
t_w3_never_gwh <- aggte(g3_att_never_gwh, type = "simple")


t_w1_nyt_gwh <- aggte(g1_att_nyt_gwh, type = "simple")
t_w2_nyt_gwh <- aggte(g2_att_nyt_gwh, type = "simple")
t_w3_nyt_gwh <- aggte(g3_att_nyt_gwh, type = "simple")

t7_gwh <- data.frame(
  Wave        = c("W1","W2","W3"),
  ATT_never   = c(t_w1_never_gwh$overall.att, t_w2_never_gwh$overall.att, t_w3_never_gwh$overall.att),
  SE_never    = c(t_w1_never_gwh$overall.se,  t_w2_never_gwh$overall.se,  t_w3_never_gwh$overall.se),
  ATT_nyt     = c(t_w1_nyt_gwh$overall.att,   t_w2_nyt_gwh$overall.att,   t_w3_nyt_gwh$overall.att),
  SE_nyt      = c(t_w1_nyt_gwh$overall.se,    t_w2_nyt_gwh$overall.se,    t_w3_nyt_gwh$overall.se)
)

t7_gwh


# ----------------------- GREEN DEAL PLACEBO (2018Q1–2021Q4) --------------

# Same treated set, forced treat date 2020Q1 (20201)
gd_data <- analysis_data_cs %>%
  filter(quarter_index >= 1L & quarter_index <= 16L) %>%
  mutate(placebo_wave = ifelse(pooled_wave == 0L, 0L, 9L)) %>%
  group_by(idname) %>%
  mutate(first_obs_q = min(quarter_index), gval = unique(placebo_wave)) %>%
  ungroup() %>%
  filter(!(gval > 0L & gval <= first_obs_q)) %>%
  select(-first_obs_q, -gval)

# % nevertreated
att_gd_never <- att_gt(
  yname   = "renewables",
  tname   = "quarter_index",
  idname  = "idname",
  gname   = "placebo_wave",
  xformla = ~ log_gdp,
  data    = gd_data,
  control_group = "nevertreated",
  anticipation  = 0,
  bstrap   = TRUE,
  clustervars = "idname"
)
summary(att_gd_never)  

# % nyt
att_gd_nyt <- att_gt(
  yname   = "renewables",
  tname   = "quarter_index",
  idname  = "idname",
  gname   = "placebo_wave",
  xformla = ~ log_gdp,
  data    = gd_data,
  control_group = "notyettreated",
  anticipation  = 0,
  bstrap   = TRUE,
  clustervars = "idname"
)
summary(att_gd_nyt) 

# Gwh nevertreated
att_gd_never_Gwh <- att_gt(
  yname   = "renewables_Gwh",
  tname   = "quarter_index",
  idname  = "idname",
  gname   = "placebo_wave",
  xformla = ~ log_gdp,
  data    = gd_data,
  control_group = "nevertreated",
  anticipation  = 0,
  bstrap   = TRUE,
  clustervars = "idname"
)
summary(att_gd_never_Gwh) 

# Gwh nyt
att_gd_nyt_Gwh <- att_gt(
  yname   = "renewables_Gwh",
  tname   = "quarter_index",
  idname  = "idname",
  gname   = "placebo_wave",
  xformla = ~ log_gdp,
  data    = gd_data,
  control_group = "notyettreated",
  anticipation  = 0,
  bstrap   = TRUE,
  clustervars = "idname"
)
summary(att_gd_never_Gwh) 

es_gd_never <- aggte(att_gd_never, type = "dynamic")
es_gd_nyt   <- aggte(att_gd_nyt,   type = "dynamic")
es_gd_never_Gwh <- aggte(att_gd_never_Gwh, type = "dynamic")
es_gd_nyt_Gwh   <- aggte(att_gd_nyt_Gwh,   type = "dynamic")

ov_gd_never      <- aggte(att_gd_never,      type = "simple")
ov_gd_nyt        <- aggte(att_gd_nyt,        type = "simple")
ov_gd_never_gwh  <- aggte(att_gd_never_Gwh,  type = "simple")
ov_gd_nyt_gwh    <- aggte(att_gd_nyt_Gwh,    type = "simple")

T8 <- tibble(
  control_group = c("nevertreated", "notyettreated", "nevertreated", "notyettreated"),
  outcome       = c("Renewables (%)", "Renewables (%)", "Renewables (GWh)", "Renewables (GWh)"),
  controls      = "+ log_gdp",
  est           = c(ov_gd_never$overall.att, ov_gd_nyt$overall.att,
                    ov_gd_never_gwh$overall.att, ov_gd_nyt_gwh$overall.att),
  se            = c(ov_gd_never$overall.se,  ov_gd_nyt$overall.se,
                    ov_gd_never_gwh$overall.se, ov_gd_nyt_gwh$overall.se)
) %>%
  mutate(
    ci_low  = est - 1.96 * se,
    ci_high = est + 1.96 * se
  ) %>%
  mutate(across(c(est, se, ci_low, ci_high), ~round(.x, 3)))

print(T8)

#---------------------------------PLOTS-----------------------------------------

# Plot % nevertreated
never_gd_pct_df <- data.frame(
  event_time = es_gd_never$egt,
  att        = es_gd_never$att.egt,
  se         = es_gd_never$se.egt
) %>%
  mutate(
    ci_lower = att - qnorm(0.975) * se,
    ci_upper = att + qnorm(0.975) * se
  )

ggdid(es_gd_never)+
  theme_minimal() +
  labs(
    title = "Event-Study: Treatment Effects on Renewables (%)",
    subtitle = "Green Deal - Never-treated",
    x = "Quarters relative to treatment",
    y = "ATT estimate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Plot % nyt
nyt_gd_pct_df <- data.frame(
  event_time = es_gd_nyt$egt,
  att        = es_gd_nyt$att.egt,
  se         = es_gd_nyt$se.egt
) %>%
  mutate(
    ci_lower = att - qnorm(0.975) * se,
    ci_upper = att + qnorm(0.975) * se
  )

ggdid(es_gd_nyt)+
  theme_minimal() +
  labs(
    title = "Event-Study: Treatment Effects on Renewables (%)",
    subtitle = "Green Deal - Not-yet-treated",
    x = "Quarters relative to treatment",
    y = "ATT estimate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Plot Gwh nevertreated
never_gd_gwh_df <- data.frame(
  event_time = es_gd_never_Gwh$egt,
  att        = es_gd_never_Gwh$att.egt,
  se         = es_gd_never_Gwh$se.egt
) %>%
  mutate(
    ci_lower = att - qnorm(0.975) * se,
    ci_upper = att + qnorm(0.975) * se
  )

ggdid(es_gd_never_Gwh)+
  theme_minimal() +
  labs(
    title = "Event-Study: Treatment Effects on Renewables (Gwh)",
    subtitle = "Green Deal - Never-treated",
    x = "Quarters relative to treatment",
    y = "ATT estimate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )

# Plot Gwh nyt
nyt_gd_gwh_df <- data.frame(
  event_time = es_gd_nyt_Gwh$egt,
  att        = es_gd_nyt_Gwh$att.egt,
  se         = es_gd_nyt_Gwh$se.egt
) %>%
  mutate(
    ci_lower = att - qnorm(0.975) * se,
    ci_upper = att + qnorm(0.975) * se
  )

ggdid(es_gd_nyt_Gwh)+
  theme_minimal() +
  labs(
    title = "Event-Study: Treatment Effects on Renewables (Gwh)",
    subtitle = "Green Deal - Not-yet-treated",
    x = "Quarters relative to treatment",
    y = "ATT estimate"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )


#-------------------------------------------------------------------------------
#             Overall Data  Descriptive plots and tables
#-------------------------------------------------------------------------------

fig1_data <- year_month_merged %>%
  distinct(country, pre_dep_share)

ggplot(year_month_merged, aes(x = reorder(country, pre_dep_share), y = pre_dep_share ,colour = country, fill = country)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Avg. Russian dependence, 2019–21 (pp)")+
  theme(legend.position = "none")+
  theme_minimal()

bins <- year_month_merged %>%
  distinct(country, pre_dep_share) %>%
  mutate(bin = case_when(
    pre_dep_share <= 10 ~ "Low (≤10pp)",
    pre_dep_share > 40  ~ "High (>40pp)",
    TRUE ~ NA_character_
  ))

traj <- year_month_merged %>%
  left_join(bins, by = "country") %>%
  filter(!is.na(bin)) %>%
  group_by(date, bin) %>%
  summarise(mean_pc = mean(Total_Renewables_PC, na.rm = TRUE), .groups = "drop")

ggplot(traj, aes(x = date, y = mean_pc, color = bin)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2022-03-01")) +
  labs(x = NULL, y = "Renewables (%)",
       colour = "Dependence level", fill = "Dependence level")+
  theme_minimal()


traj_gwh <- year_month_merged %>%
  left_join(bins, by = "country") %>%
  filter(!is.na(bin)) %>%
  group_by(date, bin) %>%
  summarise(mean_pc = mean(Total_Renewables_GWH, na.rm = TRUE), .groups = "drop")

ggplot(traj_gwh, aes(x = date, y = mean_pc, color = bin)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2022-03-01")) +
  labs(x = NULL, y = "Renewables (GWh)",
       colour = "Dependence level", fill = "Dependence level")+
  theme_minimal()


vars <- c("Total_Renewables_PC","Total_Renewables_GWH","pre_dep_share",
          "log_gdp_pc_pps","log_energy_pc_mwh",
          "gas_stock_mcm",
          "population")

make_stats <- function(df, vars) {
  df %>%
    summarise(across(all_of(vars),
                     list(mean = ~round(mean(.x, na.rm = TRUE), 2),
                          sd   = ~round(sd(.x,   na.rm = TRUE), 2),
                          min  = ~round(min(.x,  na.rm = TRUE), 2),
                          max  = ~round(max(.x,  na.rm = TRUE), 2),
                          N    = ~sum(!is.na(.x))),
                     .names = "{.col}_{.fn}")) %>%
    pivot_longer(everything(),
                 names_to = c("variable","stat"),
                 names_pattern = "^(.*)_(mean|sd|min|max|N)$",
                 values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value)
}

# Overall
t4_overall <- make_stats(year_month_merged, vars)
write_csv(t4_overall, "tables/T4_overall.csv")

# Pre
t4_pre <- make_stats(filter(year_month_merged, tname < 202203), vars)
write_csv(t4_pre, "tables/T4_pre.csv")

# Post 
t4_post <- make_stats(filter(year_month_merged, tname >= 202203), vars)
write_csv(t4_post, "tables/T4_post.csv")


t5 <- analysis_data_cs %>%
  distinct(country, pooled_wave) %>%
  left_join(year_month_merged %>% distinct(country, pre_dep_share), by = "country") %>%
  mutate(wave = recode(pooled_wave,
                              `18` = "W1=18",
                              `20` = "W2=20",
                              `21` = "W3=21",
                              `0`  = "Never",
                              .default = as.character(pooled_wave))) %>%
  group_by(pooled_wave, wave) %>%
  summarise(
    n_countries    = n(),
    mean_pre_dep   = mean(pre_dep_share, na.rm = TRUE),
    median_pre_dep = median(pre_dep_share, na.rm = TRUE),
    iqr_pre_dep    = IQR(pre_dep_share, na.rm = TRUE),
    countries      = paste(sort(country), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(pooled_wave)


