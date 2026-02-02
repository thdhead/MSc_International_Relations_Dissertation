# MSc_International_Relations_Dissertation

> **Script:** `Diss_script_final.R`

This repository contains a single end‑to‑end R script that:
- builds a **harmonised Eurostat panel dataset** (monthly + annual)
- constructs **pre‑war Russian gas dependence** measures
- runs multiple **difference‑in‑differences (DiD)** designs (TWFE + event studies + Callaway & Sant’Anna)
- exports **analysis-ready CSVs** and **descriptive tables**

---

## Table of contents
- [Overview](#overview)
- [Data inputs](#data-inputs)
- [Outputs](#outputs)
- [What the script builds](#what-the-script-builds)
- [Treatment and identification](#treatment-and-identification)
- [Models estimated](#models-estimated)
- [How to run](#how-to-run)

---

## Overview
The core empirical idea is to test whether countries that were **more dependent on Russian gas imports before the war** saw different post‑March‑2022 patterns in **renewables** (share and generation), controlling for country and time fixed effects and a set of time‑varying controls.

The workflow is fully contained in `Diss_script_final.R` and is designed to run **top‑to‑bottom**.

---

## Data inputs
Place these files in the **project root** (same folder as `Diss_script_final.R`). The script reads them directly by name.

### Monthly inputs
- `estat_nrg_cb_pem_filtered_PC.tsv` — renewables (percent)
- `estat_nrg_cb_pem_filtered_GWH.tsv` — renewables (GWh)
- `estat_nrg_ti_gasm_filtered_Total.tsv` — total gas imports (NG + LNG)
- `estat_nrg_ti_gasm_filtered_Russia.tsv` — gas imports from Russia (NG + LNG)
- `estat_nrg_cb_gasm_filtered_cons.tsv` — gas consumption
- `estat_nrg_cb_sffm_filtered.tsv` — coal consumption
- `estat_nrg_stk_gasm_filtered.tsv` — gas stocks / storage

### Annual inputs
- `estat_demo_pjan_filtered.tsv` — population
- `estat_nama_10_pc_filtered.tsv` — GDP per capita (PPS)
- `estat_nrg_bal_c_filtered.tsv` — energy balance (used for fossil share + energy consumption)

> Note: these are **pre‑filtered Eurostat TSV extracts**. The script assumes a specific Eurostat bulk TSV structure (first column with comma-separated identifiers; time columns in wide format).

---

## Outputs
### CSV datasets
- `merged_monthly_energy_dataset.csv`
- `merged_annual_data.csv`

---

## What the script builds
### 1) Cleaning + reshaping helpers
The script defines helper functions to:
- standardise **monthly** date columns into real `Date` values
- standardise **annual** columns (`YYYY`) into `Date`
- reshape Eurostat wide tables to long format
- convert Eurostat missing `":"` to `NA`

**Cyprus (`CY`) is excluded** in the cleaning functions.

### 2) Monthly energy panel
From the monthly inputs, the script constructs (by country × month):

**Renewables**
- component series (hydro, geothermal, wind, solar, other)
- totals:
  - `Total_Renewables_PC` (percent)
  - `Total_Renewables_GWH` (GWh)

**Gas imports + dependence**
- total imports (NG + LNG) → `agg_total_imports`
- Russian imports (NG + LNG) → `agg_RU_imports`
- dependence measures:
  - `Russia_dependency_pct` (RU share of total imports, in %)
  - `pre_dep_share` (pre‑war average RU import share; see below)

**Other monthly controls**
- gas consumption
- coal consumption
- gas stocks / storage

### 3) Annual controls panel
From annual inputs, the script constructs (by country × year):
- `population`
- `gdp_pc_pps`
- `fossil_share` (share of fossil fuels in the energy mix)
- `energy_per_capita_mwh`

The monthly panel is then merged with annual controls via year.

---

## Treatment and identification
### Pre‑war dependence (continuous exposure)
The script computes **pre‑war Russian import dependence** as the **average RU share of total gas imports** using months **before 2022‑03‑01**.

This measure is used as a continuous “exposure” in TWFE DiD (interacted with a post period dummy).

### Post period
For TWFE models, `post_t` is defined as:
- `post_t = 1` for months `>= 2022-03-01`
- `post_t = 0` otherwise

### Treatment timing for Callaway & Sant’Anna (staggered adoption)
To define a treatment date (first treated period) the script uses a sustained “cut‑off” rule:
- **threshold (`tau`)**: 0.10 (10%)
- **duration (`K`)**: 3 consecutive months
- **earliest possible treatment**: `2022-03-01`

A country becomes treated at the first month where its current RU import share is **≤ 10% for 3 consecutive months**, conditional on having **meaningful pre‑war dependence**.

Once treated, it stays treated.

---

## Models estimated
### A) TWFE continuous DiD (fixest)
Using `fixest::feols()` with **country and date fixed effects**, clustered at the country level:
- outcome ~ `pre_dep_share × post_t`
- variants with controls:
  - gas stocks
  - fossil share
  - log energy per capita
  - log GDP per capita (PPS)
  - population

Outcomes:
- `Total_Renewables_PC`
- `Total_Renewables_GWH`

### B) Event-study style dynamics
- dynamic paths around March 2022 using relative month indicators
- versions with continuous exposure and with binned exposure categories

### C) Callaway & Sant’Anna DiD (did)
Using the `did` package:
- converts monthly data to **quarterly**
- estimates ATT under both:
  - `control_group = "nevertreated"`
  - `control_group = "notyettreated"`
- produces cohort/wave and overall summaries + event-study plots

### D) Placebo window
A placebo exercise is run over **2018Q1–2021Q4** with a forced placebo treatment at **2020Q1**.

---

## How to run
### Requirements
- R (recommended: recent 4.x)
- RStudio (2025.05.1 Build 513)
- Packages used in the script:
  - `tidyverse`, `lubridate`, `slider`, `ggplot2`
  - `fixest`, `did`, `marginaleffects`
  - `future`, `future.apply`, `parallelly`, `recipes`

### Install packages (example)
```r
install.packages(c(
  "tidyverse","lubridate","slider","ggplot2",
  "fixest","did","marginaleffects",
  "future","future.apply","parallelly","recipes"
))
```

### Run
1. Put all required `.tsv` inputs in the project root.
2. Create the output folder:
```bash
mkdir -p tables
```
3. Run the script:
```bash
Rscript Diss_script_final.R
```


