################################################################################
##### Purpose: benefit-cost analysis of PV, BESS, & EV under NEM2 vs NBT   #####
##### Author: Ambar Nag | axnag@ucdavis.edu                                #####
##### Dependencies: definitions.R                                          #####
################################################################################

options(scipen = 10)
options(max.print = 100)
setwd("D:/UC Davis/coursework/4_2025 Fall/EGG 200/final paper/analysis")

library(tidyverse)
library(lubridate)
library(FinCal)
source(paste("code", "definitions.R", sep = "/"))

##### Step 0: read input data and initialize configurable parameters #####

param_pv_factor = 1.0     # PV system size multiplier (baseline = 5 kWdc)
param_bess_capacity = 12  # battery storage capacity in kWh
param_ev_capacity = 40    # EV battery capacity in kWh
param_ev_miles = 30       # daily average EV miles driven
param_ev_mpg = 3          # EV miles per kWh

# customer load from CPUC's NBT PD Model v6 | NBT FiT from PGE website
# PV solar output for a SF home modeled using NREL SAM
data = read.csv(paste("csv_in", "input_data_residential.csv", sep = "/"))

# PGE rate plans from PGE website
pge_rate_plans = read.csv(paste("csv_in", "pge_rate_plans.csv", sep = "/"))


##### Step 1: setup base model for a home running PV only #####

# create date/time variables, calculate net demand
data = data %>%
  mutate(
    date_time = make_datetime(year, month, day, hour),
    weekday = ifelse(wday(date_time) %in% c(1, 7), "weekend", "weekday"),
    season = ifelse(between(month, 6, 9), "summer", "winter"),
    tou_base = ifelse(between(hour, 16, 20), "peak", "off-peak"),
    pv_output = param_pv_factor * pv_output,
    net_demand_pv = customer_load - pv_output
    )

# PGE ToU rate plans for homes wo EV
plans = pge_rate_plans %>%
  filter(rate_plan == "E-TOU-C") %>%
  select(-rate_plan)

# merge ToU rate plans into data
by = join_by(season, tou_base == tou)
data = data %>%
  inner_join(plans, by) %>%
  rename(pge_rate_base = pge_rate) %>%
  # calculate electricity cost for (1) No PV (2) PV under NEM2 (3) PV under NBT
  mutate(cost_base = customer_load * pge_rate_base,
         cost_pv_nem2 = net_demand_pv * pge_rate_base,
         cost_pv_nbt = ifelse(net_demand_pv > 0, net_demand_pv*pge_rate_base, net_demand_pv*fit_nbt))

##### Step 2: introduce battery storage into the model #####

# calculate net demand for PV+BESS system
nd_vec = data$net_demand_pv
capacity = param_bess_capacity

res = calc_nd_bess(nd_vec = nd_vec, capacity = capacity)
data$soc_bess = res$soc
data$net_demand_bess = res$net_demand

# calculate electricity cost under NEM2 & NEM3 for a home running PV+BESS
data = data %>%
  mutate(
    cost_bess_nem2 = net_demand_bess * pge_rate_base,
    cost_bess_nbt = ifelse(net_demand_bess > 0, net_demand_bess*pge_rate_base, net_demand_bess*fit_nbt)
    )

##### Step 3: introduce bidirectional EV charging into the model #####

# PGE ToU rate plans for homes with EV
plans = pge_rate_plans %>%
  filter(rate_plan == "EV2-A") %>%
  select(-rate_plan)


# define peak, part-peak, off-peak for EV rate plan
data = data %>%
  mutate(
    tou_ev = case_when(
      between(hour, 16, 20) ~ "peak",
      between(hour, 21, 23) ~ "part-peak",
      hour == 15 ~ "part-peak",
      TRUE ~ "off-peak"
      ))

# merge in EV rate plans > define flags to guide EV charging/discharging
by = join_by(season, tou_ev == tou)
data = data %>%
  inner_join(plans, by) %>%
  rename(pge_rate_ev = pge_rate) %>%
  mutate(
    ev_off_peak = ifelse(tou_ev == "off-peak", 1, 0),
    # EV is away 9am-5pm on weekdays and 5pm-9pm on weekends
    ev_at_home = case_when(
      weekday == "weekday" & between(hour, 9, 16) ~ 0,
      weekday == "weekend" & between(hour, 17, 20) ~ 0,
      TRUE ~ 1
    ))

# calculate net demand for PV+BESS+EV system
nd_vec = data$net_demand_bess
capacity = param_ev_capacity
ev_at_home = data$ev_at_home
ev_off_peak =  data$ev_off_peak
# ev_load = param_ev_capacity / 8 # L2 charger takes upto 8 hours to fully charge EV
ev_drive = (param_ev_miles/param_ev_mpg) / 8 # average hourly discharge when EV driving  

# apply heuristic to derive SoC and net demand with EV
res = calc_nd_ev(nd_vec = nd_vec, 
                 capacity = capacity, 
                 ev_at_home = ev_at_home, 
                 ev_off_peak = ev_off_peak,
                 ev_drive = ev_drive)

data$soc_ev = res$soc
data$delta_soc_ev = res$delta_soc
data$net_demand_ev = res$net_demand

# calculate electricity cost under NEM2 & NEM3 for a home running PV+BESS+EV
data = data %>%
  mutate(
    cost_ev_nem2 = net_demand_ev * pge_rate_ev,
    cost_ev_nbt = ifelse(net_demand_ev > 0, net_demand_ev*pge_rate_ev, net_demand_ev*fit_nbt)
    )

##### Step 4: economic benefit-cost analysis #####
### 4.0 No PV
grid_imp_base = sum(data$customer_load)
grid_exp_base = 0
total_cost_base = sum(data$customer_load*data$pge_rate_base)

### 4.1 PV solar only
# initial investment
pv_unit_cost = 3.5 # $/Wdc
pv_system_size = 5 # kWdc
investment_pv = 1000 * pv_unit_cost * pv_system_size

# reduced load drawn from grid
grid_imp_pv = sum(sapply(data$net_demand_pv, function(x) max(x, 0))) # sum of +ve net demand
grid_exp_pv = -sum(sapply(data$net_demand_pv, function(x) min(x, 0))) # sum of -ve net demand
demand_red_pv = round(100*(grid_imp_base - grid_imp_pv)/grid_imp_base, 1) # % reduction from base demand

# cost savings under NEM2 & NBT
total_cost_pv_nem2 = sum(data$cost_pv_nem2)
total_cost_pv_nbt = sum(data$cost_pv_nbt)
savings_pv_nem2 = round(total_cost_base - total_cost_pv_nem2, 0)
savings_pv_nbt = round(total_cost_base - total_cost_pv_nbt, 0)

# payback and ROI under NEM2 & NBT
payback_pv_nem2 = round(investment_pv / savings_pv_nem2, 1)
payback_pv_nbt = round(investment_pv / savings_pv_nbt, 1)
cf_pv_nem2 = c(-investment_pv, rep(savings_pv_nem2, 20)) # 20-yr cash flow under NEM2
roi20_pv_nem2 = round(100*irr(cf_pv_nem2), 1)
cf_pv_nbt = c(-investment_pv, rep(savings_pv_nbt, 20)) # 20-yr cash flow under NBT
roi20_pv_nbt = round(100*irr(cf_pv_nbt), 1)

### 4.2 Add BESS
# cumulative investment
bess_unit_cost = 1000 # $/kWh
bess_cost = bess_unit_cost * param_bess_capacity
investment_bess = investment_pv + bess_cost

# reduced load drawn from grid
grid_imp_bess = sum(sapply(data$net_demand_bess, function(x) max(x, 0)))
grid_exp_bess = -sum(sapply(data$net_demand_bess, function(x) min(x, 0)))
demand_red_bess = round(100*(grid_imp_base - grid_imp_bess)/grid_imp_base, 1)

# cost savings under NEM2 & NBT
total_cost_bess_nem2 = sum(data$cost_bess_nem2)
total_cost_bess_nbt = sum(data$cost_bess_nbt)
savings_bess_nem2 = round(total_cost_base - total_cost_bess_nem2, 0)
savings_bess_nbt = round(total_cost_base - total_cost_bess_nbt, 0)

# payback and ROI under NEM2 & NBT
payback_bess_nem2 = round(investment_bess / savings_bess_nem2, 1)
payback_bess_nbt = round(investment_bess / savings_bess_nbt, 1)
cf_bess_nem2 = c(-investment_bess, rep(savings_bess_nem2, 20)) 
roi20_bess_nem2 = round(100*irr(cf_bess_nem2), 1)
cf_bess_nbt = c(-investment_bess, rep(savings_bess_nbt, 20)) 
roi20_bess_nbt = round(100*irr(cf_bess_nbt), 1)

### 4.3 Add bidirectional EV
# Read in python optimization output for NBT and NEM2
opt_nbt = read.csv(paste("csv_out", "py_optim_output_NBT.csv", sep = "/"))
opt_nem2 = read.csv(paste("csv_out", "py_optim_output_NEM2.csv", sep = "/"))

# cumulative investment
ev_cost = 2500 # cost of L2 charger, not the EV
investment_ev = investment_bess + ev_cost

# demand reduction & cost savings under NEM2
grid_imp_ev_nem2 = sum(opt_nem2$grid_import)
grid_exp_ev_nem2 = sum(opt_nem2$grid_export)
demand_red_ev_nem2 = round(100*(grid_imp_base - grid_imp_ev_nem2)/grid_imp_base, 1)
total_cost_ev_nem2 = sum(opt_nem2$cost_nem2)
savings_ev_nem2 = round(total_cost_base - total_cost_ev_nem2, 0)

# demand reduction & cost savings under NBT
grid_imp_ev_nbt = sum(opt_nbt$grid_import)
grid_exp_ev_nbt = sum(opt_nbt$grid_export)
demand_red_ev_nbt = round(100*(grid_imp_base - grid_imp_ev_nbt)/grid_imp_base, 1)
total_cost_ev_nbt = sum(opt_nbt$cost_nbt)
savings_ev_nbt = round(total_cost_base - total_cost_ev_nbt, 0)

# payback and ROI under NEM2 & NBT
payback_ev_nem2 = round(investment_ev / savings_ev_nem2, 1)
payback_ev_nbt = round(investment_ev / savings_ev_nbt, 1)
cf_ev_nem2 = c(-investment_ev, rep(savings_ev_nem2, 20)) 
roi20_ev_nem2 = round(100*irr(cf_ev_nem2), 1)
cf_ev_nbt = c(-investment_ev, rep(savings_ev_nbt, 20)) 
roi20_ev_nbt = round(100*irr(cf_ev_nbt), 1)

cba_df = data.frame(
  # no PV
  grid_imp_base, grid_exp_base, total_cost_base,
  
  # PV only
  grid_imp_pv, grid_exp_pv, demand_red_pv, 
  investment_pv, total_cost_pv_nem2, total_cost_pv_nbt, savings_pv_nem2, savings_pv_nbt, 
  payback_pv_nem2, payback_pv_nbt, roi20_pv_nem2, roi20_pv_nbt,
  
  # PV+BESS
  grid_imp_bess, grid_exp_bess, demand_red_bess, 
  investment_bess, total_cost_bess_nem2, total_cost_bess_nbt, savings_bess_nem2, savings_bess_nbt, 
  payback_bess_nem2, payback_bess_nbt, roi20_bess_nem2, roi20_bess_nbt,
  
  # PV+BESS+EV
  grid_imp_ev_nem2, grid_exp_ev_nem2, demand_red_ev_nem2, 
  grid_imp_ev_nbt, grid_exp_ev_nbt, demand_red_ev_nbt, 
  investment_ev, total_cost_ev_nem2, total_cost_ev_nbt, savings_ev_nem2, savings_ev_nbt, 
  payback_ev_nem2, payback_ev_nbt, roi20_ev_nem2, roi20_ev_nbt
)

# export to CSV
out_file = "cba_model_output.csv"
write.csv(cba_df, paste("csv_out", out_file, sep = "/"), row.names=F)
