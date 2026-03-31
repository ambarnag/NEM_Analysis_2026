library(tidyverse)

in_dir = "D:/UC Davis/coursework/4_2025 Fall/EGG 200/final paper/analysis/csv_in"
out_dir = "D:/UC Davis/coursework/4_2025 Fall/EGG 200/final paper/analysis/csv_out"
pge_file = "applications_pge_5_year.csv"
sce_file = "applications_sce_5_year.csv"
sdge_file = "applications_sdge_5_year.csv"

# read 5 years' data for all 3 CA IOUs and append
pge_data = read.csv(paste(in_dir, pge_file, sep = "/"))
sce_data = read.csv(paste(in_dir, sce_file, sep = "/"))
sdge_data = read.csv(paste(in_dir, sdge_file, sep = "/"))
iou_data = rbind(pge_data, sce_data, sdge_data)
rm(pge_data, sce_data, sdge_data) # free up some memory

# clean up column names
colnames(iou_data) = gsub("\\.", "_", colnames(iou_data))

# filter for Residential PV NEM2 & NBT interconnections > flag presence of BESS and EV
# filter for 2 yrs pre & 2 yrs post Apr-2023 (NEM3 eff. date)
data = iou_data %>%
  select(Application_Status, Utility, Service_County, Technology_Type, System_Size_DC, Storage_Capacity__kWh_, 
         Customer_Sector, App_Approved_Date, Third_Party_Owned, Third_Party_Owned_Type,
         Electric_Vehicle, Electric_Vehicle_Count, Total_System_Cost, NEM_Tariff) %>%
  # misc clean-up
  mutate(
    Service_County = str_to_title(Service_County),
    Third_Party_Owned_Type = toupper(Third_Party_Owned_Type),
    App_Approved_Date = as.Date(App_Approved_Date, format = "%Y-%m-%d"),
    App_Approved_Year = year(App_Approved_Date)
    ) %>%
  filter(Application_Status == "Interconnected",
         App_Approved_Year >= 2021,
         grepl("Photovoltaic|PV", Technology_Type),
         Customer_Sector == "Residential",
         NEM_Tariff %in% c("2.0", "NBT")) %>%
  mutate(
    NEM_Tariff = ifelse(NEM_Tariff == "2.0", "NEM2", "NBT"),
    Battery_Storage = ifelse(grepl("Battery", Technology_Type), "PV_Storage", "PV_Only"),
    EV_Present = ifelse(Electric_Vehicle == "Yes", "EV", "No EV"),
    PV_Size_Class = case_when(
      between(System_Size_DC, 1, 5) ~ "Small (1-5kW)",
      between(System_Size_DC, 5, 10) ~ "Medium (5-10kW)",
      TRUE ~ "Large (10+kW)"
    ),
    Own_vs_Lease = case_when(
      Third_Party_Owned_Type %in% c("LEASE", "PRE-PAID LEASE", "OTHER") ~ "Lease",
      Third_Party_Owned_Type == "PPA" ~ "PPA",
      TRUE ~ "Own"
    ))

# returns a bar chart stacked along "fill" with x=Year, y=interconnections, split=NEM Tariff
# side-effect: prints the aggregated df used for plotting
stacked_bar_chart = function(fill, title, ann_lbl=NULL) {
  plot_data = data %>%
    group_by(App_Approved_Year, NEM_Tariff, !!sym(fill)) %>%
    summarise(count = n())
  print(plot_data, n=Inf, row.names = F)
    
  plot = ggplot(plot_data, aes(x = App_Approved_Year, y = count, fill = !!sym(fill))) +
    geom_col(position = position_stack(), width = 0.6) +
    scale_fill_discrete() +
    facet_grid(. ~ factor(NEM_Tariff, levels = c("NEM2", "NBT")) , switch = "x") +
    scale_y_continuous(labels = scales::comma, breaks = scales::pretty_breaks(n = 6)) +
    labs(
      title = title,
      x = "Year",
      y = "Residential PV Interconnections",
      fill = fill
    ) +
    # theme_minimal(base_size = 10) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4),
      panel.spacing = unit(1, "lines"),
      strip.placement = "outside",
      strip.background = element_blank(),
      strip.text = element_text(size = 14),
      axis.text = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_blank()
    )
  
  return(plot)
}

# Figure 1: has the proportion of interconnections with BESS increased in NBT compared to NEM2?
pv_only_vs_batt = stacked_bar_chart(
  fill = "Battery_Storage", 
  title = "Annual Interconnections by NEM Tariff: Battery Storage"
  )

ggsave(filename = paste(out_dir, "pv_only_vs_batt.png", sep = "/"),
       plot =  pv_only_vs_batt,
       width = 10,
       height = 5,
       dpi = 300)

# Figure 2: is there a shift towards smaller system sizes in NBT compared to NEM2?
system_size = stacked_bar_chart(
  fill = "PV_Size_Class", 
  title = "Annual Interconnections by NEM Tariff: PV System Size"
  )

ggsave(filename = paste(out_dir, "system_size.png", sep = "/"),
       plot =  system_size,
       width = 10,
       height = 5,
       dpi = 300)

# median system size NEM2 vs NBT
data %>% 
  group_by(NEM_Tariff) %>% 
  summarise(avg_ss = median(System_Size_DC))

# Figure 3: has the proportion of interconnections on lease/PPA increased in NBT compared to NEM2?
own_vs_lease = stacked_bar_chart(
  fill = "Own_vs_Lease", 
  title = "Annual Interconnections by NEM Tariff: Own vs Lease or PPA"
  )

ggsave(filename = paste(out_dir, "own_vs_lease.png", sep = "/"),
       plot =  own_vs_lease,
       width = 10,
       height = 5,
       dpi = 300)

# Figure 4: has the proportion of interconnections with EV increased in NBT compared to NEM2?
ev_vs_no_ev = stacked_bar_chart(
  fill = "EV_Present", 
  title = "Annual Interconnections by NEM Tariff: Electric Vehicle"
  )

ggsave(filename = paste(out_dir, "ev_vs_no_ev.png", sep = "/"),
       plot =  ev_vs_no_ev,
       width = 10,
       height = 5,
       dpi = 300)
