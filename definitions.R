################################################################################
##### Purpose: define functions for financial benefit-cost analysis        #####
##### Author: Ambar Nag | axnag@ucdavis.edu                                #####
##### Dependencies: None                                                   #####
################################################################################

# returns battery SoC and recalculated ND based on initial ND and BESS capacity
# SoC: state of charge | ND: net demand
calc_nd_bess = function(nd_vec, capacity) {
  soc_init = capacity/2 # battery starts out at 50% of capacity
  
  # initialize empty vectors
  soc_nb = numeric(8760) # battery SoC with no bounds
  soc_lb = numeric(8760) # battery SoC with lower bound
  soc_bb = numeric(8760) # battery SoC with lower & upper bounds
  net_demand = numeric(8760) # recalculated net demand
  
  # calculate initial SoC and ND values
  soc_nb[1] = soc_init - nd_vec[1]
  soc_lb[1] = max(soc_nb[1], 0)
  soc_bb[1] = min(soc_lb[1], capacity)
  net_demand[1] = nd_vec[1] - (soc_init - soc_bb[1])
  
  # fill in remaining SoC and ND values
  for (i in 2:8760) {
    soc_nb[i] = soc_bb[i-1] - nd_vec[i]  # no bounds: previous SoC minus current net demand
    soc_lb[i] = max(soc_nb[i], 0)        # lower bound: can't discharge battery to negative SoC
    soc_bb[i] = min(soc_lb[i], capacity) # upper bound: can't charge battery beyond capacity
    net_demand[i] = nd_vec[i] - (soc_bb[i-1] - soc_bb[i]) # net demand after discharging battery
  }
  
  return (list(soc=soc_bb, net_demand=net_demand))
}

# returns EV battery SoC and recalculated ND based on initial ND and EV charging parameters
# heuristic rule: each day EV charges just enough to cover daily miles driven
calc_nd_ev = function(nd_vec, capacity, ev_at_home, ev_off_peak, ev_drive) {
  # initialize empty vectors
  soc_nb = soc_lb = soc_bb = delta_soc = net_demand = numeric(8760)
  soc_nb[1] = soc_lb[1] = soc_bb[1] = delta_soc[1] = 0
  net_demand[1] = nd_vec[1]
  
  # simple heuristic for charging/discharging EV
  for (i in 2:8760) {
    if (ev_at_home[i] == 1) {
      if (ev_off_peak[i] == 1) {
        soc_nb[i] = soc_bb[i-1] + ev_drive # EV at home and off-peak > charge
      } else {
        soc_nb[i] = soc_bb[i-1] - ev_drive # EV at home and peak > discharge
      }
      
      # apply SoC bounds and recalculate ND
      soc_lb[i] = max(soc_nb[i], 0)
      soc_bb[i] = min(soc_lb[i], capacity)
      delta_soc[i] = soc_bb[i] - soc_bb[i-1]   # change in SoC from time i-1 to i
      net_demand[i] = nd_vec[i] + delta_soc[i] # delta_soc>0: EV charging | delta_soc<0: EV discharging
      
    } else {
      soc_nb[i] = soc_bb[i-1] - ev_drive # EV not at home > driving
      soc_lb[i] = max(soc_nb[i], 0)
      soc_bb[i] = min(soc_lb[i], capacity)
      delta_soc[i] = soc_bb[i] - soc_bb[i-1]
      net_demand[i] = nd_vec[i]          # EV has no effect on net demand when absent
    }
  } 
  return (list(soc=soc_bb, delta_soc=delta_soc, net_demand=net_demand))
}
