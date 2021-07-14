library(tidyr) 

# inflate some base amount value at some inflation rate over some number of steps 
inflate_base <- function(base=0.31, inflation=0.015, n=20) {
  steps <- 1:n
  inflated <- rep(1,n) # list array of 1s to modify in loop below
  for (step in steps) { 
    if (step == 1) {
      inflated[step] <- base
    }
    else {
      inflated[step] <- (1 + inflation) * inflated[step -1]  
    }
  }
  inflated
}

# Comppute a HWC's cost to run based on base rate, inflation rate and 
# hours it runs and kilowatts (size) of the heating element
hwc <- function(inflation=0.015, 
                elec_rate=0.31, 
                hours=3, 
                element=4, 
                nyears=20, 
                perf_coeff=1) { 
  years <- 1:nyears
  rates <- inflate_base(base=elec_rate, inflation=inflation, n=nyears)
  kwh <- hours * element * 365
  annual_cost <- rates * kwh / perf_coeff
  annual_cost 
}

compare_conventional_heatpump <- function(inflation = 0.015,
                                          elec_rate = 0.31, 
                                          hours = 3,
                                          element = 4,
                                          nyears = 20,
                                          perf_coeff = 3.9,
                                          hwc_cost = 2000,
                                          hp_cost = 10000 ) {
  
  conventional <- hwc(inflation=inflation, 
                      elec_rate=elec_rate,
                      hours=hours,
                      element=element,
                      nyears=nyears,
                      perf_coeff =1)
  heatpump <- hwc(inflation=inflation, 
                      elec_rate=elec_rate,
                      hours=hours,
                      element=element,
                      nyears=nyears,
                      perf_coeff = perf_coeff)
  tibble(year=1:nyears, conventional=conventional, heatpump=heatpump, 
         conventional_cum = cumsum(conventional) + hwc_cost,
         heatpump_cum = cumsum(heatpump) + hp_cost) 
}


plot_hwc_payoff <- function(inflation = 0.03,
                            elec_rate = 0.31, 
                            hours = 3,
                            element = 4,
                            nyears = 20,
                            perf_coeff = 2,
                            hwc_cost = 2000,
                            hp_cost = 6000 ) {
  
  comparison <- compare_conventional_heatpump(inflation = inflation,
                                              elec_rate = elec_rate, 
                                              hours = hours,
                                              element = element,
                                              nyears = nyears,
                                              perf_coeff = perf_coeff,
                                              hwc_cost = hwc_cost,
                                              hp_cost = hp_cost) %>% 
  select(year, conventional_cum, heatpump_cum) %>% 
  pivot_longer(cols = c("conventional_cum", "heatpump_cum"))
 
  return(comparison)
} 
