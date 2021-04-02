
require(tidyverse)
require(units)
require(sf)

elsa_region_tidy_pct = function(.data, name_attr, rast){
        .data %>% 
                mutate(area_m2 = units::drop_units(st_area(.)),
                       protect = elsa_area_by_region(.,rast = r_clump, 1)*(r_res^2)/area_m2,
                       restore = elsa_area_by_region(.,rast = r_clump, 2)*(r_res^2)/area_m2,
                       manage = elsa_area_by_region(.,rast = r_clump, 3)*(r_res^2)/area_m2) %>%
                pivot_longer(., cols = c(protect,restore,manage), names_to='action', values_to = 'pct') %>%
                mutate(action = factor(action, levels = c("protect","restore","manage"))) %>% 
                rename(name=name_attr)
        
}