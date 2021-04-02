
require(tidyverse)
require(units)
require(sf)

elsa_region_tidy_ha = function(.data, name_attr, rast){
        .data %>% 
                mutate(protect = elsa_area_by_region(.,rast = r_clump, 1)*(r_res^2)/1e4, # Convert to hectares
                       restore = elsa_area_by_region(.,rast = r_clump, 2)*(r_res^2)/1e4,
                       manage = elsa_area_by_region(.,rast = r_clump, 3)*(r_res^2)/1e4) %>%
                pivot_longer(., cols = c(protect,restore,manage), names_to='action', values_to = 'ha') %>%
                mutate(action = factor(action, levels = c("protect","restore","manage"))) %>% 
                rename(name=name_attr)
        
}