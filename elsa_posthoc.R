library(raster)
library(sf)
library(tidyverse)
library(RPostgreSQL)
library(exactextractr)
library(glue)
library(tmap)

# Load Some Functions ####
source("../../elsa_functions/elsa_area_by_region.R")
source("../../elsa_functions/elsa_region_tidy_pct.R")
source("../../elsa_functions/elsa_region_tidy_ha.R")
source("../../elsa_functions/elsa_region_plot_pct.R")
source("../../elsa_functions/elsa_region_plot_ha.R")
source("../../elsa_functions/make_map.R")
source("../../elsa_functions/make_atlas.R")
source("../../elsa_functions/make_hm.R")


rape_data = "in_vectors/elsa_col_rape.gpkg" # Data path

r_clump = raster("elsa_outputs/elsa_all_clumped.tif")
r_nonclump = raster("elsa_outputs/elsa_all_unclumped.tif")
rap_e = read_sf(rape_data, layer = "limite_departamental", as_tibble = FALSE) %>%
        st_set_crs(value = "col_proj.wkt")
elsa_table = read_csv("elsa_outputs/ELSA_summary_results-2021-03-30.csv")
elsa_table %>% pivot_longer(cols = 3:7, names_to='actions')

#Some settings ####
elsa_pallete = c("#1C6BA2","#289028","#E5720D")
country = 'RAP-E'
r_res = res(r_clump)[1]
bar_pct = "elsa_outputs/elsa_region_bar_pct.png"
bar_ha = "elsa_outputs/elsa_region_bar_ha.png"
r_region_out = "elsa_outputs/elsa_regions.png"

# Commitment Barcharts ####
elsa_table %>%
  pivot_longer(cols = 3:7, names_to='actions') %>% 
  ggplot(aes())


# Build Barcharts %####
d1 = elsa_region_tidy_pct(rap_e, rast = r_clump, name_attr = "DPTO_CNMBR") 
d1 = elsa_region_plot_pct(d1, scenario = "Clumped")
        
d2 = elsa_region_tidy_pct(rap_e, rast = r_nonclump, name_attr = "DPTO_CNMBR")
d2 = elsa_region_plot_pct(d2, scenario = "Unclumped")

out_bar = cowplot::plot_grid(d1,d2)

ggsave(out_bar,
       filename = bar_pct,
       dpi = 300)

# Build Barcharts Hectare####
d1 = elsa_region_tidy_ha(rap_e, rast = r_clump, name_attr = "DPTO_CNMBR") 
d1 = elsa_region_plot_ha(d1, scenario = "Clumped")
        
d2 = elsa_region_tidy_ha(rap_e, rast = r_nonclump, name_attr = "DPTO_CNMBR")
d2 = elsa_region_plot_ha(d2, scenario = "Unclumped")

out_bar = cowplot::plot_grid(d1,d2)

ggsave(out_bar,
       filename = bar_ha,
       dpi = 300)

# Build some maps ####
r1 = make_map(.data = rap_e, raster = r_clump, map_title = "Clumped", name_attr = "DPTO_CNMBR", palette = elsa_pallete)

r2 = make_map(.data = rap_e, raster = r_nonclump, map_title = "Unclumped", name_attr = "DPTO_CNMBR", palette = elsa_pallete)

r_out = tmap_arrange(r1,r2,outer.margins = rep(2e-4,4))
tmap_save(r_out, dpi = 300, width = 10.2, height = 5.1, r_region_out)


pa_lock = calc(r_clump, function(x) x == 1)

pa_unlock

# Atlas ####

atlas = make_atlas(.data = rap_e, raster = r_nonclump, map_title = "Unclumped", name_attr = "DPTO_CNMBR", palette = elsa_pallete)
width = 10.2
tmap_save(atlas,asp = 0,  dpi = 300, width = 10.2, height = width/3, filename = "elsa_outputs/elsa_atlas.png")


# Heatmaps ####

elsa_hm = raster("elsa_outputs/All_HM.tif")

hm = make_hm(.data = rap_e, raster = elsa_hm, map_title = "HM", name_attr = "DPTO_CNMBR",palette = 'viridis')

tmap_save(hm, filename = "elsa_outputs/elsa_hm.png", dpi = 300)



