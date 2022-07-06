elsa_action_stats=function(pa_layer, elsa_layer){
  require(terra)
  require(dplyr)
  require(tibble)
  
  pu_layer = pa_layer

  pu_layer = terra::ifel(is.na(pu_layer), NA, 1) 
  pu_layer_sum = terra::global(terra::cellSize(pu_layer), sum, na.rm = T)$sum
  
  pa_layer = terra::ifel(pa_layer == 0, NA, pa_layer) 
  pa_layer_sum = terra::global(terra::cellSize(pa_layer), sum, na.rm = T)$sum

  elsa_protect = terra::ifel(elsa_layer != 1, NA, elsa_layer)
  elsa_protect_sum = terra::global(terra::cellSize(elsa_protect), sum, na.rm = T)$sum
  
  elsa_manage = terra::ifel(elsa_layer != 3, NA, elsa_layer)
  elsa_manage_sum = terra::global(terra::cellSize(elsa_manage), sum, na.rm = T)$sum
  
  elsa_restore = terra::ifel(elsa_layer != 2, NA, elsa_layer)
  elsa_restore_sum = terra::global(terra::cellSize(elsa_restore), sum, na.rm = T)$sum

  elsa_stats = tibble::tibble(elsa_action=c("protect_total",
                                            "protect_elsa",
                                            "manage",
                                            "restore"),
                              area_m2=c(elsa_protect_sum,
                                        elsa_protect_sum-pa_layer_sum,
                                        elsa_manage_sum,
                                        elsa_restore_sum)) %>% 
    dplyr::mutate(pct = area_m2 / pu_layer_sum * 100)
  
  return(elsa_stats)
}