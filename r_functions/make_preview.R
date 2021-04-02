# 26/01/2012 
# 
# This tries to replicate the map preview style that has been used in QGIS.
# 
# 
require(tmap)
require(basemaps)

make_preview = function(in_raster, out_image, buffer_dist = 10e4, raster_alpha = 0.7, palette = "viridis", font_family = "roboto", service = "carto", type = "dark_no_label"){
  
  basemaps::set_defaults(map_service = "carto",
                         map_res = 1,
                         map_type = "dark_no_labels")

  bm = basemaps::basemap_raster(st_buffer(st_transform(base_v,3857), dist = buffer_dist)) #dist in km
  bm = projectRaster(bm, crs = crs(base_v))
  
  map = tm_shape(bm)+
          tm_rgb() +
        tm_shape(in_raster,
                 raster.downsample = FALSE) +
        tm_raster(palette = palette,
                  alpha = raster_alpha,
                  title = "") +
        tm_shape(base_v) +
          tm_borders(col = "white",
                     lty = 'twodash',
                     lwd = 1.2,
                     alpha = 0.5)+
        tm_layout(legend.position = c("left","bottom"),
                  legend.text.color = 'white',
                  legend.text.fontfamily = font_family) +
        tm_credits(text = "Carto",
                   col = 'white',
                   align = "right",
                   fontfamily = font_family) 
        
  tmap_save(map,
            filename = paste("out_rasters/previews/", out_image, "_500m.png", sep = ""),
            height = 2155,
            outer.margins = c(0,0,0,0))
}
