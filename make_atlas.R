require(tmap)
require(basemaps)

make_atlas = function(.data,
                    raster,
                    map_title,
                    name_attr,
                    buffer_dist = 10e4,
                    raster_alpha = 0.85,
                    palette = elsa_pallete,
                    font_family = "Roboto",
                    service = "carto",
                    type = "light_no_labels"){
  
  basemaps::set_defaults(map_service = service,
                         map_res = 1,
                         map_type = type)

  bm = basemaps::basemap_raster(st_buffer(st_transform(st_combine(.data),3857), dist = buffer_dist)) #dist in km
  bm = projectRaster(bm, crs = crs(.data))
  
  map = tm_shape(.data) +
          tm_borders(alpha = 0) + #
        tm_shape(bm, bbox = .data, raster.downsample = FALSE)+
          tm_rgb() +
        tm_shape(raster,
                 raster.downsample = FALSE) +
          tm_raster(n = 3,
                    style = 'cat',
                    breaks = c(1,2,3),
                    labels = c('Protect','Restore','Manage'),
                    palette = palette,
                    alpha = raster_alpha,
                    title = "",
                    legend.is.portrait = FALSE) +
        tm_shape(.data) +
          tm_borders(col = "grey10",
                     lty = 'twodash',
                     lwd = 1.2,
                     alpha = 0.8) +
          # tm_text(text = name_attr,
          #         size='AREA',
          #         size.lowerbound = 0.5,
          #         print.tiny = TRUE) +
        tm_scale_bar(color.dark = 'black',
                    position = c("LEFT","TOP"),
                    text.size = 0.5) +
        tm_credits(text = "Carto | UNDP",
                   col = 'black',
                   position = c("RIGHT","BOTTOM"),
                   size = 0.6) +
        tm_facets(name_attr,
                  nrow = 1) +
        tm_layout(fontfamily = font_family,
                  #title = glue("{name_attr}"),
                  #title.position = c("center","tops"),
                  #title.size = 2,
                  #title.fontface = 1,
                  frame = FALSE,
                  legend.outside = TRUE, 
                  legend.outside.position = "bottom",
                  legend.position = c("center","bottom"),
                  asp = 0)
}