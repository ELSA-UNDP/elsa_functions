
require(exactextractr)
require(raster)
require(sf)

elsa_area_by_region = function(vect, rast, action){
                  r1 = calc(rast,
                            fun = function(x) (x==action)*1)
                  d = exact_extract(r1,
                                    y = vect,
                                    fun = 'sum')
                  return(d)
}