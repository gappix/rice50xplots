

load("data/ed57shp.Rdata")
load("data/ed35shp.Rdata")
load("data/r5shp.Rdata")
load("data/global1shp.Rdata")


#' Returns .shp datafile for ggplot maps for specific regional disaggregations
#' @export
#'
r50x.plots.utils.map_shapedata_selector <- list(

  "ed57shp"    = ed57shp,
  "ed35shp"    = ed35shp,
  "r5shp"      = r5shp,
  "global1shp" = global1shp
)
