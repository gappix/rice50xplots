
#' Useful function to easily concateate strings
#' Its equivalent to 'paste0' function bu nicer
#' @export
'%&%' <- function(x,y) paste0(x,y)



#' Transform t into years (starting from 2015)
#' @noRd
t_to_y = function(t){

  baseYear  = 2010
  tstep     = 5
  year      = baseYear + (t * tstep)

  year  }




#' Transform years (baseyear 2015) into t
#' @noRd
y_to_t = function(year){

  baseYear  = 2010
  tstep     = 5
  t =  (year - baseYear)/tstep

  t }




#' Function to aggregate nty dataframes into world ty ones
#' @export
WORLDaggr_ntySUMty <- function(DF_nty){


  DF_ty =  DF_nty    %>%
    group_by(t,year) %>%
    summarise(value = sum(value))   %>%
    dplyr::select(t,year, value)    %>%
    as.data.frame()

  return(DF_ty)

}


