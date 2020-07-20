library("dplyr")
library("gdxtools")


# BASE FUNCTIONS --------------------------------------





#' Extract simple variable
#' @noRd
getGDX_Variable<- function(gdx_filename,
                           variable_name,
                           unit = NULL){
  gdx_file = gdx(gdx_filename)
  myvar = gdx_file[as.character(variable_name)]

  if(is.null(unit)){return(myvar)}
  else{return(myvar %>% mutate("unit" = unit))}

}



#' Extract nty variable
#' @noRd
getGDX_Variable_nty <- function(  variable_name,
                                  gdx_filename,
                                  year_start = 0,
                                  year_limit = 2300,
                                  unit = NULL
                                ){
  gdx_file = gdx(gdx_filename)
  myvar = gdx_file[as.character(variable_name)] %>%
    mutate(year = t_to_y(as.integer(t))) %>%
    mutate(year =as.integer(year)) %>%
    mutate(t = as.integer(t))      %>%
    dplyr::filter(year >= year_start) %>%
    dplyr::filter(year <= year_limit)


  if(is.null(unit)){return(myvar)}
  else{return(myvar %>% mutate(unit = unit))}

}




#' Extract ty variable
#' @noRd
getGDX_Variable_ty <- function( variable_name,
                                gdx_filename,
                                year_start = 0,
                                year_limit = 2300,
                                unit = NULL
                              ){
  gdx_file = gdx(gdx_filename)
  myvar = gdx_file[as.character(variable_name)] %>%
    mutate(year = t_to_y(as.integer(t)) ) %>%
    mutate(year =as.integer(year)) %>%
    mutate(t = as.integer(t))      %>%
    dplyr::filter(year >= year_start) %>%
    dplyr::filter(year <= year_limit)


  if(is.null(unit)){return(myvar)}
  else{return(myvar %>% mutate("unit" = unit))}

}





# AGGREGATING FUNCTIONS -----------------------


#' Extract cumulated (5-years-period) n variable
#' @noRd
getGDX_Variable_CUML5y_n <- function(variable_name,
                                     gdx_filename,
                                unit = NULL,
                                year_start = 0,
                                year_limit=2300){

 myvar =getGDX_Variable_nty(  variable_name,
                              gdx_filename,
                              year_start,
                              year_limit   )   %>%
            group_by(n)                        %>%
            summarise(value = sum(value*5))    %>%  # multiplied because is the
            dplyr::select(n, value)            %>%
            as.data.frame()


  if(is.null(unit)){return(myvar)}
  else{return(myvar %>% mutate("unit" = unit))}

}




#' Extract world variable summing across countries
#' @noRd
getGDX_Variable_WORLDagg_ntySUMty <- function( variable_name,
                                               gdx_filename,
                                              unit = NULL,
                                              year_start = 0,
                                              year_limit = 2300 ) {


    VAR_nty =  getGDX_Variable_nty(variable_name, gdx_filename, year_start,year_limit)
    VAR_ty  =  WORLDaggr_ntySUMty(VAR_nty)

    if(is.null(unit)){return(VAR_ty)}
    else{return(VAR_ty %>% mutate("unit" = unit))}


}




## -------------:  DISAGGREGATING FUNCTIONS :-----------------------
#
#
#   getGDX_Variable_dsagg_ntyTOiso3ty <- function(   variable_name,
#                                                    gdx_file,
#                                                    unit = NULL,
#                                                    year_start = 0,
#                                                    year_limit = 2300 ){
#
#       d_n = getGDX_Variable_nty(variable_name, gdx_file, year_start,year_limit)
#       map_n_iso3 = getGDX_Parameter(gdx_file,"map_n_iso3")
#       iso3_variable = merge(map_n_iso3,d_n,by=c("n")) %>% sanitizeISO3()
#
#       if(is.null(unit)){return(iso3_variable)}
#       else{return(iso3_variable %>% mutate("unit" = unit))}
#
#   }
#
#
#
#
#   getGDX_Parameter_dsagg_ntyTOiso3ty <- function(  parameter_name,
#                                                    gdx_file,
#                                                    unit = NULL,
#                                                    year_start = 0,
#                                                    year_limit = 2300 ){
#
#       d_n = getGDX_Parameter_nty(parameter_name, gdx_file, year_start,year_limit)
#       map_n_iso3 = getGDX_Parameter(gdx_file,"map_n_iso3")
#       iso3_parameter = merge(map_n_iso3,d_n,by=c("n")) %>% sanitizeISO3()
#
#       if(is.null(unit)){return(iso3_parameter)}
#       else{return(iso3_parameter %>% mutate("unit" = unit))}
#   }
#
#












