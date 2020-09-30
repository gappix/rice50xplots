
library("dplyr")
library("data.table")


#' S3 CLASS containing useful getter for all needed parameters
#' @param gdx_file_and_path string path to gdx file associated
#' @export
RICE50xS3Class <- function(gdx_file_and_path){


  # Get the environment for this instance of the function.
  # (Which is NOT the global environment, but only a local scope)
  thisEnv <- environment()



  # gdx file reference
  my_gdx   <- gdx_file_and_path



  ## GENERAL-PURPOSE GETTERS ---------------------------


  # Retrieve generic Variable
  my_getVariable  <- function( variable_name,
                               unit = NULL){

    getGDX_Variable( variable_name,
                     unit,
                     gdx_filename = my_get_gdx_filename()
                     )
  }

  # Retrieve regional nty Variable
  my_getVariable_nty  <- function(  variable_name,
                                    year_start = 0,
                                    year_limit = r50x.SettingsS3$get_default_year_horizon(),
                                    unit = NULL
                                  ){

    getGDX_Variable_nty( variable_name,
                         year_start,
                         year_limit,
                         unit,
                         gdx_filename = my_get_gdx_filename()
                        )
  }

  # Retrieve global ty Variable
  my_getVariable_ty  <- function(   variable_name,
                                    year_start = 0,
                                    year_limit = r50x.SettingsS3$get_default_year_horizon(),
                                    unit = NULL
                                  ){

    getGDX_Variable_ty(	variable_name,
                        year_start,
                        year_limit,
                        unit,
                        gdx_filename = my_get_gdx_filename()
                      )
  }


  my_getGDX   <-   function(){  return(gdx(get("my_gdx",thisEnv)))   }


  my_get_gdx_filename   <-   function(){  return(get("my_gdx",thisEnv))   }



  ## AGGREGATING FUNCTIONS -----------------------------


  # Global ty variable summing regional nty one
  my_VAR_WORLDagg_ntySUMty  <- function( variable_name,
                                         year_start = 0,
                                         unit = NULL,
                                         year_limit = r50x.SettingsS3$get_default_year_horizon()){

      getGDX_Variable_WORLDagg_ntySUMty(  variable_name,
                                          gdx_filename = my_get_gdx_filename(),
                                          unit,
                                          year_start,
                                          year_limit   )
  }


  # Regional cumulated variable multiplying for tstep
  my_VAR_CUML5y_n <- function(variable_name,
                              unit = NULL,
                              year_start = 0,
                              year_limit = r50x.SettingsS3$get_default_year_horizon(),
                              tstep = 5
                              ){

      getGDX_Variable_CUML5y_n( variable_name,
                                unit,
                                gdx_filename = my_get_gdx_filename(),
                                year_start,
                                year_limit)
    }



  # Function to evaluate NPV from CASHflow_nty variable
  my_NPV_evaluator_nty2n <- function(CASHflow_nty
                                     , disc_rate = 0.03
                                     , start_year = 2015
                                     , end_year= 2100){


    # equal formula:  sum{t=1:T}(R(t)/(1+i)^t)
    # applied for each n-country

    CASHflow_nty %>%
      select(n,value,year) %>%
      filter(year >= start_year,
             year <= end_year   ) %>%
      mutate(yearly_value = value*((1+disc_rate)^(-(year-start_year)))) %>%
      group_by(n) %>%
      summarise(value = sum(yearly_value)) %>%
      as.data.frame()

  }





  ## DISAGGREGATING FUNCTIONS --------------------------

  # disaggregating from n regions to iso3 regions
  my_VAR_dsagg_ntyTOiso3ty <- function(   variable_name,
                                          unit = NULL,
                                          year_start = 0,
                                          year_limit = r50x.SettingsS3$get_default_year_horizon() ){


      getGDX_Variable_dsagg_ntyTOiso3ty(  variable_name,
                                          gdx_filename = my_get_gdx_filename(),
                                          unit,
                                          year_start,
                                          year_limit
                                        )
  }



  ## REGIONAL NTY GETTERS  ---------------------------------

  my_ppp2mer_nty   <- function(){ my_getVariable_nty("ppp2mer") }

  my_ABATECOSTabs_nty   <- function(){ my_getVariable_nty("ABATECOST",    unit = "Trill 2005 USD/year" ) }

  my_ABATECOSTperc_nty  <- function(){ merge( my_getVariable_nty("ABATECOST" ) %>% rename(abatecosts = value),
                                              my_getVariable_nty("ykali"   )   %>% rename(ykali      = value),
                                              by = c("n","t","year")
                                              )  %>% mutate(value = abatecosts/ykali * 100) %>% dplyr::select(n,t,year,value) %>% mutate(unit = "% baseline")
  }



  my_ABATECOST_YBASEperc_NPV <- function(disc_rate = 0.03, start_year = 2015, end_year= 2100){

    merge(my_NPV_evaluator_nty2n( CASHflow_nty = my_getVariable_nty("ABATECOST"), disc_rate, start_year, end_year) %>% rename(abcost = value),
          my_NPV_evaluator_nty2n( CASHflow_nty = my_getVariable_nty("ykali"),     disc_rate, start_year, end_year) %>% rename(ykali  = value),
          by = c("n")) %>%
      mutate( value = abcost/ykali*100 ) %>%
      mutate( unit = "% GDPbaseline NPV") %>%
      mutate( notes = "disc_rate="%&%disc_rate%&%", start="%&%start_year%&%", end="%&%end_year)
  }




  my_Ycap_nty <- function(){ merge( my_getVariable_nty("Y"    ) %>% rename(gdp    = value),
                                    my_getVariable_nty("pop"  ) %>% rename(pop  = value),
                                    by = c("n","t","year")
                                )   %>%  
                                mutate(value = (gdp * 1000000)/pop)   %>%   
                                dplyr::select(n,t,year,value)  %>%  
                                mutate(unit = "USD/person")
  }



  my_ABATEDEMI_nty  <- function(){  my_getVariable_nty("ABATEDEMI",    unit = "GtCO2/year"          )
  }

  my_CIntensity_nty <- function(){ merge(   my_getVariable_nty("EIND"    ) %>% rename(eind    = value),
                                    my_getVariable_nty("YGROSS"  ) %>% rename(ygross  = value),
                                    by = c("n","t","year")
                                )   %>%  mutate(value = eind/ygross)   %>%   dplyr::select(n,t,year,value)  %>%  mutate(unit = "kgCO2/USD")
  }

  my_CONSUMPTION_nty <- function(){  my_getVariable_nty("C",            unit = "Trill 2005 USD/year" ) }

  my_CPRICE_nty <- function(){  my_getVariable_nty("CPRICE",       unit = "USD/tCO2"            ) }


  my_EMI_nty         <- function(){  my_getVariable_nty("E",            unit = "GtCO2/year"          ) }
  my_EIND_nty        <- function(){  my_getVariable_nty("EIND",         unit = "GtCO2/year"          ) }
  my_ELAND_nty        <- function(){  my_getVariable_nty("ELAND",        unit = "GtCO2/year"          ) }


  my_MIU_nty  <- function(){
        my_getVariable_nty(variable_name = "MIU", unit = "%" ) %>% mutate(value = value * 100)

    }


  my_SCC_nty           <- function(){  my_getVariable_nty("scc",         unit = "USD/tCO2eq"          ) }



  my_DAMAGES_abs_nty   <- function(){ my_getVariable_nty("DAMAGES",      unit = "Trill 2005 USD/year" ) }

  my_DAMAGES_BASEperc_nty  <- function(){ my_getVariable_nty("damfrac_ykali", unit = "% GDPbaseline" ) }

  my_DAMAGES_YGROSSperc_nty  <- function(){ my_getVariable_nty("damfrac_ygross", unit = "% GDPgross" ) }




  my_DAMAGES_YBASEperc_NPV <- function(disc_rate = 0.03, start_year = 2015, end_year= 2100){

    merge(my_NPV_evaluator_nty2n( CASHflow_nty = my_getVariable_nty("DAMAGES"), disc_rate, start_year, end_year) %>% rename(damage = value),
          my_NPV_evaluator_nty2n( CASHflow_nty = my_getVariable_nty("ykali"),   disc_rate, start_year, end_year) %>% rename(ykali  = value),
          by = c("n")) %>%
      mutate( value = -damage/ykali*100 ) %>% # (-) is damage (+) is gain
      mutate( unit = "% GDPbaseline NPV") %>%
      mutate( notes = "disc_rate="%&%disc_rate%&%", start="%&%start_year%&%", end="%&%end_year)
  }



  #' Absolute temperatures
  my_TREGIONabs_nty     <- function(apply_TEMPcap = FALSE){

    if(apply_TEMPcap) my_getVariable_nty("TEMP_REGION_DAM", unit = "deg C" )
    else              my_getVariable_nty("TEMP_REGION",     unit = "deg C" ) }


  #' Base 1980-2010 temperatures
  my_TREGIONbase_n     <- function(){ my_getVariable("temp_region_base", unit = "deg C" ) }


  #' Temperature increments compared to base reference
  my_TREGIONincr_nty    <- function(apply_TEMPcap = FALSE){

    merge( my_TREGIONabs_nty(apply_TEMPcap) %>% dplyr::rename(temp_now = value),
           my_TREGIONbase_n()               %>% dplyr::rename(temp_base  = value) %>% select(-c("unit")),
           all.x = TRUE )  %>%
      dplyr::mutate(value = temp_now-temp_base) %>%
      dplyr::select(-c("temp_now","temp_base")) %>% mutate(unit = "deg C")
  }


  my_POPshare_nty      <- function(){ merge(my_getVariable_nty("pop")  %>% rename(regionpop  = value) ,
                                            my_VAR_WORLDagg_ntySUMty("pop")  %>% rename(worldpop  = value),
                                            all.x = TRUE) %>% mutate(value = regionpop/worldpop) %>% dplyr::select(-c("worldpop","regionpop")) %>% mutate(unit = "%")
  }


  ## WORLD TY GETTERS  ---------------------------------

  my_world_ABATECOST_ty   <- function(){  my_VAR_WORLDagg_ntySUMty("ABATECOST",  unit = "Trill 2005 USD/year" ) }
  my_world_ABATEDEMI_ty   <- function(){  my_VAR_WORLDagg_ntySUMty("ABATEDEMI",  unit = "GtCO2/year"          ) }

  my_world_CONSUMPTION_ty <- function(){  my_VAR_WORLDagg_ntySUMty("C",       unit = "Trill 2005 USD/year" ) }
  my_world_EMItot_ty      <- function(){  my_VAR_WORLDagg_ntySUMty("E",       unit = "GtCO2/year"          ) }
  my_world_EMIffi_ty      <- function(){  my_VAR_WORLDagg_ntySUMty("EIND",    unit = "GtCO2/year"          ) }





  my_world_DAMAGES_abs_ty            <- function(){  my_getVariable_nty("world_damages",    unit = "Trill 2005 USD/year") %>% filter(gdpadj=="PPP")}# %>% select(-gdpadj) }
  my_world_DAMAGES_BASEperc_ty       <- function(){  my_getVariable_nty("world_damfrac",    unit = "Trill 2005 USD/year") %>% filter(gdpadj=="PPP") %>% select(-gdpadj) }
  my_world_DAMAGES_abs_MER_ty        <- function(){  my_getVariable_nty("world_damages",    unit = "Trill 2005 USD/year") %>% filter(gdpadj=="MER") %>% select(-gdpadj) }
  my_world_DAMAGES_BASEperc_MER_ty   <- function(){  my_getVariable_nty("world_damfrac",    unit = "Trill 2005 USD/year") %>% filter(gdpadj=="MER") %>% select(-gdpadj) }

  my_world_DAMAGES_YGROSSperc_ty     <- function(){
    merge(  my_VAR_WORLDagg_ntySUMty("YNET" )    %>% rename(ynet    = value),
            my_VAR_WORLDagg_ntySUMty("YGROSS")   %>% rename(ref    = value),
            by = c("t","year")
    )  %>%
      mutate(value = (ynet-ref)/ref * 100) %>%
      dplyr::select(t,year,value) %>%
      mutate(unit = "% GDPgross")
  }


  my_world_DAMAGES_YGROSSperc_MER_ty <- function(){

    ppp2mer_nty <- my_ppp2mer_nty() %>% dplyr::rename(ppp2mer = value)

    merge( WORLDaggr_ntySUMty( merge( my_getVariable_nty("YNET" ),
                                      ppp2mer,
                                      by=c("n","t","y")) %>%
                                 mutate(value = ppp2mer * value) %>%
                                 select(-ppp2mer)
                              ) %>% rename(ynet    = value),

           WORLDaggr_ntySUMty( merge( my_getVariable_nty("YGROSS" ),
                                      ppp2mer,
                                      by=c("n","t","y")) %>%
                                 mutate(value = ppp2mer * value) %>%
                                 select(-ppp2mer)
                               ) %>% rename(ref    = value),

            by = c("t","year"))  %>%
      mutate(value = (ynet-ref)/ref * 100) %>%
      dplyr::select(t,year,value) %>%
      mutate(unit = "% GDPgross")
  }









  my_world_CIntensity_ty  <- function(){ merge(   my_VAR_WORLDagg_ntySUMty("EIND"    ) %>% rename(eind    = value),
                                                  my_VAR_WORLDagg_ntySUMty("YGROSS"  ) %>% rename(ygross  = value),
                                    by = c("t","year")
                                 )  %>% mutate(value = eind/ygross) %>% dplyr::select(t,year,value) %>% mutate(unit = "kgCO2/USD")
  }

  my_TATM_ty            <- function(){  my_getVariable_ty("TATM",          unit = "deg C"          )

  }






  #____________________________
  ## S3 CLASS EXPOSED METHODS --------------------------


    # Create the list as before
    me <- list(

      # the environment
      thisEnv = thisEnv,


      # exposed specific values
      get_ppp2mer_nty                     = get("my_ppp2mer_nty",     thisEnv) ,


      get_ABATECOSTabs_nty                = get("my_ABATECOSTabs_nty",     thisEnv) ,
      get_ABATECOSTperc_nty               = get("my_ABATECOSTperc_nty",    thisEnv) ,
      get_ABATEDEMI_nty                   = get("my_ABATEDEMI_nty",        thisEnv) ,
      get_CONSUMPTION_nty                 = get("my_CONSUMPTION_nty",      thisEnv) ,
      get_CIntensity_nty                  = get("my_CIntensity_nty",       thisEnv) ,
      get_CPRICE_nty                      = get("my_CPRICE_nty",           thisEnv) ,

      get_EMI_nty                         = get("my_EMI_nty",              thisEnv) ,
      get_EIND_nty                        = get("my_EIND_nty",             thisEnv) ,
      get_ELAND_nty                       = get("my_ELAND_nty",            thisEnv) ,
      get_MIU_nty                         = get("my_MIU_nty",              thisEnv) ,
      get_SCC_nty                         = get("my_SCC_nty",              thisEnv) ,
      get_POPshare_nty                    = get("my_POPshare_nty",         thisEnv) ,


      # gdp
      get_Ycap_nty                        = get("my_Ycap_nty",             thisEnv) ,


      # abatecosts
      get_ABATECOST_YBASEperc_NPV         = get("my_ABATECOST_YBASEperc_NPV",          thisEnv) ,


      # impacts
      get_DAMAGES_abs_nty                 = get("my_DAMAGES_abs_nty",                  thisEnv) ,
      get_DAMAGES_BASEperc_nty            = get("my_DAMAGES_BASEperc_nty",             thisEnv) ,
      get_DAMAGES_YGROSSperc_nty          = get("my_DAMAGES_YGROSSperc_nty",           thisEnv) ,
      get_DAMAGES_YBASEperc_NPV           = get("my_DAMAGES_YBASEperc_NPV",            thisEnv) ,
      get_world_DAMAGES_abs_ty            = get("my_world_DAMAGES_abs_ty",             thisEnv) ,
      get_world_DAMAGES_BASEperc_ty       = get("my_world_DAMAGES_BASEperc_ty",        thisEnv) ,
      get_world_DAMAGES_YGROSSperc_ty     = get("my_world_DAMAGES_YGROSSperc_ty",      thisEnv) ,
      get_world_DAMAGES_abs_MER_ty        = get("my_world_DAMAGES_abs_MER_ty",         thisEnv) ,
      get_world_DAMAGES_BASEperc_MER_ty   = get("my_world_DAMAGES_BASEperc_MER_ty",    thisEnv) ,
      get_world_DAMAGES_YGROSSperc_MER_ty = get("my_world_DAMAGES_YGROSSperc_MER_ty",  thisEnv) ,
      # temperatures
      get_TREGIONabs_nty                  = get("my_TREGIONabs_nty",       thisEnv) ,
      get_TREGIONincr_nty                 = get("my_TREGIONincr_nty",      thisEnv) ,
      get_TREGIONbase_n                   = get("my_TREGIONbase_n",        thisEnv) ,
      get_TATM_ty                         = get("my_TATM_ty",              thisEnv) ,

      get_world_ABATECOST_ty              = get("my_world_ABATECOST_ty",   thisEnv) ,
      get_world_ABATEDEMI_ty              = get("my_world_ABATEDEMI_ty",   thisEnv) ,
      get_world_EMItot_ty                 = get("my_world_EMItot_ty",      thisEnv) ,
      get_world_EMIffi_ty                 = get("my_world_EMIffi_ty",      thisEnv) ,

      get_world_CIntensity_ty             = get("my_world_CIntensity_ty",  thisEnv) ,
      get_world_CONSUMPTION_ty            = get("my_world_CONSUMPTION_ty", thisEnv) ,


      # exposed general-purpouse functions


      get_VARIABLE_nty                 = get("my_getVariable_nty",         thisEnv) ,
      get_VARIABLE_ty                  = get("my_getVariable_ty",          thisEnv) ,
      get_VARIABLE                     = get("my_getVariable",             thisEnv) ,

      get_GDX                          = get("my_getGDX",                  thisEnv)





    )


  # Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)



  class(me) <- append(class(me),"RICE50xS3Class")
  return(me)

}

#'
#' @export
r50x.RICE50xS3Class_selectors <- list(


  "get_TATM_2100" = "get_TATM_ty() %>% filter(year == 2100)",

"get_ppp2mer_nty"         = "get_ppp2mer_nty()",
"get_ABATECOSTabs_nty"    = "get_ABATECOSTabs_nty()",
"get_ABATECOSTperc_nty"   = "get_ABATECOSTperc_nty()" ,
"get_ABATEDEMI_nty"       = "get_ABATEDEMI_nty()",
"get_CONSUMPTION_nty"     = "get_CONSUMPTION_nty()" ,
"get_CIntensity_nty"      = "get_CIntensity_nty()",
"get_CPRICE_nty"          = "get_CPRICE_nty()",
"get_EMI_nty"             = "get_EMI_nty()",
"get_EIND_nty"            = "get_EIND_nty()",
"get_ELAND_nty"           = "get_ELAND_nty()",
"get_MIU_nty"             = "get_MIU_nty()",
"get_SCC_nty"             = "get_SCC_nty()",
"get_POPshare_nty"        = "get_POPshare_nty()",
"get_YGROSS_nty"          = "get_VARIABLE_nty(variable_name=\"YGROSS\")",
# impacts
"get_DAMAGES_abs_nty"               = "get_DAMAGES_abs_nty()",
"get_DAMAGES_BASEperc_nty"          = "get_DAMAGES_BASEperc_nty()",
"get_DAMAGES_YGROSSperc_nty"        = "get_DAMAGES_YGROSSperc_nty()",
"get_world_DAMAGES_abs_ty"          = "get_world_DAMAGES_abs_ty()",
"get_world_DAMAGES_BASEperc_ty"     = "get_world_DAMAGES_BASEperc_ty()",
"get_world_DAMAGES_YGROSSperc_ty"   = "get_world_DAMAGES_YGROSSperc_ty()",
"get_world_DAMAGES_abs_MER_ty"      = "get_world_DAMAGES_abs_MER_ty()",
"get_world_DAMAGES_BASEperc_MER_t"  = "get_world_DAMAGES_BASEperc_MER_t()",
"get_world_DAMAGES_YGROSSperc_MER"  = "get_world_DAMAGES_YGROSSperc_MER()",
# temperatures
"get_TREGIONabs_nty"        = "get_TREGIONabs_nty()",
"get_TREGIONincr_nty"       = "get_TREGIONincr_nty()",
"get_TREGIONbase_n"         = "get_TREGIONbase_n()",
"get_TATM_ty"               = "get_TATM_ty()",
"get_world_ABATECOST_ty"    = "get_world_ABATECOST_ty()",
"get_world_ABATEDEMI_ty"    = "get_world_ABATEDEMI_ty()",
"get_world_EMItot_ty"       = "get_world_EMItot_ty()",
"get_world_EMIffi_ty"       = "get_world_EMIffi_ty()",
"get_world_CIntensity_ty"   = "get_world_CIntensity_ty()",
"get_world_CONSUMPTION_ty"  = "get_world_CONSUMPTION_ty()",
# exposed general-purpouse funct
"get_VARIABLE_nty"  = "get_VARIABLE_nty()",
"get_VARIABLE_ty"   = "get_VARIABLE_ty()",
"get_VARIABLE"      = "get_VARIABLE()",
"get_GDX"           = "get_GDX()"

)



#' Create a custom selection function for RICExclass
#'
#' @param command_string string containing command that has to follor '$'-operator
#' in RICE50xS3class
#' @noRd
r50x.utils.RICE50xS3Class_selector_builder  <- function(command_string){


  return( function(.df){ return(eval(parse(text =paste0('.df$',command_string)))) })

}


