#'
#'
#' @noRd
dimension_build_helper <- function(name
                                   ,id
                                   ,default_value = NA
                                   ,regex = NULL){

  return( c("name" = name
            ,"id" = id
            ,"default_value" = default_value
            ,"regex" = regex)
  )
}



#' TODO
#'
#' @noRd
RICEx.model.experiment_dimensions = list(


  "cooperation" = dimension_build_helper(name           = "cooperation"
                                         ,id            = "cooperation"
                                         ,default_value =  NA # simulation case
                                         ,regex         = "^coop-\\w+|^noncoop-\\w+" )

  ,"disnt"      = dimension_build_helper(name           = "disentangled"
                                         ,id            = "disnt"
                                         ,default_value =  NA # dice-welfare case
                                         ,regex         = "^coop-\\w+|^noncoop-\\w+" )

  ,"run_mode"   = dimension_build_helper(name           = "run mode"
                                         ,id            = "run_mode"
                                         ,default_value = "sim"
                                         ,regex         = NULL )

  ,"prstp"      = dimension_build_helper(name           = "time discount rate"
                                         ,id            = "prstp"
                                         ,default_value = "0.015"
                                         ,regex         = "^prstp\\w+" )

  ,"baseline"   = dimension_build_helper(name           = "baseline"
                                         ,id            = "baseline"
                                         ,default_value = "ssp2"
                                         ,regex         = "^ssp\\d" )

  ,"regions"    = dimension_build_helper(name           = "regions"
                                         ,id            = "regions"
                                         ,default_value = "ed57"
                                         ,regex         =  NA )


  ,"model_version"   = dimension_build_helper(name           = "model version"
                                              ,id            = "model_version"
                                              ,default_value = NA
                                              ,regex         = "^v\\d+x\\d+x\\d+|^d\\d+x\\d+x\\d+|^\\d+x\\d+x\\d+" )

  )




# PRIVATE HELPER ----------------------------------

#' @noRd
RICEx.model.experiment_dimensions.get_default_value <- function(i){

  ifelse(is.null(RICEx.model.experiment_dimensions[[i]]["default_value"]),
         NA,
         RICEx.model.experiment_dimensions[[i]]["default_value"])

}


#' @noRd
RICEx.model.experiment_dimensions.add_dimension <- function(name
                                                           ,identifier
                                                           ,default_value = NA
                                                           ,regex = NULL){

  dimension_build_helper(name,identifier,default_value, regex)
  }






