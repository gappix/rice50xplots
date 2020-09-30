


#' Parses gdx name and path and extracts experiment informations
#'
#' @param experiment list-structure with experiment infos, output of r50x.parse.gdx_experiment function
#' @param original_list experiment structure where to add informations
#'
#' @noRd
#'
RICEx.build.add_parsed_experiment_into_list = function(experiment,original_list){



  ## Extracts and tide values  ............

  # Sanitize "-" separator
  regions  = gsub("-", ".", experiment$main_infos["regions" ])
  policy   = gsub("-", ".", experiment$main_infos["policy"  ])
  baseline = gsub("-", ".", experiment$main_infos["baseline"])
  version  = gsub("-", ".", experiment$main_infos["version" ])
  impacts  = gsub("-", ".", experiment$main_infos["impacts" ])



  # Tide cooperation/run mode
  # If run_mode = opt -> show cooperation and sanitize "-" separator
  # Otherwise simply report "sim"

  run_mode  = ifelse(experiment$main_infos["run_mode" ] == "opt",
                    gsub("-", "." , experiment$main_infos["cooperation"]),
                    "simulation")



  # Tide extra parameters

  extra    = if(is.na(experiment$extra_infos)) { ""  # Empty string
    }else {
    # combine all extra parameters as name-value (ordered)
    temp = c()
    extra_temp = experiment$extra_infos[ order(names(experiment$extra_infos))]
    for(i in 1:length(extra_temp)) temp[i] = paste0(names(extra_temp)[i], "-", extra_temp[i])
    collapsed = paste(temp, collapse = '_')
    sanitized = gsub("\\.", "x", collapsed)  # replace . with x caracther
    gsub("-", ".", sanitized)                # replace - with . caracther
  }



  # Tide informations to store

  info = list("main_infos"  = experiment$main_infos,
              "extra_infos" = experiment$extra_infos,
              "full_path"   = experiment$full_path)



  RICEx_S3data = experiment$data



  ## Call building function ............

  RICEx.build.add_experiment_to_list(   original_list = original_list,
                                        regions       = regions,
                                        policy        = policy,
                                        baseline      = baseline,
                                        impacts       = impacts,
                                        run_mode      = run_mode,
                                        extra         = extra,
                                        version       = version,
                                        info          = info,
                                        RICEx_S3data  = RICEx_S3data )

}




#' Parses gdx name and path and extracts experiment informations
#'
#' @param
#' @param
#'
#' @noRd
#'
RICEx.build.add_experiment_to_list = function( original_list,
                                               regions,
                                               policy,
                                               baseline,
                                               impacts,
                                               run_mode,
                                               extra,
                                               version,
                                               info,
                                               RICEx_S3data){


  # "DATA" becomes our ending identifier
  extra_and_data = ifelse(nchar(extra) < 1, "DATA", paste0(extra,"_DATA"))


  # data
  eval(parse(text = paste0('original_list',
                           '$',regions,
                           '$',policy,
                           '$',baseline,
                           '$',impacts,
                           '$',run_mode,
                           '$',extra_and_data,
                           ' <- RICEx_S3data')))


  original_list

}



