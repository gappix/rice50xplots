


#' Parses gdx name and path and extracts experiment informations
#'
#' @param experiment_id current experiment codename
#' @param my_experiments list-structure to store experiments
#' @param last_import_time sys_time of last import (to detect new files)
#' @export
r50x.import.experiment_table <- function(experiment_id
                                             ,RICE50xS3Class_selection_function
                                             ,data_exp_table = NULL
                                             ,last_import_time = 0
                                             ,year_start = 1
                                             ,year_end = r50x.SettingsS3$get_default_year_horizon()
){


  # TEST BOX .................................................................................................................
  # experiment_id = "PaperII"
  # last_import_time = 0
  # data_exp_table = NULL
  # RICE50xS3Class_selector = r50x.utils.RICE50xS3Class_selector_builder("get_TATM_ty %>% filter(year == 2100)")
  #............................................................................................................................

  #_______
  # Step1: find all < experiment_id > results
  gdx_with_path_list  =  r50x.find.new_results(experiment_id, last_import_time)




  #_______
  # Step2: extract and organize each new data-experiment

  # create progress bar
  i = 1
  tot = length(gdx_with_path_list)
  pb <- txtProgressBar( min = 0, max = tot,  style = 3)


  if (length(gdx_with_path_list) > 0){




    for(gdx_path_element in gdx_with_path_list){

      # TEST BOX ...................................
      # gdx_path_element = gdx_with_path_list[[2]]
      #.............................................


      # 2a: parse gdx data and experiment info automatically
      exp_parsed =  r50x.parse.gdx_experiment(gdx_path_element)


      # 2b: extract data
      variable_data = exp_parsed$data %>% RICE50xS3Class_selection_function()


      # 2c: apply filter on year (if present)
      # if("year" %in% colnames(variable_data)){ variable_data = variable_data %>%
      #   filter(year <= year_end  ) %>%
      #   filter(year >= year_start)
      # }


      # 2d: update table with data
      data_exp_table    =  r50x.build.table_with_variable_and_experiment_space( exp_table = data_exp_table
                                                                                ,experiment_parsed = exp_parsed
                                                                                ,variable_extracted_data = variable_data)


      # progress bar updated
      setTxtProgressBar(pb, i)
      i= i+1

    }
  }


  #♣ close progressbar
  Sys.sleep(0.1)
  close(pb)


  # TEST BOX ...................................
  # View(data_exp_table)
  #.............................................

  cat(orange$bold("Done! << ", length(gdx_with_path_list) ," >> experiments inside built table\n"))


  return(data_exp_table)

}
