


#' Parses gdx name and path and extracts experiment informations
#'
#' @param experiment_id current experiment codename
#' @param my_experiments list-structure to store experiments
#' @param last_import_time sys_time of last import (to detect new files)
#'
RICEx.import.experiment_table <- function(experiment_id
                                             ,RICExclass_selection_function
                                             ,data_exp_table = NULL
                                             ,last_import_time = 0
){


  # TEST BOX .................................................................................................................
  # experiment_id = "PaperII"
  # last_import_time = 0
  # data_exp_table = NULL
  # RICExclass_selection_function = RICEx.utils.RICExs3class_custom_selection_function("get_TATM_ty %>% filter(year == 2100)")
  #............................................................................................................................

  #_______
  # Step1: find all < experiment_id > results
  gdx_with_path_list  =  RICEx.find.new_results(experiment_id, last_import_time)




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
      exp_parsed =  RICEx.parse.gdx_experiment(gdx_path_element)


      # 2b: extract data
      variable_data = exp_parsed$data %>% RICExclass_selection_function()


      # 2c: update table with data
      data_exp_table    =  RICEx.build.table_with_variable_and_experiment_space(exp_table = data_exp_table
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
  # view(data_exp_table)
  #.............................................

  #cat(orange$bold("Done! << ", length(gdx_with_path_list) ," >> experiments inside built table\n"))


  return(data_exp_table)

}
