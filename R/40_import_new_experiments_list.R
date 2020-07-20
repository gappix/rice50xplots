


#' Parses gdx name and path and extracts experiment informations
#'
#' @param experiment_id current experiment codename
#' @param my_experiments list-structure to store experiments
#' @param last_import_time sys_time of last import (to detect new files)
#'
r50x.import.results_as_list <- function( experiment_id,
                                         my_experiments,
                                         last_import_time
){



  # Step1: find new results
  gdx_with_path_list  =  RICEx.find.new_results(experiment_id, last_import_time)

  if (length(gdx_with_path_list) > 0){


    cat(orange$bold("Importing << ", length(gdx_with_path_list) ," >> new gdxfile experiments \n"))
    i = 1


    # Step2: extract and organize each new data-experiment
    for(gdx_path_element in gdx_with_path_list){


      cat(green(i, " Parsing: ", gdx_path_element ," \n"))


      # 2a: parse gdx data and experiment info automatically
      exp_data_and_info =  RICEx.parse.gdx_experiment(gdx_path_element)

      # 2b: tide infos and data in nested list structure
      my_experiments    =  RICEx.build.add_parsed_experiment_into_list(exp_data_and_info, my_experiments)



      i= i+1

    }
  }


  cat(orange$bold("Done! << ", length(gdx_with_path_list) ," >> gdxfiles correctly imported in a list\n"))

  return(my_experiments)

}


