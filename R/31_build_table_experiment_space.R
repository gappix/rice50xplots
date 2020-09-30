


#' Extracts experiment informations to build a neat and clean dataframe containing experiments dimensions
#' and variable values.
#'
#' @param exp_table current experiments-space table
#' @param experiment_parsed list-structure with experiment infos, output of r50x.parse.gdx_experiment function
#' @param variable_extracted_data dataframe containing data extracted from gdx-file (e.g., TATM_ty variable)
#'
#' @noRd
r50x.build.table_with_variable_and_experiment_space <- function(exp_table = NULL
                                                                 ,experiment_parsed
                                                                 ,variable_extracted_data ){

  # simplify
  experiment = experiment_parsed
  extracted_data = variable_extracted_data


  # TEST BOX .................................................................................................................................................................................................
  #1:  experiment = r50x.parse.gdx_experiment("C:/Users/GaPPi/Documents/PhD Projects/RICEx-PROJECT/RICEx-Plots/../RICEx-Results/PaperII//ed57/RICEx_d0x9x1_CBA_ssp1_BHMn-LR_coop-pop_prstp-0.001_disnt-g2.gdx")
  #2:  experiment = r50x.parse.gdx_experiment("C:/Users/GaPPi/Documents/PhD Projects/RICEx-PROJECT/RICEx-Plots/../RICEx-Results/PaperII//ed57/RICEx_d0x9x1_BAU_ssp5_KAHNn.gdx")
  #    extracted_data = experiment$data$get_TATM_ty
  #............................................................................................................................................................................................................


  #______________
  ## Main infos   ............................

  # create new df
  new_df_m = rbind(data.frame(matrix(ncol = length(experiment$main_infos), nrow = 0)), experiment$main_infos)
  colnames(new_df_m) = names(experiment$main_infos)

  # add data
  data_df <- merge(extracted_data, new_df_m, by= NULL)



  #______________
  ## Extra infos   ............................

  if(!is.na(experiment$extra_infos[1])) {

    # create new df
    new_df_e = rbind(data.frame(matrix(ncol = length(experiment$extra_infos), nrow = 0)), experiment$extra_infos)
    colnames(new_df_e) = names(experiment$extra_infos)

    # add extra dimensions
    data_df <- merge(data_df, new_df_e, by= NULL)
  }



  #________________________
  ## Merge with main table  ............................


  if(is.null(exp_table) | (length(exp_table) <=1 ) ){

    # if null, this is the main table.
    exp_table = data_df

  }else{

    # Otherwise need to merge...
    exp_table = bind_rows(data_df, exp_table)

    # .. and substitute NAs with parameters' default values
    for(i in colnames(exp_table)){

      # TEST BOX ..........
      # i = "prstp"
      #....................
      data.table::set(exp_table, which(is.na(exp_table[[i]])), i, RICEx.model.experiment_dimensions.get_default_value(i) )
    }
  }


  # TEST BOX ..........
  # view(exp_table)
  #....................

  return(exp_table)

}


