


#'
#'
#' @param experiment_id current experiment codename
#' @param selection (TATM2100, VARIABLE_nty)
#' @param variable_name
#'
#' @export
#'
r50x.load.results_as_list <- function(experiment_id
                                      ,add_to_list = NULL
                                      ,last_import_time = 0 ){


  if(is.null(add_to_list)){ my_experiments_list = list()} else{ my_experiments_list = add_to_list }



  r50x.import.results_as_list( experiment_id,
                               my_experiments_list,
                               last_import_time
                               )

}






#'
#'
#' @param experiment_id current experiment codename
#' @param selection (TATM2100, VARIABLE_nty)
#' @param variable_name
#'
#' @export
#'
r50x.load.variable_experiment_space <- function(experiment_id
                                                 ,selection = NULL
                                                 ,variable_name = NULL ){

    if(is.null(selection)){ stop("Must select a variable case")}


    my_selection_function <- switch(selection,

        TATM2100 = {

         RICEx.utils.RICExs3class_custom_selection_function("get_TATM_ty %>% filter(year == 2100)")
    }

        ,TATM = {

         RICEx.utils.RICExs3class_custom_selection_function("get_TATM_ty")
    }



        ,world_EMIffi = {

         RICEx.utils.RICExs3class_custom_selection_function("get_world_EMIffi_ty")
    }



         ,VARIABLE_nty = {

         RICEx.utils.RICExs3class_custom_selection_function(paste0("get_VARIABLE_nty(variable_name=\"",variable_name,"\")"))
    }

        ,VARIABLE_ty = {

         RICEx.utils.RICExs3class_custom_selection_function(paste0("get_VARIABLE_ty(variable_name=\"",variable_name,"\")"))
    }
    )

    result_table = RICEx.import.experiment_table(experiment_id, my_selection_function)


    return(result_table)

}

