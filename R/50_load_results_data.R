


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
#' @param RICE50xS3Class_selector use one option among r50x.RICE50xS3Class_selector listed ones
#' @param year_start
#' @param year_end
#' @param variable_name
#'
#' @export
#'
r50x.load.variable_experiment_space <- function(experiment_id
                                                , RICE50xS3Class_selector = NULL
                                                , year_start = 1
                                                , year_end = r50x.SettingsS3$get_default_year_horizon()
                                                , variable_name
                                                ){


    # TEST BOX ..............................................................
    #  experiment_id = "FinalRushPPP"
    #  RICE50xS3Class_selector = r50x.RICE50xS3Class_selectors$get_MIU_nty
    #  year_start = 1
    #  year_end = 2100
    #  variable_name = "Mitigation"
    #........................................................................
    if(is.null(RICE50xS3Class_selector)){ stop("Must select a r50x.RICE50xS3Class_selectors option")}


    my_selection_function <- r50x.utils.RICE50xS3Class_selector_builder(RICE50xS3Class_selector)

    # my_selection_function <- switch(RICE50xS3Class_selector,
    #
    #     TATM2100 = {
    #
    #      r50x.utils.RICE50xS3Class_selector_builder("get_TATM_ty %>% filter(year == 2100)")
    # }
    #
    #     ,TATM = {
    #
    #      r50x.utils.RICE50xS3Class_selector_builder("get_TATM_ty")
    # }
    #
    #
    #
    #     ,world_EMIffi = {
    #
    #      r50x.utils.RICE50xS3Class_selector_builder("get_world_EMIffi_ty")
    # }
    #
    #
    #
    #      ,VARIABLE_nty = {
    #
    #      r50x.utils.RICE50xS3Class_selector_builder(paste0("get_VARIABLE_nty(variable_name=\"",variable_name,"\")"))
    # }
    #
    #     ,VARIABLE_ty = {
    #
    #      r50x.utils.RICE50xS3Class_selector_builder(paste0("get_VARIABLE_ty(variable_name=\"",variable_name,"\")"))
    # }
    # )

    result_table = r50x.import.experiment_table( experiment_id,
                                                 my_selection_function,
                                                 year_start,
                                                 year_end)


    if(is.null(variable_name)) return(result_table)

    return(result_table %>% mutate(variable = variable_name))
}

