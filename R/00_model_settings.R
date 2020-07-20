
#  rm(list=ls())
library('here')


#' @noRd
find_project_folder <- function(dir_pattern = "RICEx-Results"){

  #1st attempt: search here
  matching_folders <- list.files(path = paste0(here()),
                                 pattern=dir_pattern,
                                 include.dirs = TRUE,
                                 full.names = TRUE,
                                 recursive = TRUE)


  #2nd attempt: go 1 level upwards and try again
  if(length(matching_folders)==0){
    matching_folders <- list.files(path = paste0(here("..")),
                                   pattern=dir_pattern,
                                   include.dirs = TRUE,
                                   full.names = TRUE,
                                   recursive = TRUE)
  }

  #3rd attempt: go 2 levels upwards and try again
  if(length(matching_folders)==0){
    matching_folders <- list.files(path = paste0(here("../..")),
                                   pattern=dir_pattern,
                                   include.dirs = TRUE,
                                   full.names = TRUE,
                                   recursive = TRUE)
  }

  #4th and last attempt: go 3 levels upwards and try again
  if(length(matching_folders)==0){
    matching_folders <- list.files(path = paste0(here("../../..")),
                                   pattern=dir_pattern,
                                   include.dirs = TRUE,
                                   full.names = TRUE,
                                   recursive = TRUE)
  }

  #Directory not found: notify user
  if(length(matching_folders)==0){

    warning("WARNInG: No RICE50x results folder automatically detected. Please set it using < r50x.Settings.set_results_folder(path) > function.")
    return(NULL)

  } else{

    warning(paste0("INFO: <",matching_folders[[1]],"> automatically set as RICE50x model results-folder. To change it use < r50x.Settings.set_results_folder(path) > function.")
            ,immediate. = TRUE)
    return(matching_folders[[1]])
  }
}








#' @noRd
ModelEnvClass <- function(rice50x_results_dir){


  # Get the environment for this instance of the function.
  # (Which is NOT the global environment, but only a local scope)
  thisEnv <- environment()



  results_dir  <- rice50x_results_dir
  model_codename <- "RICEx"
  default_year_horizon <- 2200



  ## S3 CLASS EXPOSED METHODS

  me <- list(

    # Define the environment where this list is defined
    thisEnv = thisEnv,

    getEnv = function(){return(get("thisEnv",thisEnv))},

    get_results_dir = function(){ return(get("results_dir",thisEnv))},

    set_results_dir = function(value){return(assign("results_dir",value,thisEnv)) },

    get_default_year_horizon = function(){ return(get("default_year_horizon",thisEnv))},

    get_model_codename = function(){ return(get("model_codename",thisEnv))}

  )


  # Define the value of the list within the current environment.
  assign('this',me,envir=thisEnv)


  class(me) <- append(class(me),"ModelEnvClass")
  return(me)

}

#' Function to update model results folder
#' @export
r50x.SettingsS3 <- ModelEnvClass(rice50x_results_dir = find_project_folder())



#' Function to update model results folder
#' @export
r50x.Settings.create_settings_environment <- function(){

  assign('r50x.SettingsS3', ModelEnvClass(rice50x_results_dir = find_project_folder()),envir = environment())

}





#' Function to update model results folder
#' @param path Full path to your RICE50x results folder
#' @export
r50x.Settings.set_results_folder<-function(path){

  r50x.SettingsS3$set_results_dir(path)
  print(paste0("INFO: RICE50x results folder path updated to <", path,">"))
}
