library("stringr")


#' Parses gdx name and path and extracts experiment informations
#'
#' @param gdx_name_with_full_path
#' @export
r50x.parse.gdx_experiment <- function(gdx_name_with_full_path){


  ## TEST BOX ............................................................................................................................................................................
  # gdx_name_with_full_path =  "C:/Users/GaPPi/Documents/PhD Projects/RICEx-PROJECT/RICEx-Plots/../RICEx-Results/PaperII//ed57/RICEx_d0x9x1_BAU_ssp1_BHMn-LR.gdx"
  #.......................................................................................................................................................................................


  # storing structure

  main_infos  = c()
  extra_infos = c()

  experiment = list()
  experiment$full_path = gdx_name_with_full_path





  # Extract path components and discard occasonal empty strings
  filename_split = strsplit(gdx_name_with_full_path, "/")
  filename_split = lapply(filename_split, function(z){ z[!is.na(z) & z != ""]})[[1]]





  #### parse infos from path  ---------------------------------



  #...........................................................
  # NOTE: RICEx convention
  # Following path tokens right to left you will encounter:
  # 1- gdx_filename
  # 2- regions aggregation
  # 3- experiment name
  #...........................................................



  ## 1: EXTRACT FILENAME

  gdx_filename =  tail(filename_split,1)
  gdx_filename_noext =substr(gdx_filename, 1, nchar(gdx_filename)-4)

  # remove extracted item
  filename_split <- filename_split[-length(filename_split)]




  ## 2: EXTRACT REGIONS AGGREGATION

  main_infos = c(main_infos, "regions" =  tail(filename_split,1) )

  filename_split <- filename_split[-length(filename_split)]



  ## 3: EXTRACT EXPERIMENT NAMECODE

  main_infos = c(main_infos, "exp_code" =  tail(filename_split,1) )

  filename_split <- filename_split[-length(filename_split)]









  #### parse main infos from filename  ---------------------------------


  #...........................................................
  # NOTE: RICEx convention
  # Following flie_name tokens from left to right you get
  # 1- Model Name
  # 2- model version
  # 3- policy
  # 4- baseline
  # 5- impact
  # 6- cooperation (optional)
  #...........................................................


  # REGEX for complex parsing elements
  # remember:
  # "^" is starting, "$" is ending, "." means exact one charachter
  regex_version = "^v\\d+x\\d+x\\d+|^d\\d+x\\d+x\\d+|^\\d+x\\d+x\\d+"
  regex_baseline= "^ssp\\d"
  regex_coop    = "^coop-\\w+|^noncoop-\\w+"




  # TEST BOX .................................................
  # gdx_filename_noext = "RICEx_d0x9x1_BASE_ssp1_OFF_noncoop-pop"
  #............................................................

  # gdx infos split
  exp_infos = stringr::str_split(gdx_filename_noext, "_")[[1]]



  ## 1a: Fist token should always be model name:
  #      check and remove

  if(exp_infos[1]=="RICEx") exp_infos <- exp_infos[-1]




  ## 2a: Second token should always be model version:
  #      check, add and remove

  main_infos = c(main_infos,  "version"= ifelse( length(exp_infos[grep( stringr::regex(regex_version), exp_infos )]) >0,
                                                 exp_infos[grep( stringr::regex(regex_version), exp_infos )],
                                                 NA)) # add NA if no version is available

  exp_infos <- exp_infos[-grep( stringr::regex(regex_version), exp_infos )]




  ## 3a: first token left should always be policy
  #      add and remove

  main_infos = c(main_infos, "policy"=exp_infos[1])

  exp_infos <- exp_infos[-1]




  ## 4a: extract baseline
  #      add and remove

  main_infos = c(main_infos, "baseline"=exp_infos[grep( stringr::regex(regex_baseline),exp_infos )] )

  exp_infos <- exp_infos[-grep( stringr::regex(regex_baseline),exp_infos)]





  ## 5a: first token left should always be impact
  #      (unless we are running BASE scenario): check, add and remove

  main_infos = c(main_infos, "impacts"=exp_infos[1])

  exp_infos <- exp_infos[-1]





  ## 6a: check cooperation to undestand
  #      whether run_mode is optimization or simulation

  main_infos = c(main_infos,  "cooperation"= ifelse( length(exp_infos[grep( stringr::regex(regex_coop), exp_infos )]) >0,
                                                     exp_infos[ grep( stringr::regex(regex_coop), exp_infos )],
                                                     NA)) # add NA if no version is available

  exp_infos <- exp_infos[-grep( stringr::regex(regex_coop),exp_infos)]

  if(is.na(main_infos["cooperation"])){  main_infos = c(main_infos,  "run_mode"=  "sim")
  }  else {  main_infos = c(main_infos,  "run_mode"=  "opt") }




  experiment$main_infos = main_infos





  #### parse extra infos  ---------------------------------


  #....................................................................
  # NOTE: RICEx convention
  # Tokens left (if any) show non-default configurations (param-value)
  #....................................................................

  if(length(exp_infos) > 0 ) {

    for(tok in exp_infos){

      # split token as key-value
      split_tok = stringr::str_split(tok, '-')[[1]]

      temp = c(split_tok[2])
      names(temp) = split_tok[1]

      extra_infos = c(extra_infos, temp)

    }

    experiment$extra_infos = extra_infos

  }else{ experiment$extra_infos = NA }










  ##### data extracted in RICEx S3 class ------------------------


  experiment$data  = RICE50xS3Class(gdx_name_with_full_path)




  return(experiment)

}








