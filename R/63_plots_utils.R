





#' function to generate list from dataframe, based on specific value
#' @export
r50x.utils.listify_EXPdataframe <- function(EXPdataframe, column_to_list){

my_list = list()

for(i in unique(EXPdataframe[[column_to_list]])){


  my_list[[i]] = EXPdataframe %>% filter((!!as.symbol(column_to_list)) == i) %>% select(-c(!!column_to_list))
}

return(my_list)

}
