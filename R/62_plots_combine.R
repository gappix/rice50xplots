

#' @export
RICExplot.combo.fast_combine <- function(plotlist 
                                        , title = "Combined Plots"
                                        , columns = NULL
                                        , rows = NULL
                                        , common_legend = FALSE
                                        , legend_position = "right"){
  
mytitle = title

# SETUP LAYOUT
# columns have precedence order on rows

  if(!is.null(columns)){   nCol = columns
                           nRow = ceiling(length(plotlist)/nCol)

  } else {     if(!is.null(columns)){    nRow = rows
                                         nCol = ceiling(length(plotlist)/nRow)
                } else { 
                          nCol = ceiling(length(plotlist)/2)
                          nRow = ceiling(length(plotlist)/nCol)
                }
  }


  plottigat = annotate_figure( do.call("ggarrange", c(plotlist, ncol=nCol, nrow=nRow,  common.legend = common_legend , legend=legend_position) ) 
                               , top =  text_grob( mytitle, face = "bold", size = 16)
                              )                               

  return(plottigat)
  
}