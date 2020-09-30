
#' @export
#'
r50x.plots.combo.lineplot_ty <- function( EXPdataList
                                          , title = "RICE50x ty lineplot"
                                          , group_variable
                                          , y_label = "Value"
                                          , x_label = "Year"
                                          , legend_columns = NULL
                                          , legend = "Legend"
                                          , no_legend = FALSE
                                          , LaTeX_text = FALSE
                                          , categories = NULL
                                          , colors_per_category=NULL
                                          , columns  = NULL
                                          , show_confidence = FALSE
                                          , y_lim = NULL
                                          , x_lim = NULL
){




  if(LaTeX_text){  mytitle = TeX(title)  } else {  mytitle = title   }

  ## Prepare single plots
  plotlist    = list()

  for(p in c(1:length(EXPdataList))){


    message( paste0("preparing plot < ",names(EXPdataList)[p]," > ...") )

    EXPdata = r50x.utils.listify_EXPdataframe( EXPdataframe =  EXPdataList[[p]]
                                               , column_to_list = group_variable )

    plottigat = r50x.plots.lineplot_ty(  EXPdata = EXPdata
                                       , title = names(EXPdataList)[p]
                                       , y_label = y_label
                                       , x_label = x_label
                                       , legend_columns = legend_columns
                                       , legend = legend
                                       , LaTeX_text = LaTeX_text
                                       , categories = categories
                                       , colors_per_category = colors_per_category
                                       , show_confidence = show_confidence
    )

    #    if(!is.null(y_lim)) plottigat = plottigat + ylim(y_lim[1], y_lim[2])
    #   if(!is.null(x_lim)) plottigat = plottigat + xlim(x_lim[1], x_lim[2])

    plotlist[[p]] <- local(print(plottigat + theme(legend.position="none") ))

  }



  ## Combine plots
  message( paste0("putting all together..") )



  if(!is.null(columns)){    nCol = columns
  nRow = ceiling(length(EXPdataList)/nCol)

  } else {    nCol = ceiling(length(EXPdata)/2)
  nRow = ceiling(length(EXPdataList)/nCol)  }


  plottigat = ggpubr::annotate_figure( do.call("ggarrange", c(plotlist, ncol=nCol, nrow=nRow,  common.legend = TRUE, legend="right") )
                                       , top =  text_grob( mytitle, face = "bold", size = 16)
  )

  return(plottigat)

}


