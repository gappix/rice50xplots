

library("ggplot2")
library("latex2exp")

RICExplot.lineplot  <- function( EXPdata
                                 , title
                                 , legend = "Legend"
                                 , y_label
                                 , x_label = "Year"
                                 , LaTeX_text = FALSE
                                 , categories = NULL
                                 , colors_per_category=NULL
                                 , legend_columns = NULL
                                 , yLabel = NULL # LEGACY: Deprecated now
                                 , xLabel = NULL  # LEGACY: Deprecated now
                                 , show_confidence = FALSE
                                ){

  #LEGACY..................................
  if(!is.null(yLabel)) y_label = yLabel
  if(!is.null(xLabel)) x_label = xLabel
  #........................................


  ##  Setup informations .................

  mytitle  =  title
  myYlabel =  y_label
  myXlabel =  x_label
  myLegend =  legend


  # Number of columns needed to show all legends
  if(is.null(legend_columns)){ legend_columns = ifelse(length(EXPdata) < 13, 1, 2) }


  # Create dataframe
  mydf   <- cbind(cat=rep(names(EXPdata),sapply(EXPdata,nrow)),do.call(rbind,EXPdata))


  # Create legend names and colors associated
  if(is.null(categories)){ncolors = length(EXPdata)}else{ncolors=NULL}

  my_legend_elements_with_colors = c("black"     #BASE
                                     ,"black"    #BAU nodmg
                                     ,"#707070"  #BAU
                                     ,"#black"   #reference
                                     ,"#707070"  #BAU dam
                                     ,"#707070"  #BAUdmg
                                     ,"#707070"  #BAUdam
                                     ,"#996633"  #Historical
                                     ,"#996633"  #Hist
                                     , i_want_pollos_colors( howmany_colors        = ncolors
                                                             , howmany_categories  = categories
                                                             , colors_per_category = colors_per_category ))

  names(my_legend_elements_with_colors) = c( "BASE"
                                            ,"BAU nodmg"
                                            ,"BAU"
                                            ,"Reference"
                                            ,"BAU dam"
                                            ,"BAUdmg"
                                            ,"BAUdam"
                                            ,"Historical"
                                            ,"Hist"
                                            , names(EXPdata)[!names(EXPdata) %in% c( "BASE"
                                                                                    ,"BAU nodmg"
                                                                                    ,"BAU"
                                                                                    ,"Reference"
                                                                                    ,"BAU dam"
                                                                                    ,"BAUdmg"
                                                                                    ,"BAUdam"
                                                                                    ,"Historical"
                                                                                    ,"Hist"
                                                                                    )])

  my_legend_elements_with_colors = my_legend_elements_with_colors[names(EXPdata) ]




  ## Plotter .......................

  plottigat = ggplot() +

    geom_line(data = mydf, aes(year,value, color=cat), size = 1.2)

  if(show_confidence){

    plottigat = plottigat + geom_ribbon(data = mydf
                                        , aes( x =year
                                              , ymin=conf_min
                                              , ymax=conf_max
                                              , fill = cat ), alpha=0.3)
  }

  # graphic details
  if(LaTeX_text){
    plottigat = plottigat +

    scale_color_manual( breaks = names(my_legend_elements_with_colors),
                        labels = lapply(names(my_legend_elements_with_colors),TeX),
                        values = my_legend_elements_with_colors ) +

    scale_fill_manual( breaks = names(my_legend_elements_with_colors),
                        labels = lapply(names(my_legend_elements_with_colors),TeX),
                        values = my_legend_elements_with_colors ) +

    labs (color = TeX(myLegend), fill = TeX(myLegend)) +
    ggtitle(TeX(mytitle)) +
    xlab(TeX(myXlabel)) +
    ylab(TeX(myYlabel))

  }else{

    plottigat = plottigat +

    scale_color_manual( breaks = names(my_legend_elements_with_colors),
                        labels = names(my_legend_elements_with_colors),
                        values = my_legend_elements_with_colors ) +

    scale_fill_manual(  breaks = names(my_legend_elements_with_colors),
                        labels = names(my_legend_elements_with_colors),
                        values = my_legend_elements_with_colors ) +

    labs (color = myLegend, fill = myLegend) +
    ggtitle(mytitle) +
    xlab(myXlabel) +
    ylab(myYlabel)


  }

  # aestetics
  plottigat = plottigat +

        guides(colour=guide_legend(ncol=legend_columns)) +

        theme( legend.text   = element_text(size = 13),# margin = margin(t = 10)),
            legend.spacing.y = unit(10.0, 'mm'),
            legend.key.size = unit(1.5,"line"),
            legend.text.align = 0,
            legend.title  = element_text(size = 16),
            plot.title    = element_text(size = 16),
            axis.title.x  = element_text(size = 16),
            axis.text.x   = element_text(size = 14),
            axis.title.y  = element_text(size = 16)  )


  return(plottigat)
}




## ____ FUNCTION ____
# to evaluate multiple MAPS sharing the same dimensions and legend
# on the same chart
RICEx.plot.multi_nty_linechart <- function( EXPdata
                                ,legend
                                ,title
                                ,palette  = NULL
                                ,shape    = NULL
                                ,min_data = NULL
                                ,max_data = NULL
                                ,columns  = NULL
                                ,legend_symmetric = TRUE
                                ,legend_centre    = 0
                                ,LaTeX_text = FALSE
                            ){




  # Retrieve general informations
  myTitle   =  title
  myLegend  =  legend

  myShape   =  shape

  myPalette =  palette
  # Default palette adopted
  if(is.null(myPalette)) myPalette = rev(RColorBrewer::brewer.pal(9, "RdBu"))

  # General informations
  mydf  <- cbind(cat=rep(names(EXPdata),sapply(EXPdata,nrow)),do.call(rbind,EXPdata))
  min_measure = min(mydf$value)
  max_measure = max(mydf$value)

  if(is.null(min_data)){
    if(legend_symmetric){ min_data = min(c(min_measure, legend_centre-max_measure))
    }else{  min_data = min_measure   }
  }
  if(is.null(max_data)){
      if(legend_symmetric){ max_data = max(c(max_measure+legend_centre, -min_measure))
    }else{ max_data = max_measure  }
  }

  if(!legend_symmetric){ legend_centre = (abs(max_measure)-abs(min_measure))/2 }
  # Check df correctness
  #if("year" %in% colnames(mydf)) if(nrow(mydf %>% dplyr::select("year") %>% distinct() ) > 1) stop("More than one year found! No temporal dimension in maps!")


  # Get number of plots
  nplots     = length(EXPdata)
  map_layout = get_map_plots_layout(nplots)


  plotlist    = list()
  plotlegend  = NULL


  # Alternative approach (much slower! )
  # plotlist <- lapply(names(EXPdata), RICEx.plot.map
  #                    ,data     = mydf
  #                    ,shape    = myShape
  #                    ,legend   = myLegend
  #                    ,palette  = myPalette
  #                    ,min_data = min_data
  #                    ,max_data = max_data)
  #
  # plottigat = do.call("grid.arrange", c(plotlist, ncol=floor(sqrt(nplots))))


  for(p in c(1:length(EXPdata))){


    message( paste0("preparing plot < ",names(EXPdata)[p]," > ...") )


    # stupid guessing for most used shape mappings
    if(is.null(myShape)){
      regions = nrow(EXPdata[[p]]%>%select(n)%>%distinct())
      if (regions > 50)         myShape = ed57shp
      else if(regions >30)    myShape = ed35shp
           else   if(regions > 16)  myShape = wt17shp
                  else    if(regions > 4)   myShape = r5shp
                          else  if(regions < 2)   myShape = global1shp
                                else stop(paste0("please specify shape mapping for ", names(EXPdata)[p], " regions!"))
    }
    # Plottigat ...........................
    plottigat = RICEx.plot.map( data     = EXPdata[[p]]
                               ,title    = names(EXPdata)[p]
                               ,LaTeX_text = LaTeX_text
                               ,shape    = myShape
                               ,legend   = myLegend
                               ,palette  = myPalette
                               ,min_data = min_data
                               ,max_data = max_data
                               ,centre_data = legend_centre )

    # if(p == 1){plotlegend <- get_plotlegend(plottigat)}
    plotlist[[p]] <- local(print(plottigat  + theme(legend.position="none")))
    myShape = NULL
  }

  message( paste0("putting all together (maps are an hard job, it may take some MINUTES!)") )
  nRow = ceiling(nplots/2)
  nCol = ceiling(nplots/nRow)

  if(!is.null(columns)){

    nCol = columns
    nRow = ceiling(nplots/nCol)

  }

  annotate_figure(do.call("ggarrange", c(plotlist, ncol=nCol, nrow=nRow,  common.legend = TRUE, legend="right"))
                  ,top =  text_grob(myTitle, face = "bold", size = 16)
  )



}
# RICEx.plot.lineplot <- function(EXPdata, EXPtitle, EXPylabel, EXPlegend){

# mytitle  =  EXPtitle
# myYlabel =  EXPylabel
# myLegend =  EXPlegend

# mydf   <- cbind(cat=rep(names(EXPdata),sapply(EXPdata,nrow)),do.call(rbind,EXPdata))

# plottigat = ggplot(mydf, aes(year,value, color=cat)) + geom_line(size = 1.2)+

#   # graphic details
#   guides(colour=guide_legend(ncol=1)) +
#   scale_color_manual(breaks=names(EXPdata),
#                      values = c("black", pollo_20
#                      )) +


#   theme( legend.text   = element_text(size = 13),
#          legend.title  = element_text(size = 16),
#          plot.title    = element_text(size = 18),
#          axis.title.x  = element_text(size = 16),
#          axis.text.x   = element_text(size = 14),
#          axis.title.y  = element_text(size = 16)  ) +

#   labs (color = myLegend) +
#   ggtitle(mytitle) +
#   xlab("Year") +
#   ylab(myYlabel)  ;

# return(plottigat)
# }





RICEx.plot.lineplot_compare <- function(EXPdata, EXPtitle, EXPylabel, EXPlegend,
COMPAREdata){

# Prepare data

mytitle  =  EXPtitle
myYlabel =  EXPylabel
myLegend =  EXPlegend

CMPdf    = as.data.frame(COMPAREdata$df)
CMPx     = as.character(COMPAREdata$x)
CMPy     = as.character(COMPAREdata$y)
CMPgroup = as.character(COMPAREdata$group)
CMPcolor = as.character(COMPAREdata$color)

mydf   <- cbind(cat=rep(names(EXPdata),sapply(EXPdata,nrow)),do.call(rbind,EXPdata))

## Plot

plottigat = ggplot() +

geom_line(data = CMPdf, aes(x = paste0(CMPx), y = paste0(CMPy), group = paste0(CMPgroup), color = CMPcolor), size = 1) +

geom_line(data = mydf, aes(year,value, color=cat), size = 1.2) +

  # graphic details
  guides(colour=guide_legend(ncol=1)) +
  scale_color_manual(breaks=names(EXPdata),
                     values = c("black", wes_palette(name = "Darjeeling1"), wes_palette(name = "GrandBudapest2"),




                       "#f77f2f", #orange
                       "#db0f20",
                       "red",      # red

                       "#d64cf5", # pink

                       "#8232c7", #violet
                       "#450094",

                       "blue",   #blue
                       "#0f86db",
                       "#0ccfab",

                       "green",  #greens

                       "#32c738",
                       "#046918",



                       "#c4570e",  # brown
                       "#fff100",  #yellow
                       "#9c4317"
                     )) +


  theme( legend.text   = element_text(size = 13),
         legend.title  = element_text(size = 16),
         plot.title    = element_text(size = 18),
         axis.title.x  = element_text(size = 16),
         axis.text.x   = element_text(size = 14),
         axis.title.y  = element_text(size = 16)  ) +

  labs (color = myLegend) +
  ggtitle(mytitle) +
  xlab("Year") +
  ylab(myYlabel)  ;

return(plottigat)
}


