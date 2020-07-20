#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# 
#  
#  
# 
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



## -----------------   Source files   ---------------------------------


# library(gridExtra)
library("ggpubr")










## ____ FUNCTION ____
RICExplot.lineplot_nty <- function(EXPdata 
                                  , title = "RICEx nty lineplot"
                                  , y_label = "Value"
                                  , x_label = "Year"
                                  , legend_columns = NULL
                                  , region_code = "ed57"
                                  , LaTeX_text = FALSE
                                  , no_legend = FALSE   
                                ){
  
  ## Data preparation 
  mydata = EXPdata
  mytitle = title
  myylabel = y_label
  myxlabel = x_label
  myregioncode = region_code


  # number of columns needed to show all legends
  if(is.null(legend_columns)){   mylegendcolumns = ifelse(length(EXPdata) < 14, 1, 2) 
  } else { mylegendcolumns = legend_columns }
  



  ## Plotter
  plottigat = ggplot() + 
    
    
    geom_line(data= mydata,
              aes( x=year, y=value, group=n, color =n ),
              size = 1.1) + 
    

    scale_color_manual( values = colorize_regions(myregioncode)  ) + 
    

    guides( colour = guide_legend(ncol=mylegendcolumns), 
            fill   = guide_legend(ncol=mylegendcolumns)  ) +
    

    theme( legend.position=if(no_legend){"none"} else {"right"} ,
           plot.title = element_text(size=12),
           axis.title.x = element_text(size = 12),
           axis.text.x = element_text(size = 12),
           axis.title.y = element_text(size = 12))   
    


  # graphic details
  if(LaTeX_text){   plottigat = plottigat +       
                                  ggtitle(TeX(mytitle)) +
                                  xlab(TeX(myxlabel))   +
                                  ylab(TeX(myylabel))   
  
  }else{  plottigat = plottigat +     
                        ggtitle(mytitle) + 
                        xlab(myxlabel)   +
                        ylab(myylabel) 
  } 
     
    
    
 


  return(plottigat)
}










## ____ FUNCTION ____
RICExplot.combo.lineplot_nty <- function( EXPdata
                                        , title = "RICEx nty lineplot" 
                                        , y_label = "Value"
                                        , x_label = "Year"
                                        , region_code = "ed57"
                                        , legend_columns = NULL
                                        , no_legend = FALSE
                                        , LaTeX_text = FALSE
                                        , columns  = NULL 
                                        , y_lim = NULL
                                        , x_lim = NULL
                                    ){
  



  if(LaTeX_text){  mytitle = TeX(title)  } else {  mytitle = title   } 

  ## Prepare single plots
  plotlist    = list()

  for(p in c(1:length(EXPdata))){
    

    message( paste0("preparing plot < ",names(EXPdata)[p]," > ...") )
    
    plottigat = RICExplot.lineplot_nty(  EXPdata = EXPdata[[p]] 
                                        , title = names(EXPdata)[p] 
                                        , y_label = y_label
                                        , x_label = x_label
                                        , region_code = region_code
                                        , legend_columns = legend_columns
                                        , LaTeX_text = LaTeX_text
                                    ) 
    
#    if(!is.null(y_lim)) plottigat = plottigat + ylim(y_lim[1], y_lim[2])
 #   if(!is.null(x_lim)) plottigat = plottigat + xlim(x_lim[1], x_lim[2])
    
    plotlist[[p]] <- local(print(plottigat + theme(legend.position="none") ))
    
  }



  ## Combine plots 
  message( paste0("putting all together..") )



  if(!is.null(columns)){    nCol = columns
                            nRow = ceiling(length(EXPdata)/nCol)

  } else {    nCol = ceiling(length(EXPdata)/2)
              nRow = ceiling(length(EXPdata)/nCol)  }


  plottigat = annotate_figure( do.call("ggarrange", c(plotlist, ncol=nCol, nrow=nRow,  common.legend = TRUE, legend="right") ) 
                               , top =  text_grob( mytitle, face = "bold", size = 16)
                              )                               

  return(plottigat)

}




## ____ FUNCTION ____
# to evaluate multiple MAPS sharing the same dimensions and legend
# on the same chart
RICExplot.combo.map <- function( EXPdata 
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
