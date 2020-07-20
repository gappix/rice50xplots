


RICEx.plot.lineplot.CTAX  <- function(   EXPdata
                                        ,ADVANCE_MODELS
                                        ,HISTORICAL = NULL
                                        ,title  = "World Emissions under CTAX"
                                        ,mylegend = "Experiments"
                                        ,yLabel = "World Emissions [GtCO2]"
                                        ,xLabel = "Year"
                                        ,categories = NULL
                                        ,colors_per_category=NULL ){




  
  # number of columns needed to show all legend names
  legendColumns = ifelse(length(EXPdata) < 13, 1, 2) 
  
  
  # Create dataframe
  mydf   <- cbind(cat=rep(names(EXPdata),sapply(EXPdata,nrow)),do.call(rbind,EXPdata))
  
  
  # Create legend names and colors associated
  if(is.null(categories)){ncolors = length(EXPdata)}else{ncolors=NULL}
  
  my_legend_elements_with_colors = c("grey","black","black",  
                                     i_want_pollos_colors( howmany_colors      = ncolors,
                                                           howmany_categories  = categories,
                                                           colors_per_category = colors_per_category ))
  
  names(my_legend_elements_with_colors) = c("ADVANCE","MEDIAN","Historical",names(EXPdata)[!names(EXPdata) %in% c("ADVANCE","MEDIAN","Historical")])
  
  
  # Plotter .......................
  
    plottigat = ggplot() + 
    
    #ADVANCE
    geom_line(data= ADVANCE_MODELS,
              aes(x=year, y=value, group = model, color = "ADVANCE"),
              size = 0.8)  +
    
    geom_smooth(data= ADVANCE_MODELS,
                aes(x=year, y=value,  color = "MEDIAN"),
                method="auto",
                size = 1.1,  linetype = "dashed", se = T) +
    

    # MY DATA
    geom_line(data = mydf, aes(year,value, color=cat), size = 1.2)    
    

    # HISTORICAL DATA (Optional)
    if(!is.null(HISTORICAL)){
    plottigat = plottigat + 

        geom_line(data = HISTORICAL%>%filter(year >=1990), aes(year,value, color="Historical"), size = 1.2)

    }
    



    # GRAPHIC DETAILS
    plottigat = plottigat + 

    guides(colour=guide_legend(ncol=legendColumns)) +

    scale_color_manual( 
                        breaks = names(my_legend_elements_with_colors),
                        values = my_legend_elements_with_colors   
                       ) +


    theme( legend.text   = element_text(size = 12),# margin = margin(t = 10)),
           #legend.spacing.y = unit(10.0, 'mm'),
           
           legend.title  = element_text(size = 16),
           plot.title    = element_text(size = 16),
           axis.title.x  = element_text(size = 16),
           axis.text.x   = element_text(size = 14),
           axis.title.y  = element_text(size = 16)  ) +
    
    labs (color = mylegend) + 
    ggtitle(title) + 
    xlab(xLabel) +
    ylab(yLabel)  ; 
  


  
  return(plottigat)




}
