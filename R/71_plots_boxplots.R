library("latex2exp")


RICExplot.boxplot   <- function( EXPdata, 
                                  title,
                                  legend,
                                  yLabel,
                                  xLabel = "",
                                  LaTeX_text = FALSE,
                                  categories = NULL, 
                                  colors_per_category=NULL ){
  
  
  
  ##  Setup informations .................
  mytitle  =  title
  myYlabel =  yLabel
  myXlabel =  xLabel
  myLegend =  legend
  
  
  
  # Create dataframe
  mydf   <- cbind(cat=rep(names(EXPdata),sapply(EXPdata,nrow)),do.call(rbind,EXPdata))
  
  
  # Create legend names and colors associated
  if(is.null(categories)){ncolors = length(EXPdata)}else{ncolors=NULL}

  my_legend_elements_with_colors = c("black"     #BASE      
                                     ,"black"    #BAU nodmg
                                     ,"#707070"  #BAU dmg
                                     ,"black"    #reference
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
  
  
  my_unit = tryCatch( (EXPdata[[1]] %>% select("unit"))[[1]][1], error=function(cond){return("")})
  
  
  
  
  # Plotter .......................
  
  plottigat = ggplot(data = mydf, aes(cat,value, color=cat)) + 
    
    
    
    # MY DATA
    
    geom_boxplot(size = 1.1)   +
    
    stat_summary(fun.y = mean, geom = "errorbar", 
                 aes(ymax = ..y.., ymin = ..y..),
                 width = 1, linetype = "dashed", size = 1, color = "black") +
    
    stat_summary(fun.y=mean,  geom="text",
                 aes( label= paste0(round(..y.., digits=1),my_unit)),
                 show.legend = FALSE, 
                 color = "black",
                 vjust=-0.7)
  
  # graphic details
  if(LaTeX_text){ 
    
    myexp_labels       = sapply(names(my_legend_elements_with_colors),TeX)
    names(myexp_labels) = names(my_legend_elements_with_colors)
    
    
    plottigat = plottigat +
      
      scale_color_manual( breaks = names(my_legend_elements_with_colors),
                          labels = lapply(names(my_legend_elements_with_colors),TeX),
                          values = my_legend_elements_with_colors ) +  
      
      scale_x_discrete( labels= myexp_labels ) +
                        
                      
      
      labs (color = TeX(myLegend)) + 
      ggtitle(TeX(mytitle)) + 
      xlab(TeX(myXlabel)) +
      ylab(TeX(myYlabel))  
    
  }else{
    
    plottigat = plottigat +
      
      scale_color_manual( breaks = names(my_legend_elements_with_colors),
                          labels = names(my_legend_elements_with_colors),
                          values = my_legend_elements_with_colors ) +
      
      labs (color = myLegend) + 
      ggtitle(mytitle) + 
      xlab(myXlabel) +
      ylab(myYlabel) 
    
    
  } 
  
  # Aestetics
  plottigat = plottigat +
    
    guides(colour=guide_legend(ncol=1)) +
    
    theme( legend.text   = element_text(size = 13),
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

