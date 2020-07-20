
#' @export
RICEx.plot.export_fullpage <- function(plottigat){
  
  windows(1150, 600, pointsize = 10) #opens a separate window with the size you want 
  plottigat
  savePlot("clipboard", type="wmf") #saves plot to WMF
  
}
