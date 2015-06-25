#-----------------------------------------------------------------------------#
#' Add a patient's data to a baseplot
#' 
#' @param baseplot the plot to add the geoms
#' @param patient_data an individual patient's data
#' @param title title of the plot
#' @param segment_size width of the segments
#' @param aes_map a list of the aesthetic mappings for the plot
#' @param vline a list of the aesthetics for vline
#' @export
#'
#-----------------------------------------------------------------------------#

plot_patient <- function(baseplot,
                         patient_data,
                         title        = '',
                         aes_map      = aes_map(),
                         vline_map    = vline_aes(),
                         segment_size = 2.5)
{
  out <- baseplot + 
    ## Plots Segments  
    geom_segment(data = patient_data, 
                 aes_string(x     = aes_map$x, 
                            xend  = aes_map$xend, 
                            y     = aes_map$y, 
                            yend  = aes_map$y, 
                            color = aes_map$color,
                            shape = NULL), 
                 size = segment_size,
                 na.rm = TRUE) +
    
    ## Plot points
    geom_point(data = patient_data, 
               aes_string(x     = aes_map$x, 
                          y     = aes_map$y, 
                          shape = aes_map$shape, 
                          size  = aes_map$size,
                          fill  = aes_map$fill),
               na.rm = TRUE)  
    
    # Add up to 5 vertical lines
    # In order to avoid "Warning message: Removed X rows containing missing values"
    # messages I'm using geom_segment(na.rm = T) rather than geom_vline()
    # since geom_vline does not support na.rm.
  
    # TODO: can I do this more elegantly?
  
    for(i in 1:5){
      vl    <- paste0("vline", i)
      vlaes <- vline_map[[vl]]

      if(!is.null(aes_map[[vl]])){
        out <- out + geom_segment(data = patient_data,
                                  aes_string(y     = 0,
                                             yend  = 1,
                                             x     = aes_map[vl],
                                             xend  = aes_map[vl], 
                                             shape = NULL),
                                  size     = vlaes['size'],
                                  color    = vlaes['color'],
                                  linetype = vlaes['linetype'],
                                  na.rm = TRUE)
      }
    } 

    out <- out + ggtitle(title)
  return(out)
}
