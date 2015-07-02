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
                         segment_size = 3.5)
{
  geom_data <- geom_datasets(patient_data)
  
  #### Make Plot ####
  out <- baseplot
    # Plot Areas ##
    if(!is.null(geom_data$area)){
      out <- out + geom_rect(data = geom_data$area, 
                             aes_string(x    = aes_map$x,
                                        xmin = aes_map$x,
                                        xmax = aes_map$xend,
                                        ymin = 0,
                                        ymax = 1,
                                        shape = NULL,
                                        alpha = aes_map$alpha$variable,
                                        color = aes_map$color$variable),
                             size = 0)
    }
  
    ## Plot Segments ##
    if(!is.null(geom_data$segment)){
      out <- out +    
        geom_point(data = geom_data$segment, 
                   aes_string(x     = aes_map$xend, 
                              y     = aes_map$y, 
                              shape = aes_map$shape$variable,
                              color = aes_map$color$variable,
                              size  = aes_map$size$variable,
                              fill  = aes_map$fill$variable,
                              alpha = NULL),
                   na.rm = TRUE)  + 
        
        geom_segment(data = geom_data$segment, 
                     aes_string(x     = aes_map$x, 
                                xend  = aes_map$xend, 
                                y     = aes_map$y, 
                                yend  = aes_map$y, 
                                color = aes_map$color$variable,
                                shape = NULL,
                                alpha = NULL,
                                linetype = NULL), 
                     size = segment_size,
                     na.rm = TRUE)
    }
 
    ## Plot lines ##
    if(!is.null(geom_data$line)){
      out <- out +
        geom_line(data = geom_data$line,
                  aes_string(x     = aes_map$x,
                             y     = aes_map$y,
                             group = aes_map$group,
                             shape = NULL,
                             alpha = NULL),
                  color = 'black',
                  linetype = 'solid',
                  size = 1) + 
        geom_point(data = geom_data$line, 
                  aes_string(x     = aes_map$x, 
                             y     = aes_map$y, 
                             shape = aes_map$shape$variable,
                             color = aes_map$color$variable,
                             size  = aes_map$size$variable,
                             fill  = aes_map$fill$variable,
                             alpha = NULL),
                  na.rm = TRUE) 
    }


  
    ## Plot Vlines ##
  if(!is.null(geom_data$vline)){
    out <- out + geom_vline(data = geom_data$vline, 
                           aes_string(xintercept = aes_map$x,
                                      y    = 0,
                                      yend = 1,
                                      shape = NULL,
                                      color = aes_map$color$variable,
                                      linetype = aes_map$linetype$variable,
                                      alpha = NULL
                                      )) + 
      geom_text(data = geom_data$vline,
                aes_string(x = aes_map$x,
                           y = .5,
                           label = aes_map$label, 
                           shape = NULL,
                           alpha = NULL),
                size = 4,
                angle = 270,
                vjust = 0,
                color = 'black')
    }
  


    out <- out + ggtitle(title)
  return(out)
}
