#-----------------------------------------------------------------------------#
#' Create the underlying plot upon which to build a patient profile
#' 
#' @param data the dataset
#' @param aes_map a list of the aesthetic mappings for the plot
#' @param axis.lim.x vector of length two with lower and upper bounds of x axis
#' @param axis.name.x character string of x axis title
#' @param axis.breaks.x vector containing locations for x axis labels
#' @param axis.labels.x vector (must be same length as axis.breaks.x) with labels 
#' for the yaxis
#' @param axis.name.y character string of y axis title
#' @param axis.breaks.y vector containing locations for y axis labels
#' @param axis.labels.y vector (must be same length as axis.breaks.y) with labels 
#' for the y axis
#' @export
#'
#-----------------------------------------------------------------------------# 

make_baseplot <- function(data,
                     aes_map       = aes_map(),
                     axis.lim.x    = NULL,
                     axis.name.x   = '',
                     axis.breaks.x = NULL,
                     axis.labels.x = NULL,
                     axis.name.y   = '',
                     axis.breaks.y = NULL,
                     axis.labels.y = NULL
                     )
{ 
  # Warnings #
  if(min(data[, aes_map$y], na.rm = T) < 0  | 
     max(data[, aes_map$y], na.rm = T) > 1){
    warning('Y values must be between 0 and 1 (inclusive)')
  }
  
  # Set limits
  if(is.null(axis.lim.x)){
    axis.lim.x <- c(min(data[, aes_map$x],   na.rm = T), 
                    max(data[, aes_map$xend], na.rm = T))
  }
  
  # set y scale information
  if(is.null(axis.breaks.y)){
    axis.breaks.y <- unique(data[, aes_map$y])
  }
  if(is.null(axis.labels.y)){
    axis.labels.y <- unique(data[, aes_map$y])
  }
  
  ## Set Scale Defaults ##
  if(is.null(aes_map$color$values)){
    color.values <- brewer_pal('qual', pal = 2)(length(levels(data$color)))
  } else {
    color.values <- aes_map$color$values
  }
  
  if(is.null(aes_map$shape$values)){
    shape.values <- 1:length(levels(data$shape))
  } else {
    shape.values <- aes_map$shape$values
  }
  
  if(is.null(aes_map$size$values)){
    size.values <-  (1:length(levels(data$size)))/length(levels(data$size))
  } else {
    size.values <- aes_map$size$values
  }
  
  if(is.null(aes_map$fill$values)){
    fill.values <-  rep('white', length(levels(data$fill)))
  } else {
    fill.values <- aes_map$fill$values
  }
  
  if(is.null(aes_map$alpha$values)){
    alpha.values <-  rep(.2, length(levels(data$alpha)))
  } else {
    alpha.values <- aes_map$alpha$values
  }
  
  if(is.null(aes_map$linetype$values)){
    ltype.values <-  rep('solid', length(levels(data$linetype)))
  } else {
    ltype.values <- aes_map$linetype$values
  }
  
  
  #### Make Base plot ####
  p <- ggplot(data, aes_string(x = 'x', 
                                 y = 'y', 
                                 color = 'color', 
                                 size  = 'size',
                                 shape = 'shape',
                                 fill  = 'fill',
                                 alpha = 'alpha',
                                 linetype = 'linetype'
                                 )) + 
    ## Create a blank plot ##
    geom_blank() +

    ## Axes ##
    coord_cartesian(xlim = axis.lim.x,
                    ylim =c(0, 1)) +
    
    scale_x_continuous(name   = axis.name.x,
                       breaks = axis.breaks.x, 
                       labels = axis.labels.x) +
    scale_y_continuous(name   = axis.name.y, 
                       breaks = axis.breaks.y, 
                       labels = axis.labels.y) +

    ## Scales ##
    scale_color_manual(values = color.values) +
    scale_size_manual(values  = size.values) + 
    scale_shape_manual(values = shape.values) +
    scale_fill_manual(values  = fill.values) +
    scale_alpha_manual(values = alpha.values) +
    scale_linetype_manual(values = ltype.values) + 
    
    ## Theme ##
    theme_classic() + 
    
    ## Remove legends ##
    theme(legend_position = 'none')
  
  return(p)
}