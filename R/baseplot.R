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

start_baseplot <- function(data,
                     aes_map       = aes_map(),
                     axis.lim.x    = NULL,
                     axis.name.x   = '',
                     axis.breaks.x = NULL,
                     axis.labels.x = NULL,
                     axis.name.y   = '',
                     axis.breaks.y = NULL,
                     axis.labels.y = NULL)
{ 
  # Warnings #
  if(min(data[, aes_map$y], na.rm = T) < 0  | 
     max(data[, aes_map$y], na.rm = T) > 1){
    warning('Y values must be between 0 and 1 (inclusive)')
  }
  # Create a dataset from which to build the plot
  # Need one observation for each unique combination
  basedt <- expand.grid(x = 0, 
                        y     = unique(data[, aes_map$y]),
                        color = unique(data[, aes_map$color]),
                        size  = unique(data[, aes_map$size]),
                        shape = unique(data[, aes_map$shape]),
                        fill  = unique(data[, aes_map$fill]))
  
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
  
  # Base plot 
  p <- ggplot(basedt, aes_string(x = 'x', 
                                 y = 'y', 
                                 color = 'color', 
                                 shape = 'shape',
                                 size  = 'size')) + 
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
                       labels = axis.labels.y) 
  
  ## Done ##
  return(p)
}

#-----------------------------------------------------------------------------# 
#' Map aesthetic scales for a patient profile
#' 
#' @param p a ggplot object
#' 
#-----------------------------------------------------------------------------# 

scale_baseplot <- function(p,
     color.values = brewer_pal('qual', pal = 2)(length(levels(p$data$color))),
     color.guide  = FALSE,
     size.values  = (1:length(levels(p$data$size)))/length(levels(p$data$size)),
     size.guide   = FALSE,
     shape.values = 1:length(levels(p$data$shape)),
     shape.guide  = FALSE,
     fill.values  = rep('white', length(levels(p$data$color))),
     fill.guide   = FALSE)
{ 
  ## Warnings ##
  # TODO
  
  out <- p +
    ## Scales ##
    scale_color_manual(values = color.values,  guide = color.guide) +
    scale_size_manual(values  = size.values,   guide = size.guide ) +
    scale_shape_manual(values = shape.values,  guide = shape.guide) +
    scale_fill_manual(values  = fill.values,   guide = fill.guide)
  
  return(out)
}

#-----------------------------------------------------------------------------# 
#' Theme a patient profile
#' 
#' Adds a theme to a ggplot object. Not very useful at the moment
#' 
#' @param p a ggplot object
#' 
#-----------------------------------------------------------------------------# 

theme_baseplot <- function(p){
  p <- p + 
    theme_classic()
  return(p)
}

#-----------------------------------------------------------------------------# 
#' Make the final baseplot
#' 
#' @param dt data frame
#' @param ... list of additional arguments
#' @export
#' 
#-----------------------------------------------------------------------------# 

make_baseplot <- function(data, ...)
{
  dots <- list(...)
  
  # Start the baseplot
  base_args <- append(list(data), get_args(start_baseplot, dots))
  out <- do.call(start_baseplot, args = base_args)
  
  # Add scales
  scale_args <- append(list(p = out), get_args(scale_baseplot, dots))
  out <- do.call(scale_baseplot, args = scale_args)
  
  # Theme the baseplot
  out <- theme_baseplot(out)
  
  return(out)           
}