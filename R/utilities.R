#-----------------------------------------------------------------------------#
#' Split dataset into geoms
#' 
#'
#' @param dt dataset to split
#' @export
#-----------------------------------------------------------------------------#

geom_datasets <- function(dt){
  out <- split(dt, f = dt$geom)
  return(out)
}

#-----------------------------------------------------------------------------#
#' Split dataset into geoms
#' 
#'
#' @param dt dataset to split
#' @export
#-----------------------------------------------------------------------------#

geom_datasets <- function(dt){
  out <- split(dt, f = dt$geom)
  return(out)
}

#-----------------------------------------------------------------------------#
#' Create an aesthetic mapping for a Patient Profile
#' 
#'
#' @param fileBy variable to group files by
#' @param id the subject id variable
#' @export
#-----------------------------------------------------------------------------#

aes_map <- function(fileBy = 'id',
                    id     = 'id',
                    y      = 'y',
                    x      = 'x',
                    xend   = 'xend',
                    group  = 'group',
                    
                    color  = 'color',
                    color.values = NULL,
                    color.guide  = FALSE,
                    
                    shape  = 'shape',
                    shape.values = NULL,
                    shape.guide  = FALSE,
                    
                    size   = 'size',
                    size.values  = NULL,
                    size.guide   = FALSE,
                    
                    fill   = 'fill',
                    fill.values  = NULL,
                    fill.guide   = FALSE,
                    
                    alpha  = 'alpha',
                    alpha.values  = NULL,
                    
                    linetype = 'linetype',
                    linetype.values = NULL,
                    label   = 'label')
{
  out <- list(fileBy = fileBy,
              id     = id,
              y      = y,
              x      = x,
              xend   = xend,
              group  = group,
              color  = list(variable = color,
                            values   = color.values,
                            guide    = color.guide),
              size   = list(variable = size,
                            values   = size.values,
                            guide    = size.guide),
              shape  = list(variable = shape,
                            values   = shape.values,
                            guide    = shape.guide),
              fill   = list(variable = fill,
                            values   = fill.values,
                            guide    = fill.guide),
              alpha  = list(variable = alpha,
                            values   = alpha.values),
              linetype = list(variable = linetype,
                              values   = linetype.values),
              label    = label )
  return(out)
}
