#-----------------------------------------------------------------------------#
#' Split dataset into geoms
#'
#' @param dt dataset to split
#' @export
#' 
#-----------------------------------------------------------------------------#

geom_datasets <- function(dt){
  out <- split(dt, f = dt$geom)
  return(out)
}

#-----------------------------------------------------------------------------#
#' Create an aesthetic mapping for a Patient Profile
#'
#' @param fileBy variable to group files by
#' @param id the subject id variable
#' @param y character string naming the y variable
#' @param x character string naming the x variable
#' @param xend character string naming the xend variable
#' @param color character string naming the color variable
#' @param color.values vector of color values
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
                    
                    shape  = 'shape',
                    shape.values = NULL,
                    
                    size   = 'size',
                    size.values  = NULL,
                    
                    fill   = 'fill',
                    fill.values  = NULL,
                    
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
                            values   = color.values),
              size   = list(variable = size,
                            values   = size.values),
              shape  = list(variable = shape,
                            values   = shape.values),
              fill   = list(variable = fill,
                            values   = fill.values),
              alpha  = list(variable = alpha,
                            values   = alpha.values),
              linetype = list(variable = linetype,
                              values   = linetype.values),
              label    = label )
  return(out)
}

#-----------------------------------------------------------------------------#
#' Split dataset into geoms
#' 
#'
#' @param dt full dataset of all patients' data
#' @param aes_map see \code{\link{aes_map}}
#' @export
#-----------------------------------------------------------------------------#

aes_dataset <- function(dt, aes_map){
  cl <- sort(unique(dt[aes_map$color$variable])[ , 1])
  sh <- sort(unique(dt[aes_map$shape$variable])[ , 1])
  sz <- sort(unique(dt[aes_map$size$variable])[ , 1])
  fl <- sort(unique(dt[aes_map$fill$variable])[ , 1])
  al <- sort(unique(dt[aes_map$alpha$variable])[ , 1])
  lt <- sort(unique(dt[aes_map$linetype$variable])[ , 1]) 
  zz <- list(color = cl, shape = sh, size = sz, fill = fl, alpha = al, linetype = lt)
  n  <- max(as.numeric(lapply(zz, length)))
  # Add empty strings
  zz2 <- lapply(zz, function(x){
    c(x, rep('', n - length(x)) )
  })
  
  out <- cbind(x = 0, y =  0, as.data.frame(zz2))
  return(out)
}

