#-----------------------------------------------------------------------------#
# Get arguments from a function
# 
# Extracts the names of the arguments from a function, and creates a list 
# of those arguments where they exist in ... . 
# 
# @param FUN function for which to find arguments
# @param args_list a list of arguments. Defaults to NULL.
# @param ... any arguments. Those necessary for FUN must be named as appropriate for FUN
# @return list of arguments for FUN
# @export
# @examples
# myargs <- get_args(lm, formula = Sepal.Length ~ Sepal.Width, data = iris )
# summary(do.call('lm', myargs))
#-----------------------------------------------------------------------------#

get_args <- function(FUN, args_list = NULL, ...){
  dots <- append(args_list, list(...))
  arg_names <- names(formals(match.fun(FUN)))
  
  args <- dots[arg_names]
  args[sapply(args, is.null)] <- NULL
  
  return(args)
}

#-----------------------------------------------------------------------------#
#' Create an aesthetic mapping for a Patient Profile
#' 
#'
#' @param 
#-----------------------------------------------------------------------------#

aes_map <- function(id     = 'id',
                    y      = 'y',
                    x      = 'x',
                    xend   = 'xend',
                    color  = 'color',
                    size   = color,
                    shape  = color,
                    fill   = color,
                    vline1 = NULL,
                    vline2 = NULL,
                    vline3 = NULL,
                    vline4 = NULL,
                    vline5 = NULL)
{
  out <- list(id     = id,
              y      = y,
              x      = x,
              xend   = xend,
              color  = color,
              size   = size,
              shape  = shape,
              fill   = fill,
              vline1 = vline1,
              vline2 = vline2,
              vline3 = vline3,
              vline4 = vline4,
              vline5 = vline5)
  return(out)
}


#-----------------------------------------------------------------------------#
#' Create an aesthetic mapping for a Vertical Lines
#' 
#'
#' @param 
#-----------------------------------------------------------------------------#

vline_aes <- function(vline1.color    = 'black',
                      vline1.size     = 1,
                      vline1.linetype = 'solid',
                      vline2.color    = 'darkgrey',
                      vline2.size     = 1,
                      vline2.linetype = 'dashed', 
                      vline3.color    = 'lightgrey',
                      vline3.size     = 1,
                      vline3.linetype = 'dotted', 
                      vline4.color    = 'blue',
                      vline4.size     = 1,
                      vline4.linetype = 'dotdash', 
                      vline5.color    = 'red',
                      vline5.size     = 1,
                      vline5.linetype = 'twodash')
{
  out <- list(vline1 = list(color    = vline1.color,
                            size     = vline1.size,
                            linetype = vline1.linetype),
              vline2 = list(color    = vline2.color,
                            size     = vline2.size,
                            linetype = vline2.linetype), 
              vline3 = list(color    = vline3.color,
                            size     = vline3.size,
                            linetype = vline3.linetype), 
              vline4 = list(color    = vline4.color,
                            size     = vline4.size,
                            linetype = vline4.linetype), 
              vline5 = list(color    = vline5.color,
                            size     = vline5.size,
                            linetype = vline5.linetype)
              )
  return(out)
}