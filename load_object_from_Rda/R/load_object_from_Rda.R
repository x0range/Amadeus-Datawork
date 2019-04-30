
load_object_from_Rda <- function(f)
{
    env <- new.env()
    varname <- load(f, env)[1]
    stopifnot(length(varname)==1, class(env[[varname]])=="data.frame")
    return(env[[varname]])
}
