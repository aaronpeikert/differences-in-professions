#----functions----
is_interval_scaled <- function(x, nunique = 5){
  x <- na.omit(x)
  x <- suppressWarnings(as.numeric(x))
  if(anyNA(x))return(FALSE)
  if(length(unique(x)) < nunique)return(FALSE)
  else return(TRUE)
}
