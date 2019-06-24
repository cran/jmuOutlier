latin <- 
function(n, random=TRUE) {
  # Computes a Latin Square.
  # 'n' is row length and column length and should be an integer greater than one.
  # 'random' implies whether or not the Latin Square should be chosen at random, by sampling the subscripts.
  if ( !is.numeric(n) )  stop( "'n' must be an integer between 2 and 26.")
  if ( n != round(n) )   stop( "'n' must be an integer between 2 and 26.")
  if ( n < 2 | n > 26 )  stop( "'n' must be an integer between 2 and 26.")
  if ( !is.logical(random) ) stop( "'random' must be logical." )
  x = matrix(LETTERS[1:n], n, n)
  for (j in 2:n) x[ , j ] = x[ c(j:n, 1:(j - 1)), j ]
  if (random) x = x[ sample(n), sample(n) ]
  return(x)
}
