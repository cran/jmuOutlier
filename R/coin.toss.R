coin.toss <-
function(n, p=0.5, burn.in=0, log.scale=FALSE, col=c("black","red"), ...){
  # Construct a graph illustrating the sample proportion of heads.
  # `n' is the number of times the coin is tossed.
  # `p' is the probability of heads.
  # `burn.in' denotes how many values of p.hat should be omitted from the graph.
  # `log.scale' indicates whether or not the x-axis should have a logarithmic scale.
  # `...' consists of additional arguments for "plot".
  if ( !is.numeric(n) | n!=round(n) | n<1 )   stop( "'n' must be a positive integer.")
  if ( !is.numeric(p) | p<0 | p>1 )  stop( "'p' must be a number between 0 and 1.")
  if ( !is.numeric(burn.in) | burn.in!=round(burn.in) | burn.in<0 | burn.in>=n )  
      stop( "'burn.in' must be a nonnegative integer smaller than 'n'.")
  if ( !is.logical(log.scale) )  stop( "'log.scale' must be logical.")
  x=1:n;  y=cumsum(rbinom(length(x),1,p))/x;  p.hat=function(x){y[x]}
  plot( p.hat, burn.in+1, n, xlab="NUMBER  OF  COIN  TOSSES", ylab="SAMPLE  PROPORTION  OF  HEADS", 
        log=ifelse(log.scale,"x",""), main=paste( "Probability  of  heads  is ", p ), 
        ylim=c(min(y[(burn.in+1):n],p),max(y[(burn.in+1):n],p)), type="s", col=col[1], ... )
  abline( h=p, col=col[2] )
}
