
# Make a spline template for a visual pigment
StavengaSpline <- function(spec.range = c(300, 700), #bounds of spectrum in nanometers
                           lambda.max,#peak sensitivity
                           a.type = 'a1'){#pigment type, only a1 available currently
  wlns <- seq(min(spec.range),max(spec.range), length.out = 1e3) #
  #Stavenga, D. G. (2010). On visual pigment templates and the spectral shape of invertebrate rhodopsins and metarhodopsins. Journal of Comparative Physiology A: Neuroethology, Sensory, Neural, and Behavioral Physiology, 196(11), 869–878. doi:10.1007/s00359-010-0568-7
  # modified lognormal
  m.lognorm <- function(wl,l.max, a0, a1)
  {
    x = log10(wl/l.max)
    return(exp(-a0*x^2 * (1+a1*x+3*a1^2*x^2)))
  }
  if(a.type == 'a1')
  {
    #alpha band
    a.band <- m.lognorm(wlns,lambda.max, 380, 6.09)
    #beta band
    b.band <- 0.29*m.lognorm(wlns, 340, 247, 3.59)
    #gamma band
    g.band <- 1.99*m.lognorm(wlns, 276, 647, 23.4)
  }else
  {stop('a2 and a3 pigments not yet implemented')}
  # N.B. Stavenga normalises to max(a.band), I normalise to function max
  r.stav <- (a.band + b.band + g.band)/max(a.band + b.band + g.band)
  return(    smooth.spline(wlns, r.stav)    )
}#StavengaSpline <- function(spec.range, lambda.max)
#pointless comment