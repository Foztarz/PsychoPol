# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2022 02 07
#     MODIFIED:	James Foster              DATE: 2022 03 01
#
#  DESCRIPTION: A set of functions for analysing honeybee waggle dances.
#               
#               
#       INPUTS: 
#               
#      OUTPUTS: 
#
#	   CHANGES: - Perspective correction
#             - Fixed mistaken degree conversion in BootM4A
#             - 
#
#   REFERENCES: Batschelet E (1981).
#               Graphical presentation, Chap 1.2, p. 4-6
#               Chapter 1: Measures of Location
#               In: Circular Statistics in Biology
#               Academic Press (London)
#
#    EXAMPLES:  Source in another script or press ctrl+shift+s to add to workspace
#
# 
#TODO   ---------------------------------------------
#TODO   
#- Bimodal mean vector +
#- Fix M4A convergence
#- Parallel speedup for circ_mle

# General use -------------------------------------------------------------

# . Select files ---------------------------------------------------------
DA_select_file = function(file_type = ".csv",
                          sys_win = Sys.info()[['sysname']] == 'Windows')
{
  #On computers set up by JMU Würzburg, use user profile instead of home directory
  if(sys_win){
    #get rid of all the backslashes
    ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
  }else{#Root directory should be the "HOME" directory on a Mac (or Linux?)
    ltp = Sys.getenv('HOME')#Easier on Mac
  }
  msg = paste('Please select the',#message to display
              '"', file_type,'"',
              'file')
  here_path = tryCatch(expr = #look in the folder containing this file: sys.frame(1)$ofile
                         {file.path(dirname(sys.frame(1)$ofile))},
                       error = function(e)
                       {#if that fails, try to find the "Documents" folder
                         file.path(ltp,'Documents', 
                                   paste0('*',file_type)
                         )
                       }
  )
  # set path to files
  if(sys_win){#choose.files is only available on Windows
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    path_file  = choose.files(
      default = here_path,#look where the function is stored
      caption = msg
    )
  }else{
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    path_file = file.choose(new=F)
  }
  #show the user the path they have selected
  if(is.null(path_file))
  {stop('No file selected.')}else
  {print(path_file)}
  return(path_file)
}


DA_select_folder = function(file_type = ".csv.gz",
                            sys_win = Sys.info()[['sysname']] == 'Windows'
)
{
  #On computers set up by JMU Würzburg, use user profile instead of home directory
  if(sys_win){
    #get rid of all the backslashes
    ltp = gsub('\\\\', '/', Sys.getenv('USERPROFILE'))
  }else
  {#Root directory should be the "HOME" directory on a Mac (or Linux?)
    ltp = Sys.getenv('HOME')#Easier on Mac
  }
  msg = if(sys_win)
  {
    paste0('Please a folder containing ',#message to display
           '"',file_type,'"',
           ' files')
  }else
  {
    paste('Please select any',
          '"',file_type,'"',
          'file in the correct folder')
  }
  here_path = tryCatch(expr = #look in the folder containing this file: sys.frame(1)$ofile
                         {file.path(dirname(sys.frame(1)$ofile))},
                       error = function(e)
                       {#if that fails, try to find the "Documents" folder
                         file.path(ltp,'Documents', 
                                   '*',file_type)
                       }
  )
  # set path to files
  if(sys_win){#choose.dir is only available on Windows
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    path_folder  = choose.dir(
      default = here_path,#look where the function is stored
      caption = msg
    )
  }else{
    message('\n\n',msg,'\n\n')
    Sys.sleep(0.5)#goes too fast for the user to see the message on some computers
    path_folder = dirname(file.choose(new=F))
  }
  #show the user the path they have selected
  if(is.null(path_folder))
  {stop('Nothing selected/ no folder found.')}else
  {print(path_folder)}
  return(path_folder)
}


# . System ------------------------------------------------------------------

#Open file with default program on any OS
# https://stackoverflow.com/a/35044209/3745353
shell.exec.OS  <- function(x){
  # replacement for shell.exec (doesn't exist on MAC)
  if (exists("shell.exec",where = "package:base"))
  {return(base::shell.exec(x))}else
  {comm <- paste0('open "',x,'"')
  return(system(comm))}
}

# . Plotting ---------------------------------------------------------------

#Match default spacing in barplot()
BarPlotSpacing = function(n, # number of items
                          spa = 0.2, #spacing factor 
                          wdt = 1.0) # bar width
{
  return(
    seq( from = spa+1-wdt/2,
         to =  n*(1+spa)-wdt/2,
         length.out =  n )
  )
}


# Camera correction -------------------------------------------------------

#convert from angle to trapezoid-distorted angle
Phi = function(theta,#original angle, radians
               alpha)#angle between trapezoid and vertical, radians
{
  return(
    atan2(y = sin(theta)*(1-cos(alpha)*cos(theta)),
          x = cos(theta)*sin(alpha) )
  )
}
#estimate inverse of trapezoid distortion to get original angles
Theta = function(phi,#distorted angle, radians
                 alpha)#angle between trapezoid and vertical, radians
{
  aa = seq(from = -pi,
           to = pi,
           length.out = 1e4)
  ss = smooth.spline(x = Phi(theta = aa, alpha = alpha),
                     y = aa)
  return(predict(ss, x = phi)$y)
}
#sd(deg(Theta(Phi(crd, 0.1),0.1))- deg(crd) ) #for just 6deg tilt from x axis


Stre = function(theta,#a series of angles
                stc)#the stretch factor
{ #N.B. 0 is vertical, rotation is clockwise, units are radians 
  return(
    atan2(y = sin(theta) ,
          x = stc*cos(theta)     )
  )
}

UnStre = function(phi,#a series of angles
                  stc)#the stretch factor
{ #N.B. 0 is vertical, rotation is clockwise, units are radians 
  return(
    atan2(y = sin(phi) ,
          x = cos(phi)/stc     )
  )
}

# Use a set of known angles to calculate stretch, tilt angle, and rotation

PhiTheta_AlphaDeltaStretch = 
  function(phi,
           theta
  )
  { #I'm not sure what the mathematical relationship is, but it can be estimated
    if(diff(sapply(list(phi,theta),  length)))
    {stop('phi and theta must be the same length')}
    if(length(phi)<3)
    {warning('A minumum of 3 angle pairs is strongly recommended')}
    #Find it by minimising prediction error
    ErrFun = function(alpha_delta_stretch, #tilt angle
                      phi_theta # pairs of distorted and known angles
    )
    { #would sum of squares be quicker?
      mean(
        abs(
        sin(
          
            alpha_delta_stretch[2] +
              Theta(phi = UnStre( phi = phi_theta[,1],
                                  stc = alpha_delta_stretch[3] ),
                    alpha = alpha_delta_stretch[1]) -
              phi_theta[,2] )/2 # this should be equivalent to 1 - length of 2 angle mean vector
        )
      )
    }
    opt = optim(par = c(alpha = pi/2,
                        delta = 0,
                        stc = 1),
                fn = ErrFun,
                method = 'L-BFGS-B',
                lower = c(alpha = 0,
                          delta = -pi,
                          stc = 1e-3),
                upper = c(alpha = pi,
                          delta = pi,
                          stc = 1),
                phi_theta = cbind(phi, theta),
                control = list(maxit = 1e6,
                               pgtol = 1e-16#,
                               # trace = 1,
                               # REPORT = 1
                               )
    )
    opt_sd = sqrt(-2*log(1-opt$value))*180/pi #standard deviation in degrees
    if( opt_sd > 5) # ?sd.circular
    {warning('DISTORTION ESTIMATE HAS AN ERROR >5 degrees!')}else{
      if( opt_sd > 1){warning('Distortion estimate has an error >1 degree')}
    }
    return(
      c(opt$par,
        sd = opt_sd)   
    )
  }

# Use a set of known angles to calculate stretch, tilt angle, and rotation


# Circular data -----------------------------------------------------------


#Convert any angle in degrees to (-180,180)
Mod360.180 = function(x)
{
  deg(
    atan2(y = sin(rad(x)),
          x = cos(rad(x))
    )
  )
}

Cformat = function(angles,
                    angle_unit = "degrees", # "degrees" or "radians"
                    angle_rot = 'clock', # counter' # 'counter' for anticlockwise (imageJ) 'clock' for clockwise
                    angle_zero = pi/2 # 0 # angle start point: _0_ for right along x axis (imageJ) _pi/2_ for up along y axis (e.g. geographic North)
                    )
{
  circular(x = angles,
           units = angle_unit,
           rotation = angle_rot,
           zero = angle_zero
          )
}

# Plotting functions ------------------------------------------------------

DA_BimodPlot  = function(dat)
      {
        with(dat,
             {
               plot.circular(
                 x = Cformat(  angle ),
                 bins = 360/5-1,
                 stack = T,
                 sep = 0.07,
                 col = point_col,
                 pch = 19,
                 shrink = shrk
               )
               mtext(text = paste0(as.character(date),
                                   '\n',
                                   'bee ',
                                   bee,
                                   ', dance ',
                                   dance, 
                                   '\n',
                                   stimulus,
                                   ', ',
                                   stim_ori,
                                   '°'),
                     line = -1.5,
                     cex = 1.5/sq_cond,#3/sq_cond,
                     col = switch (as.character(stim_ori),
                                   `0` = 'red',
                                   `90` = 'cyan3',
                                   'gray'
                     )
               )
               suppressWarnings(
                 {
               mlmod = CircMLE::circ_mle(data = Cformat( angle ) )
                 }
               )
               selmod = mlmod$results[1, ]
               m1 = pi/2-selmod$q1
               m2 = pi/2-selmod$q2
               k1 = selmod$k1
               k2 = selmod$k2
               lb = selmod$lamda
               arrows.circular(x = circular(m1),
                               shrink = A1(k1),
                               col = 2,
                               lwd = lb/0.3,
                               length = 0.05
               )
               arrows.circular(x = circular(m2),
                               shrink = A1(k2),
                               col = 2,
                               lwd = (1-lb)/0.3,
                               length = 0.05
               )
             }
        )
      }

# Analysis functions ------------------------------------------------------
DA_MLpars = function(dat,
                     ...)
{
  with(dat,
       {
        suppressWarnings(
          {
            mlmod = CircMLE::circ_mle(data = Cformat( angle ),
                                      ...)
          }
        )
        selmod = mlmod$results[1, ]
        # m1 = pi/2-selmod$q1
        # m2 = pi/2-selmod$q2
        m1 = selmod$q1
        m2 = selmod$q2
        k1 = selmod$k1
        k2 = selmod$k2
        lb = selmod$lamda
        return( 
          data.frame(m1 = circular::deg(m1),
                     m2 = circular::deg(m2),
                     k1 = k1,
                     k2 = k2,
                     lb1 = lb,
                     lb2 = 1-lb
                           )
        )
      }
    )
}
Rayleigh_double = function(data,
                           out_format = 'list')
{
  if(is.circular(data))
  {cp = circularp(data)}else
  {
    cp = list(units = "degrees",
              rotation = "clock",
              zero = pi/2,
              )
    data = as.circular(x = data, 
                         control.circular = cp)
  }
  
  with(
    circular::mle.vonmises(x = data*2, 
                           control.circular = cp),
       {
         switch(EXPR = out_format,
                list = list(par = c(mu = as.numeric(mu)/2, 
                                    kappa = kappa) ),
                vector = c(mu = as.numeric(mu)/2, 
                           kappa = kappa),
                list(par = c(mu = as.numeric(mu)/2, 
                             kappa = kappa) )
               )
       }
  )
}

M4A_uvec = function(data,
                  BadStart = 1e9,
                  nchains = 36,# better to have a lot of starting positions
                  # method = "BFGS", #N.B. requires "L-BFGS-B"!
                  niter = 1e3,
                  lambda.min = 0.25,
                  start_type = 'random' # or spaced
                  )
  {

if(BadStart < 0)
  {stop("The value for starting parameters outside the preset limits must be >0")}
if (nchains < 1)
  {stop("Must set the number of chains to an integer >=1")}
if(niter < 1000)
  {
  warning("At least 1000 iterations are recommended but not required.
             Check ?optim for details.")
  }
# if(method != "Nelder-Mead" &
#     BadStart == Inf)
#   {
#   stop("Except for Nelder-Mead, all other optimization algorithms require finite starting parameters")
#   }

lambda.max = 1 - lambda.min
lambda = switch(EXPR = start_type,
                random = stats::runif(n = nchains, 
                              min = lambda.min, 
                              max = lambda.max),
                spaced = seq(from = lambda.min,
                             to = lambda.max,
                             length.out = nchains),
                stats::runif(n = nchains, 
                             min = lambda.min, 
                             max = lambda.max)
                )
m4a_uvec = function(params,
                    data,
                    BadStart = 1e9,
                    lambda.min = 0.25
                    )
  {
  if (
      #params[1] <= -1 | params[1] >= 1 | #cosine
      # params[2] <= -1 | params[2] >= 1 | #sine
      sqrt(params[2]^2 + params[1]^2) != 1 #| #must be a unit vector!
      # params[3] <= 0 | params[3] > 227 | #kappa
      # params[4] < lambda.min | params[4] > lambda.max #lambda
      ) 
    {
    R = BadStart
    return(R)
  }
  else {
    P = circularp(data)
    m1 = atan2(y = params[2], x = params[1])
    R = dmixedvonmises(x = data,
                       mu1 = as.circular(x = m1, 
                                         control.circular = P),
                       mu2 = as.circular(x = m1+pi, 
                                         control.circular = P), 
                       kappa1 = exp(params[3]), #safer on a log scale, no chance of negatives
                       kappa2 = exp(params[3]), 
                       prop = params[4])
    R = -sum(log(R))
    if(is.infinite(R)){return(BadStart)}
    if(R < -.Machine$double.xmax){return(BadStart)}
    return(R)
  }
}
    
    # Randomize starting parameters for the optimization
    c_extent = ifelse(test = circularp(data)$units == 'radians',
                      yes = pi,
                      no = 180)
    q1 = as.numeric(x =
                      switch(EXPR = start_type,
                            random = rcircularuniform(
                                             n = nchains, 
                                             control.circular = circularp(data)#list(modulo = "2pi")
                                             ),
                            spaced = seq(from = -c_extent,
                                         to = -c_extent+2*c_extent*(nchains-1)/nchains,
                                         length.out = nchains)+
                                      runif(n = 1)*c_extent,
                            seq(from = -c_extent,
                                 to = -c_extent+2*c_extent*(nchains-1)/nchains,
                                 length.out = nchains)
                      )
            )
    k1 = as.numeric(x = 
                      switch(EXPR = start_type,
                             random = log(
                                      sample(x = 1:5, 
                                             size = nchains, 
                                             replace = T)
                                      )
                             ,
                             spaced = # #exp(
                                         # seq(from = -2,# rho = 0.07
                                         #     to = 2, # rho = 0.93
                                         #  length.out = nchains)
                                        # #)
                                       rep(x = 1, times = nchains)
                            )
            )
    q1_cos = cos(
                if(circularp(data)$units == 'radians')
                           {q1}else{rad(q1)}
              )
    q1_sin = sin(
                if(circularp(data)$units == 'radians')
                 {q1}else{rad(q1)}
                )
    # Run optimization
    m4a.out = list()
    for (i in 1:nchains)
      {
      chain.out = suppressWarnings(
        {
        stats::optim(par = c(q1_cos[i],
                             q1_sin[i],
                             k1[i], 
                             lambda[i]), 
                     fn = m4a_uvec, 
                     data = data,
                     method = "L-BFGS-B", 
                     lower = c(-1,
                               -1,
                               0,
                               lambda.min),
                     upper = c(1,
                               1,
                               log(227),
                               lambda.max),
                     control = list(maxit = niter), 
                     hessian = T,
                     BadStart = 1e9,
                     lambda.min = 0.25
                     )
        }
        )
      names(chain.out)[2] = "lik"
      m4a.out[[i]] = chain.out
    }
    min_ll = which.min(sapply(m4a.out,function(x){x[2]}))
    m4a.out = lapply(X = m4a.out,#recover exponentiated log kappa 
                     FUN = function(x)
                     {
                      xx = x
                      xx[[1]][3] = exp(xx[[1]][3])
                      return(xx)
                      }
                     )
    return(m4a.out[[min_ll]])
}

BootSample = function(d, p)
{
  sample(x = d, 
         size = length(d), 
         replace = T)
}

BootM4A = function(angles,
                   speedup_parallel = TRUE, #Use the parallel package to speed up calculations,
                   nrep = 1e3,
                   clust = if(speedup_parallel) #Use a pre-assigned parallel cluster, or make a new one
                   {makeCluster(parallel::detectCores() - 1,type="SOCK")}else
                   {NULL},
                   style = 'M4A',
                   niter = 1e3,
                   type_start = if(style == 'M4A_uvec'){'spaced'}else{'random'},
                   n_chains = switch(style,
                                    M4A_uvec = 36,
                                    M4A = 36,
                                    Rayleigh_double = 0,
                                    5),
                   ...)
{
  if(speedup_parallel)
  {
    clusterExport(cl = clust,#the cluster needs some variables&functions outside parLapply
                  list('DA_MLpars',
                       'Cformat',
                       'circ_mle',
                       'M4A',
                       "M4A_uvec",
                       "Rayleigh_double",
                       'circular',
                       "as.circular",
                       "circularp",
                       "dvonmises",
                       "dmixedvonmises",
                       "rcircularuniform",
                       'deg',
                       'rad'),
                  environment()#needs to be reminded to use function environment, NOT global environment
    )
  }
  ReturnPar = function(x)
  {
    mod = switch(EXPR = style, 
                 M4A_uvec = M4A_uvec(data = x, 
                                      BadStart = 1e9, 
                                      nchains = n_chains, 
                                      # method = 'BFGS', 
                                      niter = niter, 
                                      lambda.min = 0.25,
                                      start_type = type_start),
                 M4A = M4A(data = x, 
                            BadStart = 1e9, 
                            nchains = n_chains, 
                            method = 'BFGS', 
                            niter = niter, 
                            lambda.min = 0.25),
                 # Ray_doub = Ray_doub(data = x, 
                 #            BadStart = 1e9, 
                 #            method = 'BFGS', 
                 #            niter = niter),
                 circ_mle(data = x, 
                      BadStart = 1e9, 
                      nchains = n_chains, 
                      method = 'BFGS', 
                      niter = niter, 
                      lambda.min = 0.25)$result[1,-1]
                  )
     if(style %in% 'M4A_uvec')
     {
      prm = with(mod, 
                 c(mu1 = atan2(y = par[2], x = par[1]),
                   kappa1 = par[3],
                   lambda = par[4])
                )
     }else
     {
       if(any(names(mod) %in% 'par'))
       {prm = mod$par}else
       {prm = mod}
     }
    return(prm)
  }
  mean_bs = boot(#data = Cformat(angles),
                 data = angles,
                 statistic = ReturnPar, 
                 R = nrep, 
                 sim = 'parametric',
                 ran.gen = BootSample,
                 parallel = if(speedup_parallel)
                    {
                    if(Sys.info()[['sysname']] == 'Windows')
                              {'snow'}else
                              {'multicore'}
                    }else
                      {NULL},
                 cl = if(speedup_parallel){clust}else{NULL},
                 ncpus = parallel::detectCores() - 1
                 )
}

# Replicate calls ---------------------------------------------------------

ReplCollapse = function(call,
                        n = 1e3,
                        extractPar = FALSE,
                        clust = NULL,
                        ...)
{
  # rr = replicate(n = integer(n), 
  #                expr = call,
  #                simplify = !extractPar
  #               )
  #replicate is just a wrapper for sapply (and not a great one)
  if(!is.null(clust))
  {
    rr = parSapply(cl = clust,
                   X = integer(n), 
                   FUN = function(i)
                   {
                     do.call(what = call,
                             args = list(...)
                     )
                   },
                   simplify = !extractPar
    )  
  }else
  {
    rr = sapply(X = integer(n), 
                FUN = function(i)
                {
                  do.call(what = call,
                          args = list(...)
                  )
                },
                simplify = !extractPar
    )
  }
  if(is.list(rr))
  {
    if(extractPar)
    {
      rr = lapply(X = rr,
                  FUN = function(lst)
                  {
                    c(lst$par)
                  }
      )
    }
    return(
      data.frame(
        do.call(what = rbind,
                args = rr
        )
      )
    )
  }else
  {
    return(data.frame(rr))
  }
}

# Archived ## -------------------------------------------------------------


# # Use a set of known angles to calculate tilt angle 
# PhiTheta_Alpha = function(phi,
#                           theta
# )
# { #I'm not sure what the mathematical relationship is, but it can be estimated
#   if(diff(sapply(list(phi,theta),  length)))
#   {stop('phi and theta must be the same length')}
#   if(length(phi)<3)
#   {warning('A minumum of 3 angle pairs is strongly recommended')}
#   #Find it by minimising prediction error
#   ErrFun = function(alpha, #tilt angle
#                     phi_theta # pairs of distorted and known angles
#   )
#   { #would sum of squares be quicker?
#     sd(x = Theta(phi = phi_theta[,1],alpha = alpha) - phi_theta[,2] )
#   }
#   opt = optimize(f = ErrFun,
#                  interval = c(0,pi/2),
#                  phi_theta = cbind(phi, theta)
#   )
#   if(opt$objective > 1){warning('Distortion estimate has an error >1 degree')}
#   if(opt$objective > 1){warning('DISTORTION ESTIMATE HAS AN ERROR >5 degrees!')}
#   return(opt$minimum)
# }
# 
# PhiTheta_AlphaDelta = function(phi,
#                                theta
# )
# { #I'm not sure what the mathematical relationship is, but it can be estimated
#   if(diff(sapply(list(phi,theta),  length)))
#   {stop('phi and theta must be the same length')}
#   if(length(phi)<3)
#   {warning('A minumum of 3 angle pairs is strongly recommended')}
#   #Find it by minimising prediction error
#   ErrFun = function(alpha_delta, #tilt angle
#                     phi_theta # pairs of distorted and known angles
#   )
#   { #would sum of squares be quicker?
#     mean(
#       abs(
#         sin(
#           alpha_delta[2] +
#             Theta(phi = phi_theta[,1],
#                   alpha = alpha_delta[1]) -
#             phi_theta[,2] )/2 # this should be equivalent to 1 - length of 2 angle mean vector
#       )
#     )
#   }
#   opt = optim(par = c(alpha = pi/2,
#                       delta = 0),
#               fn = ErrFun,
#               method = 'L-BFGS-B',
#               lower = c(alpha = 0,
#                         delta = -pi),
#               upper = c(alpha = pi,
#                         delta = pi),
#               phi_theta = cbind(phi, theta),
#               control = list(maxit = 1e6,
#                              pgtol = 1e-16)
#   )
#   opt_sd = sqrt(-2*log(1-opt$value))*180/pi #standard deviation in degrees
#   if( opt_sd > 5) # ?sd.circular
#   {warning('DISTORTION ESTIMATE HAS AN ERROR >5 degrees!')}else{
#     if( opt_sd > 1){warning('Distortion estimate has an error >1 degree')}
#   }
#   return(
#     c(opt$par,
#       sd = opt_sd)   
#   )
# }

# 
# DeltaStretch = 
#   function(phi,
#            theta
#   )
#   { #I'm not sure what the mathematical relationship is, but it can be estimated
#     if(diff(sapply(list(phi,theta),  length)))
#     {stop('phi and theta must be the same length')}
#     if(length(phi)<3)
#     {warning('A minumum of 3 angle pairs is strongly recommended')}
#     #Find it by minimising prediction error
#     ErrFun = function(delta_stretch, #tilt angle
#                       phi_theta # pairs of distorted and known angles
#     )
#     { #would sum of squares be quicker?
#       mean(
#         abs(
#           sin(
#             
#             delta_stretch[2] +
#               UnStre( phi = phi_theta[,1],
#                       stc = delta_stretch[2] ) -
#               phi_theta[,2] )/2 # this should be equivalent to 1 - length of 2 angle mean vector
#         )
#       )
#     }
#     opt = optim(par = c(delta = 0,
#                         stc = 1),
#                 fn = ErrFun,
#                 method = 'L-BFGS-B',
#                 lower = c(delta = -pi,
#                           stc = 1e-3),
#                 upper = c(delta = pi,
#                           stc = 1),
#                 phi_theta = cbind(phi, theta),
#                 control = list(maxit = 1e6,
#                                pgtol = 1e-16#,
#                                # trace = 1,
#                                # REPORT = 1
#                 )
#     )
#     opt_sd = sqrt(-2*log(1-opt$value))*180/pi #standard deviation in degrees
#     if( opt_sd > 5) # ?sd.circular
#     {warning('DISTORTION ESTIMATE HAS AN ERROR >5 degrees!')}else{
#       if( opt_sd > 1){warning('Distortion estimate has an error >1 degree')}
#     }
#     return(
#       c(opt$par,
#         sd = opt_sd)   
#     )
#   }

# Ray_doub = function(data,
#                     method = "BFGS",
#                     BadStart = 1e9, 
#                     nchains = 4,
#                     niter = 1e3)
# {
#   ray_doub = function(params)
#   {
#     if(params[1] <= -pi | params[1] >= pi | #cosine
#        params[2] <= 0 | params[2] > 227 ) #kappa)
#     {
#       return(BadStart)
#     }else
#     {
#       P = circularp(data)
#       doub_data = data*2
#       R = dvonmises(x = doub_data,
#                     mu = as.circular(x = params[1]*2, 
#                                      control.circular = P),
#                     kappa = params[2])
#       R = -sum(log(R))
#     }
#   }
#   # Randomize starting parameters for the optimization
#   q1 = as.numeric(x =
#                     rcircularuniform(n = nchains, 
#                                      control.circular = list(modulo = "2pi")
#                     )
#   )
#   k1 = as.numeric(x = 
#                     sample(x = 1:5, 
#                            size = nchains, 
#                            replace = T)
#   )
#   ray.out = list()
#   for (i in 1:nchains)
#   {
#     chain.out = suppressWarnings(
#       {
#         stats::optim(par = c(q1[i],
#                              k1[i]), 
#                      fn = ray_doub, 
#                      method = method, 
#                      control = list(maxit = niter), 
#                      hessian = T)
#       }
#     )
#     names(chain.out)[2] = "lik"
#     ray.out[[i]] = chain.out
#   }
#   min = which.min(sapply(ray.out,function(x){x[2]}))
#   return(ray.out[[min]])
# }

