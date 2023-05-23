# Details ---------------------------------------------------------------
#       AUTHOR:	James Foster              DATE: 2023 05 22
#     MODIFIED:	James Foster              DATE: 2023 05 22
#
#  DESCRIPTION: Simulate waggle dance scatter. Compare "directional accuracy"
#               heuristic used by Dong and colleagues with traditional measures
#               of directional scatter.
#               
#       INPUTS: 
#               
#      OUTPUTS: Simulation result
#
#	   CHANGES: - 
#
#   REFERENCES: Shihao Dong et al., (2023)
#               Social signal learning of the waggle dance in honey bees.
#               Science 379,1015-1018. DOI:10.1126/science.ade1702
#               
#               Batschelet E (1981).
#               Graphical presentation, Chap 1.2, p. 4-6
#               Chapter 1: Measures of Location
#               In: Circular Statistics in Biology
#               Academic Press (London)
# 
#       USAGE:  
#TODO   ---------------------------------------------


# Seed random number generator --------------------------------------------
#simulation should be repeatable
set.seed(20230522)#today's date


# Load packages -----------------------------------------------------------
require(circular)
require(data.table)
# require(beeswarm)

# Simulate data -----------------------------------------------------------
n_sim = 1e2
n_lev = 20
n_runs = 15# average for naïve dancers
log_kappa = seq(from = -0.3, to  = log(20), length.out = n_lev) # von Mises distribution concentration parameter
kappas = exp(log_kappa)
# kappas = seq(from = 0, to = 20, length.out = n_lev)
dance_data = replicate(n = n_sim,
                       expr = {lapply(X = kappas,
                                     FUN = rvonmises,
                                     n = n_runs,
                                     mu = circular(0)
                                     )
                               },
                       simplify = FALSE
                      )

# Plot data range ---------------------------------------------------------
#worst oriented
plot.circular(dance_data[[1]][[1]], template = 'geographics', main = paste0('kappa =', round(kappas[1],2)))
arrows.circular(x = circular(pi/2),length = 0)
#best oriented
plot.circular(dance_data[[1]][[n_lev]], template = 'geographics', main = paste0('kappa =', round(kappas[n_lev],2)))
arrows.circular(x = circular(pi/2),length = 0)



# Calculate metrics -------------------------------------------------------
RhoFun = rho.circular
SDFun = function(x){sd.circular(x)*180/pi}
MaxErr = function(x_ang){max(abs(as.numeric(
  ifelse(x_ang > pi,
         yes = 2*pi - x_ang,
         no = x_ang) 
  )))*180/pi}

rho_matrix = cbind(kappas,
                   sapply(X = dance_data,
                  FUN = function(x)
                    {sapply(X = x,
                            FUN = RhoFun)})
                  )

sd_matrix = cbind(kappas,
                   sapply(X = dance_data,
                  FUN = function(x)
                    {sapply(X = x,
                            FUN = SDFun)})
                  )

mx_matrix = cbind(kappas,
                   sapply(X = dance_data,
                  FUN = function(x)
                    {sapply(X = x,
                            FUN = MaxErr)})
                  )

rho_data = data.table::melt(data = data.table(rho_matrix), 
                            id.vars = 'kappas')

sd_data = data.table::melt(data = data.table(sd_matrix), 
                            id.vars = 'kappas')

mx_data = data.table::melt(data = data.table(mx_matrix), 
                            id.vars = 'kappas')



# Plot rho ----------------------------------------------------------------
with(rho_data,
     plot(x = kappas,
          y = value,
           pch = 20, 
           col = adjustcolor('darkblue', alpha.f = 0.05),
           ylab = 'mean vector length',
           xlab = 'von Mises concentration (kappa)',
           ylim = c(0,1),
           axes = FALSE
           )
)
axis(1, at = kappas,labels= round(kappas, 1))
axis(2, at = seq(from = 0, to  = 1, by = 0.25))
abline(h = c(0,1))
boxplot(formula = value~kappas,
        data = rho_data,
        at = kappas,
        add = TRUE,
        col = gray(0.7, 0.2),
        border = gray(0.1, 0.7),
        pars = list(boxwex = 0.3, staplewex = 0.1, outwex = 0.1),
        axes = F
)

# Plot sd ----------------------------------------------------------------
with(sd_data,
     plot(x = kappas,
          y = value,
           pch = 20, 
           col = adjustcolor('darkblue', alpha.f = 0.05),
           ylab = 'circular SD (°)',
           xlab = 'von Mises concentration (kappa)',
           ylim = c(0,180),
           axes = FALSE
           )
)
axis(1, at = kappas,labels= round(kappas, 1))
axis(2, at = seq(from = 0, to  = 180, by = 30))
abline(h = c(0,180))
boxplot(formula = value~kappas,
        data = sd_data,
        at = kappas,
        add = TRUE,
        col = gray(0.7, 0.2),
        border = gray(0.1, 0.7),
        pars = list(boxwex = 0.3, staplewex = 0.1, outwex = 0.1),
        axes = F
)
# Plot max ----------------------------------------------------------------
with(mx_data,
     plot(x = kappas,
          y = value,
           pch = 20, 
           col = adjustcolor('darkblue', alpha.f = 0.05),
           ylab = 'maxiumum error (°)',
           xlab = 'von Mises concentration (kappa)',
           ylim = c(0,180),
           axes = FALSE
           )
)
axis(1, at = kappas,labels= round(kappas, 1))
axis(2, at = seq(from = 0, to  = 180, by = 30))
abline(h = c(0,180))
boxplot(formula = value~kappas,
        data = mx_data,
        at = kappas,
        add = TRUE,
        col = gray(0.7, 0.2),
        border = gray(0.1, 0.7),
        pars = list(boxwex = 0.3, staplewex = 0.1, outwex = 0.1),
        axes = F
)


# Plot sd vs max error ----------------------------------------------------


plot(x = sd_data$value,
    y = mx_data$value,
    pch = 20, 
    col = adjustcolor('darkblue', alpha.f = 0.05),
    xlab = 'circular sd (°)',
    ylab = 'maximum error (°)',
    xlim = c(0,180),
    ylim = c(0,180),
    axes = FALSE
)
axis(1, at = seq(from = 0, to  = 180, by = 30))
axis(2, at = seq(from = 0, to  = 180, by = 30))
abline(h = c(0,180),
       v = c(0,180),
       a = 0, b = 1)


# Check medians -----------------------------------------------------------

sd_mdn = aggregate(x = value~kappas, data = sd_data, FUN = median)
mx_mdn = aggregate(x = value~kappas, data = mx_data, FUN = median)
round(range(sd_mdn$value),2)
# [1] 20.23 81.41
round(range(mx_mdn$value),2)
# [1]   43.03 161.08

#fit spline for predicting kappa from max error
mx_spl = with(mx_mdn, 
              smooth.spline(x = value,
                            y = kappas)
              )
print(
data.frame(predict(object = mx_spl,
        x = c(10, 25, 30, 40, 65)))
)