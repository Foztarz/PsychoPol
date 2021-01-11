#clean dataspace
rm(list = ls())
graphics.off()
#################################################################
#	Load & Install												#
#################################################################
chooseCRANmirror(graphics=FALSE, ind = which(getCRANmirrors()$Country == 'Sweden'))#'Germany'))
install.packages(c('rstan', 'circular', 'brms'))
library(rstan) # observe startup messages
library(circular)
library(brms)
options(mc.cores = parallel::detectCores()-1)

#################################################################
#	Prepare the Summary Dataset									#
#################################################################
#iterations
ni <- 5*10^1
assessment <- data.frame(
					med.rhat = rep(NA, ni),#median rhat error
					max.rhat = rep(NA, ni),#max rhat error
					min.mu.e = rep(NA, ni),#min µ error
					min.ray.mu.e = rep(NA, ni),#min rayleigh µ error
					med.mu.e = rep(NA, ni),#median µ error
					max.mu.e = rep(NA, ni),#max µ error
					med.ray.mu.e = rep(NA, ni),#median rayleigh µ error
					max.ray.mu.e = rep(NA, ni),#max rayleigh µ error
					med.rho.e = rep(NA, ni),#median rho error
					max.rho.e = rep(NA, ni),#max rho error
					med.ray.rho.e = rep(NA, ni),#median rayleigh rho err
					max.ray.rho.e = rep(NA, ni)#max rayleigh rho err
				)
# open the data loop and fitting Loop	

j <- 1	
for(i in 1:ni){

#################################################################
#	Invent Some Data											#
#################################################################
nb <- 10#number of beetles
nt <- 10#number of trials
true.kappa <- exp(rnorm(nt,1.5,0.1))#raw kappa
true.offset <- rvonmises(nt,0,0)*180/pi#degrees
true.mu <- mean.circular(true.offset)


MEmises_dat <- data.frame(angles = c(t(
				apply(cbind(true.offset, true.kappa),1, function(x){rvonmises(nt, x['true.offset']*pi/180, x['true.kappa'])})
				)),
				animal = as.factor(	rep(1:nb,nt)		)
				)
				
#convert to (-pi, pi)
MEmises_dat1 <- MEmises_dat
#make it between -pi & pi
MEmises_dat1$angles[MEmises_dat1$angles > pi] <-
				-(2*pi - MEmises_dat1$angles[MEmises_dat1$angles > pi])
			
#################################################################
#	Make Stan Data												#
#################################################################
form1 <- bf( angles ~ 1 + (1|animal), kappa ~ 1 + (1|animal))			
  
br.data <- make_standata(
							formula = form1,
							data = MEmises_dat1,
							family = von_mises
							)
assign(paste0('dataset.',i), br.data)

try({
fit <- stan(file =
				paste0(Sys.getenv('HOME'), '/Dropbox/R scripts/',
					'handwritten.uvMEmises6-1.stan'),#I wrote this myself
			data = get(paste0('dataset.',i)),#data object brms made
			# iter = 400, warmup = 200,
			iter = 4000, warmup = 2000,
			init = "random", cores = getOption("mc.cores", 1L),
			chains = 4+1,
			control = list(adapt_delta = 0.95))
#################################################################
#	Post-processing												#
#################################################################
fitted <- extract(fit)	#extract draws
med.mu <- median(circular(fitted$b_Intercept))#robust estimate of mean angle
q.mu <- quantile(circular(fitted$b_Intercept), c(0.025, 0.975) )#CI of mean angle
med.kappa <- median(fitted$b_kappa_Intercept)#robust estimate of kappa
q.k <- quantile(fitted$b_kappa_Intercept, c(0.025, 0.975) )#CI off kappa
#plot raw data
#visualise individual level effects
ifx.mu <- data.frame(extract(fit,
								pars=c("r_1_1[1]",
										"r_1_1[2]",
										"r_1_1[3]",
										"r_1_1[4]",
										"r_1_1[5]",
										"r_1_1[6]",
										"r_1_1[7]",
										"r_1_1[8]",
										"r_1_1[9]",
										"r_1_1[10]"
										)))
ifx.kappa <- data.frame(extract(fit, 
									pars=c("r_2_kappa_1[1]",
											"r_2_kappa_1[2]",
											"r_2_kappa_1[3]",
											"r_2_kappa_1[4]",
											"r_2_kappa_1[5]",
											"r_2_kappa_1[6]",
											"r_2_kappa_1[7]",
											"r_2_kappa_1[8]",
											"r_2_kappa_1[9]",
											"r_2_kappa_1[10]"
										  )))

robust.ifx.mu <- apply(ifx.mu,2, median.circular)
robust.ifx.kappa <- apply(ifx.kappa,2, median)
id1.mu <- med.mu + robust.ifx.mu['r_1_1.1.'] 
id2.mu <- med.mu + robust.ifx.mu['r_1_1.2.'] 
id3.mu <- med.mu + robust.ifx.mu['r_1_1.3.'] 
id4.mu <- med.mu + robust.ifx.mu['r_1_1.4.'] 
id5.mu <- med.mu + robust.ifx.mu['r_1_1.5.'] 
id6.mu <- med.mu + robust.ifx.mu['r_1_1.6.'] 
id7.mu <- med.mu + robust.ifx.mu['r_1_1.7.'] 
id8.mu <- med.mu + robust.ifx.mu['r_1_1.8.'] 
id9.mu <- med.mu + robust.ifx.mu['r_1_1.9.'] 
id10.mu <- med.mu + robust.ifx.mu['r_1_1.10.'] 
id1.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.1.'] 
id2.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.2.'] 
id3.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.3.'] 
id4.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.4.'] 
id5.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.5.'] 
id6.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.6.'] 
id7.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.7.'] 
id8.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.8.'] 
id9.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.9.'] 
id10.kappa <- med.kappa + robust.ifx.kappa['r_2_kappa_1.10.'] 

#################################################################
#	Accuracy Assessment											#
#################################################################
mu.error <- pi*true.offset/180  - c(id1.mu, id2.mu, id3.mu, id4.mu, id5.mu, id6.mu, id7.mu, id8.mu, id9.mu, id10.mu)
mu.error[mu.error > pi] <-
				-(2*pi - mu.error[mu.error > pi])
mu.error[mu.error < -pi] <-
				(2*pi + mu.error[mu.error < -pi])
rho.error <- abs( A1(true.kappa)  - A1(exp(c(id1.kappa, id2.kappa, id3.kappa, id4.kappa, id5.kappa, id6.kappa, id7.kappa, id8.kappa, id9.kappa, id10.kappa))) )

mn.error <- rep(NA, length(true.offset))
mv.error <- rep(NA, length(true.kappa))
#Rayleigh estimate of mean for each individual
for(id in 1:nb){
	#mean angle error (difference from generating mu)
	mn.error[id] <- pi*true.offset[id]/180  - mean.circular(subset(get(paste0('dataset.',i))$Y, get(paste0('dataset.',i))$J_1 == id))
	#mean vector length error (difference from generating kappa)
	mv.error[id] <- abs( A1(true.kappa)  - rho.circular(subset(get(paste0('dataset.',i))$Y, get(paste0('dataset.',i))$J_1 == id)) )
}#for(id in 1:nb)
mn.error[mn.error > pi] <-
				-(2*pi - mn.error[mn.error > pi])
mn.error[mn.error < -pi] <-
				(2*pi + mn.error[mn.error < -pi])


mu.error.deg <- cbind(abs(mu.error)*180/pi)
mn.error.deg <- cbind(abs(mn.error)*180/pi)

#################################################################
#	Store Data														#
#################################################################
assessment$med.rhat[j] <- median(rhat(fit))#median µ err
assessment$max.rhat[j] <- max(rhat(fit))#median µ err
assessment$max.mu.e[j] <- max(mu.error.deg)##max µ err
assessment$med.mu.e[j] <- median(mu.error.deg)#median µ err
assessment$max.mu.e[j] <- max(mu.error.deg)##max µ err
assessment$med.ray.mu.e[j] <- median(mn.error.deg)#median rayleigh µ err
assessment$max.ray.mu.e[j] <- max(mn.error.deg)#max rayleigh µ err
assessment$med.rho.e[j] <- median(rho.error)#median rho err
assessment$max.rho.e[j] <- max(rho.error)#max rho err
assessment$med.ray.rho.e[j] <- median(mv.error)#median rayleigh rho err
assessment$max.ray.rho.e[j] <- max(mv.error)#max rayleigh rho err
j <- j+1
})#close try fit
#################################################################
#	Close & Save												#
#################################################################
}#for(i in 1:ni)# close for loop

save(assessment, file = paste0(file = paste0(Sys.getenv('HOME'), '/Dropbox/R scripts/', 'Assessment.','n=',ni,'uvMEmises6-2.10mean.lkappa1.5.mod.Rdata')))
load(file = paste0(file = paste0(Sys.getenv('HOME'), '/Dropbox/R scripts/', 'Assessment.','n=',ni,'uvMEmises6-2.10mean.lkappa1.5.mod.Rdata')))

#################################################################
#	Plot Assessment												#
#################################################################
µ <- cbind(assessment$med.ray.mu.e,assessment$med.mu.e)
colnames(µ) <- c('Rayleigh test','Bayes')
rr <- cbind(assessment$med.ray.rho.e,assessment$med.rho.e)
colnames(rr) <- c('Rayleigh test','Bayes')

µ.max <- cbind(assessment$max.ray.mu.e,assessment$max.mu.e)
colnames(µ.max) <- c('Rayleigh test','Bayes')
rr.max <- cbind(assessment$max.ray.rho.e,assessment$max.rho.e)
colnames(rr.max) <- c('Rayleigh test','Bayes')

dev.new(width = 10)
par(mfrow = c(1,4), mai = c(0.5, 1.0, 0.5, 0.1), cex = 1.1)
boxplot(µ, ylab = 'median error estimating mean angle (°)', ylim = c(0,180) , cex.axis = 0.6, col = c('slateblue', 'salmon') ,
		pars = list(boxwex = 0.3, staplewex = 0.5, outwex = 0.5))
abline(h=c(0,180))
boxplot(µ.max, ylab = 'maximum error estimating mean angle (°)', ylim = c(0,180) , cex.axis = 0.6, col = c('slateblue', 'salmon') ,
		pars = list(boxwex = 0.3, staplewex = 0.5, outwex = 0.5))
abline(h=c(0,180))
boxplot(rr, ylab = 'median error estimating mean vector length', ylim = c(0,max(rr.max)) , cex.axis = 0.6, col = c('seagreen', 'orange') ,
		pars = list(boxwex = 0.3, staplewex = 0.5, outwex = 0.5))
abline(h=c(0,1))
boxplot(rr.max, ylab = 'maximum error estimating mean vector length', ylim = c(0,max(rr.max)) , cex.axis = 0.6, col = c('seagreen', 'orange') ,
		pars = list(boxwex = 0.3, staplewex = 0.5, outwex = 0.5))
abline(h=c(0,1))
mtext(paste0('10 beetles, 10 trials; ',ni,' simulations','\nlog(kappa)=1.5±0.1'), 3, outer = T, line = -2)
suppressWarnings(
dev.copy(pdf, paste0(Sys.getenv('HOME'), '/Dropbox/My Papers/Light Pollution/', 	
				'Assessment.uvMEmises6','n=',ni,'.pdf'),
	width= par("din")[1], height= par("din")[2], useDingbats = F)	);
	dev.off();dev.set(dev.prev())