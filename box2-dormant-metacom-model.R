# Code to run simulations and make figures for "Dormancy in Metacommunities" manuscript

# Load packages
library(tidyverse)
library(vegan)
library(progress)
library(vegetarian)
library(zoo)


###############################################################################
rm(list = ls())

set.seed(47405)

# Define model parameters
tsteps <- 30000      # Number of time steps in model
dt <- 1    # precision for model integration (step size)
M <- 20 # Number of sites
S <- 20 # Number of species
ext <- .01 # extinction thresh
disturb <- 0.001
env.type <- "static" # "static", "fluctuating", "random" potential options  
spatial.synchrony <- 0 #can range from 0 to 1, what fraction of patches have the same environment
envs <- 1 # Number of environmental variables


# set up heterogeneous environment (E)
E <- matrix(seq(0, 1, length.out = M), nrow = M, ncol = envs)
env.ampl <- 1 # amplitude of env variability [0,1]
env.period <- 1000 # bigger numbers, slower oscilations, more static env

# Function to control spatiotemporal asynchrony
E.j <- function(t, synch = spatial.synchrony, stoch = F){
  0.5*env.ampl*(
    sin((1:M)*2*pi/(M*(1-synch)) + (2*pi/env.period)*t) + 1) + ifelse(stoch, abs(rnorm(1, sd = .1)), 0)
  
}

###############################################################################
# Define metacommunity functions


# growth rate in a patch based on environmental match 
R.jx <- function(E, max.R, opt.envs, nbreadth){
  max.R * exp(-((E - opt.envs)^2/(2*nbreadth^2)))
}

# competition function, summed across all individuals in a patch
competition <- function(N,a) {
  return( 1 / ( 1 + a*(sum(N))))
}

# global dispersal function
disperse <- function(d, N, M){
  
  dispersal_matrix <- matrix(1/(M-1), nrow = M, ncol = M)
  diag(dispersal_matrix) <- 0
  
  return((N * d) %*% dispersal_matrix)

}




###############################################################################
# Definte species traits

# establish strength of species sorting/local control via niche aspects
opt.envs <- seq(0, 1, length.out = S) # species optimal environments
nbreadth <- .5 # as approach infinity, metacom approaches neutrality
max.R <- 1.2
a <- 4e-4 # strength of competition

decay <- rep(.000001, S) # Decay rate of dormant propagules
activ <- rep(0.1, S) # Reactivation rate

# gradients of dormancy and dispersal along which to evaluate diversity
dorm.grad <- c(0, 0.7)
disp.grad <- seq(0, .5, by = 0.01)

# construct output table, columns are as follows
# 1 - ddcov, 2 - dispersal, 3 - dorm, 4-6 - alpha, beta, gamma div
out.sum <- matrix(NA, nrow = 2*length(disp.grad)*length(dorm.grad), ncol = 6)

# set up loops
i = 1
loops <- length(out.sum)


for(ddcov in c(0,1)){
  
  # loop over dorm rates
  for(dorm in dorm.grad){
  
    # loop over dipsersal rates
    for(d in disp.grad){
      
      if(i == 1) pb <- progress_bar$new(total = loops, force = T)
      
      # update progress bar
      pb$update(ratio = i/loops)
    
      set.seed(47405) # so each sim is the same when there's disturbance
      
      # initialize result array, Species X Sites X Time 
      out.N <- array(NA, c(S, M, tsteps), 
                     dimnames = list(c(paste0("sp",1:S)), c(paste0("site",1:M)), c(1:tsteps)))
      out.D <- array(NA, c(S, M, tsteps),
                     dimnames = list(c(paste0("sp",1:S)), c(paste0("site",1:M)), c(1:tsteps)))
      
      # add active species to sites, seed banks are empty
      out.N[,,1] <- 1
      out.D[,,1] <- 0
      
        for(t in 1:(tsteps-1)){
          
          # if(t == 1) pb <- progress_bar$new(total = tsteps, force = T)
          
          # update progress bar
          # pb$update(ratio = t/(tsteps))
          # get current abunds
          N.t <- out.N[,,t]
          D.t <- out.D[,,t]
          
          # calculates growth rates for each species (rows) in each site (cols)
          # dimensions = S x M
          if(env.type == "fluctuating") E <- matrix(E.j(t), nrow = M, ncol = 1)
          if(env.type == "random") E <- matrix(runif(M), nrow = M, ncol = 1)
          R.t <- apply(X = E, MARGIN = 1, FUN = R.jx, max.R, opt.envs, nbreadth)
          
          # dormancy transitions
          # previous + dormancy - reactivation
          D.t1 <- D.t + (N.t * dorm) - (D.t * activ)
          # previous + reactivation - dormancy
          N.t1 <- N.t + (D.t * activ) - (N.t * dorm)
          
          # calculate competition at this time
          comp.t <- apply(N.t1, MARGIN = 2, FUN = competition, a = a)
          
          # growth and seed bank decay
          N.t2 <- R.t * N.t1 * comp.t
          D.t2 <- D.t1 - decay * D.t1
          
          # calculate immigration and then remove emigrants
          N.t3 <- N.t2 + disperse(d, N.t2, M) - (d * N.t2)
          D.t3 <- D.t2 + ddcov*(disperse(d, D.t2, M) - (d * D.t2))
          
          # patch disturbance
          N.t3[, which(rbernoulli(M, p = disturb) == 1)] <- 0
          
          out.N[,,t+1] <- ifelse(N.t3 > ext, N.t3, 0)
          out.D[,,t+1] <- ifelse(D.t3 > ext, D.t3, 0) 
          
        }
    
    # save final time point
    comm <- t(out.N[,,tsteps])
    comm[is.na(comm)] <- 0
    
    # partition diversity
    (alpha <- vegetarian::d(comm, lev = "alpha", q = 1))
    (beta <- vegetarian::d(comm, lev = "beta", q = 1))
    (gamma <- vegetarian::d(comm, lev = "gamma", q = 1))
    
    # write out dispersal, dormancy, and diversity
    out.sum[i,] <- c(ddcov, d, dorm, alpha, beta, gamma)
    i <- i + 1
    }
  }
}


colnames(out.sum) <- c("DDcov", "Dispersal", "Dormancy", "alpha", "beta", "gamma")

# make figure to compare dispersal-diversity relationships 
# To get facet labels correct, must create expressions, then parse with labeller function
# because DDcov labels aren't expressions to parse, include in single quotes
data.to.plot <- as.data.frame(out.sum) %>% 
  gather("alpha", "beta", "gamma", key = Scale, value = Diversity) %>%
  mutate(Dormancy = factor(Dormancy, levels = dorm.grad, ordered = T)) %>% 
  mutate(DDcov = ifelse(DDcov == 0, "'Negative Covariation'", "'Positive Covariation'"))  
data.to.plot$Scale <- factor(data.to.plot$Scale)
levels(data.to.plot$Scale) <- c(
  expression(paste(alpha, "-diversity")),
  expression(paste(beta, "-diversity")),
  expression(paste(gamma, "-diversity")))
data.to.plot %>% 
  ggplot(aes(Dispersal, Diversity, color = Dormancy, linetype = Dormancy)) +
  geom_line(size = 1, alpha = 1, show.legend = F) + 
  facet_grid(Scale ~ DDcov, scales = "free_y", switch = "y", labeller = label_parsed) +
  theme_minimal() + 
  scale_x_continuous(limits = c(0,.5)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("grey60", "grey20")) + 
  scale_linetype_manual(values = c("longdash", "solid")) + 
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = "grey80"),
        legend.text = element_text(size = 8), 
        legend.key.size = unit(0.3, "cm"),
        strip.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.placement = "outside",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 10)) + 
  labs(y = "") +
  ggsave("figures/box2.png", dpi = 1000, width = 6, height = 6) +
  ggsave("figures/box2.pdf", width = 6, height = 6)
