load_params <- function() {
  
  l_params <- list(
    ### Transition probabilities
    ## Natural history parameters
    # proportion of population immune
    p_imm     = 0.5,
    # infection rate (0.5-2% during nonepidemic years, 5-10% during epidemic years)
    p_inf    = 0.075,
    #  early infection rate calculation
    #p_einf = ((1/14)*(27-sqrt(729-560*(0.07)))),
    # p_einf = .00770601, 14-20 weeks inf = .0023118
    ## Clinical parameters
    # proportion of maternal infections detected
    p_det_surv = 0.5,
    p_det_sq   = 0.05,
    p_det_inf  = 0.5,  # probability of detection if assumed infected at first visit
    p_det_sus  = 0.5,  # probability of detection if assumed susceptible at first visit
    p_det_imm  = 0.05, # probability of detection if assumed immune at first visit
    # probability of fetus developing severe fetal anemia
    p_sfa      = 0.075,
    # probability of transfusion after development of severe fetal anemia
    p_det_it    = (0.95),  # maternal infection detected
    p_und_it    = (0.80),  # maternal infection undetected
    
    # probability of b19 fetal death being stillbirth
    p_sb = 0.13, 
    
    # probability of live birth
    p_lb        = 0.9,
    # probability of live birth if mother detected and severe fetal anemia
    p_det_sfa_lb = (0.9)*(0.891),
    # probability of live birth if mother undetected and severe fetal anemia
    p_und_sfa_lb = (0.9)*(0.807),
    # probability of live birth if severe fetal anemia is untreated in detected/undetected
    p_det_sfa_lb_nt  = (0.01)*(0.9),
    p_und_sfa_lb_nt = (0.01)*(0.9),
    # probability of miscarriage (fetal death when maternal infection occurs pre 14 weeks)
    p_lb_ei = (1-0.1323),
    
    ## test characteristics
    # IgG - Past Immunity
    p_igg_sens_immune = 0.9576,
    p_igg_spec_immune = 0.9785,
    
    # IgG - Acute Infection
    p_igg_sens_inf = 0.9331,
    p_igg_spec_inf = 0.9785,
    
    # IgM - Past Immunity
    p_igm_sens_immune = 0.9489,
    p_igm_spec_immune = 0.9213,
    
    # IgM - Acute Infection
    p_igm_sens_inf = 0.8723,
    p_igm_spec_inf = 0.9213,
    
    # PCR
    p_pcr_sens = 0.927,
    p_pcr_spec = 1,
    
    # Vaccination
    p_vax     = 0.5,  # maybe 33%?
    p_vax_eff = 0.9,
    
    # QALY
    #qaly_lb = 78.8,
    #qaly_fd = 0,
    
    # Cost
    # average cost of live birth
    c_lb = 11000,
    # average cost of fetal death
    c_fd = 6000,
    # average cost of severe fetal anemia
    c_sfa = 2000,
    # average cost of detecting infection in mother
    c_mid = 130,
    # average cost of monitoring for severe fetal anemia (400 per ultrasound, with average of 6 weeks monitoring)
    c_mon = 2400,
    # average cost of status quo surveillance
    c_sq = 0,
    # average cost of surveillance
    c_surv = 124000,
    
    # Deaths averted
    # number of births
    d_lb = 0,
    # number of deaths
    d_fd = 1,
    # non pvb19 associated birth or death
    d_p_lb = 0,
    # pvb19 associated death
    d_p_fd = 1,
    
    # Expected number of intrauterine transfusions
    # number of transfusions
    n_it = 1,
    # number of no transfusions
    n_nt = 0,
    
    # population parameters
    pop_size = 3600000
  )
  
  return(l_params)
}

get_params_sa <- function() {
  l_params_sa <- list(
    ### Transition probabilities
    ## Natural history parameters
    # proportion of population immune
    p_imm     = c(0.25, 0.75),
    # infection rate (1-4% during nonepidemic years, 10-15% during epidemic years)
    p_inf    = c(0.01, 0.2),
    ## Clinical parameters
    # proportion of maternal infections detected
    p_det_sq   = c(0.01, 0.2),
    p_det_surv = c(0.05, 0.7),
    p_det_inf  = c(0.05, 0.7),  # probability of detection if assumed infected at first visit
    p_det_sus  = c(0.05, 0.7),  # probability of detection if assumed susceptible at first visit
    p_det_imm  = c(0.01, 0.2),  # probability of detection if assumed immune at first visit
    # probability of fetus developing severe fetal anemia
    p_sfa      = c(0.02, 0.15),
    # probability of transfusion given
    p_det_it    = c(0.9, 1),  # maternal infection detected
    p_und_it    = c(0.6, 1),  # maternal infection undetected
    
    # probability of live birth
    p_lb         = c(0.85, 0.95),
    # probability of live birth if mother detected and severe fetal anemia
    p_det_sfa_lb = c((0.9)*(0.79), (0.9)*(0.99)),
    # probability of live birth if mother undetected and severe fetal anemia
    p_und_sfa_lb = c((0.9)*(0.7), (0.9)*(0.9)),
    # probability of live birth if severe fetal anemia is untreated in detected/undetected
    p_det_sfa_lb_nt  = c(0, (0.05)*(0.9)),
    p_und_sfa_lb_nt = c(0, (0.05)*(0.9)),
    # probability of miscarriage (fetal death when maternal infection occurs pre 14 weeks)
    p_lb_ei = c(0.828, 0.919),
    
    ## test characteristics
    # IgG - Past Immunity
    p_igg_sens_immune = c(qbeta(0.025, 1016, 45), qbeta(0.975, 1016, 45)),
    p_igg_spec_immune = c(qbeta(0.025, 727, 16), qbeta(0.975, 727, 16)),
    
    # IgG - Acute Infection
    p_igg_sens_inf = c(qbeta(0.025, 251, 18), qbeta(0.975, 251, 18)),
    p_igg_spec_inf = c(qbeta(0.025, 727, 16), qbeta(0.975, 727, 16)),
    
    # IgM - Past Immunity
    p_igm_sens_immune = c(qbeta(0.025, 464, 25), qbeta(0.975, 464, 25)),
    p_igm_spec_immune = c(qbeta(0.025, 199, 17), qbeta(0.975, 199, 17)),
    
    # IgM - Acute Infection
    p_igm_sens_inf = c(qbeta(0.025, 123, 18), qbeta(0.975, 123, 18)),
    p_igm_spec_inf = c(qbeta(0.025, 199, 17), qbeta(0.975, 199, 17)),
    
    # PCR
    p_pcr_sens = c(qbeta(0.025, 38, 3), qbeta(0.975, 38, 3)),
    p_pcr_spec = c(qbeta(0.025, 63, 0), qbeta(0.975, 63, 0)),
    
    # Vaccination
    p_vax     = c(0.2, 0.8),
    p_vax_eff = c(0.5, 1)
  )
  return(l_params_sa)
}


get_params_psa <- function(n_samp) {
  df_psa <- data.frame(
    nsamp = 1:n_samp,
    ### Transition probabilities
    ## Natural history parameters
    # proportion of population immune
    mc2d::rpert(n_samp, min = 0.25, mode = 0.5, max = 0.75),
    # infection rate (1-4% during nonepidemic years, 10-15% during epidemic years)
    mc2d::rpert(n_samp, min = 0.01, mode = 0.075, max = 0.2),
    ## Clinical parameters
    # proportion of maternal infections detected
    mc2d::rpert(n_samp, min = 0.01, mode = 0.05,max = 0.2),
    mc2d::rpert(n_samp, min = 0.05, mode = 0.5,max = 0.7),
    mc2d::rpert(n_samp, min = 0.05, mode = 0.5,max = 0.7),  # probability of detection if assumed infected at first visit
    mc2d::rpert(n_samp, min = 0.05, mode = 0.5,max = 0.7),  # probability of detection if assumed susceptible at first visit
    mc2d::rpert(n_samp, min = 0.01, mode = 0.05,max = 0.2),  # probability of detection if assumed immune at first visit
    # probability of fetus developing severe fetal anemia
    mc2d::rpert(n_samp, min = 0.02, mode = 0.075, max = 0.15),
    # probability of transfusion given
    mc2d::rpert(n_samp, min = 0.9, mode = 0.95, max = 1),  # maternal infection detected
    mc2d::rpert(n_samp, min = 0.6, mode = 0.80, max = 1),  # maternal infection undetected
    
    # probability of live birth
    mc2d::rpert(n_samp, min = 0.85, mode = 0.9, max = 0.95),
    # probability of live birth if mother detected and severe fetal anemia
    mc2d::rpert(n_samp, min = (0.9)*(0.79), mode = (0.9)*(0.891), max = (0.9)*(0.99)),
    # probability of live birth if mother undetected and severe fetal anemia
    mc2d::rpert(n_samp, min = (0.9)*(0.7), mode = (0.9)*(0.807), max = (0.9)*(0.9)),
    # probability of live birth if severe fetal anemia is untreated in detected/undetected
    mc2d::rpert(n_samp, min = 0, mode = (0.01)*(0.9), max = (0.05)*(0.9)),
    mc2d::rpert(n_samp, min = 0, mode = (0.01)*(0.9), max = (0.05)*(0.9)),
    # probability of miscarriage (fetal death when maternal infection occurs pre 14 weeks)
    mc2d::rpert(n_samp, min = 0.828, mode = 0.8677, max = 0.919),

    ## test characteristics
    # IgG - Past Immunity
    rbeta(n_samp, 1016, 45),
    rbeta(n_samp, 727, 16),
    
    # IgG - Acute Infection
    rbeta(n_samp, 251, 18),
    rbeta(n_samp, 727, 16),
    
    # IgM - Past Immunity
    rbeta(n_samp, 464, 25),
    rbeta(n_samp, 199, 17),
    
    # IgM - Acute Infection
    rbeta(n_samp, 123, 18),
    rbeta(n_samp, 199, 17),
    
    # PCR
    rbeta(n_samp, 38, 3),
    rbeta(n_samp, 63, 0),
    
    # Vaccination
    mc2d::rpert(n_samp, min = 0.2, mode = 0.5, max = 0.8 ),
    mc2d::rpert(n_samp, min = 0.5, mode = 0.9, max = 1 )
  )
  
  return(df_psa)
}

my_dists <- c(
  ### Transition probabilities
  ## Natural history parameters
  # proportion of population immune
  "beta",
  # infection rate (1-4% during nonepidemic years, 10-15% during epidemic years)
  "beta",
  ## Clinical parameters
  # proportion of maternal infections detected
  "beta",
  "beta",
  "beta",  # probability of detection if assumed infected at first visit
  "beta",  # probability of detection if assumed susceptible at first visit
  "beta",  # probability of detection if assumed immune at first visit
  # probability of fetus developing severe fetal anemia
  "beta",
  # probability of transfusion given
  "beta",  # maternal infection detected
  "beta",  # maternal infection undetected
  
  # probability of live birth
  "beta",
  # probability of live birth if mother undetected and severe fetal anemia
  "beta",
  # probability of live birth if mother detected and severe fetal anemia
  "beta",
  # probability of live birth if severe fetal anemia is untreated in detected/undetected
  "beta",
  "beta",
  # probability of miscarriage (fetal death when maternal infection occurs pre 14 weeks)
  "beta",
  
  ## test characteristics
  # IgG - Past Immunity
  "beta",
  "beta",
  
  # IgG - Acute Infection
  "beta",
  "beta",
  
  # IgM - Past Immunity
  "beta",
  "beta",
  
  # IgM - Acute Infection
  "beta",
  "beta",
  
  # PCR
  "beta",
  "beta",
  
  # Vaccination
  "beta",
  "beta"
)

my_parameterization_types <- c(
  ### Transition probabilities
  ## Natural history parameters
  # proportion of population immune
  "a, b",
  # infection rate (1-4% during nonepidemic years, 10-15% during epidemic years)
  "a, b",
  ## Clinical parameters
  # proportion of maternal infections detected
  "a, b",
  "a, b",
  "a, b",  # probability of detection if assumed infected at first visit
  "a, b",  # probability of detection if assumed susceptible at first visit
  "a, b",  # probability of detection if assumed immune at first visit
  # probability of fetus developing severe fetal anemia
  "a, b",
  # probability of transfusion given
  "a, b",  # maternal infection detected
  "a, b",  # maternal infection undetected
  
  # probability of live birth
  "a, b",
  # probability of live birth if mother undetected and severe fetal anemia
  "a, b",
  # probability of live birth if mother detected and severe fetal anemia
  "a, b",
  # probability of live birth if severe fetal anemia is untreated in detected/undetected
  "a, b",
  "a, b",
  # probability of miscarriage (fetal death when maternal infection occurs pre 14 weeks)
  "a, b",
  
  ## test characteristics
  # IgG - Past Immunity
  "a, b",
  "a, b",
  
  # IgG - Acute Infection
  "a, b",
  "a, b",
  
  # IgM - Past Immunity
  "a, b",
  "a, b",
  
  # IgM - Acute Infection
  "a, b",
  "a, b",
  
  # PCR
  "a, b",
  "a, b",
  
  # Vaccination
  "a, b",
  "a, b"
)

# optim_beta_params <- function(ll, ul) {
#   optim_result <- optim(par = c(1000, 10), fn = gen_sse_beta,
#                         ll_target = ll,
#                         ul_target = ul,
#                         method = "Nelder-Mead")
#   alpha  <- optim_result$par[1]
#   beta   <- optim_result$par[2]
#   
#   return(c(alpha = alpha, beta = beta))
# }
# gen_sse_beta <- function(params, ll_target, ul_target) {
#   alpha <- max(0, params[1])
#   beta  <- max(0, params[2])
#   v_samples  <- rbeta(100000, shape1 = alpha, shape2 = beta)
#   ll_samples <- quantile(v_samples, 0.025)
#   ul_samples <- quantile(v_samples, 0.975)
#   out <- 1 * ((ll_target - ll_samples)^2 + (ul_target - ul_samples)^2)
#   if(params[1] < 0 | params[2] < 0) {
#     out <- Inf
#   }
#   return(out)
# }



CorrelateUtils <- function(U, Q, epsilon, delta, tol){
  n <- nrow(U) #number of PSA samples
  s <- ncol(U) #number of states
  R <- matrix(rnorm(n*s,0,1), n, s) #the reference matrix.
  C <- matrix(0,s,s) #a place holder for the correlation matrix
  viol <- matrix(0,s,s) #violations matrix.
  for (j in 2:s){ # {j,k} is the selected pair of state comparisons
    for (k in 1:(j-1)){
      rho <- 1 # #bivariate correlations
      X = U[,c(j,k)] #selected columns of U
      Y = R[,c(j,k)] #selected columns of R
      viol <- 0
      while(viol<epsilon && rho>=0){ #if these conditions are met, continue
        rho <- rho - delta #reduce correltion
        Xstar = induceRankCorrelation(X, Y, rho) #correlated utilities.
        viol = mean((Q[j,k] * Xstar[,1]) < (Q[j,k] * Xstar[,2])) #compute %violations between the col. vectors.
      }
      #Viol[j,k] <- viol
      C[j,k] <- rho + delta #record the desired correlation.
    }
    # print(j) #just to show the column indices.
    # print(k)
  }
  #Fill in the other elements of C.
  C = C + t(C)
  for (j in 1:s){
    C[j,j] <- 1 # % the diagonal of ones.
  }
  ## Eigenvectors and Eigenvalues correction of C
  eigenResults <- eigen(C)
  B <- eigenResults$values
  V <- eigenResults$vectors
  B[B<=0] <- tol #to make sure C is positive definite, set eigenvalues<=0 to a very small positive number
  Cstar <- V %*% diag(B) %*% solve(V) #reconstruct C
  Ustar <- induceRankCorrelation(U, R, Cstar) #similar to above, induce the correlation.
  return(Ustar)
}
## To induce Rank correlation: inputs X: QoL vectors, Y is the reference vectors, and
## Sigma is the correlation matrix.

induceRankCorrelation <- function(X, Y, Sigma){
  if (length(Sigma)==1){ #if Sigma is a single value, convert it to a 2x2 matrix.
    Sigma <- matrix(c(1, Sigma,
                      Sigma, 1), 2, 2)
  }
  n <- nrow(X)
  s <- ncol(X)
  
  #Initialize matrices.
  Xsorted <- matrix(0, n, s)
  Yrank <- matrix(0, n, s)
  Xstar <- matrix(0, n, s)
  P <- chol(Sigma) #compute the upper triangular matrix
  Ystar <- Y %*% P #Sort the values in the reference vectors by multiplying by P
  cor(Ystar)
  for (j in 1:s){
    Xsorted[,j] <- sort(X[,j]) #Sort each variable
    Yrank[order(Ystar[,j]),j] <- seq(1:n) #Reverse sort
    Xstar[,j]=Xsorted[Yrank[,j],j] #sort Xsorted to have the same ranks as Ystar.
  }
  return(Xstar) #return the sorted vectors.
}

# Function to generate ordinal QALYs
ordered_params <- function(n.runs, v_min, v_mode, v_max, betas, tol) {
  n <- n.runs
  s <- 2
  
  m_U <- matrix(0, n, s)
  m_U[,1] <- mc2d::rpert(n, min = v_min[1], mode = v_mode, max = v_max[1])
  m_U[,2] <- mc2d::rpert(n, min = v_min[2], mode = v_mode, max = v_max[2])
  
  m_Q <- matrix(c(0,  0,
                  -1, 0), s, s, byrow = T)
  
  epsilon <- 0.05
  delta <- 0.01
  
  m_Ustar <- CorrelateUtils(m_U, m_Q, epsilon, delta, tol) #Induce correlation, and return Ustar.
  
  return(m_Ustar)
}

# my_dists_params <- list(
#   ### Transition probabilities
#   ## Natural history parameters
#   # proportion of population immune
#   optim_beta_params(0.25, 0.75),
#   # infection rate (1-4% during nonepidemic years, 10-15% during epidemic years)
#   optim_beta_params(0.01, 0.2),
#   ## Clinical parameters
#   # proportion of maternal infections detected
#   optim_beta_params(0.01, 0.2),
#   optim_beta_params(0.05, 0.7),
#   optim_beta_params(0.05, 0.7),  # probability of detection if assumed infected at first visit
#   optim_beta_params(0.05, 0.7),  # probability of detection if assumed susceptible at first visit
#   optim_beta_params(0.01, 0.2),  # probability of detection if assumed immune at first visit
#   # probability of fetus developing severe fetal anemia
#   optim_beta_params(0.02, 0.15),
#   # probability of transfusion given
#   optim_beta_params(0.9, 1),  # maternal infection detected
#   optim_beta_params(0.6, 1),  # maternal infection undetected
#   
#   # probability of live birth
#   optim_beta_params(0.85, 0.95),
#   # probability of live birth if mother detected and severe fetal anemia
#   optim_beta_params((0.9)*(0.79), (0.9)*(0.99)),
#   # probability of live birth if mother undetected and severe fetal anemia
#   optim_beta_params((0.9)*(0.7), (0.9)*(0.9)),
#   # probability of live birth if severe fetal anemia is untreated in detected/undetected
#   optim_beta_params(0, (0.05)*(0.9)),
#   optim_beta_params(0, (0.05)*(0.9)),
#   # probability of miscarriage (fetal death when maternal infection occurs pre 14 weeks)
#   optim_beta_params(0.828, 0.919),
#   
#   ## test characteristics
#   # IgG - Past Immunity
#   c(1016, 45),
#   c(727, 16),
#   
#   # IgG - Acute Infection
#   c(251, 18),
#   c(727, 16),
#   
#   # IgM - Past Immunity
#   c(464, 25),
#   c(199, 17),
#   
#   # IgM - Acute Infection
#   c(123, 18),
#   c(199, 17),
#   
#   # PCR
#   c(38, 3),
#   c(63, 0),
#   
#   # Vaccination
#   optim_beta_params(0.2, 0.8),
#   optim_beta_params(0.5, 1)
# )



