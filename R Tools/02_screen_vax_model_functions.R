# Model Function
parvo_model <- function(l_params_all) {
  with(
    as.list(l_params_all),
    {
      # vector of weights for each policy
      # status quo vector (13 branches)
      v_w_sq <- c(
        ## Immune
        p_imm       * p_lb,
        p_imm       * (1 - p_lb),
        ## Susceptible, Never Infected
        (1 - p_imm) * (1 - p_inf) * p_lb,
        (1 - p_imm) * (1 - p_inf) * (1 - p_lb),
        ## Susceptible, Infected in weeks 1-13
        (1 - p_imm) * (p_inf * 0.65) * p_lb_ei * p_lb,
        (1 - p_imm) * (p_inf * 0.65) * p_lb_ei * (1 - p_lb),
        (1 - p_imm) * (p_inf * 0.65) * (1 - p_lb_ei),
        ## Susceptible, Infected in weeks 14-20
        (1 - p_imm) * (p_inf * 0.35) * p_sfa * ((p_det_sq * p_det_it * p_det_sfa_lb) + ((1 - p_det_sq) * p_und_it * p_und_sfa_lb)),
        (1 - p_imm) * (p_inf * 0.35) * p_sfa * ((p_det_sq * p_det_it * (1 - p_det_sfa_lb)) + ((1 - p_det_sq) * p_und_it * (1 - p_und_sfa_lb))),
        (1 - p_imm) * (p_inf * 0.35) * p_sfa * ((p_det_sq * (1 - p_det_it) * p_det_sfa_lb_nt) + ((1 - p_det_sq) * (1 - p_und_it) * p_und_sfa_lb_nt)),
        (1 - p_imm) * (p_inf * 0.35) * p_sfa * ((p_det_sq * (1 - p_det_it) * (1 - p_det_sfa_lb_nt)) + ((1 - p_det_sq) * (1 - p_und_it) * (1 - p_und_sfa_lb_nt))),
        (1 - p_imm) * (p_inf * 0.35) * (1 - p_sfa) * p_lb,
        (1 - p_imm) * (p_inf * 0.35) * (1 - p_sfa) * (1 - p_lb)
      )
      
      # Screening at visit in first trimester (31 branches)
      v_w_test <- c(
        ### Immune
        # Immune, Test Immune (TP IgG, TN IgM)
        p_imm * (p_igg_sens_immune * p_igm_spec_immune +
                   p_igg_sens_immune * (1 - p_igm_spec_immune) * p_pcr_spec) * p_lb,
        p_imm * (p_igg_sens_immune * p_igm_spec_immune +
                   p_igg_sens_immune * (1 - p_igm_spec_immune) * p_pcr_spec) * (1 - p_lb),
        # Immune, Test Infected or Susceptible
        p_imm * (p_igg_sens_immune * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                   (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                   (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * p_pcr_spec +
                   (1 - p_igg_sens_immune) * p_igm_spec_immune) * p_lb,
        p_imm * (p_igg_sens_immune * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                   (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                   (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * p_pcr_spec +
                   (1 - p_igg_sens_immune) * p_igm_spec_immune) * (1 - p_lb),
        
        ### Susceptible
        ## Never Infected
        # Test Immune (FP)
        (1 - p_imm) * (1 - p_inf) * ((1 - p_igg_spec_immune) * p_igm_spec_inf +
                                       (1 - p_igg_spec_immune) * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb,
        (1 - p_imm) * (1 - p_inf) * ((1 - p_igg_spec_immune) * p_igm_spec_inf +
                                       (1 - p_igg_spec_immune) * (1 - p_igm_spec_inf) * p_pcr_spec) * (1 - p_lb),
        # Test Infected (FP)
        (1 - p_imm) * (1 - p_inf) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb,
        (1 - p_imm) * (1 - p_inf) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * (1 - p_lb),
        # Test Susceptible (TN)
        (1 - p_imm) * (1 - p_inf) * ((p_igg_spec_immune * (1 - p_igm_spec_inf) * p_pcr_spec) +
                                       (p_igg_spec_immune * p_igm_spec_inf)) * p_lb,
        (1 - p_imm) * (1 - p_inf) * ((p_igg_spec_immune * (1 - p_igm_spec_inf) * p_pcr_spec) +
                                       (p_igg_spec_immune * p_igm_spec_inf)) * (1 - p_lb),
        
        ## Infected (1-13)
        # Test Immune (FP) (IgG+ and IgM- or IgM+/PCR-)
        (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                          p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb_ei * p_lb,
        (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                          p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb_ei * (1 - p_lb),
        (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                          p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * (1 - p_lb_ei),
        # Test Infected (FP)
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb_ei * p_lb,
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb_ei * (1 - p_lb),
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * (1 - p_lb_ei),
        # Test Susceptible (TN)
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                          (1 - p_igg_sens_immune) * p_igm_spec_inf) * p_lb_ei * p_lb,
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                          (1 - p_igg_sens_immune) * p_igm_spec_inf) * p_lb_ei * (1 - p_lb),
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                          (1 - p_igg_sens_immune) * p_igm_spec_inf) * (1 - p_lb_ei),
        
        ## Infected (14-20)
        # Test Immune or Susceptible
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          p_sfa * p_und_it * p_und_sfa_lb,
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          p_sfa * p_und_it * (1 - p_und_sfa_lb),
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          p_sfa * (1 - p_und_it) * p_und_sfa_lb_nt,
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf))+
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          p_sfa * (1 - p_und_it) * (1 - p_und_sfa_lb_nt),
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          (1 - p_sfa) * p_lb,
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          (1 - p_sfa) * (1 - p_lb),
        
        # Test Infected
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * p_sfa * p_det_it * p_det_sfa_lb,
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * p_sfa * p_det_it * (1 - p_det_sfa_lb),
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * p_sfa * (1 - p_det_it) * p_und_sfa_lb_nt,
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * p_sfa * (1 - p_det_it) * (1 - p_und_sfa_lb_nt),
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * (1 - p_sfa) * p_lb,
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * (1 - p_sfa) * (1 - p_lb)
      )
      
                            
      # Vaccination (13 branches)
      v_w_vax <- c(
        ## Immune
        (p_imm + (p_vax * p_vax_eff * (1 - p_imm))) * p_lb,
        (p_imm + (p_vax * p_vax_eff * (1 - p_imm))) * (1 - p_lb),
        ## Susceptible, Never Infected
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * (1 - p_lb),
        ## Susceptible, Infected in weeks 1-13
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * p_lb_ei * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * p_lb_ei * (1 - p_lb),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * (1 - p_lb_ei),
        ## Susceptible, Infected in weeks 14-20
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          p_sfa * ((p_det_sq * p_det_it * p_det_sfa_lb) + ((1 - p_det_sq) * p_und_it * p_und_sfa_lb)),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          p_sfa * ((p_det_sq * p_det_it * (1 - p_det_sfa_lb)) + ((1 - p_det_sq) * p_und_it * (1 - p_und_sfa_lb))),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          p_sfa * ((p_det_sq * (1 - p_det_it) * p_det_sfa_lb_nt) + ((1 - p_det_sq) * (1 - p_und_it) * p_und_sfa_lb_nt)),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          p_sfa * ((p_det_sq * (1 - p_det_it) * (1 - p_det_sfa_lb_nt)) + ((1 - p_det_sq) * (1 - p_und_it) * (1 - p_und_sfa_lb_nt))),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (1 - p_sfa) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (1 - p_sfa) * (1 - p_lb)
      )
      
      # Vax + Screening at first prenatal visit vector (28 branches)
      v_w_vax_test <- c(
        ### Immune
        # Immune, Test Immune (TP IgG, TN IgM)
        (p_imm + (p_vax * p_vax_eff * (1 - p_imm))) * (p_igg_sens_immune * p_igm_spec_immune +
                                                         p_igg_sens_immune * (1 - p_igm_spec_immune) * p_pcr_spec) * p_lb,
        (p_imm + (p_vax * p_vax_eff * (1 - p_imm))) * (p_igg_sens_immune * p_igm_spec_immune +
                                                         p_igg_sens_immune * (1 - p_igm_spec_immune) * p_pcr_spec) * (1 - p_lb),
        # Immune, Test Infected or Susceptible
        (p_imm + (p_vax * p_vax_eff * (1 - p_imm))) * (p_igg_sens_immune * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                                                         (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                                                         (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * p_pcr_spec +
                                                         (1 - p_igg_sens_immune) * p_igm_spec_immune) * p_lb,
        (p_imm + (p_vax * p_vax_eff * (1 - p_imm))) * (p_igg_sens_immune * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                                                         (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                                                         (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * p_pcr_spec +
                                                         (1 - p_igg_sens_immune) * p_igm_spec_immune) * (1 - p_lb),
        
        ### Susceptible
        ## Never Infected
        # Test Immune (FP)
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * ((1 - p_igg_spec_immune) * p_igm_spec_inf +
                                                                                   (1 - p_igg_spec_immune) * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * ((1 - p_igg_spec_immune) * p_igm_spec_inf +
                                                                                   (1 - p_igg_spec_immune) * (1 - p_igm_spec_inf) * p_pcr_spec) * (1 - p_lb),
        # Test Infected (FP)
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * (1 - p_lb),
        # Test Susceptible (TN)
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * ((p_igg_spec_immune * (1 - p_igm_spec_inf) * p_pcr_spec) +
                                                                                   (p_igg_spec_immune * p_igm_spec_inf)) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * ((p_igg_spec_immune * (1 - p_igm_spec_inf) * p_pcr_spec) +
                                                                                   (p_igg_spec_immune * p_igm_spec_inf)) * (1 - p_lb),
        
        ## Infected (1-13) (What to do with IgG test results)
        # Test Immune (FP) (IgG+ and IgM- or IgM+/PCR-)
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                                                                      p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb_ei * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                                                                      p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb_ei * (1 - p_lb),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                                                                      p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * (1 - p_lb_ei),
        # Test Infected (FP)
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb_ei * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb_ei * (1 - p_lb),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * (1 - p_lb_ei),
        # Test Susceptible (TN)
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                                                                      (1 - p_igg_sens_immune) * p_igm_spec_inf) * p_lb_ei * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                                                                      (1 - p_igg_sens_immune) * p_igm_spec_inf) * p_lb_ei * (1 - p_lb),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                                                                      (1 - p_igg_sens_immune) * p_igm_spec_inf) * (1 - p_lb_ei),
        
        ## Infected (14-20)
        # Test Immune or Susceptible
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
             ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
             (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
             (p_igg_spec_immune * (1 - p_igm_sens_inf))) * p_sfa * p_und_it * p_und_sfa_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
             ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
             (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
             (p_igg_spec_immune * (1 - p_igm_sens_inf))) * p_sfa * p_und_it * (1 - p_und_sfa_lb),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
             ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
             (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
             (p_igg_spec_immune * (1 - p_igm_sens_inf))) *  p_sfa * (1 - p_und_it) * p_und_sfa_lb_nt,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
             ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
             (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
             (p_igg_spec_immune * (1 - p_igm_sens_inf))) * p_sfa * (1 - p_und_it) * (1 - p_und_sfa_lb_nt),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
             ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
             (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
             (p_igg_spec_immune * (1 - p_igm_sens_inf))) * (1 - p_sfa) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
             ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
             (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
             (p_igg_spec_immune * (1 - p_igm_sens_inf))) * (1 - p_sfa) * (1 - p_lb),
        
        # Test Infected
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (p_igm_sens_inf * p_pcr_sens) * p_sfa * p_det_it * p_det_sfa_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (p_igm_sens_inf * p_pcr_sens) * p_sfa * p_det_it * (1 - p_det_sfa_lb),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (p_igm_sens_inf * p_pcr_sens) * p_sfa * (1 - p_det_it) * p_und_sfa_lb_nt,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (p_igm_sens_inf * p_pcr_sens) * p_sfa * (1 - p_det_it) * (1 - p_und_sfa_lb_nt),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (p_igm_sens_inf * p_pcr_sens) * (1 - p_sfa) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (p_igm_sens_inf * p_pcr_sens) * (1 - p_sfa) * (1 - p_lb)
      )
      
      ### Checks
      # print(sum(v_w_sq))
      # print(sum(v_w_test))
      # print(sum(v_w_vax))
      # print(sum(v_w_vax_test))
      
      ### Reward vectors
      ## All Deaths
      # status quo
      v_death_sq <- c(rep(c(d_lb, d_fd), 3), d_fd,
                      rep(c(d_lb, d_fd), 3))
      # Screening
      v_death_test <- c(rep(c(d_lb, d_fd), 5),
                        rep(c(d_lb, d_fd, d_fd), 3),
                        rep(c(d_lb, d_fd), 6))
      # vaccination
      v_death_vax <- c(rep(c(d_lb, d_fd), 3), d_fd,
                       rep(c(d_lb, d_fd), 3))
      # vaccination + Screening
      v_death_vax_test <- c(rep(c(d_lb, d_fd), 5),
                            rep(c(d_lb, d_fd, d_fd), 3),
                            rep(c(d_lb, d_fd), 6))
      
      ## Parvo B19 Deaths
      # status quo
      v_b19_death_sq <- c(rep(0, 6), 1, rep(c(0, 1), 2), 0, 0)
      # Screening
      v_b19_death_test <- c(rep(0, 10), rep(c(0, 0, 1), 3), rep(c(0, 1, 0, 1, 0, 0), 2))
      # vaccination
      v_b19_death_vax <- c(rep(0, 6), 1, rep(c(0, 1), 2), 0, 0)
      # vaccination + Screening
      v_b19_death_vax_test <- c(rep(0, 10), rep(c(0, 0, 1), 3), rep(c(0, 1, 0, 1, 0, 0), 2))     
      
      ## Transfusions
      # status quo
      v_transfusion_sq <- c(rep(n_nt, 7), n_it, n_it, rep(n_nt, 4))
      # vector of transfusions - Screening
      v_transfusion_test <- c(rep(n_nt, 19), n_it, n_it, rep(n_nt, 4), n_it, n_it, rep(n_nt, 4))
      # vector of transfusions - vaccination
      v_transfusion_vax <- c(rep(n_nt, 7), n_it, n_it, rep(n_nt, 4))
      # vector of transfusions - vaccination + Screening
      v_transfusion_vax_test <- c(rep(n_nt, 19), n_it, n_it, rep(n_nt, 4), n_it, n_it, rep(n_nt, 4))
      
      ## B19 Stillbirths
      # status quo
      v_b19_sb_sq       <- c(rep(d_p_lb, 7), rep(c(d_p_lb, d_p_fd), 2), d_p_lb, d_p_lb)
      # Screening
      v_b19_sb_test     <- c(rep(d_p_lb, 19), rep(c(rep(c(d_p_lb, d_p_fd), 2), d_p_lb, d_p_lb), 2))
      # vaccination
      v_b19_sb_vax      <- c(rep(d_p_lb, 7), rep(c(d_p_lb, d_p_fd), 2), d_p_lb, d_p_lb)
      # vaccination + Screening
      v_b19_sb_vax_test <- c(rep(d_p_lb, 19), rep(c(rep(c(d_p_lb, d_p_fd), 2), d_p_lb, d_p_lb), 2))
      
      # total deaths per policy
      total_death_sq       <- v_w_sq %*% v_death_sq %*% pop_size
      total_death_test     <- v_w_test %*% v_death_test %*% pop_size
      total_death_vax      <- v_w_vax %*% v_death_vax %*% pop_size
      total_death_vax_test <- v_w_vax_test %*% v_death_vax_test %*% pop_size
      
      # total parvo b19 deaths per policy
      total_b19_death_sq       <- v_w_sq %*% v_b19_death_sq %*% pop_size
      total_b19_death_test     <- v_w_test %*% v_b19_death_test %*% pop_size
      total_b19_death_vax      <- v_w_vax %*% v_b19_death_vax %*% pop_size
      total_b19_death_vax_test <- v_w_vax_test %*% v_b19_death_vax_test %*% pop_size
      
      # total transfusions per policy
      total_it_sq       <- v_w_sq %*% v_transfusion_sq %*% pop_size
      total_it_test     <- v_w_test %*% v_transfusion_test %*% pop_size
      total_it_vax      <- v_w_vax %*% v_transfusion_vax %*% pop_size
      total_it_vax_test <- v_w_vax_test %*% v_transfusion_vax_test %*% pop_size
      
      # total parvo b19 deaths per policy
      total_b19_sb_sq       <- v_w_sq       %*% v_b19_sb_sq %*% pop_size
      total_b19_sb_test     <- v_w_test     %*% v_b19_sb_test %*% pop_size
      total_b19_sb_vax      <- v_w_vax      %*% v_b19_sb_vax %*% pop_size
      total_b19_sb_vax_test <- v_w_vax_test %*% v_b19_sb_vax_test %*% pop_size
      
      # Incremental Results
      # inc_deaths <- total_death_sq - total_death_test
      # inc_it     <- total_it_sq - total_it_test
      
      # print(sum(v_w_sq))
      # print(sum(v_w_test))
      # print(sum(v_w_vax))
      # print(sum(v_w_vax_test))
      
      df_output <- data.frame(Strategy        = v_names_str,
                              Deaths          = c(total_death_sq,
                                                  total_death_test,
                                                  total_death_vax,
                                                  total_death_vax_test),
                              Transfusions    = c(total_it_sq,
                                                  total_it_test,
                                                  total_it_vax,
                                                  total_it_vax_test),
                              B19_Deaths      = c(total_b19_death_sq,
                                                  total_b19_death_test,
                                                  total_b19_death_vax,
                                                  total_b19_death_vax_test),
                              B19_Stillbirths = c(total_b19_sb_sq,
                                                  total_b19_sb_test,
                                                  total_b19_sb_vax,
                                                  total_b19_sb_vax_test))
      return(df_output)})
}


parvo_model_tw <- function(l_params_all) {
  with(
    as.list(l_params_all),
    {
      # vector of weights for each policy
      # Screening at visit in first trimester (31 branches)
      v_w_test <- c(
        ### Immune
        # Immune, Test Immune (TP IgG, TN IgM)
        p_imm * (p_igg_sens_immune * p_igm_spec_immune +
                   p_igg_sens_immune * (1 - p_igm_spec_immune) * p_pcr_spec) * p_lb,
        p_imm * (p_igg_sens_immune * p_igm_spec_immune +
                   p_igg_sens_immune * (1 - p_igm_spec_immune) * p_pcr_spec) * (1 - p_lb),
        # Immune, Test Infected or Susceptible
        p_imm * (p_igg_sens_immune * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                   (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                   (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * p_pcr_spec +
                   (1 - p_igg_sens_immune) * p_igm_spec_immune) * p_lb,
        p_imm * (p_igg_sens_immune * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                   (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * (1 - p_pcr_spec) +
                   (1 - p_igg_sens_immune) * (1 - p_igm_spec_immune) * p_pcr_spec +
                   (1 - p_igg_sens_immune) * p_igm_spec_immune) * (1 - p_lb),
        
        ### Susceptible
        ## Never Infected
        # Test Immune (FP)
        (1 - p_imm) * (1 - p_inf) * ((1 - p_igg_spec_immune) * p_igm_spec_inf +
                                       (1 - p_igg_spec_immune) * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb,
        (1 - p_imm) * (1 - p_inf) * ((1 - p_igg_spec_immune) * p_igm_spec_inf +
                                       (1 - p_igg_spec_immune) * (1 - p_igm_spec_inf) * p_pcr_spec) * (1 - p_lb),
        # Test Infected (FP)
        (1 - p_imm) * (1 - p_inf) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb,
        (1 - p_imm) * (1 - p_inf) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * (1 - p_lb),
        # Test Susceptible (TN)
        (1 - p_imm) * (1 - p_inf) * ((p_igg_spec_immune * (1 - p_igm_spec_inf) * p_pcr_spec) +
                                       (p_igg_spec_immune * p_igm_spec_inf)) * p_lb,
        (1 - p_imm) * (1 - p_inf) * ((p_igg_spec_immune * (1 - p_igm_spec_inf) * p_pcr_spec) +
                                       (p_igg_spec_immune * p_igm_spec_inf)) * (1 - p_lb),
        
        ## Infected (1-13)
        # Test Immune (FP) (IgG+ and IgM- or IgM+/PCR-)
        (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                          p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb_ei * p_lb,
        (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                          p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * p_lb_ei * (1 - p_lb),
        (1 - p_imm) * (p_inf * 0.65) * (p_igg_sens_immune * p_igm_spec_inf +
                                          p_igg_sens_immune * (1 - p_igm_spec_inf) * p_pcr_spec) * (1 - p_lb_ei),
        # Test Infected (FP)
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb_ei * p_lb,
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * p_lb_ei * (1 - p_lb),
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igm_spec_inf) * (1 - p_pcr_spec)) * (1 - p_lb_ei),
        # Test Susceptible (TN)
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                          (1 - p_igg_sens_immune) * p_igm_spec_inf) * p_lb_ei * p_lb,
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                          (1 - p_igg_sens_immune) * p_igm_spec_inf) * p_lb_ei * (1 - p_lb),
        (1 - p_imm) * (p_inf * 0.65) * ((1 - p_igg_sens_immune) * (1 - p_igm_spec_inf) * p_pcr_spec +
                                          (1 - p_igg_sens_immune) * p_igm_spec_inf) * (1 - p_lb_ei),
        
        ## Infected (14-20)
        # Test Immune or Susceptible
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          p_sfa * p_und_it * p_und_sfa_lb,
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          p_sfa * p_und_it * (1 - p_und_sfa_lb),
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          p_sfa * (1 - p_und_it) * p_und_sfa_lb_nt,
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf))+
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          p_sfa * (1 - p_und_it) * (1 - p_und_sfa_lb_nt),
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          (1 - p_sfa) * p_lb,
        (1 - p_imm) * (p_inf * 0.35) * (((1 - p_igg_spec_inf) * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          ((1 - p_igg_spec_inf) * (1 - p_igm_sens_inf)) +
                                          (p_igg_spec_immune * p_igm_sens_inf * (1 - p_pcr_sens)) +
                                          (p_igg_spec_immune * (1 - p_igm_sens_inf))) *
          (1 - p_sfa) * (1 - p_lb),
        
        # Test Infected
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * p_sfa * p_det_it * p_det_sfa_lb,
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * p_sfa * p_det_it * (1 - p_det_sfa_lb),
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * p_sfa * (1 - p_det_it) * p_und_sfa_lb_nt,
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * p_sfa * (1 - p_det_it) * (1 - p_und_sfa_lb_nt),
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * (1 - p_sfa) * p_lb,
        (1 - p_imm) * (p_inf * 0.35) * (p_igm_sens_inf * p_pcr_sens) * (1 - p_sfa) * (1 - p_lb)
      )
      
      
      # Vaccination (13 branches)
      v_w_vax <- c(
        ## Immune
        (p_imm + (p_vax * p_vax_eff * (1 - p_imm))) * p_lb,
        (p_imm + (p_vax * p_vax_eff * (1 - p_imm))) * (1 - p_lb),
        ## Susceptible, Never Infected
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (1 - p_inf) * (1 - p_lb),
        ## Susceptible, Infected in weeks 1-13
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * p_lb_ei * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * p_lb_ei * (1 - p_lb),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.65) * (1 - p_lb_ei),
        ## Susceptible, Infected in weeks 14-20
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          p_sfa * ((p_det_sq * p_det_it * p_det_sfa_lb) + ((1 - p_det_sq) * p_und_it * p_und_sfa_lb)),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          p_sfa * ((p_det_sq * p_det_it * (1 - p_det_sfa_lb)) + ((1 - p_det_sq) * p_und_it * (1 - p_und_sfa_lb))),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          p_sfa * ((p_det_sq * (1 - p_det_it) * p_det_sfa_lb_nt) + ((1 - p_det_sq) * (1 - p_und_it) * p_und_sfa_lb_nt)),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          p_sfa * ((p_det_sq * (1 - p_det_it) * (1 - p_det_sfa_lb_nt)) + ((1 - p_det_sq) * (1 - p_und_it) * (1 - p_und_sfa_lb_nt))),
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (1 - p_sfa) * p_lb,
        ((p_vax * (1 - p_vax_eff)) + (1 - p_vax)) * (1 - p_imm) * (p_inf * 0.35) *
          (1 - p_sfa) * (1 - p_lb)
      )
      
      
      ### Reward vectors
      ## All Deaths
      # Screening
      v_death_test <- c(rep(c(d_lb, d_fd), 5),
                        rep(c(d_lb, d_fd, d_fd), 3),
                        rep(c(d_lb, d_fd), 6))
      # vaccination
      v_death_vax <- c(rep(c(d_lb, d_fd), 3), d_fd,
                       rep(c(d_lb, d_fd), 3))

      ## Parvo B19 Deaths
      # Screening
      v_b19_death_test <- c(rep(0, 10), rep(c(0, 0, 1), 3), rep(c(0, 1, 0, 1, 0, 0), 2))
      # vaccination
      v_b19_death_vax <- c(rep(0, 6), 1, rep(c(0, 1), 2), 0, 0)
      
      ## Transfusions
      # vector of transfusions - Screening
      v_transfusion_test <- c(rep(n_nt, 19), n_it, n_it, rep(n_nt, 4), n_it, n_it, rep(n_nt, 4))
      # vector of transfusions - vaccination
      v_transfusion_vax <- c(rep(n_nt, 7), n_it, n_it, rep(n_nt, 4))
      
      ## B19 Stillbirths
      # Screening
      v_b19_sb_test     <- c(rep(d_p_lb, 19), rep(c(rep(c(d_p_lb, d_p_fd), 2), d_p_lb, d_p_lb), 2))
      # vaccination
      v_b19_sb_vax      <- c(rep(d_p_lb, 7), rep(c(d_p_lb, d_p_fd), 2), d_p_lb, d_p_lb)
      
      # total deaths per policy
      total_death_test     <- v_w_test %*% v_death_test %*% pop_size
      total_death_vax      <- v_w_vax %*% v_death_vax %*% pop_size

      # total parvo b19 deaths per policy
      total_b19_death_test     <- v_w_test %*% v_b19_death_test %*% pop_size
      total_b19_death_vax      <- v_w_vax %*% v_b19_death_vax %*% pop_size

      # total transfusions per policy
      total_it_test     <- v_w_test %*% v_transfusion_test %*% pop_size
      total_it_vax      <- v_w_vax %*% v_transfusion_vax %*% pop_size

      # total parvo b19 deaths per policy
      total_b19_sb_test     <- v_w_test     %*% v_b19_sb_test %*% pop_size
      total_b19_sb_vax      <- v_w_vax      %*% v_b19_sb_vax %*% pop_size

      
      df_output <- data.frame(Strategy        = v_names_str,
                              Deaths          = c(total_death_test,
                                                  total_death_vax),
                              Transfusions    = c(total_it_test,
                                                  total_it_vax),
                              B19_Deaths      = c(total_b19_death_test,
                                                  total_b19_death_vax),
                              B19_Stillbirths = c(total_b19_sb_test,
                                                  total_b19_sb_vax))
      return(df_output)})
}
