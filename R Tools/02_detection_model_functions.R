# Model Function
phase2_model <- function(l_params_all) {
  with(
    as.list(l_params_all),
    {
      # # probabilities of infection 
      # # probability of infection within 1-13 WG
      # p_einf = ((1/14)*(27-sqrt(729-560*(p_inf))))
      # # probability of infection within 14-20
      # p_linf = (7/20)*((1/14)*(27-sqrt(729-560*(p_inf))))
      
      # vector of weights for each policy
      # status quo vector
      v_w_sq <- c((1-p_imm)*(p_inf*0.35)*p_det_sq*p_sfa*p_det_it*p_det_sfa_lb,
                  (1-p_imm)*(p_inf*0.35)*p_det_sq*p_sfa*p_det_it*(1-p_det_sfa_lb),
                  (1-p_imm)*(p_inf*0.35)*p_det_sq*p_sfa*(1-p_det_it)*p_det_sfa_lb_nt,
                  (1-p_imm)*(p_inf*0.35)*p_det_sq*p_sfa*(1-p_det_it)*(1-p_det_sfa_lb_nt),
                  (1-p_imm)*(p_inf*0.35)*p_det_sq*(1-p_sfa)*p_lb,
                  (1-p_imm)*(p_inf*0.35)*p_det_sq*(1-p_sfa)*(1-p_lb),
                  (1-p_imm)*(p_inf*0.35)*(1-p_det_sq)*p_sfa*p_und_it*p_und_sfa_lb,
                  (1-p_imm)*(p_inf*0.35)*(1-p_det_sq)*p_sfa*p_und_it*(1-p_und_sfa_lb),
                  (1-p_imm)*(p_inf*0.35)*(1-p_det_sq)*p_sfa*(1-p_und_it)*p_und_sfa_lb_nt,
                  (1-p_imm)*(p_inf*0.35)*(1-p_det_sq)*p_sfa*(1-p_und_it)*(1-p_und_sfa_lb_nt),
                  (1-p_imm)*(p_inf*0.35)*(1-p_det_sq)*(1-p_sfa)*p_lb,
                  (1-p_imm)*(p_inf*0.35)*(1-p_det_sq)*(1-p_sfa)*(1-p_lb),
                  (1-p_imm)*(p_inf*0.65)*(1-p_lb_ei),
                  (1-p_imm)*(p_inf*0.65)*(p_lb_ei),
                  (1-p_imm)*(1-p_inf)*p_lb,
                  (1-p_imm)*(1-p_inf)*(1-p_lb),
                  p_imm*p_lb,
                  p_imm*(1-p_lb))
      
      # surveillance vector
      v_w_surv <- c((1-p_imm)*(p_inf*0.35)*p_det_surv*p_sfa*p_det_it*p_det_sfa_lb,
                    (1-p_imm)*(p_inf*0.35)*p_det_surv*p_sfa*p_det_it*(1-p_det_sfa_lb),
                    (1-p_imm)*(p_inf*0.35)*p_det_surv*p_sfa*(1-p_det_it)*p_det_sfa_lb_nt,
                    (1-p_imm)*(p_inf*0.35)*p_det_surv*p_sfa*(1-p_det_it)*(1-p_det_sfa_lb_nt),
                    (1-p_imm)*(p_inf*0.35)*p_det_surv*(1-p_sfa)*p_lb,
                    (1-p_imm)*(p_inf*0.35)*p_det_surv*(1-p_sfa)*(1-p_lb),
                    (1-p_imm)*(p_inf*0.35)*(1-p_det_surv)*p_sfa*p_und_it*p_und_sfa_lb,
                    (1-p_imm)*(p_inf*0.35)*(1-p_det_surv)*p_sfa*p_und_it*(1-p_und_sfa_lb),
                    (1-p_imm)*(p_inf*0.35)*(1-p_det_surv)*p_sfa*(1-p_und_it)*p_und_sfa_lb_nt,
                    (1-p_imm)*(p_inf*0.35)*(1-p_det_surv)*p_sfa*(1-p_und_it)*(1-p_und_sfa_lb_nt),
                    (1-p_imm)*(p_inf*0.35)*(1-p_det_surv)*(1-p_sfa)*p_lb,
                    (1-p_imm)*(p_inf*0.35)*(1-p_det_surv)*(1-p_sfa)*(1-p_lb),
                    (1-p_imm)*(p_inf*0.65)*(1-p_lb_ei),
                    (1-p_imm)*(p_inf*0.65)*(p_lb_ei),
                    (1-p_imm)*(1-p_inf)*p_lb,
                    (1-p_imm)*(1-p_inf)*(1-p_lb),
                    p_imm*p_lb,
                    p_imm*(1-p_lb))
      
      # vector of costs for status quo
      # v_cost_sq <-c(c_lb + c_sfa + c_mid + c_mon + c_sq,
      #               c_fd + c_sfa + c_mid + c_mon + c_sq,
      #               c_lb + c_sfa + c_mid + c_mon + c_sq,
      #               c_fd + c_sfa + c_mid + c_mon + c_sq,
      #               c_lb + c_mid + c_mon + c_sq,
      #               c_fd + c_mid + c_mon + c_sq,
      #               c_lb + c_sfa + c_mid + c_sq,
      #               c_fd + c_sfa + c_mid + c_sq,
      #               c_lb + c_sfa + c_mid + c_sq,
      #               c_fd + c_sfa + c_mid + c_sq,
      #               c_lb + c_sq,
      #               c_fd + c_sq,
      #               c_lb + c_sq,
      #               c_fd + c_sq,
      #               c_lb + c_sq,
      #               c_fd + c_sq)
      
      # vector of costs for surveillance
      # v_cost_surv <-c(c_lb + c_sfa + c_mid + c_mon + c_surv,
      #                 c_fd + c_sfa + c_mid + c_mon + c_surv,
      #                 c_lb + c_sfa + c_mid + c_mon + c_surv,
      #                 c_fd + c_sfa + c_mid + c_mon + c_surv,
      #                 c_lb + c_mid + c_mon + c_surv,
      #                 c_fd + c_mid + c_mon + c_surv,
      #                 c_lb + c_sfa + c_mid + c_surv,
      #                 c_fd + c_sfa + c_mid + c_surv,
      #                 c_lb + c_sfa + c_mid + c_surv,
      #                 c_fd + c_sfa + c_mid + c_surv,
      #                 c_lb + c_surv,
      #                 c_fd + c_surv,
      #                 c_lb + c_surv,
      #                 c_fd + c_surv,
      #                 c_lb + c_surv,
      #                 c_fd + c_surv)
      # 
      # vector of total deaths
      v_death <- c(d_lb,
                   d_fd,
                   d_lb,
                   d_fd,
                   d_lb,
                   d_fd,
                   d_lb,
                   d_fd,
                   d_lb,
                   d_fd,
                   d_lb,
                   d_fd,
                   d_lb,
                   d_fd,
                   d_lb,
                   d_fd,
                   d_lb,
                   d_fd)
      
      # vector of parvovirus stillbirths
      v_pvb_deaths <- c(d_p_lb,
                        d_p_fd,
                        d_p_lb,
                        d_p_fd,
                        d_p_lb,
                        d_p_lb,
                        d_p_lb,
                        d_p_fd,
                        d_p_lb,
                        d_p_fd,
                        d_p_lb,
                        d_p_lb,
                        d_p_lb,
                        d_p_lb,
                        d_p_lb,
                        d_p_lb,
                        d_p_lb,
                        d_p_lb)
      
      
      # vector of transfusions
      v_transfusion <- c(n_it,
                         n_it,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_it,
                         n_it,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_nt,
                         n_nt)
      
      # total costs per policy
      #total_cost_sq  <- v_w_sq  %*%  v_cost_sq    
      #total_cost_surv <- v_w_surv    %*%  v_cost_surv
      # total deaths per policy
      total_death_sq <- v_w_sq %*% v_death %*% pop_size
      total_death_surv <- v_w_surv %*% v_death %*% pop_size
      pvb_death_sq <- v_w_sq %*% v_pvb_deaths %*% pop_size
      pvb_death_surv <- v_w_surv %*% v_pvb_deaths %*% pop_size
      # total transfusions per policy
      total_it_sq <- v_w_sq %*% v_transfusion %*% pop_size
      total_it_surv <- v_w_surv %*% v_transfusion %*% pop_size
      
      
      #print(sum(v_w_sq))
      #print(sum(v_w_surv))
      
      df_output <- data.frame(Strategy =  v_names_str,
                              Deaths = c(total_death_sq, total_death_surv),
                              B19_Deaths = c(pvb_death_sq, pvb_death_surv),
                              Transfusions = c(total_it_sq, total_it_surv))
      return(df_output)})
}
