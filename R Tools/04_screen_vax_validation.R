library(pacman)
p_load("devtools", "scales", "ellipse", "lazyeval", "igraph",  "ggraph",
       "reshape2", "knitr", "stringr", "jsonlite", "rstudioapi", "tidyverse",
       "dampack", "data.table", "tornado", "ggplot2", "viridis",
       "kableExtra")


## Load model parameters
source("R/01_params_functions.R")
v_names_str <- c("Status Quo", "Testing", "Vaccination",
                 "Vaccination and Testing")
n_str <- length(v_names_str)

## Load model
source("R/02_phase3_functions.R")

### Validation Tables
# Non-peak year, 2.1% SFA
l_params_all <- load_params()
l_params_all$p_sfa <- 0.021
l_params_all$pop_size <- 65000
v_p_imm <- c(0.7, 0.6, 0.5, 0.4)
v_p_inf <- c(0.005, 0.01, 0.015)

m_b19_deaths_np_2_1 <- matrix(nrow = length(v_p_imm), ncol = length(v_p_inf),
                              dimnames = list(v_p_imm, v_p_inf))
m_trans_np_2_1 <- matrix(nrow = length(v_p_imm), ncol = length(v_p_inf),
                         dimnames = list(v_p_imm, v_p_inf))
for (i in seq_along(v_p_imm)) {
  for (j in seq_along(v_p_inf)) {
    l_params_all$p_imm <- v_p_imm[i]
    l_params_all$p_inf <- v_p_inf[j]
    results <- parvo_model(l_params_all)
    m_b19_deaths_np_2_1[i, j] <- results[1, 4]
    m_trans_np_2_1[i, j]      <- results[1, 3]
  }
}

# Peak year, 2.1% SFA
l_params_all <- load_params()
l_params_all$p_sfa <- 0.021
v_p_imm <- c(0.7, 0.6, 0.5, 0.4)
v_p_inf <- c(0.02, 0.04, 0.06, 0.08)

m_b19_deaths_py_2_1 <- matrix(nrow = length(v_p_imm), ncol = length(v_p_inf),
                              dimnames = list(v_p_imm, v_p_inf))
m_trans_py_2_1 <- matrix(nrow = length(v_p_imm), ncol = length(v_p_inf),
                         dimnames = list(v_p_imm, v_p_inf))
for (i in seq_along(v_p_imm)) {
  for (j in seq_along(v_p_inf)) {
    l_params_all$p_imm <- v_p_imm[i]
    l_params_all$p_inf <- v_p_inf[j]
    results <- parvo_model(l_params_all)
    m_b19_deaths_py_2_1[i, j] <- results[1, 4]
    m_trans_py_2_1[i, j]      <- results[1, 3]
  }
}

# Non-peak year, 7.5% SFA
l_params_all <- load_params()
l_params_all$p_sfa <- 0.075
v_p_imm <- c(0.7, 0.6, 0.5, 0.4)
v_p_inf <- c(0.005, 0.01, 0.015)

m_b19_deaths_np_7_5 <- matrix(nrow = length(v_p_imm), ncol = length(v_p_inf),
                              dimnames = list(v_p_imm, v_p_inf))
m_trans_np_7_5 <- matrix(nrow = length(v_p_imm), ncol = length(v_p_inf),
                         dimnames = list(v_p_imm, v_p_inf))
for (i in seq_along(v_p_imm)) {
  for (j in seq_along(v_p_inf)) {
    l_params_all$p_imm <- v_p_imm[i]
    l_params_all$p_inf <- v_p_inf[j]
    results <- parvo_model(l_params_all)
    m_b19_deaths_np_7_5[i, j] <- results[1, 4]
    m_trans_np_7_5[i, j]      <- results[1, 3]
  }
}

# Peak year, 7.5% SFA
l_params_all <- load_params()
l_params_all$p_sfa <- 0.075
v_p_imm <- c(0.7, 0.6, 0.5, 0.4)
v_p_inf <- c(0.02, 0.04, 0.06, 0.08)

m_b19_deaths_py_7_5 <- matrix(nrow = length(v_p_imm), ncol = length(v_p_inf),
                              dimnames = list(v_p_imm, v_p_inf))
m_trans_py_7_5 <- matrix(nrow = length(v_p_imm), ncol = length(v_p_inf),
                         dimnames = list(v_p_imm, v_p_inf))
for (i in seq_along(v_p_imm)) {
  for (j in seq_along(v_p_inf)) {
    l_params_all$p_imm <- v_p_imm[i]
    l_params_all$p_inf <- v_p_inf[j]
    results <- parvo_model(l_params_all)
    m_b19_deaths_py_7_5[i, j] <- results[1, 4]
    m_trans_py_7_5[i, j]      <- results[1, 3]
  }
}