#####################
## Helper Function ##
#####################

##-----------------------------------------------------
############################
## Compare function forms ##
############################

compareCurves <- function(pt_dat) {

    ##----------------------
    ## Fit the different curves
    
    # Log curve, mixed effects
    
    log_mdl <- lme(agc ~ log(age), random = ~1|site_id, data = pt_dat)
    log_coefs <- log_mdl$coefficients$fixed
    
    log_dat <- data.frame(x = 1:100, y = log_coefs[1] + log_coefs[2]*log(1:100))
    
    # Yield curve, q(t) = exp(a - b/t)
    
    lgstc_mdl <- lme(agc_ln ~ age_inv, random = ~1|site_id, data = pt_dat)
    lgstc_coefs <- lgstc_mdl$coefficients$fixed
    
    lgstc_dat <- data.frame(x = 1:100, y = exp(lgstc_coefs[1] + lgstc_coefs[2]/1:100))
    
    # Nonlinear curve, q(t) = a / (1 + b * exp(-k*t))
    
    nl_growth_mdl <- function(psi, id, x){
      
      t <- x[, 1]
      a <- psi[id, 1]
      b <- psi[id, 2]
      k <- psi[id, 3]
      fpred <- a / (1 + b * (exp(-k*t)))
      return(fpred)
      
    }
    
    saemix_dat <- saemixData(name.data = pt_dat, name.group = "site_id", name.predictors = "age", name.response = "agc")
    saemix_model <- saemixModel(model = nl_growth_mdl, psi0 = c(a = 82, b = 28, k = 0.15))
    saemix_options <- list(map = TRUE, fim = TRUE, ll.is = FALSE, seed = 19890402, displayProgress = FALSE, print = FALSE)
    
    nl_mdl <- saemix(saemix_model, saemix_dat, saemix_options)
    nl_coefs <- coefficients(nl_mdl)$fixed
    
    nl_dat <- data.frame(x = 1:100, y = nl_coefs[1] / (1 + nl_coefs[2] * (exp(-nl_coefs[3]*1:100))))
    
    ## Chapman-Richards curve, q(t) = A * (1 - b * exp(-k*t)) ^ (1/ (1-m))
    
    cr_curve <- function(psi, id, x){
      
      t <- x[, 1]
      A <- psi[id, 1]
      k <- psi[id, 2]
      fpred <- A * (1 - exp(-k*t))^3
      return(fpred)
      
    }
    
    saemix_dat <- saemixData(name.data = pt_dat, name.group = "site_id", name.predictors = "age", name.response = "agc")
    saemix_model <- saemixModel(model = cr_curve, psi0 = c(A = 100, k = 0.01))
    saemix_options <- list(map = TRUE, fim = TRUE, ll.is = FALSE, seed = 19890402, displayProgress = FALSE, print = FALSE)
    
    cr_mdl <- saemix(saemix_model, saemix_dat, saemix_options)
    cr_coefs <- coefficients(cr_mdl)$fixed
    
    cr_dat <- data.frame(x = 1:100, y = cr_coefs[1] * (1 - exp(-cr_coefs[2] * 1:100))^3)

    fits <- log_dat %>%
      left_join(lgstc_dat, by = c("x")) %>%
      left_join(nl_dat, by = c("x")) %>%
      left_join(cr_dat, by = c("x")) %>%
      rename(y_log = y.x, y_lgstc = y.y, y_nl = y.x.x, y_cr = y.y.y)
    
    return(fits)
        
}

