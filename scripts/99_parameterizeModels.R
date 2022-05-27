#####################
## Helper Function ##
#####################

##-----------------------------------------------------
#######################################################
## Build wrapper function for model parameterization ##
#######################################################

mdlParameterization_wrapper <- function(mdl_dat, m, grps) {

    # Specify Chapman-Richards curve function
  
    cr_curve <- function(psi, id, x){
      
      t <- x[, 1]
      A <- psi[id, 1]
      k <- psi[id, 2]
      fpred <- A * (1 - exp(-k*t))^m
      return(fpred)
      
    }
    
    # Helper function to fit the Chapman Richards curve  
    
    fit_cr <- function(df, i) {
      
      # Use Fekedulegn et al., 1999 formulas to find starting values
      
      a_init <- max(df$agc)
      k_init <- ((max(df$agc) - min(df$agc)) / (max(df$age) - min(df$age))) / max(df$age)
      
      smx_df <<- df
      
      saemix_dat <- saemixData(name.data = smx_df, name.group = "site_id", name.predictors = "age", name.response = "agc")
      saemix_model <- saemixModel(model = cr_curve, psi0 = c(A = a_init, k = k_init))
      saemix_options <- list(map = TRUE, fim = TRUE, ll.is = FALSE, displayProgress = FALSE, print = FALSE)
      
      cr_mdl <- saemix(saemix_model, saemix_dat, saemix_options)
      
      cr_res <- data.frame(a = cr_mdl@results@fixed.effects[1],
                           k = cr_mdl@results@fixed.effects[2],
                           a_se = cr_mdl@results@se.fixed[1],
                           k_se = cr_mdl@results@se.fixed[2])
      
      return(cr_res)
      
    }
    
    results_df <- data.frame()
    
    set.seed(19890402)
    seeds <- round(runif(25, 1, 1000))
    
    for(j in 1:25) {
      
      seed <- seeds[j]
      
      for(i in grps) {
        
        set.seed(seed)
        
        sub_dat <- if(i %in% unique(mdl_dat$type)) {
          filter(mdl_dat, type == i)
        } else {
          filter(mdl_dat, genus == i)
        }
        
        sites <- unique(sub_dat$site_id)
        sites_n <- round(length(sites) *0.15)
        
        val_sites <- sample(sites, sites_n)
        val_df <- filter(sub_dat, site_id %in% val_sites)
        trn_df <- filter(sub_dat, !(site_id %in% val_sites))
        
        agc_avg <- mean(sub_dat$agc)
        agc_sd <- sd(sub_dat$agc)
        
        tryCatch({res <- fit_cr(trn_df, i)}, 
                 error = function(e) {res <<- rep(NA, 4)})
        
        if(!is.na(res[1])) {
          
          h <- dplyr::select(val_df, age, agc_obs = agc) %>%
            mutate(agc_prd = res$a * (1-exp(-res$k*age)^m))
          
          rmse_val <- rmse(h$agc_obs, h$agc_prd)
          
          df_line <- data.frame(group = i, seed = seed, a = res$a, 
                                a_se = res$a_se, k = res$k, k_se = res$k_se, 
                                rmse = rmse_val,
                                nrmse_avg = rmse_val / agc_avg,
                                nrmse_sd = rmse_val / agc_sd)
          
        } else {
          
          df_line <- data.frame(group = i, seed = seed, a = NA, a_se = NA,
                                k = NA, k_se = NA, rmse = NA, nrmse_avg = NA, 
                                nrmse_sd = NA)
          
        }
        
        results_df <- bind_rows(results_df, df_line)
        
      }
      
    }
    
    # Summarize RMSE values
    
    rmses <- results_df %>%
      filter(!is.na(a)) %>%
      group_by(group) %>%
      summarize(n = n(),
                rmse = mean(rmse),
                nrmse_avg_sd = sd(nrmse_avg),
                nrmse_avg = mean(nrmse_avg),
                nrmse_sd_sd = sd(nrmse_sd),
                nrmse_sd_avg = mean(nrmse_sd)) %>%
      dplyr::select(group, n, rmse, nrmse_avg) %>%
      arrange(nrmse_avg)
    
    #------------------------------------------------
    # Estimate parameters using the full dataset
    
    full_params <- data.frame()
    
    for(i in grps) {
      
      print(i)
      
      sub_dat <- if(i %in% unique(mdl_dat$type)) {
        filter(mdl_dat, type == i)
      } else {
        filter(mdl_dat, genus == i)
      }
      
      tryCatch({res <- fit_cr(sub_dat, i = NA)}, 
               error = function(e) {res <<- rep(NA, 4)})
      
      res2 <- data.frame(grp = i, a = res[1], a_se = res[2], 
                         k = res[3], k_se = res[4])
      
      full_params <- bind_rows(full_params, res2)
      
    }
    
    df_full <- full_params %>%
      mutate(m = m) %>%
      dplyr::select(grp, a, a_se, k, k_se, m) %>%
      left_join(rmses, by = c("grp" = "group")) %>%
      mutate(across(2:9, round, 4))
    
    return(df_full)
    
  }
