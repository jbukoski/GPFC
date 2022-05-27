#####################
## Helper Function ##
#####################

##-----------------------------------------------------
###########################################
## Build plot function for growth curves ##
###########################################

build_plot <- function(df, subset) {
  
  params <- read_csv("./data/params_m3.csv") %>%
    filter(grp == subset)
  
  cols <- data.frame(grp = c("Boreal needleleaf", "Temperate broadleaf", 
                             "Temperate needleleaf", "Tropical broadleaf", 
                             "Tropical needleleaf", "Acacia", "Cunninghamia", 
                             "Cryptomeria", "Eucalyptus", "Larix", "Picea", 
                             "Pinus", "Populus", "Pseudotsuga"),
                     cols = c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                              "#F0E442", "#999999", "#E69F00", "#56B4E9", 
                              "#009E73", "#F0E442", "#0072B2", "#D55E00", 
                              "#CC79A7", "#000000"),
                     ltys = c(rep(2, 5), rep(1, 9)))
  
  color <- filter(cols, grp == subset)$cols
  ltype <- filter(cols, grp == subset)$ltys
  
  A <- params$a
  A_se <- params$a_se
  k <- params$k
  k_se <- params$k_se
  m <- 3      # Note that setting m = 3 here
  
  ids <- df %>%
    group_by(binomial) %>%
    mutate(n = n()) %>%
    filter(n > 50) %>%
    pull(binomial) %>%
    unique()
  
  cntrys <- n_distinct(pull(df, country))
  sites <- n_distinct(pull(df, site_id))
  obs <- nrow(df)
  sps <- n_distinct(pull(df, binomial))
  
  cntryGrob <- grobTree(textGrob(paste("Countries:", cntrys), x = 0.05,  y = 0.95, hjust = 0, 
                                 gp = gpar(fontsize = 5, col = "black", family = "Arial")))
  siteGrob <- grobTree(textGrob(paste("Sites:", sites), x = 0.05,  y = 0.90, hjust = 0, 
                                gp = gpar(fontsize = 5, col = "black", family = "Arial")))
  obsGrob <- grobTree(textGrob(paste("Observations:", obs), x = 0.05,  y = 0.85, hjust = 0, 
                               gp = gpar(fontsize = 5, col = "black", family = "Arial")))
  spsGrob <- grobTree(textGrob(paste("Species:", sps), x = 0.05,  y = 0.80, hjust = 0, 
                               gp = gpar(fontsize = 5, col = "black", family = "Arial")))
  
  
  p <- df %>%
    ggplot() +
    geom_point(aes(x = age, y = agc), col = "grey", alpha = 0.4, size = 0.75) +
    annotation_custom(cntryGrob) +
    annotation_custom(siteGrob) +
    annotation_custom(obsGrob) +
    annotation_custom(spsGrob) +
    theme_tufte() +
    ylim(c(0, 400)) +
    xlim(c(0, 100)) +
    theme(panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.line = element_line(colour = "black", size = 0.25),
          text = element_text(size = 7, family = "Arial"))
  
  sps_dat <- data.frame()
  
  # Get species level log curve data
  
  for(i in ids) {
    
    d <- filter(df, binomial == i)
    mdl <- lmer(agc ~ log(age) + (1|site_id), data = d)
    coefs <- fixef(mdl)
    
    c <- data.frame(sps = i, x = 1:max(d$age), y = coefs[1] + coefs[2]*log(1:max(d$age)))
    
    sps_dat <- bind_rows(sps_dat, c)
    
  }
  
  # Add species level lines to plot
  
  for(j in unique(sps_dat$sps)) {
    line_dat <- filter(sps_dat, sps == j)
    p <- p + geom_line(data = line_dat, aes(x = x, y = y), col = "dark grey")
  }
  
  # Add nonlinear Chapman-Richards curve on reduced data
  
  nl_dat <- data.frame(x = 1:max(df$age), y = A * (1 - exp(-k*1:max(df$age)))^m)
  nl_low <- data.frame(x = 1:max(df$age), y_low = (A - (A_se * 1.96)) * (1 - exp(-(k - (k_se*1.96))*1:max(df$age)))^m )
  nl_high <- data.frame(x = 1:max(df$age), y_high = (A + (A_se * 1.96)) * (1 - exp(-(k + (k_se*1.96))*1:max(df$age)))^m )
  
  nl_ci <- left_join(nl_high, nl_low)
  
  p <- p + 
    geom_line(data = nl_dat, aes(x = x, y = y), col = color, lty = ltype, size = 1.02) +
    geom_ribbon(data = nl_ci, aes(x = x, ymin = y_low, ymax = y_high), alpha = 0.2, fill = color) +
    ggtitle(subset) +
    theme(plot.title = element_text(size=7, face = "plain"))
  
  return(p)
  
}
