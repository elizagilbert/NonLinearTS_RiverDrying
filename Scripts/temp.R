qs <- c(seq(0.1, 0.9, 0.1), 0.99)

q_list <- lapply(qs, function(q) { 
  # transfer_entropy will give a warning as nboot < 100 
  suppressWarnings({ 
    tefit <- transfer_entropy(final_detrended_data$Extent_detrended, final_detrended_data$Discharge_cfs_detrended, lx = 1, ly = 1, 
                              entropy = "Renyi", q = q, 
                              shuffles = 50, quantiles = c(10, 90), 
                              nboot = 20, quiet = T) 
  })
  data.table( 
    q = q, 
    dir = c("X->Y", "Y->X"), 
    coef(tefit)[, 2:3] 
  )
})
qdt <- rbindlist(q_list)
sh_dt <- data.table( 
  dir = c("X->Y", "Y->X"), 
  ete = c(calc_ete(final_detrended_data$Extent_detrended, final_detrended_data$Discharge_cfs_detrended), 
          calc_ete(final_detrended_data$Extent_detrended, final_detrended_data$Discharge_cfs_detrended))
)

qdt[, pe := qnorm(0.95) * se]
ggplot(qdt, aes(x = q, y = ete)) + 
  geom_hline(yintercept = 0, color = "darkgray") + 
  geom_hline(data = sh_dt, aes(yintercept = ete), linetype = "dashed", 
             color = "red") + 
  geom_point() + 
  geom_errorbar(aes(ymin = ete - pe, ymax = ete + pe), 
                width = 0.25/10, col = "blue") + 
  facet_wrap(~dir) + 
  labs(x = "Values for q", y = "Renyi's Transfer Entropy", 
       title = "Renyi's Transfer Entropy for different Values of q", 
       subtitle = "Extent and Discharge")

  

test <- transfer_entropy(final_detrended_data$Extent_detrended, final_detrended_data$Discharge_cfs_detrended)
