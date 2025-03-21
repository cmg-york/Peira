## ----bootpackage.ci--------------------------------------------------------------------------------------
bootstrap.ci <- function(x, conf.level, R){
  # x - observed data
  # conf.level - confidence level
  # R number of MC simulations
  
  # calculate mean and variance for the bootstrapped sample
  mean_and_var <- function(d, i){
    # d - observed data
    # i - boot::boot tells us which indices to be used to 
    #     obtain the resampling version of the observed data
    
    d_boot <- d[i]
    c(mean_boot <- mean(d_boot), variance_boot = var(d_boot))
  }
  
  # resampled mean and variance
  b <- boot::boot(x, mean_and_var, R = R)
  
  # type = "stud" stands for studentized statistics
  ret <- boot::boot.ci(b, conf = conf.level, type = "stud", 
                       # var.t0 - variance of the observed data
                       var.t0 = var(x), 
                       # var.t - variances for every 
                       #         resampled data sets
                       var.t = b$t[,2])$student[4:5]
  
  names(ret) <- c("lower", "upper")
  ret
}
