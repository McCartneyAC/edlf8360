
#MLM Functions

center <- function (df, var, center = 0){
  require(dplyr)
  user_var <- enquo(var)
  varname <- quo_name(user_var)
  center <- enquo(center)
  mutate(df, !!varname := (!!user_var-!!center))
}

standardize <-function(df, var){
  # creates z-score of variable input
  require(dplyr)
  user_var<-enquo(var)
  varname <- quo_name(user_var)
  mutate(df, !!varname := (!!user_var - mean(!!user_var))/sd(!!user_var))
}



plot_margins <- function (lm) {
  require(margins)
  require(ggplot2)
  margins_lm <- margins(lm)
  effects <- summary(margins_lm)
  ggplot(effects) +
    geom_point(aes(factor, AME)) +
    geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
    geom_hline(yintercept = 0) +
    theme(axis.text.x = element_text())
}


regress<- function(df, cluster = NA, ...) {
  require(miceadds)
  require(multiwayvcov)
  if (is.na(cluster)) {
    summary(
      lm(data = df, ...)
    )
  } else {
    summary(
      lm.cluster(data = df, cluster = cluster, ...)
    )
  }
}


icc<-function(model){
  # input must be a null model with lme4::lmer
  # e.g. model2<-lmer(lnwg ~ 1 + (1 |id), data = hours)
  output<-as.data.frame(VarCorr(model))
  rho <- (output$sdcor[1]^2 / ( output$sdcor[1]^2+output$sdcor[2]^2 ))
  return(rho)
}


ess <- function(model, participants, clusters = 0) {
  m <- (participants / cluster)
  k <- clusters
  r <- edlf8360::icc(model)
  ESS <- (m*k)/( 1 + (m-1)*r)
  return(ESS)
}

slopes_CI <- function(estimate, variance) {
  # take the output from a random slopes regression and
  # calculate the 95% confidence interval from the slopes. 
  ub<-estimate + (1.96*sqrt(variance))
  lb<-estimate - (1.96*sqrt(variance))
  print(paste("UB is: ", round(ub,3), ". LB is", round(lb,3), "."))
}

