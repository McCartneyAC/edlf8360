
#MLM Functions

center <- function (df, var, center = 0){
  require(dplyr)
  user_var <- enquo(var)
  varname <- quo_name(user_var)
  center <- enquo(center)
  mutate(df, !!varname := (!!user_var-!!center))
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
  r <- ess(model)
  ESS <- (m*k)/( 1 + (m-1)*r)
  return(ESS)
}
