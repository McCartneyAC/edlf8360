
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


regress <- function(df, ...) {
  summary(
    lm(data = df, ...)
  )
}
