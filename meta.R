## Example Forest Plot
forest <- ggplot(results, aes(y=study_n, x=means)) +
    geom_segment(x=0, xend=0, y=0, yend=-max(results$study_n) - 4, color='gray', linetype='dotted') +
    geom_point(aes(size=size, shape=as.factor(shape))) +
    geom_errorbarh(aes(xmin=ci_l, xmax=ci_u), height=.1) +
    scale_shape_manual(values=shapes) +
    annotate('text', x=0, y=-1, label=title) +
    geom_hline(yintercept=div_coords, lty='dashed') +
    scale_y_reverse(limits=c(max(results$study_n) + 1, -1)) +
    scale_x_continuous(breaks=seq(-breakpoint, breakpoint, ifelse(breakpoint %% 10 == 0, 10, 5)), 
                       limits=c(-breakpoint-3, breakpoint+3)) +
    xlab(NULL) +
    ylab(NULL) +
    meta_theme
