#A function that takes in the results from the model-generating functions and creates a forest plot.
plot_meta <- function(results, width, height, ilab='Lab', save=FALSE, filename='', 
                      summary_only=FALSE, auth_width, title, g1_label, g2_label) {
  meta_theme <- theme_bw() +
    theme(rect = element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.ticks.y = element_blank(), 
          panel.spacing = unit(c(0, 0, 0, 0), "npc"),
          plot.margin = unit(c(5, 0, 5, 0), "pt"),
          axis.line.x=element_line(color='black'),
          axis.line.y=element_blank(),
          axis.text.x=element_text(size=10, color='black'),
          axis.text.y=element_blank(),
          legend.position='none',
          plot.title = element_text(hjust = 0.5))
  
  weights <- (1/sqrt(results$se))/sum(1/sqrt(results$se), na.rm=T)
  psize <- ((weights - min(weights, na.rm=T))/(max(weights, na.rm=T) - min(weights, na.rm=T)))
  results$size <- psize #careful that this weighting for the summary stat is appropriate

  #arrange alphabetically by author
  #results <- arrange(results, desc(class_n), class, group_priority, authors) %>%
  #           mutate(study_n=1:n())
  
  results <- arrange(results, desc(class_n), class, group_priority, desc(means)) %>%
               mutate(study_n=1:n())
  
  results$size[results$shape==17] = .3
  if (17 %in% results$shape) {
    shapes <- c(15, 17, 18)
    shift_by <- setNames(1:length(sort(unique(results$class))), sort(unique(results$class)))
    results <- mutate(results, study_n=study_n+shift_by[class]) %>%
               mutate(study_n=ifelse(group_priority %in% c(2, 3), study_n, study_n - 1))
    
    if (length(unique(results$class)) > 2) {
      results <- mutate(results, study_n=ifelse(shift_by[class] > 1, study_n + 1, study_n))
    }
    
    class_lines <- unlist(group_by(results, class) %>% 
                      filter(shape != 17) %>% 
                      summarize(m=min(study_n)) %>% 
                      select(m) - 1)
    if (length(unique(results$class)) > 2) {
      div_coords <- c(class_lines, 0, class_lines[2] - class_lines[1])
    } else {
      div_coords <- c(class_lines, 0)
    }
  
  } else {
    shapes <- c(15, 18)
    results <- mutate(results, study_n=ifelse(class=='summary', study_n+1, study_n))
    div_coords <- c(min(filter(results, class=='summary')$study_n) - 1, 0)
  }
  offset <- .005
  if (summary_only) {
    results <- filter(results, class=='summary') %>%
               mutate(study_n=1:n())
    div_coords <- 0
    offset <- 0
  }
  
  maxval <- max(abs(na.omit(c(results$ci_u, results$ci_l))))
  breakpoint <- case_when(maxval < 5 ~ 5, 
                          maxval > 5 & maxval < 10 ~ 15,
                          maxval > 10 & maxval < 20 ~ 25)
  
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
  
  text_theme <- theme(panel.grid.major = element_blank(), 
                      legend.position = "none",
                      panel.border = element_blank(),
                      axis.text.y =  element_blank(),
                      axis.ticks =  element_blank(),
                      axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5),
                      axis.line.x=element_line(color='transparent'),
                      axis.text.x=element_text(color='transparent'))
  
  results <- mutate(results, mean_annot=ifelse(!is.na(means), sprintf('% 0.2f', means), NA),
                    ci=ifelse(!is.na(ci_u), sprintf('[% 00.2f, % 00.2f]', ci_l, ci_u), NA),
                    m1_annot=ifelse(!is.na(mean1), sprintf('% 0.2f', mean1), NA),
                    m2_annot=ifelse(!is.na(mean2), sprintf('% 0.2f', mean2), NA))
  
  authors <- ggplot(results) +
    annotate('text', x=.015, y=-1, label=ilab) +
    annotate("text", x=offset, y=filter(results, group_priority!=1)$study_n, 
             label=filter(results, group_priority!=1)$authors, hjust=0) +
    geom_hline(yintercept=div_coords, lty='dashed') +
    scale_y_reverse(limits=c(max(results$study_n) + 1, -1)) +
    xlim(0, .05) +
    xlab(NULL) +
    ylab(NULL) +
    meta_theme +
    text_theme
    
  if (min(results$group_priority) == 1) {
    authors <- authors + 
      annotate("text", x=0, y=filter(results, group_priority==1)$study_n, 
               label=toupper(filter(results, group_priority==1)$authors), 
               hjust=0, fontface='bold')
  }
  
  m1 <- ggplot() +
    annotate('text', x=.015, y=-1, label=g1_label) +
    annotate("text", x=0, y=results$study_n, label=results$m1_annot, hjust=0) +
    geom_hline(yintercept=div_coords, lty='dashed') +
    scale_y_reverse(limits=c(max(results$study_n) + 1, -1)) +
    xlim(0, .05) +
    xlab(NULL) +
    ylab(NULL) +
    meta_theme +
    text_theme
  
  n1 <- ggplot() +
    annotate('text', x=.015, y=-1, label='N') +
    annotate("text", x=0, y=results$study_n, label=results$n1, hjust=0) +
    geom_hline(yintercept=div_coords, lty='dashed') +
    scale_y_reverse(limits=c(max(results$study_n) + 1, -1)) +
    xlim(0, .05) +
    xlab(NULL) +
    ylab(NULL) +
    meta_theme +
    text_theme
  
  m2 <- ggplot() +
    annotate('text', x=.015, y=-1, label=g2_label) +
    annotate("text", x=0, y=results$study_n, label=results$m2_annot, hjust=0) +
    geom_hline(yintercept=div_coords, lty='dashed') +
    scale_y_reverse(limits=c(max(results$study_n) + 1, -1)) +
    xlim(0, .05) +
    xlab(NULL) +
    ylab(NULL) +
    meta_theme +
    text_theme
  
  n2 <- ggplot() +
    annotate('text', x=.015, y=-1, label='N') +
    annotate("text", x=0, y=results$study_n, label=results$n2, hjust=0) +
    geom_hline(yintercept=div_coords, lty='dashed') +
    scale_y_reverse(limits=c(max(results$study_n) + 1, -1)) +
    xlim(0, .05) +
    xlab(NULL) +
    ylab(NULL) +
    meta_theme +
    text_theme
  
  effect <- ggplot() +
    annotate('text', x=.015, y=-1, label='ES') +
    annotate("text", x=0, y=results$study_n, label=results$mean_annot, hjust=0) +
    geom_hline(yintercept=div_coords, lty='dashed') +
    scale_y_reverse(limits=c(max(results$study_n) + 1, -1)) +
    xlim(0, .05) +
    xlab(NULL) +
    ylab(NULL) +
    meta_theme +
    text_theme
  
  ci <- ggplot() +
    annotate('text', x=.02, y=-1, label='95% CI') +
    annotate("text", x=0, y=results$study_n, label=results$ci, hjust=0) +
    geom_hline(yintercept=div_coords, lty='dashed') +
    scale_y_reverse(limits=c(max(results$study_n) + 1, -1)) +
    xlim(0, .05) +
    xlab(NULL) +
    ylab(NULL) +
    meta_theme +
    text_theme
  
  widths <- c(auth_width, 1.1, .5, 1.85, .5, 3.75, .75, 1.25)
  metaplot <- grid.arrange(ggplotGrob(authors), ggplotGrob(m1), ggplotGrob(n1),
               ggplotGrob(m2), ggplotGrob(n2), ggplotGrob(forest), 
               ggplotGrob(effect), ggplotGrob(ci), 
               ncol=8, widths=unit(widths, "in"))
  if (save) {
    mapply(function(f, d) ggsave(plot=metaplot, filename=f, units='in', width=sum(widths), 
                                 height=height, device=d, path='Results/'),
           f=paste0(filename, c('.png', '.pdf')), d=c("png", CairoPDF))
  }
  metaplot
}
