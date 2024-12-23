library(tidyverse)
library(cowplot)

# formula by pascucci et al. 2023
dog = function(x, amplitude = 1, width = 1){
  c = sqrt(2/exp(1)^-.5)
  return(x*amplitude*width*c*exp(1)^-((width*x)^2))
}

psychometric_function <- function(x, slope, x0) {
  1 / (1 + exp(-slope * (x - x0)))
}

theme_crossmod = theme_classic() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=12, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = c(0.95, 0.05),
        plot.subtitle = element_text(size=10),
        axis.title = element_text(size=12),
        axis.text = element_text(size = 10),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        # axis.title.y = element_markdown(),
        legend.justification = c("right", "bottom")) 

# WITHIN-MODALITY TRANSFER
(p_pf1 = ggplot() +
    lims(x = c(-9, 9),
         y = c(0,1)) +
    geom_vline(xintercept = 0, linetype = 2, alpha = .5) +
    geom_hline(yintercept = 0.5, linetype = 2, alpha = .5) +
    geom_function(aes(color = 'Same preceding modality \n Increased post-perceptual processes', linetype = 'Same preceding modality \n Increased post-perceptual processes'),
                  fun = psychometric_function, 
                  args = list(slope = 2.5, x0 = 0), n = 1000, size = 1.5, alpha = .7) +
    geom_function(aes(color = 'Same preceding modality \n Base post-perceptual processes', linetype = 'Same preceding modality \n Base post-perceptual processes'),
                  fun = psychometric_function, 
                  args = list(slope = 1.6, x0 = 0), n = 1000, size = 1.5, alpha = .7) +
    # geom_function(aes(color = 'Different preceding modality \n Increased post-perceptual processes', linetype = 'Different preceding modality \n Increased post-perceptual processes'),
    #               fun = psychometric_function,
    #               args = list(slope = 0.7, x0 = 0), n = 1000, size = 1.5, alpha = .7) +
    geom_function(aes(color = 'Different preceding modality \n Base post-perceptual processes', linetype = 'Different preceding modality \n Base post-perceptual processes'),
                  fun = psychometric_function,
                  args = list(slope = 0.5, x0 = 0), n = 1000, size = 1.5, alpha = .7) +
    labs(x = "", y = "P(B)") +
    scale_color_manual(name = 'Condition', 
                       labels = c('Same preceding modality \n Base post-perceptual processes',
                                  'Same preceding modality \n Increased post-perceptual processes',
                                  'Different preceding modality \n Base post-perceptual processes'),
                       values = c('#3B528BFF', '#5DC863FF', '#5DC863FF')) +
    scale_linetype_manual(name = 'Condition',
                          labels = c('Same preceding modality \n Base post-perceptual processes',
                                     'Same preceding modality \n Increased post-perceptual processes',
                                     'Different preceding modality \n Base post-perceptual processes'),
                              values = c(1,1,6)) +
    scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
    theme_crossmod +
    theme(legend.position = 'none'))

(p_pf2 = ggplot() +
    lims(x = c(-9, 9),
         y = c(0,1)) +
    geom_vline(xintercept = 0, linetype = 2, alpha = .5) +
    geom_hline(yintercept = 0.5, linetype = 2, alpha = .5) +
    geom_function(aes(linetype = 'Increased post-perceptual processes'),
                  fun = psychometric_function, 
                  args = list(slope = 2.5, x0 = 0), n = 1000, size = 1.5, alpha = .7) +
    geom_function(aes(linetype = 'Base post-perceptual processes'),
                  fun = psychometric_function, 
                  args = list(slope = 1.6, x0 = 0), n = 1000, size = 1.5, alpha = .7) +
    geom_function(aes(linetype = 'Baseline'),#, linetype = 'Different preceding modality \n Base post-perceptual processes'),
                  fun = psychometric_function,
                  args = list(slope = 0.5, x0 = 0), n = 1000, size = 1.5, alpha = .3) +
    labs(x = "Stimulus strength", y = "P(B)") +
    scale_linetype_manual(name = 'Condition',
                          labels = c('Base post-perceptual processes',
                                     'Baseline',
                                     'Increased post-perceptual processes'),
                          values = c(1,3,6)) +
    scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
    theme_crossmod +
    theme(legend.position = 'none'))

(p_dog1 = ggplot() +
    lims(x = c(-5, 5),
         y = c(-2,2)) +
    geom_vline(xintercept = 0, linetype = 2, alpha = .5) +
  geom_hline(yintercept = 0, linetype = 2, alpha = .5) +
  geom_function(aes(color = 'Same preceding modality \n Base post-perceptual processes', linetype = 'Same preceding modality \n Base post-perceptual processes'), 
                fun = dog, args = list(amplitude = 1.6, width = 1), n = 1000, size = 1.5, alpha = .7) +
  geom_function(aes(color = 'Same preceding modality \n Increased post-perceptual processes', linetype = 'Same preceding modality \n Increased post-perceptual processes'), 
                fun = dog, args = list(amplitude = 2.2, width = 1), n = 1000, size = 1.5, alpha = .7) +
    geom_function(aes(color = 'Different preceding modality \n Base post-perceptual processes', linetype = 'Different preceding modality \n Base post-perceptual processes'), 
                  fun = dog, args = list(amplitude = 0, width = 1), n = 1000, size = 1.5, alpha = .7) +
    # geom_function(aes(color = 'Different preceding modality \n Increased post-perceptual processes', linetype = 'Different preceding modality \n Increased post-perceptual processes'), 
                  # fun = dog, args = list(amplitude = 0.8, width = 1), n = 1000, size = 1.5, alpha = .7) +
  labs(x = "", y = "Response error on current trial") +
    scale_color_manual(name = 'Condition', 
                       labels = c(
                         'Different preceding modality \n Base post-perceptual processes',
                         'Same preceding modality \n Base post-perceptual processes',
                                  'Same preceding modality \n Increased post-perceptual processes'),
                       values = c('#3B528BFF', '#5DC863FF', '#5DC863FF')) +
    scale_linetype_manual(name = 'Condition',
                          labels = c('Different preceding modality \n Base post-perceptual processes',
                                     'Same preceding modality \n Base post-perceptual processes',
                                     'Same preceding modality \n Increased post-perceptual processes'),
                          values = c(1,1,6)) +
  theme_crossmod)

(p_dog2 = ggplot() +
    lims(x = c(-5, 5),
         y = c(-2,2)) +
    geom_vline(xintercept = 0, linetype = 2, alpha = .5) +
    geom_hline(yintercept = 0, linetype = 2, alpha = .5) +
    geom_function(aes(linetype = 'Base post-perceptual processes'), 
                  fun = dog, args = list(amplitude = 1.6, width = 1), n = 1000, size = 1.5, alpha = .7) +
    geom_function(aes(linetype = 'Increased post-perceptual processes'), 
                  fun = dog, args = list(amplitude = 2.2, width = 1), n = 1000, size = 1.5, alpha = .7) +
    geom_function(aes(linetype = 'Baseline'), 
                  fun = dog, args = list(amplitude = 0.5, width = 1), n = 1000, size = 1.5, alpha = .3) +
    # geom_function(aes(color = 'Different preceding modality \n Increased post-perceptual processes', linetype = 'Different preceding modality \n Increased post-perceptual processes'), 
    #               fun = dog, args = list(amplitude = 0.8, width = 1), n = 1000, size = 1.5, alpha = .7) +
    labs(x = "Peak velocity of previous trial", y = "Response error on current trial") +
    scale_linetype_manual(name = 'Condition',
                          labels = c('Base post-perceptual processes',
                                     'Baseline',
                                     'Increased post-perceptual processes'),
                          values = c(1,3,6)) +
    theme_crossmod)


title_afc = ggdraw() + geom_rect(mapping = aes(xmin=0, xmax=1, ymin=0, ymax=1), fill = 'lightgrey') +
  draw_label("2-AFC",  x = 0.5,  hjust = 0.5,  size = 18)
title_adjust = ggdraw() + geom_rect(mapping = aes(xmin=0, xmax=1, ymin=0, ymax=1), fill = 'lightgrey') +
  draw_label("Adjustment",  x = 0.5,  hjust = 0.5,  size = 18)
title1 = ggdraw() + geom_rect(mapping = aes(xmin=0, xmax=1, ymin=0, ymax=1), fill = 'white') +
  draw_label("Within-modality transfer only",  x = 0.5,  hjust = 0.5,  size = 12, fontface = 'bold')
title2 = ggdraw() + geom_rect(mapping = aes(xmin=0, xmax=1, ymin=0, ymax=1), fill = 'white') +
  draw_label("Between-modality transfer",  x = 0.5,  hjust = 0.5,  size = 12, fontface = 'bold')


plot_grid(
  plot_grid(title_afc, title_adjust),
  title1,
  plot_grid(p_pf1, p_dog1, labels = c('A', 'B')),
  title2,
  plot_grid(p_pf2, p_dog2, labels = c('C', 'D')),
  ncol = 1,
  rel_heights = c(.1, .1, 1, .1, 1)
)

