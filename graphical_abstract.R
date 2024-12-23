library(tidyverse)
library(cowplot)
library(extrafont)
# loadfonts(device = "all")

# formula by pascucci et al. 2023
dog = function(x, amplitude = 1, width = 1){
  c = sqrt(2/exp(1)^-.5)
  return(x*amplitude*width*c*exp(1)^-((width*x)^2))
}

psychometric_function <- function(x, slope, x0) {
  1 / (1 + exp(-slope * (x - x0)))
}

theme_graphical = theme_classic() +
  theme(panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(size=5),
        legend.title = element_blank(),
        legend.text = element_text(size = 4),
        legend.position.inside = c(0.95, 0.05),
        plot.subtitle = element_text(size=10),
        axis.title = element_text(size=4),
        axis.text = element_text(size = 4),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = 'arial'),
        # axis.title.y = element_markdown(),
        legend.justification = c("right", "bottom")) 

# Psychometric Function
(p_pf = ggplot() +
    lims(x = c(-9, 9), y = c(0,1)) +
    geom_vline(xintercept = 0, linetype = 2, alpha = .5) +
    geom_hline(yintercept = 0.5, linetype = 2, alpha = .5) +
    geom_function(aes(color = 'A'),# = '#5DC863FF',
                  fun = psychometric_function,
                  args = list(slope = 1.6, x0 = 0), n = 1000, size = .8, alpha = .7) +
    geom_function(aes(color = 'B'),# '#3B528BFF',
                  fun = psychometric_function,
                  args = list(slope = 0.5, x0 = 0), n = 1000, size = .8, alpha = .7) +
    labs(x = "Stimulus", y = "P(B)", title = 'Discrimination task') +
    scale_y_continuous(breaks = c(0, 1), labels = c("0", "1")) +
    scale_x_continuous(breaks = c(-5, 0, 5), labels = c("-x", "0", "x"), limits = c(-10,10)) +
    scale_color_manual(values = c('#5DC863FF', '#3B528BFF')) +
    theme_graphical +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'none',
          axis.title.y = element_text(vjust = 2, face = 'bold'),
          axis.text.x = element_text(),
          axis.ticks.x = element_line()))

# DOG Function
(p_dog = ggplot() +
    lims(x = c(-5, 5),
         y = c(-2,2)) +
    geom_vline(xintercept = 0, linetype = 2, alpha = .5) +
    geom_hline(yintercept = 0, linetype = 2, alpha = .5) +
    geom_function(aes(color = 'A'),#color = '#5DC863FF', 
                  fun = dog, args = list(amplitude = .8, width = .8), n = 1000, size = .8, alpha = .7,
                  show.legend = F) +
    geom_function(aes(color = 'B'),#color = '#3B528BFF', 
                  fun = dog, args = list(amplitude = 0, width = 1), n = 1000, size = .8, alpha = .7,
                  show.legend = F) +
    labs(x = "Stimulus", y = 'Adjustment error', title = "Adjustment task") +
    scale_y_continuous(breaks = c(0), labels = c("0"), limits = c(-.8,.8)) +
    scale_x_continuous(breaks = c(-2.5,0,2.5), labels = c("-x","0","x"), limits = c(-5,5)) +
    scale_color_manual(values = c('#5DC863FF', '#3B528BFF'),
                       labels = c('Performance \n(including history effects)', 'Baseline performance \n(excluding history effects)')) +
    theme_graphical +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(),
          axis.title.y = element_text(vjust = 2, face = 'bold'),
          axis.ticks.x = element_line(),
          legend.spacing.y = unit(1.0, 'mm')) +
    guides(color = guide_legend(byrow = F))) 


p_img = ggdraw() + draw_image('history.png')

(p = plot_grid(p_img, 
               plot_grid(p_pf, p_dog),
               ncol = 1,
               align = 'h'))



# ggsave('graphical_abstract.jpg',
#        p,
#        width = 60,
#        height = 50,
#        units = 'mm')

