# Function to plot the results of parameter recovery as a function of the number of trials for R and MATLAB

## INPUTS
# * Excel sheets with the results of parameter recovery as a function of number of trials
#
# N-by-2 dataframe for Pexp, Mconf, mu_m and rho for R and MATLAB containing the results of parameter recovery.
# # The first column is the number of trials and the second column is the correlation.

## OUTPUTS
#
# * gtable
#
# A png with a gtable containing four plots, one for each parameter, comparing the results of parameter recovery
# as a function of the number of trials for R and MATLAB


install.packages("readxl")
install.packages("ggplot")
install.packages("gridExtra")
library(readxl)
library(ggplot2)
library(gridExtra)

# plotting function
create_plot <- function(data_R, data_M, title) {
  plot <- ggplot() +
    geom_line(data = data_M, aes(x = `Trial number`, y = `Correlation`, color = 'MATLAB'), size = 0.9) +
    geom_point(data = data_M, aes(x = `Trial number`, y = `Correlation`, color = 'MATLAB'), size = 1.7) +
    geom_line(data = data_R, aes(x = `Trial number`, y = `Correlation`, color = 'R'), size = 0.9) +
    geom_point(data = data_R, aes(x = `Trial number`, y = `Correlation`, color = 'R'), size = 1.7) +
    scale_x_continuous(name = 'Trial number', breaks = seq(10, 100, by = 10), limits = c(10, 100), expand = c(0, 1)) +
    scale_y_continuous(name = 'Correlation', breaks = c(0.5, 0.75, 0.9, 1), limits = c(0, 1), expand = c(0, 0)) +
    geom_hline(yintercept = c(0.75, 0.9), linetype = 'dashed', size = 0.7) +
    ggtitle(title) +
    annotate('text', x = 5, y = 1.1, label = 'a)', size = 4, hjust = 0, vjust = 1, color = 'black') +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = 'black', size = 0.7),
          plot.title = element_text(size = 20, hjust = 0.5),
          legend.position = c(0.8, 0.25), # Anpassung der Legendenposition auf oben rechts
          legend.key.size = unit(2.5, 'lines'),
          legend.text = element_text(size = 11)) +
    scale_color_manual(name = NULL, values = c('MATLAB' = 'red', 'R' = 'blue')) +
    guides(color = guide_legend())
  return(plot)
}

# read data
data_Pexp_R <- read_excel('../BIM_Parameter_Recovery/R_correlation_data/Pexp_Parameter_Recovery.xlsx')
data_Mconf_R <- read_excel('../BIM_Parameter_Recovery/R_correlation_data/Mconf_Parameter_Recovery.xlsx')
data_mu_m_R <- read_excel('../BIM_Parameter_Recovery/R_correlation_data/mu_m_Parameter_Recovery.xlsx')
data_rho_R <- read_excel('../BIM_Parameter_Recovery/R_correlation_data/rho_Parameter_Recovery.xlsx')

data_Pexp_M <- read_excel('../BIM_Parameter_Recovery/MATLAB_correlation_data/Pexp_Parameter_Recovery_M.xlsx')
data_Mconf_M <- read_excel('../BIM_Parameter_Recovery/MATLAB_correlation_data/Mconf_Parameter_Recovery_M.xlsx')
data_mu_m_M <- read_excel('../BIM_Parameter_Recovery/MATLAB_correlation_data/mu_m_Parameter_Recovery_M.xlsx')
data_rho_M <- read_excel('../BIM_Parameter_Recovery/MATLAB_correlation_data/rho_Parameter_Recovery_M.xlsx')

# call plotting function
plot_Pexp <- create_plot(data_R = data_Pexp_R, data_M = data_Pexp_M, title = expression(P[exp]))
plot_Mconf <- create_plot(data_R = data_Mconf_R, data_M = data_Mconf_M, title = expression(M[conf]))
plot_mu_m <- create_plot(data_R = data_mu_m_R, data_M = data_mu_m_M, title = bquote(mu[m]))
plot_rho <- create_plot(data_R = data_rho_R, data_M = data_rho_M, title = bquote(rho))

# list the plots
plots <- list(plot_Pexp, plot_Mconf, plot_mu_m, plot_rho)

# arrange plots in gtable
gtable <- grid.arrange(grobs = plots, ncol = 2, nrow = 2, padding = unit(5, 'in'))

# save plots
ggsave(
  file = 'Param_Rec_R_MATLAB.png',
  plot = gtable,
  width = 13,
  height = 8,
  units = 'in',
  dpi = 300
)
