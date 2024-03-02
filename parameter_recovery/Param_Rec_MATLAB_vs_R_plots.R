# Laden der Bibliotheken
library(readxl)
library(ggplot2)
library(gridExtra)

# Funktion zur Erstellung eines Plots
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

# Einlesen der zusätzlichen Excel-Dateien
daten_Pexp_R <- read_excel('./parameter_recovery/R_correlation_data/Pexp_Parameter_Recovery.xlsx')
daten_Mconf_R <- read_excel('./parameter_recovery/R_correlation_data/Mconf_Parameter_Recovery.xlsx')
daten_mu_m_R <- read_excel('./parameter_recovery/R_correlation_data/mu_m_Parameter_Recovery.xlsx')
daten_rho_R <- read_excel('./parameter_recovery/R_correlation_data/rho_Parameter_Recovery.xlsx')

daten_Pexp_M <- read_excel('./parameter_recovery/MATLAB_correlation_data/Pexp_Parameter_Recovery_M.xlsx')
daten_Mconf_M <- read_excel('./parameter_recovery/MATLAB_correlation_data/Mconf_Parameter_Recovery_M.xlsx')
daten_mu_m_M <- read_excel('./parameter_recovery/MATLAB_correlation_data/mu_m_Parameter_Recovery_M.xlsx')
daten_rho_M <- read_excel('./parameter_recovery/MATLAB_correlation_data/rho_Parameter_Recovery_M.xlsx')

# Erstellung der Plots
plot_Pexp <- create_plot(data_R = daten_Pexp_R, data_M = daten_Pexp_M, title = expression(P[exp]))
plot_Mconf <- create_plot(data_R = daten_Mconf_R, data_M = daten_Mconf_M, title = expression(M[conf]))
plot_mu_m <- create_plot(data_R = daten_mu_m_R, data_M = daten_mu_m_M, title = bquote(mu[m]))
plot_rho <- create_plot(data_R = daten_rho_R, data_M = daten_rho_M, title = bquote(rho))

# Zusammenfügen der Plots
plots <- list(plot_Pexp, plot_Mconf, plot_mu_m, plot_rho)

# Anordnen der Plots
gtable <- grid.arrange(grobs = plots, ncol = 2, nrow = 2, padding = unit(5, 'in'))

# Speichern der Plots
ggsave(
  file = 'Results_ParamRec.png',
  plot = gtable,
  width = 13,
  height = 8,
  units = 'in',
  dpi = 300
)
