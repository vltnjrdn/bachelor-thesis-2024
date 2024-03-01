# R script for calling the BIM simulation function for Pexp and Mconf.
# default for ntrial is 50'000
# choose target directory in line 32

# install.packages("rlang")
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("extrafont")
# install.packages("gridExtra")
# install.packages("gtable")
# install.packages("ggdist")
# install.packages("GoFKernel")

library(rlang)
library(MASS)
library(ggplot2)
library(gridExtra)
library(gtable)
library(ggdist)
library(GoFKernel)

# set parameters for simulation
# ntrial should not be lower than 50
ntrial <- 50000

Pexp <- seq(0.3, 0.7, 0.1)
Mconf <- seq(0.3, 0.7, 0.1)
rho <- 0
mu_m <- 0

# choose directory
wd <- 'C:/Users/Valentina/OneDrive - Universitaet Bern/Bachelor/simBIM'
setwd(wd)

# generate target directory for the 25 plots and the gtable(matrix with all 25 plots)
folder_name <- sprintf('Simulation_Pexp_Mconf_%.d__%s',
                       ntrial,
                       format(Sys.time(), '%Y.%m.%d_%H.%M.%S'))
dir.create(folder_name, showWarnings = FALSE)
setwd(folder_name)

# Generate all combinations of parameters
params <- expand.grid(
  Mconf = Mconf,
  Pexp = Pexp,
  mu_m = mu_m,
  rho = rho
)

#Generate a list for the 25 plots
plots <- list()

# Simulate data and draw figures
for (i in 1:nrow(params)) {
  # Simulate data
  observed_data <- BIM_simulation(params$Pexp[i],
                   params$Mconf[i],
                   params$mu_m[i],
                   params$rho[i],
                   ntrial)

  # Extract recall values for recalled and unrecalled trials
  conf_recalled <- observed_data[observed_data[, 2] == 1, 1] / 100
  conf_unrecalled <- observed_data[observed_data[, 2] == 0, 1] / 100

  # adjust extreme values
  conf_recalled[conf_recalled == 1] <- 0.9999
  conf_recalled[conf_recalled == 0] <- 0.0001
  conf_unrecalled[conf_unrecalled == 1] <- 0.9999
  conf_unrecalled[conf_unrecalled == 0] <- 0.0001

  # compute silverman's rule of thumb for bandwidth selection
  silverman_bw <- function(m) {
    n <- length(m)
    h_silverman <- 1.4826 * median(abs(m - median(m))) * (4 / (3 * n)) ^ (1 / 5)

    return(h_silverman)
  }

  # kernel density estimation
  density_recalled <- density_bounded(
    conf_recalled,
    bounds = c(0, 1),
    bandwidth = silverman_bw(conf_recalled),
    n = 1000,
  )
  density_unrecalled <- density_bounded(
    conf_recalled,
    bounds = c(0, 1),
    bandwidth = silverman_bw(conf_unrecalled),
    n = 1000,
  )

  # scale density values
  density_recalled$y <- density_recalled$y *
                        length(conf_recalled) /
                        (length(conf_recalled) + length(conf_unrecalled))

  density_unrecalled$y <- density_unrecalled$y *
                          length(conf_unrecalled) /
                          (length(conf_recalled) + length(conf_unrecalled))

  # plotting
  plot_data <- data.frame(
    x = density_recalled$x,
    y_recalled = density_recalled$y,
    y_unrecalled = density_unrecalled$y,
    group = rep(c('Recalled', 'Unrecalled'), each = 1000)
  )

  # generate the single plots
  plot_single <- ggplot(data = plot_data, mapping = aes(x = x)) +
    geom_line(aes(y = y_recalled, colour = 'Recalled')) +
    geom_line(aes(y = y_unrecalled, colour = 'Unrecalled')) +
    xlab('confidence') +
    ylab('prob. density') +
    ggtitle(bquote(paste(
      P[exp], " = ", .(params$Pexp[i]), ", ", M[conf], " = ", .(params$Mconf[i])
    ))) +
    ylim(0, 2.5) +
    xlim(0, 1) +
    scale_colour_manual(
      name = NULL,
      breaks = c('Recalled', 'Unrecalled'),
      labels = c('Recalled', 'Unrecalled'),
      values = c('Recalled' = 'blue', 'Unrecalled' = 'red')
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      legend.key = element_blank(),
      legend.position = c(0.76, 0.8),
      legend.direction = 'vertical',
      plot.title = element_text(size = 15, hjust = 0.5),
      panel.border = element_rect(
        color = 'black',
        fill = NA,
        linewidth = 1
      )
    )

  # save each plot in the target directory
  ggsave(
    paste0('fig_pexp_', params$Pexp[i], '_mconf_', params$Mconf[i], '.png'),
    plot_single,
    width = 6,
    height = 4,
    units = 'in',
    dpi = 300
  )

  # generate plots for the gtable
  plot_gtable <- plot_single +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = 'none'
    )

  # add each plot to the list
  plots[[i]] <- plot_gtable
}

#generate gtable (all 25 plots)
gtable <- grid.arrange(grobs = plots, ncol = 5, nrow = 5)


#save gtable (all 25 plots) in the target directory
ggsave(
  paste0('Pexp_Mconf_all.png'),
  gtable,
  width = 11,
  height = 9,
  units = 'in',
  dpi = 300
)

#reset working directory
setwd(wd)
