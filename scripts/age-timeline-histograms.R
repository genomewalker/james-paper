library(tidyverse)
library(ggpol)
source("libs/libs.R")

# Load data
raw_hostmetagenome <- load_thedir_data("data/ancientmetagenome-hostassociated.tsv", "Host Associated Metagenome")
raw_hostsinglegenome <- load_thedir_data("data/ancientsinglegenome-hostassociated.tsv", "Host Associated Single Genome")
raw_environmental <- load_thedir_data("data/ancientmetagenome-environmental.tsv", "Environmental Metagenome")

# Combine data
dat_hist <- stats_age_timeline(raw_hostmetagenome,
                               raw_hostsinglegenome,
                               raw_environmental) %>%
  filter(sample_age < 50000)

# Define colors
dir_colours <- c("Host Associated Metagenome" = "#73cff3",
                 "Host Associated Single Genome" = "#d74182",
                 "Environmental Metagenome" = "#2da46a")

# Plot the histograms
nclass <- "FD"
p1 <- plot_figure_age_timeline(dat_hist, dataset = "Host Associated Metagenome", nclass = nclass, log = TRUE)
p2 <- plot_figure_age_timeline(dat_hist, dataset = "Host Associated Single Genome", nclass = nclass, log = TRUE)
p3 <- plot_figure_age_timeline(dat_hist, dataset = "Environmental Metagenome", nclass = nclass, log = TRUE)

# Combine the histograms
ggplot() +
  geom_col(data = p1$data, aes(breaks, counts, fill = List), color = "black", width = unique(p1$data$width), position = position_nudge(x = unique(p1$data$nudge))) +
  geom_col(data = p2$data, aes(breaks, counts, fill = List), color = "black", width = unique(p2$data$width), position = position_nudge(x = unique(p2$data$nudge))) +
  geom_col(data = p3$data, aes(breaks, counts, fill = List), color = "black", width = unique(p3$data$width), position = position_nudge(x = unique(p3$data$nudge))) +
  # scale_x_reverse() +
  scale_y_sqrt() +
  scale_x_reverse(breaks = c(2,3,4,5), labels = scales::comma(c(10^2, 10^3, 10^4, 10^5)), limits = c(5, 2)) +
  theme_classic() +
  scale_fill_manual(values = dir_colours, guide = guide_legend(ncol = 1)) +
  ylab("Samples (n)") +
  xlab("Years before present")  +
  theme(legend.position = "none",
        strip.background = element_blank()) +
  facet_wrap(~factor(List, levels=names(dir_colours)))

# Alternative visualization
dat_hist %>%
  mutate(List = fct_rev(List)) %>%
  ggplot(aes(List, sample_age, fill = List)) +
  ggpol::geom_boxjitter(jitter.shape = 21, jitter.color = "black", jitter.alpha = 0.7, jitter.size = 2,
                        color = "black", alpha = 0.9, errorbar.draw = TRUE, outlier.shape = NA, width = 0.4, errorbar.length = 0.2,
                        jitter.params = list(width = 0.05, height = 1)) +
  scale_y_continuous(trans = reverselog_trans(), breaks = c(10^2,10^3, 10^4, 10^5), labels = comma(c(10^2, 10^3, 10^4, 10^5)), limits = c(10^5, 10^2)) +
  theme_classic() +
  scale_fill_manual(values = dir_colours, guide = guide_legend(ncol = 1)) +
  xlab("") +
  ylab("Years before present")  +
  theme(legend.position = "none",
        strip.background = element_blank()) +
  coord_flip()


