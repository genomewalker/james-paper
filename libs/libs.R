library(tidyverse)
library(scales)
# Load the datal
load_thedir_data <- function(path, name) {
  read_tsv(path, col_types = cols()) %>%
    mutate(List = name) %>%
    select(List, everything())
}

# Combine data
stats_age_timeline <- function(...){
  x <- list(...)

  dat <- lapply(x, FUN = function(y) {
    select(
      y,
      List, sample_name, geo_loc_name, latitude, longitude, sample_age, publication_year
    ) %>%
      distinct()
  }) %>%
    bind_rows() %>%
    mutate(List = factor(List, levels = names(dir_colours)))

  return(dat)
}

# from https://stackoverflow.com/a/11054781
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv,
            log_breaks(base = base),
            domain = c(1e-100, Inf)
  )
}

# Plot histograms
plot_figure_age_timeline <- function(X, dataset, nclass = "Sturges", log = TRUE){

  X <- X %>% filter(List == dataset) %>% ungroup()

  # We steal the breaks from the base histogram
  if (log){
    hist_data <- hist(log10(X$sample_age), plot = FALSE, breaks = nclass)
    scale_log = scale_x_reverse(breaks = c(2,3,4,5), labels = comma(c(10^2, 10^3, 10^4, 10^5)), limits = c(5, 2))
  }else{
    hist_data <- hist(X$sample_age, plot = FALSE, breaks = nclass)
    scale_log = scale_x_reverse()
  }

  # We calculate where we want to have the columns in the plot
  width <- mean(diff(hist_data$breaks))
  nudge <- -(width/2)
  hist_df <- tibble(counts = hist_data$counts, breaks = (hist_data$breaks[1:length(hist_data$breaks)-1]), List = dataset) %>%
    mutate(width = width, nudge = nudge)

  # Let's plot
  plot <- ggplot(hist_df, aes(breaks, counts, fill = List)) +
    geom_col(color = "black", width = width, position = position_nudge(x = nudge)) +
    #scale_y_log10() +
    scale_log +
    theme_classic(base_size = 8) +
    scale_fill_manual(values = dir_colours, guide = guide_legend(ncol = 1)) +
    ylab("Samples (n)") +
    xlab("Years before present")  +
    theme(legend.position = "none",
          strip.background = element_blank())
  return(list(plot = plot, data = hist_df))
}
