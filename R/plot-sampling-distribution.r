#' Plot Sampling Distribution
#'
#' @param x Vector of estimates
#' @param truth The underlying parameter 'truth' value
#'
#' @return ggplot object
#'

plot_sampling_distribution <- function(x,truth) {
  d <- density(x)
  p_df <- as_tibble(cbind(x = d$x, density = d$y))
  p_df %>%
    ggplot(aes(x = x, y = density)) + geom_line() +
    hrbrthemes::theme_ipsum() +
    labs(x = "Estimate", y = "Density") +
    geom_vline(aes(xintercept = truth)) +
    annotate("text",x = mean(x), y = min(d$y*1.2), vjust=-1,label  = glue::glue("  \tMean: {formatC(mean(x),digits = 3, format='f')}\n   SD: {formatC(sd(x),digits = 3, format = 'f')}"), hjust = 0)
}
