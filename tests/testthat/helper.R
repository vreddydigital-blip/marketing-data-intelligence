# Shared fixtures for tests. global.R has been sourced by tests/testthat.R.
# Uses deterministic synthetic data; no network access required.

make_campaign_df <- function(n = 200, seed = 1) {
  set.seed(seed)
  data.frame(
    date = rep(seq(as.Date("2024-01-01"), by = "day", length.out = 20), length.out = n),
    campaign = rep(c("A", "B", "C"), length.out = n),
    channel = rep(c("Search", "Social", "Email"), each = 1, length.out = n),
    impressions = sample(1000:5000, n, replace = TRUE),
    clicks = 0,
    spend = round(runif(n, 50, 500), 2),
    leads = 0,
    conversions = 0,
    revenue = round(runif(n, 100, 2000), 2)
  ) |>
    (\(d) {
      d$clicks <- round(d$impressions * runif(n, .01, .1))
      d$leads <- round(d$clicks * runif(n, .05, .2))
      d$conversions <- round(d$clicks * runif(n, .02, .1))
      d
    })()
}

make_value_df <- function() {
  d <- make_campaign_df(30)
  d$clicks[1:3] <- 0          # zero denominators
  d$impressions[4:5] <- NA    # missing denominators
  d
}
