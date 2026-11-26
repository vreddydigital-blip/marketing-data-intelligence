# Generates synthetic sample datasets. Run from the repo root:
#   Rscript sample_data/generate_sample_data.R
# Output: marketing_campaigns.csv, sales_data.csv, customer_data.csv
# ALL DATA IS SYNTHETIC AND FICTIONAL, created for demonstration and testing.

set.seed(20240317)
out_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- grep("^--file=", args, value = TRUE)
  if (length(f)) dirname(dirname(normalizePath(sub("^--file=", "", f[1])))) else dirname(getwd())
}
root <- out_dir()

fwrite_like <- function(df, name) {
  path <- file.path(root, "sample_data", name)
  readr::write_csv(df, path)
  cat(sprintf("wrote %s (%d rows x %d cols)\n", path, nrow(df), ncol(df)))
}

# ---- Marketing campaigns ----
n_days <- 120
dates <- seq(as.Date("2024-01-01"), by = "day", length.out = n_days)
channels <- c("Paid Search", "Paid Social", "Email", "Display", "Affiliate", "Referral")
campaigns <- sprintf("%s_%s", c("BFSpring", "BFSpring", "BFSpring", "BFSpring", "BFSpring", "BFSpring",
                                "Promo20", "Promo20", "Promo20", "Evergreen", "Evergreen", "Evergreen"),
                     c("Search", "Social", "Email", "Display", "Affiliate", "Referral")) |>
  (\(x) sort(rep(x, length.out = 1200)) )()
n_rows <- 1200
mk <- function(n) rep(dates, length.out = n)
base_spend <- c("Paid Search" = 450, "Paid Social" = 380, "Email" = 80, "Display" = 210,
                "Affiliate" = 150, "Referral" = 90)

mk_df <- data.frame(
  date = mk(n_rows),
  channel = rep(channels, length.out = n_rows),
  campaign = campaigns
)
# seasonal spend with weekly pattern and one tracking incident (weekend spike)
mk_df$spend <- base_spend[mk_df$channel] *
  (1 + 0.25 * sin(2 * pi * as.numeric(mk_df$date) / 365) +
   ifelse(lubridate::wday(mk_df$date, week_start = 1) > 5, 0.15, 0)) *
  exp(rnorm(n_rows, 0, 0.05))
mk_df$impressions <- round(mk_df$spend * rnorm(n_rows, 45, 4))
mk_df$clicks <- pmin(mk_df$impressions, round(mk_df$impressions * runif(n_rows, 0.01, 0.09)))
mk_df$leads <- round(mk_df$clicks * runif(n_rows, 0.02, 0.18))
mk_df$conversions <- round(mk_df$clicks * runif(n_rows, 0.005, 0.12))
mk_df$revenue <- round(mk_df$conversions * runif(n_rows, 30, 220), 2)
# inject 1.5% missing values
for (col in c("impressions", "clicks", "spend", "leads", "conversions", "revenue")) {
  idx <- sample.int(n_rows, round(n_rows * 0.015))
  mk_df[idx, col] <- NA
}
# inject messy categories and whitespace
ws <- sample.int(n_rows, 20)
mk_df$channel[ws] <- paste0(" ", mk_df$channel[ws])
dupe_src <- mk_df[sample.int(n_rows, 25), ]
mk_df <- rbind(mk_df, dupe_src)
fwrite_like(mk_df, "marketing_campaigns.csv")

# ---- Sales ----
n_rows2 <- 1500
products <- c("Portable Battery 20k", "Bluetooth Speaker", "USB-C Hub", "Smart Bulb Kit",
              "Noise-Canceling Headphones", "Fitness Tracker", "Desk Lamp", "Keyboard Mini")
cats <- c(Portable_Battery_20k = "Electronics", Bluetooth_Speaker = "Electronics", `USB-C_Hub` = "Accessories",
          Smart_Bulb_Kit = "Smart Home", Noise_Canceling_Headphones = "Audio", Fitness_Tracker = "Wearables",
          Desk_Lamp = "Home", Keyboard_Mini = "Accessories")
lens <- c(Mobile = 1.0, Web = 0.9, Store = 0.7, Partner = 0.4)
sm <- data.frame(
  order_date = as.Date("2024-01-01") + sample(0:119, n_rows2, replace = TRUE),
  product = sample(products, n_rows2, replace = TRUE, prob = c(.2, .18, .12, .1, .12, .09, .1, .09)),
  region = sample(c("North", "South", "East", "West"), n_rows2, replace = TRUE),
  customer_id = sprintf("C%05d", sample(1:800, n_rows2, replace = TRUE))
)
sm$cost <- round(runif(n_rows2, 12, 90), 2)
sm$quantity <- sample(1:5, n_rows2, replace = TRUE, prob = c(.45, .3, .15, .07, .03))
sm$revenue <- round(sm$cost * runif(n_rows2, 1.15, 2.1) * sm$quantity, 2)
sm$cost <- round(sm$cost * sm$quantity, 2)
fwrite_like(sm, "sales_data.csv")

# ---- Customers ----
n_rows3 <- 600
cx <- data.frame(
  customer_id = sprintf("C%05d", 1:n_rows3),
  acquisition_source = sample(c("Search Ads", "Social", "Referral", "Email", "Organic", "Partners"),
                              n_rows3, replace = TRUE, prob = c(.28, .26, .12, .14, .15, .05)),
  email_opt_in = sample(c(TRUE, FALSE), n_rows3, replace = TRUE),
  loyalty_segment = sample(c("Bronze", "Silver", "Gold", "Platinum"), n_rows3, replace = TRUE, prob = c(.5, .3, .15, .05))
)
cx$last_purchase_date <- as.Date("2024-04-29") - sample(0:300, n_rows3, replace = TRUE)
cx$purchase_frequency <- pmax(1, round(rexp(n_rows3, rate = 1/4)))
spent_base <- c(Bronze = 120, Silver = 340, Gold = 900, Platinum = 2000)
cx$total_spend <- round(spent_base[cx$loyalty_segment] * exp(rnorm(n_rows3, 0, .35)), 2)
# recast as numeric engagement metrics for ML demos
cx$engagement = round(runif(n_rows3, 0, 100), 0)
cx$engagement_score <- cx$engagement
cx$engagement <- NULL
cx$days_since_purchase <- as.numeric(as.Date("2024-04-29") - cx$last_purchase_date)
fwrite_like(cx, "customer_data.csv")
cat("done\n")
