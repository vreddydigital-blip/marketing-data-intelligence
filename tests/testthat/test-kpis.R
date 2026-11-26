test_that("CTR/CPC/CPM/Conversion-rate are computed from summed totals (known values)", {
  d <- make_campaign_df(30)
  m <- detect_marketing_columns(d)
  ks <- kpi_summary(d, m)
  t <- ks$totals
  imp <- sum(d$impressions); clk <- sum(d$clicks); sp <- sum(d$spend)
  cv <- sum(d$conversions); rev <- sum(d$revenue)
  expect_equal(t$impressions, imp)
  expect_equal(t$clicks, clk)
  expect_equal(t$spend, sp)
  expect_equal(ks$kpis$ctr, clk / imp * 100)
  expect_equal(ks$kpis$cpc, sp / clk)
  expect_equal(ks$kpis$cpa, sp / cv)
  expect_equal(ks$kpis$roas, rev / sp)
})

test_that("CPL and CPA handle zero/missing denominators without misleading output", {
  d <- make_value_df()
  m <- detect_marketing_columns(d)
  row_k <- compute_row_kpis(d, m)
  expect_true(all(is.na(row_k$.cpc[1:3])))          # clicks = 0
  expect_true(all(is.na(row_k$.ctr[4:5])))          # impressions NA
  expect_false(any(is.infinite(c(row_k$.cpc, row_k$.cpa, row_k$.ctr)), na.rm = TRUE))
})

test_that("KPI availability flags track the mapping", {
  d <- data.frame(a = 1:5)
  m <- detect_marketing_columns(d)
  present <- kpi_roles_present(m)
  expect_false(present[["ctr"]])
  expect_false(present[["roas"]])
})

test_that("grouped_performance returns weighted (not averaged) KPIs", {
  d <- make_campaign_df(40)
  m <- detect_marketing_columns(d)
  gp <- grouped_performance(d, m, "campaign")
  expect_setequal(names(gp), c("campaign", "spend", "impressions", "clicks", "leads",
                               "conversions", "revenue", "ctr", "cpc", "cpa", "roas"))
  a <- gp[gp$campaign == "A", ]
  raw <- d[d$campaign == "A", ]
  expect_equal(a$cpc, sum(raw$spend) / sum(raw$clicks))
})
