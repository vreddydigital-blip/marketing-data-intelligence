# Marketing analytics: automatic column detection, KPI computation with safe
# division, and aggregations for campaign/channel/time reporting.

MARKETING_PATTERNS <- list(
  impressions = "impress|impression|views|view",
  clicks      = "click",
  spend       = "spend|cost|budget",
  leads       = "lead",
  conversions = "convers|conversion|orders|order",
  revenue     = "revenue|sales?_amount|income|turnover|amount",
  sessions    = "session",
  users       = "user|visitor",
  engagement  = "engag",
  campaign    = "campaign",
  channel     = "channel",
  source      = "source",
  medium      = "medium",
  date        = "date|day|month|week|time"
)

detect_marketing_columns <- function(df) {
  mapping <- list()
  lower_names <- tolower(names(df))
  for (role in names(MARKETING_PATTERNS)) {
    hits <- names(df)[grepl(MARKETING_PATTERNS[[role]], lower_names)]
    hits <- Filter(function(h) {
      x <- df[[h]]
      if (role %in% c("campaign", "channel", "source", "medium")) {
        is.character(x) || is.factor(x)
      } else if (role == "date") {
        inherits(x, c("Date", "POSIXct", "POSIXt", "POSIXlt"))
      } else {
        is.numeric(x)
      }
    }, hits)
    if (length(hits)) mapping[[role]] <- hits[1]
  }
  mapping
}

KPI_FORMULAS <- c(
  ctr = "CTR = Clicks / Impressions x 100",
  cpc = "CPC = Spend / Clicks",
  cpm = "CPM = Spend / Impressions x 1000",
  conversion_rate = "Conversion Rate = Conversions / Clicks x 100",
  cpl = "CPL = Spend / Leads",
  cpa = "CPA = Spend / Conversions",
  roas = "ROAS = Revenue / Spend",
  revenue_per_lead = "Revenue per Lead = Revenue / Leads"
)

#' Row-level KPIs using safe division. Denominators that are 0 or missing
#' produce NA (rendered as "N/A"), never Inf or misleading values.
compute_row_kpis <- function(df, mapping) {
  out <- df
  get_num <- function(role) {
    if (is.null(mapping[[role]]) || !mapping[[role]] %in% names(df)) return(rep(NA_real_, nrow(df)))
    as.numeric(df[[mapping[[role]]]])
  }
  imp <- get_num("impressions"); clk <- get_num("clicks"); sp <- get_num("spend")
  ld <- get_num("leads"); cv <- get_num("conversions"); rev <- get_num("revenue")
  out$.ctr              <- safe_div(clk, imp) * 100
  out$.cpc              <- safe_div(sp, clk)
  out$.cpm              <- safe_div(sp, imp) * 1000
  out$.conversion_rate  <- safe_div(cv, clk) * 100
  out$.cpl              <- safe_div(sp, ld)
  out$.cpa              <- safe_div(sp, cv)
  out$.roas             <- safe_div(rev, sp)
  out$.revenue_per_lead <- safe_div(rev, ld)
  out
}

kpi_roles_present <- function(mapping) {
  needed <- list(
    ctr = c("clicks", "impressions"), cpc = c("spend", "clicks"),
    cpm = c("spend", "impressions"), conversion_rate = c("conversions", "clicks"),
    cpl = c("spend", "leads"), cpa = c("spend", "conversions"),
    roas = c("revenue", "spend"), revenue_per_lead = c("revenue", "leads")
  )
  vapply(needed, function(req) all(req %in% names(mapping)), logical(1))
}

#' Aggregate KPI totals. Totals are recomputed from summed numerators and
#' denominators so weighted averages are correct (never an unweighted mean of
#' row-level ratios).
kpi_summary <- function(df, mapping) {
  get_num <- function(role, default = 0) {
    if (is.null(mapping[[role]]) || !mapping[[role]] %in% names(df)) return(default)
    s <- sum(as.numeric(df[[mapping[[role]]]]), na.rm = TRUE)
    if (is.na(s)) default else s
  }
  imp <- get_num("impressions"); clk <- get_num("clicks"); sp <- get_num("spend")
  ld <- get_num("leads"); cv <- get_num("conversions"); rev <- get_num("revenue")
  present <- kpi_roles_present(mapping)
  list(
    totals = list(spend = sp, impressions = imp, clicks = clk, leads = ld,
                  conversions = cv, revenue = rev),
    kpis = list(
      ctr = if (present[["ctr"]]) safe_div(clk, imp) * 100 else NA_real_,
      cpc = if (present[["cpc"]]) safe_div(sp, clk) else NA_real_,
      cpm = if (present[["cpm"]]) safe_div(sp, imp) * 1000 else NA_real_,
      conversion_rate = if (present[["conversion_rate"]]) safe_div(cv, clk) * 100 else NA_real_,
      cpl = if (present[["cpl"]]) safe_div(sp, ld) else NA_real_,
      cpa = if (present[["cpa"]]) safe_div(sp, cv) else NA_real_,
      roas = if (present[["roas"]]) safe_div(rev, sp) else NA_real_,
      revenue_per_lead = if (present[["revenue_per_lead"]]) safe_div(rev, ld) else NA_real_
    ),
    available_roles = names(mapping)
  )
}

grouped_performance <- function(df, mapping, group_col) {
  if (is.null(group_col) || !group_col %in% names(df)) return(NULL)
  get_s <- function(role) {
    if (is.null(mapping[[role]]) || !mapping[[role]] %in% names(df)) return(NA_real_)
    role
  }
  agg <- df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      spend = sum(as.numeric(.data[[get_s("spend")]]), na.rm = TRUE),
      impressions = if (!is.na(get_s("impressions"))) sum(as.numeric(.data[[get_s("impressions")]]), na.rm = TRUE) else NA_real_,
      clicks = if (!is.na(get_s("clicks"))) sum(as.numeric(.data[[get_s("clicks")]]), na.rm = TRUE) else NA_real_,
      leads = if (!is.na(get_s("leads"))) sum(as.numeric(.data[[get_s("leads")]]), na.rm = TRUE) else NA_real_,
      conversions = if (!is.na(get_s("conversions"))) sum(as.numeric(.data[[get_s("conversions")]]), na.rm = TRUE) else NA_real_,
      revenue = if (!is.na(get_s("revenue"))) sum(as.numeric(.data[[get_s("revenue")]]), na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
  if (!is.na(get_s("impressions")) && any(!is.na(agg$impressions)))
    agg$ctr <- safe_div(agg$clicks, agg$impressions) * 100
  if (!is.na(get_s("spend")) && any(!is.na(agg$clicks)) && any(!is.na(agg$spend)))
    agg$cpc <- safe_div(agg$spend, agg$clicks)
  if (!is.na(get_s("spend")) && any(!is.na(agg$conversions)))
    agg$cpa <- safe_div(agg$spend, agg$conversions)
  if (!is.na(get_s("spend")) && any(!is.na(agg$spend)) && any(!is.na(agg$revenue)))
    agg$roas <- safe_div(agg$revenue, agg$spend)
  agg
}

daily_series <- function(df, mapping, freq = c("day", "week", "month")) {
  freq <- match.arg(freq)
  date_col <- mapping$date
  if (is.null(date_col) || !date_col %in% names(df)) return(NULL)
  d <- as.Date(df[[date_col]])
  period <- switch(freq,
    day = d,
    month = as.Date(format(d, "%Y-%m-01"))
  )
  if (freq == "week") period <- as.Date(cut(d, "week"))
  get_s <- function(role, default = NA_real_) {
    if (is.null(mapping[[role]]) || !mapping[[role]] %in% names(df)) {
      return(rep(default, length(period)))
    }
    as.numeric(df[[mapping[[role]]]])
  }
  out <- data.frame(
    period = period,
    spend = get_s("spend"), impressions = get_s("impressions"),
    clicks = get_s("clicks"), conversions = get_s("conversions"),
    revenue = get_s("revenue"), leads = get_s("leads"),
    stringsAsFactors = FALSE
  )
  agg <- stats::aggregate(cbind(spend, impressions, clicks, conversions, revenue, leads) ~ period,
                          data = out, FUN = function(z) sum(z, na.rm = TRUE), na.action = stats::na.pass)
  names(agg) <- c("period", "spend", "impressions", "clicks", "conversions", "revenue", "leads")
  agg[order(agg$period), , drop = FALSE]
}

funnel_counts <- function(df, mapping) {
  get_s <- function(role) {
    if (is.null(mapping[[role]]) || !mapping[[role]] %in% names(df)) return(NA_real_)
    sum(as.numeric(df[[mapping[[role]]]]), na.rm = TRUE)
  }
  stages <- data.frame(
    stage = c("Impressions", "Clicks", "Leads", "Conversions", "Revenue"),
    value = c(get_s("impressions"), get_s("clicks"), get_s("leads"), get_s("conversions"), NA_real_)
  )
  stages
}
