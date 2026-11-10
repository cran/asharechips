## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 6, fig.height = 4)
library(asharechips)

## ----mean---------------------------------------------------------------------
chip_mean_cost(c(10, 11, 12), c(50, 20, 30))

## ----input--------------------------------------------------------------------
daily <- data.frame(
  trade_date = c("20260106", "20260102", "20260105"),
  high = c(12, 10, 11), low = c(12, 10, 11), close = c(12, 10, 11),
  vol = c(100, 100, 100), amount = c(120, 100, 110),
  turnover_rate = c(30, 0, 20)
)
chips <- estimate_chip_distribution(daily)
as.data.frame(chips)
summary(chips)

## ----analytical---------------------------------------------------------------
expected <- c(
  "10 CNY" = exp(-0.5),
  "11 CNY" = (1 - exp(-0.2)) * exp(-0.3),
  "12 CNY" = 1 - exp(-0.3)
)
expected
stopifnot(isTRUE(all.equal(unname(expected), chips$distribution$weight)))

## ----plot---------------------------------------------------------------------
plot(chips, main = "Synthetic daily cost distribution")

## ----linear-------------------------------------------------------------------
linear <- estimate_chip_distribution(daily, turnover_transform = "linear")
as.data.frame(linear)

## ----diagnostics--------------------------------------------------------------
chips$diagnostics[c("trade_date", "replacement_fraction",
                    "applied_replacement_fraction", "initialized_here")]
chips$summary[c("initialization_weight", "history_coverage")]

## ----initial------------------------------------------------------------------
initial <- data.frame(price = c(8, 12), weight = c(0.5, 0.5))
with_initial <- estimate_chip_distribution(daily, initial_distribution = initial)
summary(with_initial)

## ----uniform------------------------------------------------------------------
range_data <- data.frame(
  trade_date = "20260102", high = 12, low = 10, close = 11, turnover_rate = 0
)
uniform <- estimate_chip_distribution(range_data, price_method = "uniform", price_step = 0.25)
plot(uniform, main = "Uniform allocation is a modelling assumption")

## ----prepare------------------------------------------------------------------
raw_daily <- daily[setdiff(names(daily), "turnover_rate")]
raw_daily$ts_code <- "000001.SZ"
basic <- data.frame(ts_code = "000001.SZ", trade_date = daily$trade_date,
                    turnover_rate = daily$turnover_rate)
factors <- data.frame(ts_code = "000001.SZ", trade_date = daily$trade_date,
                      adj_factor = 1)
input <- prepare_tushare_data(raw_daily, basic, factors, ts_code = "000001.SZ")
estimate_chip_distribution(input, adjustment_col = "adj_factor")

## ----chinese-path-------------------------------------------------------------
system.file("README.zh-CN.md", package = "asharechips")

