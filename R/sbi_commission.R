# SBI Securities commission model

#' SBI Securities US Stock Commission
#'
#' @description Commission for US stocks and ETFs traded through an SBI Securities
#' internet-course account: a percent of each order's value, capped per order.
#' The defaults are SBI's published rates for US cash trades, 0.45% of the
#' order's value (0.495% with consumption tax), at least 0 and at most 20 US
#' dollars (22 with tax) per order, as listed on
#' https://search.sbisec.co.jp/v2/popwin/attention/trading/stock_gai_23.html
#' (read 2026-10-31). SBI rounds commissions in a way that page does not state,
#' so no rounding is applied. Currency conversion costs and the SEC fee on sales
#' are not included.
#'
#' Buys of the ETFs in SBI's commission-free program ("SBI ETF Selection") cost
#' nothing; pass their tickers as `free_buy_tickers`. The list changes over
#' time (VT, VTI, VOO, QQQ, SPYD, VGT, EPI, AGG, GLDM, and IYR on 2023-07-14),
#' so it has no default.
#'
#' @param shares_traded vector of shares traded per asset (negative for sells)
#' @param prices vector of prices used to value the trades, named by ticker
#'   (or name `shares_traded`)
#' @param unadjprices vector of unadjusted prices (unused; kept for the
#'   `commission_fun` signature of `min_commission_backtest()`)
#' @param pct_per_order commission as a fraction of each order's value
#' @param max_dollars_per_order the most commission charged on one order
#' @param free_buy_tickers tickers whose buys are commission-free
#'
#' @return vector of commissions
#' @export
#'
#' @examples
#' sbi_us_commission(c("VTI" = 10, "TLT" = -20), c(300, 90), c(300, 90))
#' sbi_us_commission(c("VTI" = 10, "TLT" = -20), c(300, 90), c(300, 90), free_buy_tickers = "VTI")
sbi_us_commission <- function(shares_traded, prices, unadjprices, pct_per_order = 0.00495, max_dollars_per_order = 22, free_buy_tickers = character(0)) {
  commissions <- pmin(abs(shares_traded * prices) * pct_per_order, max_dollars_per_order)
  # min_commission_backtest() passes trades without names, so the tickers come from the prices
  tickers <- if (is.null(names(shares_traded))) names(prices) else names(shares_traded)
  free <- tickers %in% free_buy_tickers & shares_traded > 0
  commissions[which(free | shares_traded == 0 | is.na(commissions))] <- 0

  commissions
}

#' SBI Securities US Margin Commission
#'
#' @description Commission for US stocks and ETFs traded on margin through an
#' SBI Securities internet-course account: 0.3% of each order's value (0.33%
#' with consumption tax), at least 0 and at most 15 US dollars (16.5 with tax)
#' per order, for opening and closing trades alike, as listed on
#' https://search.sbisec.co.jp/v2/popwin/attention/trading/stock_gai_23.html
#' (read 2026-11-01). The cap was checked against SBI's trade reports for TLT in
#' September 2026: two opening orders of about 9,960 dollars paid 16.50 each,
#' and one closing order of 246 shares (19,211 dollars) paid 16.50 in total,
#' split between the two positions it closed. No SEC fee was charged on that
#' sale, so none is included.
#'
#' Margin interest (4.5% a year on long positions) is not a commission; pass it
#' to `min_commission_backtest()` as `long_financing_costs`.
#'
#' @inheritParams sbi_us_commission
#'
#' @return vector of commissions
#' @export
#'
#' @examples
#' sbi_us_margin_commission(c("TLT" = 123, "VTI" = -10), c(80.93, 300), c(80.93, 300))
sbi_us_margin_commission <- function(shares_traded, prices, unadjprices, pct_per_order = 0.0033, max_dollars_per_order = 16.5) {
  sbi_us_commission(shares_traded, prices, unadjprices, pct_per_order = pct_per_order, max_dollars_per_order = max_dollars_per_order)
}
