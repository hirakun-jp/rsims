# Japanese tax on realised gains

#' Japanese Tax on Realised Gains
#'
#' @description Tax a Japanese resident pays on a backtest traded in a tokutei
#' (specified) account with tax withheld at source: 20.315% (15.315% national
#' income tax including the reconstruction surtax, and 5% local tax) on the
#' net realised gain of each calendar year.
#'
#' Gains and losses realised in the same account in the same year are netted,
#' and the account settles the withholding by itself, so the tax depends on the
#' year's net, not on each sale. A net loss can be carried forward for three
#' years only by filing a tax return, so `carry_forward_years` defaults to 3 and
#' can be set to 0 for an investor who does not file.
#'
#' Realised gains use the average cost of each position, with commissions
#' added to the cost of buys and taken from the proceeds of sales. Interest,
#' long financing, and short borrow from `min_commission_backtest()` are counted
#' in the year they are paid, as margin interest is part of the settled gain on
#' a margin trade.
#'
#' The function works in the backtest's currency, so the yen gain that is
#' actually taxed, which also moves with the exchange rate, is not modelled.
#' Prices in a backtest are usually adjusted for dividends, so dividends show up
#' as realised gains; they are taxed at the same 20.315%, but the US withholding
#' of 10% on them and its foreign tax credit are not modelled.
#'
#' @param results Output of `min_commission_backtest()`.
#' @param tax_rate Tax rate on the net realised gain of a year.
#' @param carry_forward_years Years a net loss can be carried forward.
#'
#' @return tibble with one row per year: `year`, `realised` (net realised gain
#'   including interest and financing), `loss_used` (carried losses set
#'   against it), and `tax` (a positive amount), and the date the tax is
#'   deducted at, the last date of the year in `results`.
#' @export
#'
#' @references National Tax Agency, No.1476 (tokutei accounts and the rates)
#' and No.1474 (netting and carrying forward losses, which need a tax return),
#' https://www.nta.go.jp/taxes/shiraberu/taxanswer/shotoku/1476.htm and
#' https://www.nta.go.jp/taxes/shiraberu/taxanswer/shotoku/1474.htm (read
#' 2026-11-01).
jp_realised_gains_tax <- function(results, tax_rate = 0.20315, carry_forward_years = 3) {
  trades <- results[results$ticker != "Cash", ]
  trades <- trades[order(trades$ticker, trades$date), ]

  realised <- numeric(nrow(trades))
  for (tk in unique(trades$ticker)) {
    idx <- which(trades$ticker == tk)
    pos <- 0
    avg <- 0
    for (j in idx) {
      q <- trades$share_trades[j]
      if (is.na(q) || q == 0) next
      price <- trades$close[j]
      comm <- trades$commission[j]
      closing <- if (pos != 0 && sign(q) != sign(pos)) min(abs(q), abs(pos)) else 0
      opening <- abs(q) - closing
      comm_close <- comm * closing / abs(q)
      comm_open <- comm - comm_close
      if (closing > 0) {
        realised[j] <- sign(pos) * closing * (price - avg) - comm_close
        pos <- pos + sign(q) * closing
        if (pos == 0) avg <- 0
      }
      if (opening > 0) {
        # commissions raise the cost of a long and lower the proceeds of a short
        new_cost <- opening * price + sign(q) * comm_open
        avg <- (abs(pos) * avg + new_cost) / (abs(pos) + opening)
        pos <- pos + sign(q) * opening
      }
    }
  }

  cost_cols <- intersect(c("interest", "short_borrow", "long_financing"), names(results))
  carrying <- rowSums(as.matrix(results[, cost_cols]), na.rm = TRUE)

  year_of <- function(d) as.integer(format(d, "%Y"))
  years <- sort(unique(year_of(results$date)))
  by_year <- sapply(years, function(y) {
    sum(realised[year_of(trades$date) == y]) + sum(carrying[year_of(results$date) == y])
  })

  losses <- numeric(0)  # carried losses, oldest first, named by year
  out <- data.frame(year = years, realised = by_year, loss_used = 0, tax = 0)
  for (k in seq_along(years)) {
    y <- years[k]
    losses <- losses[as.integer(names(losses)) >= y - carry_forward_years]
    gain <- by_year[k]
    if (gain > 0) {
      for (n in names(losses)) {
        use <- min(gain, losses[[n]])
        losses[[n]] <- losses[[n]] - use
        gain <- gain - use
        out$loss_used[k] <- out$loss_used[k] + use
      }
      out$tax[k] <- tax_rate * gain
    } else if (gain < 0 && carry_forward_years > 0) {
      losses[as.character(y)] <- -gain
    }
  }

  out$date <- as.Date(sapply(years, function(y) max(results$date[year_of(results$date) == y])), origin = "1970-01-01")
  tibble::as_tibble(out)
}
