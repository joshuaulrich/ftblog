# vim: tabstop=4 shiftwidth=4 expandtab
#
#  ftblog: FOSS Trading Blog
#
#  Copyright (C) 2024 Joshua M. Ulrich
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' Estimate equal risk portfolio weights
#'
#' This function uses \code{FRAPO::PERC()} to estimate the Equal Risk
#' Contribution portfolio weights.
#'
#' @param returns An xts object containing returns for two or more assets.
#' @param n_days Number of days to use in the covariance matrix calculation.
#'
#' @return An xts object containing weights for each asset in \code{returns}.
#'
#' @author Joshua Ulrich
#'
portf_wts_equal_risk <-
function(returns, n_days = 60)
{
    if (!requireNamespace("FRAPO", quietly = TRUE)) {
        stop("please install the FRAPO package")
    }
    n_day_returns <- last(returns, n_days)
    sigma <- cov(n_day_returns)

    capture.output({  # this optimization function is chatty
        optim_portf <- FRAPO::PERC(sigma, percentage = FALSE)
    })

    return(FRAPO::Weights(optim_portf))
}

#' Calculate equal risk portfolio returns
#'
#' This function calculates the portfolio weights at the end of each month
#' using estimated portfolio risk from the returns over the last 60 days.
#' Then those weights are used to calculate the portfolio returns for the
#' following month.
#'
#' @param returns An xts object containing returns for two or more assets.
#' @param n_days Number of days of returns to use in the portfolio estimation.
#' @param n_days_vol Number of days to use in the covariance matrix calculation.
#'
#' @return An xts object containing the portfolio return for each day in
#'     \code{returns}.
#'
#' @author Joshua Ulrich
#'
portf_return_equal_risk <-
function(returns,
         n_days = 120,
         n_days_vol = 60)
{
    month_end_i <- endpoints(returns, "months")       # rebalance monthly
    month_end_i <- month_end_i[month_end_i > n_days]  # skip 'n_days'
    weights <- returns * NA                           # pre-allocate

    # calculate portfolio weights using the prior 'n_days'
    for (i in month_end_i) {
        n_day_returns <- returns[(i - n_days):i, ]
        weights[i,] <- portf_wts_equal_risk(n_day_returns, n_days_vol)
    }

    weights <- lag(weights)      # use prior month-end weights
    weights <- na.locf(weights)  # fill weights for all days

    Rp <- xts(rowSums(returns * weights), index(returns))
    colnames(Rp) <- "R_eq_risk"
    return(Rp)
}

#' Calculate momentum-ranked equal risk portfolio returns
#'
#' This function calculates the portfolio returns for a momentum-ranked subset
#' of the assets in \code{returns}. It determines weights at the end of each
#' month using estimated portfolio risk from the returns of the \code{n_assets}
#' with the highest momentum over the last \code{n_days}. Then those weights
#' are used to calculate the portfolio returns for the following month.
#'
#' @param returns An xts object containing returns for two or more assets.
#' @param n_assets Number of highest momentum assets in the portfolio.
#' @param n_days Number of days of returns to use in the portfolio estimation.
#' @param n_days_vol Number of days to use in the covariance matrix calculation.
#' @param use_abs_momo Require all assets in the portfolio to have positive
#'     momentum in the last \code{n_days} (default \code{FALSE}).
#'
#' @return An xts object containing the portfolio return for each day in
#'     \code{returns}.
#'
#' @author Joshua Ulrich
#'
portf_return_momo_equal_risk <-
function(returns,
         n_assets = 5,
         n_days = 120,
         n_days_vol = 60,
         use_abs_momo = FALSE)
{
    month_end_i <- endpoints(returns, "months")       # rebalance monthly
    month_end_i <- month_end_i[month_end_i > n_days]  # skip 'n_days'
    weights <- returns * NA                           # pre-allocate

    # calculate portfolio components and weights using the prior 'n_days'
    for (i in month_end_i) {
        # total returns for each asset class over the prior 'n_days'
        n_day_returns <- returns[(i - n_days):i, ]
        momentum_returns <- apply(1 + n_day_returns, 2, prod) - 1
        weights[i, ] <- 0

        if (isTRUE(use_abs_momo)) {
            # momentum must be positive
            abs_momo_rank <- which(momentum_returns > 0)
            top_cols <- head(abs_momo_rank, n_assets)
        } else {
            # relative momentum
            momentum_rank <- order(momentum_returns, decreasing = TRUE)
            top_cols <- head(momentum_rank, n_assets)
        }

        if (length(top_cols) >= 2) {
            weights[i, top_cols] <-
                portf_wts_equal_risk(n_day_returns[, top_cols], n_days_vol)
        }
    }

    weights <- lag(weights)      # use prior month-end weights
    weights <- na.locf(weights)  # fill weights for all days

    Rp <- xts(rowSums(returns * weights), index(returns))
    colnames(Rp) <- "R_momo_eq_risk"
    return(Rp)
}

#' Calculate momentum-ranked equal weight portfolio returns
#'
#' This function calculates the returns of an equal-weight portfolio of the
#' \code{n_assets} with the highest momentum over the last \code{n_days}.
#' Then those weights are used to calculate the portfolio returns for the
#' following month.
#'
#' @param returns An xts object containing returns for two or more assets.
#' @param n_assets Number of highest momentum assets in the portfolio.
#' @param n_days Number of days of returns to use in the portfolio estimation.
#'
#' @return An xts object containing the portfolio return for each day in
#'     \code{returns}.
#'
#' @author Joshua Ulrich
#'
portf_return_momo <-
function(returns,
         n_assets = 5,
         n_days = 120)
{
    month_end_i <- endpoints(returns, "months")       # rebalance monthly
    month_end_i <- month_end_i[month_end_i > n_days]  # skip 'n_days'
    weights <- returns * NA                           # pre-allocate

    # calculate portfolio components and weights using the prior 'n_days'
    for (i in month_end_i) {
        # total returns for each asset class over the prior 'n_days'
        n_day_returns <- returns[(i - n_days):i, ]
        momentum_returns <- apply(1 + n_day_returns, 2, prod) - 1

        # find 'n_assets' with highest total return over previous 'n_days'
        momentum_rank <- order(momentum_returns, decreasing = TRUE)
        top_n_loc <- head(momentum_rank, n_assets)

        # set all weights to 0, then equal-risk-weight the top 'n_assets'
        weights[i, ] <- 0
        weights[i, top_n_loc] <- 1 / n_assets
    }

    weights <- lag(weights)      # use prior month-end weights
    weights <- na.locf(weights)  # fill weights for all days

    Rp <- xts(rowSums(returns * weights), index(returns))
    colnames(Rp) <- "R_momo"
    return(Rp)
}

#' Estimate global minimum variance portfolio weights
#'
#' This function uses \code{FRAPO::PGMV()} to estimate the Global Minimum
#' Variance portfolio weights.
#'
#' @param returns An xts object containing returns for two or more assets.
#' @param n_days_vol Number of days to use in the covariance matrix calculation.
#'
#' @return An xts object containing weights for each asset in \code{returns}.
#'
#' @author Joshua Ulrich
#'
portf_wts_min_var <-
function(returns,
         n_days_vol = 20)
{
    # the covariance matrix is the correlation using all returns, but
    # the volatility of the past 'n_days_vol'
    vol_returns <- last(returns, n_days_vol)
    sigma <- cov(vol_returns)

    capture.output({  # this optimization function is chatty
        min_var_portf <- FRAPO::PGMV(sigma, percentage = FALSE)
    })
    return(FRAPO::Weights(min_var_portf))

    Ra <- as.matrix(returns)
    max_pct <- rep(1, ncol(Ra))
    min_var_portf <- tseries::portfolio.optim(x = Ra, covmat = sigma, reshigh = max_pct)
    weights <- round(min_var_portf$pw, 7)

    return(weights)
}

#' Calculate momentum-ranked minimum variance portfolio returns
#'
#' This function calculates the returns for the global minimum variance
#' portfolio using the \code{n_assets} with the highest momentum over the last
#' \code{n_days}.
#' Then those weights are used to calculate the portfolio returns for the
#' following month.
#'
#' @param returns An xts object containing returns for two or more assets.
#' @param n_assets Number of highest momentum assets in the portfolio.
#' @param n_days Number of days of returns to use in the portfolio estimation.
#' @param n_days_vol Number of days to use in the covariance matrix calculation.
#' @param use_abs_momo Require all assets in the portfolio to have positive
#'     momentum in the last \code{n_days} (default \code{FALSE}).
#'
#' @return An xts object containing the portfolio return for each day in
#'     \code{returns}.
#'
#' @author Joshua Ulrich
#'
portf_return_momo_min_var <-
function(returns,
         n_assets = 5,
         n_days = 120,
         n_days_vol = 60,
         use_abs_momo = FALSE)
{
    month_end_i <- endpoints(returns, "months")       # rebalance monthly
    month_end_i <- month_end_i[month_end_i > n_days]  # skip 'n_days'
    weights <- returns * NA                           # pre-allocate

    # calculate portfolio components and weights using the prior 'n_days'
    for (i in month_end_i) {
        n_day_returns <- returns[(i - n_days):i, ]
        momentum_returns <- apply(1 + n_day_returns, 2, prod) - 1
        weights[i, ] <- 0

        if (isTRUE(use_abs_momo)) {
            # momentum must be positive
            abs_momo_rank <- which(momentum_returns > 0)
            top_cols <- head(abs_momo_rank, n_assets)
        } else {
            # relative momentum
            momentum_rank <- order(momentum_returns, decreasing = TRUE)
            top_cols <- head(momentum_rank, n_assets)
        }

        if (length(top_cols) >= 2) {
            weights[i, top_cols] <-
                portf_wts_min_var(n_day_returns[, top_cols], n_days_vol)
        }
    }

    weights <- lag(weights)      # use prior month-end weights
    weights <- na.locf(weights)  # fill weights for all days

    Rp <- xts(rowSums(returns * weights), index(returns), weights = weights)
    colnames(Rp) <- "R_momo_min_var"
    return(Rp)
}
