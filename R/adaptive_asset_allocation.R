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

#' Estimate portfolio weights
#'
#' These functions estimate weights for various portfolio specifications.
#'
#' \describe{
#'   \item{`portf_wts_efficient`}{estimates the mean-variance efficient
#'     portfolio weights using [tseries::portfolio.optim()].
#'   }
#'   \item{`portf_wts_min_var`}{estimates the Global Minimum Variance portfolio
#'     weights using [FRAPO::PGMV()].
#'   }
#'   \item{`portf_wts_equal_risk`}{estimates the Equal Risk Contribution
#'     portfolio weights using [FRAPO::PERC()].
#'   }
#' }
#'
#' See the documentation for each portfolio weight estimation function for
#' details on how they work.
#'
#' @param returns An xts object containing returns for two or more assets.
#' @param n_days_vol Number of days to use in the covariance matrix calculation.
#'
#' @return An xts object containing weights for each asset in `returns`.
#'
#' @author Joshua Ulrich
#'
#' @rdname portfolio_weights
#'
portf_wts_efficient <-
function(returns,
         n_days_vol = 20)
{
    if (!requireNamespace("tseries", quietly = TRUE)) {
        stop("please install the tseries package")
    }
    # the covariance matrix is the correlation using all returns, but
    # the volatility of the past 'n_days_vol'
    vol_returns <- last(returns, n_days_vol)
    sigma <- cov(vol_returns)

    Ra <- as.matrix(returns)
    min_var_portf <- tseries::portfolio.optim(x = Ra, covmat = sigma)
    weights <- round(min_var_portf$pw, 7)

    return(weights)
}

#' @rdname portfolio_weights
portf_wts_min_var <-
function(returns,
         n_days_vol = 20)
{
    if (!requireNamespace("FRAPO", quietly = TRUE)) {
        stop("please install the FRAPO package")
    }
    vol_returns <- last(returns, n_days_vol)
    sigma <- cov(vol_returns)

    capture.output({  # this optimization function is chatty
        min_var_portf <- FRAPO::PGMV(sigma, percentage = FALSE)
    })

    return(FRAPO::Weights(min_var_portf))
}

#' @rdname portfolio_weights
portf_wts_equal_risk <-
function(returns,
         n_days_vol = 60)
{
    if (!requireNamespace("FRAPO", quietly = TRUE)) {
        stop("please install the FRAPO package")
    }
    n_day_returns <- last(returns, n_days_vol)
    sigma <- cov(n_day_returns)

    capture.output({  # this optimization function is chatty
        optim_portf <- FRAPO::PERC(sigma, percentage = FALSE)
    })

    return(FRAPO::Weights(optim_portf))
}

#' Calculate portfolio returns
#'
#' These functions calculate returns for various portfolio specifications. They
#' calculate portfolio weights at the end of each month and apply those weights
#' to the following month.
#'
#' These function use `n_days` of returns to estimate the asset weights,
#' and `n_days_vol` to calculate the covariance matrix. Functions with a
#' `n_assets` argument will only include up to that number of assets in
#' the portfolio.
#'
#' \describe{
#'   \item{`portf_return_equal_risk`}{uses [FRAPO::PERC()] to calculate
#'     the Equal Risk Contribution portfolio return using all assets.
#'   }
#'   \item{`portf_return_momo`}{calculates the equal weight portfolio of
#'     the `n_assets` with the highest momentum over the past `n_days`.
#'   }
#'   \item{`portf_return_momo_equal_risk`}{calculates the Equal Risk
#'     Contribution portfolio return using only the `n_assets` with the
#'     highest momentum over the last `n_days`.
#'   }
#'   \item{`portf_return_momo_efficient`}{uses [tseries::portfolio.optim()]
#'     to calculate the mean-variance efficient portfolio return using the
#'     `n_assets` with the highest momentum over the last `n_days`.
#'   }
#'   \item{`portf_return_momo_min_var`}{uses [FRAPO::PGMV()] to calculate
#'     to calculate the Global Minimum Variance portfolio return using the
#'     `n_assets` with the highest momentum over the last `n_days`.
#'   }
#' }
#'
#' The `momo_type` argument can be one of the following values:
#' \describe{
#'   \item{relative}{use the top `n_assets` with the highest momentum.
#'   }
#'   \item{positive}{use the top `n_assets` with the highest _positive_
#'     momentum. Assets with negative momentum are excluded.
#'   }
#'   \item{above average}{use the top `n_assets` with the highest above-average
#'     momentum. Assets with below average momentum are excluded.
#'   }
#' }
#'
#' @param returns An xts object containing returns for two or more assets.
#' @param n_assets Number of highest momentum assets in the portfolio.
#' @param n_days Number of days of returns to use in the portfolio estimation.
#' @param n_days_vol Number of days to use in the covariance matrix calculation.
#' @param momo_type Type of momentum to use. See Details.
#'
#' @return An xts object containing the portfolio return for each day in
#'     `returns`.
#'
#' @author Joshua Ulrich
#'
#' @rdname portfolio_returns
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

#' @rdname portfolio_returns
portf_return_momo_equal_risk <-
function(returns,
         n_assets = 5,
         n_days = 120,
         n_days_vol = 60,
         momo_type = c("relative", "positive", "above average"))
{
    month_end_i <- endpoints(returns, "months")       # rebalance monthly
    month_end_i <- month_end_i[month_end_i > n_days]  # skip 'n_days'
    weights <- returns * NA                           # pre-allocate
    momo_type <- match.arg(momo_type)

    # calculate portfolio components and weights using the prior 'n_days'
    for (i in month_end_i) {
        # total returns for each asset class over the prior 'n_days'
        n_day_returns <- returns[(i - n_days):i, ]
        momentum_returns <- apply(1 + n_day_returns, 2, prod) - 1
        weights[i, ] <- 0

        top_cols <- .find_top_momo_columns(momentum_returns, n_assets, momo_type)

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

#' @rdname portfolio_returns
portf_return_momo <-
function(returns,
         n_assets = 5,
         n_days = 120,
         momo_type = c("relative", "positive", "above average"))
{
    month_end_i <- endpoints(returns, "months")       # rebalance monthly
    month_end_i <- month_end_i[month_end_i > n_days]  # skip 'n_days'
    weights <- returns * NA                           # pre-allocate
    momo_type <- match.arg(momo_type)

    # calculate portfolio components and weights using the prior 'n_days'
    for (i in month_end_i) {
        # total returns for each asset class over the prior 'n_days'
        n_day_returns <- returns[(i - n_days):i, ]
        momentum_returns <- apply(1 + n_day_returns, 2, prod) - 1

        top_cols <- .find_top_momo_columns(momentum_returns, n_assets, momo_type)

        # set all weights to 0, then equal-risk-weight the top 'n_assets'
        weights[i, ] <- 0
        weights[i, top_cols] <- 1 / n_assets
    }

    weights <- lag(weights)      # use prior month-end weights
    weights <- na.locf(weights)  # fill weights for all days

    Rp <- xts(rowSums(returns * weights), index(returns))
    colnames(Rp) <- "R_momo"
    return(Rp)
}

#' @rdname portfolio_returns
portf_return_momo_min_var <-
function(returns,
         n_assets = 5,
         n_days = 120,
         n_days_vol = 60,
         momo_type = c("relative", "positive", "above average"))
{
    month_end_i <- endpoints(returns, "months")       # rebalance monthly
    month_end_i <- month_end_i[month_end_i > n_days]  # skip 'n_days'
    weights <- returns * NA                           # pre-allocate
    momo_type <- match.arg(momo_type)

    # calculate portfolio components and weights using the prior 'n_days'
    for (i in month_end_i) {
        n_day_returns <- returns[(i - n_days):i, ]
        momentum_returns <- apply(1 + n_day_returns, 2, prod) - 1
        weights[i, ] <- 0

        top_cols <- .find_top_momo_columns(momentum_returns, n_assets, momo_type)

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

#' @rdname portfolio_returns
portf_return_momo_efficient <-
function(returns,
         n_assets = 5,
         n_days = 120,
         n_days_vol = 60,
         momo_type = c("relative", "positive", "above average"))
{
    month_end_i <- endpoints(returns, "months")       # rebalance monthly
    month_end_i <- month_end_i[month_end_i > n_days]  # skip 'n_days'
    weights <- returns * NA                           # pre-allocate
    momo_type <- match.arg(momo_type)

    # calculate portfolio components and weights using the prior 'n_days'
    for (i in month_end_i) {
        n_day_returns <- returns[(i - n_days):i, ]
        momentum_returns <- apply(1 + n_day_returns, 2, prod) - 1
        weights[i, ] <- 0

        top_cols <- .find_top_momo_columns(momentum_returns, n_assets, momo_type)

        if (length(top_cols) >= 2) {
            weights[i, top_cols] <-
                portf_wts_efficient(n_day_returns[, top_cols], n_days_vol)
        }
    }

    weights <- lag(weights)      # use prior month-end weights
    weights <- na.locf(weights)  # fill weights for all days

    Rp <- xts(rowSums(returns * weights), index(returns), weights = weights)
    colnames(Rp) <- "R_momo_efficient"
    return(Rp)
}

.find_top_momo_columns <-
function(returns,
         n_assets = 5,
         type = c("relative", "positive", "above average"))
{
    type <- match.arg(type)

    include_cols <- switch(type,
        "relative"      = rep(TRUE, length(returns)),
        "positive"      = returns > 0,
        "above average" = returns > mean(returns, na.rm = TRUE))

    which_cols <- which(include_cols)

    if (length(which_cols) > 0) {
        # at least 1 column meets the 'type' criteria
        # rank returns from highest to lowest
        momo_rank <- order(returns, decreasing = TRUE)
        # which columns have the highest rank and meet the 'type' criteria?
        top_cols <- momo_rank[momo_rank %in% which_cols]
        # keep the top 'n_assets'
        top_cols <- head(top_cols, n_assets)
    } else {
        top_cols <- integer()
    }

    return(top_cols)
}
