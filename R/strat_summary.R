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

#' Calculate Portfolio Summary Statistics
#'
#' This function takes an \pkg{xts} object of returns and calculates
#' some portfolio summary statistics.
#'
#' @param returns An xts object of returns.
#'
#' @return A data.frame of portfolio statistics.
#'
#' @author Joshua Ulrich
#'
portf_summary <-
function(returns)
{
    stats <- PerformanceAnalytics::table.AnnualizedReturns(returns)
    stats <- rbind(stats,
                   "Worst Drawdown" = -maxDrawdown(returns))
    stats <- round(stats, 3)

    return(stats)
}
