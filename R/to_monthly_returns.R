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

#' Convert daily returns to monthly
#'
#' This function takes an \pkg{xts} object of daily returns and aggregates
#' them to a monthly series.
#'
#' @param returns An xts object of daily returns.
#'
#' @return A series of monthly returns.
#'
#' @author Joshua Ulrich
#'
to_monthly_returns <-
function(returns) {
    apply.monthly(returns, PerformanceAnalytics::Return.cumulative)
}
