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

#' Format a number as a percentage
#'
#' This function formats its input as a percentage.
#'
#' @param x A numeric vector
#' @param digits Number of decimal digits
#'
#' @return A character string ending in "%"
#'
#' @author Joshua Ulrich
#' @examples
#'
#' format_pct(0.01)  # 1.0%
#'
format_pct <-
function(x, digits = 1)
{
    fmt <- paste0("%0.", digits, "f")
    paste0(sprintf(fmt, 100 * x), "%")
}
