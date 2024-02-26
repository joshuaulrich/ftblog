#' Adaptive Asset Allocation Returns
#'
#' @docType data 
#' @keywords data 
"aaa_returns"

.aaa_asset_table <- read.csv(text = "ETF,ETF Start,Fund,Fund Start
    Cash,                       SHV, 2007-01,  --- ,   ---
    US Equity,                  VTI, 2001-05, VTSMX, 1992-04
    European Equity,            VGK, 2005-03, VEURX, 1990-06
    Japanese Equity,            EWJ, 1996-03, FJPNX, 1992-12
    Emerging Market Equity,     EEM, 2003-04, VEIEX, 1994-12
    US Real Estate,             ICF, 2001-02, VGSIX, 1996-05
    International Real Estate,  RWX, 2006-12, XRFIX, 1997-09
    Intermediate Term Treasury, IEF, 2002-07, VFITX, 1991-12
    Long Term Treasury,         TLT, 2002-07, VUSTX, 1986-12
    Commodities,                DBC, 2006-02, QRACX, 1997-03
    Gold,                       GLD, 2004-11, SGGDX, 1993-08",
    strip.white = TRUE, check.names = FALSE)

.create_aaa_returns <-
function(asset_table)
{
    getSymbols <- quantmod::getSymbols
    Ad <- quantmod::Ad
    Return.calculate <- PerformanceAnalytics::Return.calculate

    # ETF/Fund pairs
    assets <- data.frame(t(asset_table[-1, c(1, 3)]))

    splice_returns <-
    function(Ra, Rb)
    {
        # fills NA values in Ra with values from Rb
        R <- merge(Ra, Rb, join = "left")
        is_na <- is.na(R[, 1])
        R[is_na, 1] <- R[is_na, 2]

        colnames(R) <- NULL
        return(R[,1])
    }

    import_spliced_returns <-
    function(symbols, from = "1990-01-01")
    {
        symbols <- symbols[1:2]

        data_env <- new.env()
        getSymbols(symbols, from = from, env = data_env, src = "tiingo")

        # combine into xts object
        prices <- do.call(merge, lapply(data_env, Ad))

        # clean up column names
        colnames(prices) <- sub(".Adjusted", "", colnames(prices), fixed = TRUE)

        # order columns
        prices <- prices[, symbols]

        # calculate returns
        returns <- Return.calculate(prices)

        result <- splice_returns(returns[,1], returns[,2])

        return(result)
    }

    return_history <- do.call(merge, lapply(assets, import_spliced_returns))

    shv <- Ad(getSymbols("SHV", from = "2007-01-01", src = "tiingo", auto.assign = FALSE))
    r_shv <- Return.calculate(shv)
    returns <- merge(Cash = drop(r_shv), return_history)

    # Cash
    tbill_rates <- na.locf(getSymbols("DTB3", src = "FRED", auto.assign = FALSE))
    tbill_returns <- (1+tbill_rates/100)^(1/360)-1
    returns[, "Cash"] <- splice_returns(returns[, "Cash"], tbill_returns)

    # Replace missing returns with GSCI index from Yahoo Finance
    gsci <- Ad(getSymbols("^SPGSCI", from = "1990-01-01", src = "yahoo", auto.assign = FALSE))
    asset_col <- "Commodities"
    returns[, asset_col] <- splice_returns(returns[, asset_col], Return.calculate(gsci))

    # Replace missing returns with Russell Global Real Estate Securities Fund
    global_re <- Ad(getSymbols("RRESX", from = "1990-01-01", src = "tiingo", auto.assign = FALSE))
    asset_col <- "International.Real.Estate"
    returns[, asset_col] <- splice_returns(returns[, asset_col], Return.calculate(global_re))

    # Replace missing returns with Willshire Real Estate Securities index from FRED
    # Daily data start in 1996-01-31
    us_re <- getSymbols("WILLRESIND", from = "1996-01-31", src = "FRED", auto.assign = FALSE)
    asset_col <- "US.Real.Estate"
    returns[, asset_col] <- splice_returns(returns[, asset_col], Return.calculate(us_re))

    returns <- na.omit(returns)["/2023"]
}
