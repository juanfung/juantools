## Utility functions that I use a lot, mainly for writing papers/reports
## Author: Juan F. Fung
## Date: 2020-11-04

## create environment for local variables
juantools.env = new.env()

#' Load packages, installing if not already installed
#'
#' This function takes a list of package names and tries to load them;
#' any packages that are not found are installed
#' 
#' @param ... package names, can be a list, a vector, or a bunch of
#'     strings
#'
#' @importFrom utils install.packages
#'
#' @export
using = function(...) {
    ## Credit: https://stackoverflow.com/a/44660688/5037901
    ## TODO: warnings; more options
    libs = unlist(list(...))
    req = unlist(lapply(libs, require, character.only=TRUE))
    need = libs[req==FALSE]
    if (length(need)>0) { 
        install.packages(need)
        lapply(need, require, character.only=TRUE)
    }
}


#' Convert a string to numeric
#' 
#' Convert any character to numeric, drop commas, dollar signs, etc
#'
#' @param x A string representing a number
#'
#' @return x A numeric representation of x
#'
#' @examples
#' as.num.char('$1,000.00')
#'
#' @export
as.num.char = function(x) {
    x = as.numeric(gsub("(\\$|,)", "", x))
    return(x)
}


#' Convert factor to numeric
#'
#' Convert a factor to string then to numeric, using as.num.char
#'
#' @param x A factor
#'
#' @return x A numeric representation of x
#'
#' @examples
#' as.num.fac(factor('$1,000.00'))
#'
#' @export
as.num.fac = function(x) {
    x = as.num.char(as.character(x))
    return(x)
}

#' Compute mean by ignoring NAs
#'
#' Compute mean, with option na.rm=TRUE by default
#'
#' @param x Numeric vector
#'
#' @return Mean of x values, ignoring NAs
#'
#' @examples
#' x = c(2, 5, NA, 7)
#' mean(x)
#' avg(x)
#'
#' @return
#'
#' @export
avg = function(x) {mean(x, na.rm=TRUE)}


#' Summary statistics for vector with standard deviation
#'
#' Function that appends standard deviation to vector summary
#' statistics, and ignores NAs by default
#'
#' @param x A numeric vector
#' @param ... Other arguments passed to summary
#'
#' @return Summary + standard deviation
#'
#' @examples
#' x = c(2, 5, NA, 7)
#' sd_summary(x)
#'
#' @importFrom stats na.omit sd
#' 
#' @export
sd_summary = function(x, ...) {
    return(c(summary(na.omit(x), ...), sd(x, na.rm=TRUE)))
}


## Format digits display

## i. NIST uses space for thousands separator

#' Format thousands place separator
#'
#' This function changes the printed thousands separator in a number,
#' using space as the default
#'
#' @param x A number
#' @param sep Separator; default is space, `" "`
#'
#' @return A number with correct separator
#'
#' @examples
#' digit_sep(1000)
#' digit_sep(10000)
#'
#' @export
digit_sep = function(x, sep=" ") {
    return( format(x, big.mark=sep, scientific=FALSE) )
}

#' Format thousands place separator for numbers bigger than 9999
#' 
#' Wrapper to `digit_sep` to print thousands separated by space,
#' *except* for numbers in the 1000s
#'
#' @param x A number
#' @param sep Separator; default is space, `" "`
#'
#' @return A number with correct separator
#'
#' @examples
#' thou_sep(1000)
#' thou_sep(10000)
#'
#' @export
thou_sep = function(x, sep=" ") {
    return(ifelse(nchar(x)>4, digit_sep(x, sep), as.character(x)))
}

## ii. NIST uses space before percent sign

#' Convert decimal to percent and add space before percent sign
#'
#' @param x Numeric
#' @param places Integer; how many places to round to?
#' @param sep Logical; add space before percent sign? Default is `TRUE`
#'
#' @return x String reprenting x as percent, with space added before
#'     percent sign
#'
#' @examples
#' percent_sep(0.12345)
#' percent_sep(0.12345, places=3)
#' percent_sep(0.12345, sep=FALSE)
#'
#' @export
percent_sep = function(x, places=2, sep=TRUE) {
    x = as.character(round(x * 100, digits=places))
    x = gsub("^(.*)$", ifelse(sep, "\\1 %", "\\1%"), x)
    return(x)
}

## iii. nice scientific notation for plots
## source:StackOverflow

#' Function to print nice scientific notation in plot legends
#'
#' This function formats a number to an expression in plotmath format
#'
#' @param x Numeric
#'
#' @return Expression for x in plotmath scientific notation
#'
#' @export
fancy_scientific = function(x) {
    # turn in to character string in scientific notation
    x = format(x, scientific = TRUE)
    # quote the part before the exponent to keep all the digits
    x = gsub("^(.*)e", "'\\1'e", x)
    # turn the 'e+' into plotmath format
    x = gsub("e", "%*%10^", x)
    # return this as an expression
    parse(text=x)
}

## iv. convert Sq ft to Sq m
assign('ft2_m2', 0.092903, envir=juantools.env)

#' Convert numeric square feet to square meters
#'
#' This function can convert from square feet to square meters and
#' vice versa
#'
#' @param x Numeric; assumed units are sq ft
#' @param reverse Logical; if `TRUE` then x is converted to sq ft;
#'     default is `FALSE`
#'
#' @return x Numeric, converted to appropriate units
#'
#' @examples
#' sqft2sqm(1000)
#' sqft2sqm(sqft2sqm(1000), reverse=TRUE)
#'
#' @export
sqft2sqm = function(x, reverse=FALSE) {
    if (reverse) {
        x = x / juantools.env$ft2_m2
    } else {
        x = x * juantools.env$ft2_m2
    }
    return(x)
}

#' Convert numeric square meters to square feet
#' 
#' Wrapper to sqft2sqm that converts in the reverse direction
#'
#' @param x Numeric; assumed units are sq m
#'
#' @return x Numeric, converted to sq ft
#'
#' @examples
#' sqm2sqft(100)
#' # identical
#' sqft2sqm(100, reverse=TRUE)
#'
#' @export
sqm2sqft = function(x) {
    return(sqft2sqm(x, reverse=TRUE))
}


## TODO: function to print sq ft (sq m)


## TODO: import package `here` and create .here file
