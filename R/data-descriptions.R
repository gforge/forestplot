#' HRQoL regression coefficients (Sweden and Denmark)
#'
#' A list of regression coefficients with 95\% confidence intervals for
#' health-related quality of life (HRQoL) 1 year after total hip arthroplasty.
#'
#' @description
#' The object contains two elements, \code{"Sweden"} and \code{"Denmark"}.
#' Each element is a matrix with one row per contrast and the columns
#' \code{coef}, \code{lower}, and \code{upper}.
#'
#' Age is modelled using a spline in the underlying regression model and is
#' therefore presented as a contrast (e.g. "85 vs 65 years") rather than a
#' single linear effect.
#'
#' @format
#' A named list with two matrices (\code{Sweden}, \code{Denmark}).
#' Each matrix has 4 rows and 3 columns:
#' \itemize{
#'   \item \code{coef}: Estimated regression coefficient (beta).
#'   \item \code{lower}: Lower 95% confidence bound.
#'   \item \code{upper}: Upper 95% confidence bound.
#' }
#'
#' @source
#' Gordon M, Paulsen A, Overgaard S, Garellick G, Pedersen AB, Rolfson O.
#' Factors influencing health-related quality of life after total hip replacement
#' - a comparison of data from the Swedish and Danish hip arthroplasty registers.
#' BMC Musculoskelet Disord. 2013;14:316. doi:10.1186/1471-2474-14-316. PubMed PMID: 24192304.
#'
#' @examples
#' data(HRQoL)
#' names(HRQoL)
#' HRQoL$Sweden
#' HRQoL$Denmark
"HRQoL"


#' HRQoL regression coefficients in long format
#'
#' A long-format data frame version of \code{\link{HRQoL}} with one row per
#' contrast and country, intended for plotting (e.g. forest plots).
#'
#' @description
#' The data frame includes a textual label for each contrast and columns for
#' the estimate and its 95\% confidence interval. The \code{group} column can
#' be used for faceting or subgroup styling.
#'
#' @format
#' A data frame with 8 rows and 5 variables:
#' \itemize{
#'   \item \code{labeltext}: Contrast label (character).
#'   \item \code{mean}: Estimated regression coefficient (beta) (numeric).
#'   \item \code{lower}: Lower 95% confidence bound (numeric).
#'   \item \code{upper}: Upper 95% confidence bound (numeric).
#'   \item \code{group}: Country/group label (character).
#' }
#'
#' @source
#' Gordon M, Paulsen A, Overgaard S, Garellick G, Pedersen AB, Rolfson O.
#' Factors influencing health-related quality of life after total hip replacement
#' - a comparison of data from the Swedish and Danish hip arthroplasty registers.
#' BMC Musculoskelet Disord. 2013;14:316. doi:10.1186/1471-2474-14-316. PubMed PMID: 24192304.
#'
#' @examples
#' data(dfHRQoL)
#' dfHRQoL
#'
#' # Example: construct label + CI text for a table column
#' with(dfHRQoL, paste0(sprintf("%.3f", mean), " [", sprintf("%.3f", lower), ", ", sprintf("%.3f", upper), "]"))
"dfHRQoL"

#' Example dataset: Inventors vs Melodifestival Winners
#'
#' A synthetic example dataset for demonstrating subgroup forest plots.
#'
#' @description
#' The dataset mimics a meta-analysis table with study-level binary outcomes,
#' inverse-variance weights, formatted odds ratios with 95\% confidence
#' intervals, and subgroup summary rows.
#'
#' @format
#' A data frame with 23 rows and 19 variables:
#' \itemize{
#'   \item \code{author}: Study label (includes headers, studies, subtotals, total).
#'   \item \code{ai}: Events in treatment group.
#'   \item \code{n1i}: Total in treatment group.
#'   \item \code{ci}: Events in control group.
#'   \item \code{n2i}: Total in control group.
#'   \item \code{weights}: Inverse-variance weights in percent.
#'   \item \code{orci}: Formatted odds ratio with 95\% CI, or "Not Estimated".
#'   \item \code{rb.a}--\code{rb.f}: Risk-of-bias domains ("+", "-", or "?").
#'   \item \code{est}: Odds ratio estimate.
#'   \item \code{lb}: Lower 95% CI bound.
#'   \item \code{ub}: Upper 95% CI bound.
#'   \item \code{cicol}: Reserved column for formatted CI display.
#'   \item \code{group}: Subgroup label ("Inventors", "Melodifestival Winners", "Total").
#'   \item \code{type}: Row type ("header", "study", "subtotal", "total").
#' }
#'
#' @source Simulated data.
#'
#' @examples
#' data(inventors_vs_mello)
#' head(inventors_vs_mello)
"inventors_vs_mello"
