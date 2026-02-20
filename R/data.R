#' New Jersey School District Enrollment Data (Simulated)
#'
#' A simulated dataset loosely representing the demographic composition and
#' geographic positions of school districts in New Jersey. The figures are
#' synthetic and are provided solely to demonstrate the SegSwarmEP workflow.
#' For real analyses use official data from the New Jersey Department of
#' Education (<https://www.nj.gov/education/data/>) or the National Center for
#' Education Statistics (<https://nces.ed.gov/>).
#'
#' @format A data frame with 60 rows and 8 variables:
#' \describe{
#'   \item{district_id}{Character; unique district identifier (e.g., `"NJ_001"`).}
#'   \item{district_name}{Character; district name.}
#'   \item{county}{Character; county in which the district is located.}
#'   \item{white}{Numeric; number of white (non-Hispanic) students enrolled.}
#'   \item{nonwhite}{Numeric; number of non-white students enrolled
#'     (Black, Hispanic, Asian, and other groups combined).}
#'   \item{total}{Numeric; total student enrollment (`white + nonwhite`).}
#'   \item{lat}{Numeric; approximate latitude of the district centroid.}
#'   \item{lon}{Numeric; approximate longitude of the district centroid.}
#' }
#'
#' @source Simulated data for package demonstration purposes.
#'
#' @examples
#' data(nj_districts)
#' head(nj_districts)
#' segregation_summary(nj_districts, "white", "nonwhite")
"nj_districts"
