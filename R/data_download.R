#' Download example datasets for SegSwarmEP
#'
#' Downloads example workspace data files from the GitHub repository to get
#' started with the SegSwarmEP package. Two datasets are available:
#' \itemize{
#'   \item `ACO_7_workspace.rda` - A smaller 7-district example from New Jersey
#'     (recommended for quick testing and learning).
#'   \item `ACO_21_workspace.rda` - A larger 21-district example from New Jersey
#'     (for more complex scenarios).
#' }
#'
#' Each workspace file contains pre-prepared data objects ready to use with
#' [aco_districting()]:
#' \describe{
#'   \item{`sf.dist`}{An `sf` spatial data frame of New Jersey school districts
#'     with enrollment demographics.}
#'   \item{`adj_matrix`}{The adjacency matrix for the districts.}
#'   \item{`graph`}{An `igraph` object of the district network.}
#'   \item{`df.dist`}{A non-spatial version of the district data.}
#' }
#'
#' @param dataset Character. Either `"ACO_7"` (default, ~2MB) or `"ACO_21"`
#'   (~11.8MB) to specify which example dataset to download.
#' @param destdir Character. The directory where the `.rda` file will be saved.
#'   Defaults to the current working directory.
#' @param overwrite Logical. If `TRUE`, overwrites an existing file. Defaults
#'   to `FALSE`.
#'
#' @return Invisibly returns the file path where the data was saved.
#'
#' @examples
#' \dontrun{
#' # Download the smaller 7-district example
#' download_example_data("ACO_7")
#' load("ACO_7_workspace.rda")
#' 
#' # Now you can use the data
#' str(sf.dist)
#' 
#' # Download the larger 21-district example to a specific folder
#' download_example_data("ACO_21", destdir = "~/data")
#' }
#'
#' @export
download_example_data <- function(dataset = c("ACO_7", "ACO_21"),
                                   destdir = ".",
                                   overwrite = FALSE) {
  dataset <- match.arg(dataset)
  
  # GitHub repository base URL (raw files)
  base_url <- "https://github.com/wootenjp/SegSwarmEP-R-/raw/main/"
  
  # Determine filename
  if (dataset == "ACO_7") {
    filename <- "ACO_7named_workspace.rda"
    local_name <- "ACO_7_workspace.rda"
  } else {
    filename <- "ACO_21_workspace.rda"
    local_name <- "ACO_21_workspace.rda"
  }
  
  url <- paste0(base_url, filename)
  dest_path <- file.path(destdir, local_name)
  
  # Check if file already exists
  if (file.exists(dest_path) && !overwrite) {
    stop("File '", dest_path, "' already exists. ",
         "Set overwrite = TRUE to replace it.")
  }
  
  # Create directory if needed
  if (!dir.exists(destdir)) {
    dir.create(destdir, recursive = TRUE)
  }
  
  # Download
  message("Downloading ", dataset, " dataset (", 
          if (dataset == "ACO_7") "~2MB" else "~11.8MB", 
          ") from GitHub...")
  
  tryCatch({
    utils::download.file(url, destfile = dest_path, mode = "wb", quiet = FALSE)
    message("Successfully downloaded to: ", dest_path)
    message("\nTo use the data, run: load('", dest_path, "')")
  }, error = function(e) {
    stop("Failed to download data: ", conditionMessage(e))
  })
  
  invisible(dest_path)
}
