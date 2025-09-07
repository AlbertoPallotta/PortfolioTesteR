#' List available example scripts
#'
#' @description
#' Shows all example scripts included with the PortfolioTesteR package.
#' These examples demonstrate various strategy patterns and library functions.
#'
#' @return Character vector of example filenames
#' @export
#'
#' @examples
#' # See available examples
#' list_examples()
#'
#' # Run a specific example
#' # run_example("example_momentum_basic.R")
list_examples <- function() {
  example_dir <- system.file("examples", package = "PortfolioTesteR")
  if (example_dir == "") {
    stop("Examples directory not found. Please reinstall the package.")
  }
  examples <- list.files(example_dir, pattern = "\\.R$")
  if (length(examples) == 0) {
    message("No examples found.")
  } else {
    message("Available examples:")
    for (ex in examples) {
      message("  - ", ex)
    }
  }
  invisible(examples)
}

#' Run an example script
#'
#' @description
#' Executes an example script from the PortfolioTesteR package.
#' Use list_examples() to see available examples.
#'
#' @param example_name Character string with the example filename
#' @param echo Logical, whether to print the code as it runs (default TRUE)
#'
#' @return NULL (invisibly), runs the example for its side effects
#' @export
#'
#' @examples
#' \dontrun{
#' # List available examples
#' list_examples()
#'
#' # Run the basic momentum example
#' run_example("example_momentum_basic.R")
#' }
run_example <- function(example_name, echo = TRUE) {
  example_path <- system.file("examples", example_name, package = "PortfolioTesteR")

  if (!file.exists(example_path)) {
    stop("Example '", example_name, "' not found. Use list_examples() to see available examples.")
  }

  message("Running example: ", example_name)
  message("=" , strrep("=", nchar(example_name) + 17))

  source(example_path, echo = echo)
  invisible(NULL)
}
