#' Run the OmniPeak Application
#'
#' @importFrom crayon blue bgBlue bold black
#' @export
run_OmniPeak <- function(...) {

  cat("\n")
  cat(crayon::blue("     +--------------------+\n"))
  app_name <- paste0(
    crayon::black(crayon::bold(crayon::bgBlue("     OmniPeak     ")))
  )
  cat(crayon::blue("     | "), app_name, crayon::blue(" |\n"), sep = "")
  cat(crayon::blue("     +--------------------+\n"))
  cat("\n")
  cat(crayon::blue(crayon::bold("Reshaping metabolomics peak table\n")))
  cat("\n")

flush.console()

  old_opts <- options(shiny.maxRequestSize = 1 * 1024^3)

    on.exit({
    options(old_opts)
    gc()
    }, add = TRUE)

  shiny::addResourcePath("www", system.file("www", package = "OmniPeak"))

  app <- shiny::shinyApp(ui = app_ui(), server = app_server)
  shiny::runApp(app, ...)
}
