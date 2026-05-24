suppressPackageStartupMessages(suppressWarnings(requireNamespace("shiny", quietly = TRUE)))

app_args <- list(
  appDir = "C:/Users/mbaff/Downloads/cfu plot tool",
  host = Sys.getenv("CFU_APP_HOST", "127.0.0.1"),
  launch.browser = interactive()
)

requested_port <- Sys.getenv("CFU_APP_PORT", "")
if (nzchar(requested_port)) {
  app_args$port <- as.integer(requested_port)
}

suppressPackageStartupMessages(suppressWarnings(do.call(shiny::runApp, app_args)))
