#!/usr/bin/env Rscript
# CFU Plot Studio launcher.
#
#   Rscript run_app.R
#
# or, on Windows, double-click "Run CFU Plot Studio.bat".
#
# Locates the app relative to this file, installs anything missing into a
# private library beside it, picks a free loopback port and opens a browser.
# Nothing is installed system-wide and nothing leaves the machine.
#
# Environment overrides:
#   CFU_APP_HOST   bind address              (default 127.0.0.1)
#   CFU_APP_PORT   fixed port                (default: first free from 4267)
#   CFU_APP_LIB    private library location  (default <app>/.Rlibrary)
#   CFU_NO_INSTALL set to 1 to fail instead of installing anything

app_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  hit <- grep("^--file=", args, value = TRUE)
  if (length(hit) > 0) {
    dirname(normalizePath(sub("^--file=", "", hit[1]), winslash = "/", mustWork = TRUE))
  } else {
    normalizePath(getwd(), winslash = "/")
  }
})

if (!file.exists(file.path(app_dir, "app.R"))) {
  stop("app.R was not found next to run_app.R (looked in ", app_dir, ").", call. = FALSE)
}

# A private library keeps a first run from writing into a system library, which
# on a managed machine is usually not writable anyway.
lib_dir <- Sys.getenv("CFU_APP_LIB", file.path(app_dir, ".Rlibrary"))
if (!dir.exists(lib_dir)) {
  dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)
}
if (dir.exists(lib_dir)) .libPaths(c(lib_dir, .libPaths()))

REQUIRED <- c(
  "shiny", "ggplot2", "dplyr", "readr", "tibble", "tidyr", "scales",
  "emmeans", "broom", "DT", "colourpicker", "jsonlite"
)
# Export formats that degrade gracefully: the app hides or falls back on these.
OPTIONAL <- c(
  officer   = "PowerPoint export",
  rvg       = "editable vector art in PowerPoint",
  gganimate = "animated GIF export",
  gifski    = "GIF encoding"
)

have <- function(p) requireNamespace(p, quietly = TRUE)

missing_required <- REQUIRED[!vapply(REQUIRED, have, logical(1))]
if (length(missing_required) > 0) {
  if (identical(Sys.getenv("CFU_NO_INSTALL"), "1")) {
    stop("Missing packages: ", paste(missing_required, collapse = ", "),
         "\nInstall them, or unset CFU_NO_INSTALL to let this script do it.", call. = FALSE)
  }
  message("")
  message("First run: installing ", length(missing_required), " package(s) into")
  message("  ", lib_dir)
  message("  ", paste(missing_required, collapse = ", "))
  message("This happens once. It can take several minutes.")
  message("")

  repos <- getOption("repos")
  if (is.null(repos[["CRAN"]]) || !nzchar(repos[["CRAN"]]) || identical(unname(repos[["CRAN"]]), "@CRAN@")) {
    repos <- c(CRAN = "https://cloud.r-project.org")
  }
  utils::install.packages(missing_required, lib = lib_dir, repos = repos)

  still_missing <- missing_required[!vapply(missing_required, have, logical(1))]
  if (length(still_missing) > 0) {
    stop("Could not install: ", paste(still_missing, collapse = ", "),
         "\nCheck the messages above. Behind a proxy, set HTTPS_PROXY before running.",
         call. = FALSE)
  }
}

missing_optional <- names(OPTIONAL)[!vapply(names(OPTIONAL), have, logical(1))]
if (length(missing_optional) > 0) {
  message("Optional packages not installed, so these exports are unavailable:")
  for (p in missing_optional) message("  ", p, "  -> ", OPTIONAL[[p]])
  message('Install them any time with: install.packages(c("',
          paste(missing_optional, collapse = '", "'), '"))')
  message("")
}

host <- Sys.getenv("CFU_APP_HOST", "127.0.0.1")

# Find a free port rather than assume one, so a second copy of the app does not
# die with a bare "address already in use".
#
# Probe by CONNECTING, not by binding. Windows SO_REUSEADDR lets a second bind
# to a live listening port succeed, so serverSocket() reports a busy port as
# free and the failure only surfaces later, inside Shiny.
port_in_use <- function(host, port) {
  con <- try(
    suppressWarnings(socketConnection(
      host = host, port = port, open = "r+", blocking = TRUE, timeout = 1
    )),
    silent = TRUE
  )
  if (inherits(con, "try-error")) return(FALSE)
  close(con)
  TRUE
}

pick_port <- function(host, candidates) {
  for (p in candidates) {
    if (!port_in_use(host, p)) return(p)
  }
  NULL
}

requested <- Sys.getenv("CFU_APP_PORT", "")
port <- if (nzchar(requested)) {
  p <- suppressWarnings(as.integer(requested))
  if (!is.na(p) && port_in_use(host, p)) {
    stop("CFU_APP_PORT is set to ", p, ", but something is already listening there.",
         call. = FALSE)
  }
  p
} else {
  pick_port(host, 4267:4287)
}
if (length(port) != 1 || is.na(port)) port <- NULL

message("CFU Plot Studio")
message("  app:  ", app_dir)
if (!is.null(port)) message("  url:  http://", host, ":", port)
message("  stop: close this window, or press Ctrl+C")
message("")

run_args <- list(
  appDir = app_dir,
  host = host,
  launch.browser = function(url) try(utils::browseURL(url), silent = TRUE)
)
if (!is.null(port)) run_args$port <- port

suppressPackageStartupMessages(do.call(shiny::runApp, run_args))
