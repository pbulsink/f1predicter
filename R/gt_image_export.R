#' Save a gt table as a PNG using ragg (no webshot/chrome required)
#'
#' @param gt_table A gt_tbl object
#' @param filename Output PNG file path
#' @param width Width in pixels. If `NULL` (default), the width is
#'   automatically determined from the table's natural content size.
#' @param height Height in pixels. If `NULL` (default), the height is
#'   automatically determined from the table's natural content size.
#' @param scale Scaling factor (default 1)
#' @param dpi Resolution in dots per inch used for auto-detection (default 150).
#'   Ignored when both `width` and `height` are supplied explicitly.
#' @param padding Extra pixels added to each auto-detected dimension (default
#'   20). Ignored when both `width` and `height` are supplied explicitly.
#' @return The filename (invisibly)
#' @export
#' @examples
#' \dontrun{
#' tbl <- gt::gt(data.frame(x = 1:3, y = c("a", "b", "c")))
#' save_gt_as_png_ragg(tbl, "output.png", width = 400, height = 200)
#' }
save_gt_as_png_ragg <- function(
  gt_table,
  filename,
  width = NULL,
  height = NULL,
  scale = 1,
  dpi = 150,
  padding = 20
) {
  now <- Sys.time()
  tryCatch(
    gt::gtsave(gt_table, filename),
    error = function(e) {
      cli::cli_alert_danger("Couldn't save file with `gtsave`.")
    }
  )

  if (file.exists(filename) && file.info(filename)$mtime >= now) {
    return(invisible(filename))
  }
  if (!requireNamespace("ragg", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg ragg} is required for PNG export. Please install it with {.code `install.packages('ragg')`}."
    )
  }
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg gt} is required for this function. Please install it with {.code `install.packages('gt')`}."
    )
  }
  if (!inherits(gt_table, "gt_tbl")) {
    cli::cli_abort("{.arg gt_table} must be a {.cls gt_tbl} object.")
  }
  if (!is.character(filename) || length(filename) != 1 || is.na(filename)) {
    cli::cli_abort(
      "{.arg filename} must be a single non-missing character string."
    )
  }
  if (
    !is.null(width) && (!is.numeric(width) || length(width) != 1 || width < 1)
  ) {
    cli::cli_abort("{.arg width} must be a single number >= 1.")
  }
  if (
    !is.null(height) &&
      (!is.numeric(height) || length(height) != 1 || height < 1)
  ) {
    cli::cli_abort("{.arg height} must be a single number >= 1.")
  }
  if (!is.numeric(scale) || length(scale) != 1 || scale <= 0) {
    cli::cli_abort("{.arg scale} must be a single positive number.")
  }
  if (!is.numeric(dpi) || length(dpi) != 1 || dpi <= 0) {
    cli::cli_abort("{.arg dpi} must be a single positive number.")
  }
  if (!is.numeric(padding) || length(padding) != 1 || padding < 0) {
    cli::cli_abort("{.arg padding} must be a single non-negative number.")
  }

  # Render gt table to grid object
  gt_grob <- gt::as_gtable(gt_table)

  # Auto-detect width and/or height from the gtable's natural dimensions.
  # A temporary null PDF device provides the graphics context required by
  # grid::convertWidth/convertHeight to resolve unit values to inches.
  if (is.null(width) || is.null(height)) {
    dims <- local({
      grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 20, height = 20)
      on.exit(grDevices::dev.off(), add = TRUE)

      list(
        width = if (is.null(width)) {
          w_in <- sum(grid::convertWidth(
            gt_grob$widths,
            "in",
            valueOnly = TRUE
          ))
          ceiling(w_in * dpi) + padding
        } else {
          width
        },
        height = if (is.null(height)) {
          h_in <- sum(grid::convertHeight(
            gt_grob$heights,
            "in",
            valueOnly = TRUE
          ))
          ceiling(h_in * dpi) + padding
        } else {
          height
        }
      )
    })
    width <- dims$width
    height <- dims$height
  }

  if (width < 1 || height < 1) {
    cli::cli_abort(
      "Computed canvas dimensions ({width} x {height} px) must both be >= 1. \\
      Increase {.arg dpi} or {.arg padding}, or supply explicit {.arg width}/{.arg height}."
    )
  }

  # Use ragg to save as PNG
  ragg::agg_png(
    filename,
    width = width,
    height = height,
    units = "px",
    scaling = scale
  )
  grid::grid.draw(gt_grob)
  grDevices::dev.off()
  invisible(filename)
}
