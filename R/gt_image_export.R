#' Save a gt table as a PNG using ragg (no webshot/chrome required)
#'
#' @param gt_table A gt_tbl object
#' @param filename Output PNG file path
#' @param width Width in pixels (default 1400)
#' @param height Height in pixels (default 800)
#' @param scale Scaling factor (default 1)
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
  width = 1400,
  height = 800,
  scale = 1
) {
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
  # Render gt table to grid object
  gt_grob <- gt::as_gtable(gt_table)
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
