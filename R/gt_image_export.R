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
save_gt_as_png_ragg <- function(
  gt_table,
  filename,
  width = NULL,
  height = NULL,
  scale = 1,
  dpi = 150,
  padding = 20
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

  # Auto-detect width and/or height from the gtable's natural dimensions.
  # A temporary null PDF device provides the graphics context required by
  # grid::convertWidth/convertHeight to resolve unit values to inches.
  if (is.null(width) || is.null(height)) {
    dims <- local({
      grDevices::pdf(file = grDevices::nullfile(), width = 20, height = 20)
      on.exit(grDevices::dev.off(), add = TRUE)

      list(
        width = if (is.null(width)) {
          w_in <- sum(grid::convertWidth(gt_grob$widths, "in", valueOnly = TRUE))
          ceiling(w_in * dpi) + padding
        } else {
          width
        },
        height = if (is.null(height)) {
          h_in <- sum(grid::convertHeight(gt_grob$heights, "in", valueOnly = TRUE))
          ceiling(h_in * dpi) + padding
        } else {
          height
        }
      )
    })
    width <- dims$width
    height <- dims$height
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
