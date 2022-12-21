wax_colours <- list(
  d_pal = c(
            "#ff6361",
            "#8b5196",
            "#ffb140",
            "#087E8B",
            "#0B3954",
            "#DD7373",
            "#4C5B5C",
            "#D64550",
            "#DAEFB3",
            "#3B3561",
            "#D1D1D1",
            "#EAD94C",
            "#EA1744",
            "#30BCED",
            "#DD4B1A",
            "#EAB464",
            "#FAAA8D",
            "#FEEFDD",
            "#B88E8D",
            "#351431",
            "#5F1A57",
            "#34435E",
            "#696D7D",
            "#235789",
            "#E34A6F"
  ),
  c_binar_pal = c(
             "#8b5196",
             "#003f5c",
             "#ff6361"
  ),
  c_single_hue = c(
             "#003f5c",
             "#8b5196",
             "#ff6361",
             "#ffb140"
  )
)

#' Prepare color palette like viridis methode
#'
#' @param n is the number of colors to be in the palette.
#' @param alpha is the alpha transparency, a number in [0,1], see argument alpha in hsv.
#' @param begin is the (corrected) hue in [0,1] at which the color map begins.
#' @param end is the (corrected) hue in [0,1] at which the color map ends.
#' @param direction sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param option is choice of palette in wax_colours data
#'
wax_pal <- function(n, alpha = 1, begin = 0, end = 1, direction = 1, option = "c_single_hue") {
  if (begin < 0 | begin > 1 | end < 0 | end > 1) {
    stop("begin and end must be in [0,1]")
  }
  if (abs(direction) != 1) {
    stop("direction must be 1 or -1")
  }
  if (n == 0) {
    return(character(0))
  }
  if (direction == -1) {
    tmp <- begin
    begin <- end
    end <- tmp
  }
  map_cols <- wax_colours[[option]]
  fn_cols <- grDevices::colorRamp(map_cols, space = "Lab", interpolate = "spline")
  cols <- fn_cols(seq(begin, end, length.out = n)) / 255
  grDevices::rgb(cols[, 1], cols[, 2], cols[, 3], alpha = alpha)
}

#' Generate color palette like viridis methode
#'
#' @param alpha is the alpha transparency, a number in [0,1], see argument alpha in hsv.
#' @param begin is the (corrected) hue in [0,1] at which the color map begins.
#' @param end is the (corrected) hue in [0,1] at which the color map ends.
#' @param direction sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param option is choice of palette in wax_colours data
wax_pal2 <- function(alpha = 1, begin = 0, end = 1, direction = 1, option = "c_single_hue") {
  function(n) {
    wax_pal(n, alpha, begin, end, direction, option)
  }
}


#' Scale color from wax_palette
#'
#' @param ... is the information passed to the underlying functions. Auto-completed, please ignore.
#' @param alpha is the alpha transparency, a number in [0,1], see argument alpha in hsv.
#' @param begin is the (corrected) hue in [0,1] at which the color map begins.
#' @param end is the (corrected) hue in [0,1] at which the color map ends.
#' @param direction sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param option is choice of palette in wax_colours data
#' @param discrete Operator defining the use of the palette (TRUE for discrete, FALSE for continuous).
#' @param aesthetics Character string or vector of character strings listing the
#'   name(s) of the aesthetic(s) that this scale works with.
#'
#' @import ggplot2
#'
scale_wax <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                      discrete = FALSE, option = "c_single_hue", aesthetics = "colour") {
  if (discrete) {
    discrete_scale(aesthetics = aesthetics,
                   scale_name = "wax_pal",
                   wax_pal2(alpha, begin, end, direction, option), ...)
  } else if (aesthetics == "colour") {
    scale_color_gradientn(colours = wax_pal(256, alpha, begin, end, direction, option), ...)
  } else if (aesthetics == "fill") {
    scale_fill_gradientn(colours = wax_pal(256, alpha, begin, end, direction, option), ...)
  } else {
    stop("Please, check discrete and asthetics parameters.")
  }
}


#' @export
#' @rdname scale_wax
scale_color_wax <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                      discrete = FALSE, option = "c_single_hue", aesthetics = "colour") {

  scale_wax(..., alpha = alpha, begin = begin, end = end, direction = direction,
                      discrete = discrete, option = option, aesthetics = aesthetics)
}

#' @export
#' @rdname scale_wax
scale_fill_wax <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                      discrete = FALSE, option = "c_single_hue", aesthetics = "fill") {

  scale_wax(..., alpha = alpha, begin = begin, end = end, direction = direction,
                      discrete = discrete, option = option, aesthetics = aesthetics)
}

#' @export
#' @rdname scale_wax
scale_fill_wax_d <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                      discrete = TRUE, option = "c_single_hue", aesthetics = "fill") {

  scale_wax(..., alpha = alpha, begin = begin, end = end, direction = direction,
                      discrete = discrete, option = option, aesthetics = aesthetics)
}

#' @export
#' @rdname scale_wax
scale_fill_wax_c <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                      discrete = FALSE, option = "c_single_hue", aesthetics = "fill") {

  scale_wax(..., alpha = alpha, begin = begin, end = end, direction = direction,
                      discrete = discrete, option = option, aesthetics = aesthetics)
}

#' @export
#' @rdname scale_wax
scale_color_wax_c <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                      discrete = FALSE, option = "c_single_hue", aesthetics = "colour") {

  scale_wax(..., alpha = alpha, begin = begin, end = end, direction = direction,
                      discrete = discrete, option = option, aesthetics = aesthetics)
}

#' @export
#' @rdname scale_wax
scale_color_wax_d <- function(..., alpha = 1, begin = 0, end = 1, direction = 1,
                      discrete = TRUE, option = "c_single_hue", aesthetics = "colour") {

  scale_wax(..., alpha = alpha, begin = begin, end = end, direction = direction,
                      discrete = discrete, option = option, aesthetics = aesthetics)
}
