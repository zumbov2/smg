#' Retrieve data from the webtool Swiss Maps Generator
#'
#' \code{get_shapes} retrieves geodata via the webtool Swiss Maps Generator.
#'
#' @param shapes name of shapes to download. Available are \code{"country"}, \code{"cantons"},
#'      \code{"municipalities"} and \code{"lakes"}.
#' @param projection name of projection. Available are \code{"wgs84"} and \code{"cartesian"}.
#' @param year year of geodata. Available from 2010.
#' @param simplify degree of simplification of shapes. Ranging from \code{0} (no simplification)
#'     to \code{100} (maximal degree of simplification).
#'
#' @return If only one type of shapes is requested, a \code{Simple feature collection} is returned.
#'     For multiple shapes, a list of the corresponding collections is returned.
#' @export
#'
#' @examples
#' # Retrieve geodata
#' get_shapes()
#'
get_shapes <- function(shapes = c("country", "cantons", "municipalities", "lakes"),
                       projection = "wgs84", year = 2020, simplify = 0) {

  res <- call_api(shapes, projection, year, simplify)
  if (length(shapes) > 1) res_shapes <- list()

  if ("country" %in% shapes) {

    country <- extract_country(res)
    if (length(shapes) == 1) return(country)
    if (length(shapes) > 1) res_shapes[["country"]] <- country

    }
  if ("cantons" %in% shapes) {

    cantons <- extract_cantons(res)
    if (length(shapes) == 1) return(cantons)
    if (length(shapes) > 1) res_shapes[["cantons"]] <- cantons

  }
  if ("municipalities" %in% shapes) {

    municipalities <- extract_municipalities(res)
    if (length(shapes) == 1) return(municipalities)
    if (length(shapes) > 1) res_shapes[["municipalities"]] <- municipalities

  }
  if ("lakes" %in% shapes) {

    lakes <- extract_lakes(res)
    if (length(shapes) == 1) return(lakes)
    if (length(shapes) > 1) res_shapes[["lakes"]] <- lakes

  }

  return(res_shapes)

  }

