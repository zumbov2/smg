# Call API
#' @importFrom httr GET
#' @noRd
call_api <- function(shapes, projection, year, simplify) {

  # Check inputs
  if (sum(!shapes %in% c("country", "cantons", "municipalities", "lakes")) > 0) stop("Check input for parameter 'shapes'.")
  if (sum(!projection %in% c("wgs84", "cartesian")) > 0) stop("Check input for parameter 'projection'.")
  if (length(projection) > 1) message("Multiple entries found for 'projection'. Select first entry for API request.")
  if (year < 2010) stop("Check input for parameter 'year'.")
  if (simplify > 100 | simplify < 0) stop("Check input for parameter 'simplify'.")

  # Call API
  res <- httr::GET(
    url = "https://swiss-maps.vercel.app/api/generate",
    query = list(
      format = "topojson",
      projection = projection[1],
      year = year,
      simplify = paste0(100 - simplify, "%"),
      shapes = paste0(shapes, collapse = ",")
    )
  )

  # Return
  return(res)

}

# Extract country
#' @importFrom sf st_read
#' @noRd
extract_country <- function(res) sf::st_read(res, layer = "country", quiet = T)

# Extract cantons
#' @importFrom sf st_read
#' @noRd
extract_cantons <- function(res) sf::st_read(res, layer = "cantons", quiet = T)

# Extract municipalities
#' @importFrom sf st_read
#' @noRd
extract_municipalities <- function(res) sf::st_read(res, layer = "municipalities", quiet = T)

# Extract lakes
#' @importFrom sf st_read
#' @noRd
extract_lakes <- function(res) sf::st_read(res, layer = "lakes", quiet = T)
