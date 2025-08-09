#' API_TOKEN S4 class
#'
#' Holds EliteForm API credentials/metadata.
#'
#' @slot url Character scalar with the EliteForm base URL.
#' @slot api_key Character scalar with the X-ApiKey value.
#' @slot TEAM_ID Character (typically length-1) team identifier.
#'
#' @exportClass API_TOKEN
#' @keywords classes
setClass("API_TOKEN", slots = list(
  url = "character",
  api_key = "character",
  TEAM_ID = "numeric"
))
