#' Elite Form API Login
#'
#' Implements Elite Form's Login using your “X-ApiKey” and your custom Elite Form URL
#' @param url URL string
#' @param api_key X-ApiKey string
#' @return Returns a S4 Object that saves your X-ApiKey and URL and acts as a token for later pulls
#' @examples
#' # Create an API token with your EliteForm URL and API key
#' token <- EF_CreateAPIToken("https://your.eliteform.com", "your-api-key")
#'
#' # Access components of the token
#' token@url
#' token@api_key
#'
#' @export

EF_CreateAPIToken <- function(url,api_key ){
  setClass("API_TOKEN", slots=list(url="character", api_key="character", TEAM_ID = "numeric"))
  token <- new("API_TOKEN", url = url, api_key = api_key)
  return(token)
}


#' Get List of Teams
#'
#' Get team names and IDs for the organization
#' @param token API Token Object (see \code{\link{EF_CreateAPIToken}}
#' @return A data frame with the following columns:
#' \describe{
#'   \item{TeamId}{`numeric` (long) — Team unique identifier}
#'   \item{Name}{`character` (string) — Team Name}
#' }
#' @examples
#' Download Team List
#' Team_list <- EF_GetTeamsList(token)
#'
#' @export

EF_GetTeamsList <- function(token){
  pull_json <- GET(
    url = paste0(token@url, "/api/v1/getteamslist"),
    add_headers(accept = "application", `X-ApiKey` = token@api_key)
  )

  teams_df <- fromJSON(content(test, as="text"))
  return(teams_df)
}

#' Update the Token Team Attribute
#'
#' Update the custom API Token to select the team to pull for the remainder of values
#' Must be saved as the token values ie token <- EF_UpdateTokenTeam(token, 1)
#' @param token API Token Object (see \code{\link{EF_CreateAPIToken}}
#' @param TEAM_ID Team Id referenced in  \code{\link{EF_GetTeamsList}}
#' @return Custom API Token Object
#' @examples
#' #Intialize Token
#' token <- EF_CreateAPIToken("https://your.eliteform.com", "your-api-key")
#'
#' # Get Team ID
#' Team_list <- EF_GetTeamsList(token)
#'
#' # Update Token with Team ID
#' token <- EF_UpdateTokenTeam(token, TEAM_ID$TeamId)
#'
#' @export

EF_UpdateTokenTeam <- function(token, TEAM_ID){
  token@TEAM_ID <- TEAM_ID
  return(token)
}

#' Convert Date to EliteForm Format
#' @return Returns date in MMDDYYYY format

convert_to_MMddyyyy <- function(date_str) {
  # Try parsing the input using multiple formats
  parsed_date <- tryCatch({
    as.Date(date_str, tryFormats = c("%m/%d/%Y", "%m-%d-%Y", "%m%d%Y",
                                     "%Y/%m/%d", "%Y-%m-%d", "%Y%m%d"))
  }, error = function(e) {
    NA
  })

  # Check if parsing was successful
  if (is.na(parsed_date)) {
    stop("Error: Invalid date format. Please use a recognizable date format like YYYY-MM-DD or MM/DD/YYYY.")
  }

  # Format to "MMddyyyy"
  formatted <- format(parsed_date, "%m%d%Y")
  return(formatted)
}

#' Get All Tracked Reps
#'
#' Get all tracked reps for a team on a specific date. If you don’t specify `reportDate`, the current date will be used.
#'
#' @param token API Token Object (see \code{\link{EF_CreateAPIToken}})
#' @param date Input date as a string in the format \code{"\%m-\%d-\%Y"} (e.g., \code{"07-28-2025"}).
#'
#' @return Returns a data frame with the following columns:
#' \describe{
#'   \item{Date}{`character` — Date of the lift (MM-DD-YYYY format)}
#'   \item{Time}{`character` — Time the lift was performed (HH:MM)}
#'   \item{AthleteId}{`numeric` — Unique athlete identifier}
#'   \item{LastName}{`character` — Athlete's last name}
#'   \item{FirstName}{`character` — Athlete's first name}
#'   \item{Exercise}{`character` — Name of the exercise performed}
#'   \item{Reps}{`integer` — Number of repetitions tracked}
#'   \item{PeakVelocity}{`numeric` — Peak bar velocity (m/s)}
#' }
#' @details
#'If you don’t specify reportDate, the current date will be used.(These results do not include lifts that were entered through Paperless. For Paperless results, use \code{\link{EF_GetAllSets}} with includePaperless set.)
#' @export


EF_GetAllTrackedReps <- function(token, date = ""){
  request_url <- "/api/v1/GetAllTrackedReps/"
  if(token$TEAM_ID == as.numeric())
  if(date != ""){
    date = convert_to_MMddyyyy(date)
    request <- paste0(request_url, token@TEAM_ID, "/",date)

  } else {
    request <- paste0(request_url, token@TEAM_ID,"/")

  }
  pull_json <- GET(
    url = paste0(token@url, request),
    add_headers(accept = "application", `X-ApiKey` = token@api_key)
  )
  df <- fromJSON(content(pull_json, as="text"))
  return(df)
}


#' Get All Tracked Reps
#'
#'Get all tracked reps for a team on a specific data
#' @param token API Token Object (see \code{\link{EF_CreateAPIToken}})
#' @param date Input date as a string in the format \code{"\%m-\%d-\%Y"} (e.g., \code{"07-28-2025"}).
#' @return Returns a data frame with the following columns:
#' \describe{
#'   \item{Date}{`character` — Date of the lift (MM-DD-YYYY format)}
#'   \item{Time}{`character` — Time the lift was performed (HH:MM)}
#'   \item{AthleteId}{`numeric` — Unique athlete identifier (long)}
#'   \item{LastName}{`character` — Athlete's last name}
#'   \item{FirstName}{`character` — Athlete's first name}
#'   \item{Exercise}{`character` — Name of the exercise performed}
#'   \item{SetNumber}{`integer` — Set number within the session}
#'   \item{RepNumber}{`integer` — Repetition number within the set}
#'   \item{LoadFactor}{`numeric` — Load factor used in the lift}
#'   \item{ActualWeight}{`numeric` — Actual weight lifted (kg)}
#'   \item{AverageVelocity}{`numeric` — Average bar velocity (m/s)}
#'   \item{PeakVelocity}{`numeric` — Peak bar velocity (m/s)}
#'   \item{AveragePower}{`numeric` — Average power output (W)}
#'   \item{PeakPower}{`numeric` — Peak power output (W)}
#'   \item{ActualRest}{`integer` — Rest duration before the lift (seconds)}
#'   \item{ReactionTimeInMS}{`integer` — Reaction time in milliseconds}
#'   \item{Work}{`numeric` — Total work performed (Joules)}
#'   \item{TrackerClassificationId}{`integer` — Internal classification ID}
#'   \item{ExerciseId}{`integer` — Internal exercise identifier}
#'   \item{ExerciseName}{`character` — Full name of the exercise}
#'   \item{TimeToPeak}{`numeric` — Time to reach peak velocity (s)}
#'   \item{TargetValue}{`numeric` — Target value set for the rep}
#'   \item{TargetMetric}{`character` — Unit or type of the target metric}
#'   \item{Duration}{`numeric` — Total duration of the rep (s)}
#'   \item{EccentricAvgVelocity}{`numeric` — Average eccentric velocity (m/s)}
#'   \item{EccentricAvgPower}{`numeric` — Average eccentric power (W)}
#'   \item{EccentricPeakVelocity}{`numeric` — Peak eccentric velocity (m/s)}
#'   \item{EccentricPeakPower}{`numeric` — Peak eccentric power (W)}
#'   \item{EccentricDuration}{`numeric` — Duration of eccentric phase (s)}
#' }
#' @details
#' NOTE:
#' This call includes all fields provided in the EF Data Points reports but were not available in \code{\link{EF_GetAllTrackedReps}}
#' If you don’t specify reportDate, the current date will be used. (These results do not include lifts that were entered through Paperless. For Paperless results, use GetAllSets with includePaperless set.)
#'If you don’t specify reportDate, the current date will be used.(These results do not include lifts that were entered through Paperless. For Paperless results, use \code{\link{EF_GetAllSets}} with includePaperless set.)
#'@export

EF_GetAllTrackedReps3 <- function(token, date = ""){
  request_url <- "/api/v1/GetAllTrackedReps3/"
  if(date != ""){
    date = convert_to_MMddyyyy(date)
    request <- paste0(request_url, token@TEAM_ID, "/",date)

  } else {
    request <- paste0(request_url, token@TEAM_ID,"/")

  }
  pull_json <- GET(
    url = paste0(token@url, request),
    add_headers(accept = "application", `X-ApiKey` = token@api_key)
  )
  df <- fromJSON(content(pull_json, as="text"))
  return(df)
}


#' Get all Sets
#'
#'Get all sets for a team on a specific date
#' @param token API Token Object (see \code{\link{EF_CreateAPIToken}})
#' @param date Input date as a string in the format \code{"\%m-\%d-\%Y"} (e.g., \code{"07-28-2025"}).
#' @param includePaperless
#' @description
#' NOTE:
#' 1 sets includePaperless to true, and 0 sets it to false.
#' If you don’t specify reportDate, the current date will be used.
#' If you don’t specify includePaperless, the option will be set to true.
#' @return Returns a data frame with the following columns:
#' \describe{
#'   \item{Date}{`character` — Date of the lift (MM-DD-YYYY format)}
#'   \item{Time}{`character` — Time the lift was performed (HH:MM)}
#'   \item{AthleteId}{`numeric` — Unique athlete identifier (long)}
#'   \item{LastName}{`character` — Athlete's last name}
#'   \item{FirstName}{`character` — Athlete's first name}
#'   \item{Exercise}{`character` — Name of the exercise performed}
#'   \item{SetNumber}{`integer` — Set number within the session}
#'   \item{RepsAssigned}{`integer` — Number of repetitions assigned}
#'   \item{RepsCompleted}{`integer` — Number of repetitions completed}
#'   \item{LoadFactor}{`numeric` — Load factor used in the lift}
#'   \item{ActualWeight}{`numeric` — Actual weight lifted (kg)}
#'   \item{AverageVelocity}{`numeric` — Average bar velocity (m/s)}
#'   \item{PeakVelocity}{`numeric` — Peak bar velocity (m/s)}
#'   \item{AveragePower}{`numeric` — Average power output (W)}
#'   \item{PeakPower}{`numeric` — Peak power output (W)}
#'   \item{ActualRest}{`integer` — Rest duration before the lift (seconds)}
#'   \item{ReactionTimeInMS}{`integer` — Reaction time before lift initiation (milliseconds)}
#'   \item{Work}{`numeric` — Total work performed during the rep (Joules)}
#' }
#' @export
EF_GetAllSets <- function(token, date = "", includePaperless = ""){
  includePaperless <- as.character(includePaperless)
  if (!(includePaperless %in% c("1", "0", ""))){
    stop("Error: Input must be 1, 0 or ''")
  }
  request_url <- "/api/v1/GetAllSets/"
  if(date != ""){
    date = convert_to_MMddyyyy(date)
    request <- paste0(request_url, token@TEAM_ID, "/",date)

  } else {
    request <- paste0(request_url, token@TEAM_ID,"/")

  }
  pull_json <- GET(
    url = paste0(token@url, request),
    add_headers(accept = "application", `X-ApiKey` = token@api_key)
  )
  df <- fromJSON(content(pull_json, as="text"))
  return(df)
}

#' Get all 1 Rep Max for Entire Team
#'
#' Get all current 1RMs for a team
#' @param token API Token Object (see \code{\link{EF_CreateAPIToken}})
#' @return Returns a data frame with the following columns:
#' \describe{
#'   \item{Date}{`character` — Date of the record (MM-DD-YYYY format)}
#'   \item{AthleteId}{`numeric` — Unique athlete identifier (long)}
#'   \item{LastName}{`character` — Athlete's last name}
#'   \item{FirstName}{`character` — Athlete's first name}
#'   \item{Exercise}{`character` — Name of the exercise performed}
#'   \item{Result}{`numeric` — Recorded result or output metric}
#' }
#' @export


EF_Get1RMs <- function(token){

  request <- paste0("/api/v1/get1rms/", token@TEAM_ID)
  print(request)
  pull_json <- GET(
    url = paste0(token@url, request),
    add_headers(accept = "application", `X-ApiKey` = token@api_key)
  )
  print(pull_json)
  df <- fromJSON(content(pull_json, as="text"))
  return(df)
}

#' Get all Power 1 Rep Max for Entire Team
#'
#' Get all current Power
#' @param token API Token Object (see \code{\link{EF_CreateAPIToken}})
#' @return Returns a data frame with the following columns:
#' \describe{
#'   \item{Date}{`character` — Date of the lift (MM-DD-YYYY format)}
#'   \item{AthleteId}{`numeric` — Unique athlete identifier (long)}
#'   \item{LastName}{`character` — Athlete's last name}
#'   \item{FirstName}{`character` — Athlete's first name}
#'   \item{Exercise}{`character` — Name of the exercise performed}
#'   \item{AveragePower}{`numeric` — Average power output (Watts)}
#'   \item{PeakPower}{`numeric` — Peak power output (Watts)}
#' }
#' @export

EF_GetPower1RMs <- function(token){

  request <- paste0("/api/v1/getpower1rms/", token@TEAM_ID)
  print(request)
  pull_json <- GET(
    url = paste0(token@url, request),
    add_headers(accept = "application", `X-ApiKey` = token@api_key)
  )
  print(pull_json)
  df <- fromJSON(content(pull_json, as="text"))
  return(df)
}

#' Get all Player Info for entire team
#'
#' Get basic information for all athletes on a team
#' @param token API Token Object (see \code{\link{EF_CreateAPIToken}})
#' @param date Input date as a string in the format \code{"\%m-\%d-\%Y"} (e.g., \code{"07-28-2025"}).
#' @return Returns a data frame with the following columns:
#' \describe{
#'   \item{AthleteId}{`numeric` — Unique athlete identifier (long)}
#'   \item{LastName}{`character` — Athlete's last name}
#'   \item{FirstName}{`character` — Athlete's first name}
#'   \item{Positions}{`character` — Athlete's playing position(s)}
#'   \item{Classification}{`character` — Athlete's roster classification (e.g., Freshman, Senior)}
#'   \item{Categories}{`character` — Custom or predefined grouping categories (e.g., Returners, Injured)}
#' }
#'
#' @details If you don’t specify reportDate, the current date will be used. A date is required because the athletes belonging to a team can vary by season. Any date within the desired season will work for specifying a season. EF_GetAthleteInfo <- function(token){
#' @export
EF_GetAthleteInfo <- function(token, date = ""){
  request_url <- "/api/v1/getathleteinfo/"
  if(date != ""){
    date = convert_to_MMddyyyy(date)
    request <- paste0(request_url, token@TEAM_ID, "/",date)

  } else {
    request <- paste0(request_url, token@TEAM_ID,"/")

  }
  pull_json <- GET(
    url = paste0(token@url, request),
    add_headers(accept = "application", `X-ApiKey` = token@api_key)
  )
  df <- fromJSON(content(pull_json, as="text"))
  return(df)
}
