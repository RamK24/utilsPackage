#' Download a report from Redcap
#'
#' This function downloads a report from Redcap using the specified API token, URL, and report ID.
#'
#' @param redcapTokenName The name of the environment variable containing the Redcap API token.
#' @param redcapUrl The URL of the Redcap instance.
#' @param redcapReportId The ID of the report to download.
#' @return A tibble containing the downloaded report data.
#' @import httr
#' @importFrom tibble as_tibble
#' @examples
#' \dontrun{
#' redcapTokenName <- "token"
#' redcapUrl <- "https://redcap.example.com"
#' redcapReportId <- "231"
#' report_data <- downloadRedcapReport(redcapTokenName, redcapUrl, redcapReportId)
#' head(report_data)
#' }
downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId) {
  redcap_token = Sys.getenv(redcapTokenName)
  if (redcap_token == "") {
    stop("API token not found.")
  }
else{
  url <- paste0(redcapUrl, "/api/")
  formData <- list(
    token = redcap_token,
    content = 'report',
    format = 'csv',
    report_id = redcapReportId,
    csvDelimiter = '',
    rawOrLabel = 'raw',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'csv'
  )

  response = httr::POST(url, body = formData, encode = "form")

  if (httr::status_code(response) != 200) {
    stop("Request to Redcap failed.")
  }

  report = httr::content(response, "text")
  return(as_tibble(report))
}
}
