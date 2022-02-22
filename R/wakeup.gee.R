#' Wakeup Google Earth Engine
#'
#' If you receive an error message regarding the Google Earth Engine server being disconnected due to inactivity, use this command to re-establish the connection.
#'
#' @param online.storage select online storage integration (mandatory for images download). Options are "drive" for Google Drive, "gcs" for Google Cloud Storage or NULL.
#'
#' @return
#' @export
#'
#' @examples
#' wakeup.gee()
#'
wakeup.gee<-function(online.storage="drive"){
  if(is.null(online.storage)){ee_Initialize(user = 'ndef', drive = F, gcs = F)}
  if(online.storage=="gcs"){ee_Initialize(user = 'ndef', drive = F, gcs = T)}
  if(online.storage=="drive"){ee_Initialize(user = 'ndef', drive = T, gcs = F)}
}
