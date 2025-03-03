#' downloadGoogleDriveFolder
#'
#' @description recursively download all files and subdirectories in a google 
#' drive folder
#'
#' @author Selene Fregosi
#'
#' @param folder_id Google Drive folder ID, parsed as the 'id' column finding
#'                  the drive info using googleddrive::drive_get or using 
#'                  using googledrive::as_id and the folder URL 
#' @param local_path full path to the folder to save the downloaded files                 
#'
#' @export
#' @import googledrive drive_ls drive_download
#'
#' @examples
#' 

# Define the recursive download function
downloadGoogleDriveFolder <- function(folder_id, local_path) {
  # Create the local directory if it doesn't exist
  dir.create(local_path, showWarnings = FALSE, recursive = TRUE)
  
  # List all items in the folder
  items <- googledrive::drive_ls(as_id(folder_id))
  
  # Loop through each item
  for (i in seq_len(nrow(items))) {
    item <- items[i, ]
    
    # Check if the item is a folder
    if (item$drive_resource[[1]]$mimeType == "application/vnd.google-apps.folder") {
      # If it's a folder, recursively download its contents
      subfolder_local_path <- file.path(local_path, item$name)
      downloadGoogleDriveFolder(item$id, subfolder_local_path)
      ###
      # the magic is here, it recalls the self-defined function again
      ###
    } else {
      # If it's a file, download it to the local path
      cat(sprintf("Downloading file: %s\n", file.path(local_path, item$name)))
      googledrive::drive_download(
        file = item,
        path = file.path(local_path, item$name),
        overwrite = TRUE
      )
    }
  }
}
