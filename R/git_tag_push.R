#' Tag Version in Git and Push
#'
#' This function creates a tag for the specified version and pushes the tag to the remote repository.
#'
#' @param version A string specifying the version number for the tag. Default is "1.0.0".
#' @param message A string specifying the tag message. Default is constructed as "Release version " followed by the version number.
#'
#' @return This function returns a message indicating whether the tag operation was successful or not.
#'
#' @examples
#' \dontrun{
#'   git_tag_push("1.0.2")
#' }
#'
#' @export
git_tag_push <- function(version = "1.0.0", message = paste("Release version", version)) {
  # Create the tag command
  tag_command <- paste('git tag -a', shQuote(paste("v", version, sep = "")), '-m', shQuote(message))

  # Execute the tag command
  tag_result <- system(tag_command, ignore.stderr = TRUE)

  # Check if the tag creation was successful
  if (tag_result != 0) {
    stop("Failed to create tag. Please check the output for errors.")
  }

  # Create the push command for the tag
  push_tag_command <- paste('git push origin', shQuote(paste("v", version, sep = "")))

  # Execute the push command
  push_tag_result <- system(push_tag_command)
  if (push_tag_result != 0) {
    stop("Failed to push the tag to the remote repository. Please check the output for errors.")
  }

  return(paste("Successfully tagged version", version))
}
