#' Commit Changes and Tag Version in Git
#'
#' This function commits all changes in the current directory to Git, creates a
#' tag for the specified version, and pushes both the commit and the tag to the
#' remote repository.
#'
#' @param version A string specifying the version number for the tag. Default is "1.0.0".
#' @param message A string specifying the commit message. Default is constructed as "Release version " followed by the version number.
#'
#' @return This function returns a message indicating whether the operations were successful or not.
#'
#' @examples
#' \dontrun{
#'   commit_and_tag("1.0.2")
#' }
#'
#' @export
commit_and_tag <- function(version = "1.0.0", message = paste("Release version", version)) {
  # Commit all changes
  system('git add .')
  commit_result <- system(paste('git commit -m', shQuote(message)), ignore.stderr = TRUE)

  # Check if the commit was successful
  if (commit_result != 0) {
    stop("Failed to commit changes. Please check the output for errors.")
  }

  # Create the tag
  tag_result <- system(paste('git tag -a', shQuote(paste("v", version, sep = "")), '-m', shQuote(message)), ignore.stderr = TRUE)

  # Check if the tag creation was successful
  if (tag_result != 0) {
    stop("Failed to create tag. Please check the output for errors.")
  }

  # Push both the commit and the tag to the remote repository
  push_result <- system('git push')
  if (push_result != 0) {
    stop("Failed to push changes to the remote repository. Please check the output for errors.")
  }

  push_tag_result <- system(paste('git push origin', shQuote(paste("v", version, sep = ""))))
  if (push_tag_result != 0) {
    stop("Failed to push the tag to the remote repository. Please check the output for errors.")
  }

  return(paste("Successfully committed changes and tagged version", version))
}
