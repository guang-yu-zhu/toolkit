#' Commit Changes and Push to Git
#'
#' This function commits all changes in the current directory to Git and pushes the commit to the remote repository.
#'
#' @param message A string specifying the commit message. 
#'
#' @return This function returns a message indicating whether the operations were successful or not.
#'
#' @examples
#' \dontrun{
#'   git_commit_push("")
#' }
#'
#' @export
git_commit_push <- function(message = "") {
  # Create the command for adding changes and committing
  commit_command <- paste('git add . && git commit -m', shQuote(message))

  # Execute the commit command
  commit_result <- system(commit_command, ignore.stderr = TRUE)

  # Check if the commit was successful
  if (commit_result != 0) {
    stop("Failed to commit changes. Please check the output for errors.")
  }

  # Create the push command
  push_command <- 'git push'

  # Execute the push command
  push_result <- system(push_command, ignore.stderr = TRUE)
  if (push_result != 0) {
    stop("Failed to push changes to the remote repository. Please check the output for errors.")
  }

  return("Successfully committed changes and pushed to the remote repository.")
}
