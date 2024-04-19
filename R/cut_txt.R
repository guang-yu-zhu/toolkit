#' Cut text file into chunks with specified number of chapters
#'
#' This function reads a text file and splits it into chunks where each chunk contains a specified number of chapters.
#' Chapters are identified by markers like "第*章".
#'
#' @param file The path to the text file to be cut.
#' @param marker The regular expression pattern to identify chapters in the text file.
#' @param num_chapters The number of chapters to include in each chunk. Default is 25.
#' @return NULL (The function writes output files.)
#' @export
#'
#' @examples
#' \dontrun{
#' cut_txt(file = "your_file.txt", marker = "第.*章", num_chapters = 25)
#' }
#'
#' @importFrom pacman p_load
#' @importFrom stringr str_pad
#' @importFrom dplyr mutate_all
#' @importFrom tools file_path_sans_ext
cut_txt<-function(file,marker="第.*章",num_chapters=25){
  #pacman::p_load(stringr,dplyr)
  name = tools::file_path_sans_ext(file)
  lines <- readLines(file,encoding = "gbk")
  lines <- iconv(lines, from = "gbk", to = "UTF-8")

  # Find the indices of lines containing "第*章" (chapter markers)
  chapter_indices <- grep(marker, lines)

  # Calculate the number of chunks
  num_chunks <- ceiling(length(chapter_indices) / num_chapters)
  numDigits= nchar(length(chapter_indices))

  # Initialize an empty list to store the chunks
  chunks <- vector("list", length = num_chunks)
  chapters_in_chunks <- vector("list", length = num_chunks)
  chapter_index = matrix(0,num_chunks,2)

  # Iterate over each chunk
  for (i in 1:num_chunks) {
    # Determine the start and end chapter indices for the current chunk
    (start_chapter_index <- (i - 1) * num_chapters + 1)
    (end_chapter_index <- min(start_chapter_index + num_chapters -1 , length(chapter_indices)))
    chapter_index[i,1]=start_chapter_index
    chapter_index[i,2]=end_chapter_index
    # Determine the start and end indices for the current chunk
    # start_index is the index of the start_chapter
    (start_index = chapter_indices[start_chapter_index])
    # end_index is last line if its the last chunk, otherwise it is the index of the next_start_chapter
    if(i == num_chunks){
      end_index=length(lines)
    }
    else{
      next_start_chapter_index <- i * num_chapters + 1
      end_index = chapter_indices[next_start_chapter_index]
    }

    # Extract lines for the current chunk
    chapter_lines <- lines[chapter_indices[start_chapter_index:end_chapter_index]]
    chunk_lines <- lines[start_index:end_index]
    # Store the chunk in the list
    chapters_in_chunks[[i]] <- chapter_lines
    chunks[[i]] <- chunk_lines
  }
  chapter_index<-
    chapter_index%>%as.data.frame()%>%
    dplyr::mutate_all(~stringr::str_pad(.,width=numDigits,side = 'left',pad='0'))
  # Write each chunk to a separate text file
  for (i in 1:num_chunks) {
    chunk_file_name = paste0(name,'|',paste0(chapter_index[i,],collapse = '-'),".txt")
    chunk_file_name
    writeLines(chunks[[i]],chunk_file_name)
  }
}
