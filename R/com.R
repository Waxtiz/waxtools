#' Boolean function to check if a number is even or odd
#'
#' @param nchartext is an integer vector.
#'
nchar_is_even <- function(nchartext) {
  if((nchartext %% 2) == 0) {
    TRUE
  } else {
    FALSE
  }
}

#' Defines the style of a level 3 title - default version
#'
#' @param text is a title
#' @param even is a boolean. the text has an even number of letters (TRUE). The text has an odd number of letters (FALSE)
#' @param nchartext is an integer vector. Is the number of characters in the title.
#' @param max_length is an integer vector. Corresponds to the size that will be generated.
#' @param char is fill character. "#" by default.
#'
#' @importFrom glue glue


H3 <- function(text, even = T, nchartext, max_length, char) {
  nchar_with_even_odd <- ifelse(even, 1, 0)

  glue(
    strrep(char, (max_length/2)-(nchartext/2)-1),
    " ",
    text,
    " ",
    strrep(char, (max_length/2)-(nchartext/2)-nchar_with_even_odd)
  )
}

#' Defines the style of a level 2 title - full version
#'
#' @param text is a title
#' @param even is a boolean. the text has an even number of letters (TRUE). The text has an odd number of letters (FALSE)
#' @param nchartext is an integer vector. Is the number of characters in the title.
#' @param max_length is an integer vector. Corresponds to the size that will be generated.
#' @param char is fill character. "#" by default.
#'
#' @importFrom glue glue
H2_full <- function(text, even = T, nchartext, max_length, char) {
  nchar_with_even_odd <- ifelse(even, 1, 0)

  glue::glue(
    strrep(char, max_length),"\n",
    strrep(char, (max_length/2)-(nchartext/2)-1),
    " ",
    text,
    " ",
    strrep(char, (max_length/2)-(nchartext/2)-nchar_with_even_odd),"\n",
    strrep(char, max_length)
  )
}

#' Defines the style of a level 2 title - simple version
#'
#' @param text is a title
#' @param even is a boolean. the text has an even number of letters (TRUE). The text has an odd number of letters (FALSE)
#' @param nchartext is an integer vector. Is the number of characters in the title.
#' @param max_length is an integer vector. Corresponds to the size that will be generated.
#' @param char is fill character. "#" by default.
#'
#' @importFrom glue glue
H2_simple <- function(text, even = T, nchartext, max_length, char) {
  nchar_with_even_odd <- ifelse(even, 1, 0)

  glue::glue(
    strrep(char, max_length),"\n",
    char,
    strrep(" ", (max_length/2)-(nchartext/2)-1),
    text,
    strrep(" ", (max_length/2)-(nchartext/2)-nchar_with_even_odd),
    char,"\n",
    strrep(char, max_length)
  )
}


#' Defines the style of a level 2 title - default version
#'
#' @param text is a title
#' @param even is a boolean. the text has an even number of letters (TRUE). The text has an odd number of letters (FALSE)
#' @param nchartext is an integer vector. Is the number of characters in the title.
#' @param max_length is an integer vector. Corresponds to the size that will be generated.
#' @param char is fill character. "#" by default.
#'
#' @importFrom glue glue
H2 <- function(text, even = T, nchartext, max_length, char) {
  nchar_with_even_odd <- ifelse(even, 1, 0)

  glue::glue(
    strrep(char, max_length),"\n",
    strrep(char, 3),
    strrep(" ", (max_length/2)-(nchartext/2)-3),
    text,
    strrep(" ", (max_length/2)-(nchartext/2)-nchar_with_even_odd-2),
    strrep(char, 3),"\n",
    strrep(char, max_length)
  )
}




#' Defines the style of a level 1 title - default version
#'
#' @param text is a title
#' @param even is a boolean. the text has an even number of letters (TRUE). The text has an odd number of letters (FALSE)
#' @param nchartext is an integer vector. Is the number of characters in the title.
#' @param max_length is an integer vector. Corresponds to the size that will be generated.
#' @param char is fill character. "#" by default.
#'
#' @importFrom glue glue
H1 <- function(text, even = T, nchartext, max_length, char) {
  nchar_with_even_odd <- ifelse(even, 1, 0)

  glue::glue(
    strrep(char, max_length),"\n",
    strrep(char, 3), strrep(" ", max_length-6), strrep(char, 3),"\n",
    strrep(char, 3),
    strrep(" ", (max_length/2)-(nchartext/2)-3),
    text,
    strrep(" ", (max_length/2)-(nchartext/2)-nchar_with_even_odd-2),
    strrep(char, 3),"\n",
    strrep(char, 3),, strrep(" ", max_length-6), strrep(char, 3),"\n",
    strrep(char, max_length)
  )
}

#' Defines the style of a level 4 title - default version
#'
#' @param text is a title
#' @param even is a boolean. the text has an even number of letters (TRUE). The text has an odd number of letters (FALSE)
#' @param nchartext is an integer vector. Is the number of characters in the title.
#' @param max_length is an integer vector. Corresponds to the size that will be generated.
#' @param char is fill character. "#" by default.
#'
#' @importFrom glue glue
H4 <- function(text, even = T, nchartext, max_length, char) {
  nchar_with_even_odd <- ifelse(even, 1, 0)

  glue::glue(
    strrep(" ", (max_length/2)-(nchartext/2)-3),
    char, "  ",
    text,
    "  ", char,
    strrep(" ", (max_length/2)-(nchartext/2)-nchar_with_even_odd-2)
  )
}


#' Defines the style of a level 4 title - default version
#'
#' @param text is a title
#' @param even is a boolean. the text has an even number of letters (TRUE). The text has an odd number of letters (FALSE)
#' @param nchartext is an integer vector. Is the number of characters in the title.
#' @param max_length is an integer vector. Corresponds to the size that will be generated.
#' @param char is fill character. "#" by default.
#'
#' @importFrom glue glue
H5 <- function(text, even = T, nchartext, max_length, char) {
  nchar_with_even_odd <- ifelse(even, 1, 0)
  max_length <- max_length/2

  glue::glue(
    strrep(char, (max_length/2)-(nchartext/2)-1),
    " ",
    text,
    " ",
    strrep(char, (max_length/2)-(nchartext/2)-nchar_with_even_odd)
  )
}




#' Defines the style of a level 4 title - default version
#'
#' @param text is a title
#' @param heading is the desired heading level. Levels are inspired by HTML titles.
#' @param ide is the current integrated development environment. Used to set title width. Only Pycharm & Rstudio are currently supported.
#' @param char is fill character. "#" by default.
#'
#' @importFrom glue glue
#' @importFrom crayon blue
#' @importFrom utils writeClipboard
#'
#' @export
com <- function(text = NULL, heading = "h3", ide = "pycharm", char = "#") {
  if (ide == "pycharm") {
    max_length <- 120
  } else if (ide == "rstudio") {
    max_length <- 80
  } else {
    stop("The 'ide' parameter is probably wrong. For now, only 'pycharm' or 'rstudio' IDEs are supported. Please enter one of these values in the 'ide' option.")
  }

  if (is.null(text)) {
    com <- glue(strrep("#", max_length))
  } else {
    text <- as.character(text)
    nchartext <- nchar(text)
    nchar_is_even_text <- nchar_is_even(nchartext)

    if (heading == "h3" | heading == 3) {
      com <- H3(text, nchar_is_even_text, nchartext, max_length, char)
    } else if (heading == "h2" | heading == 2) {
      com <- H2(text, nchar_is_even_text, nchartext, max_length, char)
    } else if (heading == "h1" | heading == 1) {
      com <- H1(text, nchar_is_even_text, nchartext, max_length, char)
    } else if (heading == "h2_simple") {
      com <- H2_simple(text, nchar_is_even_text, nchartext, max_length, char)
    } else if (heading == "h2_full") {
      com <- H2_full(text, nchar_is_even_text, nchartext, max_length, char)
    } else if (heading == "h4" | heading == 4) {
      com <- H4(text, nchar_is_even_text, nchartext, max_length, char)
    } else if (heading == "h5" | heading == 5) {
      com <- H5(text, nchar_is_even_text, nchartext, max_length, char)
    } else {
      stop("The 'heading' parameter is probably wrong. Please, choose a heading level between 'h1' to 'h5'.")
    }
  }
  writeClipboard(com)
  message(blue("\nYour comment has been successfully copied to the clipboard.\n"))
}