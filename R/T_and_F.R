#' Fix T and F Directory
#'
#' @param path folder to clean
#'
#' @return invisible NULL
#' @export
#'
#' @examples
#' # TODO
fix_dir_T_and_F <- function(path = 'R') {
  files <- fs::dir_ls(path, glob = '*.R')

  for (f in files) {
    fix_T_and_F(f)
  }

  invisible(NULL)
}

#' Fix T and F Files
#'
#' @param path file to clean
#'
#' @return invisble NULL
#' @export
#'
#' @examples
#' # TODO
fix_T_and_F <- function(path) {
  if (!fs::file_exists(path)) {
    stop('`path` does not point to a file.')
  }

  lines <- readr::read_lines(path)

  patts <- T_F_patts()
  repls <- T_F_repls()

  for (i in seq_along(patts)){
    lines <- stringr::str_replace_all(lines, patts[i], repls[i])
  }

  readr::write_lines(lines, file = path)

  invisible(NULL)
}

T_F_patts <- function() {
  c('=F\\)', '= F\\)',
    '=F\\]', '= F\\]',
    '=F,', '= F,',
    '=F\\}', '= F\\}',
    '=T\\)', '= T\\)',
    '=T\\]', '= T\\]',
    '=T,', '= T,',
    '=T\\}', '= T\\}'
  )
}

T_F_repls <- function() {
  c('= FALSE\\)', '= FALSE\\)',
    '= FALSE\\]', '= FALSE\\]',
    '= FALSE,', '= FALSE,',
    '= FALSE\\}', '= FALSE\\}',
    '= TRUE\\)', '= TRUE\\)',
    '= TRUE\\]', '= TRUE\\]',
    '= TRUE,', '= TRUE,',
    '= TRUE\\}', '= TRUE\\}'
  )
}
