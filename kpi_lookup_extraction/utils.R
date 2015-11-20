# R version during development:
# > R.version.string
# [1] "R version 3.2.0 (2015-04-16)"
#
# 32bit R used during development:
# > .Machine$sizeof.pointer
# [1] 4

## Helpful util functions

check_files <- function(f, ext) {
  ## checks file:
  #   - datatype and length
  #   - extension
  #   - existence
  if(!is.character(f) | length(f) != 1) {
    stop(paste0("Parameter f should be a character datatype and have length = 1. Intead, f:\n\n", f))
  }
  if(!is.character(ext) | length(ext) != 1) {
    stop(paste0("Parameter ext should be a character datatype and have length = 1. Intead, ext:\n\n", ext))
  }
  if(!grepl(paste0(".*\\.", ext, "$"), f)) {
    stop(paste0("File \"", f, "\" does not have expected \".", ext,"\" extension."))
  }
  if(!file.exists(f)) {
    stop(paste0("File \"", f, "\" not found. Please input valid file path name."))
  }
}

varcheck_w_fun_len <- function(x, fun, len, ...) {
  (fun(x, ...) & length(x)==len)
}

rm_htspc <- function(df) {
  if(!is.data.frame(df)) stop("rm_htspc function only accepts data.frame arg.")
  #remove header and trailing spaces for char and factor columns in data.frames
  for(col in names(df)) {
    colWasFactor <- class(df[, col]) == "factor"
    if (class(df[, col]) == "character" | colWasFactor) {
      df[, col] <- sub("^\\s+", "", df[, col])
      df[, col] <- sub("\\s+$", "", df[, col])
      if(colWasFactor) df[, col] <- as.factor(df[, col])
    }
  }
  
  df
}

chk_empty <- function(obj) { 
  if(length(obj) > 1) stop("chk_empty function only accepts obj arg with length <= 1")
  if(is.null(obj) || obj == "") NA else obj
}