# R version during development:
# > R.version.string
# [1] "R version 3.2.0 (2015-04-16)"
#
# 32bit R used during development:
# > .Machine$sizeof.pointer
# [1] 4
#
# packages required other than base rtools:
# install.packages("dplyr")
#
# package versions during script dev:
# > packageVersion("dplyr")
# [1] ‘0.4.2’

##############################################
# Source dependencies
##############################################
source("utils.R", local = T)
require(dplyr)
##############################################

#kpi sub patterns (perl = T, ignore.case = T):
kpi_expr_rplc_pat <- rbind(
  cbind(c("\\s*",
          "/100$",
          "100\\*",          
          "\\*100(?!\\.001)",
          "\\[",
          "\\]"
          #"\\((?![\\d]+\\*[\\d\\*]+\\))",
          #"(?!\\([\\d]+\\*[\\d\\*]+)\\)",
          #"^1-"
  ), "")) 

kpi_expr_mapped_rplc_pat <- rbind(
  cbind(c("\\sas\\s.+",
          "^.*-\\>",    #e.g.: "Traffic in GB->"
          "\\s*",
          "\\+0?\\.[0]+1",
          "\\.GSM",
          "\\.UMTS",
          "\\.LTE"), ""),
  kpi_expr_rplc_pat,
  cbind(c("(Sum\\(|sum\\(|SUM\\()",
          "(Round\\(|round\\(|ROUND\\()"), "("),
  c("([\\(\\)[:alpha:]])\\*100\\.0+([[:punct:][:alpha:]])", "\\1\\2"),
  c(",\\d+\\)", ")"))

#udc sub patterns
udc_field_mapped_rplc_pat <- rbind(
  cbind(c("\\s*",
          "100\\*",
          "\\{",
          "\\}",
          "\\[",
          "\\]"), ""),
  c("!", "."))

make_udc_df <- function(inputfile) { 
  addRows <- function(df) {
    # load and read xml
    require(XML)
    rootnode <-xmlRoot(xmlTreeParse(inputfile))
    node <- lapply(rootnode[], xmlToList)
    
    for (i in which(names(node) == "udc-data")) {       
      df <- rbind(df, data.frame(udc_field = node[[i]][["field-name"]],
                                 udc_field_mapped = node[[i]][["expression"]],
                                 stringsAsFactors=F))
    }
    df
  }
  
  df <- data.frame(udc_field = character(),
                   udc_field_mapped = character(),
                   stringsAsFactors = F)
  
  df <- addRows(df)
  
  rm_htspc(df)
}

##clean kpi_expr/kpi_mapped/udc_expr strings
#transform ignore_case
transform_str_col <- function(df, rplc_pat, str_col) {
  df[, paste0(str_col, "_trans")] <- df[, str_col]
  for(pair in as.list(as.data.frame(t(rplc_pat), stringsAsFactors = F))) {
    df[, paste0(str_col, "_trans")] <- mapply(gsub, pair[1], pair[2], df[, paste0(str_col, "_trans")], perl = T, ignore.case = T)
  }
  df
}

get_fields <- function(s) {
  fields <- unlist(strsplit(s, "[\\+\\-\\*/\\(\\)]", perl = T))
  fields[which(fields != "")]
}

get_ops_brkts <- function(s) {
  ops_brkts <- unlist(strsplit(s, "[\\w\\.]+", perl = T))
  ops_brkts[which(ops_brkts != "")]
}

rm_parentheses <- function(s) {
  s <- gsub("\\(", "", s)
  gsub("\\)", "", s)
}

stitch_fields_ops_brkts <- function(fields, ops_brkts) {
  fields_len <- length(fields)
  ops_brkts_len <- length(ops_brkts)
  if(ops_brkts_len == 0) {
    fields
  } else if(ops_brkts_len == fields_len) {
    if(grepl("^\\(", ops_brkts[1])) paste0(ops_brkts, fields, collapse = "")
    else paste0(fields, ops_brkts, collapse = "")
  } else {
    vlong <- if(ops_brkts_len > fields_len) ops_brkts else fields  
    vshort <- c(if(ops_brkts_len < fields_len) ops_brkts else fields, "")
    paste0(vlong, vshort, collapse = "")
  }
}

split_n_list <- function(split_fun, s_v) { lapply(as.list(s_v), split_fun) }

collapse_vect <- function(s_v_l, sep) { sapply(s_v_l, paste0, collapse = sep) }

eval_strs_w_1s <- function(xfields_l, ops_brkts_l) {
  lapply(
    mapply(stitch_fields_ops_brkts,
           sapply(lapply(xfields_l, length), function(x) {
             if(is.na(x) | x == 0) 0
             else rep(1,x)}),
           ops_brkts_l),
    function(x) {
      tryCatch({ c(x, eval(parse(text=x))) },
               error = function(err) { if(grepl("Error in parse",as.character(err))) NA else stop(err) })
    })
}

compare_expr <- function(udc_df, kpi_df) {
  ## Compare kpi_df$kpi_expr_trans and kpi_df$kpi_expr_mapped_trans expressions
  #   1. Create kpi_df$udc_expr_mapped column: 
  #       - replace field names in kpi_df$kpi_expr_trans with mapped field names by referencing udc_df table
  #   2. Create kpi_df$kpi_xfield_mapped and kpi_df$udc_xfield_mapped columns:
  #       - comma separated expr fields extracted from kpi_df$kpi_expr_mapped_trans and kpi_df$udc_expr_mapped respectively
  #   3. Compare kpi_df$kpi_xfield_mapped and kpi_df$udc_xfield_mapped and store results in kpi_df$xfield_mapped_match
  #   4. Create kpi_df$kpi_expr_mapped_ops and kpi_df$udc_expr_mapped_ops columns:
  #       - expr operators(brackets removed) extracted from kpi_df$kpi_expr_mapped_trans and kpi_df$udc_expr_mapped respectively
  #   5. Compare kpi_df$kpi_expr_mapped_ops and kpi_df$udc_expr_mapped_ops and store results in kpi_df$expr_mapped_ops_match
  #   6. Create kpi_df$kpi_expr_mapped_eval and kpi_df$udc_expr_mapped_eval columns:
  #       - replace all fields names in kpi_df$kpi_expr_mapped_trans and kpi_df$udc_expr_mapped respectively with "1"
  #   7. Compare kpi_df$kpi_expr_mapped_eval and kpi_df$udc_expr_mapped_eval calculated expressions and store results in
  #      kpi_df$expr_mapped_eval_match
  #   8. Consolidate results in kpi_df$kpi_udc_expr_mapped_match by evaluating (kpi_df$xfield_mapped_match &
  #      kpi_df$expr_mapped_ops_match & kpi_df$expr_mapped_eval_match)
  
  kpi_df$udc_expr_mapped <- sapply(as.list(kpi_df$kpi_expr_trans), function(s) {   
    #   - split kpi_expr_trans string to extract fields
    #   - split kpi_expr_trans string to extract ops_brkts
    #   - match fields with udc_df$udc_field to get respective udc_df$udc_field_mapped
    #   - stitch mapped fields with ops_brkts and add to column kpi_df$udc_expr_mapped
    fields <- get_fields(s)
    ops_brkts <- get_ops_brkts(s)
    fields <- paste0("(", sapply(fields, function(x) {
      ifelse(grepl(".*_.*", x), udc_df[udc_df$udc_field == x, "udc_field_mapped_trans"], x)
    }), ")")
    stitch_fields_ops_brkts(fields, ops_brkts)
  })
  
  #   - split kpi_df$kpi_expr_mapped_trans, stitch with comma and add to kpi_df$kpi_xfield_mapped
  #   - split kpi_df$udc_expr_mapped, stitch with comma and add to kpi_df$udc_xfield_mapped
  #   - create column kpi_df$xfield_mapped_match
  kpi_xfield_mapped_l <- split_n_list(get_fields, kpi_df$kpi_expr_mapped_trans)
  udc_xfield_mapped_l <- split_n_list(get_fields, kpi_df$udc_expr_mapped)  
  kpi_df$kpi_xfield_mapped <- collapse_vect(kpi_xfield_mapped_l, ",")
  kpi_df$udc_xfield_mapped <- collapse_vect(udc_xfield_mapped_l, ",")
  
  kpi_df$xfield_mapped_match <- kpi_df$kpi_xfield_mapped == kpi_df$udc_xfield_mapped
  
  #   - split kpi_df$kpi_expr_mapped_trans to extract kpi_expr_mapped_ops_brkts_l
  #   - split kpi_df$udc_expr_mapped to extract udc_expr_mapped_ops_brkts_l
  #   - extract ops from kpi_expr_mapped_ops_brkts_l into column kpi_df$kpi_expr_mapped_ops
  #   - extract ops from udc_expr_mapped_ops_brkts_l into column kpi_df$udc_expr_mapped_ops
  #   - create column kpi_df$expr_mapped_ops_match
  kpi_expr_mapped_ops_brkts_l <- split_n_list(get_ops_brkts, kpi_df$kpi_expr_mapped_trans)
  udc_expr_mapped_ops_brkts_l <- split_n_list(get_ops_brkts, kpi_df$udc_expr_mapped)
  kpi_df$kpi_expr_mapped_ops <- rm_parentheses(collapse_vect(kpi_expr_mapped_ops_brkts_l, ""))
  kpi_df$udc_expr_mapped_ops <- rm_parentheses(collapse_vect(udc_expr_mapped_ops_brkts_l, ""))
  
  kpi_df$expr_mapped_ops_match <- kpi_df$kpi_expr_mapped_ops == kpi_df$udc_expr_mapped_ops
  
  #   - stitch "1" with alternating kpi_expr_mapped_ops_brkts_l, then eval and store in kpi_df$kpi_expr_mapped_eval,
  #     e.g.: eval(parse(text="(1+1+1)*1/1+1"))
  #   - stitch "1" with alternating udc_expr_mapped_ops_brkts_l, then eval and store in kpi_df$udc_expr_mapped_eval
  #   - create column kpi_df$expr_mapped_eval_match 
  kpi_expr_mapped_eval <- as.data.frame(do.call(rbind, eval_strs_w_1s(kpi_xfield_mapped_l, kpi_expr_mapped_ops_brkts_l)), 
                                        row.names = F, stringsAsFactors = F)
  udc_expr_mapped_eval <- as.data.frame(do.call(rbind, eval_strs_w_1s(udc_xfield_mapped_l, udc_expr_mapped_ops_brkts_l)), 
                                        row.names = F, stringsAsFactors = F)
  
  kpi_df$kpi_expr_mapped_eval <- kpi_expr_mapped_eval[, 1]
  kpi_df$udc_expr_mapped_eval <- udc_expr_mapped_eval[, 1]
  kpi_df$expr_mapped_eval_match <- kpi_expr_mapped_eval[, 2] == udc_expr_mapped_eval[, 2]
  
  #   - Create column kpi_df$kpi_udc_expr_mapped_match
  kpi_df$kpi_udc_expr_mapped_match <-
    mapply (function(m1, m2, m3) {
      m <- c(m1, m2, m3)
      if(any(is.na(m))) NA
      else all(m)
    }, kpi_df$xfield_mapped_match, kpi_df$expr_mapped_ops_match, kpi_df$expr_mapped_eval_match)
  
  kpi_df
}