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

#bu node paths
qItem_nodepaths <- list(c("namespace", "folder", "querySubject"))
qItem_nodepaths_byName <- list(list(c("namespace", "^(Huawei|Ericsson|ALU) KPIs$"),
                                    c("folder", "^Hourly KPIs$"),
                                    c("querySubject", NA),
                                    c("queryItem", NA)),
                               list(c("namespace", "^(Huawei|Ericsson|ALU) KPIs$"),
                                    c("folder", "^Hourly KPIs$"),
                                    c("querySubject", NA),
                                    c("queryItemFolder", NA),
                                    c("queryItem", NA)))
#qItem_nodepaths_byName <- list(data.frame(path = c("namespace", "folder", "querySubject"),
#                                          name = c("Huawei KPIs", "Hourly KPIs", NA)))


if (FALSE) {
  # Old paths
  qItem_nodepaths <- list(c("querySubject", "queryItem"),
                          c("namespace", "folder", "querySubject", "queryItem"),
                          c("namespace", "folder", "querySubject", "queryItemFolder", "queryItem"))
}

#dim node paths
measure_nodepaths <- list(c("dimension", "measure"),
                          c("dimension", "measureFolder","measure"))

get_layer <- function(idx, layerName, rootnode) {
  #root[["namespace"]][[6]][["name"]][[1]]$value
  nspc <- rootnode[["namespace"]][[idx]]
  if(is.null(nspc[["name"]][[1]]$value) | !grepl(layerName, nspc[["name"]][[1]]$value))
    stop(paste0("get_layer: namespace idx does not match layername."))
  lapply(nspc[], xmlToList)
}

addRows_byName <- function(df, node, path_byName, fun, lastNode=NULL, topNamespace=NULL) {
  if(length(path_byName) > 0) {
    node_idx <- which(names(node) == path_byName[[1]][1])
    if(length(node_idx) != 0) {
      for(i in node_idx) {
        if(is.na(path_byName[[1]][2]) | grepl(path_byName[[1]][2], sub("\\s*\\(en-in\\)\\s*", "", node[[i]]$name$text))) {
          #print(paste0("i: ", i))
          df <- addRows_byName(df, node[[i]], path_byName[-1], fun,
                               if(toupper(sub("\\(en-in\\)\\s*", "", node$name$text))=="COUNTERS") lastNode else node,
                               if(is.null(topNamespace)) node[[i]]$name$text else topNamespace)
        }  
      }
    } 
    df
  } else {
    fun(df, node, lastNode, topNamespace)
  }
}

addRows <- function(df, node, path, fun, lastNode=NULL) {
  if(length(path) > 0) {
    node_idx <- which(names(node) == path[1])
    if(length(node_idx) != 0) {
      for(i in node_idx) {
        #print(paste0("i: ", i))
        df <- addRows(df, node[[i]], path[-1], fun,
                      if(toupper(sub("\\(en-in\\)\\s*", "", node$name$text))=="COUNTERS") lastNode else node)
      }
    } 
    df
  } else {
    fun(df, node, lastNode)
  }
}

chk_empty <- function(obj) { 
  if(length(obj) > 1) stop("chk_empty function only accepts obj arg with length <= 1")
  if(is.null(obj) || obj == "") NA else obj
}

make_model_df <- function(layer, nodepaths, model) {
  if(!is.list(nodepaths)) stop("make_model_df: nodepaths is not a list class object!")
  #print(paste0("model: ", model))
  extract_expr_smry <- function(n) {
    expr_pat <- "^\\[.*\\]\\.\\[(.+)\\].\\[.+\\]"
    for(e in n[["expression"]]) {
      if(grepl(expr_pat,e, perl=T)) {
        return(sub(".* Copy of ",
                   "",
                   sub(expr_pat,"\\1",e)))
      }
    }
    NA
  }
  
  bind_rws_2_model_df <- function(df, node, lastNode) {
    if(is.null(lastNode)){ stop("lastNode is null.")}
    
    #will not add rows for:
    # - node$expression that contains "Data Layer"
    # - lastNode$name$text in summary_name_2_rm list
    summary_name_2_rm <- c("Cell 2G",
                           "Cell 3G",
                           "Cell 4G",
                           "Time",
                           "Etisalat Configuration Table")
    if((sum(grepl("Data Layer", node$expression)) > 0) |
         (lastNode$name$text %in% summary_name_2_rm)) return(df)
    #print(paste0("node_name: ", node$name$text))
    
    rbind(df, data.frame(full_summary_name = chk_empty(lastNode$name$text),
                         measure_name = chk_empty(node$name$text),
                         expr_smry_name = extract_expr_smry(node),
                         #sub on node$expression extracts the expr after the last dot
                         #e.g.: from "[Huawei].[Huawei_BSC_RNC_OFFICE_Daily].[2G Assignment Success Rate - Request]"
                         #      to "2G Assignment Success Rate - Request"
                         full_expr = chk_empty(paste0(node$expression, collapse='')),
                         expr = chk_empty(paste0(sub(".*\\.\\[(.*)\\]$","\\1", node$expression), collapse='')),
                         regAgg = chk_empty(node$regularAggregate),
                         format = chk_empty(node$format),
                         stringsAsFactors=F))
  }
  
  clean_data <- function(df) {
    #remove rows with measure_name in measure_name_2_rm
    measure_name_2_rm <- c("TSTAMP",
                           ".*_ID",
                           "DA_.*")
    for(toRm in measure_name_2_rm) {
      df <- subset(df, !grepl(toRm, measure_name))
    }
    
    #remove all "(en-in)" in measure_name, full_summary_name, bu_expr and dim_expr
    for(col in c("measure_name", "full_summary_name", "expr_smry_name", "expr")) {
      df[, col] <- gsub("\\s*\\(en-in\\)\\s*", "", df[, col])
    }
    
    #split format column
    extracted_mf <- sub("&lt;formatGroup&gt;&lt;(.*)Format xml:lang=&quot;en-us&quot; (.*)/&gt;&lt;/formatGroup&gt;",
                        "\\1 \\2",
                        df$format)
    df[, c("format", "precision")] <- do.call(rbind, strsplit(extracted_mf, " "))
    df$precision <- sub(".*&quot;(\\d)&quot;", "\\1", df$precision)
    #Deals with format with no precision
    #TODO: too ugly =S must change
    if(length(df[!is.na(df$format) & !is.na(df$precision) & df$format==df$precision,]$precision)>0) {
      df[!is.na(df$format) & !is.na(df$precision) & df$format==df$precision,]$precision <- NA
    }
    
    ## uppercase columns
    upperCols <- c("full_summary_name", "measure_name", "expr_smry_name", "expr", "regAgg", "format")
    df[, upperCols] <- data.frame(sapply(df[, upperCols], toupper),
                                  stringsAsFactors=F)
    
    #remove all spaces in regAgg column values 
    df$regAgg <- gsub("\\s", "", df$regAgg)
    
    #standardise and split full_summary_name
    df$expr_smry_name <- with(df, mapply(sub, "NEW DIMENSION\\d*", full_summary_name, expr_smry_name)) #replace "New Dimension\\d*"      
    df$full_summary_name <- gsub(" ", "_", df$full_summary_name)
    for (toReplace in list(c("_HSM_V$", "_HOURLY"),
                           c("_DSM_V$", "_DAILY"),
                           c("HUAWEI_", "ETI_HUAMA_"),
                           c("ALU_", "ETI_ALUMA_"),
                           c("ERICSSON_", "ETI_ERIMA_"),
                           c("EUTRAN", "EUT"),
                           c("CELL_AVAILABILITY_AND_USAGE", "CELLAVUS"),
                           c("CELL_RETAINABILITY_AND_MOBILITY", "CELLREMO"),
                           c("_FEMTO", "F"))) {
      df$full_summary_name <- sub(toReplace[1], toReplace[2], df$full_summary_name)
    }
    df[, c("summary_name", "granularity")] <- do.call(rbind, strsplit(sub("(.*)_(DAILY|HOURLY)$", "\\1@\\2", df$full_summary_name), "@"))
    df[df$granularity != "DAILY" & df$granularity != "HOURLY", "granularity"] <- NA
    
    rm_htspc(df)
  }
  
  ##############################################
  # Data frame creation and process steps:
  # 1. Add data into a data.frame
  # 2. Name columns
  # 3. Return data.frame
  ##############################################
  
  df <- data.frame(full_summary_name = character(),
                   measure_name = character(),
                   expr_smry_name = character(),
                   full_expr = character(),
                   expr = character(),
                   regAgg = character(),
                   format = character(),
                   stringsAsFactors=F)
  for (np in nodepaths) {
    #print(paste0("np: ", paste0(np, collapse = ".")))
    df <- addRows(df, layer, np, bind_rws_2_model_df)
  }
  df <- clean_data(rm_htspc(df))    
  names(df) <- c("full_summary_name",
                 "measure_name",
                 paste0(model, "_expr_smry_name"),
                 paste0(model, "_full_expr"),
                 paste0(model, "_expr"),
                 paste0(model, "_regAgg"),
                 paste0(model, "_format"),
                 paste0(model, "_precision"),
                 "summary_name",
                 "granularity")
  df
}

prep_kpi_df <- function(raw_kpi_df, vendor) {
  if(!(vendor %in% c("Huawei", "Ericsson", "ALU"))) stop("vendor arg should only contain characters 'Huawei' or 'Ericsson' or 'ALU'.")
  
  df <- raw_kpi_df[, c("KPI_Name..used.in.model.",
                       ifelse(vendor == "Huawei",  "Summary.Table", paste0(vendor, ".Summary.Table")),
                       "Format",
                       "Decimal.Places",
                       paste0(vendor, ".Cognos.Formula"),
                       paste0(vendor, ".Cognos.Aggregation"))]
  names(df) <- c("measure_name",
                 "summary_name",
                 "kpi_format",
                 "kpi_precision",
                 "kpi_expr",
                 "kpi_regAgg")
  # upper case column values
  df[-4] <- data.frame(sapply(df[-4], toupper), stringsAsFactors=F)
  
  #remove all spaces from regAgg column
  df$kpi_regAgg <- gsub("\\s", "", df$kpi_regAgg)
  
  rm_htspc(df)
}

make_support_df <- function(merged_df) {
  ## support_df
  # 1. transform all NUMERATOR/DENOMINATOR to NUM/DEN in measure_name and dim_expr column
  #
  # 2. add columns group, obj_type and *_expr_trans
  #
  # 3. bu_regAgg vs dim_regAgg comparison criteria: "UNSUPPORTED" is equal to "CALCULATED".
  #    Using support_df to replace "UNSUPPORTED" with "CALCULATED" in bu_regAgg and dim_regAgg columns to
  #    ease bu vs dim comparison. 
  #
  # 4. Value "PERCENT" in bu_format and dim_format columns is equal to "%" in kpi_format column
  #    Using support_df to replace "PERCENT" with "%" in bu_format and dim_format columns to ease kpi vs bu
  #    and kpi vs dim comparison
  #
  # 5. split kpi_regAgg column. E.g.: kpi_regAgg = "SUM/SUM=AVG" will be split to 3 columns:
  #     - column kpi_regAgg_NUM = "SUM"     #part of string before "/"
  #     - column kpi_regAgg_DEN = "SUM"     #part of the string after "/" and before "="
  #     - column kpi_regAgg_PARENT = "CALCULATED"  #part of the string after "=" if "AVG" will be replaced by "CALCULATED"
  #     - For kpi_regAgg values such as “SUM/CONSTANT=AVG” for obj_type = “DISTINCT”, part of string before “/” will be used
  #       for comparison. E.g.:"SUM” from kpi_regAgg value “SUM/CONSTANT=AVG” would be used for comparison
  tryCatch({
    support_df <- merged_df
    
    # transform all NUMERATOR/DENOMINATOR to NUM/DEN in measure_name and dim_expr column
    support_df$measure_name <- sub("NUMERATOR", "NUM", support_df$measure_name)
    support_df$measure_name <- sub("DENOMINATOR", "DEN", support_df$measure_name)
    support_df$dim_expr <- sub("NUMERATOR", "NUM", support_df$dim_expr)
    support_df$dim_expr <- sub("DENOMINATOR", "DEN", support_df$dim_expr)
    
    ## Adding group column
    support_df$group <- NA
    support_df[grepl("\\s*[_-]\\s*DEN",support_df$measure_name) | grepl("\\s*[_-]\\s*NUM", support_df$measure_name), "group"] <-
      filter(support_df, grepl("\\s*[_-]\\s*DEN", measure_name) | grepl("\\s*[_-]\\s*NUM", measure_name)) %>%
      mutate(group = paste0(full_summary_name, "@", sub("(.*\\S)\\s*[_-].*", "\\1", measure_name))) %>%
      select(group)   
    support_df$group <- as.factor(support_df$group)   
    support_df[is.na(support_df$group) & paste0(support_df$full_summary_name, "@", support_df$measure_name) %in% levels(support_df$group), "group"] <- 
      filter(support_df, is.na(group), paste0(full_summary_name, "@", measure_name)  %in% levels(group)) %>%
      mutate(group = paste0(full_summary_name, "@", measure_name)) %>%
      select(group)
    
    ## Adding obj_type column PARENT/DEN/NUM/UNHANDLED/DISTINCT
    support_df$obj_type <- "DISTINCT"
    support_df[!is.na(support_df$group), "obj_type"] <- "PARENT"
    support_df[grepl("\\s*[_-]\\s*DEN$",support_df$measure_name), "obj_type"] <- "DEN"
    support_df[grepl("\\s*[_-]\\s*NUM$", support_df$measure_name), "obj_type"] <- "NUM"
    # unhandled <- (table(support_df$group)%>%as.data.frame%>%filter(Freq!=3))$Var1%>%as.character%>%unique
    unhandled <- 
      c((xtabs(~ group + as.factor(obj_type), data=support_df, drop.unused.levels=T) %>%
           as.data.frame %>%
           filter(Freq!=1))$group %>%
          as.character,
        (table(support_df$group) %>%
           as.data.frame %>%
           filter(Freq!=3))$Var1 %>%
          as.character) %>% unique   
    if_grp <- support_df[support_df$obj_type == "PARENT" &
                           (grepl("^IF\\s+\\(", support_df$bu_expr) |
                              grepl("^IF\\s+\\(", support_df$dim_expr)), "group"]
    support_df[support_df$group %in% c(unhandled, as.character(if_grp)), "obj_type"] <- "UNHANDLED"
    
    ## Adding bu_expr_trans, dim_expr_trans and kpi_expr_trans column  
    for(col in c("bu_expr", "dim_expr", "kpi_expr")) {
      m_expr_trans <- paste0(col, "_trans")
      support_df[, m_expr_trans] <- support_df[, col]
      support_df[support_df$obj_type != "PARENT", m_expr_trans] <- NA
      support_df[, m_expr_trans] <- sub("^100\\s*\\*\\s*", "", support_df[, m_expr_trans])
      support_df[, m_expr_trans] <- sub("\\)\\s*/\\s*100\\s*$", "", support_df[, m_expr_trans])
      support_df[, m_expr_trans] <- sub("^\\(\\s*(\\S)", "\\1", support_df[, m_expr_trans])
      if(col == "dim_expr") {
        for(g in support_df[support_df$obj_type == "PARENT", "group"]) {
          num_den <- unlist(strsplit((filter(support_df, group == g, obj_type == "PARENT"))$dim_expr_trans, "/"))
          support_df[support_df$group == g & support_df$obj_type == "PARENT", "dim_expr_trans"] <-
            paste0((filter(support_df, group == g, measure_name == num_den[1]))$dim_expr,
                   "/",
                   (filter(support_df, group == g, measure_name == num_den[2]))$dim_expr)
        }
      }
      support_df[, m_expr_trans] <- gsub("\\s+", "", support_df[, m_expr_trans])
      support_df[, m_expr_trans] <- gsub("\\(", "", support_df[, m_expr_trans])
      support_df[, m_expr_trans] <- gsub("\\)", "", support_df[, m_expr_trans])    
    }
    
    # transform column regAgg and format in support_df
    for (chg in list(c("regAgg", "UNSUPPORTED", "CALCULATED"),
                     c("regAgg", "AVERAGE", "AVG"),
                     c("format", "PERCENT", "%"))) {
      for(m in c("bu_", "dim_")) {
        m_col <- paste0(m, chg[1])
        support_df[!is.na(support_df[, m_col]) & support_df[, m_col]==chg[2], m_col] <- chg[3]
        #support_df[!is.na(support_df[, m_col]) & grepl(chg[2], support_df[, m_col]), m_col] <- chg[3]
      }
    } 
    
    # only "^\\w+/\\w+=\\w+$" is handled for calculated regAgg 
    kpi_split_regAgg <- c("kpi_regAgg_NUM", "kpi_regAgg_DEN", "kpi_regAgg_PARENT")
    target_pat <- "^\\w+/\\w+=\\w+$"
    dist_regAgg <- support_df[support_df$obj_type == "DISTINCT", "kpi_regAgg"]
    support_df[, kpi_split_regAgg] <- NA
    
    if(any(grepl(target_pat, dist_regAgg))) {
      support_df[support_df$obj_type == "DISTINCT", "kpi_regAgg"] <-
        sub("^(\\w+)/\\w+=\\w+$", "\\1", dist_regAgg)
    }
    
    if (any(grepl(target_pat, support_df$kpi_regAgg))) {
      support_df[grep(target_pat, support_df$kpi_regAgg), c("kpi_regAgg_NUM","kpi_regAgg_PARENT")] <- 
        do.call(rbind, strsplit(support_df[grep(target_pat, support_df$kpi_regAgg), "kpi_regAgg"], "="))
      support_df[, c("kpi_regAgg_NUM","kpi_regAgg_DEN")] <- do.call(rbind, strsplit(support_df$kpi_regAgg_NUM, "/"))
      support_df$kpi_regAgg_PARENT <- sub("^AVG$", "CALCULATED", support_df$kpi_regAgg_PARENT)
    }
  }, error = function(err) {
    print(paste("Error occured in make_support_df function: ", err))
    print("Returning support_df on error.")
    return(support_df)
  })
  
  support_df
}

get_result <- function(m) {
  if(any(is.na(m))) NA
  else if(all(m)) "PASS"
  else "FAIL"
}

add_result_col <- function(compared_df) {
  ## summarise compare results
  #   - “UNHANDLED” for UNHANDLED rows
  #   - “PASS” if all TRUE for the appropriate match columns (see relevant match columns by obj_type below)
  #   - “FAIL” if FALSE for any of the appropriate match columns (see relevant match columns by obj_type below)
  #   - NA if NA for any of the appropriate match columns (see relevant match columns by obj_type below)
  
  if(FALSE) {
    # For ref:
    match_columns <- c("bu_dim_regAgg_match",
                       "bu_dim_format_match",
                       "bu_dim_precision_match",
                       "kpi_bu_format_match",
                       "kpi_bu_precision_match",
                       "kpi_bu_regAgg_match",
                       "bu_child_tag_match",
                       "kpi_bu_expr_match",
                       "kpi_dim_format_match",
                       "kpi_dim_precision_match",
                       "kpi_dim_regAgg_match",
                       "dim_child_tag_match",
                       "kpi_dim_expr_match",
                       "bu_dim_expr_match")
  }
  
  ## Relevant match columns by obj_type
  # PARENT
  parent_match_col <- c("bu_dim_regAgg_match",
                        "bu_dim_format_match",
                        "bu_dim_precision_match",
                        "kpi_bu_format_match",
                        "kpi_bu_precision_match",
                        "kpi_bu_regAgg_match",
                        "kpi_bu_expr_match",
                        "kpi_dim_format_match",
                        "kpi_dim_precision_match",
                        "kpi_dim_regAgg_match",
                        "kpi_dim_expr_match")
  # NUM/DEN
  child_match_col <- c("bu_dim_regAgg_match",
                       "bu_dim_format_match",
                       "bu_dim_precision_match",
                       "kpi_bu_regAgg_match",
                       "bu_child_tag_match",
                       "kpi_dim_regAgg_match",
                       "dim_child_tag_match",
                       "bu_dim_expr_match")
  # DISTINCT
  distinct_match_col <- c("bu_dim_regAgg_match",
                          "bu_dim_format_match",
                          "bu_dim_precision_match",
                          "kpi_bu_format_match",
                          "kpi_bu_precision_match",
                          "kpi_bu_regAgg_match",
                          "kpi_bu_expr_match",
                          "kpi_dim_format_match",
                          "kpi_dim_precision_match",
                          "kpi_dim_regAgg_match",
                          "kpi_dim_expr_match",
                          "bu_dim_expr_match")
  
  and_results <- function(ot, chk_match_cols) {
    compared_df[chk_ot == ot, "results"] <<- 
      sapply(
        as.list(as.data.frame(t(
          compared_df[chk_ot == ot, chk_match_cols]
        ))), get_result)
  }
  
  chk_ot <- compared_df$obj_type
  compared_df$results <- NA
  compared_df[chk_ot == "UNHANDLED", "results"] <- "UNHANDLED"
  and_results("PARENT", parent_match_col)
  and_results("NUM", child_match_col)
  and_results("DEN", child_match_col)
  and_results("DISTINCT", distinct_match_col)
  
  compared_df
}

compare_columns <- function(merged_df) {
  if(nrow(merged_df) == 0) { stop("merged_df nrow is zero. Nothing to validate.") }
  tryCatch({
    support_df <- make_support_df(merged_df)
    #add columns group, obj_type, bu_expr_trans, dim_expr_trans and kpi_expr_trans column to merged_df
    merged_df[, c("bu_expr_trans", "dim_expr_trans", "kpi_expr_trans", "group", "obj_type")] <-
      support_df[, c("bu_expr_trans", "dim_expr_trans", "kpi_expr_trans", "group", "obj_type")]
    support_df <- arrange(support_df, group, desc(obj_type))
    merged_df  <- arrange(merged_df, group, desc(obj_type))
    chk_g <- merged_df$group
    chk_ot <- merged_df$obj_type
    
    ## bu vs dim for "regAgg", "format" and "precision" values
    col <- c("regAgg", "format", "precision")
    merged_df[, paste0("bu_dim_", col, "_match")] <- 
      (support_df[, paste0("bu_", col)] == support_df[, paste0("dim_", col)])
    
    
    for(m in c("bu_", "dim_")) {
      ## kpi vs bu and kpi vs dim for "format" and "precision" values    
      col <- c("format", "precision")
      m_col <- paste0(m, col)
      merged_df[, paste0("kpi_", m_col, "_match")] <- (support_df[, paste0("kpi_", col)] == support_df[, m_col])
      
      ## kpi vs bu and kpi vs dim for "regAgg"
      m_col <- paste0(m, "regAgg")
      kpi_m_col <- paste0("kpi_", m, "regAgg_match")
      merged_df[, kpi_m_col] <- NA
      for(g in support_df[chk_ot == "PARENT", "group"]) {
        for (member in c("PARENT", "NUM", "DEN")) {        
          v <- support_df[chk_g==g & chk_ot==member, m_col] == 
            support_df[chk_g==g & chk_ot == "PARENT", paste0("kpi_regAgg_", member)]          
          merged_df[chk_g==g & chk_ot==member, kpi_m_col] <- v #ifelse(length(v) == 1, v, NA)       
        }
      }     
      merged_df[chk_ot == "DISTINCT", kpi_m_col] <-
        support_df[chk_ot == "DISTINCT", m_col] == support_df[chk_ot == "DISTINCT", "kpi_regAgg"]
      
      # child tag validation
      kpi_m_col <- paste0(m, "child_tag_match")
      merged_df[, kpi_m_col] <- NA
      merged_df[chk_ot == "NUM" | chk_ot == "DEN", kpi_m_col] <- FALSE
      merged_df[grepl("\\s*[_-]\\s*DEN$", support_df$measure_name) &
                  grepl("_DEN$", support_df[, paste0(m, "expr")]) &
                  chk_ot == "DEN", kpi_m_col] <- TRUE
      merged_df[grepl("\\s*[_-]\\s*NUM$", support_df$measure_name) &
                  grepl("_NUM$", support_df[, paste0(m, "expr")]) &
                  chk_ot == "NUM", kpi_m_col] <- TRUE
      
      ## kpi vs bu and kpi vs dim for for "expr"
      # DISTINCT expr validation
      m_col <- paste0(m, "expr")
      kpi_col <- "kpi_expr"
      kpi_m_col <- paste0("kpi_", m, "expr_match")
      merged_df[, kpi_m_col] <- NA    
      merged_df[chk_ot == "DISTINCT", kpi_m_col] <-
        merged_df[chk_ot == "DISTINCT", m_col] == merged_df[chk_ot == "DISTINCT", kpi_col]
      # PARENT expr validation
      chk_parent_ot <- chk_ot == "PARENT"
      merged_df[chk_parent_ot, kpi_m_col] <-
        support_df[chk_parent_ot, paste0(m_col, "_trans")] == support_df[chk_parent_ot, paste0(kpi_col,"_trans")]
    }
    
    # bu vs dim expr for obj_type NUM/DEN/DISTINCT
    merged_df$bu_dim_expr_match <- NA
    num_den_dist_ot <- chk_ot == "DISTINCT" | chk_ot == "NUM"| chk_ot == "DEN"
    merged_df[num_den_dist_ot, "bu_dim_expr_match"] <-
      support_df[num_den_dist_ot, "bu_expr"] == support_df[num_den_dist_ot, "dim_expr"]
    
    add_result_col(merged_df)
  }, error = function(err) {
    print(paste("Error occured in compare_columns function: ", err))
    print("Returning merged_df on error.")
    return(merged_df)
  }) 
}