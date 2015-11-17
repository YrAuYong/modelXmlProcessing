# R version during development:
# > R.version.string
# [1] "R version 3.2.0 (2015-04-16)"
#
# 32bit R used during development:
# > .Machine$sizeof.pointer
# [1] 4
#
# packages required other than base rtools:
# install.packages("XML")
# install.packages("xlsx")
# install.packages("dplyr")
#
# package versions during script dev:
# > packageVersion("XML")
# [1] ‘3.98.1.2’
# > packageVersion("xlsx")
# [1] ‘0.5.7’
# > packageVersion("dplyr")
# [1] ‘0.4.2’

##############################################
# Source dependencies
##############################################
source("utils.R", local=T)
##############################################
get_raw_kpi_df <- function(kpifile) {
  check_files(kpifile, "xlsx")
  require(xlsx)
  message("loading kpi table from kpifile, sheet \"Dashboards and Reports\" in raw_kpi_df variable.")
  read.xlsx(kpifile, sheetName="Dashboards and Reports", header=TRUE, stringsAsFactors=F)
}

process_modelxml <- function(kpifile, modelfile="model.xml", udcfile=NULL, reloadkpi=FALSE, phase=1) {
#process_modelxml <- function(vendor, kpifile, modelfile="model.xml", reloadkpi=FALSE, phase=1) {
  ##############################################
  # Input Arg checks
  ##############################################
  check_files(modelfile, "xml")
  check_files(kpifile, "xlsx")
  message(paste0("kpifile: ", kpifile))
  message(paste0("modelfile: ", modelfile))
  if (!is.null(udcfile)) {
    check_files(udcfile, "xml")
    message(paste0("udcfile: ", udcfile))
  }
  ## checks bu_nspc and dim_nspc
  if(!is.numeric(phase) | length(phase)!=1 | !(phase %in% c(1,2))) {
    stop(paste0("process_modelxml argument 'phase' should be numeric,",
                "have length == 1 and have values '1' or '2' only."))
  }
  ## checks vendor arg
  #if(!(varcheck_w_fun_len(vendor, is.character, 1) & (toupper(vendor) %in% c("HUAWEI", "ERICSSON", "ALU"))))
  #  stop("vendor arg should only contain characters 'Huawei' or 'Ericsson' or 'ALU' and have length = 1")
  
  ## checks reloadkpi arg
  if(!is.logical(reloadkpi) | length(reloadkpi) != 1)
    stop(paste0("reloadkpi arg should be logical and have length = 1. Instead, reloadkpi:\n\n", reloadkpi))
  
  ##############################################
  # INIT
  ##############################################  
  # dependencies
  require(dplyr)
  source("model_kpi.R", local = T)
  
  ## Config constant for diff phases
  # TODO
  nsIdx <-structure(list(bu_nsIdx=c(6,2),
                         dim_nsIdx=c(7,6)),
                    .Names=c("bu_nsIdx","dim_nsIdx"),
                    row.names = c(NA,-2L),
                    class = "data.frame")
  bu.nsIdx <-nsIdx[phase,"bu_nsIdx"]
  dim.nsIdx <-nsIdx[phase,"dim_nsIdx"]
  
  # load and read xml
  require(XML)
  rootnode <-xmlRoot(xmlTreeParse(modelfile))
  vendor <- rev(unlist(strsplit(xmlSApply(rootnode[["namespace"]][[2]], xmlValue), " ")))[1]
  for(v in c("Huawei", "Ericsson", "ALU")) {
    vendor <- sub(v, v, vendor, ignore.case = T)
  }
  if(!(vendor %in% c("Huawei", "Ericsson", "ALU"))) stop("vendor arg should only contain characters 'Huawei' or 'Ericsson' or 'ALU'.")
  
  ##############################################
  # Model data process steps:
  # 1. Create data frame for Business layer
  # 2. Create data frame for Dimensional layer
  # 3. Merge Business and Dimensional layer
  # 4. If kpi_df dont exist, create kpi_df
  # 5. Merge bu_dim_df with kpi_df 
  # 6. Compare expr, regularAggregator, format and precision columns and add results in new columns for:
  #     - bu vs dim
  #     - kpi vs bu
  #     - kpi vs dim
  # 7. Run process_udcxml and merge results to model_kpi_df
  # 8. Output results to model_kpi_validation.csv file
  ##############################################
  
  ## 1 ##
  bu_df <- make_model_df(get_layer(bu.nsIdx, "Business Layer", rootnode), qItem_nodepaths, "bu")
  #write.csv(bu_df, "model_business.csv", row.names=F)
  
  ## 2 ##
  dim_df <- make_model_df(get_layer(dim.nsIdx, "Dimension(al)? Layer", rootnode), measure_nodepaths, "dim")
  #write.csv(dim_df, "model_dimensional.csv", row.names=F)
  
  ## 3 ##
  bu_dim_df <-  merge(bu_df, dim_df, all=T)
  
  ## 4 ##
  #load, read and cache kpifile
  if(reloadkpi | !exists("raw_kpi_df", mode="list")) raw_kpi_df <<- get_raw_kpi_df(kpifile)
  
  ## 5 ##
  model_kpi_df <- merge(bu_dim_df, prep_kpi_df(raw_kpi_df, vendor), all.x=T)

  ## 6 ##
  model_kpi_df <- compare_columns(model_kpi_df)
  
  names(model_kpi_df)[grep("_match$", names(model_kpi_df), perl = T)]
  ## 7 ##
  if(!is.null(udcfile)) {
    #cu_wd <- getwd()
    #setwd(owd)
    tryCatch({
      message(paste0("Executing process_udcxml for vendor ", vendor))
      model_kpi_df$id <- 1:nrow(model_kpi_df)
      model_kpi_df <- merge(model_kpi_df, process_udcxml(kpifile, udcfile, vendor, ret = TRUE), by = "kpi_expr", all.x = T, sort = F)
      model_kpi_df <- model_kpi_df[, c(setdiff(names(model_kpi_df), "results"), "results")]
      prev_results <- !is.na(model_kpi_df$results) & (model_kpi_df$results != "UNHANDLED")
      if (any(prev_results)) {
        model_kpi_df[prev_results, "results"] <-
          mapply(function(x, y) { get_result(c(x, y)) },
                 sapply(model_kpi_df[prev_results, "results"], function(x) { if(x == "PASS") TRUE else FALSE }),
                 model_kpi_df$kpi_udc_expr_mapped_match[prev_results])
      }
      #rearrange columns and rows
      model_kpi_df <- rename(model_kpi_df, kpi_expr_trans.model = kpi_expr_trans.x, kpi_expr_trans.udc = kpi_expr_trans.y)
      name_cols <- c("measure_name", "summary_name", "full_summary_name", "granularity", "group", "obj_type")
      udc_validation_cols <- c("kpi_expr_mapped", "kpi_expr_trans.udc", "kpi_expr_mapped_trans", "udc_expr_mapped", "kpi_xfield_mapped",
                               "udc_xfield_mapped", "kpi_expr_mapped_ops", "udc_expr_mapped_ops", "kpi_expr_mapped_eval", "udc_expr_mapped_eval")
      match_rs_cols <- c(names(model_kpi_df)[grep("_match$", names(model_kpi_df))], "results")
      model_validation_cols <- sort(setdiff(names(model_kpi_df), c(name_cols, udc_validation_cols, match_rs_cols)))
      model_kpi_df <- model_kpi_df[, c(name_cols, model_validation_cols, udc_validation_cols, match_rs_cols)] %>%
        arrange(id) %>% select(-id)
    }, error = function(err) {
      warning(paste0("Merging abandoned. Error occured while merging udc validation to model_kpi_df: ", err))
    }, finally = {
      #setwd(cu_wd)
    })
  }
  
  ## 8 ##
  # Temporarily set current dir to be dir of modelfile, on exit the current dir will
  # be reverted to the original current dir before function is called
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dirname(modelfile)) 
  #modelfile <- basename(modelfile)
  write.csv(model_kpi_df[, !(names(model_kpi_df) %in% c("full_summary_name", "bu_full_expr", "dim_full_expr"))],
            "model_kpi_validation.csv",
            row.names=F)
  write.csv(model_kpi_df, "model_kpi_validation_full.csv", row.names=F)
  
  message(paste0("process_modelxml execution done! Generated files:\n\t- ",
                 getwd(), "/model_kpi_validation.csv\n\t- ",
                 getwd(), "/model_kpi_validation_full.csv"))
  
  ##############################################
  
  #model_kpi_df[, !(names(model_kpi_df) %in% c("bu_full_expr", "dim_full_expr"))]
}

process_udcxml <- function(kpifile, udcfile, vendor = c("Huawei", "Ericsson", "ALU"), reloadkpi = FALSE, ret = FALSE) {
  ##############################################
  # Input Arg checks
  ##############################################
  check_files(udcfile, "xml")
  check_files(kpifile, "xlsx")
  message(paste0("kpifile: ", kpifile))
  message(paste0("udcfile: ", udcfile))
  ## checks vendor arg
  if(!is.character(vendor) | !all(toupper(vendor) %in% c("HUAWEI", "ERICSSON", "ALU"))) {
    stop("vendor arg should only be a vector containing characters 'Huawei' or 'Ericsson' or 'ALU'") 
  }
  
  ## checks reloadkpi arg
  if(!is.logical(reloadkpi) | length(reloadkpi) != 1)
    stop(paste0("reloadkpi arg should be logical and have length = 1. Instead, reloadkpi:\n\n", reloadkpi))
  
  ## checks ret arg
  if (!varcheck_w_fun_len(ret, is.logical, 1)) {
    stop("ret arg should be logical and have length = 1")
  }
  
  ##############################################
  # INIT
  ##############################################
  # source dependencies
  source("udc_kpi.R", local= T)
  require(dplyr)
  # Temporarily set current dir to be dir of udcfile, on exit the current dir will
  # be reverted to the original current dir before function is called
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dirname(udcfile)) 
  udcfile <- basename(udcfile)
  
  # transform vendor names
  for(v in c("Huawei", "Ericsson", "ALU")) {
    vendor <- sub(v, v, vendor, ignore.case = T)
  }
  vendor <- unique(vendor)
  ##############################################
  ## Udc kpi validation process
  # 1. Read udcfile and load in udc_df table
  # 2. If raw_kpi_df not exist or reloadkpi is true, read/load/cache raw_kpi_df
  # 3. Subset kpi_expr_mapped and kpi_expr columns from raw_kpi_df
  # 4. Clean kpi_expr/kpi_mapped/udc_expr values and store transformed values in *_trans columns
  # 5. Compare kpi_df$kpi_expr_trans and kpi_df$kpi_expr_mapped_trans expressions
  #     a. Create kpi_df$udc_expr_mapped column: 
  #         - replace field names in kpi_df$kpi_expr_trans with mapped field names by referencing udc_df table
  #     b. Create kpi_df$kpi_xfield_mapped and kpi_df$udc_xfield_mapped columns:
  #         - comma separated expr fields extracted from kpi_df$kpi_expr_mapped_trans and kpi_df$udc_expr_mapped respectively
  #     c. Compare kpi_df$kpi_xfield_mapped and kpi_df$udc_xfield_mapped and store results in kpi_df$xfield_mapped_match
  #     d. Create kpi_df$kpi_expr_mapped_ops and kpi_df$udc_expr_mapped_ops columns:
  #         - expr operators(brackets removed) extracted from kpi_df$kpi_expr_mapped_trans and kpi_df$udc_expr_mapped respectively
  #     e. Compare kpi_df$kpi_expr_mapped_ops and kpi_df$udc_expr_mapped_ops and store results in kpi_df$expr_mapped_ops_match
  #     f. Create kpi_df$kpi_expr_mapped_eval and kpi_df$udc_expr_mapped_eval columns:
  #         - replace all fields names in kpi_df$kpi_expr_mapped_trans and kpi_df$udc_expr_mapped respectively with "1"
  #     g. Compare kpi_df$kpi_expr_mapped_eval and kpi_df$udc_expr_mapped_eval calculated expressions and store results in
  #        kpi_df$expr_mapped_eval_match
  #     h. Consolidate results in kpi_df$kpi_udc_expr_mapped_match by evaluating (kpi_df$xfield_mapped_match &
  #        kpi_df$expr_mapped_ops_match & kpi_df$expr_mapped_eval_match) 
  #6. Write udc_df and kpi_df into csv files
  ##############################################
    
  udc_df <- make_udc_df(udcfile)
  # clean udc_field_mapped strings 
  udc_df <- transform_str_col(udc_df, udc_field_mapped_rplc_pat, "udc_field_mapped")
  
  if(reloadkpi | !exists("raw_kpi_df", mode="list")) raw_kpi_df <<- get_raw_kpi_df(kpifile)
  
  for (v in vendor) {
    kpi_df <- na.omit(raw_kpi_df[, c(paste0(v, ".Mapped"), paste0(v, ".Cognos.Formula"))])
    names(kpi_df) <- c("kpi_expr_mapped", "kpi_expr")
    
    if(nrow(kpi_df) == 0) {
      warning(paste0("nrow(kpi_df) is zero. File \"", getwd(), "/", v, "_udc_kpi_validation.csv\" not generated."))
    } else {
      ##clean kpi_expr/kpi_mapped strings 
      kpi_df <- transform_str_col(kpi_df, kpi_expr_mapped_rplc_pat, "kpi_expr_mapped") %>%
        transform_str_col(kpi_expr_rplc_pat, "kpi_expr")
      
      kpi_df <- compare_expr(udc_df, kpi_df)
      
      write.csv(kpi_df, paste(v, "_udc_kpi_validation.csv"), row.names=F)
      
      message(paste0("Generated file: ", getwd(), "/", v, "_udc_kpi_validation.csv"))
    } 
  }
  
  write.csv(udc_df, "udc_xml_table.csv", row.names=F)
  
  message(paste0("Generated file: ", getwd(), "/udc_xml_table.csv"))
  message("process_udcxml execution done!\n")
  
  ##############################################
  if(ret) return(kpi_df)
}

message(
  paste0("Usage of process_modelxml function:\n\n",
         "\tBy default \"model.xml\" in current directory would be used, udcfile arg is NULL and reloadkpi arg is FALSE.\n",
         "\tWhen udcfile is NULL, udc validation will not be executed. To run udc validation, input udc file name.\n",
         "\tTo run function, Example commands: \n",
         "\t\t- process_modelxml(\"./kpifile.xlsx\")\n",
         "\t\t- process_modelxml(\"./path/to/kpifile.xlsx\", \"./path/to/model.xml\")\n",
         "\t\t- process_modelxml(\"D:/full/path/to/kpifile.xlsx\", \"D:/full/path/to/model.xml\")\n",
         "\t\t- process_modelxml(\"./path/to/kpifile.xlsx\", \"./path/to/model.xml\", udcfile = \"./path/to/udcfile.xml\")\n"))

message(
  paste0("Usage of process_udcxml function:\n\n",
         "\tValid vendor names for vendor arg are characters 'Huawei' or 'Ericsson' or 'ALU'.\n",
         "\tBy default, reloadkpi arg is FALSE and if vendor is not specified function will be executed for all vendors.\n",
         "\tTo run function, example commands: \n",
         "\t\t- process_udcxml(\"./kpifile.xlsx\", \"./path/to/udc.xml\")\n",
         "\t\t- process_udcxml(\"D:/full/path/to/kpifile.xlsx\", \"D:/full/path/to/udc.xml\")\n",
         "\t\t- process_udcxml(\"./kpifile.xlsx\", \"./path/to/udc.xml\", \"Huawei\")\n"))

message(
  paste0("Kpi file caching:\n\n",
         "\tIt would take some time to load and read the kpi file. Hence, the kpi table(subset columns used) would be cached \n",
         "\tin an environment variable, \"raw_kpi_df\" after running process_modelxml or process_udcxml function for at least \n",
         "\tthe first time. \n",
         "\tTo reload/read a new kpi file, set reloadkpi arg to TRUE:\n",
         "\t\t- process_modelxml(\"./path/to/kpifile.xlsx\", \"./path/to/model.xml\", reloadkpi=TRUE)\n",
         "\t\t- process_udcxml(\"./path/to/kpifile.xlsx\", \"./path/to/udc.xml\", reloadkpi=TRUE)\n\n"))
