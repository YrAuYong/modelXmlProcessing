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
# install.packages("dplyr")
#
# package versions during script dev:
# > packageVersion("XML")
# [1] ‘3.98.1.2’
# > packageVersion("dplyr")
# [1] ‘0.4.2’

extract_kpi_names <- function(modelfile) {
  ##############################################
  # INIT
  ##############################################  
  # dependencies
  require(dplyr)
  source("utils.R", local=T)
  source("model_kpi.R", local = T)
  ##############################################
  # Input Arg checks
  ##############################################
  check_files(modelfile, "xml")
  message(paste0("modelfile: ", modelfile))
  ############################################## 
  
  make_kpiName_df <- function() {
    # load and read xml
    require(XML)
    rootnode <-xmlRoot(xmlTreeParse(modelfile))
    
    get_kpi_by_locale <- function(node, locale) {
      if (node[[1]]$.attrs[["locale"]] == locale)
        node[[1]]$text
      else if(node[[2]]$.attrs[["locale"]] == locale)
        node[[2]]$text
      else
        stop(paste0("Failed to extract name where locale is \"", locale, "\""))
    }
    
    bind_rws_2_kpiNames_df <- function(df, node, lastNode, topNamespace) {
      if(is.null(lastNode) | is.null(topNamespace)){ stop("lastNode or topNamespace is null.")}  
      
      if(FALSE) {
        #will not add rows for:
        # - node$expression that contains "Data Layer"
        # - lastNode$name$text in summary_name_2_rm list
        kpi_ref_2_rm <- c("Cell 2G",
                          "Cell 3G",
                          "Cell 4G",
                          "Time",
                          "Etisalat Configuration Table",
                          "POHC",
                          #paste0("ALL ", vendor, " Region"),
                          "Cell 3G Femto")
        if((sum(grepl("Data Layer", node$expression)) > 0) |
             (lastNode$name$text %in% kpi_ref_2_rm)) return(df)
      }
      
      rbind(df,data.frame(kpi_name = chk_empty(get_kpi_by_locale(node, "en")),
                          kpi_name.en_in = chk_empty(get_kpi_by_locale(node, "en-in")),
                          kpi_ref = chk_empty(get_kpi_by_locale(lastNode, "en")),
                          kpi_ref.en_in = chk_empty(get_kpi_by_locale(lastNode, "en-in")),
                          vendor = chk_empty(sub(" KPIs", "", topNamespace)),
                          stringsAsFactors=F))
    }
    
    clean_data <- function(df) {      
      #remove rows with kpi_name in kpi_name_2_rm
      kpi_name_2_rm <- c("(TSTAMP|TimeSTAMP)",
                         ".*_ID",
                         "DA_.*",
                         "\\[Cell\\]")
      
      for(n in c("kpi_name", "kpi_name.en_in")) {
        for(toRm in kpi_name_2_rm) {
          df <- df[!grepl(toRm, df[, n]), ]
        }
      }
      
      #remove header and trailing spaces
      df <- rm_htspc(df)    
      
      v_prefix <- c(Huawei = "[Huawei CS (rel)]",
                    Ericsson = "[Ericsson Access (rel)]",
                    ALU ="[ALU Access (rel)]")
      
      #concat kpi_ref and kpi_name
      for(pair in list(c("kpi_name", "kpi_ref"), c("kpi_name.en_in", "kpi_ref.en_in"))) {
        df[, pair[2]] <- paste0(v_prefix[df$vendor],".[", df[, pair[2]], "].[", df[, pair[1]], "]")
      }
     
      df[, c("kpi_name", "kpi_ref", "kpi_ref.en_in")]
    }
    
    ###################################################
    # make_kpiName_df steps:
    # 1. load & read XML
    # 2. Extract kpi Names under the bussiness layer
    # 3. Rename kpi_names df columns
    # 4. Clean kpi Names in df
    ###################################################
    
    df <- data.frame(kpi_name = character(),
                     kpi_name.en_in = character(),
                     kpi_ref = character(),
                     kpi_ref.en_in = character(),
                     vendor = character(),
                     stringsAsFactors=F)
    
    
    #only extract bussiness layer
    for (np in qItem_nodepaths_byName) {
      #print(paste0("np: ", paste0(np, collapse = ".")))
      df <- addRows_byName(df, get_layer(6, "Business Layer", rootnode), np, bind_rws_2_kpiNames_df)
    }
    
    names(df) <- c("kpi_name", "kpi_name.en_in", "kpi_ref", "kpi_ref.en_in", "vendor")
    
    clean_data(df)
  }
  
  kpiName_df <- make_kpiName_df()
  
  # Temporarily set current dir to be dir of modelfile, on exit the current dir will
  # be reverted to the original current dir before function is called
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dirname(modelfile)) 
  write.csv(kpiName_df, "kpi_names.csv", row.names=F)
  
  message(paste0("extract_kpi_names execution done! Generated files:\n\t- ",
                 getwd(), "/kpi_names.csv\n"))
  
  ##############################################
  
  #kpiName_df
}

message(
  paste0("Usage of extract_kpi_names function:\n\n",
         "\t- extract_kpi_names(\"./path/to/model.xml\")\n",
         "\t- extract_kpi_names(\"D:/full/path/to/model.xml\")\n"))
