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

extract_kpi_lookup <- function(modelfile, presentationLayerFile = NULL) {
  ##############################################
  # Source dependencies
  ##############################################
  #require(dplyr)
  source("utils.R", local=T)
  source("model_kpi.R", local = T)
  ##############################################
  # Input Arg checks
  ##############################################
  check_files(modelfile, "xml")
  if(!is.null(presentationLayerFile)) check_files(presentationLayerFile, "csv")
  message(paste0("modelfile: ", modelfile))
  message(paste0("presentationLayerFile: ", presentationLayerFile))
  ##############################################
  # INIT
  ##############################################    
  # Temporarily set current dir to be dir of modelfile, on exit the current dir will
  # be reverted to the original current dir before function is called
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dirname(modelfile))
  # Create dirs for output files
  output_dir <- "lookup_output"
  cuDT <- format(Sys.time(), "%Y%m%d-%H:%M:%S")
  if(!dir.exists(file.path(output_dir))) dir.create(file.path(output_dir))
  dir.create(file.path(output_dir, cuDT))
  setwd(file.path(output_dir, cuDT))
  ############################################## 
  
  make_kpiLookup_df <- function() {
    
    ##############################################
    # Prepare variables
    ##############################################
    require(XML)
    # groups vars
    vendors <- c("Huawei","Ericsson", "ALU")
    techs <- c("2G", "3G", "4G")
    pres_grps <- list(kpi_ref = "kpi_ref", hr_key = "hr_key", entity = "entity")
    # load and read input files
    rootnode <-xmlRoot(xmlTreeParse(modelfile))
    if(!is.null(presentationLayerFile)) pres_df <- read.csv(presentationLayerFile, stringsAsFactors=FALSE)
    
    ##############################################
    # Nested functions
    ##############################################
    get_grp <- function(grp, target) {
      grp[sapply(grp,
                 function(g) {
                   grepl(g, target, ignore.case = T)
                 })]
    }
    
    sqr_bracketify <- function(s, prefixDot = FALSE) {
      paste0(ifelse(prefixDot, ".", ""), "[", s, "]")
    }
    
    add_kpi_counter <- function(df, node, lastNode, topNamespace) {
      if(is.null(lastNode) || is.null(topNamespace)){ stop("lastNode or topNamespace is null.")}
      #kpi_name = paste0("[", vendors[sapply(vendors, function(v){ grepl(v, topNamespace, ignore.case = T) })],
      #"].[", chk_empty(node$name$text), "]")
      rbind(df,data.frame(vendor = chk_empty(get_grp(vendors, topNamespace)),
                          kpi_ref_name = chk_empty(node$name$text),
                          kpi_ref_link = chk_empty(paste0("[", topNamespace, "].[", lastNode$name$text, "]")),  #"].[", node$name$text, "]")),
                          counter_name = chk_empty(paste0(
                            sub(".*\\.(\\[.*\\])$", "\\1", node$expression[which(names(node$expression) == "refobj")]),
                            collapse=',')),
                          counter_ref_link = chk_empty(paste0(
                            gsub("^(.*)\\.\\[.*\\]$", "\\1", node$expression[which(names(node$expression) == "refobj")]),
                            collapse=',')),
                          #hr_key_link = ,
                          #entity_name =,
                          tech = chk_empty(get_grp(techs, lastNode$name$text)),
                          stringsAsFactors=F))
    }
    
    make_bu_df <- function() {
      bu_df <- data.frame(vendor = character(),
                          kpi_ref_name = character(),
                          kpi_ref_link = character(),
                          counter_name = character(),
                          counter_ref_link = character(),
                          tech = character(),
                          stringsAsFactors=F)
      
      # only extract from bussiness layer
      # Add Hourly KPIs and counters to bu_df
      bu_node_chunk <- getNodeChunks(rootnode,"namespace/namespace[name=Business Layer]")
      
      for (np in c(paste0("namespace[name=", vendors, "]/folder[name=Hourly KPIs]/querySubject/queryItem"))) {
        bu_df <- addRows_byfilter(bu_df, lapply(bu_node_chunk[], xmlToList), np, add_kpi_counter)
      }
      
      # Add hour_key to bu_df    
      bu_df$hr_key_link <- paste0(sqr_bracketify(bu_df$vendor),
                                  sqr_bracketify(xmlValue(
                                    getNodeChunks(
                                      bu_node_chunk,
                                      "querySubject[name=Time]/name")), T))
      bu_df$hr_key_name <- xmlValue(
        getNodeChunks(
          bu_node_chunk,
          "querySubject[name=Time]/queryItem[name=Hour key Start]/name"))
      #bu_df$hr_key_link <- mapply(gsub,
      #                       "Data Layer",
      #                       bu_df$vendor,
      #                       xmlValue(
      #                         getNodeChunks(
      #                           bu_node_chunk,
      #                           "querySubject[name=Time]/queryItem[name=Hour key Start]/expression/refobj")),
      #                       USE.NAMES = F)
      
      # Add entity_name to bu_df
      bu_df$entity_name <- NA
      pairs <- expand.grid(techs, vendors, stringsAsFactors = F)
      for (rw in 1:nrow(pairs)) {
        type <- pairs[rw, 1]
        v <- pairs[rw, 2]
        entity_path <- paste0("querySubject[name=", type, " ", sub("Huawei", "Huwawei", v), "]") #temp workaround to accommodate typo Huwawei
        bu_df$entity_link[bu_df$tech == type & bu_df$vendor == v] <- 
          paste0(sqr_bracketify(v), sqr_bracketify(xmlValue(getNodeChunks(bu_node_chunk, paste0(entity_path, "/name"))), T))
        bu_df$entity_name[bu_df$tech == type & bu_df$vendor == v] <-
          xmlValue(getNodeChunks(bu_node_chunk, paste0(entity_path, "/queryItem[name=Cell ID]/name")))
        
        #bu_df$entity_name[bu_df$tech == type & bu_df$vendor == v] <- mapply(gsub,
        #                                                                    "Data Layer",
        #                                                                    v,
        #                                                                    xmlValue(getNodeChunks(bu_node_chunk, entity_path)),
        #                                                                    USE.NAMES = F)
      }
      
      write.csv(bu_df, "bu_df.csv", row.names=F)
      bu_df
    }
    
    make_pres_df <- function() {
      pres_node_chunk <- getNodeChunks(rootnode, "namespace/namespace[name=Presentation Layer]")
      
      pres_df <- data.frame(vendor = character(),
                            FQN = character(),
                            Ref.Obj = character(),
                            grp = character(),
                            stringsAsFactors = F)
      
      #To delete
      get_xmlValue <- function(p) {
        xmlValue(getNodeChunks(pres_node_chunk, p))
      }
           
      for(v in vendors) {
        v_node_chunk <- getNodeChunks(pres_node_chunk, paste0("namespace[name=", v, "]"))
        v_name <- sqr_bracketify(xmlValue(getNodeChunks(v_node_chunk, "name")))
        
        #add kpi_ref_grp
        kpi_ref_grp <- cbind(
          FQN = paste0(v_name,
                       sqr_bracketify(sapply(
                         getNodeSet(v_node_chunk, "//folder[name='Hourly KPIs']/shortcut/name"),
                         xmlValue), T)),
          Ref.Obj = sapply(getNodeSet(v_node_chunk, "//folder[name='Hourly KPIs']/shortcut/refobj"), xmlValue),
          grp = pres_grps$kpi_ref
        )
        
        #add hr_key_grp
        hr_key_var <- paste0(v_name,
                             sqr_bracketify(xmlValue(getNodeChunks(v_node_chunk, "shortcut[name=Time]/name")), T))        
        hr_key_grp <- cbind(
          FQN = hr_key_var,
          Ref.Obj = hr_key_var,
          grp = pres_grps$hr_key)
        
        #add entity_grp
        entity_grp <- NULL
        for (tch in techs) {
          entity_grp <- rbind(entity_grp, cbind(
            FQN = paste0(v_name,
                         sqr_bracketify(sapply(
                           getNodeSet(v_node_chunk, paste0("/namespace/shortcut[contains(concat(' ', name, ' '), '", tch, "')]/name")), xmlValue), T)),
            Ref.Obj = paste0(v_name,
                             gsub(".*(\\.\\[.*\\])$", "\\1",
                                 sapply(getNodeSet(v_node_chunk, paste0("/namespace/shortcut[contains(concat(' ', name, ' '), '", tch, "')]/refobj")), xmlValue))),
            grp = pres_grps$entity))
        }
        
        #rbind all grps and add to pres_df
        pres_df <- rbind(
          pres_df,
          cbind(vendor = v, rbind(kpi_ref_grp, hr_key_grp, entity_grp))
        )
      }
      
      write.csv(pres_df, "pres_df.csv", row.names=F)
      pres_df
    }
    
    merge_pres_bu_df <- function(lookup_df, pres_df) {
      if(FALSE) {
        # lookup_df$kpi_ref = pres_df$FQN + lookup_df$kpi_ref_name by linking lookup_df$kpi_ref_link to pres_df$Ref.Obj
        for (ref in pres_df$Ref.Obj)
          lookup_df$kpi_ref[lookup_df$kpi_ref_link == ref] <-
          paste0(pres_df$FQN[pres_df$Ref.Obj == ref],
                 sqr_bracketify(lookup_df$kpi_ref_name[lookup_df$kpi_ref_link == ref], T))
        
        for (ref in unique(pres_df$hr_key_link))
          lookup_df$hr_key[lookup_df$hr_key_link == ref] <-
          paste0(ref, sqr_bracketify(lookup_df$hr_key_name[lookup_df$hr_key_link == ref], T))
      }
          
      for (grp in pres_grps) {
        for (ref in pres_df$Ref.Obj[pres_df$grp == grp]) {
          lookup_df[lookup_df[, paste0(grp, "_link")] == ref, grp] <-
            paste0(pres_df$FQN[pres_df$Ref.Obj == ref],
                   sqr_bracketify(lookup_df[lookup_df[, paste0(grp, "_link")] == ref, paste0(grp, "_name")], T))
        }
      }

      lookup_df
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
    # make_kpiLookup_df steps:
    # 1. load & read XML
    # 2. Extract kpi lookups under the bussiness layer
    # 3. if presentationLayerFile NULL make pres_df
    # 4. combine bu_df and pres_df into lookup_df
    ###################################################
       
    bu_df <- make_bu_df()
    
    if(is.null(presentationLayerFile)) {
      pres_df <- make_pres_df()
    }
    
    lookup_df <- merge_pres_bu_df(bu_df, pres_df)
    write.csv(lookup_df, "kpi_lookup_all.csv", row.names=F)
    
    if(FALSE) {
      lookup_df <- clean_data(lookup_df)
    }
    
    list(lookup = lookup_df, bu = bu_df, pres = pres_df)
  }
  
  kpiLookup_df <- make_kpiLookup_df()
  
  
  
  
  if(FALSE) {
    write.csv(kpiLookup_df, "kpi_lookup.csv", row.names=F)
    
    message(paste0("extract_kpi_lookup execution done! Generated files:\n\t- ",
                   getwd(), "/kpi_lookup.csv\n"))
     
  }
  
  ##############################################
  
  kpiLookup_df
}

message(
  paste0("Usage of extract_kpi_lookup function:\n\n",
         "\t- extract_kpi_lookup(\"./path/to/model.xml\")\n",
         "\t- extract_kpi_lookup(\"D:/full/path/to/model.xml\")\n"))
