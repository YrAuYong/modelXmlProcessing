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


extract_kpi_lookup <- function(modelfile, alarmFile) {
  ##############################################
  # Source dependencies
  ##############################################
  require(dplyr)
  source("utils.R", local=T)
  ##############################################
  # Input Arg checks
  ##############################################
  check_files(modelfile, "xml")
  check_files(alarmFile, "xlsx")
  message(paste0("modelfile: ", modelfile))
  message(paste0("alarmFile: ", alarmFile))
  ##############################################
   
  make_kpiLookup_df <- function() {
    
    ##############################################
    # Prepare variables
    ##############################################
    require(XML)
    require(xlsx)
    # groups vars
    VENDORS <- c("Huawei","Ericsson", "ALU")
    TECHS <- c("2G", "3G", "4G")
    PRES_GRPS <- list(kpi_ref = "kpi_ref", hr_key = "hr_key", entity = "entity")
    TABS <- c("Hourly Alarm", "Dashboard hourly Cell")
    ALARM_COLS <- c("kpi_ref_name", "alarm")
    ##############################################
    # load and read input files
    ##############################################
    rootnode <- xmlRoot(xmlTreeParse(modelfile))
    
    alarm_df <- NULL
    for (v in VENDORS) {
      for (tab in TABS) {
        temp_df <- read.xlsx(alarmFile, sheetName=paste(v, tab), header=T, stringsAsFactors=F)
        names(temp_df)[grepl("KPI.Name.*used.in.model", names(temp_df)) |
                         grepl("Alarm.Name", names(temp_df))] <- ALARM_COLS
        alarm_df <- rbind(alarm_df, cbind(vendor = v, temp_df[, ALARM_COLS]))
      }
    }
    alarm_df <- unique(alarm_df) %>% filter(!is.na(kpi_ref_name)) %>% rm_htspc
    row.names(alarm_df) <- NULL
    
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
    cuDT <- format(Sys.time(), "%Y%m%d-%H%M%S")
    if(!dir.exists(file.path(output_dir))) dir.create(file.path(output_dir))
    dir.create(file.path(output_dir, cuDT), recursive = TRUE)
    setwd(file.path(output_dir, cuDT))
    ##############################################
    # Nested functions
    ##############################################
    
    ## assumes path looks like these:
    ## - "tag1/tag2[tag2childtag=123]/tag3[tag3childtag=abc doremi]/tag4"
    ## - "tag1/tag2[tag2childtag=abc]/tag3[tag3childtag=^doremi$]/tag4"
    ## tags are matched with whole word and children tag values matched supports patterns partially (only follow down the path of 1st pattern match)
    ## search pattern only for children values, doesnt check for tag attributes/properties 
    parsePath <- function(path) {
      path <- unlist(strsplit(path, "/"))
      list(path = path,
           pathHead = sub("(.*)\\[.*\\]","\\1", path[1]),
           byfilter = strsplit(sub(".+\\[(.*)\\]", "\\1", path[1]), "=")[[1]])
    }
    
    getNodeChunks <- function(node, path) {
      if(length(path) == 0) {
        node
      } else {
        pathProp <- parsePath(path)
        path <- pathProp$path
        pathHead <- pathProp$pathHead
        byfilter <- pathProp$byfilter
        #print(paste0("pathHead: ", pathHead, ", byfilter: ", paste0(byfilter, collapse = ",")))
        if (length(byfilter) == 2) {
          for (i in as.numeric(which(names(node) == pathHead))) {
            #print(paste0("node[[", i, "]]$name = ", node[[i]]$name))
            #print(paste0("xmlValue(node[[i]]$children[[byfilter[1]]]) = ", xmlValue(node[[i]]$children[[byfilter[1]]])))
            if (grepl(byfilter[2], xmlValue(node[[i]]$children[[byfilter[1]]]), ignore.case = T)) {
              return(getNodeChunks(node[[i]], path[-1]))
            }
          }
        } else {
          #print(paste0("is.null(node[[pathHead]]) = ", is.null(node[[pathHead]])))
          if(!is.null(node[[pathHead]])) {
            return(getNodeChunks(node[[pathHead]], path[-1]))
          } 
        }
        warning(paste0("getNodeChunks: path \"", paste0(path, collapse = "/"), "\" not found in node. Returning NULL"))
        NULL
      }
    }
    
    addRows_byfilter <- function(df, node, path_byfilter, fun, lastNode=NULL, topNamespace=NULL) {
      if(length(path_byfilter) > 0) {
        pathProp <- parsePath(path_byfilter)
        path <- pathProp$path
        pathHead <- pathProp$pathHead
        byfilter <- pathProp$byfilter
        node_idx <- which(names(node) == pathHead)
        #print(paste0("pathHead: ", pathHead, ", byfilter: ", paste0(byfilter, collapse = ",")))
        for(i in node_idx) {
          if (length(byfilter) != 2 || grepl(byfilter[2], node[[i]][[byfilter[1]]]$text, ignore.case = T)) {
            df <- addRows_byfilter(df, node[[i]], path[-1], fun,
                                   if(toupper(sub("\\(en-in\\)\\s*", "", node$name$text))=="COUNTERS") lastNode else node,
                                   if(is.null(topNamespace)) node[[i]]$name$text else topNamespace)
          }
        }
        
        df
      } else {
        fun(df, node, lastNode, topNamespace)
      }
    }
    
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

      rbind(df,data.frame(vendor = chk_empty(get_grp(VENDORS, topNamespace)),
                          kpi_ref_name = chk_empty(node$name$text),
                          kpi_ref_link = chk_empty(paste0("[", topNamespace, "].[", lastNode$name$text, "]")),
                          counter_name = chk_empty(paste0(
                            sub(".*\\.(\\[.*\\])$", "\\1", node$expression[which(names(node$expression) == "refobj")]),
                            collapse=',')),
                          counter_ref_link = chk_empty(paste0(
                            gsub("^(.*)\\.\\[.*\\]$", "\\1", node$expression[which(names(node$expression) == "refobj")]),
                            collapse=',')),
                          tech = chk_empty(get_grp(TECHS, lastNode$name$text)),
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
      
      for (np in c(paste0("namespace[name=", VENDORS, "]/folder[name=Hourly KPIs]/querySubject/queryItem"))) {
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
      
      # Add entity_name to bu_df
      bu_df$entity_name <- NA
      pairs <- expand.grid(TECHS, VENDORS, stringsAsFactors = F)
      for (rw in 1:nrow(pairs)) {
        type <- pairs[rw, 1]
        v <- pairs[rw, 2]
        entity_path <- paste0("querySubject[name=", type, " ", sub("Huawei", "Huwawei", v), "]") #temp workaround to accommodate typo Huwawei
        bu_df$entity_link[bu_df$tech == type & bu_df$vendor == v] <- 
          paste0(sqr_bracketify(v), sqr_bracketify(xmlValue(getNodeChunks(bu_node_chunk, paste0(entity_path, "/name"))), T))
        bu_df$entity_name[bu_df$tech == type & bu_df$vendor == v] <-
          xmlValue(getNodeChunks(bu_node_chunk, paste0(entity_path, "/queryItem[name=Cell ID]/name")))
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
           
      for(v in VENDORS) {
        v_node_chunk <- getNodeChunks(pres_node_chunk, paste0("namespace[name=", v, "]"))
        v_name <- sqr_bracketify(xmlValue(getNodeChunks(v_node_chunk, "name")))
        
        #add kpi_ref_grp
        kpi_ref_grp <- cbind(
          FQN = paste0(v_name,
                       sqr_bracketify(sapply(
                         getNodeSet(v_node_chunk, "//folder[name='Hourly KPIs']/shortcut/name"),
                         xmlValue), T)),
          Ref.Obj = sapply(getNodeSet(v_node_chunk, "//folder[name='Hourly KPIs']/shortcut/refobj"), xmlValue),
          grp = PRES_GRPS$kpi_ref
        )
        
        #add hr_key_grp
        hr_key_var <- paste0(v_name,
                             sqr_bracketify(xmlValue(getNodeChunks(v_node_chunk, "shortcut[name=Time]/name")), T))        
        hr_key_grp <- cbind(
          FQN = hr_key_var,
          Ref.Obj = hr_key_var,
          grp = PRES_GRPS$hr_key)
        
        #add entity_grp
        entity_grp <- NULL
        for (tch in TECHS) {
          entity_grp <- rbind(entity_grp, cbind(
            FQN = paste0(v_name,
                         sqr_bracketify(sapply(
                           getNodeSet(v_node_chunk, paste0("/namespace/shortcut[contains(concat(' ', name, ' '), '", tch, "')]/name")), xmlValue), T)),
            Ref.Obj = paste0(v_name,
                             gsub(".*(\\.\\[.*\\])$", "\\1",
                                 sapply(getNodeSet(v_node_chunk, paste0("/namespace/shortcut[contains(concat(' ', name, ' '), '", tch, "')]/refobj")), xmlValue))),
            grp = PRES_GRPS$entity))
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
      #Example: lookup_df$kpi_ref = pres_df$FQN + lookup_df$kpi_ref_name by matching lookup_df$kpi_ref_link to pres_df$Ref.Obj
      for (grp in PRES_GRPS) {
        for (ref in pres_df$Ref.Obj[pres_df$grp == grp]) {
          lookup_df[lookup_df[, paste0(grp, "_link")] == ref, grp] <-
            paste0(pres_df$FQN[pres_df$Ref.Obj == ref],
                   sqr_bracketify(lookup_df[lookup_df[, paste0(grp, "_link")] == ref, paste0(grp, "_name")], T))
        }
      }

      lookup_df
    }
    
    clean_data <- function(lookup_df) {
      #remove POHC & NA
      lookup_df <- lookup_df %>% filter(!grepl("POHC", kpi_ref_name) & !grepl("POHC", kpi_ref)) %>% na.omit %>% unique
      row.names(lookup_df) <- NULL
      
      lookup_df
    }
    
    ###################################################
    # make_kpiLookup_df steps:
    # 1. load & read input files
    # 2. Extract kpi lookups under the bussiness layer
    # 3. Extract kpi lookups under the presentation layer
    # 4. combine bu_df, pres_df and alarm_df into lookup_df
    ###################################################
       
    bu_df <- make_bu_df() %>% rm_htspc

    pres_df <- make_pres_df() %>% rm_htspc
    
    ##TODO!! ALARM refer to TECH also, need to merge by VENDOR, KPI_REF_NAME and TECHS, currently only merged by VENDOR and KPI_REF_NAME
    lookup_df <- merge_pres_bu_df(bu_df, pres_df) %>% merge(alarm_df) %>% clean_data
    lookup_df_smry <- select(lookup_df, vendor, kpi_ref_name, kpi_ref, hr_key, entity, alarm) %>%
      rename(VENDOR = vendor, 'KPI NAME' = kpi_ref_name, 'KPI REFERENCE' = kpi_ref, 'HOUR KEY' = hr_key, 'ENTITY REFERENCE' = entity, 'ALARM NAME' = alarm)

    write.csv(lookup_df, "kpi_lookup_all.csv", row.names=F)
    write.csv(lookup_df_smry, "kpi_lookup.csv", row.names=F)

    message(paste0("extract_kpi_lookup execution done! Generated files:\n\t- ",
                   getwd(), "/kpi_lookup.csv\n"))
    
    list(lookup_all = lookup_df,
         lookup = lookup_df_smry,
         bu = bu_df,
         pres = pres_df,
         alarm = alarm_df)
  }
  
  kpiLookup_df <- make_kpiLookup_df()

  ##############################################
  
  #kpiLookup_df
}

message(
  paste0("Usage of extract_kpi_lookup function:\n\n",
         "\t- extract_kpi_lookup(\"./path/to/model.xml\", \"./path/to/alarm.xlsx\")\n",
         "\t- extract_kpi_lookup(\"D:/full/path/to/model.xml\", \"D:/full/path/to/alarm.xlsx\")\n"))
