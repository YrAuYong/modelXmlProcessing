# modelXmlProcessing
Scripts for extracting data from model.xml, apply cleaning and transformation on extracted data for various purposes:
- validation 
- lookup table generation [https://github.com/YrAuYong/modelXmlProcessing/blob/master/README.md#generate-lookup-table]

## model validation
### Prerequisite:
-	R installation (32 bit version console used)
-	Rtools package installation
-	Basic commands examples:
	- dir() - works like ls in unix
	- getwd() - like pwd in unix
	- setwd("D:/your/dir") - set current working directory
	- install packages – for example to install XML packages “install.packages("XML")”

### Environment:
Scripts were written using 32bit R version 3.2.0 in windows:

-	cmd to check R version:  
`> R.version.string`  
`[1] "R version 3.2.0 (2015-04-16)"`
-	32bit R used during development (make sure the java set in your environment paths is 32 bit version):  
`> .Machine$sizeof.pointer`  
`[1] 4`
-	Package installations required other than base rtools:
	-	XML : `> install.packages("XML")`
	-	xlsx: `> install.packages("xlsx")`
	-	dplyr: `> install.packages("dplyr")`
-	package versions during script developement:  
    `> packageVersion("XML")`  
    `[1] ‘3.98.1.2’`  
    `> packageVersion("xlsx")`  
    `[1] ‘0.5.7’`  
    `> packageVersion("dplyr")`  
    `[1] ‘0.4.2’`

### Scripts:

- model_validation.R
	- main script contains process_modelxml and process_udcxml functions.
-	model_kpi.R
	- contain functions for model xml validation in reference to kpi xlsx.
-	udc_kpi.R
	- contain functions for udc xml validation in reference to kpi xlsx.
-	utils.R
	- general utility functions used by all other scripts above.

### Input files:
Function: process_modelxml:

-	model xml 
-	kpi xlsx

Function: process_udcxml:

-	udc xml
-	kpi xlsx

### Usage:

    > source("model_validation.R")
    Usage of process_modelxml function:
    
    	By default "model.xml" in current directory would be used, udcfile arg is NULL and reloadkpi arg is FALSE.
    	When udcfile is NULL, udc validation will not be executed. To run udc validation, input udc file name.
    	To run function, Example commands: 
    		- process_modelxml("./kpifile.xlsx")
    		- process_modelxml("./path/to/kpifile.xlsx", "./path/to/model.xml")
    		- process_modelxml("D:/full/path/to/kpifile.xlsx", "D:/full/path/to/model.xml")
    		- process_modelxml("./path/to/kpifile.xlsx", "./path/to/model.xml", udcfile = "./path/to/udcfile.xml")
    
    Usage of process_udcxml function:
    
    	Valid vendor names for vendor arg are characters 'Huawei' or 'Ericsson' or 'ALU'.
    	By default, reloadkpi arg is FALSE and if vendor is not specified function will be executed for all vendors.
    	To run function, example commands: 
    		- process_udcxml("./kpifile.xlsx", "./path/to/udc.xml")
    		- process_udcxml("D:/full/path/to/kpifile.xlsx", "D:/full/path/to/udc.xml")
    		- process_udcxml("./kpifile.xlsx", "./path/to/udc.xml", "Huawei")
    
    Kpi file caching:
    
    	It would take some time to load and read the kpi file. Hence, the kpi table(subset columns used) would be cached 
    	in an environment variable, "raw_kpi_df" after running process_modelxml or process_udcxml function for at least 
    	the first time. 
    	To reload/read a new kpi file, set reloadkpi arg to TRUE:
    		- process_modelxml("./path/to/kpifile.xlsx", "./path/to/model.xml", reloadkpi=TRUE)
    		- process_udcxml("./path/to/kpifile.xlsx", "./path/to/udc.xml", reloadkpi=TRUE)


### Output files:
**Function: process_modelxml:**

-	`model_kpi_validation.csv` located in the same directory as the model xml provided and contain variables:

> 	- measure_name
> 	- summary_name
> 	- full_summary_name
> 	- granularity
> 	- bu_expr_smry_name
> 	- bu_expr
> 	- bu_regAgg
> 	- bu_format
> 	- bu_precision
> 	- dim_expr_smry_name
> 	- dim_expr
> 	- dim_regAgg
> 	- dim_format
> 	- dim_precision
> 	- kpi_format
> 	- kpi_precision
> 	- kpi_expr
> 	- kpi_regAgg
> 	- bu_expr_trans
> 	- dim_expr_trans
> 	- kpi_expr_trans
> 	- group
> 	- obj_type
> 	- bu_dim_regAgg_match
> 	- bu_dim_format_match
> 	- bu_dim_precision_match
> 	- kpi_bu_format_match
> 	- kpi_bu_precision_match
> 	- kpi_bu_regAgg_match
> 	- bu_child_tag_match
> 	- kpi_bu_expr_match
> 	- kpi_dim_format_match
> 	- kpi_dim_precision_match
> 	- kpi_dim_regAgg_match
> 	- dim_child_tag_match
> 	- kpi_dim_expr_match
> 	- bu_dim_expr_match
> 	- results

- if udcfile is provided, in addition to the variables in the above, the `model_kpi_validation_full.csv` will also contain:

> 	- kpi_expr_trans.model
> 	- kpi_expr_mapped
> 	- kpi_expr_trans.udc
> 	- kpi_expr_mapped_trans
> 	- udc_expr_mapped
> 	- kpi_xfield_mapped
> 	- udc_xfield_mapped
> 	- kpi_expr_mapped_ops
> 	- udc_expr_mapped_ops
> 	- kpi_expr_mapped_eval
> 	- udc_expr_mapped_eval
> 	- xfield_mapped_match
> 	- expr_mapped_ops_match
> 	- expr_mapped_eval_match
> 	- kpi_udc_expr_mapped_match

-	`model_kpi_validation_full.csv` located in the same directory as the model xml provided and in addition to the variables in the `model_kpi_validation.csv`, contain variables:

> 	- full_summary_name
> 	- bu_full_expr
> 	- dim_full_expr

**Function: process_udcxml:**
-	`<vendor>_udc_kpi_validation.csv` will contain variables:

> 	- kpi_expr_mapped
> 	- kpi_expr
> 	- kpi_expr_mapped_trans
> 	- kpi_expr_trans
> 	- udc_expr_mapped
> 	- kpi_xfield_mapped
> 	- udc_xfield_mapped
> 	- xfield_mapped_match
> 	- kpi_expr_mapped_ops
> 	- udc_expr_mapped_ops
> 	- expr_mapped_ops_match
> 	- kpi_expr_mapped_eval
> 	- udc_expr_mapped_eval
> 	- expr_mapped_eval_match
> 	- kpi_udc_expr_mapped_match

### Function descriptions: 

**WIP**


## Generate lookup table

### Usage:

```
> source("kpi_names_extraction.R")
Usage of extract_kpi_names function:

        - extract_kpi_names("./path/to/model.xml")
        - extract_kpi_names("D:/full/path/to/model.xml")
> extract_kpi_names("./modelxml_20151109/model.xml")
modelfile: ./modelxml_20151109/model.xml
extract_kpi_names execution done! Generated files:
        - D:/R workdir/etisalat/modelxml_20151109/kpi_names.csv
```

### Model.xml node paths extracted:
-	Under Business Layer hierarchy project.namespace.namespace(name=”Business Layer”):
  * namespace(name=”Huawei/ Ericsson/ALU KPIs”).folder(name=”Hourly KPIs”).querySubject.queryItem
  * namespace(name=”Huawei/ Ericsson/ALU KPIs”).folder(name=”Hourly KPIs”).querySubject.queryItemFolder.queryItem

### Output kpi_names.csv Columns:
-	Kpi_name – queryItem name for locale=”en”
-	Kpi_ref – “`<prefix>.<querySubject>.<queryItem>`” for locale=”en”
-	Kpi_ref.en_in – “`<prefix>.<querySubject>.<queryItem>`” for locale=”en-in”

Where, `<prefix>` : 

1.	For Huawei: [Huawei CS (rel)].
2.	For Ericsson: [Ericsson Access (rel)].
3.	For ALU: [ALU Access (rel)].

### Clean up and filters:
-	Rows with the following kpi_ref* patterns are removed:
  * "TSTAMP”
  * “TimeSTAMP",
  *	".*_ID",
  *	"DA_.*"
  *	"[Cell]"
-	Removed header and trailing spaces

### Environment:

```
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
```
