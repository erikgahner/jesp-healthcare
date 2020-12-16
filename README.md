# Personal politics? 
---

### Description and data sources

Replication material for 'Personal politics? Healthcare policies, personal experiences and government attitudes' published in the Journal of European Social Policy. This repository contains all files required to produce the figures, tables and numerical information provided in the manuscript and supplementary material.

### Author/contact

 - Erik Gahner Larsen, erikgahner@gmail.com

### Repository content

- `01_create-data.R` = R script used to create the datasets used for the analysis (requires original data)
- `02_analysis.R` = R script used for all analyses in the article and supplementary material
- `data_issp_healthcare.csv` = Data from the ISSP (generated via `01_create-data.R`)
- `HEALTH_PROT_31052019005406509.csv` = Data from OECD (required to run analysis in Online Appendix H)
- `sessionInfo.txt` = Output from sessionInfo() in R

### Session info

The analyses were made with [RStudio](http://www.rstudio.com/) (Version 1.3.1073) with the following R session:

```
## R version 4.0.2 (2020-06-22)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Catalina 10.15.7

## Matrix products: default
## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib

## locale:
## [1] en_GB.UTF-8/en_GB.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8

## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods   base     

## other attached packages:
##  [1] LMERConvenienceFunctions_3.0 xtable_1.8-4                 insight_0.11.1               sjstats_0.18.0              
##  [5] interplot_0.2.2              arm_1.11-2                   MASS_7.3-53                  abind_1.4-5                 
##  [9] lme4_1.1-26                  Matrix_1.2-18                gridExtra_2.3                conflicted_1.0.4            
## [13] stargazer_5.2.2              plyr_1.8.6                   mice_3.12.0                  haven_2.3.1                 
## [17] forcats_0.5.0                stringr_1.4.0                dplyr_1.0.2                  purrr_0.3.4                 
## [21] readr_1.4.0                  tidyr_1.1.2                  tibble_3.0.4                 ggplot2_3.3.2               
## [25] tidyverse_1.3.0             

## loaded via a namespace (and not attached):
##  [1] TH.data_1.0-10       minqa_1.2.4          colorspace_2.0-0     ellipsis_0.3.1       sjlabelled_1.1.7    
##  [6] estimability_1.3     htmlTable_2.1.0      parameters_0.10.1    base64enc_0.1-3      fs_1.5.0            
## [11] rstudioapi_0.13      farver_2.0.3         fansi_0.4.1          mvtnorm_1.1-1        lubridate_1.7.9.2   
## [16] xml2_1.3.2           codetools_0.2-18     splines_4.0.2        knitr_1.30           sjmisc_2.8.5        
## [21] spam_2.5-1           Formula_1.2-4        jsonlite_1.7.2       nloptr_1.2.2.2       interactionTest_1.2 
## [26] broom_0.7.2          cluster_2.1.0        dbplyr_2.0.0         png_0.1-7            effectsize_0.4.1    
## [31] compiler_4.0.2       httr_1.4.2           emmeans_1.5.3        backports_1.2.1      assertthat_0.2.1    
## [36] cli_2.2.0            htmltools_0.5.0.9003 tools_4.0.2          dotCall64_1.0-0      coda_0.19-4         
## [41] gtable_0.3.0         glue_1.4.2           maps_3.3.0           Rcpp_1.0.5           cellranger_1.1.0    
## [46] vctrs_0.3.5          nlme_3.1-151         xfun_0.19            rvest_0.3.6          lifecycle_0.2.0     
## [51] statmod_1.4.35       zoo_1.8-8            scales_1.1.1         hms_0.5.3            parallel_4.0.2      
## [56] sandwich_3.0-0       LCFdata_2.0          RColorBrewer_1.1-2   fields_11.6          memoise_1.1.0       
## [61] rpart_4.1-15         latticeExtra_0.6-29  stringi_1.5.3        bayestestR_0.8.0     checkmate_2.0.0     
## [66] boot_1.3-25          rlang_0.4.9          pkgconfig_2.0.3      lattice_0.20-41      labeling_0.4.2      
## [71] htmlwidgets_1.5.3    tidyselect_1.1.0     magrittr_2.0.1       R6_2.5.0             generics_0.1.0      
## [76] Hmisc_4.4-2          multcomp_1.4-15      DBI_1.1.0            mgcv_1.8-33          pillar_1.4.7        
## [81] foreign_0.8-80       withr_2.3.0          survival_3.2-7       nnet_7.3-14          performance_0.6.1   
## [86] modelr_0.1.8         crayon_1.3.4         utf8_1.1.4           jpeg_0.1-8.1         readxl_1.3.1        
## [91] data.table_1.13.4    reprex_0.3.0         digest_0.6.27        munsell_0.5.0       

```
