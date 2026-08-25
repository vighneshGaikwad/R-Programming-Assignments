
R version 4.6.1 (2026-06-24) -- "Happy Hop"
Copyright (C) 2026 The R Foundation for Statistical Computing
Platform: aarch64-apple-darwin23

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[R.app GUI 1.83 (8608) aarch64-apple-darwin23]

[History restored from /Users/yashlokare/.Rapp.history]

> R.version.string
[1] "R version 4.6.1 (2026-06-24)"
> dir.create("~/Desktop/R_Programming_Assignment")
> setwd("~/Desktop/R_Programming_Assignment")
> getwd()
[1] "/Users/yashlokare/Desktop/R_Programming_Assignment"
> url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
> heart_data <- read.csv(url, header = FALSE, na.strings = "?")
> head(heart_data)
  V1 V2 V3  V4  V5 V6 V7  V8 V9 V10 V11 V12 V13 V14
1 63  1  1 145 233  1  2 150  0 2.3   3   0   6   0
2 67  1  4 160 286  0  2 108  1 1.5   2   3   3   2
3 67  1  4 120 229  0  2 129  1 2.6   2   2   7   1
4 37  1  3 130 250  0  0 187  0 3.5   3   0   3   0
5 41  0  2 130 204  0  2 172  0 1.4   1   0   3   0
6 56  1  2 120 236  0  0 178  0 0.8   1   0   3   0
> colnames(heart_data) <- c(
+   "age",
+   "sex",
+   "cp",
+   "trestbps",
+   "chol",
+   "fbs",
+   "restecg",
+   "thalach",
+   "exang",
+   "oldpeak",
+   "slope",
+   "ca",
+   "thal",
+   "target"
+ )
> colnames(heart_data)
 [1] "age"      "sex"      "cp"       "trestbps" "chol"     "fbs"      "restecg"  "thalach"  "exang"    "oldpeak"  "slope"    "ca"       "thal"     "target"  
> summary(heart_data$trestbps)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   94.0   120.0   130.0   131.7   140.0   200.0 
> set.seed(123)
> 
> heart_data$trestbps[c(1, 2, 3)] <- c(-20, -50, -10)
> 
> heart_data$trestbps[c(4, 5, 6)] <- NA
> 
> heart_data$trestbps[c(7, 8, 9)] <- c(320, 350, 400)
> summary(heart_data$trestbps)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
  -50.0   120.0   130.0   132.3   140.0   400.0       3 
> heart_data$trestbps[1:10]
 [1] -20 -50 -10  NA  NA  NA 320 350 400 140
> clean_bp <- function(bp) {
+   
+   if (is.na(bp)) {
+     return(NA)
+     
+   } else if (bp < 0) {
+     return(NA)
+     
+   } else if (bp > 250) {
+     return(250)
+     
+   } else {
+     return(bp)
+   }
+ }
> clean_bp(-20)
[1] NA
> clean_bp(320)
[1] 250
> clean_bp(140)
[1] 140
> clean_bp(NA)
[1] NA
> heart_data$trestbps_clean <- sapply(
+   heart_data$trestbps,
+   clean_bp
+ )
> heart_data[1:10, c("trestbps", "trestbps_clean")]
   trestbps trestbps_clean
1       -20             NA
2       -50             NA
3       -10             NA
4        NA             NA
5        NA             NA
6        NA             NA
7       320            250
8       350            250
9       400            250
10      140            140
> summary(heart_data$trestbps_clean)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
   94.0   120.0   130.0   132.9   140.0   250.0       6 
> sum(is.na(heart_data$trestbps_clean))
[1] 6
> safe_bp_metrics <- function(chol_vec, bp_vec) { list(mean_bp = tryCatch({ if(all(is.na(bp_vec))) stop("All NA"); mean(bp_vec, na.rm=TRUE) }, error=function(e) NA), ratio = tryCatch({ if(any(bp_vec==0, na.rm=TRUE)) warning("Zero denominator"); chol_vec/bp_vec }, error=function(e) rep(NA, length(bp_vec)))) }
> metrics_result <- safe_bp_metrics(heart_data$chol, heart_data$trestbps_clean)
> heart_data$chol_bp_ratio <- metrics_result$ratio
> clean_bp_loop <- function(bp_vec) { cleaned <- numeric(length(bp_vec)); for(i in 1:length(bp_vec)) cleaned[i] <- clean_bp(bp_vec[i]); return(cleaned) }
> clean_bp_vectorized <- function(bp_vec) ifelse(is.na(bp_vec) | bp_vec < 0, NA, ifelse(bp_vec > 250, 250, bp_vec))
> system.time(for(i in 1:1000) clean_bp_loop(heart_data$trestbps))
   user  system elapsed 
  0.073   0.001   0.074 
> system.time(for(i in 1:1000) clean_bp_vectorized(heart_data$trestbps))
   user  system elapsed 
  0.023   0.003   0.026 
> cat("NA count:", sum(is.na(heart_data$trestbps_clean)), "| Min:", min(heart_data$trestbps_clean, na.rm=TRUE), "| Max:", max(heart_data$trestbps_clean, na.rm=TRUE), "| Mean:", mean(heart_data$trestbps_clean, na.rm=TRUE), "| Median:", median(heart_data$trestbps_clean, na.rm=TRUE))
NA count: 6 | Min: 94 | Max: 250 | Mean: 132.8519 | Median: 130
> write.csv(heart_data, "cleaned_heart_data.csv", row.names = FALSE)
> options(repos = c(CRAN = "https://cloud.r-project.org/"))
> if(!require("naniar")) install.packages("naniar"); if(!require("skimr")) install.packages("skimr")
Loading required package: naniar
also installing the dependencies ‘bit’, ‘prettyunits’, ‘bit64’, ‘progress’, ‘utf8’, ‘farver’, ‘labeling’, ‘RColorBrewer’, ‘stringi’, ‘clipr’, ‘crayon’, ‘hms’, ‘vroom’, ‘tzdb’, ‘Rcpp’, ‘generics’, ‘pillar’, ‘R6’, ‘tidyselect’, ‘gtable’, ‘isoband’, ‘S7’, ‘scales’, ‘withr’, ‘stringr’, ‘cpp11’, ‘pkgconfig’, ‘readr’, ‘viridisLite’, ‘gridExtra’, ‘plyr’, ‘dplyr’, ‘ggplot2’, ‘purrr’, ‘tidyr’, ‘tibble’, ‘norm’, ‘magrittr’, ‘visdat’, ‘rlang’, ‘forcats’, ‘viridis’, ‘glue’, ‘UpSetR’, ‘cli’, ‘vctrs’, ‘lifecycle’

trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/bit_4.6.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/prettyunits_1.2.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/bit64_4.8.2.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/progress_1.2.3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/utf8_1.2.6.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/farver_2.1.2.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/labeling_0.4.3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/RColorBrewer_1.1-3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/stringi_1.8.9.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/clipr_0.8.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/crayon_1.5.3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/hms_1.1.4.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/vroom_1.7.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/tzdb_0.5.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/Rcpp_1.1.2.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/generics_0.1.4.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/pillar_1.11.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/R6_2.6.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/tidyselect_1.2.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/gtable_0.3.6.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/isoband_0.3.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/S7_0.2.2.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/scales_1.4.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/withr_3.0.3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/stringr_1.6.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/cpp11_0.5.5.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/pkgconfig_2.0.3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/readr_2.2.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/viridisLite_0.4.3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/gridExtra_2.3.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/plyr_1.8.9.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/dplyr_1.2.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/ggplot2_4.0.3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/purrr_1.2.2.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/tidyr_1.3.2.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/tibble_3.3.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/norm_1.0-11.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/magrittr_2.0.5.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/visdat_0.6.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/rlang_1.3.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/forcats_1.0.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/viridis_0.6.5.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/glue_1.8.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/UpSetR_1.4.1.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/cli_3.6.6.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/vctrs_0.7.3.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/lifecycle_1.0.5.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/naniar_1.1.0.tgz'

The downloaded binary packages are in
	/var/folders/7r/5ml_1_8j4yb6fkglqwpkkm5r0000gn/T//RtmpIj4bJb/downloaded_packages
Warning message:
In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  there is no package called ‘naniar’
Loading required package: skimr
also installing the dependencies ‘digest’, ‘fastmap’, ‘evaluate’, ‘highr’, ‘xfun’, ‘yaml’, ‘htmltools’, ‘jsonlite’, ‘base64enc’, ‘knitr’, ‘repr’

trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/digest_0.6.39.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/fastmap_1.2.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/evaluate_1.0.5.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/highr_0.12.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/xfun_0.60.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/yaml_2.3.12.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/htmltools_0.5.9.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/jsonlite_2.0.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/base64enc_0.1-6.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/knitr_1.51.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/repr_1.1.7.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/skimr_2.2.2.tgz'

The downloaded binary packages are in
	/var/folders/7r/5ml_1_8j4yb6fkglqwpkkm5r0000gn/T//RtmpIj4bJb/downloaded_packages
Warning message:
In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  there is no package called ‘skimr’
> install.packages(c("naniar", "skimr"))
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/naniar_1.1.0.tgz'
trying URL 'https://cloud.r-project.org/bin/macosx/sonoma-arm64/contrib/4.6/skimr_2.2.2.tgz'

The downloaded binary packages are in
	/var/folders/7r/5ml_1_8j4yb6fkglqwpkkm5r0000gn/T//RtmpIj4bJb/downloaded_packages
> library(naniar); library(skimr)

Attaching package: ‘skimr’

The following object is masked from ‘package:naniar’:

    n_complete

> adult_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
> adult_df <- read.csv(adult_url, header = FALSE, stringsAsFactors = FALSE, strip.white = TRUE)
> # ==============================================================================
> # LAB 4: ADVANCED MISSING DATA HANDLING (FULL SCRIPT)
> # ==============================================================================
> 
> # 1. Load Required Libraries
> library(naniar)
> library(skimr)
> 
> # 2. Load UCI Adult Dataset and Assign Column Names
> adult_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data"
> adult_df <- read.csv(adult_url, header = FALSE, stringsAsFactors = FALSE, strip.white = TRUE)
> 
> colnames(adult_df) <- c(
+   "age", "workclass", "fnlwgt", "education", "education_num", 
+   "marital_status", "occupation", "relationship", "race", "sex", 
+   "capital_gain", "capital_loss", "hours_per_week", "native_country", "income"
+ )
> 
> # 3. Task 1: Introduce Missing/Invalid Data Scenarios
> set.seed(42)
> adult_df$age[c(10, 25, 50)] <- 999               # Impossible age values
> adult_df$workclass[c(15, 30)] <- ""              # Blank strings
> adult_df$hours_per_week[c(5, 12)] <- NaN         # NaN values
> adult_df$education_num[c(20, 40)] <- NA          # Standard NA values
> 
> cat("\n--- TASK 1: MISSING & INVALID DATA DETECTION ---\n")

--- TASK 1: MISSING & INVALID DATA DETECTION ---
> cat("NA count in education_num:", sum(is.na(adult_df$education_num)), "\n")
NA count in education_num: 2 
> cat("NaN count in hours_per_week:", sum(is.nan(adult_df$hours_per_week)), "\n")
NaN count in hours_per_week: 2 
> cat("Blank string count in workclass:", sum(adult_df$workclass == ""), "\n")
Blank string count in workclass: 2 
> cat("Impossible age (999) count:", sum(adult_df$age == 999, na.rm = TRUE), "\n")
Impossible age (999) count: 3 
> 
> # Demonstrate NULL behavior
> test_obj <- NULL
> cat("is.null() on R object:", is.null(test_obj), "\n")
is.null() on R object: TRUE 
> cat("is.null() on dataframe cell (Always FALSE):", is.null(adult_df$workclass[1]), "\n")
is.null() on dataframe cell (Always FALSE): FALSE 
> 
> cat("\n--- Variable-wise Missing Summary Before Cleaning ---\n")

--- Variable-wise Missing Summary Before Cleaning ---
> print(miss_var_summary(adult_df))
# A tibble: 15 × 3
   variable       n_miss pct_miss
   <chr>           <int>    <num>
 1 education_num       2  0.00614
 2 hours_per_week      2  0.00614
 3 age                 0  0      
 4 workclass           0  0      
 5 fnlwgt              0  0      
 6 education           0  0      
 7 marital_status      0  0      
 8 occupation          0  0      
 9 relationship        0  0      
10 race                0  0      
11 sex                 0  0      
12 capital_gain        0  0      
13 capital_loss        0  0      
14 native_country      0  0      
15 income              0  0      
> 
> 
> # 4. Task 3: Create Custom Median-Imputation Function
> custom_median_impute <- function(x) {
+   if (!is.numeric(x)) stop("Input vector must be numeric.")
+   x[is.nan(x)] <- NA
+   med_val <- median(x, na.rm = TRUE)
+   x[is.na(x)] <- med_val
+   return(x)
+ }
> 
> 
> # 5. Task 2: Apply Missing-Data Treatment Pipeline
> adult_cleaned <- adult_df
> 
> # Convert impossible numeric age values (999) to NA
> adult_cleaned$age[adult_cleaned$age == 999] <- NA
> 
> # Replace blank strings and '?' in categorical fields with 'Unknown'
> adult_cleaned$workclass[adult_cleaned$workclass == "" | adult_cleaned$workclass == "?"] <- "Unknown"
> adult_cleaned$occupation[adult_cleaned$occupation == "?"] <- "Unknown"
> adult_cleaned$native_country[adult_cleaned$native_country == "?"] <- "Unknown"
> 
> # Impute missing numeric values using custom median function
> adult_cleaned$age <- custom_median_impute(adult_cleaned$age)
> adult_cleaned$hours_per_week <- custom_median_impute(adult_cleaned$hours_per_week)
> adult_cleaned$education_num <- custom_median_impute(adult_cleaned$education_num)
> 
> # Evaluate complete cases ratio
> complete_ratio <- sum(complete.cases(adult_cleaned)) / nrow(adult_cleaned)
> cat("\nComplete cases ratio after treatment:", round(complete_ratio * 100, 2), "%\n")

Complete cases ratio after treatment: 100 %
> 
> 
> # 6. Task 4 & 5: Validate Cleaned Dataset & Export Deliverable
> cat("\n--- Missing Summary After Cleaning ---\n")

--- Missing Summary After Cleaning ---
> print(miss_var_summary(adult_cleaned))
# A tibble: 15 × 3
   variable       n_miss pct_miss
   <chr>           <int>    <num>
 1 age                 0        0
 2 workclass           0        0
 3 fnlwgt              0        0
 4 education           0        0
 5 education_num       0        0
 6 marital_status      0        0
 7 occupation          0        0
 8 relationship        0        0
 9 race                0        0
10 sex                 0        0
11 capital_gain        0        0
12 capital_loss        0        0
13 hours_per_week      0        0
14 native_country      0        0
15 income              0        0
> 
> cat("\n--- Skimr Comprehensive Summary ---\n")

--- Skimr Comprehensive Summary ---
> print(skim(adult_cleaned))
── Data Summary ────────────────────────
                           Values       
Name                       adult_cleaned
Number of rows             32561        
Number of columns          15           
_______________________                 
Column type frequency:                  
  character                9            
  numeric                  6            
________________________                
Group variables            None         

── Variable type: character ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  skim_variable  n_missing complete_rate min max empty n_unique whitespace
1 workclass              0             1   7  16     0        9          0
2 education              0             1   3  12     0       16          0
3 marital_status         0             1   7  21     0        7          0
4 occupation             0             1   5  17     0       15          0
5 relationship           0             1   4  14     0        6          0
6 race                   0             1   5  18     0        5          0
7 sex                    0             1   4   6     0        2          0
8 native_country         0             1   4  26     0       42          0
9 income                 0             1   4   5     0        2          0

── Variable type: numeric ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  skim_variable  n_missing complete_rate     mean        sd    p0    p25    p50    p75    p100 hist 
1 age                    0             1     38.6     13.6     17     28     37     48      90 ▇▇▅▂▁
2 fnlwgt                 0             1 189778.  105550.   12285 117827 178356 237051 1484705 ▇▁▁▁▁
3 education_num          0             1     10.1      2.57     1      9     10     12      16 ▁▁▇▃▁
4 capital_gain           0             1   1078.    7385.       0      0      0      0   99999 ▇▁▁▁▁
5 capital_loss           0             1     87.3    403.       0      0      0      0    4356 ▇▁▁▁▁
6 hours_per_week         0             1     40.4     12.3      1     40     40     45      99 ▁▇▃▁▁
> 
> # Export deliverable CSV file
> write.csv(adult_cleaned, "cleaned_adult_data.csv", row.names = FALSE)
> cat("\nSuccessfully saved 'cleaned_adult_data.csv' to:", getwd(), "\n")

Successfully saved 'cleaned_adult_data.csv' to: /Users/yashlokare/Desktop/R_Programming_Assignment 
> 
