# Copyright (C) 2017  The Regents of the University of Michigan
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see [http://www.gnu.org/licenses/].
# ========================================================================


## utility functions for reading and preprocessing data prior to structural modeling.

library(magrittr)
library(plyr)
library(dplyr)
library(stringr)

## DROP_THRESH represents the threshold for missingness; columns with more than DROP_THRESH percent of values missing will be dropped
DROP_THRESH = 0.1

## FACTOR COLS contains names of columns that need to be converted to factors manually; these are likely columns with numeric values that are actually categorical (i.e., student ID)
FACTOR_COLS = c("STDNT_ID")

##VALID_GRADES represents outcomes the model should consider; only records wtih a grade in VALID_GRADES are passed to the model
VALID_GRADES = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-")

## drops columns drop data frame with proportion of missing observations >= DROP_THRESH.
## df: data to apply filtering to (DataFrame).
## drop_thresh: threshold (0 < drop_thresh < 1) used for dropping; drops columns drop data frame with proportion of missing observations >= drop_thresh (numeric).
## keep_cols: names of columns to retain in df even if they exceed drop_thresh (character).
## returh: df will columns with proportion of missingness exceeding drop_thresh removed.
apply_drop_thresh <- function(df, drop_thresh = DROP_THRESH, keep_cols = c("PREV_TERM_CUM_GPA")) {
    namevec = names(df)
    navec = apply(df, 2, function(x) sum(is.na(x))/nrow(df))
    dropvec = navec >= drop_thresh
    df_out = df[,!dropvec]
    for (i in seq_along(keep_cols)) {
        if (!(keep_cols[i] %in% names(df_out))) {
            df_out <- cbind(df_out,  df[,keep_cols[i]])
            names(df_out)[ncol(df_out)] <- keep_cols[i] 
        }
    }
    return(df_out)
}


## add "MISSING" indicator for missing levels of factor variables.
## checks for columns which end in "_CD", "_IND", or "_NM".
## df: DataFrame.
## return: df with any missing values for matching columns filled with "MISSING".
create_missing_ind <- function(df) {
    miss = apply(df, 2, function(x) sum(is.na(x)))
    fix_cols = grep(".*_IND$|.*_CD|.*_NM",names(miss[miss!=0]), value = TRUE)
    for (c in fix_cols) {
        newcol = sapply(df[,c], function(x) if (is.na(x)) "MISSING" else as.character(x))
        df[,c] <- as.factor(newcol)
    }
    return(df)
}
    

## drop any columns from df that are factors with more than thresh levels.
## df: DataFrame.
## lev_thresh: cardinality threshold for variables.
## keep_cols: columns to keep even if they exceed lev_thresh.
## return: df with any columns whose cardinality exceeds lev_thresh removed.
drop_high_cardinality_fct <- function(df, lev_thresh = 20, keep_cols = c("SBJCT_CD")) {
    levs = sapply(df, function(x) ifelse(is.factor(x), length(levels(x)), 0))
    high_card_cols = names(levs[levs > lev_thresh])
    high_card_cols = high_card_cols[high_card_cols != keep_cols]
    df_out = select(df, -one_of(high_card_cols))
    return(df_out)
}


## wrapper to read data from fp, applying filters, changing column data types, and dropping columns using DROP_COLS, FACTOR_COLS, VALID_GRADES, DROP_THRESH specified in module (above)
## fp (character): single file path or vector of file paths for multiple, identically-columned CSV datasets.
## impute (string): data imputation method; none are currently implemented ("none" is only valid value).
## print_miss_info: if TRUE, prints short info about missing data contained/dropped from data (boolean).
## factor_thresh: drop factors with greater than this many levels; to keep all, set this to zero
## keep_train_cols: columns in dataframes to be retained no matter their missingness or cardinality (character).
## use_drop_cols: if TRUE, will drop columns with names in DROP_COLS; otherwise all columns are returned (boolean).
## drop_high_cardinality: if TRUE, drop columns exceeding a cardinality thredhold; otherwise all columns are returned (boolean).
## return: single dataframe compiled from all files in fp.
read_data <- function(fp, impute = "None", print_miss_info = TRUE, factor_thresh = 20, keep_train_cols = c("PREV_TERM_CUM_GPA"), use_drop_thresh = TRUE, drop_high_cardinality = TRUE) {
    # TODO: check input values and break/warn if error
    df_list = lapply(fp, read.csv, stringsAsFactors = TRUE, na.strings = c("NA", "NaN", " ", ""), header = TRUE)
    df_in = do.call(rbind, df_list)
    df = df_in %>%
        filter(GRD_BASIS_ENRL_CD == "GRD", CLASS_GRDD_IND == 1) %>% # keep graded courses only
        filter(CRSE_GRD_OFFCL_CD %in% VALID_GRADES) %>% # keep only grades in VALID_GRADES
        filter(grepl("^[^G|P].*", PRMRY_CRER_CD) | is.na(PRMRY_CRER_CD)) %>% # exclude graduate and professional students; TODO: confirm that this is logical/correct/best identifier for undergrads vs. grads
        select(-one_of(DROP_COLS)) %>%
        select(-ends_with("_DES")) %>% # drop any _DES columns; these are always redundant to a corresponding _CD column and sometimes include typos/multiple desc of same level
        select(-ends_with("_DT")) %>% # drop any _DT columns; model should ignore dates because it will be tested on different time period
        select(-ends_with("_YR_MO")) %>% # drop other date columns
        mutate_at(vars(one_of(FACTOR_COLS)), factor) %>%
        mutate_at(vars(ends_with("_IND")), factor)  %>%
        mutate_at(vars(ends_with("_CD")), factor)
    if (use_drop_thresh == TRUE){
        df %<>% 
            apply_drop_thresh(keep_cols = keep_train_cols) %>%
            create_missing_ind()
    }
    else if (use_drop_thresh != TRUE) {
        df %<>% create_missing_ind()
    }
        
    
    if (print_miss_info == TRUE){
        miss = apply(df, 2, function(x) sum(is.na(x)))
        message("Dataframe has ", ncol(df), " columns after initial preprocessing; ", length(miss[miss!= 0]), " columns contain missing values; ", round(sum(miss)/(nrow(df)*ncol(df)),5), " of total observations missing")
        print("Missing observations:")
        print(miss[miss!=0])
    }
    # imputation
    if (impute == "none") {
        df_complete= df[complete.cases(df),]
        mdr = nrow(df_complete)/nrow(df)
        message("Returning complete cases only; this is ", 100*round(mdr, 4), "% of preprocessed data.")
        df <- df_complete
    }
    # drop high-cardinality cols
    if (drop_high_cardinality == TRUE){
        df %<>% drop_high_cardinality_fct(lev_thresh = factor_thresh)
    }
    return(df)
}


## create "coarse-grained" grades from "fine-grained" grades; this amounts to removing +/- in grade_col and then releveling factor to remove those levels
make_grades_coarse <- function(df, grade_col = "CRSE_GRD_OFFCL_CD") {
    gradevec = df[,grade_col]
    coarse_gradevec = sapply(gradevec, function(x) str_replace(as.character(x), "[+-]", ''))
    df[,grade_col] <- factor(coarse_gradevec)
    return(df)
}


## drop any subjects where the count of unique grades observed was less than or equal to thresh
apply_unique_grade_thresh <- function(df, subject_col = "SBJCT_CD", thresh = 1) {
    grade_counts = df %>% ddply(.(SBJCT_CD), summarize, unique_grades = length(unique(CRSE_GRD_OFFCL_CD)))
    drop_subs = grade_counts[grade_counts$unique_grades <= thresh,"SBJCT_CD"] 
    df_out = filter(df, !(SBJCT_CD %in% drop_subs)) %>% droplevels()
    return(df_out)
}


## for any factor columns, drop observations in testdf which are not observed in traindf (otherwise cannot predict) and set to have same levels
match_train_levels <- function(traindf, testdf) {
    testdf_out <- testdf
    for(col in seq(ncol(traindf))) {
        tr_c = traindf[,col]
        te_c = testdf[,col]
        if (is.factor(tr_c) & is.factor(te_c) & !(identical(levels(tr_c), levels(te_c)))) {
            # get levels in testdf not in traindf and drop those observations; then relevel to drop those levels from factor
            drop_levs = levels(te_c)[!(levels(te_c) %in% levels(tr_c))]
            testdf_out <- testdf_out[!(testdf_out[,col] %in% drop_levs),]
            testdf_out[,col] <- droplevels(testdf_out[,col])
            
        }
        # for ANY factor col, drop NA levels due to bug in predict.randomForest http://stackoverflow.com/questions/24829674/r-random-forest-error-type-of-predictors-in-new-data-do-not-match
        if (is.factor(te_c)){
            # replace na levels
            te_c = testdf_out[,col]
            levels(te_c)[is.na(levels(te_c))] <- "NA"
            testdf_out[,col] <- te_c
            }
    }
    # add one observation of traindf to testdf to force factors to have same levels, even if unused in testdf; then drop that row and return testdf_out
    # this is hack-y, but is easiest way I found to force all factor levels to be the same even when those levels may never occur in testdf.
    testdf_out <- rbind(testdf_out, traindf[1,])
    testdf_out <- testdf_out[-nrow(testdf_out),]
    return(testdf_out)
}
