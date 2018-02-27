source("preproc_utils.R")
library(dplyr)
DROP_COLS = c("X.SNPSHT_RPT_DT_x", "TERM_SHORT_DES_x", "X.SNPSHT_RPT_DT_y", "TERM_SHORT_DES_y", "GRD_BASIS_ENRL_CD", "GRD_BASIS_ENRL_DES", 
              "CLASS_LONG_DES", "CLASS_GRDD_IND", "CLASS_MTG_START_DT", "CLASS_MTG_END_DT", "CLASS_MTG_START_TM", "CLASS_MTG_END_TM", "MAX_UMPLC_MATH_TEST_SCR", "CLASS_NBR", "LAST_TERM_ATTND_CD")
# note that this includes testing term, term 2020
DATA_FP = c("/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_2020.csv", 
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1970.csv", 
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1920.csv", 
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1870.csv", 
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1820.csv",
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1770.csv",
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1720.csv",
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1670.csv",
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1620.csv",
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1570.csv",
            "/Users/joshgardner/Documents/UM-Graduate/UMSI/LED_Lab/lak-2018/lak18/data/nw_1520.csv"
)

results = list()
for (i in seq_along(DATA_FP)){
    fp = DATA_FP[i]
    # todo: this is a total hack; use a proper regex
    term = sub("nw_", "", sub(".csv", "", unlist(str_split(fp, "/"))[length(unlist(str_split(fp, "/")))]))
    # read data; applying same filtering for actual predictive model
    df = read_data(fp, use_drop_thresh = FALSE)
    # select only columns needed here; drop any missing observations (these will be students without gpa or without any coenrollments)
    df_subset = df %>% dplyr::select(matches("^(ML_|B_L|CL_|PL_|GRD_PNTS_PER_UNIT_NBR|PREV_TERM_CUM_GPA)")) %>% na.omit 
    # model with only network features
    df_nw = df_subset %>% dplyr::select(matches("^(ML_|B_L|CL_|PL_|GRD_PNTS_PER_UNIT_NBR)"))
    mod.lm.nw_only = lm(GRD_PNTS_PER_UNIT_NBR ~ ., data = df_nw)
    summary(mod.lm.nw_only)
    # model with only previous term cumulative gpa
    mod.lm.gpa_only = lm(GRD_PNTS_PER_UNIT_NBR ~ PREV_TERM_CUM_GPA, data = df_subset)
    summary(mod.lm.gpa_only)
    # model with network + previous term cumulative gpa
    mod.lm.nw_gpa = lm(GRD_PNTS_PER_UNIT_NBR ~ ., data = df_subset)
    summary(mod.lm.nw_gpa)
    # conduct f-test to compare gpa-only model to gpa + nw model
    f_test_res = anova(mod.lm.gpa_only, mod.lm.nw_gpa)
    row = data.frame(term = term, nw_only_r_squared = summary(mod.lm.nw_only)$r.squared, gpa_only_r_squared = summary(mod.lm.gpa_only)$r.squared, combined_r_squared = summary(mod.lm.nw_gpa)$r.squared, f_statistic = f_test_res[["F"]][2], f_test_p_val = f_test_res[["Pr(>F)"]][2])
    results[[i]] <- row
}
rsquared_analysis_results = do.call("rbind", results)
write.csv(rsquared_analysis_results, file = "network_rsquared_analysis_results.csv", row.names = FALSE)

## ==============================================================================================================
## ==============================================================================================================
# same analayis but with single large dataframe for ALL terms; this includes train+test terms together to demonstrate consistency across the entire dataset
master_df = NULL
for (i in seq_along(DATA_FP)){
    fp = DATA_FP[i]
    df = read_data(fp, use_drop_thresh = FALSE)
    df %<>% dplyr::select(matches("^(ML_|B_L|CL_|PL_|GRD_PNTS_PER_UNIT_NBR|PREV_TERM_CUM_GPA)")) %>% na.omit 
    master_df = rbind(master_df, df)
}

# model with only network features
df_nw = master_df %>% dplyr::select(matches("^(ML_|B_L|CL_|PL_|GRD_PNTS_PER_UNIT_NBR)"))
mod.lm.nw_only = lm(GRD_PNTS_PER_UNIT_NBR ~ ., data = df_nw)
summary(mod.lm.nw_only)
# model with only previous term cumulative gpa
mod.lm.gpa_only = lm(GRD_PNTS_PER_UNIT_NBR ~ PREV_TERM_CUM_GPA, data = master_df)
summary(mod.lm.gpa_only)
# model with network + previous term cumulative gpa
mod.lm.nw_gpa = lm(GRD_PNTS_PER_UNIT_NBR ~ ., data = master_df)
summary(mod.lm.nw_gpa)
# conduct f-test to compare gpa-only model to gpa + nw model
f_test_res = anova(mod.lm.gpa_only, mod.lm.nw_gpa)
row = data.frame(term = "ALL" , nw_only_r_squared = summary(mod.lm.nw_only)$r.squared, gpa_only_r_squared = summary(mod.lm.gpa_only)$r.squared, combined_r_squared = summary(mod.lm.nw_gpa)$r.squared, f_statistic = f_test_res[["F"]][2], f_test_p_val = f_test_res[["Pr(>F)"]][2])
rsquared_analysis_results = rbind(rsquared_analysis_results, row)
write.csv(rsquared_analysis_results, file =  "network_rsquared_analysis_results.csv", row.names = FALSE)

## ==============================================================================================================
## ==============================================================================================================

# same analayis but with single large dataframe for TRAINING TERMS ONLY; this mirrors the setup we use for predictive modeling and shows train-only data to show that there aren't differences between the train and test datasets.
master_df = NULL
for (i in seq_along(DATA_FP[-1])+1){
    fp = DATA_FP[i]
    df = read_data(fp, use_drop_thresh = FALSE)
    df %<>% dplyr::select(matches("^(ML_|B_L|CL_|PL_|GRD_PNTS_PER_UNIT_NBR|PREV_TERM_CUM_GPA)")) %>% na.omit 
    master_df = rbind(master_df, df)
}

# model with only network features
df_nw = master_df %>% dplyr::select(matches("^(ML_|B_L|CL_|PL_|GRD_PNTS_PER_UNIT_NBR)"))
mod.lm.nw_only = lm(GRD_PNTS_PER_UNIT_NBR ~ ., data = df_nw)
summary(mod.lm.nw_only)
# model with only previous term cumulative gpa
mod.lm.gpa_only = lm(GRD_PNTS_PER_UNIT_NBR ~ PREV_TERM_CUM_GPA, data = master_df)
summary(mod.lm.gpa_only)
# model with network + previous term cumulative gpa
mod.lm.nw_gpa = lm(GRD_PNTS_PER_UNIT_NBR ~ ., data = master_df)
summary(mod.lm.nw_gpa)
# conduct f-test to compare gpa-only model to gpa + nw model
f_test_res = anova(mod.lm.gpa_only, mod.lm.nw_gpa)
row = data.frame(term = "ALL_TRAIN" , nw_only_r_squared = summary(mod.lm.nw_only)$r.squared, gpa_only_r_squared = summary(mod.lm.gpa_only)$r.squared, combined_r_squared = summary(mod.lm.nw_gpa)$r.squared, f_statistic = f_test_res[["F"]][2], f_test_p_val = f_test_res[["Pr(>F)"]][2])
rsquared_analysis_results = rbind(rsquared_analysis_results, row)
write.csv(rsquared_analysis_results, file =  "network_rsquared_analysis_results.csv", row.names = FALSE)


