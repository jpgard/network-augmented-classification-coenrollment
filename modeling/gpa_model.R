data_files =  list.files(pattern = "*.Rdata$")
for (f in data_files){
    load(f)
}

gpa_model_predict <- function(prev_term_cum_gpa){
    preds <- rep(NA, length(prev_term_cum_gpa))
    for (i in seq_along(preds)){
        gpa = prev_term_cum_gpa[i]
        if (gpa > 3.6) pred = "A"
        else if (gpa > 2.6) pred = "B"
        else if (gpa > 1.6) pred = "C"
        else pred = "D"
        preds[i] <- pred
    }
    return(preds)
}

res = gpa_model_predict(flat_test_data$PREV_TERM_CUM_GPA)
res = factor(res)
test_labs = flat_test_data$CRSE_GRD_OFFCL_CD
sum(res == test_labs)/length(test_labs)
fetch_multiclass_metrics(test_labs, res)
