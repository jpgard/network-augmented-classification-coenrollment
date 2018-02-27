# assortativity
library(stringr)
rows = list()
for (file in list.files("../data/chris-network-feats", full.names = TRUE)) {
    df = read.csv(file)
    term = str_extract_all(file, "[[:digit:]]{4}")[[1]]
    if (length(unique(df$assort)) == 1){
        assortativity_coef = df$assort[1]
    }
    else {
        message(paste0("warning; there is an inconsistent assortativity coefficient in file", file))
    }
    # apply t-test with n-2 degrees of freedom https://onlinecourses.science.psu.edu/stat501/node/259
    n = nrow(df)
    t = assortativity_coef*sqrt(n - 2)/(sqrt(1 - assortativity_coef^2))
    p = 2*pt(-abs(t), n-2)
    rows[[term]] = data.frame(term = term, assortativity_coefficient = assortativity_coef, t_statistic = t, p_val = p)
}
results = do.call("rbind", rows)
results
write.csv(results, file="../output/assortativity_coefficients.csv", row.names = FALSE)
