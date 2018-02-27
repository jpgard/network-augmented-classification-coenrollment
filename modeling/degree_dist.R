library(ggplot2)
library(dplyr)
library(stringr)
library(scales)

# https://stackoverflow.com/questions/11053899/how-to-get-a-reversed-log10-scale-in-ggplot2
reverselog_trans <- function(base = exp(1)) {
    trans <- function(x) -log(x, base)
    inv <- function(x) base^(-x)
    trans_new(paste0("reverselog-", format(base)), trans, inv, 
              log_breaks(base = base), 
              domain = c(1e-100, Inf))
}


#TODO: make this a null vector; no need to drop anytthing
DROP_COLS = c("X.SNPSHT_RPT_DT_x", "TERM_SHORT_DES_x", "X.SNPSHT_RPT_DT_y", "TERM_SHORT_DES_y", "GRD_BASIS_ENRL_CD", "GRD_BASIS_ENRL_DES", 
              "CLASS_LONG_DES", "CLASS_GRDD_IND", "CLASS_MTG_START_DT", "CLASS_MTG_END_DT", "CLASS_MTG_START_TM", "CLASS_MTG_END_TM", "MAX_UMPLC_MATH_TEST_SCR", "CLASS_NBR", "LAST_TERM_ATTND_CD")

source("preproc_utils.R")
data_dir = "../data/chris-network-feats/"
cumulative_dfs_list = list()
data_files = list.files(data_dir, full.names = T)
for (i in seq_along(data_files)){
    term_file = data_files[i]
    # fetch degree and gpa by student
    term = str_extract_all(term_file, "[[:digit:]]{4}")[[1]]
    message(paste0("evaluating term ", term))
    degree_df = read.csv(term_file)
    degree_df$X = as.factor(degree_df$X)
    term_df = read_data(paste0("../data/nw_", term, ".csv"), use_drop_thresh = F, drop_high_cardinality = F)
    term_sid_gpa = dplyr::select(term_df, one_of(c("PREV_TERM_CUM_GPA", "STDNT_ID")))
    merged_df = dplyr::inner_join(degree_df, term_sid_gpa, by = c("X" = "STDNT_ID")) %>% unique()
    merged_df$GPA_BIN = cut(merged_df$PREV_TERM_CUM_GPA, breaks = c(1, 2, 3, 4, 100))
    # plot degree distributions overall and by performance groups
    plt = ggplot(merged_df, aes(x = X0)) + geom_histogram(bins = 100) + ggtitle(paste0("Overall Degree Distribution \nTerm ", term)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) + xlab("Vertex Degree") + ylab("Count")
    ggsave(paste0("../output/img/degree_dist_overall_", term, ".pdf"), device = "pdf", width = 9, height = 3)
    plt = merged_df %>% na.omit() %>% ggplot(aes(x = X0)) + geom_histogram(bins = 100) + ggtitle(paste0("Degree Distribution By Performance Group \nTerm ", term)) + facet_grid(GPA_BIN ~ .) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5)) + xlab("Vertex Degree") + ylab("Count")
    ggsave(paste0("../output/img/degree_dist_performancegroup_", term, ".pdf"), device = "pdf", width = 7, height = 5)
    # cumulative degree distribution to demonstrate exponential dist of degree as in Newman 2003
    temp_df = merged_df %>% ddply(.(X0), summarize, n = length(X0)) %>% mutate(n_prop = (n/sum(n)))
    temp_df$cum_degree_dist = cumsum(temp_df$n_prop)
    temp_df$cum_degree_dist_1 = 1-temp_df$cum_degree_dist
    cumulative_dfs_list[[i]] <- temp_df
    plot(temp_df$X0, temp_df$cum_degree_dist, type = "l", ylim = c(1,0), main = "Cumulative Degree Distribution")
    # working plot; TODO: get x-axis flipped; wasn't working with log scale.
    plt = temp_df %>% ggplot(aes(x=X0, y = cum_degree_dist)) + geom_line() + annotation_logticks() + scale_x_log10() + scale_y_continuous(trans=reverselog_trans(10)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))  + xlab("Vertex Degree") + ylab("Cumulative Degree Distribution") + ggtitle(paste0("Cumulative Degree Distribution \n Term " , term))
    ggsave(paste0("../output/img/cumulative_degree_dist_", term, ".pdf"), device = "pdf", width = 4.2, height = 4)
}

#TODO: drop term 2070

# melt df
terms = NULL
for (i in seq_along(data_files)){
    term_file = data_files[i]
    term = str_extract_all(term_file, "[[:digit:]]{4}")[[1]]
    terms = c(terms, term)
}
names(cumulative_dfs_list) <- terms
temp = bind_rows(cumulative_dfs_list, .id = "term")
plt = temp %>% filter(term != "2070") %>% ggplot(aes(x=X0, y = cum_degree_dist, group = term)) + geom_line() + annotation_logticks() + scale_x_log10() + scale_y_continuous(trans=reverselog_trans(10)) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0.5))  + xlab("Vertex Degree") + ylab("Cumulative Degree Distribution") + ggtitle(paste0("Cumulative Degree Distribution (All Semesters)")) + theme(panel.border = element_rect(size = 2))
ggsave(paste0("../output/img/cumulative_degree_dist_ALL.pdf"), device = "pdf", width = 4.2, height = 4)
