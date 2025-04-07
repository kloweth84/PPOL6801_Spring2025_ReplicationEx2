# as of July 5th 2022

# files needed to run this script
# common
# data/base/bill_dfm.rds
# data/base/101_114_HR_S_true_topic.csv
# data/base/bill_key_paper.rds
# data/base/fitted/bill_lda_extraK0_iter3000_randnum[1-5]_paper.rds

# for type = "paper"
# data/base/fitted/bill_key_extraK0_iter3000_randnum[1-5]_paper.rds
# data/base/bill_key_paper.rds

# for type = "appendix"
# data/base/fitted/bill_key_extraK0_iter3000_randnum[1-5]_appendix.rds

################################
# Read packages and functions  #
################################
library(keyATM)
library(quanteda)
library(tidyverse)
library(multiROC)
library(xtable)
library(clue)
library(patchwork)

source("code/base_utilities.R")


########################
# Load common objects  #
########################
# Document and docid
docs <- readRDS("data/base/bill_dfm.rds")
docid <- docs@Dimnames$docs

# True topics
docid_subbed <- gsub(".txt", "", docid)
true_topic <- inner_join(tibble::tibble(docid = docid_subbed),
                         read_csv("data/base/101_114_HR_S_true_topic.csv"),  by = "docid")
docid_use_df <- tibble::tibble(docid = true_topic$docid)

K <- 21  # number of topics

## Bill labels
bill_label <- c("Macroeconomics", "Civil rights", "Health", "Agriculture",
                "Labor", "Education", "Environment", "Energy", "Immigration",
                "Transportation", "Law & crime", "Social welfare", "Housing",
                "Domestic commerce", "Defense", "Technology", "Foreign trade",
                "International affairs", "Government operations", "Public lands",
                "Culture")

#####################
# Document feature  #
#####################

dfm_use_df <- convert(docs, to = "data.frame")
doc_term_len <- apply(dfm_use_df[, -1], 1, sum)

cat(sprintf("Total documents: %d \n", nrow(docs)))
cat(sprintf("Unique term: %d \n", ncol(docs)))
cat(sprintf("On average %f terms per document \n", mean(doc_term_len)))
cat(sprintf("Max %d terms length  \n", max(doc_term_len)))
cat(sprintf("Min %d terms length  \n", min(doc_term_len)))
cat(sprintf("On average %f documents per session  \n", mean(table(gsub("th.*$", "", docid)))))

count_table <- data.frame(Topic_label = bill_label,
  Count = as.integer(apply(true_topic[, -1], 2, sum)),
  Percentage = round(apply(true_topic[, -1], 2, sum)/nrow(true_topic), 4) *  100 )

keys_list <- readRDS("data/base/bill_key_paper.rds")
names(keys_list) <- bill_label

cat(sprintf("On average %f keywords per topic \n", mean(sapply(keys_list, length))))

keyATM_doc <- keyATM_read(docs)
tmp <- prop_show(keyATM_docs = keyATM_doc, keywords = keys_list)

tmp %>% mutate(Topic_id = as.integer(gsub("Topic", "", Topic))) %>%
  group_by(Topic_id) %>%
  filter(Ranking <= 3) %>%
  select(Word) %>%
  group_split() %>%
  map(~select(., Word)) %>%
  map(~pull(.)) %>%
  map(~paste(., collapse = " ")) %>% flatten_chr(.) %>%
  data.frame(Keywords = ., stringsAsFactors =  FALSE) %>%
  bind_cols(., Topic_label = bill_label) %>%
  inner_join(., count_table, by = "Topic_label") %>%
  arrange(desc(Count)) %>%
  select(Topic_label, Count, Percentage, Keywords) -> count_table_paper

# Print Table 1 in paper
print(xtable(count_table_paper), include.rownames = FALSE)

# Create Table S1
tmp2 <- map_df(map(keys_list, ~paste(.x, collapse = " ")), ~as.data.frame(.x), .id = "Label")
tmp2 %>%
  left_join(count_table_paper %>%dplyr::select(Topic_label, Count), by = c("Label" = "Topic_label")) %>%
  arrange(desc(Count)) %>%
  dplyr::select(!Count) %>%
  rename(Keywords = .x) %>%
  xtable() %>%
  print(include.rownames = FALSE)


#####################################################
# Load objects for results in the main text (paper) #
#####################################################
## Load objects (let's start with paper)
type <- "paper"  # we have two sets of keywords, one for paper and the other for the appendix
loaded_data_key <- load_base_data(type) # replace = TRUE if necessary
loaded_data_lda <- load_base_data(type, model = "lda") # replace = TRUE if necessary

post_lda_list <- loaded_data_lda$post_lda_list
post_key_list <- loaded_data_key$post_key_list

post_lda_list_theta <- loaded_data_lda$post_lda_list_theta
post_key_list_theta <- loaded_data_key$post_key_list_theta

hung_list <- loaded_data_lda$hung_list

# Which chain to use
chain_med <- get_median_chain(post_key_list, post_lda_list)

# type == "paper"
# > chain_med
# $med_key
# [1] 3

# $med_lda
# [1] 5

# Median index
chain_med_key <- chain_med$med_key
chain_med_lda <- chain_med$med_lda


###################
# Draw ROC curves #
###################
# Merge all results
post_key_thetas <- bind_cols(post_key_list_theta)
post_lda_thetas <- bind_cols(post_lda_list_theta)

# Add true topic label
joined <- inner_join(true_topic %>%
                     rename_if(stringr::str_detect(names(.), "^X"), ~paste0(., "_true")),
                     post_key_thetas %>%
                     rename_if(stringr::str_detect(names(.), "docid...1"), ~gsub("docid...1", "docid", .)), by = 'docid') %>%
            inner_join(post_lda_thetas %>%
                     rename_if(stringr::str_detect(names(.), "docid...1"), ~gsub("docid...1", "docid", .)), by = "docid")


## implement
roc_res <- multi_roc(as.data.frame(joined), force_diag = TRUE)

## plot format
plot_roc_df <- plot_roc_data(roc_res)

# Save figures
ROC_by_topic(plot_roc_df, type = "paper")


########################
# ROC figure for paper #
# Figure 1             #
########################
ids_main <- c(5, 10, 17, 9, 11, 19)
ROC_plot_paper(plot_roc_df, ids_main, type = "paper", savename = "bill_base_roc_med") # replace


###############################
# ROC curves other topics     #
# Figures S1 in appendix #
###############################
## for AJPS
use_topics <- c(19, 20, 15, 14, 11, 3, 18, 10)
ROC_plot_ajps(plot_roc_df, use_topics, type = "paper", savename = "bill_base_roc_med_All1_ajps") # replace

use_topics <- c(1, 7, 6, 8, 16, 5, 17, 2)
ROC_plot_ajps(plot_roc_df, use_topics, type = "paper", savename = "bill_base_roc_med_All2_ajps") # replace

use_topics <- c(12, 4, 13, 9, 21)
ROC_plot_ajps(plot_roc_df, use_topics, type = "paper", savename = "bill_base_roc_med_All3_ajps") # replace


###########################
# Create top words tables #
###########################
roc_auc <- unlist(roc_res$AUC)
roc_auc_aggregate <- roc_auc[grepl("micro", names(roc_auc))]

sort(roc_auc_aggregate[grepl("key", names(roc_auc_aggregate))])
sort(roc_auc_aggregate[grepl("lda", names(roc_auc_aggregate))])

top_key <- top_words(post_key_list[[chain_med_key]], n = 20)
top_lda <- top_words_lda(post_lda_list[[chain_med_lda]], n = 20)

check_topic_list <- list(
                         c(5, 10, 17),
                         c(9, 11, 19),
                         c(19, 20, 15),
                         c(14, 11, 3),
                         c(18, 10, 1),
                         c(7, 6, 8),
                         c(16, 5, 17),
                         c(2, 12, 4),
                         c(13, 9, 21)
                        )

# Table 2 in paper and Table S2 in appendix
for (i in 1:length(check_topic_list)) {
  create_table(obj_key = post_key_list[[chain_med_key]],
               obj_lda = post_lda_list[[chain_med_lda]],
               label_name = bill_label,
               topic_num = check_topic_list[[i]],
               filename_add = type)
               # filename_add = paste0(type, "_replace"))
}


###########################
# Proportion of documents #
###########################
# use bill_key_paper.rds inside the function
# Figure S3
prop_documents(ids_main, compare_to = "Government operations", type, model = "base")


###########################
# ROC with pooled results #
###########################
# Figure S2
joined %>%
  dplyr::select(!starts_with("docid...")) %>%
  pivot_longer(cols = !c("docid", paste0("X", 1:21, "_true")),
               names_to = c(".value", "set"),
               names_pattern = "(X[0-9].*?_pred_[a-z]{3})_(rand[0-9].*?)") -> joined_pooled

# Get ROC
roc_res_pooled <- multi_roc(as.data.frame(joined_pooled), force_diag = T)
plot_roc_pooled_df <- plot_roc_data(roc_res_pooled)

## for AJPS
use_topics <- c(19, 20, 15, 14, 11, 3, 18, 10)
ROC_plot_ajps(plot_roc_pooled_df, use_topics, type = "paper", savename = "bill_base_roc_med_All1_pooled_ajps", pooled = 1) # replace

use_topics <- c(1, 7, 6, 8, 16, 5, 17, 2)
ROC_plot_ajps(plot_roc_pooled_df, use_topics, type = "paper", savename = "bill_base_roc_med_All2_pooled_ajps", pooled = 1) # replace

use_topics <- c(12, 4, 13, 9, 21)
ROC_plot_ajps(plot_roc_pooled_df, use_topics, type = "paper", savename = "bill_base_roc_med_All3_pooled_ajps", pooled = 1) # replace


#########################
# Different keyword set #
#  results in appendix  #
#########################
# Create Table S3
keys_list_appendix <- readRDS("data/base/bill_key_appendix.rds")
tmp3 <- map_df(map(keys_list_appendix, ~paste(.x, collapse = " ")), ~as.data.frame(.x), .id = "Label")
tmp3 %>%
  left_join(count_table_paper %>%dplyr::select(Topic_label, Count), by = c("Label" = "Topic_label")) %>%
  arrange(desc(Count)) %>%
  dplyr::select(!Count) %>%
  rename(Keywords = .x) %>%
  xtable() %>%
  print(include.rownames = FALSE)


# analysis
type <- "appendix"
loaded_data_key_appendix <- load_base_data(type)

post_key_list <- loaded_data_key_appendix$post_key_list
post_key_list_theta <- loaded_data_key_appendix$post_key_list_theta

# Which chain to use
chain_med <- get_median_chain(post_key_list, post_lda_list)

# $med_key
# [1] 1

# $med_lda
# [1] 5

# Median index
chain_med_key <- chain_med$med_key
chain_med_lda <- chain_med$med_lda

# Merge all results
post_key_thetas <- bind_cols(post_key_list_theta)
post_lda_thetas <- bind_cols(post_lda_list_theta)

# Add true topic label
joined <- inner_join(true_topic %>%
                     rename_if(stringr::str_detect(names(.), "^X"), ~paste0(., "_true")) ,
                     post_key_thetas %>%
                     rename_if(stringr::str_detect(names(.), "docid...1"), ~gsub("docid...1", "docid", .)), by = 'docid') %>%
            inner_join(post_lda_thetas %>%
                     rename_if(stringr::str_detect(names(.), "docid...1"), ~gsub("docid...1", "docid", .)), by = "docid")

# Get ROC
roc_res <- multi_roc(as.data.frame(joined), force_diag = T)
plot_roc_df <- plot_roc_data(roc_res)

# ROC Curves
# Figure S4
use_topics <- c(19, 20, 15, 14, 11, 3, 18, 10)
ROC_plot_ajps(plot_roc_df, use_topics, type = "appendix", savename = "bill_base_roc_med_All1_ajps")

use_topics <- c(1, 7, 6, 8, 16, 5, 17, 2)
ROC_plot_ajps(plot_roc_df, use_topics, type = "appendix", savename = "bill_base_roc_med_All2_ajps")

use_topics <- c(12, 4, 13, 9, 21)
ROC_plot_ajps(plot_roc_df, use_topics, type = "appendix", savename = "bill_base_roc_med_All3_ajps")


top_key <- top_words(post_key_list[[chain_med_key]], n = 20)
top_lda <- top_words_lda(post_lda_list[[chain_med_lda]], n = 20)


check_topic_list <- list(
                         c(19, 20, 15),
                         c(14, 11, 3),
                         c(18, 10, 1),
                         c(7, 6, 8),
                         c(16, 5, 17),
                         c(2, 12, 4),
                         c(13, 9, 21)
                        )

# Table S4
for (i in 1:length(check_topic_list)) {
  create_table(obj_key = post_key_list[[chain_med_key]],
               obj_lda = post_lda_list[[chain_med_lda]],
               label_name = bill_label,
               topic_num = check_topic_list[[i]],
               filename_add = type)
}



######################################################################################
# END
######################################################################################
