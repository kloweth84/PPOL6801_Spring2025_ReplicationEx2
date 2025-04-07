# PPOL6801_Spring2025_ReplicationEx2
**Repository for the Replication Exercise 2 part of the Text as Data course at Georgetown University**

Replication Exercise completed by Katharyn Loweth and Wendy Shi 

This repository contains the code, dataset, and visualization for replicating the work of Eshima, S., Imai, K., and Sasaki, T. on **Keyword-Assisted Topic Models**. Please see the following section that guides through what each folder contains. 

# Code:
- **Replication_code.qmd**: The main code for replication
- **Replication_code.html**: The compiled results
- **base_main.R**: The original code created by authors
- **common_utilities.R**: The original code created by authors that we were unable to run

# Datasets:
- **101_114_HR_S_true_topic.csv**: True topic for each congressional bill
- **bill_dfm.rds**: Pre-processed congresional bill (Main corpus)
- **bill_key_paper.rds**: Manually selected keywords for each topics

To run the replicated code, please also download the below files from Harvard Dataverse (not included due to file size):
- **bill_key_extraK0_iter3000_randnum1_paper.rds**: Author's keyATM model result random simulation 1
- **bill_key_extraK0_iter3000_randnum2_paper.rds**: Author's keyATM model result random simulation 2
- **bill_key_extraK0_iter3000_randnum3_paper.rds**: Author's keyATM model result random simulation 3
- **bill_key_extraK0_iter3000_randnum4_paper.rds**: Author's keyATM model result random simulation 4
- **bill_key_extraK0_iter3000_randnum5_paper.rds**: Author's keyATM model result random simulation 5
- **bill_lda_extraK0_iter3000_randnum1_paper**: Author's wLDA model result random simulation 1
- **bill_lda_extraK0_iter3000_randnum2_paper**: Author's wLDA model result random simulation 2
- **bill_lda_extraK0_iter3000_randnum3_paper**: Author's wLDA model result random simulation 3
- **bill_lda_extraK0_iter3000_randnum4_paper**: Author's wLDA model result random simulation 4
- **bill_lda_extraK0_iter3000_randnum5_paper**: Author's wLDA model result random simulation 5
  

# Visualizations:
- **01estimated_count_vish**.png: keyATM estinated topic distribution across corpus 
- **01keyword_vish.png**: Keywords distribution across the corpus
- **01keyATM_fit.pdf**： Log-likelyhood and Perplexity of model performance over iterations
- **01real_count_plot.png**: Real topic distribution accross the corpus
- **02RepExercise2_roccurve1.jpg**: ROC curve for replicated keyATM and wLDA model
- **02RepExercise2_roccurve2.jpg**: ROC curve for replicated keyATM and wLDA model compares to author's ROC curve
- **02RepExercise2_roccurve3.jpg**: ROC curve of the five replications of the keyATM model with randomly sampled keywords compares to replicated results with full keywords
- **03excel_key.xlsx**: Excel sheet used for making keywords table for replicated results
- **03foreign_trade.png**: keyATM and wLDA replicated words for Foreign Trade
- **03Replication_result.png**: KeyATM and wLDA model words identified for the six topics mentioned in the paper
- **03transportation.png**: keyATM and wLDA replicated words for Transportation

# Final Reports
- **RepEx2_report.pdf**: Written report for our replication
- **RepEx2_slidedeck.pdf**: Slide deck for the presentation

# Reference

Paper citation: Eshima, S., Imai, K. and Sasaki, T. (2024), Keyword-Assisted Topic Models. American Journal of Political Science, 68: 730-750. https://doi.org/10.1111/ajps.12779

Citation for Replication Data: Eshima, S., Imai, K., & Sasaki, T. (2023). Replication Data for: Keyword Assisted Topic Models (Version V1) [dataset]. Harvard Dataverse. https://doi.org/doi:10.7910/DVN/RKNNVL

https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/RKNNVL&version=1.0

Link to KeyATM R package information: https://keyatm.github.io/keyATM/
