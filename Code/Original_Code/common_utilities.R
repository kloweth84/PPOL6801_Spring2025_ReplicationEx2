# Functions used in the main file.
source("code/common_utilities.R")

##
## Load objects
##
load_base_data <- function(type, replace = FALSE, model = c("key", "lda"), load_saved = FALSE) {

  # Check type
  model <- match.arg(model)
  type <- paste0("_", type)

  if (model == "key") {
    if (load_saved & file.exists(paste0("data/base/fitted/loaded", type, "_key.rds"))) {
      obj <- readRDS(paste0("data/base/fitted/loaded", type, "_key.rds"))
      message("Use the saved object, `loaded_key.rds`")
      return(obj)
    }

    post_key_list <- list()
    post_key_list_theta <- list()

    for (i in 1:5) {
      path <- paste0("data/base/fitted/bill_key_extraK0_iter3000_randnum", i, type, ".rds")
      fitted <- readRDS(path)
      post_key_list[[i]] <- fitted
      theta <- bind_cols(list(docid_use_df, tibble::as_tibble(fitted$theta)))
      colnames(theta)[-1] <- paste0("X", c(1:K), "_pred_key_rand", i)
      post_key_list_theta[[i]] <- theta
    }

    obj <- list(post_key_list = post_key_list,
                post_key_list_theta = post_key_list_theta
                )

    if (load_saved) {
      saveRDS(obj, paste0("data/base/fitted/loaded", type, "_key.rds"))
    }

    return(obj)

  } else {
    if (load_saved & file.exists(paste0("data/base/fitted/loaded", type, "_lda.rds"))) {
      obj <- readRDS(paste0("data/base/fitted/loaded", type, "_lda.rds"))
      message("Use the saved object, `loaded_lda.rds`")
      return(obj)
    }

    post_lda_list <- list()
    post_lda_list_theta <- list()
    hung_list <- list()
    message("Matching topics, it may take time for the first time...")

    for (i in 1:5) {
      if (replace == FALSE) {
        filename <- paste0("output/base/tmp/bill_lda_extraK0_iter3000_randnum", i, type, ".rds")
      } else {
        filename <- paste0("output/base/tmp/bill_lda_extraK0_iter3000_randnum", i, type, "_replace.rds")
      }


      if (check_file_existence(filename)) {
        message("`rds` already exists. Load: ", filename)
        tmp <- readRDS(filename)
        post_lda_list[[i]] <- tmp
        post_lda_list_theta[[i]] <- bind_cols(docid = docid_use_df, data.frame(tmp$theta))
        colnames(post_lda_list_theta[[i]])[-1] <- paste0("X", c(1:K), "_pred_lda_rand", i)

        next
      }

      if (replace == FALSE) {
        path_lda <- paste0("data/base/fitted/bill_lda_extraK0_iter3000_randnum", i, "_paper.rds")
      } else {
        path_lda <- paste0("data/base/fitted/bill_lda_extraK0_iter3000_randnum", i, "_replace.rds")
      }
      fitted_lda <- readRDS(path_lda)

      temp_lda_theta <- bind_cols(list(docid_use_df, tibble::as_tibble(fitted_lda$theta)))
      hung_lda_tmp <- match_auc_multi(postl_theta_join_ = temp_lda_theta, true_topic_ = true_topic, replace_ = replace)
      hung_list[[i]] <- hung_lda_tmp

      lda_phi_update <- fitted_lda$phi[as.vector(hung_list[[i]][[1]]), ]
      lda_theta_update <- fitted_lda$theta[, as.vector(hung_list[[i]][[1]])]

      post_lda_list_theta[[i]] <- bind_cols(docid = docid_use_df, data.frame(lda_theta_update))
      post_lda_list_theta[[i]] <- post_lda_list_theta[[i]][, c(1:(K+1))]
      colnames(post_lda_list_theta[[i]])[-1] <- paste0("X", c(1:K), "_pred_lda_rand", i)

      tmp_lda_update <- list(theta = lda_theta_update, phi = lda_phi_update, hung = hung_lda_tmp,
                             model_fit = fitted_lda$model_fit)
      post_lda_list[[i]] <- tmp_lda_update
      saveRDS(tmp_lda_update, file = filename)

      print(paste0("Done: ", i, "/5"))
    }

    # options(warn=-1)

    obj <- list(post_lda_list = post_lda_list,
                post_lda_list_theta = post_lda_list_theta,
                hung_list = hung_list
                )

    if (load_saved) {
      saveRDS(obj, paste0("data/base/fitted/loaded", type, "_lda.rds"))
    }

    return(obj)
  }
}


get_median_chain <- function(post_key_list, post_lda_list) {
  # Get the median among the chain

  # keyATM ppl
  key_med <- rep(NA, length(post_key_list))
  lda_med <- rep(NA, length(post_lda_list))
  for (i in 1:length(post_key_list)) {
    key_med[i] <- tail(post_key_list[[i]]$model_fit$Perplexity, 1)
    lda_med[i] <- tail(post_lda_list[[i]]$model_fit$Perplexity, 1)
  }

  ppl <- tibble::tibble(
                          keyATM = key_med,
                          LDA    = lda_med,
                          Chain  = 1:length(key_med)
                        )


  ppl %>%
    pivot_longer(-Chain, names_to = "Model", values_to = "Perplexity") -> ppl

  # keyATM
  ppl %>%
    filter(Model == "keyATM") %>%
    arrange(Chain) %>%
    pull(Perplexity) -> ppl_key

  chain_med_key <- (1:length(ppl_key))[ppl_key == median(ppl_key)]


  # LDA
  ppl %>%
    filter(Model == "LDA") %>%
    arrange(Chain) %>%
    pull(Perplexity) -> ppl_lda

  chain_med_lda <- (1:length(ppl_lda))[ppl_lda == median(ppl_lda)]

  return(list(med_key = chain_med_key, med_lda = chain_med_lda))

}




##
## ROC curves
##
ROC_by_topic <- function(plot_roc_df, type, replace = FALSE) {

  # Check type
  type <- paste0(type, "_")

  for (i in 1:21) {
    if (replace == FALSE) {
      plot_roc_df$Method2 <- ifelse( grepl("key", as.character(plot_roc_df$Method) ), "keyATM", "wLDA")
      plot_roc_df$Method2 <- factor(plot_roc_df$Method2)
      ggplot() +
          geom_abline(intercept = 0, slope = 1, colour='black', linetype = 'dashed') +
          geom_line(data = plot_roc_df %>% filter(Method2 == "wLDA" & Group %in% paste0("X", i)),
                    aes(x = 1-Specificity, y=Sensitivity, colour = "#c4c4c4", linetype = Method), size=.8) +
          geom_line(data = plot_roc_df %>% filter(Method2 == "keyATM" & Group %in% paste0("X", i)),
                    aes(x = 1-Specificity, y=Sensitivity, colour = "#3391e8", linetype = Method), size=.8) +
          scale_linetype_manual(values = c(rep("solid", 10)), guide="none") +
          scale_color_manual(values = c("#3391e8", "#c4c4c4"), labels = c("keyATM", "wLDA"), name = "Model") +
          scale_y_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
          scale_x_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
          theme_bw() +
          form1 +
          xlab("False Positive Rate") +
          ylab("True Positive Rate") +
          ggtitle(bill_label[i]) +
          theme(plot.title = element_text(hjust = 0.5),
                legend.justification=c(1, 0), legend.position=c(.98, .02),
            legend.background = element_rect(fill=NULL, size=.8, linetype="solid")) -> p
      savename <- paste0("output/base/ROC_by_topic/", type, "Topic_", i, ".png")

      } else {
        plot_roc_df$Method2 <- ifelse( grepl("key", as.character(plot_roc_df$Method) ), "keyATM",
                                      ifelse( grepl("lda2", as.character(plot_roc_df$Method)), "LDA2",
                                      "LDA"))
        plot_roc_df$Method2 <- factor(plot_roc_df$Method2)
        ggplot() +
          geom_abline(intercept = 0, slope = 1, colour='black', linetype = 'dashed') +
          geom_line(data = plot_roc_df %>% filter(Method2 == "LDA" & Group %in% paste0("X", i)),
                    aes(x = 1-Specificity, y=Sensitivity, colour = "#c4c4c4", linetype = Method), size=.8) +
          geom_line(data = plot_roc_df %>% filter(Method2 == "LDA2" & Group %in% paste0("X", i)),
                    aes(x = 1-Specificity, y=Sensitivity, colour = "black", linetype = Method), size=.8) +
          geom_line(data = plot_roc_df %>% filter(Method2 == "keyATM" & Group %in% paste0("X", i)),
                    aes(x = 1-Specificity, y=Sensitivity, colour = "#3391e8", linetype = Method), size=.8) +
          scale_linetype_manual(values = c(rep("solid", 15)), guide="none") +
          scale_color_manual(values = c("#3391e8", "#c4c4c4", "black"), labels = c("keyATM", "LDA", "LDA2"), name = "Model") +
          scale_y_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
          scale_x_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
          theme_bw() +
          form1 +
          xlab("False Positive Rate") +
          ylab("True Positive Rate") +
          ggtitle(bill_label[i]) +
          theme(plot.title = element_text(hjust = 0.5),
                legend.justification=c(1, 0), legend.position=c(.98, .04),
            legend.background = element_rect(fill=NULL, size=.8, linetype="solid")) -> p
      savename <- paste0("output/base/ROC_by_topic/", type, "Topic_", i, "_replace.png")
      }
    ggsave(savename, p, width = 5, height = 5.5)
  }

}


ROC_plot_paper <- function(plot_roc_df, use_ids, type, savename) {

  # Check type
  type <- paste0(type, "_")

  # Get labels
  use_ids_X <- paste0("X", use_ids)
  use_labels <- bill_label[use_ids]

  plot_roc_df$Method2 <- ifelse( grepl("key", as.character(plot_roc_df$Method) ), "keyATM", "wLDA")
  plot_roc_df$Method2 <- factor(plot_roc_df$Method2)

  if (length(use_ids) == 6) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5],
     use_labels[6])
    names(use_label_id) <- use_ids_X
  } else {
    stop("The number of topics not applicable")
  }

  # Get objects
  plot_roc_df2 <- plot_six_figs_get_obj(plot_roc_df, use_ids_X)
  median_auc <- plot_six_figs_get_auc(plot_roc_df2)
  dat_text <- plot_six_figs_create_auc_df(median_auc, use_ids_X)
  dat_text$Group2 <- factor(dat_text$Group2, levels = as.character(dat_text$Group2))
  # p <- plot_six_figs(plot_roc_df2, dat_text, use_label_id)

  plot_roc_df2$Method4 <- factor(ifelse(plot_roc_df2$Method2 == "wLDA", "wLDA", "keyATM"), levels = c("keyATM", "wLDA"))

  plot_roc_df2$Group2 <- factor(unlist(use_label_id[plot_roc_df2$Group2], use.names = FALSE),
                       levels = unlist(use_label_id, use.names = F))
  dat_text$Group2 <- factor(unlist(use_label_id[dat_text$Group2], use.names = FALSE),
                            levels = unlist(use_label_id, use.names = F))

  if (length(use_ids) == 6) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.30, .645),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else {
    stop("Check the number of topics you selected.")
  }

  ggplot() +
    geom_abline(intercept = 0, slope = 1, colour='black', linetype = 'dashed') +
    geom_line(data = plot_roc_df2 %>% filter(Method4 == "wLDA"),
              aes(x = 1-Specificity, y=Sensitivity, colour = "#7a7a7a", linetype = Method), size=.8) +
    geom_line(data = plot_roc_df2 %>% filter(Method4 == "keyATM"),
              aes(x = 1-Specificity, y=Sensitivity, colour = "#0f00ca", linetype = Method), size=.8) +
    scale_linetype_manual(values = c(rep("solid", 10)), guide="none") +
    scale_color_manual(values = c("#3391e8", "#c4c4c4"), labels = c("keyATM", "wLDA")) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
    theme_bw() +
    form1 +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    add_theme +
    facet_wrap(. ~ Group2, scales = "free") +
    geom_text(
       data    = dat_text,
       mapping = aes(x = x, y = y, label = label),
       size = 5,
       hjust = 0) -> p

  # Save
  if (length(use_ids) == 6) {
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 15, height = 11.5)
  } else {
    stop("The number of topics not applicable")
  }

}

ROC_plot <- function(plot_roc_df, use_ids, type, savename) {

  # Check type
  # type <- match.arg(type)
  type <- paste0(type, "_")

  # Get labels
  use_ids_X <- paste0("X", use_ids)
  use_labels <- bill_label[use_ids]

  plot_roc_df$Method2 <- ifelse( grepl("key", as.character(plot_roc_df$Method) ), "keyATM", "wLDA")
  plot_roc_df$Method2 <- factor(plot_roc_df$Method2)

  if (length(use_ids) == 6) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5],
     use_labels[6])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 3) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 8) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5],
     use_labels[6],
     use_labels[7],
     use_labels[8])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 5) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5])
  } else {
    stop("The number of topics not applicable")
  }


  # Get objects
  plot_roc_df2 <- plot_six_figs_get_obj(plot_roc_df, use_ids_X)
  median_auc <- plot_six_figs_get_auc(plot_roc_df2)
  dat_text <- plot_six_figs_create_auc_df(median_auc, use_ids_X)
  dat_text$Group2 <- factor(dat_text$Group2, levels = as.character(dat_text$Group2))
  # p <- plot_six_figs(plot_roc_df2, dat_text, use_label_id)

  plot_roc_df2$Method4 <- factor(ifelse(plot_roc_df2$Method2 == "wLDA", "wLDA", "keyATM"), levels = c("keyATM", "wLDA"))

  plot_roc_df2$Group2 <- factor(unlist(use_label_id[plot_roc_df2$Group2], use.names = FALSE),
                       levels = unlist(use_label_id, use.names = F))
  dat_text$Group2 <- factor(unlist(use_label_id[dat_text$Group2], use.names = FALSE),
                            levels = unlist(use_label_id, use.names = F))

  if (length(use_ids) == 6) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.30, .645),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 3) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.30, .236),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 8) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.46, .84),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 5) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.46, .78),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else {
    stop("Check the number of topics you selected.")
  }

  ggplot() +
    geom_abline(intercept = 0, slope = 1, colour='black', linetype = 'dashed') +
    geom_line(data = plot_roc_df2 %>% filter(Method4 == "wLDA"),
              aes(x = 1-Specificity, y=Sensitivity, colour = "#7a7a7a", linetype = Method), size=.8) +
    geom_line(data = plot_roc_df2 %>% filter(Method4 == "keyATM"),
              aes(x = 1-Specificity, y=Sensitivity, colour = "#0f00ca", linetype = Method), size=.8) +
    scale_linetype_manual(values = c(rep("solid", 10)), guide="none") +
    scale_color_manual(values = c("#3391e8", "#c4c4c4"), labels = c("keyATM", "wLDA")) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
    theme_bw() +
    form1 +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    add_theme +
    facet_wrap(. ~ Group2, scales = "free", ncol = 2) +
    geom_text(
       data    = dat_text,
       mapping = aes(x = x, y = y, label = label),
       size = 5,
       hjust = 0) -> p

  # Save
  if (length(use_ids) == 6) {
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 15, height = 11.5)
  } else if (length(use_ids) == 3) {
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 15, height = 5.75)
  } else if (length(use_ids) == 8){
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 10, height = 21)
  } else if (length(use_ids) == 5){
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 10, height = 16)
  } else {
    stop("The number of topics not applicable")
  }

}


ROC_plot_vb <- function(plot_roc_df, use_ids, type, savename) {

  # Check type
  # type <- match.arg(type)
  type <- paste0(type, "_")

  # Get labels
  use_ids_X <- paste0("X", use_ids)
  use_labels <- bill_label[use_ids]

  plot_roc_df$Method2 <- ifelse( grepl("vb", as.character(plot_roc_df$Method) ), "VB", "Gibbs")
  plot_roc_df$Method2 <- factor(plot_roc_df$Method2)

  if (length(use_ids) == 6) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5],
     use_labels[6])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 3) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 8) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5],
     use_labels[6],
     use_labels[7],
     use_labels[8])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 5) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5])
  } else {
    stop("The number of topics not applicable")
  }

  plot_six_figs_create_auc_df_vb <- function(.median_auc, use_ids_X) {
  # Argument: .median_auc = median_auc, .use_ids_X = use_ids_X

    len <- length(use_ids_X)
    label_use <- character(len)
    for (i in 1:len) {
      j <- 2 * i
      label_key <- paste0("Gibbs: ", format(round(.median_auc[(j-1), 3], 2)[[1]], nsmall=2))
      label_lda <- paste0("VB: ", format(round(.median_auc[j, 3], 2)[[1]], nsmall=2))
      label_use[i] <- paste("Median AUROC", label_key, label_lda, sep = "\n")
    }

    ret <- data.frame(
      label = label_use,
      Group2 = use_ids_X,
      # Group2 = c("X3", "X5", "X9", "X11", "X12", "X19"),
      x = c(rep(0.65, len)),
      y = c(rep(0.09, len))
    )
    return(ret)
  }

  # Get objects
  plot_roc_df2 <- plot_six_figs_get_obj(plot_roc_df, use_ids_X)
  median_auc <- plot_six_figs_get_auc(plot_roc_df2)
  dat_text <- plot_six_figs_create_auc_df_vb(median_auc, use_ids_X)
  dat_text$Group2 <- factor(dat_text$Group2, levels = as.character(dat_text$Group2))
  # p <- plot_six_figs(plot_roc_df2, dat_text, use_label_id)

  plot_roc_df2$Method4 <- factor(ifelse(plot_roc_df2$Method2 == "VB", "VB", "Gibbs"),
                                 levels = c("Gibbs", "VB"))

  plot_roc_df2$Group2 <- factor(unlist(use_label_id[plot_roc_df2$Group2], use.names = FALSE),
                       levels = unlist(use_label_id, use.names = F))
  dat_text$Group2 <- factor(unlist(use_label_id[dat_text$Group2], use.names = FALSE),
                            levels = unlist(use_label_id, use.names = F))

  if (length(use_ids) == 6) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.30, .645),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 3) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.30, .236),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 8) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.46, .84),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 5) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.46, .78),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else {
    stop("Check the number of topics you selected.")
  }

  ggplot() +
    geom_abline(intercept = 0, slope = 1, colour='black', linetype = 'dashed') +
    geom_line(data = plot_roc_df2 %>% filter(Method4 == "VB"),
              aes(x = 1-Specificity, y=Sensitivity, colour = "#7a7a7a", linetype = Method), size=.8) +
    geom_line(data = plot_roc_df2 %>% filter(Method4 == "Gibbs"),
              aes(x = 1-Specificity, y=Sensitivity, colour = "#0f00ca", linetype = Method), size=.8) +
    scale_linetype_manual(values = c(rep("solid", 10)), guide="none") +
    scale_color_manual(values = c("#3391e8", "#c4c4c4"), labels = c("Gibbs", "VB")) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
    theme_bw() +
    form1 +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    add_theme +
    facet_wrap(. ~ Group2, scales = "free", ncol = 2) +
    geom_text(
       data    = dat_text,
       mapping = aes(x = x, y = y, label = label),
       size = 5,
       hjust = 0) -> p

  # Save
  if (length(use_ids) == 6) {
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 15, height = 11.5)
  } else if (length(use_ids) == 3) {
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 15, height = 5.75)
  } else if (length(use_ids) == 8){
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 10, height = 21)
  } else if (length(use_ids) == 5){
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 10, height = 16)
  } else {
    stop("The number of topics not applicable")
  }

}



ROC_plot_ajps <- function(plot_roc_df, use_ids, type, savename, pooled = 0) {

  # Check type
  # type <- match.arg(type)
  type <- paste0(type, "_")

  # Get labels
  use_ids_X <- paste0("X", use_ids)
  use_labels <- bill_label[use_ids]

  plot_roc_df$Method2 <- ifelse( grepl("key", as.character(plot_roc_df$Method) ), "keyATM", "wLDA")
  plot_roc_df$Method2 <- factor(plot_roc_df$Method2)

  if (length(use_ids) == 6) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5],
     use_labels[6])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 3) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 8) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5],
     use_labels[6],
     use_labels[7],
     use_labels[8])
    names(use_label_id) <- use_ids_X
  } else if (length(use_ids) == 5) {
    use_label_id <- list(use_labels[1],
     use_labels[2],
     use_labels[3],
     use_labels[4],
     use_labels[5])
  } else {
    stop("The number of topics not applicable")
  }


  # Get objects
  plot_roc_df2 <- plot_six_figs_get_obj(plot_roc_df, use_ids_X)
  median_auc <- plot_six_figs_get_auc(plot_roc_df2)
  dat_text <- plot_six_figs_create_auc_df(median_auc, use_ids_X, pooled = pooled)
  dat_text$Group2 <- factor(dat_text$Group2, levels = as.character(dat_text$Group2))
  # p <- plot_six_figs(plot_roc_df2, dat_text, use_label_id)

  plot_roc_df2$Method4 <- factor(ifelse(plot_roc_df2$Method2 == "wLDA", "wLDA", "keyATM"), levels = c("keyATM", "wLDA"))

  plot_roc_df2$Group2 <- factor(unlist(use_label_id[plot_roc_df2$Group2], use.names = FALSE),
                       levels = unlist(use_label_id, use.names = F))
  dat_text$Group2 <- factor(unlist(use_label_id[dat_text$Group2], use.names = FALSE),
                            levels = unlist(use_label_id, use.names = F))

  if (length(use_ids) == 6) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.30, .645),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 3) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.30, .236),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 8) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.22, .66),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else if (length(use_ids) == 5) {
    add_theme <- theme(axis.text=element_text(color="black",size=22),
         axis.text.y=element_text(color="black",size=22),
         axis.title=element_text(color="black",size=24),
         legend.text = element_text(size=22),
         legend.justification=c(1, 0), legend.position=c(.3, .655),
         legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))
  } else {
    stop("Check the number of topics you selected.")
  }

  ggplot() +
    geom_abline(intercept = 0, slope = 1, colour='black', linetype = 'dashed') +
    geom_line(data = plot_roc_df2 %>% filter(Method4 == "wLDA"),
              aes(x = 1-Specificity, y=Sensitivity, colour = "#7a7a7a", linetype = Method), size=.8) +
    geom_line(data = plot_roc_df2 %>% filter(Method4 == "keyATM"),
              aes(x = 1-Specificity, y=Sensitivity, colour = "#0f00ca", linetype = Method), size=.8) +
    scale_linetype_manual(values = c(rep("solid", 10)), guide="none") +
    scale_color_manual(values = c("#3391e8", "#c4c4c4"), labels = c("keyATM", "wLDA")) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
    scale_x_continuous(limits = c(0, 1), breaks = c(0, .2, .4, .6, .8, 1)) +
    theme_bw() +
    form1 +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    add_theme +
    # facet_wrap(. ~ Group2, scales = "free", ncol = 4) +
    geom_text(
       data    = dat_text,
       mapping = aes(x = x, y = y, label = label),
       size = 5,
       hjust = 0) -> p

  # Save
  if (length(use_ids) == 6) {
    p + facet_wrap(. ~ Group2, scales = "free", ncol = 3) -> p
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 15, height = 11.5)
  } else if (length(use_ids) == 3) {
    p + facet_wrap(. ~ Group2, scales = "free", ncol = 3) -> p
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 15, height = 5.75)
  } else if (length(use_ids) == 8) {
    p + facet_wrap(. ~ Group2, scales = "free", ncol = 4) -> p
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 20, height = 10.5)
  } else if (length(use_ids) == 5) {
    p + facet_wrap(. ~ Group2, scales = "free", ncol = 3) -> p
    ggsave(filename = paste0("output/base/", type, savename, ".png"),
           p, width = 15, height = 10.5)
  } else {
    stop("The number of topics not applicable")
  }

}



###########################
# Proportion of documents #
###########################

prop_documents <- function(use_ids, compare_to, type, model) {

  # Check type
  # type <- match.arg(type)
  type <- paste0(type, "_")

  # Get labels
  use_ids_X <- paste0("X", use_ids)
  use_labels <- bill_label[use_ids]

  # Vocabulary
  vocabulary <- colnames(docs)

  # Keywords
  seed_list <- readRDS("data/base/bill_key_paper.rds")

  text_df <- tibble::tibble(
                          text_split =
                            apply(docs, 1,
                                   function(x){
                                    return(rep(vocabulary, x))
                                   }
                                )
                         )

  names(text_df$text_split) <- NULL
  text_df2 <- bind_cols(text_df, data.frame(docid = docid_subbed))
  text_df3 <- inner_join(text_df2, true_topic, by = "docid")

  keys_in_topic_list <- list()
  j <- 1
  for (i in use_ids){
    keys_in_topic_list[[j]] <- check_text_keys(text = text_df3, topic_id = i, keys = seed_list, model = model)
    j <- j + 1
  }

  uniq_keys_df2 <- data.frame(table(keys_in_topic_list[[1]]$uniq)/nrow(keys_in_topic_list[[1]]),
    id = use_labels[1])
  prop_keys_df2 <- data.frame(prop = keys_in_topic_list[[1]]$prop_keys,
    id = use_labels[1])

  for (i in 2:6){
    tmp <- keys_in_topic_list[[i]]
    .id <- use_labels[i]
    uniq_keys_df2 <- rbind(uniq_keys_df2, data.frame(table(tmp$uniq)/nrow(tmp), id = .id))
    prop_keys_df2 <- rbind(prop_keys_df2, data.frame(prop = tmp$prop_keys, id = .id))
  }

  uniq_keys_df2$id <- factor(uniq_keys_df2$id)
  uniq_keys_df2$id2 <- ifelse(uniq_keys_df2$id == compare_to, compare_to, "Other topics")
  uniq_keys_df2$id2 <- factor(uniq_keys_df2$id2, levels = c(compare_to, "Other topics"))

  prop_keys_df2$id <- factor(prop_keys_df2$id)
  prop_keys_df2$id2 <- ifelse(prop_keys_df2$id == compare_to, compare_to, "Other topics")
  prop_keys_df2$id2 <- factor(prop_keys_df2$id2, levels = c(compare_to, "Other topics"))


  ## density plot: keywords per each bill
  p_density <- ggplot(prop_keys_df2) +
    stat_density(aes(x = prop, color = id2), geom="line", position = "identity", size = 1.7) +
    form0 +
    theme(legend.position=c(.7, .8)) +
    scale_x_continuous(breaks=c(.1, .2), limits = c(0, 0.25), name = "Proportion of keywords in each bill", expand = c(0, 0)) +
    scale_y_continuous(name = "Density", expand = c(0, 0), limits = c(0, 30), breaks = c(0, 10, 20)) +
    scale_color_manual(values = c("#3391e8", "gray60"))
  # prop_pp2
  ggsave(filename = paste0("output/base/", type, "bill_prop_keys_per_doc_density.png"),
         p_density, width = 6.2, height = 6.5)

  ## bar plot
  uniq_keys_df2 %>% filter(id2 == "Other topics") %>% group_by(Var1) %>%
    mutate(Freq = mean(Freq)) %>% select(-id) %>% distinct() %>%
    bind_rows(uniq_keys_df2 %>% filter(id2 == compare_to) %>% select(Var1, Freq, id2)) -> uniq_keys_df3

  uniq_keys_df3 <- uniq_keys_df3 %>%
                      ungroup() %>%
                      mutate(Var1 = as.numeric(as.character(Var1)) ) %>%
                      add_row(Var1 = 20, Freq = as.numeric(0.0), id2 = "Other topics")
  uniq_keys_df3$Freq <- as.numeric(uniq_keys_df3$Freq)
  uniq_keys_df3$id2 <- factor(uniq_keys_df3$id2, levels = c(compare_to, "Other topics"))

  uniq_keys_df3$Var1 <- as.character(uniq_keys_df3$Var1)
  uniq_keys_df3$Var1 <- as.numeric(uniq_keys_df3$Var1)
  uniq_keys_df3$Var1 <- factor(uniq_keys_df3$Var1)

  p_bar <- ggplot(uniq_keys_df3) +
    geom_col(aes(x = Var1, y = Freq, fill = id2), position = position_dodge2(width = 0, preserve = "single"), width = 0.8) +
    form0 +
    scale_x_discrete(breaks = c(0, 5, 10, 15, 20, 25), expand = c(.05,.05), name = "Number of unique keywords per bill" ) +
    scale_y_continuous(expand = c(0, 0), name = "Proportion", limits = c(0, 0.3), breaks = c(0, 0.1, 0.2)) +
    theme(legend.position=c(.7, .8)) +
    scale_fill_manual(values = c("#3391e8", "#c4c4c4"), guide = "none")
  ggsave(filename = paste0("output/base/", type, "bill_prop_keys_per_doc_bar.png"),
         p_bar, width = 6.2, height = 6.5)

  p_both <- p_density + p_bar # density and bar
  ggsave(filename = paste0("output/base/", type, "bill_prop_keys_per_doc_both.png"),
         p_both, width = 12.5, height = 6.5)
}



###########
# plot pi #
###########

pi_plot <- function(pi_obj, use_ids, compare_to, use_iter)
{
  # use labels
  use_ids_X <- paste0("X", use_ids)
  use_labels <- bill_label[use_ids]
  compare_id <- paste0("X", compare_to)

  # remove burn-ins: use last iterations specified by use_iter
  pi_obj <- tail(pi_obj, use_iter)

  # create matrix
  pi_obj_mat <- do.call(rbind, pi_obj)

  # choose topics to use and edit
  pi_obj_mat %>% data.frame(.) %>%
              select(all_of(use_ids)) %>%
              gather(1:length(use_ids), key = "Topic", value = "value") %>%
              group_by(Topic) %>%
              summarise(mean = mean(value),
                        uq = quantile(value, .975),
                        lq = quantile(value, .25)) %>%
              mutate(Topic = factor(Topic, levels = use_ids_X)) %>%
              mutate(Compare = factor(ifelse(Topic == compare_id, 1, 0))) -> pi_obj_plot

  # modify labels for plot
  use_labels_plot <- gsub(" ", "\n", use_labels)
  use_labels_plot <- gsub("\n&", " &", use_labels_plot)

  # plot
  p <- ggplot(pi_obj_plot, aes(y = mean, x = Topic, color = Compare)) +
        form3 +
        geom_point(size = 4) +
        geom_errorbar(aes(ymin = lq, ymax = uq), data = pi_obj_plot, width = 0, size = 2) +
        scale_color_manual(values = c("gray60", "#3391e8"), guide = "none") +
        scale_x_discrete(labels = use_labels_plot) +
        scale_y_continuous(name = "Value (Proportion) ", limits = c(0, 0.15)) +
        ylab("Value") +
        theme(axis.text=element_text(color="black",size=12),
         axis.title=element_text(color="black",size=16),
         # legend.text = element_text(size=22),
         # legend.justification=c(1, 0), legend.position=c(.46, .78),
         # legend.background = element_rect(fill=NULL, size=.8, linetype="solid"),
         strip.background =element_blank(),
         strip.text = element_text(size=24))

  ggsave(filename = "output/base/bill_pi.png", p, width = 6.5, height = 6)
}



