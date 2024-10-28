#library(tidyverse)

ggally_cor_New<-
  function (data, mapping, ..., stars = TRUE, method = "pearson", 
            use = "complete.obs", display_grid = FALSE, digits = 3, title_args = list(...), 
            group_args = list(...), justify_labels = "right", align_percent = 0.5, 
            title = "Corr", alignPercent = warning("deprecated. Use `align_percent`"), 
            displayGrid = warning("deprecated. Use `display_grid`")) 
  {
    if (!missing(alignPercent)) {
      warning("`alignPercent` is deprecated. Please use `align_percent` if alignment still needs to be adjusted")
      align_percent <- alignPercent
    }
    if (!missing(displayGrid)) {
      warning("`displayGrid` is deprecated. Please use `display_grid`")
      display_grid <- displayGrid
    }
    na.rm <- if (missing(use)) {
      NA
    }
    else {
      (use %in% c("complete.obs", "pairwise.complete.obs", 
                  "na.or.complete"))
    }
    ggally_statistic(data = data, mapping = mapping, na.rm = na.rm, 
                     align_percent = align_percent, display_grid = display_grid, 
                     title_args = title_args, group_args = group_args, justify_labels = justify_labels, 
                     justify_text = "left", sep = if ("colour" %in% names(mapping)) 
                       ": "
                     else ":\n", title = title, text_fn = function(x, y) {
                       if (GGally:::is_date(x)) {
                         x <- as.numeric(x)
                       }
                       if (GGally:::is_date(y)) {
                         y <- as.numeric(y)
                       }
                       corObj <- stats::cor.test(x, y, method = method, 
                                                 use = use)
                       cor_est <- as.numeric(corObj$estimate)
                       cor_txt <- formatC(cor_est, digits = digits, format = "f")
                       if (isTRUE(stars)) {
                         cor_txt <- stringr::str_c(cor_txt, signif_stars(corObj$p.value))
                         cor_Df <- as.numeric(corObj$parameter)
                         #cor_CI <- as.numeric(corObj$conf.int)
                         #cor_txt2 <- formatC(cor_CI, digits = digits, format = "f")
                         #cor_txt <- str_c(cor_txt, paste0("[",cor_txt2[1],',',cor_txt2[2],"]"),sep="\n")
                         cor_txt <- stringr::str_c(cor_txt, paste0("(DF=",cor_Df,")"),sep="\n")
                       }
                       cor_txt
                     })
  }


#GGally::ggpairs(data, upper = list(continuous = ggally_cor_New))