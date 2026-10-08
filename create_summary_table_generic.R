create_summary_table <- function(data, group_var, vars) {
  summary_table <- data.frame(stringsAsFactors = FALSE)
  
  for (var in vars) {
    # count non-missing for this variable
    N_val <- sum(!is.na(data[[var]]))
    
    if (is.numeric(data[[var]])) {
      # --- Normality test per group ---
      levels <- unique(data[[group_var]])
      x1 <- data[[var]][ data[[group_var]] == levels[1] ]
      x2 <- data[[var]][ data[[group_var]] == levels[2] ]
      
      sw1 <- tryCatch(shapiro.test(x1)$p.value, error = function(e) NA)
      sw2 <- tryCatch(shapiro.test(x2)$p.value, error = function(e) NA)
      
      if (!is.na(sw1) && !is.na(sw2) && sw1 > 0.05 && sw2 > 0.05) {
        # ---- both groups normal: mean ± SD + t‑test ----
        grp_stats <- data %>%
          group_by(!!sym(group_var)) %>%
          summarise(
            mu = mean(!!sym(var), na.rm = TRUE),
            sd = sd(!!sym(var), na.rm = TRUE),
            .groups = "drop"
          )
        grp_text <- grp_stats %>%
          mutate(txt = paste0(round(mu, 2), " ± ", round(sd, 2))) %>%
          pull(txt)
        
        total_mu <- mean(data[[var]], na.rm = TRUE)
        total_sd <- sd(data[[var]], na.rm = TRUE)
        total_text <- paste0(round(total_mu, 2), " ± ", round(total_sd, 2))
        
        p_val <- t.test(
          formula    = as.formula(paste0(var, " ~ ", group_var)),
          data       = data,
          var.equal  = TRUE
        )$p.value
        
      } else {
        # ---- non-normal: median (Q1–Q3) + Wilcoxon ----
        grp_stats <- data %>%
          group_by(!!sym(group_var)) %>%
          summarise(
            med = median(!!sym(var), na.rm = TRUE),
            Q1  = quantile(!!sym(var), 0.25, na.rm = TRUE),
            Q3  = quantile(!!sym(var), 0.75, na.rm = TRUE),
            .groups = "drop"
          )
        grp_text <- grp_stats %>%
          mutate(txt = paste0(
            round(med, 2), " (",
            round(Q1, 2), "–", round(Q3, 2), ")"
          )) %>%
          pull(txt)
        
        total_med <- median(data[[var]], na.rm = TRUE)
        total_Q1  <- quantile(data[[var]], 0.25, na.rm = TRUE)
        total_Q3  <- quantile(data[[var]], 0.75, na.rm = TRUE)
        total_text <- paste0(
          round(total_med, 2), " (",
          round(total_Q1, 2), "–", round(total_Q3, 2), ")"
        )
        
        p_val <- wilcox.test(
          formula = as.formula(paste0(var, " ~ ", group_var)),
          data    = data
        )$p.value
      }
      
      # append numeric row
      summary_table <- rbind(
        summary_table,
        data.frame(
          Variable = gsub("_", " ", var),
          Total    = total_text,
          `No` = grp_text[1],
          `Yes` = grp_text[2],
          `P value`= round(p_val, 3),
          N        = N_val,
          stringsAsFactors = FALSE
        )
      )
      
    } else {
      # --- categorical: counts % + χ²/Fisher + totals ---
      stats_cat <- data %>%
        group_by(!!sym(group_var), !!sym(var)) %>%
        summarise(Count = n(), .groups = "drop") %>%
        group_by(!!sym(group_var)) %>%
        mutate(Freq = Count / sum(Count))
      
      total_cat <- data %>%
        group_by(!!sym(var)) %>%
        summarise(Count = n(), .groups = "drop") %>%
        mutate(Freq = Count / nrow(data)) %>%
        mutate(Total = paste0(Count, " (", round(Freq*100,1), "%)")) %>%
        select(!!sym(var), Total)
      
      stats_fmt <- stats_cat %>%
        group_by(!!sym(var)) %>%
        summarise(
          No  = paste0(Count[1], " (", round(Freq[1]*100,1), "%)"),
          Yes = paste0(Count[2], " (", round(Freq[2]*100,1), "%)"),
          .groups = "drop"
        ) %>%
        left_join(total_cat, by = var)
      
      # χ² with warning handler
      ct <- table(data[[var]], data[[group_var]])
      warn_flag <- FALSE
      chi <- withCallingHandlers(
        chisq.test(ct, correct = FALSE),
        warning = function(w) {
          if (grepl("approximation may be incorrect", w$message))
            warn_flag <<- TRUE
          invokeRestart("muffleWarning")
        }
      )
      p_val <- if (warn_flag) fisher.test(ct)$p.value else chi$p.value
      
      # overall categorical row
      summary_table <- rbind(
        summary_table,
        data.frame(
          Variable = gsub("_", " ", var),
          Total    = "",
          `No` = "",
          `Yes` = "",
          `P value`= round(p_val, 3),
          N        = N_val,
          stringsAsFactors = FALSE
        )
      )
      # one line per level
      for (i in seq_len(nrow(stats_fmt))) {
        lvl <- stats_fmt[[var]][i]
        summary_table <- rbind(
          summary_table,
          data.frame(
            Variable = paste0("  ", lvl),
            Total    = stats_fmt$Total[i],
            `No` = stats_fmt$No[i],
            `Yes` = stats_fmt$Yes[i],
            `P value`= "",
            N        = "",
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  
  # final column names
  colnames(summary_table) <- c("Variable", "Total", "No", "Yes", "P value", "N")
  return(summary_table)
}

