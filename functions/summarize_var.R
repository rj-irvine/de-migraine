summarize_var <- function(data, x, group_var) {
  if (is.numeric(data[[x]])) {
    temp1 <- data %>%
      group_by(across(all_of(group_var))) %>%
      summarise(
        mean_sd = paste0(
          round(mean(.data[[x]], na.rm = TRUE), 2),
          " (",
          round(sd(.data[[x]], na.rm = TRUE), 2),
          ")"
        ),
        median_iqr = paste0(
          round(median(.data[[x]], na.rm = TRUE), 2),
          " (",
          round(IQR(.data[[x]], na.rm = TRUE), 2),
          ")"
        ),
        range = paste0(
          round(min(.data[[x]], na.rm = TRUE), 2),
          "-",
          round(max(.data[[x]], na.rm = TRUE), 2)
        ),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(mean_sd, median_iqr, range))

    temp2 <- c(paste0(var_label(data[[x]])), NA)
    result <- rbind(temp2, temp1)
    return(result)
  } else if (class(data[[x]]) %in% c("character", "factor")) {
    temp1 <- data %>%
      group_by(across(all_of(group_var))) %>%
      mutate(group_n = n()) %>%
      group_by(across(all_of(c(x, group_var)))) %>%
      summarise(
        n_events = n(),
        group_n_val = unique(group_n),
        .groups = "drop"
      ) %>%
      mutate(
        pct = round(n_events / group_n_val * 100, 2),
        ci_lower = round(qbeta(0.025, n_events, group_n_val - n_events + 1) * 100, 2),
        ci_upper = round(qbeta(0.975, n_events + 1, group_n_val - n_events) * 100, 2),
        value = paste0(
          prettyNum(n_events, big.mark = ","),
          " (", pct, "% [", ci_lower, "%-", ci_upper, "%])"
        )
      ) %>%
      dplyr::select(-n_events, -group_n_val, -pct, -ci_lower, -ci_upper) %>%
      rename(name = !!sym(x)) %>%
      mutate(
        name = ifelse(is.na(name), "Missing", name),
        name = paste0("     ", name)
      ) %>%
      pivot_wider(
        names_from = all_of(group_var),
        values_from = value
      )

    temp2 <- c(paste0(var_label(data[[x]])), NA)
    result <- rbind(temp2, temp1)
    return(result)
  }
}
