patpop_matched_obs <- function(id_var) {
  # Convert id_var string to symbol for tidy evaluation
  id_sym <- rlang::sym(id_var)

  result <- patpop_matched |>
    dplyr::select(!!id_sym, index_date) |>
    dplyr::left_join(
      (temp |>
        dplyr::collect()),
      by = dplyr::join_by(!!id_sym == person_id, y$event_date > x$index_date)
    ) |>
    dplyr::arrange(!!id_sym, event_date) |>
    rename(person_id = !!id_sym)

  return(result)
}

# test <- codelist_translate |> collect()
# filter(list_code %in% c("observation_code", "observation_category_code")) |>
#   collect()
