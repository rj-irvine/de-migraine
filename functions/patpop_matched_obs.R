patpop_matched_obs <- function(id_var) {
  # Convert id_var string to symbol for tidy evaluation
  id_sym <- rlang::sym(id_var)

  # `temp` is expected to be a local data frame already (the caller collects it
  # after restricting to the matched cohort). patpop_matched is also local, so
  # both sides of this join are data frames — required because we cannot copy a
  # local frame into Snowflake. collect() on an already-local frame is a no-op,
  # so this is safe whether temp arrives local or lazy.
  result <- patpop_matched |>
    dplyr::select(!!id_sym, index_date) |>
    dplyr::left_join(
      dplyr::collect(temp),
      by = dplyr::join_by(!!id_sym == person_id, y$event_date > x$index_date)
    ) |>
    dplyr::arrange(!!id_sym, event_date) |>
    rename(person_id = !!id_sym)

  return(result)
}

# test <- codelist_translate |> collect()
# filter(list_code %in% c("observation_code", "observation_category_code")) |>
#   collect()
