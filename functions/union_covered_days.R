# Covered days for one patient: merge overlapping [start, end) intervals and sum.
# win_start / win_end optionally clip the intervals to an observation window.
# Shared by 09_rx_patterns.R and 10_daysupply_check.R so the sensitivity run
# uses exactly the same coverage logic as the reported numbers.
union_covered_days <- function(start, end, win_start = NULL, win_end = NULL) {
  s <- as.numeric(start)
  e <- as.numeric(end)
  if (!is.null(win_start)) s <- pmax(s, as.numeric(win_start))
  if (!is.null(win_end)) e <- pmin(e, as.numeric(win_end))
  keep <- !is.na(s) & !is.na(e) & e > s
  if (!any(keep)) return(0)
  s <- s[keep]
  e <- e[keep]
  ord <- order(s)
  s <- s[ord]
  e <- e[ord]
  cur_s <- s[1]
  cur_e <- e[1]
  total <- 0
  if (length(s) > 1) {
    for (i in 2:length(s)) {
      if (s[i] <= cur_e) {
        if (e[i] > cur_e) cur_e <- e[i]
      } else {
        total <- total + (cur_e - cur_s)
        cur_s <- s[i]
        cur_e <- e[i]
      }
    }
  }
  total + (cur_e - cur_s)
}
