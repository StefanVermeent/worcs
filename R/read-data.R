read_csv <- function(file, select_vars = NULL, filter_rows = NULL, shuffle_vars = NULL, long_format = FALSE, seed = 4858342, ...) {

  data <- readr::read_csv(file, ...)

  select_vars <- if(is.null(select_vars)) {names(data)}
  filter_rows <- if(is.null(filter_rows)) {"rownames(data) %in% unique(rownames(data))"}


  data <- data |>
    select(!!select_vars) |>
    filter(filter_rows |> rlang::parse_expr() |> rlang::eval_tidy()) |>
    shuffle(data = ., shuffle_vars = shuffle_vars, long_format = long_format)

  data_hash <- digest::digest(data)

  committed_hashes <-
    gert::git_log()$message |>
    str_subset("\\[\\[data_access\\]\\].*[a-z0-9]{32}") |>
    str_extract("[a-z0-9]{32}")

  message(length(committed_hashes), " previously committed data files found.")

  if(!data_hash %in% committed_hashes) {

    response = NA

    while(response != 'Y' & response != "n") {
      response <- readline(prompt = "NEW DATA FILE DETECTED. This will trigger an automatic commit to GitHub. Are you sure you want to continue? [Y/n]:")
    }

    if(response == "n") {
      return(message("Reading the data file was aborted. Use the glance_* functions to get a summary report prior to reading in the full dataset."))
    }

    message <- readline(prompt = "If you want, you can type a short commit message about the data file. Press enter for a default message: ")

    log_milestone


  }




    log_milestone(..., milestone, message)

      git2r::add(...)
      git2r::commit(paste0("[[", milestone, "]]", " ", message))



  }





}



shuffle <- function(data, shuffle_vars, long_format, seed = set.seed) {

  if(is.null(shuffle_vars)) {
    return(data)
  }

  if(long_format) {
    row_nums <- data |> group_by_at(shuffle_vars[[1]]) |> summarise(n = n()) |> dplyr::pull(n)
  } else {
    row_nums <- rep(1, nrow(data))
  }

  set.seed(seed)

    data <- shuffle_vars |>
      map_dfc(function(x){
        data |>
          select(matches(x)) |>
          mutate(rows = rep(1:length(row_nums), row_nums)) |>
          group_split(rows) |>
          sample() |>
          bind_rows() |>
         select(-rows)
      }) |>
      bind_cols(
        data |>
          select(-matches(shuffle_vars))
      ) |>
      select(names(data)) |>
      arrange(across(matches(shuffle_vars[[1]])))

  data
}


check_object_hash <- function(hash) {


  committed_hashes <- gert::git_log()$message |>
    str_subset("\\[\\[data_access\\]\\]") |>
    str_remove(string = _, pattern = "\\[\\[data_access\\]\\].*object_hash\\s")

  if(hash %in% committed_hashes) {
    FALSE
  } else {
    TRUE
  }


}
