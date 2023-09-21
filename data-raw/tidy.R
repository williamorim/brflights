arquivos_csv <- list.files(
  "data-raw/anac_files/",
  full.names = TRUE,
  pattern = "2019|2020|2021|2022|2023"
)

renomear_colunas <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "mero.*voo|nr.*voo") ~ "flight_number",
    stringr::str_detect(x, "empresa") ~ "airline",
    stringr::str_detect(x, "assentos") ~ "number_of_seats",
    stringr::str_detect(x, "digo.*di|d_i$|cd_di") ~ "di_code",
    stringr::str_detect(x, "grupo_di") ~ "di_group",
    stringr::str_detect(x, "linha") ~ "flight_type",
    stringr::str_detect(x, "origem") ~ "origin_airport",
    stringr::str_detect(x, "destino") ~ "destination_airport",
    stringr::str_detect(x, "partida.*prevista") ~ "planned_departure_date",
    stringr::str_detect(x, "partida.*real") ~ "actual_departure_date",
    stringr::str_detect(x, "chegada.*prevista") ~ "planned_arrival_date",
    stringr::str_detect(x, "chegada.*real") ~ "actual_arrival_date",
    stringr::str_detect(x, "situa") ~ "flight_status",
    stringr::str_detect(x, "justificativa") ~ "justification_code",
    TRUE ~ x
  )
}

ler_tabelas <- function(path) {
  tab <- readr::read_csv2(
    path,
    locale = readr::locale(encoding = "latin1")
  )
  if (ncol(tab) == 1) {
    tab <- readr::read_csv(
      path,
      locale = readr::locale(encoding = "latin1")
    )
  }
  if (ncol(tab) == 1) {
    tab <- readr::read_tsv(
      path,
      locale = readr::locale(encoding = "latin1")
    )
  }
  tab <- tab |>
    janitor::clean_names() |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    dplyr::select(-dplyr::any_of(c("di_group", "x1"))) |>
    dplyr::rename_with(
      .fn = renomear_colunas
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )

  # These columns are inverted for some tables
  if (nchar(tab$destination_airport[1]) > 4) {
    aux <- tab$destination_airport
    tab$destination_airport <- tab$actual_departure_date
    tab$actual_departure_date <- aux
  }

  return(tab)
}

dados <- purrr::map(arquivos_csv, ler_tabelas) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    flight_status = tolower(flight_status),
    flight_status = dplyr::case_when(
      stringr::str_detect(flight_status, "informado") ~ "not informed",
      stringr::str_detect(flight_status, "n.*realizado") ~ "not carried out",
      flight_status == "realizado" ~ "carried out",
      flight_status == "cancelado" ~ "canceled",
      TRUE ~ NA_character_
    ),
    airline = ifelse(nchar(airline) != 3, NA_character_, airline),
    number_of_seats = as.numeric(number_of_seats),
    dplyr::across(
      dplyr::ends_with("date"),
      ~ ifelse(
        stringr::str_detect(tolower(.x), "informado"),
        NA_character_,
        .x
      )
    )
  )

dplyr::glimpse(dados)

saveRDS(dados, "data-raw/brflights.rds")
