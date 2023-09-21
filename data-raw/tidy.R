arquivos_csv <- list.files(
  "data-raw/anac_files/",
  full.names = TRUE,
  pattern = "csv$"
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
  tab |>
    janitor::clean_names() |>
    janitor::remove_empty(which = c("rows", "cols")) |>
    dplyr::select(-dplyr::any_of("x1")) |>
    dplyr::rename_with(
      .fn = renomear_colunas
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )
}

dados <- purrr::map(arquivos_csv, ler_tabelas) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    flight_status = tolower(flight_status),
    flight_status2 = dplyr::case_when(
      stringr::str_detect(flight_status, "informado") ~ "not informed",
      stringr::str_detect(flight_status, "n.*realizado") ~ "not carried out",
      flight_status == "realizado" ~ "carried out",
      flight_status == "cancelado" ~ "canceled",
      TRUE ~ NA_character_
    ),
    airline = ifelse(nchar(airline) != 3, NA_character_, airline)
  )

dados |>
  # dplyr::mutate(
  #   airline = ifelse(
  #     nchar(airline) != 3 | airline == "-->",
  #     NA_character_,
  #     airline
  #   )
  # ) |>
  dplyr::pull(planned_departure_date) |>
  unique()

