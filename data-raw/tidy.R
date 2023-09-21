arquivos_csv <- list.files(
  "data-raw/anac_files/",
  full.names = TRUE,
  pattern = "csv$"
)

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
    janitor::remove_empty(which = c("rows", "cols"))
}

dados <- tibble::tibble(
  arquivo = arquivos_csv,
  tabela = purrr::map(arquivo, ler_tabelas)
)

dados$tabela[[1]] |> dplyr::glimpse()

renomear_colunas <- function(x) {
  dplyr::case_when(
    stringr::str_detect(x, "mero.*voo|nr.*voo") ~ "flight_number",
    stringr::str_detect(x, "empresa") ~ "airline",
    stringr::str_detect(x, "assentos") ~ "number_of_seats",
    stringr::str_detect(x, "digo.*di|d_i$|cd_di") ~ "di_code",
    stringr::str_detect(x, "linha") ~ "flight_type",
    stringr::str_detect(x, "origem") ~ "origin_airport",
    stringr::str_detect(x, "destino") ~ "destination_airport",
    stringr::str_detect(x, "partida.*prevista") ~ "planned_departure_date",
    stringr::str_detect(x, "partida.*real") ~ "actual_departure_date",
    stringr::str_detect(x, "chegada*prevista") ~ "planned_arrival_date",
    stringr::str_detect(x, "chegada*real") ~ "actual_arrival_date",
    stringr::str_detect(x, "situa") ~ "flight_status",
    stringr::str_detect(x, "justificativa") ~ "justification_code",
    TRUE ~ x
  )
}

nomes <- purrr::map(dados$tabela, names)

purrr::map(
  nomes,
  ~ stringr::str_detect(.x, "assentos")
) |>
  purrr::map_lgl(any) |>
  all()

purrr::map(
  nomes,
  ~ stringr::str_detect(.x, "assentos")
) |>
  purrr::map_dbl(sum) |>
  unique()



nomes[
  purrr::map(nomes, ~ stringr::str_detect(.x, "assentos")) |>
    purrr::map_lgl(any)
]



# dados |>
#   dplyr::mutate(
#     tabela = purrr::map(
#       tabela,
#       ~ .x |>
#         dplyr::rename_with(
#           .x,
#           .fn = renomear_colunas
#         )
#     )
#   )
