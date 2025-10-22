# Scraping

res <- httr::GET("https://www.gov.br/anac/pt-br/assuntos/dados-e-estatisticas/historico-de-voos")

links <- res |>
  httr::content() |>
  rvest::html_nodes("table.plain") |>
  rvest::html_nodes("a") |>
  rvest::html_attr("href") |>
  unique()

fazer_download <- purrr::possibly(
  function(url) {
    usethis::ui_todo("Baixando arquivo {basename(url)}...")
    download.file(
      url = url,
      destfile = glue::glue("data-raw/anac_files/{basename(url)}"),
      quiet = TRUE
    )
  },
  otherwise = 1
)

tab_download <- tibble::tibble(
  url = links,
  flag_erro = purrr::map_dbl(links, fazer_download)
)


tab_download |>
  dplyr::filter(flag_erro == 1) |>
  View()

# Importação manual
readr::read_csv2(
  "https://www.gov.br/anac/pt-br/assuntos/dados-e-estatisticas/base-historica-1/vra/2016/VRA_do_Ms_052016.csv",
  locale = readr::locale(encoding = "latin1")
) |>
  write.csv("data-raw/anac_files/vra_05_2016.csv")

readr::read_csv2(
  "https://www.gov.br/anac/pt-br/assuntos/dados-e-estatisticas/base-historica-1/vra/2017/vra_do_mes082017bic.csv",
  locale = readr::locale(encoding = "latin1")
) |>
  write.csv("data-raw/anac_files/vra_08_2017.csv")


