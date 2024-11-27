
# Get raw data ------------------------------------------------------------

# obter caminho do arquivo
path_tp <- here::here(
    "01_raw_data",
    "TabelaProducao_2023_V2.xlsx"
)
path_tp

base_tp <- path_tp |>
    # nome das planilhas
    readxl::excel_sheets() |>
    # trazer como colunas
    purrr::set_names() |>
    # ler e separar bases as retornando como lista
    purrr::map(
        .f = readxl::read_excel,
        path = path_tp,
        skip = 1,
        guess_max = 50000
    ) |>
    purrr::map(.f = janitor::clean_names)
base_tp

# Transform data ----------------------------------------------------------

# criar lista organizada com os dados das TPs - renomear
tabelas_de_producao <- list(
    tp_tc_s_aj = base_tp$TP_TC_SA,
    tp_tl_s_aj = base_tp$TP_TL_SA,
    tp_tc_c_aj = base_tp$TP_TC_CA,
    tp_tl_c_aj = base_tp$TP_TL_CA
)
tabelas_de_producao

# Export data -------------------------------------------------------------

tabelas_de_producao |>
    readr::write_rds(
        file = here::here(
            "02_data_rds",
            "data_tps_brutas.rds"),
        compress = "xz"
    )
