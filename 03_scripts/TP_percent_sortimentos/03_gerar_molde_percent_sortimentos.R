# TC - sem ajuste - gerar % dos sortimentos e de crescimento volumétrico

# Get data ----------------------------------------------------------------

tps <- readr::read_rds(
    file = "02_data_rds/data_tps_brutas.rds"
)
tps

# Transform data ----------------------------------------------------------

# realizar filtros nas tps sem ajuste de tl
tp_tl_s_aj <- tps$tp_tl_s_aj |>
    dplyr::mutate(
        m3 = dplyr::across(ke_m3:k3_m3) |> rowSums(),
        ke_percent = ke_m3/m3,
        k1_percent = k1_m3/m3,
        k2_percent = k2_m3/m3,
        k3_percent = k3_m3/m3,
        th11_age = paste0(estrato, "-", idade)
    ) |>
    dplyr::select(th11_age, ke_percent:k3_percent)
tp_tl_s_aj |> dplyr::glimpse()

# Export data -------------------------------------------------------------

tp_tl_s_aj |> writexl::write_xlsx("05_outputs/tp_tl_molde.xlsx")

