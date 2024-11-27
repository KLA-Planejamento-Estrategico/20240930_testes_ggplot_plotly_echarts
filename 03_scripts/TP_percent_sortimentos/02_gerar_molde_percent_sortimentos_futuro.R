# TC - sem ajuste - gerar % dos sortimentos e de crescimento volumétrico

# Load all ----------------------------------------------------------------

idade_base_euc <- 7
idade_base_pin <- 15

# Get data ----------------------------------------------------------------

tps <- readr::read_rds(
    file = "02_data_rds/data_tps_brutas.rds"
)
tps

# Transform data ----------------------------------------------------------

# realizar filtros nas tps sem ajuste de tl
tp_tl_s_aj <- tps$tp_tl_s_aj |>
    dplyr::filter(
        regiao == "futuro_sc",
        stringr::str_detect(estrato, "24")
    )
tp_tl_s_aj

# gerar ima (m³/ha/ano) e área nas tps
tp_tl_ima_vol <- tp_tl_s_aj |> dplyr::mutate(
    ima = v_ha_m3 / idade,
    vol_ha_ano = ima * idade
)
tp_tl_ima_vol

# gerar porcentagem de volume (m³/ha) idade e IMA base -------------------------
# para estratos eucalipto
tp_tl_percent_vol_euc <- tp_tl_ima_vol |>
    dplyr::mutate(
        percent_vol = dplyr::case_when(
            estrato == "EUC24_SC" & idade == idade_base_euc ~ vol_ha_ano/vol_ha_ano,
            .default = 0
        )
    )
tp_tl_percent_vol_euc

# todos os estratos
tp_tl_percent_vol_euc_pin <- tp_tl_percent_vol_euc |>
    dplyr::mutate(
        percent_vol = dplyr::case_when(
            estrato == "PIN24_SC" & idade == idade_base_pin ~ vol_ha_ano/vol_ha_ano,
            .default = percent_vol
        )
    )
tp_tl_percent_vol_euc_pin

# check de ítens com percent_area = 1
tp_tl_percent_vol_euc_pin |> dplyr::filter(percent_vol == 1)

# verificar NAs em percent_vol
tp_tl_percent_vol_euc_pin |> dplyr::filter(is.na(percent_vol))


# selecionar apenas os volumes referentes ao valor de ima e idade fornecido
vol_cem_porcent_euc <- tp_tl_percent_vol_euc_pin |>
    dplyr::filter(estrato == "EUC24_SC") |>
    dplyr::mutate(
        vol_cem_porcent = dplyr::case_when(percent_vol == 1 ~ vol_ha_ano)
    ) |> dplyr::select(
        vol_cem_porcent
    ) |> dplyr::filter(!is.na(vol_cem_porcent))
vol_cem_porcent_euc

vol_cem_porcent_pin <- tp_tl_percent_vol_euc_pin |>
    dplyr::filter(estrato == "PIN24_SC") |>
    dplyr::mutate(
        vol_cem_porcent = dplyr::case_when(
            percent_vol == 1 ~ vol_ha_ano,
            .default =
        )
    ) |> dplyr::select(
        vol_cem_porcent
    ) |> dplyr::filter(!is.na(vol_cem_porcent))
vol_cem_porcent_pin

# trazer valores dos volumes
valor_vol_cem_porcent_euc <- vol_cem_porcent_euc$vol_cem_porcent
valor_vol_cem_porcent_euc

valor_vol_cem_porcent_pin <- vol_cem_porcent_pin$vol_cem_porcent
valor_vol_cem_porcent_pin


# gerar porcentagem dos demais volumes (m³/ha) ---------------------------------
tp_tl_percent_vol_aj_euc <- tp_tl_percent_vol_euc_pin |>
    dplyr::mutate(
        percent_vol = dplyr::case_when(
            estrato == "EUC24_SC" &
                percent_vol == 0 ~ vol_ha_ano / valor_vol_cem_porcent_euc,
            .default = percent_vol
        )
    )
tp_tl_percent_vol_aj_euc

tp_tl_percent_vol_aj_euc_pin <- tp_tl_percent_vol_aj_euc |>
    dplyr::mutate(
        percent_vol = dplyr::case_when(
            estrato == "PIN24_SC" &
                percent_vol == 0 ~ vol_ha_ano / valor_vol_cem_porcent_pin,
            .default = percent_vol
        )
    )
tp_tl_percent_vol_aj_euc_pin


# gerar porcentagem dos sortimentos ---------------------------------------
tp_tl_sort <- tp_tl_percent_vol_aj_euc_pin |>
    dplyr::mutate(
        dplyr::across(ends_with("m3"), ~ .x / v_ha_m3),
        dplyr::across(ends_with("tssc"), ~ .x / dplyr::across(ends_with("tssc")) |> rowSums()),
    )
tp_tl_sort

# selecionar apenas ítens que servirão de molde para tp futura -----------------
tp_tl_molde <- tp_tl_sort |>
    dplyr::select(
        regiao,
        estrato,
        especie,
        idade,
        ke_tssc:k3_m3,
        n = f_ha,
        vmi,
        dbm,
        percent_vol_m3_ha_ano = percent_vol
    )
tp_tl_molde

# Export data -------------------------------------------------------------

tp_tl_molde |> writexl::write_xlsx("05_outputs/tp_tl_molde_futuro.xlsx")

