
library("sf")

# Get data ----------------------------------------------------------------

delivery <- sf::read_sf("01_raw_data/DELIVERY_A.dbf") |> janitor::clean_names()
delivery |> dplyr::glimpse()

# Transform data ----------------------------------------------------------

# realizar filtros nas tps sem ajuste de tl
sorts <- delivery |>
    dplyr::filter(!theme3 == "future") |>
    dplyr::mutate(
        sortimento = dplyr::case_when(
            stringr::str_detect(product, "[Kk][Ee]") ~ "ke",
            stringr::str_detect(product, "[Kk]1") ~ "k1",
            stringr::str_detect(product, "[Kk]2") ~ "k2",
            stringr::str_detect(product, "[Kk]3") ~ "k3",
            .default = "NDA"
        )
        # check = paste0(theme3, "-", sortimento)
    ) |>
    dplyr::select(theme3,volume, sortimento, destinatio,on) |>
    dplyr::group_by(theme3, sortimento,destinatio, on) |>
    dplyr::summarise(volume = sum(volume, na.rm = TRUE), .groups = "drop") |>
    # janitor::get_dupes(check)
    tidyr::pivot_wider(
        names_from = sortimento,
        values_from = volume
    ) |>
    janitor::get_dupes(theme3)
sorts |> dplyr::glimpse()
# sorts |> dplyr::filter(sortimento == "NDA")

# Export data -------------------------------------------------------------

sorts |> writexl::write_xlsx("05_outputs/sortimentos_delivery.xlsx")

