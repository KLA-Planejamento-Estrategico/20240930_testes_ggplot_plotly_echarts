
library(echarts4r)

data <- readxl::read_xlsx("01_raw_data/teste_primeiro_grafico.xlsx") |> janitor::clean_names()

# Definindo cores para cada tipo de densidade
cores <- c("#1f78b4", "#33a02c", "#e31a1c")

# Gráfico de área ---------------------------------------------------------

# transformando os dados com pivot_longer (necessário para o gráfico)
data_long <- data  |>
    tidyr::pivot_longer(
        cols = starts_with("x20"), names_to = "ano", values_to = "volume"
    )|>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x")
    ) |>
    dplyr::rename(densidade = estoque_em_pe) |>
    dplyr::group_by(ano, densidade) |>
    dplyr::summarise(volume = sum(volume, na.rm = TRUE))

# dplyr::ungroup() |>

# teste seguindo o exemplo de gráfico de área
ano_test <- as.numeric(rep(seq(2022,2041)))
vol_test <- data_long$volume
densidade_test <- data_long$densidade
#densidade_test <- rep(LETTERS[1:3],times=20)
data_test <- data.frame(ano_test, vol_test, densidade_test)
#|> dplyr::group_by(ano_test, densidade_test)

data_test |> e_charts(x = ano_test) |>
    e_area(serie = vol_test, name = "volume") |># se tirar o nome fica colorido
    e_x_axis(type = "category") |>
    e_y_axis(type = "value") |>
    e_tooltip(trigger = "axis")
#e_legend(show = TRUE) |>
#e_color(cores)

# Gráfico de linha --------------------------------------------------------

data |>
    tidyr::pivot_longer(
        cols = starts_with("x20"), names_to = "ano", values_to = "volume"
    )|>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x")
    ) |>
    dplyr::rename(densidade = estoque_em_pe) |>
    dplyr::group_by(ano, densidade) |>
    dplyr::summarise(volume = sum(volume, na.rm = TRUE)) |>
    e_charts(x = ano) |> # eixo x
    e_line(serie = volume) |> # gráfico de linha - tendo como base o volume
    e_tooltip() # rótulo de pontos

