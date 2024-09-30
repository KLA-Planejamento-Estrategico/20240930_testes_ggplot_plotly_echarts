
library(echarts4r)


# Gráfico de dispersão ----------------------------------------------------

mtcars |>
    e_charts(x = wt) |> # eixo x
    e_scatter(serie = mpg) |> # dispersão - guia o y
    e_tooltip() # rótulos nos pontos


# Gráfico de linha -------------------------------------------------------

ggplot2::txhousing |> # base exemplo do pacote ggplot2
    dplyr::mutate(year = as.character(year)) |>
    dplyr::group_by(year) |>
    dplyr::summarise(sales = mean(sales, na.rm = TRUE)) |>
    e_charts(x = year) |> # eixo x
    e_line(serie = sales) |> # gráfico de linha - tendo como base venda
    e_tooltip() # rótulo dos pontos


# Gráfico de linhas - empilhadas ------------------------------------------

ggplot2::txhousing |>
    dplyr::filter(city %in% c("Austin", "Dallas", "Houston")) |>
    dplyr::mutate(year = as.character(year)) |>
    dplyr::group_by(city, year) |> # agrupou por cidade e ano, como temos 3 cidades 3 eixos onde cada cidade será referenciada a um ano
    dplyr::summarise(sales = mean(sales, na.rm = TRUE)) |>
    e_charts(x = year) |> # eixo x
    e_line(serie = sales) |> # gráfico de linha - tendo como base venda
    e_tooltip() # rótulo de pontos
