

library(plotly)
library(ggplot2)

data <- readxl::read_xlsx("01_raw_data/teste_segundo_grafico_fechado.xlsx") |> janitor::clean_names()
data

# Gráfico de dispersão  ---------------------------------------------------

# transformando os dados com pivot_longer
data_long <- data  |>
    tidyr::pivot_longer(
        cols = starts_with("x20"), names_to = "ano", values_to = "volume"
    )|>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x"),
        volume = volume/10^3
    ) |>
    dplyr::rename(classe = x1)


# barras empilhadas
d <- data_long |>
    tibble::rownames_to_column() |>
    ggplot(aes(x = ano, y = volume, fill = classe, text = paste(classe, "- Volume:", volume, "m³"))) +
    # mapear que é um gráfico, dizer quem são os eixos e colocar nome nos rótulos
    geom_bar(stat = "identity") +# altura da barra igual ao máx de y
    labs(
        x = "Ano",
        y = "Milhares",
        title = "Estoque de Euc Própria Processo SP (m³)"
    ) +
    scale_fill_manual(values = scales::brewer_pal(palette = "Greens")(3)) +
    theme_minimal(base_size = 14) +
    theme(
        text = element_text(color = "grey20"),
        legend.position = "bottom",
        # legend.box = "horizontal",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold")
    )

ggplotly (d, tooltip = "text")
