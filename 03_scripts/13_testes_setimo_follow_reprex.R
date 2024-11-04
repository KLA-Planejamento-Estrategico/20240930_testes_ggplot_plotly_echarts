
library(ggiraph)
library(ggplot2)

# definindo as cores
tres_cores_pin <- c("#92D050", "#00B050", "#548235")  # as cores a serem usadas
cinco_cores_pin <- c("#E0F7E0", "#B2E1B2", "#80D080", "#4CAF50", "#009900")
tres_cores_euc <- c("#FFA500", "#FF8C00", "#FF4500" )
cinco_cores_euc <- c("#FFF3E0", "#FFE0B2", "#FFCC80", "#FF9800", "#FF5722")


data_past_tribble <- tibble::tribble(
                                   ~classe, ~`2020`, ~`2021`, ~`2022`, ~`2023`, ~`2024`, ~`2025`, ~`2026`, ~`2027`, ~`2028`, ~`2029`, ~`2030`, ~`2031`, ~`2032`, ~`2033`, ~`2034`, ~`2035`, ~`2036`, ~`2037`, ~`2038`, ~`2039`,
        "Estoque de Euc Processo - 6 anos", 147.697,  27.838, 199.565,   94.19, 143.085,      0L,   2.487,      0L,  49.412, 169.909, 383.006, 448.102,  40.477,   9.248, 190.582,   4.231,  119.65, 193.039, 175.005, 135.927,
        "Estoque de Euc Processo - 7 anos", 107.565, 174.907,  32.841, 171.273,   55.08,      0L,       0,      0L,       0,  52.007,  57.853, 258.981,  334.34,  37.381,   1.359, 219.378,       0, 119.898, 109.159,  21.516,
    "Estoque de Euc Processo - 8_100 anos", 345.392, 411.085, 302.369,  77.763,       0,      0L,       0,      0L,       0,       0,  57.928,  82.607, 195.349,  410.81, 427.986, 458.358, 465.486, 489.664, 511.087, 530.155
    ) |> janitor::clean_names()


# transformando os dados
data_long <- data_past_tribble |>
    tidyr::pivot_longer(
        cols = starts_with("x20"), names_to = "ano", values_to = "volume"
    ) |>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x")
    ) |>
    dplyr::mutate(
        classe = dplyr::case_when(
            classe == "Estoque de Euc Processo - 6 anos" ~ "6 anos",
            classe == "Estoque de Euc Processo - 7 anos" ~ "7 anos",
            TRUE ~ "≥ 8 anos"  # Aqui está a classe que queremos no topo
        ),
        volume = round(volume, 1)
    )

# reordenar a classe para garantir que "≥ 8 anos" fique no topo
data_long_fator <- data_long |>
    dplyr::mutate(
        classe = forcats::fct_relevel(
            classe,
            "≥ 8 anos",  # ≥ 8 anos acima
            "7 anos",
            "6 anos"
        )
    ) |>
    dplyr::group_by(ano) |>
    dplyr::mutate(total_volume = sum(volume)) |>
    dplyr::ungroup()

# vetor de cores nomeado
cores <- tres_cores_pin
classes <- unique(data_long_fator$classe) # |> sort() # mesma coisa q arrange só q por vetor
names(cores) <- classes
data_long_fator$cor <- cores[data_long_fator$classe]

max_y <- max(data_long_fator$total_volume, na.rm = TRUE) |> round(0)
intervalo_y <- ceiling(max_y / 500) * 100
# divide o valor máximo por 5 e arredonda para o múltiplo de 100 mais próximo

# barras empilhadas
d <- ggplot(
        data = data_long_fator,
        mapping = aes(
            x = ano,
            y = volume,
            fill = classe,
            # usando a classe para definir as cores
            tooltip = paste("Volume:", volume, "mil m³\nIdade:", classe)
        )) +
    geom_bar_interactive(
        stat = "identity", # não conta as obss usa como está nos dados
        position = "stack", # barras empilhadas
        color = "white", size = 1 # bordas brancas
    ) +

    theme(
        # sempre por ultimo
        text = element_text(size = 28, family = "Times New Roman"),
        panel.grid.major.y = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "top",
        legend.box = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(
            hjust = 0.5,
            face = "bold",
            margin = margin(b = 30)
        ),
        # aumentando a margem inferior do titulo
        panel.background = element_rect(fill = "white"),
        legend.key.width = unit(1.5, "cm") # legenda com cor retangular
    ) +

    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, intervalo_y * 5),  # define o limite de y 5 vezes o intervalo calculado
        breaks = seq(0, intervalo_y * 5, by = intervalo_y),  # corta 5 intervalos
        labels = scales::label_number(accuracy = 1)  # tira as casas decimais
    ) +
    scale_fill_manual(values = cores) +  # usando cores definidas
    labs(title = "Estoque de Euc Própria Processo SP (m³)",
         x = "Anos",
         y = "Milhares"
    )

# interatividade
# girafe(
#     ggobj = d,
#     width_svg = 20,
#     height_svg = 5
# )

