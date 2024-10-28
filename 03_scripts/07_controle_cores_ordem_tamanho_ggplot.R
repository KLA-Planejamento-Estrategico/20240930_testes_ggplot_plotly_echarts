

library(plotly)
library(ggplot2)

data <- readxl::read_xlsx("01_raw_data/teste_segundo_grafico_fechado.xlsx") |> janitor::clean_names()
data


# Sem fator ---------------------------------------------------------------


# transformando os dados com pivot_longer
data_long <- data  |>
    tidyr::pivot_longer(
        cols = starts_with("x20"), names_to = "ano", values_to = "volume"
    )|>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x"),
        volume = volume/10^3
    ) |>
    dplyr::rename(classe = x1) |>
    # transformar em fatores p manter a ordem
    dplyr::mutate(
        classe = factor(
            classe,
            levels = c("Estoque de Euc Processo - 7 anos",
                       "Estoque de Euc Processo - 6 anos",
                       "Estoque de Euc Processo - 8_100 anos"),
            ordered = TRUE
        )
    ) |>
    dplyr::arrange(dplyr::desc(classe))

levels(data_long$classe)

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
    theme_minimal(base_size = 14) +
    theme(
        text = element_text(color = "grey20"),
        legend.position = "bottom",
        # legend.box = "horizontal",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
#alteração p ajuste dos eixos
    # adicionando linhas contínuas para os eixos
    axis.line = element_line(color = "black"), # Adiciona uma linha nos eixos x e y
    # Ajustando para que os eixos x e y se toquem no zero
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black")
    ) + # fim da alteração
    # Remover o espaçamento ao redor dos eixos
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c("#92D050", "#00B050", "#548235"),
                      breaks = c("Estoque de Euc Processo - 6 anos", "Estoque de Euc Processo - 7 anos", "Estoque de Euc Processo - 8_100 anos"))
d
# breaks = levels(data_long$classe))
ggplotly (d, tooltip = "text")
