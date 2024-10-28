# cópia do script 07

library(plotly)
library(ggplot2)
library(forcats)  # no lugar de factor

data <- readxl::read_xlsx("01_raw_data/teste_segundo_grafico_fechado.xlsx") |> janitor::clean_names()

# Sem fator ---------------------------------------------------------------

# transformando os dados com pivot_longer
data_long <- data |>
    tidyr::pivot_longer(
        cols = starts_with("x20"), names_to = "ano", values_to = "volume"
    ) |>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x"),
        volume = volume / 10^3
    ) |>
    dplyr::rename(classe = x1) |>
    dplyr::mutate(
        classe = forcats::fct_relevel(classe,  # organizando os níveis
                                      "Estoque de Euc Processo - 8_100 anos",
                                      "Estoque de Euc Processo - 7 anos",
                                      "Estoque de Euc Processo - 6 anos"
                                      # "Estoque de Euc Processo - 8_100 anos",
                                      # "Estoque de Euc Processo - 7 anos"
        )
    ) |>
    dplyr::arrange(dplyr::desc(classe))

# barras empilhadas
d <- data_long |>
    tibble::rownames_to_column() |>
    ggplot(aes(x = ano, y = volume, fill = classe, text = paste(classe, "- Volume:", volume, "m³"))) +
    geom_bar(stat = "identity") +  # altura da barra igual ao máx de y
    labs(
        x = "Ano",
        y = "Milhares",
        title = "Estoque de Euc Própria Processo SP (m³)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        text = element_text(color = "grey20"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.line = element_line(color = "black")  # adiciona uma linha nos eixos x e y
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_manual(values = c("#92D050", "#00B050", "#548235"),
                      breaks = levels(data_long$classe))  # usando levels do fator
d
# Gerar o gráfico interativo
ggplotly(d, tooltip = "text")


# Exemplos com forcats ----------------------------------------------------

# reordenar fatores
library(ggplot2)
library(forcats)

# reordenar os níveis de [cyl] com base na média de mpg
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$cyl <- forcats::fct_reorder(mtcars$cyl, mtcars$mpg, .desc = TRUE)

ggplot(mtcars, aes(x = cyl, y = mpg)) +
    geom_boxplot() +
    labs(
        title = "Reordenar Fatores com fct_reorder",
        x = "Número de Cilindros",
        y = "Milhas por Galão"
)

# recodificar os níveis de [cyl]
mtcars$cyl <- forcats::fct_recode(
    mtcars$cyl,
    "Quatro Cilindros" = "4",
    "Seis Cilindros" = "6",
    "Oito Cilindros" = "8"
)
table(mtcars$cyl)

# agrupar cilindradas em categorias


# reordenar manualmente os níveis de [gear]
mtcars$gear <- forcats::fct_relevel(as.factor(mtcars$gear), "4", "5", "3")

# Visualizar o novo ordenamento
ggplot(mtcars, aes(x = gear, fill = gear)) +
    geom_bar() +
    labs(
        title = "Reordenar Fatores com fct_relevel",
        x = "Número de Marchas"
)

