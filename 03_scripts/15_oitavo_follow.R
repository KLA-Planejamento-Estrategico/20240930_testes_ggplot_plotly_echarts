
# gráfico de área plantada

# Load all ----------------------------------------------------------------

library(gdtools)
library(ggiraph)
library(ggplot2)

# font_family_exists("Atkinson Hyperlegible") # verificar se já existe
# font_family_exists("Roboto Condensed") # verificar se já existe


# Get raw data ------------------------------------------------------------

data <- readxl::read_xlsx("01_raw_data/area_plantada_pin_cenario_a.xlsx") |>
    janitor::clean_names()
data

# Transform data ----------------------------------------------------------

# Transformando para o formato desejado

data_long <- data |>
    tidyr::pivot_longer(
        cols = c(starts_with("x20"), "est"),
        names_to = "ano",
        values_to = "area"
    ) |>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x"),
        cor = dplyr::case_when(
            classe == "Área plantada Pinus" ~ "#4CAF50",
            classe == "Área plantada Eucalipto" ~ "#FFA500",
            classe == "Área de giro" ~ "grey",
            classe == "Restrito" ~ "#FF4500",
            .default = "black"
        ),
        area = round(area, 1),
        ano = as.numeric(ano)
    ) |>
    # total por ano
    dplyr::group_by(ano)  |>
    dplyr::mutate(
        total_area = sum(area),
        total_area = round(total_area, 0)
    ) |>
    dplyr::ungroup() |>
    # NA em ano para Estrutural
    dplyr::mutate(
        ano = as.character(ano),
        ano = dplyr::case_when(
            is.na(ano) ~ "Est.",
            .default = ano
        )
    )
data_long

# ordenar a classe p garantir ordem das colunas
data_long_fator <- data_long |>
    dplyr::mutate(
        classe = forcats::fct_relevel(
            classe,
            "Restrito",
            "Área de giro",
            "Área plantada Eucalipto",
            "Área plantada Pinus"
        ),
        ano = as.character(ano)
    ) |>
    dplyr::arrange(classe)
data_long_fator

# trabalhando os dados de forma segmentada ------------------------------------
# nomear vetor de cores p garantir que a legenda seja gerada

cores <- unique(data_long_fator$cor) # puxa como vetores
classes <- unique(data_long_fator$classe) # puxa como vetores
names(cores) <- classes # nomeia um vetor com o outro

# gerando valor máximo do eixo y p add escala automática como queremos

max_y <- max(data_long_fator$total_area, na.rm = TRUE) |> round(0)
intervalo_y <- ceiling((max_y / 5) / 10) * 10
# /10 p encontrar a quantidade de dezenas no intervado de /5
# *10 p saber o valor da dezena mais próxima

# gráfico de barras empilhadas ------------------------------------------------


d <- ggplot(
    data = data_long_fator,
    mapping = aes(
        x = ano,
        y = area,
        fill = classe,
        # usando a classe para definir as cores
        tooltip = paste("Área:", area, "mil ha\nCategoria:", classe)
        )) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, intervalo_y * 5),
        # define o limite de y 5 vezes o intervalo calculado
        # breaks = seq(0, intervalo_y * 5, by = intervalo_y),
        # pega o intervalo_y e vai somando até chegar em intervalo_y * 5
        labels = scales::label_number(accuracy = 1)  # tira as casas decimais
    ) +
    geom_bar_interactive(
        stat = "identity",
        # não conta as obss usa como está nos dados
        position = "stack",
        # barras empilhadas
        color = "white",
        size = 0.5 # bordas brancas
    ) +
    geom_text(
        aes(
            ano,
            # posição do eixo x
            total_area + max_y * 0.05,
            # distancia dos rótulos 5% acima da barra em relação ao total
            label = total_area,
            fill = NULL # remove o mapeamento de cor
        ),
        data = data_long_fator,
        fontface = "bold", # negrito
        size = 5
    ) +
    theme(
        # sempre por ultimo
        text = element_text(size = 20),
        # panel.grid.major.y = element_line(color = "grey20", linetype = "dashed"),
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
    scale_fill_manual(values = cores) +  # usando cores definidas
    labs(title = "Área Plantada (kha)",
         x = "Anos",
         y = "Milhares")
d

# interatividade e ajuste de tamanho de tipo de fonte -------------------------
girafe(
    ggobj = d,
    fonts = list(sans = "Roboto Condensed"),
    width_svg = 16,
    height_svg = 7
)
