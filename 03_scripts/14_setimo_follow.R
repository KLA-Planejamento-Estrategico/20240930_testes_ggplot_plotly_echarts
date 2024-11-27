# continuação do sétimo follow
# alterando cor, fonte dentro de ggiraph e adicionando totais
# Load all ----------------------------------------------------------------

library(gdtools)
library(ggiraph)
library(ggplot2)

# instalar gdtools
# gdtools::register_gfont("Roboto Condensed") # baixar a fonte no site google e registrar
font_family_exists("Atkinson Hyperlegible") # verificar se já existe
font_family_exists("Roboto Condensed") # verificar se já existe

# Get raw data ------------------------------------------------------------

data <- readxl::read_xlsx("01_raw_data/teste_segundo_grafico_fechado.xlsx") |>
    janitor::clean_names()
data

# Transform data ----------------------------------------------------------

data_long <- data |>
    tidyr::pivot_longer(
        cols = starts_with("x20"), names_to = "ano", values_to = "volume"
    ) |>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x"),
        volume = volume / 10^3
    ) |>
    dplyr::mutate(
        classe = dplyr::case_when(
            classe == "Estoque de Euc Processo - 6 anos" ~ "6 anos",
            classe == "Estoque de Euc Processo - 7 anos" ~ "7 anos",
            .default = "≥ 8 anos"
        ),
        cor = dplyr::case_when(
            classe == "6 anos" ~ "#FFCC80",
            classe == "7 anos" ~ "#FFA500",
            classe == "≥ 8 anos" ~ "#FF4500",
            .default = "black"
        ),
        volume = round(volume, 1)
    )
data_long

# ordenar a classe p garantir que "≥ 8 anos" fique no topo
data_long_fator <- data_long |>
    dplyr::mutate(
        classe = forcats::fct_relevel(
            classe,
            "≥ 8 anos",  # ≥ 8 anos acima
            "7 anos",
            "6 anos",
            # "≥ 8 anos", # teste
        )
    ) |>
    # criar totais
    dplyr::group_by(ano) |>
    dplyr::mutate(
        total_volume = sum(volume),
        total_volume = round(total_volume, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(classe)
data_long_fator

# trabalhando os dados de forma segmentada ------------------------------------
# nomear vetor de cores p garantir que a legenda seja gerada

cores <- unique(data_long_fator$cor) # puxa como vetores
classes <- unique(data_long_fator$classe) # puxa como vetores
names(cores) <- classes # nomeia um vetor com o outro
# data_long_fator$cor <- cores[data_long_fator$classe] # leva as cores como vetor p direto p data.frame

# gerando valor máximo do eixo y p add escala automática como queremos

max_y <- max(data_long_fator$total_volume, na.rm = TRUE) |> round(0)
# intervalo_y <- ceiling(max_y / 500) * 100 # ajustar p próxima centena
intervalo_y <- ceiling((max_y / 5) / 10) * 10 # ajustar y p ficar um pouquinho maior
# /10 p encontrar a quantidade de dezenas no intervado de /5
# *10 p saber o valor da dezena mais próxima

mult_breaks_eixo_y <- 5

# gráfico de barras empilhadas ------------------------------------------------

d <- ggplot(
    data = data_long_fator,
    mapping = aes(
        x = ano,
        y = volume,
        fill = classe,
        # usando a classe para definir as cores
        tooltip = paste("Volume:", volume, "mil m³\nIdade:", classe)
        )) +
    # ajuste eixo y
    # alteração - arredondar p próxima dezena e não centena
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, intervalo_y * 6), # com multiplicador p ajuste do max.
        # limits = c(0, intervalo_y + 1000), # com somatório p ajustar o max.
        # define o limite de y 5 vezes o intervalo calculado
        # breaks = seq(0, intervalo_y * mult_breaks_eixo_y, by = intervalo_y),
        # validar se vale a pena adicionar as quebras ou deixar que defina sozinho
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
            total_volume + max_y * 0.05,
            # distancia dos rótulos 3% acima da barra em relação ao total
            label = total_volume,
            fill = NULL # remove o mapeamento de cor
        ),
        data = data_long_fator,
        fontface = "bold", # negrito
        size = 5 # não acompanha o tamanho dito dentro do theme
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
    scale_fill_manual(
        values = cores,
        guide = guide_legend(reverse = TRUE) # alterar ordem somente das legendas
    ) +  # usando cores definidas
    labs(title = "Estoque de Euc Própria Processo SP (m³)",
         x = "Anos",
         y = "Milhares"
    )
d

# interatividade e ajuste de tamanho de tipo de fonte -------------------------
girafe(
    ggobj = d,
    fonts = list(sans = "Roboto Condensed"),
    width_svg = 20,
    height_svg = 5
)
