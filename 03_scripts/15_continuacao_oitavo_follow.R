
# gráfico de DBM

# Load all ----------------------------------------------------------------

library(gdtools)
library(ggiraph)
library(ggplot2)

# font_family_exists("Atkinson Hyperlegible") # verificar se já existe
# font_family_exists("Roboto Condensed") # verificar se já existe


# Get raw data ------------------------------------------------------------

data <- readxl::read_xlsx("01_raw_data/densidade_basica.xlsx") |>
    janitor::clean_names()
data

# Transform data ----------------------------------------------------------

# Transformando para o formato desejado

data_long <- data |>
    tidyr::pivot_longer(
        cols = starts_with("x20"),
        names_to = "ano",
        values_to = "dbm"
    ) |>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x"),
        cor = dplyr::case_when(
            classe == "Densidade Pin" ~ "#4CAF50",
            classe == "Densidade Euc" ~ "#FFA500",
            .default = "black"
        ),
        dbm = round(dbm, 2),
        ano = as.numeric(ano)
        )
data_long

# ordenar a classe p garantir ordem das colunas
data_long_fator <- data_long |>
    dplyr::mutate(
        classe = forcats::fct_relevel(
            classe,
            "Densidade Euc",
            "Densidade Pin"
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

max_y <- max(data_long_fator$dbm, na.rm = TRUE)
max_y_afastado <- max_y * 1.1 # 10% acima do total
intervalo_y <- ceiling(max_y_afastado / 5)

# gráfico de barras empilhadas ------------------------------------------------

data <- mtcars

# criar nova coluna com os nomes dos carros
data$carname <- row.names(data)

# criar gráfico
gg_point <- data |>
    ggplot2::ggplot() +
    ggiraph::geom_point_interactive(aes(
        x = wt,
        y = qsec,
        color = disp,
        tooltip = carname,
        data_id = carname
    )) +
    ggplot2::theme_minimal()

# exibir gráfico interativo
ggiraph::girafe(ggobj = gg_point)


d <- data_long_fator |>
    ggplot(
    mapping = aes(
        x = ano,
        y = dbm,
        # usando a classe para definir as cores
        # tooltip = paste("DBM:", dbm, "g/m³\nGênero:", classe),
        color = classe,
        group = classe
    )) +
    ggiraph::geom_point_interactive(
        aes(
            # x = ano,
            # y = dbm,
            tooltip = paste("DBM:", dbm, "g/m³\nGênero:", classe),
            data_id = classe,
            shape = classe # diferencia os marcadores por classe
        ),
       size = 6
    ) +
    scale_y_continuous(
        expand = c(0, 0),
        limits = c(0, max_y_afastado),
        # define o limite de y 5 vezes o intervalo calculado
        # breaks = seq(0, max_y_afastado, by = intervalo_y),
        # pega o intervalo_y e vai somando até chegar max_y_afastado
        labels = scales::label_number(accuracy = 1)  # tira as casas decimais
    ) +
    geom_line_interactive(
        aes(data_id = classe),
        size = 2 # grossura das linhas
    ) +
    scale_color_manual(values = cores) +  # usando cores definidas
    theme(
        # sempre por ultimo
        text = element_text(size = 24),
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
    ) +
    labs(title = "DBM (g/cm³)",
         x = "Anos",
         y = "Massa úmida (tucc)")
d

# interatividade e ajuste de tamanho de tipo de fonte -------------------------
girafe(
    ggobj = d,
    fonts = list(sans = "Roboto Condensed"),
    width_svg = 16,
    height_svg = 7,
    options = list(
        opts_hover(
            css = "fill:yellow;stroke:black;stroke-width:3px;"
        )
    ) # adicionar destaque no grupo - cor amarelo e cor e tamanho da borda
)

