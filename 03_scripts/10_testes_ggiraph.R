
library(ggiraph)
library(ggplot2)
library(camcorder)  # pacote que grava o estado dos gráficos

data <- readxl::read_xlsx("01_raw_data/teste_segundo_grafico_fechado.xlsx") |>
    janitor::clean_names()


# gravação
gg_record(
    dir = "recording_dir", # diretório p salvar as imagens
    device = "png", # formato da imagem
    width = 20, # largura
    height = 7, # altura
    dpi = 300, # resolução da imagem
    bg = "white" # fundo branco
)

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
    dplyr::mutate(
        # classe = dplyr::case_when(
            # classe == "Estoque de Euc Processo - 6 anos" ~ "6 anos",
            # classe == "Estoque de Euc Processo - 7 anos" ~ "7 anos",
            # .default = "≥ 8 anos"
        # ),

        volume = round(volume, 1)
    ) |>
    # add coluna com cores que quero
    dplyr::mutate(
        cores = dplyr::case_when(
            classe == "Estoque de Euc Processo - 6 anos" ~ "#92D050",
            classe == "Estoque de Euc Processo - 7 anos" ~ "#00B050",
            .default = "#548235"
        )
    )
data_long

# barras empilhadas
d <- data_long |>
    tibble::rownames_to_column() |>
    ggplot(aes(
        x = ano,
        y = volume,
        fill = cores,
        tooltip = paste("Volume:", volume, "m³\nIdade:", classe)
            )
        ) +
    geom_bar_interactive(stat = "identity", position = "stack") +
    # conta as obs; garante q sejam empilhadas; transparência
    theme(
        text = element_text(size = 28, family = "Times New Roman"),
        panel.grid.major.y = element_line(color = "grey20", linetype = "dashed"),  # linhas de grade principais tracejadas
        panel.grid.minor = element_blank(),  # remove as linhas de grade menores
        axis.line = element_line(color = "black"), # adiciona linha nos eixos x e y
        axis.ticks = element_line(color = "black"),  # adiciona ticks aos eixos, traços nos valores
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        legend.position = "bottom", # posiciona a legenda abaixo do gráfico
        legend.box = "horizontal", # define a caixa da legenda para horizontal
        legend.title = element_blank(), # tira o título da legenda
        plot.title = element_text(hjust = 0.5, face = "bold"), # centralizado e em negrito
        panel.background = element_rect(fill = "white")
    ) +
    scale_y_continuous(  # adição para garantir que o eixo y comece no zero
        expand = c(0, 0),  # remove qualquer espaçamento extra
        limits = c(0, 1000),  # limite inferior do eixo y é 0, superior é automático
        breaks = seq(0, 1000, by = 200)  # define os intervalos a cada 200 com o limite especificado
    ) +
    # scale_fill_manual(values = c(
    #     "6 anos" = "#92D050",
    #     "7 anos" = "#00B050",
    #     "≥ 8 anos" = "#548235"
    # )) +
    scale_fill_identity() + # legenda deixa de ser gerada por causa dele, antes fill era cores
    labs(title = "Estoque de Euc Própria Processo SP (m³)",
         x = NULL,
         y = "Milhares"
        )  # rótulos
d

# trazendo interatividade p gráfico
girafe(
    ggobj = d,
    width_svg = 20,  # ajuste de largura
    height_svg = 5    # ajuste de altura
)

gg_stop_recording()

