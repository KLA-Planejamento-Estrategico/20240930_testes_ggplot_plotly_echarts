
library(ggiraph)
library(ggplot2)

# definindo as cores
tres_cores_pin <- c("#92D050", "#00B050", "#548235")  # as cores a serem usadas
cinco_cores_pin <- c("#E0F7E0", "#B2E1B2", "#80D080", "#4CAF50", "#009900")
tres_cores_euc <- c("#FFA500", "#FF8C00", "#FF4500" )
cinco_cores_euc <- c("#FFF3E0", "#FFE0B2", "#FFCC80", "#FF9800", "#FF5722")

# carregando os dados
data <- readxl::read_xlsx("01_raw_data/teste_segundo_grafico_fechado.xlsx") |>
    janitor::clean_names()

# Gravação
camcorder::gg_record(
    dir = "recording_dir", # diretório p salvar as imagens
    device = "png", # formato da imagem
    width = 20, # largura
    height = 7, # altura
    dpi = 300, # resolução da imagem
    bg = "white" # fundo branco
)

# transformando os dados
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
classes <- unique(data_long_fator$classe)
names(cores) <- classes
data_long_fator$cor <- cores[data_long_fator$classe]

# calcular os totais empilhados
totais <- data_long_fator |>
    dplyr::group_by(ano) |>
    dplyr::summarize(total_volume = sum(volume), .groups = 'drop')
max_y <- max(data_long_fator$total_volume, na.rm = TRUE) |> round(0)
intervalo_y <- ceiling(max_y / 500) * 100
# divide o valor máximo por 5 e arredonda para o múltiplo de 100 mais próximo

# barras empilhadas
d <- data_long_fator |>
    tibble::rownames_to_column() |>
    ggplot(aes(
        x = ano,
        y = volume,
        fill = classe,  # usando a classe para definir as cores
        tooltip = paste("Volume:", volume, "mil m³\nIdade:", classe)
    )) +
    geom_bar_interactive(
        stat = "identity", # não conta as obss usa como está nos dados
        position = "stack", # barras empilhadas
        color = "white", size = 1 # bordas brancas
    ) +
    # # linha dos totais
    # geom_line(
    #     data = data_long_fator  |>  distinct(ano, total_volume), aes(x = ano, y = total_volume),
    #     group = 1, # dados agrupados cada um como um valor por ano
    #     color = "black",
    #     size = 1, # espessura
    #     linetype = "dashed" # linha pontilhada atoa
    # ) +
    # # rótulo dos totais
    # geom_text(
    #     data = data_long_fator |>  distinct(ano, total_volume), aes(x = ano, y = total_volume,
    #     label = round(total_volume, 0)),
    #     vjust = -0.5, color = "black"
    # ) +
    theme(
        text = element_text(size = 28, family = "Times New Roman"),
        panel.grid.major.y = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        legend.position = "top",
        legend.box = "horizontal",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 30)), # aumentando a margem inferior do titulo
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
d
camcorder::gg_stop_recording()

# interatividade
girafe(
    ggobj = d,
    width_svg = 20,
    height_svg = 5
)

