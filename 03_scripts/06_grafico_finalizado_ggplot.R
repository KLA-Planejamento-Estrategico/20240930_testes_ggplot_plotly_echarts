
library(plotly)
library(ggplot2)
library(camcorder)  # pacote que grava o estado dos gráficos

# gravação
gg_record(
dir = "recording_dir",  # diretório p salvar as imagens
    device = "png",         # formato da imagem
    width = 10,             # largura
    height = 8,             # altura
    dpi = 300               # resolução da imagem
)

data <- readxl::read_xlsx("01_raw_data/teste_segundo_grafico_fechado.xlsx") |> janitor::clean_names()
data


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
        legend.box = "horizontal",
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        # axis.line.x = element_line(color = "grey20", linetype = "solid"),
        panel.grid.major.y = element_line(color = "grey20", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        #alteração p ajuste dos eixos
        # adicionando linhas contínuas para os eixos
        axis.line = element_line(color = "black"), # Adiciona uma linha nos eixos x e y
        # ajustando para que os eixos x e y se toquem no zero
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        legend.position = "bottom", # posiciona a legenda abaixo do gráfico
        legend.box = "horizontal", # define a caixa da legenda para horizontal
        legend.spacing.x = unit(0.5, "cm"), # aumenta o espaçamento horizontal entre os itens da legenda
        legend.title = element_blank() # tira o título da legenda
    ) +
    scale_y_continuous(  # adição para garantir que o eixo y comece no zero
        expand = c(0, 0),  # remove qualquer espaçamento extra
        limits = c(0, NA)  # limite inferior do eixo y é 0, superior é automático
    )
d
# gg_stop_recording()

ggplotly (d, tooltip = "text") |>
    # layout(width = 1400, height = 250)

