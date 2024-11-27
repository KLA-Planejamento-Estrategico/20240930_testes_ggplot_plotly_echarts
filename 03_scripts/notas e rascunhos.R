
# gdtools::register_gfont("Roboto Condensed") # registrar fonte baixada

# definindo cores
tres_cores_pin <- c("#92D050", "#00B050", "#548235")  # as cores a serem usadas
cinco_cores_pin <- c("#E0F7E0", "#B2E1B2", "#80D080", "#4CAF50", "#009900")
tres_cores_euc <- c("#FFCC80", "#FFA500", "#FF4500" )
cinco_cores_euc <- c("#FFF3E0", "#FFE0B2", "#FFCC80", "#FF9800", "#FF5722")

# exemplos de gravação script 14
camcorder::gg_record(
    dir = "recording_dir", # diretório p salvar as imagens
    device = "png", # formato da imagem
    width = 20, # largura
    height = 7, # altura
    dpi = 300, # resolução da imagem
    bg = "white" # fundo branco
)
camcorder::gg_stop_recording()

# |> sort() funciona como um arrange

# scale_x_continuous(
# breaks = unique(data_long_fator$ano),  # certeza q vai exibir todos os anos
# expand = c(0, 0)  # tira espaço extra nas extremidades
# ) +


mtcars |> class()
diamonds |> class()

p1 <-
    ggplot(data = diamonds,
           mapping = aes(x = color, fill = cut, data_id = cut)) +
    geom_bar_interactive(aes(tooltip = sprintf("%s: %.0f", fill, after_stat(count))), size = 3)

girafe(ggobj = p1)
