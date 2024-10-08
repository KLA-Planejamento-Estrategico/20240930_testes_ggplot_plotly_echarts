
library(ggiraph)
library(tidyverse)


# Gráfico de dispersão ----------------------------------------------------

mtcars |> dplyr::glimpse()

mtcars_db <- rownames_to_column(mtcars, var = "carname") |>
    dplyr::glimpse()

myplot <- ggplot(
    data = mtcars_db,
    mapping = aes(
        x = disp, y = qsec,
        # here we add interactive aesthetics
        # enquanto tooltip faz a "animação" aparecer quando passamos o mouse
        # o data_id torna os ítens que serão mostrados únicos - os próprios dados
        tooltip = carname, data_id = carname
    )
) +
    geom_point_interactive( # gera camada de pontos interativos - objeto interativo
        size = 3, hover_nearest = TRUE
    ) # size é o tamanho da fonte e hover_nearest trás destaque

interactive_plot <- girafe(ggobj = myplot) # interatividade do gráfico como um todo
# ggobj torna possível trazer um objeto p função

htmltools::save_html(interactive_plot, "primeiro_grafico_ggiraph.html")
# caminho p salvar o html, poderia ser qualquer um com .html como numa exportação

# Gráfico de barras -------------------------------------------------------
# simples
# contar quantos carros existem por número de cilindros (cyl)
df <- mtcars
df$cyl <- as.factor(df$cyl)  # Transformar cilindros em categorias

df |> tibble::as_tibble() |> dplyr::pull(cyl)

df <- df |>
    dplyr::mutate(
        cyl = as.factor(cyl)
    )


p <- ggplot(df, aes(x = cyl)) +
    geom_bar_interactive(aes(tooltip = ..count..),  # contagem de cada categoria
                         fill = "steelblue", color = "black") +
    # fill preenchimento e color bordas
    labs(title = "Contagem de Carros por Número de Cilindros",
         x = "Número de Cilindros", y = "Contagem")
# rótulos dos eixos

# Tornar o gráfico interativo com girafe
girafe(ggobj = p)



# Gráfico de linhas com destaque ------------------------------------------

# dados
library(dplyr)
library(ggplot2)
library(patchwork)

dat <- gapminder::gapminder |>
    janitor::clean_names() |>
    mutate(
        # Reformat continent as a character instead of as a factor
        # (will be important later)
        id = levels(continent)[as.numeric(continent)],
        continent = forcats::fct_reorder(continent, life_exp)
    )

color_palette <- thematic::okabe_ito(5)
names(color_palette) <- unique(dat$continent)
base_size <- 18
mean_life_exp <- dat |>
    group_by(continent, year, id) |>
    summarise(mean_life_exp = mean(life_exp)) |>
    ungroup()

# gráfico
line_chart <- mean_life_exp |>
    ggplot(aes(x = year, y = mean_life_exp, col = continent, data_id = id)) +
    geom_line_interactive(linewidth = 2.5) + # espessura da linha do gráfico
    geom_point_interactive(size = 4) + # tamanho do ponto
    theme_minimal(base_size = base_size) +
    labs( # rótulos dos eixos
        x = element_blank(), # não permitir esse rótulo
        y = 'Life expectancy (in years)',
        title = 'Life expectancy over time'
    ) +
    theme(
        text = element_text(
            color = 'grey20'
        ),
        legend.position = 'none', # sem legenda
        panel.grid.minor = element_blank(), # sem linha de grade
        plot.title.position = 'plot' # posição do título
    )
    # scale_color_manual(values = color_palette)

girafe(
    ggobj = line_chart,
    options = list( # várias operações
        # opts_hover(css = ''), # opção p linha "selecionada"
        opts_hover_inv(css = "opacity:0.1;"), # quando o mouse passa numa as demais ficam assim
        # 10% mais transparentes
        opts_sizing(rescale = FALSE) # dimensão do gráfico sem especificação
    ),
    height_svg = 6, # altura do gráfico
    width_svg = 9 # largura
)

