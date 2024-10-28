# How to Use Better Colors in ggplot (3 Easy Ways) by Albert Rapp
# https://www.youtube.com/watch?v=ZVSbY4z7tMY

# Primeira forma ----------------------------------------------------------

library(tidyverse)

my_penguins_plot <- palmerpenguins::penguins |>
    filter(!is.na(sex)) |>
    ggplot(
        aes(
            x = bill_length_mm,
            y = body_mass_g,
            color = species
            )
    ) +
    geom_point(size = 5,alpha = 0.85) + # tamanho e transparencia dos pontos
    theme_minimal(base_size = 16, base_family = "Source Sans Pro") +
    labs(
        x = "Bill length (in mm)",
        y = "Penguin weights (in g)",
        title = "Penguins from the Palmer Archipelago",
        color = "Species"
    )
my_penguins_plot

my_penguins_plot +
    scale_color_brewer(palette = "Set2")

my_penguins_plot +
    scale_color_viridis_d(option = "cividis")


# Bonus -------------------------------------------------------------------

palmerpenguins::penguins |>
    filter(!is.na(sex)) |>
    ggplot(
        aes(
            x = bill_length_mm,
            y = body_mass_g,
            fill = species
        )
    ) +
    geom_point(shape = 21, size = 5,alpha = 0.85, color = "grey10") +
    # shp define que o formato será um círculo com borda
    theme_minimal(base_size = 16, base_family = "Source Sans Pro") +
    labs(
        x = "Bill length (in mm)",
        y = "Penguin weights (in g)",
        litle = "Penguins from the Palmer Archipelago",
        color = "Species"
    ) +
    scale_fill_viridis_d(option = "cividis")
#scale_color_*

# Segunda forma -----------------------------------------------------------

my_favorite_colors <- c("#E69F00", "#009E73", "#007282")
names(my_favorite_colors) <- c("Adelie", "Chinstrap", "Gentoo")

my_favorite_colors["Adelie"]
my_favorite_colors[1]

my_penguins_plot +
    scale_color_manual(values = my_favorite_colors) # escolhemos as cores manualmente
# scale_color_manual

# Terceira forma ----------------------------------------------------------

cores_viridis <- viridisLite::viridis(n = 4)
cores_viridis[1]

ex_3 <- palmerpenguins::penguins |>
    filter(!is.na(sex)) |>
    mutate(
        my_color = case_when(
            bill_length_mm < 40 ~ cores_viridis[1],
            between(bill_length_mm, 40, 50) ~ "#009E73",
            bill_length_mm > 50 ~ "#E69F00"
        )
    )

cores <- ex_3$my_color |> unique()
species <- ex_3$species |> unique()

names(cores) <- species


ex_3 |> ggplot(
        aes(
            x = bill_length_mm,
            y = body_mass_g,
            color = my_color
        )
    ) +
    geom_point(size = 5, alpha = 0.85) +
    theme_minimal(base_size = 16, base_family = "Source Sans Pro") +
    labs(
        x = "Bill length (in mm)",
        y = "Penguin weights (in g)",
        title = "Penguins from the Palmer Archipelago",
        color = "Species",

    ) +
    scale_color_identity()
# scale_color_identity
