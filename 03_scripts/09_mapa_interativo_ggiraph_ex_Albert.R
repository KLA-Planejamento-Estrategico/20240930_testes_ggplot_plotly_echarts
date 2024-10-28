
library(tidyverse)
library(giscoR) # pacote com os dados e funções para trabalhá-los
library(ggiraph)
library(sf) # para manipular os dados geométricos

# Organizando os dados ----------------------------------------------------

germany_districts <- gisco_get_nuts(
    year = "2021",  # filtra um ano específico da base de dados
    nuts_level = 3, # pegamos a menor unidade territorial do país
    epsg = 3035, # tipo de projeção que se quer utilizar
    country = 'Germany'
) |>
    # Nicer output
    as_tibble() |>
    janitor::clean_names()
germany_districts

# adicionando a fronteira dos estados
germany_states <- gisco_get_nuts(
    year = "2021",
    nuts_level = 1,
    epsg = 3035,
    country = 'Germany'
) |>
    as_tibble() |>
    janitor::clean_names()
germany_states

# mesclando os dados geográficos - qual distrito pertence a qual estado
state_nmbrs <- map_dbl(
    germany_districts$geometry,
    \(x) { # transformou na função anônima x as coords dos distritos
        map_lgl( # diz ser mapa lógico
            germany_states$geometry, # pegamos os dados geométricos
            \(y) st_within(x, y) |> # transformou na função anônima y as coords do estado
                as.logical() # transformou em valores lógicos
        ) |> which()
    }
)
state_nmbrs

# selecionando os dados de estado e distritos
# já que temos para cada nome do distrito um número do estado correspondente
germany_districts_w_state <- germany_districts |>
    mutate(
        state = germany_states$nuts_name[state_nmbrs]
    )
germany_districts_w_state |> select(nuts_name, state)
germany_districts_w_state |> dplyr::glimpse()

# gerando o texto do tooltip com outra fonte e tamanho
# script CSS p gerar função onde todos os nomes receberão essa formatação
make_nice_label <- function(nuts_name, state) {
    nuts_name_label <- htmltools::span(
        nuts_name, # trabalhando na coluna do distrito
        style = htmltools::css(
            fontweight = 600,
            font_family = 'Source Sans Pro',
            font_size = '32px'
        )
    )
    state_label <- htmltools::span(
        state, # trabalhando na coluna do estado
        style = htmltools::css(
            font_family = 'Source Sans Pro', # nome da fonte
            font_size = '20px' # tamanho da fonte
        )
    )
    glue::glue('{nuts_name_label}<br>{state_label}') # <> pula linha
}


?purrr::map2_chr()


argumento1_nutsname <- germany_districts_w_state$nuts_name
argumento2_states <- germany_districts_w_state$state


# apenas 1 item de cada vez
resultado_funcao <- make_nice_label(
    nuts_name = "Nürnberg, Kreisfreie Stadt",
    state = "Bayern"
)


# vai dar erro
make_nice_label(
    nuts_name = argumento1_nutsname,
    state = argumento2_states
)


resultado_funcao_vetor <- purrr::map2_chr(
    .x = argumento1_nutsname,
    .y = argumento2_states,
    .f = make_nice_label # colocando as colunas anteriores na função
)



htmltools::HTML(resultado_funcao)
htmltools::html_print(resultado_funcao)


# Escrevendo para um arquivo
writeLines(resultado_funcao,
           "exemplo.html"
)
# Abrindo o arquivo no navegador padrão
browseURL(
    "exemplo.html"
)

# Gráfico estático --------------------------------------------------------

gg_plt <- germany_districts_w_state |>
    mutate(

        nice_label = map2_chr(
            nuts_name,
            state,
            make_nice_label # colocando as colunas anteriores na função
        )
    ) |>
    ggplot(aes(geometry = geometry)) + # geometry <POLYGON [m]>
    geom_sf( # gráfico "espacial"
        data = germany_states,
        aes(fill = nuts_name), # preencher cada estado
        color = 'black',
        linewidth = 0.5 # largura da linha de contorno
    )  +
    geom_sf_interactive( # tornar camada interativa
        fill = NA, # tirar preenchimento da camada que estava por cima (cinza)
        aes(
            data_id = nuts_id,
            tooltip = nice_label
        ),
        linewidth = 0.1 # deixando os "municipios" com linhas mais finas
    ) +
    theme_void() + # tema sem grade e coordenadas
    theme(
        legend.position = 'none' # retirar legenda
    ) + # adicionar palheta de cores que se quer
    scale_fill_manual(
        values = c("#A0CBE8FF", "#F28E2BFF", "#FFBE7DFF", "#59A14FFF", "#8CD17DFF", "#B6992DFF", "#F1CE63FF", "#499894FF", "#86BCB6FF", "#E15759FF", "#FF9D9AFF", "#79706EFF", "#BAB0ACFF", "#D37295FF", "#FABFD2FF", "#B07AA1FF", "#D4A6C8FF", "#9D7660FF", "#D7B5A6FF")
    )


# gerar interatividade
girafe(
    ggobj = gg_plt,
    options = list( # listas de opções
        opts_hover( # comportamento de quando passamos o mouse
            css = girafe_css(
                css = '', # não vamos adicionar um estilo específico
                area = 'stroke: black; fill: black;' # contorno e preenchimento em preto apenas na seleção
            )
        )
    )
)
