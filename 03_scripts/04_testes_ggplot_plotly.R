library(plotly)
library(ggplot2)

data <- readxl::read_xlsx("01_raw_data/teste_primeiro_grafico.xlsx") |> janitor::clean_names()


# Gráfico de dispersão  ---------------------------------------------------

# transformando os dados com pivot_longer
data_long <- data  |>
    tidyr::pivot_longer(
        cols = starts_with("x20"), names_to = "ano", values_to = "volume"
    )|>
    dplyr::mutate(
        ano = stringr::str_remove_all(ano, pattern = "x"),
        volume = volume/10^6
    ) |>
    dplyr::rename(densidade = estoque_em_pe)
# dplyr::group_by(ano, densidade) |>
# dplyr::summarise(volume = sum(volume, na.rm = TRUE))

# dispersão
d <- data_long |>
    tibble::rownames_to_column() |>
    ggplot(aes(x = ano, y = volume, color = densidade, text = rowname)) +
    # mapear que é um gráfico, dizer quem são os eixos e colocar nome nos rótulos
    geom_point()
ggplotly (d)


# Gráfico de área ---------------------------------------------------------

ano <- data_long$ano
vol <- data_long$volume
# densidade_distinct <- data_long |> dplyr::distinct(densidade)
densidade <- data_long$densidade
data <- data.frame(ano, vol, densidade)

data$densidade <- factor(data$densidade , levels=c("Alta DBM (PTA)", "Média DBM (PTA)", "Média-baixa (PTA)"))

d <- ggplot(data, aes(x=ano, y=vol, fill=densidade)) +
    geom_area()

ggplotly(d)


# teste seguindo o exemplo de gráfico de área
ano_test <- as.numeric(rep(seq(2022,2041)))
vol_test <- data_long$volume
densidade_test <- data_long$densidade
#densidade_test <- rep(LETTERS[1:3],times=20)
data_test <- data.frame(ano_test, vol_test, densidade_test)

a <- ggplot(data_test, aes(x=ano_test, y=vol_test, fill=densidade_test)) +
    geom_area()

ggplotly(a)
