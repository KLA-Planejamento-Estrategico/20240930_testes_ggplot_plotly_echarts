
library(ggplot2)
library(plotly)


# Gráfico de dispersão ----------------------------------------------------

p <- mtcars |>
    tibble::rownames_to_column() |>
    ggplot(aes(x = wt, y = mpg, color = as.character(cyl), text = rowname)) +
    # mapear que é um gráfico, dizer quem são os eixos e colocar nome nos rótulos
    geom_point() # gráfico de dispersão
# theme_light() # tema disponível
ggplotly(p) # deixar interativo com as configurações básicas da função


# Gráfico de área ---------------------------------------------------------

library(dplyr)

time <- as.numeric(rep(seq(1,7),each=7))
value <- runif(49, 10, 100)
group <- rep(LETTERS[1:7],times=7)
data <- data.frame(time, value, group)

data$group <- factor(data$group , levels=c("B", "A", "D", "E", "G", "F", "C") )

p <- ggplot(data, aes(x=time, y=value, fill=group)) +
    geom_area()

ggplotly(p)
