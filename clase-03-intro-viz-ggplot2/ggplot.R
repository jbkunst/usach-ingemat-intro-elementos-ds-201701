library(ggplot2)
library(dplyr)

data(mpg)

glimpse(mpg)


#' aes signficado el como representar
#' la información en el geom_LOQUESEA
ggplot(data = mpg) +
  geom_point(aes(x = hwy, y = displ)) +
  geom_smooth(aes(x = hwy, y = cty))

ggplot(data = mpg) +
  geom_point(aes(x = hwy, y = displ)) +
  geom_point(aes(x = hwy, y = cty))

#' NO!
#' 

p <- ggplot(data = mpg) +
  geom_point(aes(x = hwy, y = displ)) 

q <- p + stat_smooth(aes(x = hwy, y = displ), se = FALSE)
p
q

#' # Como incluir otra (fuente) infomacion
mpg_res <- summarise(mpg,
          disp_promedio = mean(displ),
          hwy_promedio = mean(hwy))
mpg_res

ggplot(mpg_res) +
  geom_point(aes(x = hwy_promedio,
                 y = disp_promedio))
# muh! bien!

p + geom_point(aes(x = hwy_promedio,
                   y = disp_promedio),
               data = mpg_res,
               color = "red",
               size = 2)
# mah mejor!

#' Aun mas mejor
count(mpg, class, sort = TRUE)

mpg_res2 <- mpg %>% 
  group_by(class) %>% 
  summarise(
    n = n(),
    disp_promedio = mean(displ),
    hwy_promedio = mean(hwy)
    )
mpg_res2

p2 <- p + geom_point(aes(x = hwy_promedio,
                   y = disp_promedio,
                   color = class),
               data = mpg_res2,
               size = 5)

p2
ggsave(p2, filename = "pedos.pdf")

p2 +
  geom_smooth(aes(hwy, displ, group = drv, color = drv))

ggplot(mpg) +
  geom_point(aes(hwy, displ)) + 
  facet_grid(drv ~ trans)

ggplot(mpg) +
  geom_density2d(aes(hwy, displ)) + 
  facet_grid(drv ~ trans)


ggplot(mpg) +
  geom_point(aes(hwy, displ), alpha = 0.5) + 
  geom_density2d(aes(hwy, displ)) + 
  facet_grid(drv ~ trans)


#' densidad estimada
ggplot(mpg) +
  geom_point(aes(hwy, displ), alpha = 0.5) + 
  geom_density2d(aes(hwy, displ)) 

ggplot(mpg) +
  geom_histogram(aes(cty)) +
  geom_freqpoly(aes(cty))

ggplot(mpg) +
  geom_density(aes(cty))


#' Los parametros fijos van FUERA
#' del aes(), 
ggplot(mpg) +
  geom_density(aes(cty, group = class,
                   fill = drv), alpha = 0.5)

ggplot(mpg) +
  geom_density(aes(cty, group = class,
                   fill = class), alpha = 0.5) +
  facet_wrap(~ class, ncol = 1)

ggplot(mpg) +
  geom_density(aes(cty, group = class,
                   fill = class), alpha = 0.5) +
  scale_fill_manual(values = rainbow(7)) +
  labs(
    caption = "este un gra",
    title = "asda",
    subtitle = "ASda",
    x = "ASDa"
  )



