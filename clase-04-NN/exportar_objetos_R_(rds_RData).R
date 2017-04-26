head(iris)

p <- ggplot(iris) +
  geom_point(aes(Sepal.Length, Sepal.Width, color = Species)) +
  theme_minimal()

saveRDS(p, "grafico.rds")

t <- iris %>% 
  group_by(Species) %>% 
  summarise(
    n = n(),
    mean = mean(Sepal.Length)
  )

saveRDS(t, "mitable.rds")

save(t, p, file = "losobjectos.RData")

load("losobjectos.RData")
