##########################################################################
#' En este programa se encuentra el código con el ejemplo del modelo con
#' mezcla de Normales. El ejemplo mostrado es de los pingüinos Palmer.
#' https://www.youtube.com/watch?v=2Tw0peN814k&ab_channel=MLTArtificialIntelligence
#' https://github.com/allisonhorst/palmerpenguins
#' https://allisonhorst.github.io/palmerpenguins/articles/articles/examples.html
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")


# Palmer penguins ---------------------------------------------------------
# install.packages("palmerpenguins")
# library(palmerpenguins)
data(package = 'palmerpenguins')

head(penguins)
head(penguins_raw)
str(penguins)

# vignette("examples")

# install.packages("tidyverse")
# library(tidyverse)
# penguins %>% count(species)
# 
# penguins %>% 
#   group_by(species) %>% 
#   summarize(across(where(is.numeric), mean, na.rm = TRUE))


# install.packages(ggplot2)
# library(ggplot2)

# Complete data scenario --------------------------------------------------
#' This is the case where all the data is defined

##### Penguin mass vs. flipper length
mass_flipper <- ggplot(data = penguins, 
                       aes(x = flipper_length_mm,
                           y = body_mass_g)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Penguin size, Palmer Station LTER",
       subtitle = "Flipper length and body mass for Adelie, Chinstrap and Gentoo Penguins",
       x = "Flipper length (mm)",
       y = "Body mass (g)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.2, 0.7),
        legend.background = element_rect(fill = "white", color = NA),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

mass_flipper


##### Flipper length vs. bill length
flipper_bill <- ggplot(data = penguins,
                       aes(x = flipper_length_mm,
                           y = bill_length_mm)) +
  geom_point(aes(color = species, 
                 shape = species),
             size = 3,
             alpha = 0.8) +
  theme_minimal() +
  scale_color_manual(values = c("darkorange","purple","cyan4")) +
  labs(title = "Flipper and bill length",
       subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
       x = "Flipper length (mm)",
       y = "Bill length (mm)",
       color = "Penguin species",
       shape = "Penguin species") +
  theme(legend.position = c(0.85, 0.15),
        legend.background = element_rect(fill = "white", color = NA),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, face= "italic"),
        plot.caption.position = "plot")

flipper_bill

##### Histograms

flipper_hist <- ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_histogram(aes(fill = species), 
                 alpha = 0.5, 
                 position = "identity") +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  theme_minimal() +
  labs(x = "Flipper length (mm)",
       y = "Frequency",
       title = "Penguin flipper lengths")

flipper_hist


# Incomplete data scenario --------------------------------------------------


flipper_len_density <- ggplot(data = penguins, aes(x = flipper_length_mm)) +
  geom_density() +
  theme_minimal() +
  labs(title = "Flipper length density",
       x = "Flipper length (mm)",
       y = "Density",
       color = "Penguin species",
       shape = "Penguin species")
  
flipper_len_density <- flipper_len_density + 
  geom_vline(xintercept = 192,col = "red",size = 2)+
  geom_vline(xintercept = 215,col = "blue",size = 2)

flipper_len_density


# Black-box approach to get GMM -------------------------------------------
#GMM = Gaussian Mixture Model
#' En el "Black-box approach" no sabemos realmente qué ocurre dentro de las
#' iteraciones. En este caso con la semilla = 1 la función hace 30 iteraciones
#' antes de converger.


# install.packages("plotGMM")
# library("plotGMM")
# install.packages("mixtools")
# library("mixtools")
# set.seed(1)
# mixmdl <- mixtools::normalmixEM(faithful$waiting, k = 2)
# x <- mixmdl$x
# x <- data.frame(x)
# ggplot2::ggplot(data.frame(x)) +
#   ggplot2::geom_density(ggplot2::aes(x), color="black", fill="black") +
#   ggplot2::stat_function(geom = "line", fun = plot_mix_comps,
#                          args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),
#                          colour = "red") +
#   ggplot2::stat_function(geom = "line", fun = plot_mix_comps,
#                          args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),
#                          colour = "blue")

set.seed(1)
wait <- penguins$flipper_length_mm
wait <- na.omit(penguins$flipper_length_mm)
mixmdl <- normalmixEM(wait,k = 2)
gmm_plot_black_box <- data.frame(x = mixmdl$x) %>%
  ggplot() + 
  geom_histogram(aes(x,..density..),binwidth = 1,colour = "black",
                 fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl$mu[1], mixmdl$sigma[1], lam = mixmdl$lambda[1]),
                colour = "red", lwd = 1.5) + 
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(mixmdl$mu[2], mixmdl$sigma[2], lam = mixmdl$lambda[2]),
                colour = "blue", lwd = 1.5) +
  ylab("Density") +
  xlab("Values")

gmm_plot_black_box

##Gráfica con densidad del modelo
wait <- na.omit(penguins$flipper_length_mm)
set.seed(1)
mixmdl <- normalmixEM(wait,k = 2)
plot(mixmdl,which=2)
lines(density(wait), lty=2, lwd=2)
lines(density(rnorm(1000,mean = mixmdl$mu,sd = mixmdl$sigma)), lty=1,
      lwd=2,col = "blue")

# Output 1:
mixmdl$lambda
mixmdl$mu
mixmdl$sigma

# Output 2:
mixmdl$loglik
head(mixmdl$posterior,n=25L)
#' En la columna 1 de la tabla anterior se tiene la probabilidad
#' de que el dato del renglón i pertenezca a la primera componente
#' (a la normal 1). En la segunda columna se tiene la probabilidad
#' de que el dato del renglón i pertenezca a la segunda componente
#' (a la normal 2).



# What's going on inside the E-M? -----------------------------------------
#' Imagine when our data is "unlabeled" or "incomplete"

# set.seed(1)
wait <- na.omit(penguins$flipper_length_mm)
wait.kmeans <- kmeans(wait,2)
wait.kmeans.cluster <- wait.means$cluster

wait.df <- data.frame(x = wait, cluster = wait.kmeans.cluster)

# waitl.df <- wait.df %>%
#   mutate(num = nrow(wait.df)) %>%
#   ggplot(aes(x = wait, colour = factor(wait.kmeans.cluster))) + #y = num,
#   geom_point() +
#   ylab("Values") +
#   ylab("Data Point Number") +
#   scale_color_discrete(name = "Cluster") +
#   ggtitle("K-means Clustering")
# 
# waitl.df


# E-M initialization step -------------------------------------------------

wait.summary.df <- wait.df %>%
  # grouping(cluster) %>%
  grouping(wait.kmeans.cluster) %>%
  # group_by(cluster)
  summarise(mu = mean(x),variance = var(x),std = sd(x),size = n(), .groups = 'drop' )

wait.summary.df <- wait.summary.df %>%
  mutate(alpha = size / sum(size))


wait.summary.df$cluster
wait.summary.df$size
wait.summary.df$alpha





# Final ggplot ------------------------------------------------------------
#'  Gráfica obtenida con las funciones "e.step" y "m.step". El código
#'  mostrado en el video no está completo por lo que no se pudo correr aquí.
#'  La gráfica final es la misma que la guardada en "gmm_plot_black_box"

wait <- na.omit(penguins$flipper_length_mm)
final_gmm_plot <- data.frame(x = wait) %>%
  ggplot() + 
  geom_histogram(aes(x,..density..),binwidth = 1,colour = "black",
                 fill = "white") +
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(m.step$mu[1], sqrt(m.step$var[1]),
                            lam = m.step$lambda[1]),
                colour = "red", lwd = 1.5) + 
  stat_function(geom = "line", fun = plot_mix_comps,
                args = list(m.step$mu[2], sqrt(m.step$var[2]),
                            lam = m.step$lambda[2]),
                colour = "blue", lwd = 1.5) +
  ylab("Density") +
  xlab("Values") +
  ggtitle("Final FMM Fit")

final_gmm_plot




