##########################################################################
#' En este programa se encuentran ejemplos de la función ggboxplot()
#' https://www.datanovia.com/en/blog/how-to-create-a-nice-box-and-whisker-plot-in-r/
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola

# Load required R packages
install.packages('ggpubr')
library(ggpubr)


# Ej. ---------------------------------------------------------------------
# Data preparation
df <- ToothGrowth
head(df)


# Create basic boxplot
ggboxplot(df, x = "dose", y = "len", add = "jitter")
ggboxplot(df, x = "dose", y = "len")


# Create a box plot with summary table
ggsummarystats(
  df, x = "dose", y = "len", add = "jitter",
  color = "supp", palette = "npg",
  ggfunc = ggboxplot
)




# Ej. R documentation -----------------------------------------------------
#' https://www.rdocumentation.org/packages/ggpubr/versions/0.4.0/topics/ggboxplot
# Load data
data("ToothGrowth")
df <- ToothGrowth

# Basic plot
# +++++++++++++++++++++++++++
# width: change box plots width
ggboxplot(df, x = "dose", y = "len", width = 0.8)

# Change orientation: horizontal
ggboxplot(df, "dose", "len", orientation = "horizontal")

# Notched box plot
ggboxplot(df, x = "dose", y = "len",
          notch = TRUE)

# Add dots
# ++++++++++++++++++++++++++
ggboxplot(df, x = "dose", y = "len",
          add = "dotplot")

# Add jitter points and change the shape by groups
ggboxplot(df, x = "dose", y = "len",
          add = "jitter", shape = "dose")


# Select and order items
# ++++++++++++++++++++++++++++++

# Select which items to display: "0.5" and "2"
ggboxplot(df, "dose", "len",
          select = c("0.5", "2"))

# Change the default order of items
ggboxplot(df, "dose", "len",
          order = c("2", "1", "0.5"))


# Change colors
# +++++++++++++++++++++++++++
# Change outline and fill colors
ggboxplot(df, "dose", "len",
          color = "black", fill = "gray")

# Change outline colors by groups: dose
# Use custom color palette
# Add jitter points and change the shape by groups
ggboxplot(df, "dose", "len",
          color = "dose", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter", shape = "dose")

# Change fill color by groups: dose
ggboxplot(df, "dose", "len",
          fill = "dose", palette = c("#00AFBB", "#E7B800", "#FC4E07"))


# Box plot with multiple groups
# +++++++++++++++++++++
# fill or color box plot by a second group : "supp"
ggboxplot(df, "dose", "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))




# Ej. con datos tesis -----------------------------------------------------
load("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal/mat_calif_generaciones.RData")
mat_calif_generaciones

#Definimos el data.frame que necesitamos para graficar los boxplots
df_calif_gen <- data.frame(Calificaciones = 0,
                           Generaciones = sort(rep(c(1:dim(mat_calif_generaciones)[2]),
                                                   dim(mat_calif_generaciones)[1])))
for(k in 1:dim(mat_calif_generaciones)[2]){
  vec <- (10*(k-1))+1:10
  # cat("\nk = ", k,"\n\n vec = ",vec)
  df_calif_gen[vec,1] <- mat_calif_generaciones[,k]
}



# Create basic boxplot
ggboxplot(df_calif_gen, x = "Generaciones",
          y = "Calificaciones",
          add = "jitter")













