##########################################################################
#' En este programa se encuentran ejemplos del uso del operador %>%. Se
#' utiliza para encadenar varias funciones aplicadas a datos. Ésto es útil
#' para filtrar información en matrices.
#' https://uc-r.github.io/pipe
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# Ej. pipe operator -------------------------------------------------------
# install.packages('magrittr')
# install.packages('dplyr')
library(magrittr)
library(dplyr)

#' This operator will forward a value, or the result of an expression,
#' into the next function call/expression. For instance a function to
#' filter data can be written as:
#' filter(data, variable == numeric_value)
#' or
#' data %>% filter(variable == numeric_value)


# Nested Option: ----------------------------------------------------------
#' This first option is considered a "nested" option such that functions
#' are nested within one another. Historically, this has been the traditional
#' way of integrating code; however, it becomes extremely difficult to read
#' what exactly the code is doing and it also becomes easier to make
#' mistakes when making updates to your code. Although not in violation of
#' the DRY principle (Do not Repeat Yourself), it definitely violates the
#' basic principle of readability and clarity, which makes communication
#' of your analysis more difficult. 
arrange(
  summarize(
    group_by(
      filter(mtcars, carb > 1),
      cyl
    ),
    Avg_mpg = mean(mpg)
  ),
  desc(Avg_mpg)
)



# Multiple Object Option: -------------------------------------------------
#' This second option helps in making the data wrangling steps more explicit
#' and obvious but definitely violates the DRY principle. By sequencing
#' multiple functions in this way you are likely saving multiple outputs
#' that are not very informative to you or others; rather, the only reason
#' you save them is to insert them into the next function to eventually get
#' the final output you desire. This inevitably creates unnecessary copies
#' and wrecks havoc on properly managing your objects.basically it results
#' in a global environment charlie foxtrot! 

a <- filter(mtcars, carb > 1)
b <- group_by(a, cyl)
c <- summarise(b, Avg_mpg = mean(mpg))
d <- arrange(c, desc(Avg_mpg))
print(d)


# %>% Option: -------------------------------------------------------------
#' This final option which integrates %>% operators makes for more efficient
#' and legible code. Its efficient in that it doesn't save unncessary objects
#' (as in option 2) and performs as effectively (as both option 1 & 2) but
#' makes your code more readable in the process. Its legible in that you can
#' read this as you would read normal prose (we read the %>% as "and then"):
#' "take mtcars and then filter and then group by and then summarize and
#' then arrange."

mtcars %>%
  filter(carb > 1) %>%
  group_by(cyl) %>%
  summarise(Avg_mpg = mean(mpg)) %>%
  arrange(desc(Avg_mpg))



# Ej. tesis ---------------------------------------------------------------

m_grande_total <- param$m_grande_total
m_grande_total %>% filter(Alumnos > 10)
View(m_grande_total %>% filter(Alumnos > 10))
View(m_grande_total %>% filter(Materia == "Probabilidad I"))

m_grande_total %>% filter(Materia == "Probabilidad I") %>% 
  filter(Profesor == "Jaime Vázquez Alamilla")
View(m_grande_total %>% filter(Materia == "Probabilidad I") %>% 
       filter(Profesor == "Jaime Vázquez Alamilla"))

m_grande_total %>% filter(Materia == "Probabilidad I") %>% 
  filter(Profesor == "Jaime Vázquez Alamilla")
a <- m_grande_total %>% filter(Materia == "Probabilidad I") %>% 
  filter(Profesor == "Jaime Vázquez Alamilla")
a[,1:3]




















