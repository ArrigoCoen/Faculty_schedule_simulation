##########################################################################
#' En este programa se encuentran algunos ejemplos para cambiar el nombre
#' de las etiquetas en el eje X.
#' http://www.sthda.com/english/wiki/add-an-axis-to-a-plot-with-r-software
#' https://www.tenderisthebyte.com/blog/2019/04/25/rotating-axis-labels-in-r/
##########################################################################

#Source ------------------------------------------------------------------
#Se utiliza el comando source() para poder cargar todas las funciones que
#se encuentran en el archivo Fn_Asignación.
rm(list=ls())  # Borra variables
cat("\014") # Borra consola
#Se establece el directorio en el que se va a trabajar
setwd("C:/Users/miri_/Dropbox/Carpeta compartida MIri/Faculty_schedule_simulation/Hidden_files/Tesis/Programas/ProgFinal")
source("Fn_Asignacion.R")



# Nombres en plot ---------------------------------------------------------
# A Silly Axis Example
# specify the data
x <- c(1:10); y <- x; z <- 10/x

# plot x vs. y
plot(x, y,type="b", pch=21, col="red",
     yaxt="n", lty=3, xlab="X", ylab="Y",axes = FALSE)
axis(side=1, at = 1:10, labels=LETTERS[1:10])
axis(2)
box() #- To make it look like "usual" plot



# Nombres en boxplot ------------------------------------------------------
## Set the random seed so your data will be the same as mine.
set.seed(1234)
dat <- list("Cool dataset one"   = rnorm(100, 0),
            "Cool dataset two"   = rnorm(100, 4),
            "Another long name"  = rnorm(100, 5),
            "Really really long" = rnorm(100, 4))


boxplot(dat, xaxt = "n", yaxt = "n")

axis(side = 1, labels = FALSE)
axis(side = 2, las = 2, mgp = c(3, 0.75, 0))

text(x = 1:length(dat),
     y = par("usr")[3] - 0.45,
     labels = names(dat),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 1.2,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.965)

# Nombres en barplot ------------------------------------------------------
## Set the random seed so your data will be the same as mine.
set.seed(1234)
dat <- rnorm(50,0,1)
dat <- dat[dat>0]
barplot(dat, xaxt = "n", yaxt = "n")

axis(side = 1, labels = FALSE)
axis(2)

text(x = 1:length(dat),
     y = par("usr")[3] - 0.35,
     labels = c("Cool data"),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     #cex = 1.2,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.2)

box() #- To make it look like "usual" plot





# Más ejemplos ------------------------------------------------------------

# NOT RUN {
require(stats) # for rnorm
plot(1:4, rnorm(4), axes = FALSE)
axis(1, 1:4, LETTERS[1:4])
axis(2)
box() #- to make it look "as usual"

plot(1:7, rnorm(7), main = "axis() examples",
     type = "s", xaxt = "n", frame = FALSE, col = "red")
axis(1, 1:7, LETTERS[1:7], col.axis = "blue")
# unusual options:
axis(4, col = "violet", col.axis = "dark violet", lwd = 2)
axis(3, col = "gold", lty = 2, lwd = 0.5)

# one way to have a custom x axis
plot(1:10, xaxt = "n")
axis(1, xaxp = c(2, 9, 7))
# }



# Ej. 2 -------------------------------------------------------------------


# NOT RUN {
plot(-1:1, -1:1, type = "n", xlab = "Re", ylab = "Im")
K <- 16; text(exp(1i * 2 * pi * (1:K) / K), col = 2)

## The following two examples use latin1 characters: these may not
## appear correctly (or be omitted entirely).
plot(1:10, 1:10, main = "text(...) examples\n~~~~~~~~~~~~~~",
     sub = "R is GNU <U+00A9>, but not <U+00AE> ...")
mtext("<U+00AB>Latin-1 accented chars<U+00BB>: <U+00E9><U+00E8> <U+00F8><U+00D8> <U+00E5><<U+00C5> <U+00E6><<U+00C6>", side = 3)
points(c(6,2), c(2,1), pch = 3, cex = 4, col = "red")

text(x = 1:10,
     y = par("usr")[3] - 0.35,
     labels = c("Cool data"),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     #cex = 1.2,
     ## Adjust the labels to almost 100% right-justified.
     adj = 0.2)

text(6, 2, "the text is CENTERED around (x,y) = (6,2) by default",
     cex = .8)
text(2, 1, "or Left/Bottom - JUSTIFIED at (2,1) by 'adj = c(0,0)'",
     adj = c(0,0))
text(4, 9, expression(hat(beta) == (X^t * X)^{-1} * X^t * y))
text(4, 8.4, "expression(hat(beta) == (X^t * X)^{-1} * X^t * y)",
     cex = .75)
text(4, 7, expression(bar(x) == sum(frac(x[i], n), i==1, n)))

## Two more latin1 examples
text(5, 10.2,
     "Le fran<U+00E7>ais, c'est fa<U+00E7>ile: R<U+00E8>gles, Libert<U+00E9>, Egalit<U+00E9>, Fraternit<U+00E9>...")
text(5, 9.8,
     "Jetz no chli z<U+00FC>rit<U+00FC><U+00FC>tsch: (noch ein bi<U+00DF>chen Z<U+00FC>rcher deutsch)")
# }