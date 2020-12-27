##########################################################################
#' En este programa se encuentra un ejemplo para utilizar la función
#' apply para una lista de matrices.
#' 
#' Algunos videos de su uso:
#' https://www.youtube.com/watch?v=f0U74ZvLfQo&ab_channel=RichardWebster
#' https://www.youtube.com/watch?v=ejVWRKidi9M&ab_channel=RichardWebster
#' https://www.youtube.com/watch?v=HmBPDTtb6Bg&ab_channel=RichardWebster
##########################################################################
# Ej. 1 -------------------------------------------------------------------
A <- matrix(c(2,4,3,5), 2)
B <- matrix(c(6,8,7,9), 2)
A <- matrix(3,3,3,)
B <- matrix(5,3,3,)

X <- list(A, B)
Y <- do.call(cbind, X)
Y <- array(Y, dim=c(dim(X[[1]]), length(X)))

apply(Y, c(1, 2), mean, na.rm = TRUE)
apply(Y, c(1, 2), var, na.rm = TRUE)

#' apply(X, MARGIN, FUN, ...)
#' X an array, including a matrix.

#' MARGIN a vector giving the subscripts which the function will be
#' applied over. E.g., for a matrix 1 indicates rows, 2 indicates
#' columns, c(1, 2) indicates rows and columns. Where X has named
#' dimnames, it can be a character vector selecting dimension names.

#' FUN the function to be applied: see 'Details'. In the case of
#' functions like +, %*%, etc., the function name must be backquoted
#' or quoted.

# Ej. 2 -------------------------------------------------------------------

A <- matrix(c(2,4,3,5), 2)
B <- matrix(c(6,8,7,9), 2)

Y <- cbind(A,B)
Y <- array(Y, dim=c(dim(A),2))

apply(Y, c(1, 2), mean, na.rm = TRUE)

# Ej. 3 -------------------------------------------------------------------
A <- matrix(c(2,4,3,5), 2)
B <- matrix(c(6,8,7,9), 2)

apply(array(cbind(A,B), dim=c(dim(A),2)), c(1, 2), mean, na.rm = TRUE)
