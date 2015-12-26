## This file contains the function pair: makeCacheMatrix and cacheSolve.
## These functions can be used to calculate, cache and retrieve the inverse of an invertible matrix.
##
## Usage Example
## =============
" # (begin multi-line comment) ========================

> m <- matrix(rnorm(100, 50, 10), nrow = 10, ncol = 10)
> m_cache_matrix <- makeCacheMatrix(m)
> cacheSolve(m_cache_matrix)
[calculating inverse]
[,1]         [,2]        [,3]        [,4]         [,5]        [,6]         [,7]        [,8]        [,9]        [,10]
[1,]  0.14565820 -0.043074822 -0.08235415  0.03352019 -0.037846159 -0.04550050 -0.020517784 -0.09249064  0.10998585  0.037918583
[2,]  0.36108151 -0.090959705 -0.24673764  0.16699876 -0.090961183 -0.15931841 -0.010388022 -0.22427269  0.25123729  0.044886644
[3,]  0.07386489 -0.009231841 -0.08227419  0.02593144 -0.019312881 -0.03782041  0.031929458 -0.03713565  0.03714128  0.013360848
[4,] -0.17280369  0.047791453  0.13923043 -0.07678742  0.020943188  0.08652820  0.008873098  0.11715518 -0.12605070 -0.046947949
[5,]  0.17911644 -0.065722448 -0.14375091  0.08999462 -0.034071147 -0.06253619 -0.005253026 -0.12800831  0.13472831  0.040440697
[6,] -0.05937585  0.018361302  0.01403702 -0.02019653  0.021686434  0.02906047 -0.014194004  0.05769902 -0.03910947 -0.005041512
[7,] -0.23858998  0.074243815  0.17379680 -0.11059095  0.074382934  0.10007891  0.009565113  0.15119375 -0.17219410 -0.060468702
[8,]  0.03200969 -0.039341239 -0.01214113  0.02610060 -0.003798789 -0.05423538  0.030470091 -0.01583554  0.02857212  0.008571507
[9,] -0.22573365  0.080037116  0.15087334 -0.10801949  0.046601691  0.11000518 -0.015493201  0.12285987 -0.12900899 -0.027430329
[10,] -0.12124272  0.039439579  0.11454329 -0.03596921  0.028314380  0.04371066 -0.003870047  0.07066492 -0.12055981 -0.012258606
> cacheSolve(m_cache_matrix)
[returning cached inverse]
[,1]         [,2]        [,3]        [,4]         [,5]        [,6]         [,7]        [,8]        [,9]        [,10]
[1,]  0.14565820 -0.043074822 -0.08235415  0.03352019 -0.037846159 -0.04550050 -0.020517784 -0.09249064  0.10998585  0.037918583
[2,]  0.36108151 -0.090959705 -0.24673764  0.16699876 -0.090961183 -0.15931841 -0.010388022 -0.22427269  0.25123729  0.044886644
[3,]  0.07386489 -0.009231841 -0.08227419  0.02593144 -0.019312881 -0.03782041  0.031929458 -0.03713565  0.03714128  0.013360848
[4,] -0.17280369  0.047791453  0.13923043 -0.07678742  0.020943188  0.08652820  0.008873098  0.11715518 -0.12605070 -0.046947949
[5,]  0.17911644 -0.065722448 -0.14375091  0.08999462 -0.034071147 -0.06253619 -0.005253026 -0.12800831  0.13472831  0.040440697
[6,] -0.05937585  0.018361302  0.01403702 -0.02019653  0.021686434  0.02906047 -0.014194004  0.05769902 -0.03910947 -0.005041512
[7,] -0.23858998  0.074243815  0.17379680 -0.11059095  0.074382934  0.10007891  0.009565113  0.15119375 -0.17219410 -0.060468702
[8,]  0.03200969 -0.039341239 -0.01214113  0.02610060 -0.003798789 -0.05423538  0.030470091 -0.01583554  0.02857212  0.008571507
[9,] -0.22573365  0.080037116  0.15087334 -0.10801949  0.046601691  0.11000518 -0.015493201  0.12285987 -0.12900899 -0.027430329
[10,] -0.12124272  0.039439579  0.11454329 -0.03596921  0.028314380  0.04371066 -0.003870047  0.07066492 -0.12055981 -0.012258606

" # (end multi-line comment) ========================



## makeCacheMatrix creates a function that returns a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # checks for invalid matrix (matrix with NA elements, non-square matrix, singular matrix)
    if (!is.matrix(x) || is.na(x)) {
        stop("x is not a matrix", call. = TRUE)
    } else if (nrow(x) != ncol(x)) {
        stop("x is not square, thus is not invertible", call. = TRUE)
    } else if (any(is.na(x))) {
        stop("x contains NA, thus is not invertible", call. = TRUE)
    } else if (det(x) == 0) {
        stop("x is singular, thus is not invertible", call. = TRUE)
    }
    
    cached_inverse <- NULL
    set <- function(y) {
        x <<- y
        cached_inverse <<- NULL
    }
    get <- function() {
        x
    }
    set_inverse <- function(inv) {
        cached_inverse <<- inv
    }
    get_inverse <- function() {
        cached_inverse
    }
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

solveCacheWrapperMatrix <- function(x, ...) {
    calculated_inverse <- solve(x$get())
    x$set_inverse(calculated_inverse)
    calculated_inverse
}

## cacheSolve creates a function that calculates the inverse of the given matrix wrapper object (which was returned by makeCacheMatrix$get)
## Note: x may pass the singularity test in makeCacheMatrix, but result in an error such as matrix being computationally singular
cacheSolve <- function(x, ...) {

    # attempt to retrieve cached inverse and return it if possible
    cached_inverse <- x$get_inverse()
    if (!is.null(cached_inverse)) {
        message("[returning cached inverse]")
        return(cached_inverse)
    }
    
    # calculate, cache and return inverse of x
    message("[calculating inverse]")
    
    solveResult <- tryCatch({
            solveCacheWrapperMatrix(x)
        }, 
        warning = function(condition) {
            message("[warning encountered in attempt to calculate inverse]")
            stop(condition)
        },
        error = function(condition) {
            message("[error encountered in attempt to calculate inverse]")
            stop(condition)
        }
    )
    
    solveResult
}



