#install.packages("recommenderlab")
require(recommenderlab) 

#leer los datos de los ratings - 10 000 054 ratings
data=read.csv("ml-10M100K/ratings.dat", sep = ":", colClasses = c(NA, "NULL"), header = FALSE) 
#quitar la columna con timestamp
data <- data[,-4]
#convertir los ratings en una matriz de tipo realRatingMatrix
#las columnas son las películas y las filas los usuarios
matrix <- as(data, "realRatingMatrix")

#colCounts - cuántas veces ha sido calificada la película
#rowCount - cuántas calificaciónes ha dado un usuario
#trabajamos con las películas que han sido valoradas más de 100 veces
#y los usuarios que han valorado por lo menos 100 películas
#debido a limitaciones computacionales

#7 799 240 ratings
matrix_new <- matrix[rowCounts(matrix) > 100, colCounts(matrix) > 100]
matrix_new


#creamos el esquema
scheme <- evaluationScheme(matrix_new, method = "split", train = .9,
                           k = 1, given = 10, goodRating = 4) 

scheme

#función para obtener parámetros de la librería recommenderlabrats de sanealytics
.get_parameters <- function(p, parameter) {
  if(!is.null(parameter) && length(parameter) != 0) {
    o <- pmatch(names(parameter), names(p))
    
    if(any(is.na(o)))
      stop(sprintf(ngettext(length(is.na(o)),
                            "Unknown option: %s",
                            "Unknown options: %s"),
                   paste(names(parameter)[is.na(o)],
                         collapse = " ")))
    
    p[o] <- parameter
  }
  
  p
}

#el algoritmo de factorización matricial de recommenderlabrats de sanealytics
REAL_RSVD <- function(data, parameter= NULL) {
  
  p <- .get_parameters(list(
    categories = min(100, round(dim(data@data)[2]/2)),
    normalize = "center",
    lambda = 1.5, # regularization
    optim_more = FALSE,
    minRating = NA,
    itmNormalize = FALSE,
    sampSize = NULL,
    scaleFlg = FALSE,
    item_bias_fn=function(x) {0},
    maxit = 100, # Number of iterations for optim
    optimize = function(...) {optim(method = "L-BFGS-B", ...)}
  ), parameter)
  
  model <- c(list(
    description = "full matrix",
    data = data
  ), p)
  
  predict <- function(model, newdata, n = 10,
                      type=c("topNList", "ratings"), ...) {
    
    type <- match.arg(type)
    n <- as.integer(n)
    
    # Combine new user
    combineddata <- model$data@data
    combineddata <- rbind(combineddata, newdata@data)
    
    Y <- t(as.matrix(combineddata)) # This changes NAs to 0
    if(!is.null(model$sampSize)) {
      Y <- Y[, sample(ncol(Y), model$sampSize)]
      print("Took sample of size ", model$sampSize)
    }
    
    R <- 1 * (Y != 0)
    
    Y.avg  <- apply(Y, 1, function(x) {tmp = x
    tmp[tmp==0] <- NA
    mn = mean(tmp, na.rm = TRUE)
    if(is.na(mn)) 0 else mn
    })
    
    # Adjusted
    if(p$itmNormalize) {
      print("Mean normalize by items")
      Y      <- (Y - Y.avg) * R
    } else {
      print("Did not Mean normalize")
    }
    
    # initialization
    num_movies <- dim(Y)[1]
    num_users  <- dim(Y)[2]
    num_features <- model$categories
    lambda = model$lambda
    maxit = model$maxit
    # We are going to scale the data so that optim converges quickly
    scale.fctr <- base::max(base::abs(Y))
    if (model$scaleFlg) {
      print("scaling down")
      Y <- Y / scale.fctr
    }
    
    print(system.time(
      res <- model$optimize(par = runif(num_movies * num_features + num_users * num_features), 
                            fn = J_cost, gr = grr, 
                            Y=Y, R=R, 
                            num_users=num_users, num_movies=num_movies,num_features=num_features, 
                            lambda=lambda, control=list(maxit=maxit, trace=1)) 
    ))    
    
    print(paste("final cost: ", res$value, " convergence: ", res$convergence, 
                res$message, " counts: ", res$counts["function"]))
    
    unrolled <- unroll_Vecs(res$par, Y, R, num_users, num_movies, num_features)
    
    X_final     <- unrolled$X
    theta_final <- unrolled$Theta
    
    Y_final <- (X_final %*% t(theta_final) )
    if (model$scaleFlg) {
      Y_final <- Y_final * scale.fctr 
    }
    
    FUN <- match.fun(model$item_bias_fn)
    Y.adj   <- FUN(Y.avg)
    print(paste0("applying item_bias_fn  range ", min(Y.adj), " to ", max(Y.adj)))
    Y_final <- Y_final + Y.adj
    
    dimnames(Y_final) = dimnames(Y)
    
    ratings <- t(Y_final)
    
    # Only need to give back new users
    ratings <- ratings[(dim(model$data@data)[1]+1):dim(ratings)[1],]
    
    ratings <- new("realRatingMatrix", data=drop0(ratings))
    
    ratings@normalize <- newdata@normalize
    
    ratings <- removeKnownRatings(ratings, newdata)
    
    if(type=="ratings") return(ratings)
    
    getTopNLists(ratings, n=n, minRating=model$minRating)
    
  }
  
  # Helper functions
  
  unroll_Vecs <- function (params, Y, R, num_users, num_movies, num_features) {
    endIdx <- num_movies * num_features
    
    X     <- matrix(params[1:endIdx], nrow = num_movies, ncol = num_features)
    Theta <- matrix(params[(endIdx + 1): (endIdx + (num_users * num_features))], 
                    nrow = num_users, ncol = num_features)
    
    Y_dash     <-   (((X %*% t(Theta)) - Y) * R)
    
    return(list(X = X, Theta = Theta, Y_dash = Y_dash))
  }
  
  J_cost <-  function(params, Y, R, num_users, num_movies, num_features, lambda) {
    
    unrolled <- unroll_Vecs(params, Y, R, num_users, num_movies, num_features)
    X <- unrolled$X
    Theta <- unrolled$Theta
    Y_dash <- unrolled$Y_dash
    
    J <-  .5 * sum(   Y_dash ^2)  + lambda/2 * sum(Theta^2) + lambda/2 * sum(X^2)
    
    return (J) #list(J, grad))
  }
  
  grr <- function(params, Y, R, num_users, num_movies, num_features, lambda) {
    
    unrolled <- unroll_Vecs(params, Y, R, num_users, num_movies, num_features)
    X <- unrolled$X
    Theta <- unrolled$Theta
    Y_dash <- unrolled$Y_dash
    
    X_grad     <- (   Y_dash  %*% Theta) + lambda * X
    Theta_grad <- ( t(Y_dash) %*% X)     + lambda * Theta
    
    grad = c(X_grad, Theta_grad)
    return(grad)
  }
  
  ## construct recommender object
  new("Recommender", method = "RSVD", dataType = class(data),
      ntrain = nrow(data), model = model, predict = predict)
}


#registramos el recomendador
recommenderRegistry$set_entry(method="RSVD", dataType = "realRatingMatrix", 
                              fun=REAL_RSVD,
                              description="Recommender based on Low Rank Matrix Factorization (real data).")

#creamos una lista con los recomendadores que queremos comparar
algorithms <- list(
  "random items" = list(name="RANDOM", param=list(normalize = "Z-score")),
  "popular items" = list(name="POPULAR", param=list(normalize = "Z-score")),
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=50, minRating=3)),
  "Matrix Factorization" = list(name="RSVD", param=list(categories = 10,
                                                        lambda = 10,
                                                        maxit = 100))
)

#ejecutamos los algoritmos y predecimos para los n películas
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20)) #n=c(1, 3, 5, 10, 15, 20))

#curva ROC
plot(results, annotate = 1:4, legend="topleft")

#precision/recall
plot(results, "prec/rec", annotate=3)




