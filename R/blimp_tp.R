## traceplot object class
# Copyright Brian Keller 2022, all rights reserved

# traceplot class
#' @export
setClass("blimp_tp", slots=list(name='character', data="matrix"))


# Compute psrf
psrf <- function(x, split_chain = TRUE) {
    # Coerce x to matrix
    x <- as.matrix(x)

    # Set up params
	n <- ifelse(split_chain, floor(NROW(x) / 2), NROW(x))
	m <- ifelse(split_chain, NCOL(x) * 2, NCOL(x))
	# If not enough draws error
	if (n < 2) stop('Not enough draws')

	# Create matrix of entire parameters
	chaindat <-  do.call('cbind', apply(x, 2, function(x){
	    if (split_chain)
	        return(cbind(x[1:n],  x[ (n+1):(2 * n) ]))
	    else
	        return(x[1:n])
	}, simplify = F))

	# Compute PSR
	t_j <- colMeans(chaindat)
    t_bar <- mean(t_j)

	# Compute between
	B = (1/ (m - 1)) * sum( (t_j - t_bar)^2 );

	# Compute within
	W <- sum((sweep(chaindat, 2, t_j)^2) / (n-1) )
	W <- ((n-1)/n) * (W / m);

	# Compute psr
	return(sqrt((W + B) / W))
}

psrf_lhalf <- function(x, split_chain = TRUE) {
    # Coerce x to matrix
    x <- as.matrix(x)
    return(psrf(x[(floor(NROW(x) / 2) + 1 ):NROW(x),], split_chain))
}

#' @export
plot.blimp_tp <- function(x, y, colors=NULL, ...) {
    # Check colors
    if (is.null(colors)) {
        colors <- seq_along(x@data)
    } else {
        if (length(colors) != length(x@data)) {
            stop(paste0("colors must be length of number of chains (",length(x@data),")"))
        }
    }

    # Create title
    plot_title <- paste0(x@name, '; PSRF = ', sprintf('%.3f;', psrf_lhalf(x@data) ))

    for (i in seq_len(NCOL(x@data))) {
        if (i == 1)
            plot(x@data[,i], type = 'l', col = colors[i],
                 xlab = 'iteration', ylab ='value',
                 main = plot_title, ...)
        else
            lines(x@data[,i], type = 'l', col=colors[i])
    }
}

# Plot traceplot obj
#' @export
setMethod("plot", "blimp_tp", function(x, y,colors=NULL, ...) {
    plot.blimp_tp(x,y, colors, ...)
})

# Show traceplot obj
#' @export
setMethod("show", "blimp_tp", function(object) {
    plot.blimp_tp(object)
})

# Generate traceplot objects
traceplot <- function(object, param) {
    if (missing(param)) {
        out <- lapply(seq_len(nrow(object@estimates)), function(iter) {
            return(new('blimp_tp',
                       name = rownames(object@estimates)[iter],
                       data = sapply(object@burn, function(x) x[,iter + 2] )
            ))
        })
    } else {
        out <- new('blimp_tp',
                   name = rownames(object@estimates)[param],
                   data = sapply(object@burn, function(x) x[,param + 2])
        )
    }
    return(out)
}


