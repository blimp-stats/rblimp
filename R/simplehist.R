# mcelreath  simplehist:  see https://github.com/rmcelreath/rethinking
# Modified by Brian Keller
# rangi2 <- "#8080FF"


btable <- function(x, breaks='Sturges',...) {
    tmp <- hist(x,breaks = breaks, plot = F)
    y <- array(tmp$counts, length(tmp$counts), dimnames = list(tmp$mids))
    class(y) <- 'table'
    return(y)
}

plot_wrapper <- function(x, ylab, lwd, col, breaks, ...) {
    plot(x, ylab = ylab, lwd = lwd, col = col[1], ...)
}


simplehist_wrapper <- function (x, round = TRUE, ylab = "Frequency", off = 0.0, lwd = 3,
                        col = c("black", "red"), plot = TRUE, ...) {
    # Added bk
    if (round == FALSE) {
        y <- btable(x,...)
    } else {
        x <- round(x)
        y <- table(x)
    }
    if ( plot ) plot_wrapper(y, ylab = ylab, lwd = lwd, col = col[1], ...)
    return(invisible(y))
}
simplehist_wrapper_list <- function (x, round = TRUE, xlab = "", ylab = "Frequency", off = 0.0, lwd = 3,
                        col = c("black", "red"), plot = TRUE, ticks = NULL, ...) {
    maxX <- max(sapply(x, function(i) {max(i)}))
    ticksX <- unlist(lapply(x, function(i){as.numeric(attr(i,"dimnames")[[1]])}))
    if (is.null(ticks)) {
        ticks <- pretty(ticksX)
        rangeX <- c(min(ticks),max(ticks))
    } else {
        rangeX <- c(min(ticksX),max(ticksX))
    }
    if (plot) {
        plot(NULL,xlim = rangeX + c(-1, 1) * off, ylim = c(0, maxX), xlab = xlab, ylab = ylab, xaxt = "n", ...)
        axis(side=1, at=ticks)
        if ( length(col) < length(x) ) col <- rep_len(col,length(x))
        for (i in seq_along(x)) {
            xoff <- (i - 1) * off
            attr(x[[i]],"dimnames") <- list(as.numeric(attr(x[[i]],"dimnames")[[1]]) + xoff)
            lines(x[[i]], lwd = lwd, col = col[i],...)
        }
    }
    return(invisible(x))
}


#' @export
setGeneric("simplehist",
    function( x, param, round = TRUE, ylab = "Frequency", off = 0.0, lwd = 3,
                        col = c("black", "red"), plot = TRUE, ...) {
        if (class(x) == 'table') {
            if ( plot ) plot_wrapper(x, ylab = ylab, lwd = lwd, col = col[1], ...)
            return(invisible(x))
        } else {
            return(simplehist_wrapper(x, round = round, ylab = ylab, off = off, lwd = lwd,
                                      col = col, plot = plot, ...))
        }
    }
)

setMethod("simplehist", "blimp_obj",
function( x, param, round = FALSE, ylab = "Frequency", off = 0.0, lwd = 3,
                        col = c("black", "red"), plot = TRUE, ...) {
    Y <- simplehist_wrapper(x@iterations[,param], round = round, ylab = ylab, off = off, lwd = lwd,
                        col = col, plot = plot, ...)
    if ( length(col) < 2 ) col <- rep_len(col, 2)
    if ( plot) abline(v = x@estimates[param,3:4],  # Add 95% intervals
       col=col[2], lwd=2, lty=2)
    attr(Y,'ci') <-  x@estimates[param,3:4]
    return(invisible(Y))
})

setMethod("simplehist", "list",
function( x, param, round = FALSE, ylab = "Frequency", off = 0.0, lwd = 3,
                        col = c("black", "red"), plot = TRUE, ...) {

    x <- lapply(x, function(i) {
         simplehist(i, round = round, ylab = ylab, off = off, lwd = lwd,
                        col = col, plot = FALSE, ...)
    })

    Y <- simplehist_wrapper_list(x, round = round, ylab = ylab, off = off, lwd = lwd,
                        col = col, plot = plot, ...)
    if (plot) {
        if ( length(col) < (length(x)*2 )) col <- rep_len(col,length(x)*2)
    # Add cis
        for (i in seq_along(x)) {
            ci <- attr(x[[i]],'ci')
            if(!is.null(ci)) {
                xoff <- (i - 1) * off
                abline(v = ci + xoff, col=col[i+length(x)], lwd=2, lty=2) # Add 95% intervals
            }
        }
    }
    return(invisible(Y))
})

