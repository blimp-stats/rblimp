# Copyright Brian Keller 2026, all rights reserved

#' Posterior summaries of conditional (simple) effects
#'
#' @description
#' Summarizes the conditional-effect posterior draws produced by Blimp's
#' `SIMPLE` command (stored in `model@simple`) as a table of posterior
#' medians, standard deviations, and credible-interval bounds. This surfaces
#' simple slopes numerically -- the same effects that [`simple_plot`],
#' [`jn_plot`], and [`jn_map`] visualize -- which are otherwise not shown by
#' [`summary`]. When `model@simple` is non-empty, the same table is also
#' appended to `summary(model)` automatically.
#'
#' Three kinds of conditional effect are reported, one block per effect:
#' \describe{
#'   \item{Conditional intercept and slope}{From `INTER:`/`SLOPE:` draws, e.g.
#'     `y ~ x | m @ +1 SD`. Each block has an `Intercept` and a `Slope` row.}
#'   \item{Compound-parameter effect}{From `PARAM:` draws -- a generated
#'     conditional effect whose focal is a labeled-parameter expression (e.g.
#'     `b4 + b7*mod`). Each block has a single `Effect` row (no intercept).}
#' }
#'
#' @param model a [`blimp_obj`]. The model must have a `SIMPLE` command output
#'   saved.
#' @param ci a value between 0 and 1 specifying the credible-interval size.
#'   Default `0.95`.
#' @param digits integer number of decimal places used when the result is
#'   printed. Default `3`.
#' @returns invisibly, an object of class `blimp_simple_effects`: a named list
#'   with one numeric matrix per conditional effect (rows are `Intercept`/`Slope`
#'   or `Effect`; columns are the posterior summaries). Returns `NULL`
#'   (invisibly) with a message when no `SIMPLE` output is saved.
#' @seealso [`simple_plot`], [`jn_plot`], [`jn_map`], [`summary`]
#' @examplesIf has_blimp()
#' \dontrun{
#' mydata <- rblimp_sim(
#'     c(
#'         'x ~ normal(0, 1)',
#'         'm ~ normal(0, 1)',
#'         'y ~ normal(10 + 0.5*x + m + 0.2*x*m, 1)'
#'     ),
#'     n = 200,
#'     seed = 981273
#' )
#' m1 <- rblimp(
#'     'y ~ x m x*m',
#'     mydata,
#'     center = ~ m,
#'     simple = 'x | m',
#'     seed = 10972,
#'     burn = 1000,
#'     iter = 1000
#' )
#' simple_effects(m1)
#' }
#' @importFrom methods is
#' @importFrom stats median sd quantile
#' @export
simple_effects <- function(model, ci = 0.95, digits = 3) {
    if (!is(model, 'blimp_obj')) throw_error(
        "{.arg model} is not a `blimp_obj`"
    )
    if (ci >= 1.0 || ci <= 0.0) throw_error(
        "The {.arg ci} must be between 0 and 1"
    )

    ce <- conditional_effects_table(model, ci)
    if (length(ce) == 0) {
        cli::cli_alert_info(
            "No {.field SIMPLE} (conditional effects) output is saved in this model."
        )
        return(invisible(NULL))
    }

    out <- structure(ce, class = "blimp_simple_effects", ci = ci, digits = digits)
    out
}

#' Build the per-effect posterior-summary matrices from `model@simple`.
#'
#' Returns a named list (one numeric matrix per conditional effect, in Blimp's
#' emission order): `INTER:`/`SLOPE:` columns are paired by their shared effect
#' label into `Intercept`/`Slope` rows; each `PARAM:` column becomes a single
#' `Effect` row. Returns an empty list when there is no SIMPLE output.
#' @noRd
conditional_effects_table <- function(model, ci = 0.95) {
    simple <- model@simple
    if (NROW(simple) == 0 || ncol(simple) == 0) return(list())

    sn <- names(simple)
    ci_p  <- (1 - ci) / 2
    probs <- c(ci_p, 1 - ci_p)
    fmt_pct <- function(p) paste0(formatC(100 * p, format = "g", digits = 4), "%")
    # Mirror Blimp's Conditional Effects columns. "Estimate" is the posterior
    # median (as in the rest of the summary); the frequentist ChiSq/PValue/N_Eff
    # columns Blimp prints are not recomputed here.
    stat_names <- c("Estimate", "StdDev", fmt_pct(ci_p), fmt_pct(1 - ci_p))

    col_stats <- function(v) {
        q <- unname(quantile(v, probs, na.rm = TRUE))
        c(median(v, na.rm = TRUE), sd(v, na.rm = TRUE), q[1], q[2])
    }

    blocks <- list()

    # Conditional intercepts/slopes: pair INTER/SLOPE columns by shared label.
    is_is <- grepl('^(INTER|SLOPE): ', sn)
    if (any(is_is)) {
        labs <- sub('^(INTER|SLOPE): ', '', sn)
        for (L in unique(labs[is_is])) {
            rows <- list()
            ic <- which(sn == paste0("INTER: ", L))
            sc <- which(sn == paste0("SLOPE: ", L))
            if (length(ic) == 1) rows[["Intercept"]] <- col_stats(simple[[ic]])
            if (length(sc) == 1) rows[["Slope"]]     <- col_stats(simple[[sc]])
            if (length(rows) == 0) next
            mat <- do.call(rbind, rows)
            colnames(mat) <- stat_names
            blocks[[L]] <- mat
        }
    }

    # Compound-parameter effects: one `Effect` row each (no intercept).
    isp <- startsWith(sn, 'PARAM: ')
    if (any(isp)) {
        for (j in which(isp)) {
            L <- sub('^PARAM: ', '', sn[j])
            mat <- matrix(col_stats(simple[[j]]), nrow = 1,
                          dimnames = list("Effect", stat_names))
            blocks[[L]] <- mat
        }
    }

    blocks
}

#' Render a `blimp_simple_effects` object (or a raw block list) to the console.
#' Shared by [print.blimp_simple_effects] and the `summary` method so the two
#' stay consistent.
#'
#' Mirrors Blimp's "Conditional Effects" table: one shared column header, each
#' conditional effect as a sub-header, and its `Intercept`/`Slope` (or `Effect`)
#' rows indented beneath -- rendered in the same house style as the rest of
#' [summary()] (cli rules + column-aligned values).
#' @noRd
render_conditional_effects <- function(ce, digits = 3, header = TRUE) {
    if (length(ce) == 0) return(invisible(ce))

    # Stack every effect so the columns align across the whole table, then use
    # the same value/column-header recipe as the estimates tables in summary().
    all_mat <- do.call(rbind, ce)
    stat_names <- colnames(all_mat)
    values <- format_estimates(all_mat, digits)

    # Row labels indented 3 spaces, padded to a common width (as in summary()).
    rnames <- paste0("   ", rownames(all_mat))
    nw <- max(nchar(rnames))
    rname_pad <- vapply(rnames, function(nm)
        paste0(nm, strrep(" ", max(0L, nw - nchar(nm)))), character(1), USE.NAMES = FALSE)

    if (header) cli::cli_h2("Conditional Effects")
    cat("\n")

    # Column header row, right-aligned to each value column's width.
    cname_fmt <- vapply(seq_along(stat_names), function(i)
        format(stat_names[i], width = max(nchar(values[, i])), justify = "right"),
        character(1))
    cat(c(strrep(" ", nw), cname_fmt), fill = TRUE)

    # Each conditional effect: a sub-header, then its Intercept/Slope (or
    # Effect) rows beneath -- mirroring Blimp's Conditional Effects grouping.
    ri <- 0L
    for (i in seq_along(ce)) {
        cli::cli_h3(names(ce)[i])
        for (r in seq_len(nrow(ce[[i]]))) {
            ri <- ri + 1L
            cat(paste0(rname_pad[ri], " "))
            cat(values[ri, , drop = FALSE], fill = TRUE)
        }
    }

    # Blimp's intercept note, only when a conditional intercept is shown.
    if ("Intercept" %in% rownames(all_mat)) {
        cli::cli_alert_info(paste(
            "Intercepts are computed by setting predictors not involved",
            "in the conditional effect to zero."
        ))
    }

    invisible(ce)
}

#' Print method for `blimp_simple_effects`
#' @param x A `blimp_simple_effects` object from [`simple_effects`].
#' @param digits Integer number of decimal places. Defaults to the value stored
#'   on `x` (or 3).
#' @param ... Unused.
#' @return `x`, invisibly.
#' @exportS3Method print blimp_simple_effects
print.blimp_simple_effects <- function(x, digits = attr(x, "digits"), ...) {
    if (is.null(digits)) digits <- 3
    render_conditional_effects(x, digits)
    invisible(x)
}
