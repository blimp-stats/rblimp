#' Create APA-formatted table of model results
#'
#' @description
#' Generates an APA-formatted HTML table of model estimates that opens in the RStudio Viewer.
#' Provides publication-ready tables with proper formatting for Bayesian model results.
#'
#' @param object A [`blimp_obj`] containing model results.
#' @param selector Optional character string specifying variable name or block to display.
#'   If missing, creates separate tables for each variable.
#' @param digits Integer specifying number of decimal places. Default is 3.
#' @param caption Optional character string for table caption.
#' @param show_chains Logical indicating whether to show chain information. Default is TRUE.
#' @param show_neff Logical indicating whether to show effective sample size. Default is FALSE.
#' 
#' @details
#' The function creates APA-style tables with the following features:
#' - Proper coefficient formatting with confidence intervals
#' - Organized sections for different parameter types
#' - Publication-ready styling
#' - Responsive design for different screen sizes
#' 
#' Tables are displayed in the RStudio Viewer pane and can be exported or copied.
#'
#' @return 
#' Invisibly returns the HTML content. Primary purpose is displaying in Viewer.
#'
#' @examples
#' \dontrun{
#' # Create APA table for all variables
#' model_table(model)
#' 
#' # Create table for specific variable
#' model_table(model, "y1")
#' 
#' # Customize formatting
#' model_table(model, digits = 2, caption = "Bayesian Model Results")
#' }
#' 
#' @export
model_table <- function(object, selector, digits = 3, caption = NULL, 
                       show_chains = TRUE, show_neff = FALSE) {
    
    # Input validation
    if (!inherits(object, 'blimp_obj')) {
        throw_error("Argument {.arg object} must be a {.cls blimp_obj}.")
    }
    
    if (!is.numeric(digits) || length(digits) != 1 || digits < 0) {
        throw_error("Argument {.arg digits} must be a non-negative integer.")
    }
    
    # Get model information
    niter <- nrow(object@iterations)
    nchain <- length(object@burn)
    
    # Start HTML document
    html <- paste0(
        "<!DOCTYPE html>\n",
        "<html>\n<head>\n",
        "<title>Model Results</title>\n",
        "<style>\n",
        get_apa_css(),
        "\n</style>\n</head>\n<body>\n"
    )
    
    # Add main title
    html <- paste0(html, "<div class='container'>\n")
    html <- paste0(html, "<h1>Model Results</h1>\n")
    
    
    if (missing(selector)) {
        # Create tables for all variables
        html <- paste0(html, create_all_variables_tables(object, digits, show_neff))
    } else {
        # Create table for specific selector
        html <- paste0(html, create_selector_table(object, selector, digits, show_neff))
    }
    
    # Add custom caption if provided
    if (!is.null(caption)) {
        html <- paste0(html, "<p class='caption'>", caption, "</p>\n")
    }
    
    # Add APA-style footnote with model information
    if (show_chains) {
        html <- paste0(html, "<p class='footnote'><em>Note.</em> ")
        html <- paste0(html, "Model fitted with ", format(niter, big.mark = ","), 
                      " iterations using ", nchain, " chains.")
        if (any(colnames(object@estimates) == "Estimate")) {
            html <- paste0(html, " Estimates based on posterior median.")
        }
        html <- paste0(html, "</p>\n")
    }
    
    # Close HTML
    html <- paste0(html, "</div>\n</body>\n</html>")
    
    # Write to temporary file and open in viewer
    temp_file <- tempfile(fileext = ".html")
    writeLines(html, temp_file)
    
    # Try to open in RStudio viewer, fallback to browser
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        rstudioapi::viewer(temp_file)
    } else {
        utils::browseURL(temp_file)
    }
    
    # Return HTML invisibly
    invisible(html)
}

#' Generate APA-style CSS
#' @noRd
get_apa_css <- function() {
    "
    body {
        font-family: 'Times New Roman', Times, serif;
        font-size: 12pt;
        line-height: 1.6;
        margin: 0;
        padding: 20px;
        background-color: #ffffff;
    }
    
    .container {
        max-width: 1000px;
        margin: 0 auto;
    }
    
    h1 {
        font-size: 16pt;
        font-weight: bold;
        text-align: center;
        margin-bottom: 20px;
        border-bottom: none;
    }
    
    h2 {
        font-size: 14pt;
        font-weight: bold;
        margin-top: 25px;
        margin-bottom: 15px;
    }
    
    .footnote {
        font-size: 10pt;
        margin-top: 15px;
        text-align: left;
        line-height: 1.4;
    }
    
    .variable-section {
        margin-bottom: 30px;
        page-break-inside: avoid;
    }
    
    .apa-table {
        width: 100%;
        border-collapse: collapse;
        margin: 20px 0;
        font-size: 11pt;
    }
    
    .apa-table th {
        border-top: 2px solid #000;
        border-bottom: 1px solid #000;
        padding: 8px 12px;
        text-align: center;
        font-weight: bold;
        background-color: #fff;
    }
    
    .apa-table td {
        padding: 6px 12px;
        text-align: right;
        border: none;
    }
    
    .apa-table .param-name {
        text-align: left;
        font-style: italic;
    }
    
    .apa-table .section-header {
        text-align: left;
        font-weight: bold;
        font-style: normal;
        background-color: #f8f8f8;
        border-top: 1px solid #ccc;
    }
    
    .apa-table tbody tr:last-child td {
        border-bottom: 2px solid #000;
    }
    
    .caption {
        font-style: italic;
        text-align: center;
        margin-top: 10px;
        font-size: 11pt;
    }
    
    .ci-format {
        white-space: nowrap;
    }
    
    @media print {
        .variable-section {
            break-inside: avoid;
        }
    }
    "
}

#' Create tables for all variables
#' @noRd
create_all_variables_tables <- function(object, digits, show_neff) {
    # Get all unique variables (exclude blocks)
    oname <- attr(object@iterations, "outcome_name")
    block_info <- attr(object@iterations, "block")
    
    all_variables <- unique(tolower(oname))
    if (!is.null(block_info)) {
        unique_variables <- all_variables[!all_variables %in% tolower(block_info)]
    } else {
        unique_variables <- all_variables
    }
    
    html <- ""
    
    for (i in seq_along(unique_variables)) {
        var_name <- unique_variables[i]
        html <- paste0(html, "<div class='variable-section'>\n")
        header_title <- if (grepl(" ", var_name)) "Covariance Matrix: " else "Outcome Variable: "
        html <- paste0(html, "<h2>", header_title, toupper(var_name), "</h2>\n")
        html <- paste0(html, create_variable_table(object, var_name, digits, show_neff))
        html <- paste0(html, "</div>\n")
    }
    
    return(html)
}

#' Create table for specific selector
#' @noRd
create_selector_table <- function(object, selector, digits, show_neff) {
    html <- "<div class='variable-section'>\n"
    header_title <- if (grepl(" ", selector)) "Covariance Matrix: " else "Outcome Variable: "
    html <- paste0(html, "<h2>", header_title, toupper(selector), "</h2>\n")
    html <- paste0(html, create_variable_table(object, selector, digits, show_neff))
    html <- paste0(html, "</div>\n")
    return(html)
}

#' Create table for single variable
#' @noRd  
create_variable_table <- function(object, variable, digits, show_neff) {
    # Get estimates for this variable
    oname <- attr(object@iterations, "outcome_name")
    if (is.null(oname)) {
        return("<p>No outcome information available.</p>")
    }
    
    oname <- tolower(oname)
    ptype <- attr(object@iterations, "parameter_type")
    
    sel <- which(oname == tolower(variable))
    
    if (length(sel) == 0) {
        return(paste0("<p>Variable '", variable, "' not found.</p>"))
    }
    
    est <- object@estimates[sel, , drop = FALSE]
    
    # Clean up row names using the same logic as summary()
    clean_names <- rownames(est)
    clean_names <- gsub(paste0(variable, ' '), '   ', clean_names)
    clean_names <- gsub(' ~', '', clean_names)
    clean_names <- gsub(' R2:', '', clean_names)
    clean_names <- gsub('\\(standardized\\)', '', clean_names)
    clean_names <- gsub('residual variance', 'Residual Var.', clean_names)
    clean_names <- gsub('residual SD', 'Residual SD', clean_names)
    
    # Remove leading spaces for table display
    clean_names <- trimws(clean_names)
    
    # Determine which columns to include
    col_names <- colnames(est)
    include_se <- "StdDev" %in% col_names || "SD" %in% col_names
    include_ci <- "2.5%" %in% col_names && "97.5%" %in% col_names
    include_pvalue <- "PValue" %in% col_names
    include_test_stat <- "ChiSq" %in% col_names
    include_neff <- show_neff && "N_Eff" %in% col_names
    
    # Calculate total column count
    total_cols <- 2  # Parameter + Estimate
    if (include_se) total_cols <- total_cols + 1
    if (include_ci) total_cols <- total_cols + 1
    if (include_test_stat) total_cols <- total_cols + 1
    if (include_pvalue) total_cols <- total_cols + 1
    if (include_neff) total_cols <- total_cols + 1
    
    # Start table
    html <- "<table class='apa-table'>\n"
    
    # Table header
    html <- paste0(html, "<thead>\n<tr>\n")
    html <- paste0(html, "<th class='param-name'>Parameter</th>\n")
    html <- paste0(html, "<th>Estimate</th>\n")
    
    if (include_se) {
        html <- paste0(html, "<th><em>SE</em></th>\n")
    }
    if (include_ci) {
        html <- paste0(html, "<th>95% CI</th>\n")
    }
    if (include_test_stat) {
        html <- paste0(html, "<th>&chi;&sup2;</th>\n")
    }
    if (include_pvalue) {
        html <- paste0(html, "<th><em>p</em></th>\n")
    }
    if (include_neff) {
        html <- paste0(html, "<th><em>N</em><sub>eff</sub></th>\n")
    }
    
    html <- paste0(html, "</tr>\n</thead>\n<tbody>\n")
    
    # Group by parameter type
    for (param_level in levels(ptype[sel])) {
        param_indices <- which(ptype[sel] == param_level)
        
        if (length(param_indices) > 0) {
            # Add section header
            html <- paste0(html, "<tr><td class='section-header' colspan='",
                          total_cols, "'>", param_level, "</td></tr>\n")
            
            # Add parameters in this section
            for (j in param_indices) {
                html <- paste0(html, "<tr>\n")
                html <- paste0(html, "<td class='param-name'>", clean_names[j], "</td>\n")
                
                # Estimate
                if ("Estimate" %in% col_names) {
                    estimate_val <- est[j, "Estimate"]
                    if (!is.null(estimate_val) && !is.na(estimate_val) && !is.nan(estimate_val)) {
                        html <- paste0(html, "<td>", format(round(estimate_val, digits), nsmall = digits), "</td>\n")
                    } else {
                        html <- paste0(html, "<td></td>\n")
                    }
                } else {
                    html <- paste0(html, "<td></td>\n")
                }
                
                # Standard Error
                if (include_se) {
                    se_col <- if ("StdDev" %in% col_names) "StdDev" else if ("SD" %in% col_names) "SD" else NULL
                    if (!is.null(se_col)) {
                        se_val <- est[j, se_col]
                        if (!is.null(se_val) && !is.na(se_val) && !is.nan(se_val)) {
                            html <- paste0(html, "<td>", format(round(se_val, digits), nsmall = digits), "</td>\n")
                        } else {
                            html <- paste0(html, "<td></td>\n")
                        }
                    } else {
                        html <- paste0(html, "<td></td>\n")
                    }
                }
                
                # Confidence Interval
                if (include_ci) {
                    if ("2.5%" %in% col_names && "97.5%" %in% col_names) {
                        ci_low <- est[j, "2.5%"]
                        ci_high <- est[j, "97.5%"]
                        if (!is.null(ci_low) && !is.na(ci_low) && !is.nan(ci_low) && !is.null(ci_high) && !is.na(ci_high) && !is.nan(ci_high)) {
                            ci_text <- paste0("[", format(round(ci_low, digits), nsmall = digits), 
                                             ", ", format(round(ci_high, digits), nsmall = digits), "]")
                            html <- paste0(html, "<td class='ci-format'>", ci_text, "</td>\n")
                        } else {
                            html <- paste0(html, "<td></td>\n")
                        }
                    } else {
                        html <- paste0(html, "<td></td>\n")
                    }
                }
                
                # Test statistic
                if (include_test_stat) {
                    if ("ChiSq" %in% col_names) {
                        test_val <- est[j, "ChiSq"]
                        if (!is.null(test_val) && !is.na(test_val) && !is.nan(test_val)) {
                            html <- paste0(html, "<td>", format(round(test_val, 2), nsmall = 2), "</td>\n")
                        } else {
                            html <- paste0(html, "<td></td>\n")
                        }
                    } else {
                        html <- paste0(html, "<td></td>\n")
                    }
                }
                
                # P-value
                if (include_pvalue) {
                    if ("PValue" %in% col_names) {
                        p_val <- est[j, "PValue"]
                        if (!is.null(p_val) && !is.na(p_val) && !is.nan(p_val)) {
                            if (p_val < 0.001) {
                                html <- paste0(html, "<td>&lt;.001</td>\n")
                            } else {
                                html <- paste0(html, "<td>", format(round(p_val, 3), nsmall = 3), "</td>\n")
                            }
                        } else {
                            html <- paste0(html, "<td></td>\n")
                        }
                    } else {
                        html <- paste0(html, "<td></td>\n")
                    }
                }
                
                # Effective sample size
                if (include_neff) {
                    if ("N_Eff" %in% col_names) {
                        neff_val <- est[j, "N_Eff"]
                        if (!is.null(neff_val) && !is.na(neff_val) && !is.nan(neff_val)) {
                            html <- paste0(html, "<td>", format(round(neff_val, 0), big.mark = ","), "</td>\n")
                        } else {
                            html <- paste0(html, "<td></td>\n")
                        }
                    } else {
                        html <- paste0(html, "<td></td>\n")
                    }
                }
                
                html <- paste0(html, "</tr>\n")
            }
        }
    }
    
    html <- paste0(html, "</tbody>\n</table>\n")
    return(html)
}