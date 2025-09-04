#' Internal error function
#' Wrapper for `cli_abort` to not specify the call
#' @importFrom cli cli_abort
#' @noRd
throw_error <- function(message, ..., .envir = parent.frame(), .frame = .envir) {
    cli::cli_abort(message, ..., .envir = .envir, .frame = .frame, call = NULL)
}


#' Internal function for checking strings are identical with case insensitive
#' @noRd
is_equal <- function(a, b) {
    tolower(a) == tolower(b)
}

#' Calculate maximum parameter name width across multiple variables for alignment
#' 
#' @description
#' Calculates the maximum character width of parameter names across multiple 
#' variables to ensure consistent alignment in multivariate model output.
#' Applies the same parameter name cleaning logic as the summary method.
#' 
#' @details
#' This function processes parameter names for multiple variables and finds the
#' maximum width after applying standard cleaning operations:
#' - Remove variable prefixes and replace with standard spacing
#' - Clean special characters and standardize terminology  
#' - Add extra spacing for correlation/covariance parameters to align with main variables
#' 
#' Used internally to coordinate alignment between main variables and their
#' associated correlation models in multivariate output.
#'
#' @param object A blimp_obj containing model results
#' @param variables Character vector of variable names to process
#' @return Integer representing the maximum parameter name width in characters
#' @noRd
calculate_max_par_width <- function(object, variables) {
    # Extract outcome name information
    oname <- attr(object@iterations, "outcome_name")
    if (is.null(oname)) return(0)
    
    oname <- tolower(oname)
    max_width <- 0
    
    # Process each variable to find maximum parameter name width
    for (variable in variables) {
        # Find parameters for this variable
        sel <- which(oname == tolower(variable))
        if (length(sel) > 0) {
            est <- object@estimates[sel, , drop = FALSE]
            
            # Apply identical cleaning logic as summary method
            clean_names <- rownames(est)
            clean_names <- gsub(paste0(variable, ' '), '   ', clean_names)
            clean_names <- gsub(' ~', '', clean_names)
            clean_names <- gsub(' R2:', '', clean_names)
            clean_names <- gsub('\\(standardized\\)', '', clean_names)
            clean_names <- gsub('residual variance', 'Residual Var.', clean_names)
            clean_names <- gsub('residual SD', 'Residual SD', clean_names)
            
            # Add extra prefix spacing for correlation/covariance models
            # This ensures alignment with main variable parameters like "   Intercept"
            if (any(grepl("^(Cov|Cor)\\(", clean_names))) {
                clean_names <- paste0("   ", clean_names)
            }
            
            # Update maximum width across all variables
            var_max_width <- max(nchar(clean_names))
            max_width <- max(max_width, var_max_width)
        }
    }
    
    return(max_width)
}

