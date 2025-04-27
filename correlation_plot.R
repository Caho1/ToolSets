# plot_script.R (使用 commandArgs, 增加 method 参数)

# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr) # Optional

# --- Argument Parsing using commandArgs ---
args <- commandArgs(trailingOnly = TRUE)

# --- Argument Parsing ---
# Expecting: --input <in> --output <out> --method <method>
# Total 6 arguments
input_csv_path <- NULL
output_pdf_path <- NULL
plot_method <- "lm" # Default method if not specified

# Simple loop to parse named arguments
i <- 1
while (i <= length(args)) {
  if (args[i] == "--input" && i + 1 <= length(args)) {
    input_csv_path <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--output" && i + 1 <= length(args)) {
    output_pdf_path <- args[i + 1]
    i <- i + 2
  } else if (args[i] == "--method" && i + 1 <= length(args)) {
    plot_method <- args[i + 1]
    i <- i + 2
  } else {
    # Unknown argument or missing value
     message("Error: Unknown argument or missing value for argument: ", args[i])
     quit(status = 2)
  }
}

# Basic validation of paths
if (is.null(input_csv_path) || is.null(output_pdf_path)) {
    message("Usage: Rscript plot_script.R --input <input_csv_path> --output <output_pdf_path> --method <plot_method>")
    message("Error: --input and --output arguments are required.")
    quit(status = 2)
}

# Validate plot method
valid_methods <- c("none", "lm", "glm", "gam", "loess")
if (!plot_method %in% valid_methods) {
    message("Error: Invalid plot method '", plot_method, "'. Valid methods are: ", paste(valid_methods, collapse=", "))
    # Defaulting to 'none' or 'lm' might be an option, but erroring is safer
    quit(status = 2)
}


message("Input CSV Path: ", input_csv_path)
message("Output PDF Path: ", output_pdf_path)
message("Plot Method: ", plot_method)


# --- Read Data ---
tryCatch({
    input_data <- read_csv(input_csv_path, col_types = cols(X = col_double(), Y = col_double()))
    if (nrow(input_data) < 2) stop("Input data must have at least 2 rows.")
    if (!all(c("X", "Y") %in% colnames(input_data))) stop("Input CSV must contain columns named 'X' and 'Y'.")
    if (!is.numeric(input_data$X) || !is.numeric(input_data$Y)) stop("Columns X and Y must contain numeric data.")
}, error = function(e) {
    message("Error reading or validating input CSV '", input_csv_path, "': ", e$message)
    quit(status = 3)
})


# --- Generate Plot ---
tryCatch({
    plot_title <- "Scatter Plot of X vs Y"
    # Base plot
    p <- ggplot(input_data, aes(x = X, y = Y)) +
        geom_point(alpha = 0.7, color = "steelblue") +
        labs(
            title = plot_title,
            subtitle = paste("Fitting Method:", plot_method), # Add method to subtitle
            x = "Variable X",
            y = "Variable Y"
        ) +
        theme_minimal(base_size =45) +
        theme(text = element_text(size = 40),
              plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))

    # Add smoothing layer based on method, skip if 'none'
    if (plot_method != "none") {
         # Ensure necessary package for gam is loaded if method is gam
         if (plot_method == "gam") {
             if (!requireNamespace("mgcv", quietly = TRUE)) {
                 stop("Package 'mgcv' needed for method 'gam'. Please install it.", call. = FALSE)
             }
             # Explicitly load if needed, or rely on ggplot2 finding it
             # library(mgcv)
         }

         # Add the smoothing layer
         p <- p + geom_smooth(method = plot_method, se = TRUE, color = "firebrick", fill = "lightcoral", alpha=0.1)
         message("Added geom_smooth layer with method: ", plot_method)
    } else {
         message("Skipping geom_smooth layer (method is 'none').")
    }


}, error = function(e) {
    message("Error generating plot: ", e$message)
    quit(status = 4)
})

# --- Save Plot to PDF ---
tryCatch({
    ggsave(filename = output_pdf_path, plot = p, device = "pdf", width = 23, height = 23, units = "in")
    message("Successfully generated PDF plot: ", output_pdf_path)
}, error = function(e) {
    message("Error saving plot to PDF '", output_pdf_path, "': ", e$message)
    quit(status = 5)
})

# Exit successfully
quit(status = 0)