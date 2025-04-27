# two_group_plot.R

# Load libraries
library(readr)
library(ggplot2)
library(dplyr) # Optional, used for potential summarise later

# --- Argument Parsing ---
args <- commandArgs(trailingOnly = TRUE)

input_csv_path <- NULL
output_pdf_path <- NULL
plot_type <- "boxplot" # Default plot type

i <- 1
while (i <= length(args)) {
  if (args[i] == "--input" && i + 1 <= length(args)) {
    input_csv_path <- args[i + 1]; i <- i + 2
  } else if (args[i] == "--output" && i + 1 <= length(args)) {
    output_pdf_path <- args[i + 1]; i <- i + 2
  } else if (args[i] == "--plottype" && i + 1 <= length(args)) { # Changed arg name
    plot_type <- args[i + 1]; i <- i + 2
  } else {
     message("Error: Unknown argument or missing value: ", args[i]); quit(status = 2)
  }
}

if (is.null(input_csv_path) || is.null(output_pdf_path)) {
    message("Usage: Rscript two_group_plot.R --input <in.csv> --output <out.pdf> --plottype <type>"); quit(status = 2)
}

valid_plot_types <- c("boxplot", "violin", "density")
if (!plot_type %in% valid_plot_types) {
    message("Error: Invalid plot type '", plot_type, "'. Valid types: ", paste(valid_plot_types, collapse=", ")); quit(status = 2)
}

message("Input CSV: ", input_csv_path)
message("Output PDF: ", output_pdf_path)
message("Plot Type: ", plot_type)

# --- Read Data ---
tryCatch({
    input_data <- read_csv(input_csv_path, col_types = cols(Group = col_character(), Value = col_double())) # Read Group as character initially
    if (nrow(input_data) < 4) stop("Input data must have at least 4 rows (>=2 per group).") # Basic check
    if (!all(c("Group", "Value") %in% colnames(input_data))) stop("Input CSV needs 'Group' and 'Value' columns.")
    if (!is.numeric(input_data$Value)) stop("Column 'Value' must be numeric.")

    # Convert Group to factor for plotting
    input_data$Group <- as.factor(input_data$Group)
    if (nlevels(input_data$Group) != 2) stop("Data must contain exactly two distinct groups.")

}, error = function(e) {
    message("Error reading/validating CSV '", input_csv_path, "': ", e$message); quit(status = 3)
})

# --- Generate Plot ---
tryCatch({
    plot_title <- paste("Comparison of Value between Groups")
    plot_subtitle <- paste("Plot Type:", plot_type)

    # Base plot - map Value to y, Group to x
    p <- ggplot(input_data, aes(x = Group, y = Value, fill = Group)) + # Use fill aesthetic
             labs(title = plot_title, subtitle = plot_subtitle, x = "Group", y = "Value") +
             theme_minimal(base_size = 45) +
             theme(text = element_text(size = 35),
                   plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5),
                   legend.position = "none") # Often hide legend when x-axis shows groups

    # Add specific geom based on plot_type
    if (plot_type == "boxplot") {
        p <- p + geom_boxplot(alpha = 0.7, outlier.shape = NA) + # alpha for transparency, hide outliers by default
                 geom_jitter(width = 0.1, alpha = 0.5) # Add jittered points
    } else if (plot_type == "violin") {
        p <- p + geom_violin(trim = FALSE, alpha = 0.7) + # trim=FALSE prevents trimming tails
                 geom_boxplot(width=0.1, fill="white", alpha = 0.5) # Overlay a narrow boxplot
                # geom_jitter(width = 0.1, alpha = 0.3) # Optional jitter on violin
    } else if (plot_type == "density") {
        # Density plot needs a different aesthetic mapping
        p <- ggplot(input_data, aes(x = Value, fill = Group, color = Group)) +
             geom_density(alpha = 0.5) + # Use transparency
             labs(title = plot_title, subtitle = plot_subtitle, x = "Value", y = "Density") +
             theme_minimal(base_size = 45) +
             theme(text = element_text(size = 35),
                   plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5),
                   legend.position = "top") # Show legend for density
    }
     message("Generated plot object using type: ", plot_type)

}, error = function(e) {
    message("Error generating plot: ", e$message); quit(status = 4)
})

# --- Save Plot ---
tryCatch({
    ggsave(filename = output_pdf_path, plot = p, device = "pdf", width = 25, height = 25, units = "in") # Adjust size if needed
    message("Successfully generated PDF plot: ", output_pdf_path)
}, error = function(e) {
    message("Error saving plot to PDF '", output_pdf_path, "': ", e$message); quit(status = 5)
})

quit(status = 0)