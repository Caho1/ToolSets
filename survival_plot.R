# survival_plot.R

# Load libraries
library(survival)
library(survminer) # For ggsurvplot
library(readr)
library(dplyr)
library(optparse) # Use optparse for robust argument handling
library(ggpubr)

# --- Argument Parsing ---
option_list <- list(
  make_option(c("-i", "--input"), type="character", default=NULL, help="Path to input CSV file", metavar="character"),
  make_option(c("-o", "--output"), type="character", default=NULL, help="Path to output PDF file", metavar="character"),
  make_option(c("--show_ci"), type="logical", default=TRUE, help="Show confidence intervals? [default %default]", metavar="logical"),
  make_option(c("--show_risk_table"), type="logical", default=FALSE, help="Show risk table? [default %default]", metavar="logical")
)

opt_parser <- OptionParser(option_list=option_list, description = "Generate Kaplan-Meier survival plot.")
tryCatch({
    opt <- parse_args(opt_parser)
}, error = function(e) {
     message("Error parsing arguments: ", e$message)
     print_help(opt_parser)
     quit(status = 2)
})


if (is.null(opt$input) || is.null(opt$output)){
  message("Error: --input and --output arguments are required.")
  print_help(opt_parser)
  quit(status=2)
}

message("Input CSV: ", opt$input)
message("Output PDF: ", opt$output)
message("Show CI: ", opt$show_ci)
message("Show Risk Table: ", opt$show_risk_table)

# --- Read Data ---
tryCatch({
    input_data <- read_csv(opt$input, col_types = cols(
        Time = col_double(),
        Status = col_integer(),
        Group = col_character() # Read group as character
    )) %>%
    mutate(
        # Ensure empty strings in Group become NA or a specific level if needed
        Group = if_else(is.na(Group) | Group == "", "Overall", Group)
    )

    if (nrow(input_data) < 3) stop("Input data must have at least 3 rows.")
    if (!all(c("Time", "Status") %in% colnames(input_data))) stop("Input CSV needs 'Time' and 'Status' columns.")
    if (!is.numeric(input_data$Time) || any(input_data$Time < 0)) stop("Column 'Time' must be non-negative numeric.")
    if (!is.integer(input_data$Status) || !all(input_data$Status %in% c(0, 1))) stop("Column 'Status' must be integer 0 or 1.")

    # Convert Group to factor
    input_data$Group <- as.factor(input_data$Group)
    num_groups <- nlevels(input_data$Group)
    message("Number of groups detected: ", num_groups)
    if (num_groups > 5) warning("More than 5 groups found, plot might become cluttered.")


}, error = function(e) {
    message("Error reading/validating CSV '", opt$input, "': ", e$message); quit(status = 3)
})


# --- Create Survival Object and Fit Model ---
tryCatch({
    # Create Surv object
    surv_obj <- Surv(time = input_data$Time, event = input_data$Status)

    # Fit Kaplan-Meier curves - handle single vs multiple groups
    if (num_groups <= 1) {
        # Fit overall survival if only one group (or no group column)
        fit <- survfit(surv_obj ~ 1, data = input_data)
        formula_str <- "Surv(Time, Status) ~ 1"
        # Log-rank p-value is not applicable for a single group
        show_pval <- FALSE
    } else {
        # Fit by group
        fit <- survfit(Surv(Time, Status) ~ Group, data = input_data)
        formula_str <- "Surv(Time, Status) ~ Group"
        # Show p-value for multiple groups
        show_pval <- TRUE
    }
     message("Fitted survival model using formula: ", formula_str)

}, error = function(e) {
     message("Error creating Surv object or fitting model: ", e$message); quit(status = 4)
})


# --- Generate Plot using ggsurvplot ---
tryCatch({
    # Note: ggsurvplot returns a list containing the plot and table (if requested)
    # We need to print this list to the PDF device.

    # Customize ggsurvplot arguments
    ggsp <- ggsurvplot(
                fit,
                data = input_data,
                conf.int = opt$show_ci,         # Use argument
                risk.table = opt$show_risk_table,  # Use argument
                pval = show_pval,              # Show p-value only if multiple groups
                pval.method = show_pval,       # Show log-rank test method text if pval is shown
                legend.title = "Group",        # Title for legend
                legend.labs = levels(input_data$Group), # Use factor levels for labels
                xlab = "Time",                 # X-axis label
                ylab = "Survival Probability", # Y-axis label
                title = "Kaplan-Meier Survival Curve",
                ggtheme = theme_minimal(base_size = 35),     # Use a clean theme
                risk.table.y.text = FALSE,     # Don't show y-axis text on risk table
                risk.table.height = if(opt$show_risk_table) 0.25 else 0 # Adjust height if shown
             )

    message("Generated ggsurvplot object.")

}, error = function(e) {
    message("Error during ggsurvplot generation: ", e$message); quit(status = 5)
})

# --- Save Plot to PDF ---
tryCatch({
    # 定义PDF设备和尺寸
    pdf(file = opt$output, width = 20, height = 20) # 可能需要调整高度

    # 检查 ggsp 是否包含 plot 和 table (只有当 risk.table=TRUE 时才有 table)
    if (opt$show_risk_table && !is.null(ggsp$table)) {
        # 使用 ggarrange 排列主图和风险表 (垂直排列)
        # 可以调整 heights 参数来控制主图和风险表的相对高度
        combined_plot <- ggarrange(ggsp$plot, ggsp$table, heights = c(2.5, 1), # 例如主图占 2.5 份，风险表占 1 份
                                  ncol = 1, nrow = 2, align = "v")
        print(combined_plot) # 打印组合后的图
        message("Arranged plot and risk table using ggarrange.")
    } else {
        # 如果没有风险表，只打印主图
        print(ggsp$plot) # 或者直接 print(ggsp) 也可以，因为它会打印主图
        message("Printing main plot only.")
    }

    dev.off() # 关闭PDF设备
    message("Successfully saved plot to PDF: ", opt$output)

}, error = function(e) {
    message("Error saving plot to PDF '", opt$output, "': ", e$message); quit(status = 6)
})

quit(status = 0) # Success