##################################
#                                #
#        CROSS-VALIDATION        #
#                                #
##################################


# L. Geers, December 2024

# Locates and statistically tests effects in time-series data. It does so by using crossvalidation to identify time points to test, 
# and then using a linear mixed effects model to actually perform the statistical test. 
# More specifically, the data is subdivided in 4 subsets. 
# It takes one of the subsets (the test set) out of the full dataset, and conducts a linear mixed effects model on each sample 
# of the remaining data (the training set). The sample with the highest absolute z value in the training set is used
# as the sample-to-be-tested for the test set. This procedure is repeated for all subsets of the data, and for all fixed effects in the model.
# Finally, a single linear mixed effects model is conducted for each fixed effects on the samples that were thus identified.
# See Mathôt & Vilotijević (2022) for more details

# Is based on a dataset where each row is a trial and where
# one column indicates the subject's number (called "subj")
# one column indicates the trial's number (called "trial")
# one column indicates the fixed effects level (name must be provided in "Parameters" below)
# one column containing the dependent variable value per sample in the data (name of the dv and number of samples mus be provided)
 
# !! Do not forget to fill in the parameters below


# PACKAGES
# --------

# 1. List of the packages that will be needed
packages= c("lme4", "car", "tidyr", "ggplot2", "dplyr", "emmeans","lmerTest")

# 2. Function to install packages
install_if_not_installed <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
  }
}

# 3. Install  and load packages if required
lapply(packages, install_if_not_installed)
lapply(packages, require, character.only= T)



# CLEANING WORKING SPACE
# ----------------------

rm(list=ls())


# PARAMETERS (change when needed)
# ----------

filename <- ".txt" #your file's name
nframes <- 300 #number of samples in your eyetracking data
fframe_to_be_tested <- 100 # first sample to test
lframe_to_be_tested <- 300 # last sample to test
fixed_effect <- "..." # name of your independent variable
    level_max <- "..." # name of the level you expect to be the highest
    level_min <- "..." # name of the level you expect to be the lowest
dependent_variable <- "..." #name of your dependent variable 
donw_sample_freq <- 10 #downsampling thas has been applied to raw data
corr_method <- "bonferroni" #correction method for the posthoc


# DATA IMPORTATION
# -----------------

# Get the directory name of the current script
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the script directory
setwd(script_dir)

# Import data
data <- read.table(filename, header = TRUE, sep = "\t") #change file name (that should be in the same directory as this script)



# DATA CLEANING
# --------------

# Exclude data to be excluded
data <- data[data$isIncluded == 1,]

# Define variables/columns to keep
dv_vars <- paste0(dependent_variable, 1:nframes)
keep <- c('subject', fixed_effect, dv_vars)

# Define fixed effect as categorical
data[[fixed_effect]] <- as.factor(data[[fixed_effect]])
data[[fixed_effect]] <- factor(data[[fixed_effect]], levels = c(level_max, level_min))


# Rename categorical levels
levels(data[[fixed_effect]])[levels((data[[fixed_effect]])) == level_max] <- 2
levels(data[[fixed_effect]])[levels((data[[fixed_effect]])) == level_min] <- 1


# Trim
data <- data[,keep]



# INTERLEAVED SPLITTING
# ---------------------
data$sample1 <- rep(c(0,1,1,1), length.out = nrow(data))
data$sample2 <- rep(c(1,0,1,1), length.out = nrow(data))
data$sample3 <- rep(c(1,1,0,1), length.out = nrow(data))
data$sample4 <- rep(c(1,1,1,0), length.out = nrow(data))


# LOCALIZER LMER
# --------------

# Create empty variables to be filled by the following loop
data$sample_to_be_tested <- NA



for (s in 1:4){
  
  # Create empty variables to be filled by the following loop
  t_values <- numeric()
  data_sample <- data.frame()
  
  # Reconstitute  the name of each split 
  sample <- paste0("sample", s) 
  
  # Keeps only the training trial in that split sample 
  data_sample <- subset(data, data[, sample] == 1) 

  # For each sample
  for (f in fframe_to_be_tested:lframe_to_be_tested) {
    
    # Define the name of the column where the data of that sample are stored
    current_dependent_variable <- paste0(dependent_variable,  f)
    
    # Write the formula 
    model_loc_formula <- as.formula(paste(current_dependent_variable, "~", fixed_effect, "+ (1|subject)"))
    
    # Compute the linear model
    my_loc_lmer <- lmer(model_loc_formula, data = data_sample, control = lmerControl(optCtrl = list(maxfun = 1000)))
    
    # Store the output and in particular the t values associated to the fixed effect
    summary_output <- summary(my_loc_lmer)
    t_values <- c(t_values, summary_output$coefficients[2, "t value"])

  }

  # Identify the test trials
  index <- which(data[,sample] == 0) 
  
  # Associate the sample-to-be-tested (sample where t-value is maximal)
  data[index, "sample_to_be_tested"]  <- (which.max(t_values)) + (fframe_to_be_tested-1)

}


# TEST LMER
# ---------

# Create an empty variable to be filled by the loop
data$new_dependent_variable <- NA


for (l in 1:nrow(data)){
  
  # Retrieve the column name of the sample to-be-tested for that trial
  col_name <- paste0(dependent_variable, data$sample_to_be_tested[l])
  col_num <- which(names(data) == col_name)
  
  # Stores data from that column in a new variable
  data$new_dependent_variable[l] <- data[l, col_num]
  

}

# Open a file connection for writing
file <- file("output_glmer_test.txt", "w")

# Redirect the output to the file
sink(file)

# Computes the model
model_test_formula <- as.formula(paste0("new_dependent_variable", " ~ ", fixed_effect, " + (1|subject)"))
my_test_lmer <- lmer(model_test_formula, data = data, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000), check.conv.grad = "warning"))

# Test the effect of condition
anova(my_test_lmer)

# Save sample-to-be-tested
data$sample_to_be_tested <- as.factor(data$sample_to_be_tested)
print("Samples tested are:")
print(as.numeric(levels(data$sample_to_be_tested))*donw_sample_freq)

# Close the file connection
sink()
close(file)



# PLOT
# ----

# Transform data in long format
data_long <- gather(data, key = "Sample", value = "Value", dv_vars[1]:dv_vars[length(dv_vars)])
data_long$Sample <- gsub(paste0(dependent_variable), "", data_long$Sample)
data_long$Sample <- as.numeric(data_long$Sample)*donw_sample_freq
data_long <- na.omit(data_long)

# Summarize across participants
data_long_summary_subj <- data_long  %>%
                                    group_by(subject, Sample, .data[[fixed_effect]])%>%
                                    summarise(Mean = mean(Value, na.rm = TRUE))

# Summarize across samples
data_long_summary_sample <- data_long_summary_subj  %>%
                                                    group_by(Sample, .data[[fixed_effect]])%>%
                                                    summarise(Mean = mean(Mean, na.rm = TRUE))



# Change variable format 
data_long_summary_sample[[fixed_effect]] <- as.factor(data_long_summary_sample[[fixed_effect]])

data$sample_to_be_tested <- as.factor(data$sample_to_be_tested)
sample_to_be_tested <- as.numeric(levels(data$sample_to_be_tested))*donw_sample_freq

# Create image where to draw the graph 
png("plot_with_sample_to_be_tested.png", width = 1920, height = 1080)

# Draw mean data
g <- ggplot(data_long_summary_sample, aes(x = Sample, y = Mean, color = data_long_summary_sample[[fixed_effect]]))+
        geom_line(size = 1)+
        xlab("Time (msec)")+
        ylab("Pupil Size")+
        labs(color = "Condition", color = "Congruent to Dark")+
        scale_color_brewer(palette = "Set1") +
        theme_classic()+
        theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
          text = element_text(size = 25)) 


# With samples to be tested
for (i in 1:length(sample_to_be_tested)) {
  g <- g + geom_vline(xintercept = sample_to_be_tested[i], linetype = "dashed")
}

# Display the plot
g

# Save the plot 
dev.off()

