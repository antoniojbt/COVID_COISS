############
# COISS paper 1
# L. Bonifaz
# May 2024
############


############
# TO DO:
  # Run DiD as primary analysis; followed by eg Cox or MLM
  # Check assumptions: independence, expected frequencies, sample size
  # plot/check residuals, R2, etc
  # Easy to digest presentation
  # Competing Risks Analysis?

# Done:
  # needs proper selection of covariates
  # check problems with missing data, exclude vars based on this
  # Create a table of counts, proportions, etc. for simple overview
  # Save tables, see tendency
  # change follow-up to 30 days, replot 4a script with this
############


############
project_loc <- '/Users/antoniob/Documents/work/science/projects/projects/ongoing/laura_bonifaz/COVID_COISS/p1_2021_intervention/results/'
getwd()
setwd(project_loc)
############


############
library(episcout)
library(ggplot2)
############


############
file_name <- 'COVID19MEXICO2021_2022_COVID-only_COISS-only/1_general_desc/counts_by_dates_outcome_.txt'

counts_d <- epi_read(file_name)
counts_d
counts_d$perc <- (counts_d$Freq / sum(counts_d$Freq)) * 100
counts_d

str(counts_d)
counts_d$Var1 <- factor(counts_d$Var1)
counts_d$Var2 <- factor(counts_d$Var2)
counts_d$outcome <- factor(counts_d$outcome)
colnames(counts_d)[1:2] <- c('intervention', 'analysis')
colnames(counts_d)
############


############
# Add vars needed for plotting:
counts_d$time <- c('01-01-2021', # pre-T0; start of data collection for database 2021
                      '01-01-2021',
                      '01-01-2021',
                      '01-01-2021',
                      '30-07-2021', # T0; start of
                      '30-07-2021',
                      '30-07-2021',
                      '30-07-2021',
                      '03-08-2021', # gap T0 T1; start of; one day post T0
                      '03-08-2021',
                      '03-08-2021',
                      '03-08-2021',
                      '03-09-2021', # T1; start of
                      '03-09-2021',
                      '03-09-2021',
                      '03-09-2021',
                      '09-10-2021', # gap T1 T2; start of; one day post T1
                      '09-10-2021',
                      '09-10-2021',
                      '09-10-2021',
                      '19-12-2021', # T2; start of
                      '19-12-2021',
                      '19-12-2021',
                      '19-12-2021',
                      '28-02-2022', # post-T2; start of; one day post T2
                      '28-02-2022',
                      '28-02-2022',
                      '28-02-2022'
                      )
counts_d$time <- as.Date(counts_d$time, format = "%d-%m-%Y")
counts_d$order <- 1:nrow(counts_d)

# Add group variable that combines intervention and outcome:
counts_d$tx_outcome <- paste(counts_d$intervention, counts_d$outcome, sep = "_")
counts_d$tx_outcome <- factor(counts_d$tx_outcome)

str(counts_d)
summary(counts_d)
############


############
# Plot all:
df <- counts_d
colnames(df)

p1 <- ggplot(df, aes(x = time)) +
  geom_line(aes(y = Freq, colour = "Freq"), linewidth = 1) +  # Line for Frequency
  geom_line(aes(y = perc * max(Freq) / max(perc), colour = "perc"), linewidth = 1, linetype = "dashed") +  # Line for Percentage, scaled
  scale_y_continuous(
    name = "Frequency",
    sec.axis = sec_axis(~ . * max(df$perc) / max(df$Freq), name = "Percentage")
  ) +
  labs(x = "Time", y = "Frequency / Percentage", title = "Change Over Time", colour = "Metric") +
  theme_minimal()

p1

# Plot by group:
# Change order and time to displace lines, easier to visualise but not accurate, lines are superimposed.
p2 <- ggplot(df, aes(x = order, group = intervention)) +
  geom_line(aes(y = Freq, colour = intervention), linewidth = 1) +  # Line for Frequency by Var1
  geom_line(aes(y = perc * max(df$Freq) / max(df$perc), colour = intervention), linewidth = 1, linetype = "dashed") +  # Line for Percentage, scaled, by Var1
  scale_y_continuous(
    name = "Frequency",
    sec.axis = sec_axis(~ . * max(df$perc) / max(df$Freq), name = "Percentage")
  ) +
  labs(
    x = "Time",
    y = "Frequency / Percentage",
    title = "Change Over Time by Intervention",
    colour = "Group"
  ) +
  theme_minimal()

p2


# Plot by group:
p3 <- ggplot(df, aes(x = time, group = tx_outcome)) +
  geom_line(aes(y = Freq, colour = tx_outcome), linewidth = 1) +  # Line for Frequency by Var1
  geom_line(aes(y = perc * max(df$Freq) / max(df$perc), colour = tx_outcome), linewidth = 1, linetype = "dashed") +  # Line for Percentage, scaled, by Var1
  scale_y_continuous(
    name = "Frequency",
    sec.axis = sec_axis(~ . * max(df$perc) / max(df$Freq), name = "Percentage")
  ) +
  labs(
    x = "Time",
    y = "Frequency / Percentage",
    title = "Change Over Time by Intervention and Outcome",
    colour = "Group"
  ) +
  theme_minimal()

p3


# # Line plot with dual grouping (exactly the same as above):
# p4 <- ggplot(df, aes(x = time, group = interaction(intervention, outcome))) +
#   geom_line(aes(y = Freq, colour = interaction(intervention, outcome)), linewidth = 1) +  # Line for Frequency by Var1 and outcome
#   geom_line(aes(y = perc * max(df$Freq) / max(df$perc), colour = interaction(intervention, outcome)), linewidth = 1, linetype = "dashed") +  # Line for Percentage, scaled, by Var1 and outcome
#   scale_y_continuous(
#     name = "Frequency",
#     sec.axis = sec_axis(~ . * max(df$perc) / max(df$Freq), name = "Percentage")
#   ) +
#   labs(
#     x = "Time",
#     y = "Frequency / Percentage",
#     title = "Change Over Time by Intervention and Outcome",
#     colour = "Group"
#   ) +
#   theme_minimal()
#
# p4



# Assuming 'df' has a Date column and you're plotting 'Freq'
p4 <- ggplot(df, aes(x = time, y = Freq, group = interaction(intervention, outcome), colour = interaction(intervention, outcome))) +
  geom_line(alpha = 0.3) +  # Make original lines semi-transparent
  geom_smooth(se = FALSE, method = "loess", span = 0.2) +  # Add smoothed lines
  labs(title = "Trends Over Time", x = "Date", y = "Frequency") +
  theme_minimal()

p4

############
