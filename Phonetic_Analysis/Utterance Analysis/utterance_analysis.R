# Libraries
library(magrittr)
library(tidyverse) 
library(lme4)
library(lmerTest)
library(sjPlot)
library(ggpubr)
library(influence.ME)
library(ordinal)
library(ggstance)
library(ggstatsplot)
library(ggthemes)
library(forcats)
library(sjstats)
library(extrafont)
extrafont::font_import()

df = read.csv("df.csv", sep = ",")
#df %<>% select(-X)


# Function to filter outliers based on 2.75* IQR
filter_outliers <- function(column) {
  iqr_value <- IQR(column)
  lower_bound <- quantile(column, 0.25) - 2.75 * iqr_value
  upper_bound <- quantile(column, 0.75) + 2.75 * iqr_value
  return(column >= lower_bound & column <= upper_bound)
}

df$Duration <- df$Event_Duration
df$Average_Space <- (df$Right_S_to_W_Distance_Change + df$Left_S_to_W_Distance_Change) / 2
df$Average_Amplitude <- (df$Right_Hand_Movement_Amplitude + df$Left_Hand_Movement_Amplitude) / 2
df$Average_Distance_Normalized <- df$Average_Amplitude / df$Duration
df$Average_Space_Normalized <- ((df$Left_S_to_W_Distance_Change + df$Right_S_to_W_Distance_Change) / 2) / df$Duration


# Apply the function to all three columns and keep rows without outliers
df_filtered <- df[filter_outliers(df$Duration) &
                    filter_outliers(df$Average_Distance_Normalized) &
                    filter_outliers(df$Average_Space_Normalized), ]

# Encode vector types
df$Narrative %<>% as.factor() 
df$Accessibility %<>% as.numeric() # for plotting purposes
df$Participant %<>% as.factor()
df$Event %<>% as.factor()
df$Average_Amplitude  %<>% as.double()
df$Average_Space  %<>% as.double()
df$Average_Speed %<>% as.double()
df$Event_Duration %<>% as.double()
df$Nativeness %<>% as.factor()
df$Exposure %<>% as.factor()

options(scipen=999)

library("dgof")

shapiro.test(log(df_filtered$Duration))
hist(df$Duration)
ks.test(jitter(log(df_filtered$Duration)), "pnorm")

shapiro.test(log(df_filtered$Average_Distance_Normalized))
ks.test(jitter(df$Average_Distance_Normalized), "pnorm")
ks.test(jitter(log(df_filtered$Average_Distance_Normalized)), "pnorm")

shapiro.test(log(df_filtered$Average_Space_Normalized))
ks.test(jitter(log(df_filtered$Average_Space_Normalized)), "pnorm")


# Apply the function to all three columns and keep rows without outliers
df_filtered <- df[filter_outliers(df$Duration) &
                    filter_outliers(df$Average_Distance_Normalized) &
                    filter_outliers(df$Average_Space_Normalized), ]



df_summary <- df %>%
  group_by(Nativeness) %>%
  summarise(
    R_Distance = mean(Right_Hand_Movement_Amplitude),
    L_Distance = mean(Left_Hand_Movement_Amplitude),
    R_Space= mean(Right_S_to_W_Distance_Change),
    L_Space = mean(Left_S_to_W_Distance_Change),
    .groups = 'drop'  
  )


# Contrast coding
df_filtered$Nativeness <- factor(df_filtered$Nativeness, levels = c("Native","Late"))
df_filtered$Exposure <- factor(df_filtered$Exposure, levels = c("0-3","4-7","13-17","8-12"))

contrasts(df_filtered$Nativeness) <- contr.sum(2)
contrasts(df_filtered$Exposure) <- contr.sum(4)

# Model for duration
model1_d <- lmer(log(Event_Duration) ~ Nativeness  + (1|Participant) + (1|Event) + (1|Narrative), data = df_filtered)
model2_d <- lmer(log(Event_Duration) ~ as.factor(Exposure)  +(1|Participant) + (1|Event) + (1|Narrative), data = df)


#tab_model(model1_dis)
#tab_model(model2_d)

# Model for amplitude
model1_dis <- lmer(log(Average_Distance_Normalized) ~ Nativeness  + (1|Participant) + (1|Event) + (1|Narrative), data = df_filtered)


# Model for space
model1_space <- lmer(log(Average_Space_Normalized) ~ Nativeness + (1|Participant) + (1|Event) + (1|Narrative), data = df_filtered)

#tab_model(model1_space)
#tab_model(model2_space)

#tab_model(c(model1_d,model1_dis,model1_space), show.intercept = FALSE)


####

remove_outliers <- function(df, columns) {
  df %>% 
    filter(across(all_of(columns), ~abs(scale(.)) < 3))
}

columns_to_check <- c('Average_Amplitude', 'Average_Space', 'Average_Speed')

df <- remove_outliers(df, columns_to_check)

# Calculate positions for annotations to ensure they are within the plot area
max_x <- max(as.numeric(df$Nativeness), na.rm = TRUE)
max_y <- max(df_summary$Mean_Amplitude + df_summary$SE_Amplitude, na.rm = TRUE)


# Adjustments to ensure all elements are correctly plotted
p_amplitude <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_jitter(data = df, aes(x = Nativeness, y = Average_Space_Normalized, color=factor(Nativeness)),size = 2, alpha = 0.5, show.legend = FALSE) +
  geom_errorbar(data = df_summary, aes(x = Nativeness, ymin = Mean_Normalized_Space - SE_Normalized_Space, ymax = Mean_Normalized_Space + SE_Normalized_Space), width = 0.1, color = "#F8766D") +
  geom_point(data = df_summary, aes(x = Nativeness, y = Mean_Normalized_Space), size = 3, color = "red") +
  geom_smooth(data = df, aes(x = as.numeric(Nativeness), y = Average_Space_Normalized), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary, aes(x = as.numeric(Nativeness), y = Mean_Normalized_Space), alpha = 1, color = "red", size = 0.6) +
  geom_label(data = df_summary, aes(x = Nativeness, y = Mean_Normalized_Space, label = sprintf("%.3f", Mean_Normalized_Space)), vjust = -0.5, fill = "white", color = "black", fontface = "bold", size = 5) +
  xlab("Acquisition Group") + ylab("Average Sign Space Use") +
  #geom_text(aes(x = max_x, y = max_y, label = sprintf("p-value > %.4f", 0.01)), 
   #         color = "black", hjust = 0.5, vjust = -25, check_overlap = TRUE, size = 4.5) +
  theme(
    plot.title = element_text(hjust = 0.25),
    axis.title.x = element_text(margin = margin(t = 20), size=18),
    axis.title.y = element_text(margin = margin(r = 20), size=18),
    legend.position = "none"
  )+
  theme(text = element_text(size=23)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35)))
p_amplitude
ggsave("p_normalized_space_nat.png",p_amplitude,  width = 6.5, height =6)



# Print the plot
# Plot for Average Speed
p_speed <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_point(data = df, aes(x = Jittered_Accessibility, y = Average_Amplitude), color="black",size = 2, alpha = 0.1, show.legend = FALSE) +
  geom_errorbar(data = df_summary, aes(x = Accessibility, ymin = mean_amplitude - se_amplitude, ymax = mean_amplitude + se_amplitude), width = 0.1, color = "#F8766D") +
  geom_point(data = df_summary, aes(x = Accessibility, y = mean_amplitude), size = 3, color = "red") +
  geom_smooth(data = df, aes(x = Accessibility, y = Average_Amplitude), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary, aes(x = Accessibility, y = mean_amplitude), alpha = 1, color = "red", size = 0.6) +
  #scale_x_continuous(breaks = df$Accessibility) +  # Specify breaks at all unique x values
  xlab("Accessibility") + ylab("Average Accumulated Hand Distance") +
  #geom_text(aes(x = max_x, y = max_y, label = sprintf("p-value < %.4f\n β = %.4f", 0.001, 0.027)), 
  #         color = "black", hjust = 1, vjust = -5, check_overlap = TRUE, size = 5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 20), size=18),
    axis.title.y = element_text(margin = margin(r = 20), size=18),
    legend.position = "none"
  )+
  theme(text = element_text(size=15)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35))) + scale_x_continuous(breaks = df$Accessibility) 
p_speed
ggsave("p_distance_ac.png",p_speed,  width = 6.5, height =6)

p_space <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_point(data = df, aes(x = Jittered_Accessibility, y = Average_Space), color="black",size = 2, alpha = 0.1, show.legend = FALSE) +
  geom_errorbar(data = df_summary, aes(x = Accessibility, ymin = mean_speed - se_amplitude, ymax = mean_speed + se_amplitude), width = 0.1, color = "#F8766D") +
  geom_point(data = df_summary, aes(x = Accessibility, y = mean_speed), size = 3, color = "red") +
  geom_smooth(data = df, aes(x = Accessibility, y = Average_Space), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary, aes(x = Accessibility, y = mean_speed), alpha = 1, color = "red", size = 0.6) +
  xlab("Accessibility") + ylab("Average Sign Space Use)") +
  geom_text(aes(x = max_x, y = max_y, label = sprintf("p-value = %.4f\n β = %.4f", 0.115, 0.002)), 
            color = "black", hjust = 1, vjust = -3.5,  check_overlap = TRUE, size = 5) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 20), size=18),
    axis.title.y = element_text(margin = margin(r = 20), size=18),
    legend.position = "none"
  )+
  theme(text = element_text(size=15)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35))) + scale_x_continuous(breaks = df$Accessibility) 
p_space
ggsave("p_space_ac.png",p_space,  width = 6.5, height =6)