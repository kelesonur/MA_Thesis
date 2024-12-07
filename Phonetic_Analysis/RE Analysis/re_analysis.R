# Libraries
library(magrittr)
library("dgof")
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


#Outcome variables
df$Average_Space <- (df$Right_S_to_W_Distance_Change + df$Left_S_to_W_Distance_Change) / 2
df$Average_Amplitude <- (df$Right_Hand_Movement_Amplitude + df$Left_Hand_Movement_Amplitude) / 2
  
df$Average_Distance_Normalized <- df$Average_Amplitude / df$Duration
df$Average_Space_Normalized <- df$Average_Space / df$Duration


# Encode vector types
df$Narrative %<>% as.integer() 
df$Accessibility %<>% as.numeric() # for stats
df$Participant %<>% as.integer()
df$RE %<>% as.factor() 
df$Order %<>% as.integer()
df$Discourse %<>% as.factor()
df$Average_Amplitude  %<>% as.double()
df$Average_Space  %<>% as.double()
df$Average_Distance_Normalized %<>% as.double()
df$Average_Space_Normalized %<>% as.double()
df$Duration %<>% as.double()
df$Nativeness %<>% as.factor() 

options(scipen=999)

df_filtered = df

# Contrast coding
df_filtered$RE <- factor(df_filtered$RE, levels = c("FARE","KEDİ"))
df_filtered$Discourse <- factor(df_filtered$Discourse, levels = c("Introduction","Maintenance","Re-introduction"))
df_filtered$Nativeness <- factor(df_filtered$RE, levels = c("Native","Late"))

contrasts(df_filtered$RE) <- contr.sum(2)
contrasts(df_filtered$Nativeness) <- contr.sum(2)
contrasts(df_filtered$Discourse) <- contr.sum(3)

shapiro.test(df_filtered$Duration)
shapiro.test(log(df_filtered$Duration))
hist(df_filtered$Duration)

ks.test(jitter(df_filtered$Duration), "pnorm")
ks.test(jitter(log(df_filtered$Duration)), "pnorm")

shapiro.test(df_filtered$Average_Distance_Normalized)
shapiro.test(log(df_filtered$Average_Distance_Normalized))

ks.test(jitter(df_filtered$Average_Distance_Normalized), "pnorm")
ks.test(jitter(log(df_filtered$Average_Distance_Normalized)), "pnorm")

shapiro.test(df_filtered$Average_Space_Normalized)
shapiro.test(log(df_filtered$Average_Space_Normalized))

ks.test(jitter(df_filtered$Average_Space_Normalized), "pnorm")
ks.test(jitter(log(df_filtered$Average_Space_Normalized)), "pnorm")

# Function to filter outliers based on 3 * IQR
filter_outliers <- function(column) {
  iqr_value <- IQR(column)
  lower_bound <- quantile(column, 0.25) -3 * iqr_value
  upper_bound <- quantile(column, 0.75) + 3* iqr_value
  return(column >= lower_bound & column <= upper_bound)
}

# Apply filtering separately for each accessibility level
df_filtered <- df %>% 
  group_by(Accessibility) %>% 
  filter(filter_outliers(Duration) & 
           filter_outliers(Average_Distance_Normalized) & 
           filter_outliers(Average_Space_Normalized))

# Model for duration
model1_d <- lmer(log(Duration) ~ Discourse + (1|Participant) + (1|Narrative), data = df_filtered)
model2_d <- lmer(log(Duration) ~ Discourse + RE + (1|Participant)+ (1|Narrative), data = df_filtered)
model3_d <- lmer(log(Duration) ~ Discourse*RE + (1|Participant) + (1|Narrative), data = df_filtered)
model4_d <- lmer(log(Duration) ~ RE + (1|Participant) + (1|Narrative), data = df_filtered)

model1_d_a <- lmer(log(Duration) ~ Accessibility + (1|Participant) + (1|Narrative), data = df_filtered)
model2_d_a <- lmer(log(Duration) ~ Accessibility + RE + (1|Participant) + (1|Narrative), data = df_filtered)
model3_d_a <- lmer(log(Duration) ~ Accessibility*RE + (1|Participant) + (1|Narrative), data = df_filtered)
model4_d_a <- lmer(log(Duration) ~ RE + (1|Participant) + (1|Narrative), data = df_filtered)

#anova(model1_d_a,model2_d_a)

#tab_model(model1_d_a)


model1_dis <-lmer(log(Average_Distance_Normalized) ~ Discourse + Nativeness + (1|Participant) + (1|Narrative), data = df_filtered)
model2_dis <- lmer(log(Average_Distance_Normalized) ~ RE + (1|Participant)+ (1|Narrative), data = df_filtered)
model3_dis <- lmer(log(Average_Distance_Normalized) ~ Discourse+Nativeness + (1|Participant)+ (1|Narrative), data = df_filtered)
model4_dis <- lmer(log(Duration) ~ Discourse*RE + (1|Participant)+ (1|Narrative), data = df_filtered)

#anova(model1_dis,model3_dis)

#tab_model(model3_dis)

model1_dis_a <-lmer(log(Average_Distance_Normalized) ~ Accessibility + (1|Participant) + (1|Narrative), data = df_filtered)
model2_dis_a <- lmer(log(Average_Distance_Normalized) ~ RE + (1|Participant)+ (1|Narrative), data = df_filtered)
model3_dis_a <- lmer(log(Average_Distance_Normalized) ~ Accessibility+RE + (1|Participant)+ (1|Narrative), data = df_filtered)
model4_dis_a <- lmer(log(Average_Distance_Normalized) ~ Accessibility*RE + (1|Participant)+ (1|Narrative), data = df_filtered)

#anova(model1_dis_a,model3_dis_a)
#tab_model(model1_dis_a)

# Model for space

model1_s <-lmer(log(Average_Space_Normalized) ~ Discourse + (1|Participant) + (1|Narrative), data = df_filtered)
model2_s <- lmer(log(Average_Space_Normalized) ~ RE + (1|Participant)+ (1|Narrative), data = df_filtered)
model3_s <- lmer(log(Average_Space_Normalized) ~ Discourse+RE + (1|Participant)+ (1|Narrative), data = df_filtered)
model4_s <- lmer(log(Average_Space_Normalized) ~ Discourse*RE + (1|Participant)+ (1|Narrative), data = df_filtered)

#anova(model3_s,model4_s)

#tab_model(model3_s)

model1_s_a <-lmer(log(Average_Space_Normalized) ~ Accessibility + (1|Participant) + (1|Narrative), data = df_filtered)
model2_s_a <- lmer(log(Average_Space_Normalized) ~ RE + (1|Participant)+ (1|Narrative), data = df_filtered)
model3_s_a <- lmer(log(Average_Space_Normalized) ~ Accessibility+RE + (1|Participant)+ (1|Narrative), data = df_filtered)
model4_s_a <- lmer(log(Average_Space_Normalized) ~ Accessibility*RE + (1|Participant)+ (1|Narrative), data = df_filtered)

#anova(model3_s_a,model4_s_a)
#tab_model(model4_s_a)

# Discourse table
# Tabulate model
tab_model(c(model3_d,model1_dis,model3_s), show.intercept = FALSE)

tab_model(c(model2_d_a,model1_dis_a,model4_s_a), show.intercept = FALSE)

########### PLOT ########

df_summary_discourse <- df_filtered %>%
  group_by(Discourse) %>%
  summarise(
    Mean_Duration = mean(Duration),
    SE_Duration = sd(Duration) / sqrt(n()),
    Mean_Amplitude_Normalized = mean(Average_Distance_Normalized),
    SE_Amplitude_Normalized = sd(Average_Distance_Normalized) / sqrt(n()),
    Mean_Space_Normalized = mean(Average_Space_Normalized),
    SE_Space_Normalized = sd(Average_Space_Normalized) / sqrt(n()),
    .groups = 'drop'  # Ensure the summarised dataframe doesn't carry grouping
  )


df_summary_acc <- df %>% 
  group_by(Accessibility) %>%
  summarise(
    n = n(),
    Mean_Duration = mean(Duration),
    SE_Duration = sd(Duration) / sqrt(n()),
    Mean_Amplitude_Normalized = mean(Average_Distance_Normalized),
    SE_Amplitude_Normalized = sd(Average_Distance_Normalized) / sqrt(n()),
    Mean_Space_Normalized = mean(Average_Space_Normalized),
    SE_Space_Normalized = sd(Average_Space_Normalized) / sqrt(n()),
    .groups = 'drop'  # Ensure the summarised dataframe doesn't carry grouping
  )

#write.csv(df_summary_discourse,"df_summary_discourse.csv")

# Adding jitter to Accessibility directly in the data frame
df_filtered$Jittered_Accessibility <- df_filtered$Accessibility + runif(nrow(df_filtered), -0.05, 0.05)

# Calculate positions for annotations to ensure they are within the plot area
max_x <- max(as.numeric(df_summary_acc$Discourse), na.rm = TRUE)
max_y <- max(df_summary_acc$Mean_Duration + df_summary_acc$SE_Duration, na.rm = TRUE)

# Distance plot
df_filtered$Discourse <- factor(df_filtered$Discourse, levels = c("Introduction","Re-introduction","Maintenance"))

p_amplitude <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_jitter(data = df_filtered, aes(x = Discourse, y = Average_Distance_Normalized, color=factor(Discourse)),size = 2, alpha = 0.5, show.legend = FALSE) +
  geom_errorbar(data = df_summary_discourse, aes(x = Discourse, ymin = Mean_Amplitude_Normalized - SE_Amplitude_Normalized, ymax = Mean_Amplitude_Normalized + SE_Amplitude_Normalized), width = 0.2, color = "#F8766D") +
  geom_point(data = df_summary_discourse, aes(x = Discourse, y = Mean_Amplitude_Normalized), size = 3, color = "red") +
  geom_smooth(data = df_filtered, aes(x = as.numeric(Discourse), y = Average_Distance_Normalized), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary_discourse, aes(x = as.numeric(Discourse), y = Mean_Amplitude_Normalized), alpha = 1, color = "red", size = 0.6) +
  geom_label(data = df_summary_discourse, aes(x = Discourse, y = Mean_Amplitude_Normalized, label = sprintf("%.3f", Mean_Amplitude_Normalized)), vjust = -0.5, fill = "white", color = "black", fontface = "bold", size = 5) +
  xlab("Discourse") + ylab("Average Accumulated Hand Distance") +
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

ggsave("p_distance_discourse.png",p_amplitude,  width = 6.5, height =6)


p_duration <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_jitter(data = df_filtered, aes(x = Discourse, y = Duration, color=factor(Discourse)),size = 2, alpha = 0.5, show.legend = FALSE) +
  geom_errorbar(data = df_summary_discourse, aes(x = Discourse, ymin = Mean_Duration - SE_Duration, ymax = Mean_Duration + SE_Duration), width = 0.2, color = "#F8766D") +
  geom_point(data = df_summary_discourse, aes(x = Discourse, y = Mean_Duration), size = 3, color = "red") +
  geom_smooth(data = df_filtered, aes(x = as.numeric(Discourse), y = Duration), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary_discourse, aes(x = as.numeric(Discourse), y = Mean_Duration), alpha = 1, color = "red", size = 0.6) +
  geom_label(data = df_summary_discourse, aes(x = Discourse, y = Mean_Duration, label = sprintf("%.3f", Mean_Duration)), vjust = -0.5, fill = "white", color = "black", fontface = "bold", size = 5) +
  xlab("Discourse") + ylab("Average Duration (sec)") +
  theme(
    plot.title = element_text(hjust = 0.25),
    axis.title.x = element_text(margin = margin(t = 20), size=18),
    axis.title.y = element_text(margin = margin(r = 20), size=18),
    legend.position = "none"
  )+
  theme(text = element_text(size=23)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35)))
p_duration

ggsave("p_duration_discourse.png",p_duration,  width = 6.5, height =6)

p_space <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_jitter(data = df_filtered, aes(x = Discourse, y = Average_Space_Normalized, color=factor(Discourse)),size = 2, alpha = 0.5, show.legend = FALSE) +
  geom_errorbar(data = df_summary_discourse, aes(x = Discourse, ymin = Mean_Space_Normalized - SE_Space_Normalized, ymax = Mean_Space_Normalized + SE_Space_Normalized), width = 0.2, color = "#F8766D") +
  geom_point(data = df_summary_discourse, aes(x = Discourse, y = Mean_Space_Normalized), size = 3, color = "red") +
  geom_smooth(data = df_filtered, aes(x = as.numeric(Discourse), y = Average_Space_Normalized), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary_discourse, aes(x = as.numeric(Discourse), y = Mean_Space_Normalized), alpha = 1, color = "red", size = 0.6) +
  geom_label(data = df_summary_discourse, aes(x = Discourse, y = Mean_Space_Normalized, label = sprintf("%.3f", Mean_Space_Normalized)), vjust = -0.5, fill = "white", color = "black", fontface = "bold", size = 5) +
  xlab("Discourse") + ylab("Average Sign Space Use") +
  theme(
    plot.title = element_text(hjust = 0.25),
    axis.title.x = element_text(margin = margin(t = 20), size=18),
    axis.title.y = element_text(margin = margin(r = 20), size=18),
    legend.position = "none"
  )+
  theme(text = element_text(size=23)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35)))
p_space

ggsave("p_space_discourse.png",p_space,  width = 6.5, height =6)


## Accessibility Plots ##

p_amplitude_acc <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_jitter(data = df_filtered, aes(x = Jittered_Accessibility, y = Average_Distance_Normalized, color=Discourse),size = 2, alpha = 0.5, show.legend = TRUE) +
  geom_errorbar(data = df_summary_acc, aes(x = Accessibility, ymin = Mean_Amplitude_Normalized - SE_Amplitude_Normalized, ymax = Mean_Amplitude_Normalized + SE_Amplitude_Normalized), width = 0.2, color = "#F8766D") +
  geom_point(data = df_summary_acc, aes(x = Accessibility, y = Mean_Amplitude_Normalized), size = 3, color = "red") +
  geom_smooth(data = df_filtered, aes(x = as.numeric(Accessibility), y = Average_Distance_Normalized), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary_acc, aes(x = as.numeric(Accessibility), y = Mean_Amplitude_Normalized), alpha = 1, color = "red", size = 0.6) +
  geom_label(data = df_summary_acc, aes(x = Accessibility, y = Mean_Amplitude_Normalized, label = sprintf("%.3f", Mean_Amplitude_Normalized)), vjust = -0.5, fill = "white", color = "black", fontface = "bold", size = 5) +
  xlab("Accessibility Score") + ylab("Average Accumulated Hand Distance") +
  theme(
    plot.title = element_text(hjust = 0.25),
    axis.title.x = element_text(margin = margin(t = 20), size=18),
    axis.title.y = element_text(margin = margin(r = 20), size=18),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
  )+
  theme(text = element_text(size=23)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35)))
p_amplitude_acc

ggsave("p_distance_acc.png",p_amplitude_acc,  width = 6.5, height =6)


p_duration_acc <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_jitter(data = df_filtered, aes(x = Jittered_Accessibility, y = Duration, color=Discourse),size = 2, alpha = 0.5, show.legend = TRUE) +
  geom_errorbar(data = df_summary_acc, aes(x = Accessibility, ymin = Mean_Duration - SE_Duration, ymax = Mean_Duration + SE_Duration), width = 0.2, color = "#F8766D") +
  geom_point(data = df_summary_acc, aes(x = Accessibility, y = Mean_Duration), size = 3, color = "red") +
  geom_smooth(data = df_filtered, aes(x = as.numeric(Accessibility), y = Duration), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary_acc, aes(x = as.numeric(Accessibility), y = Mean_Duration), alpha = 1, color = "red", size = 0.6) +
  geom_label(data = df_summary_acc, aes(x = Accessibility, y = Mean_Duration, label = sprintf("%.3f", Mean_Duration)), vjust = -0.5, fill = "white", color = "black", fontface = "bold", size = 5) +
  xlab("Accessibility Score") + ylab("Average Duration (sec)") +
  theme(
    plot.title = element_text(hjust = 0.25),
    axis.title.x = element_text(margin = margin(t = 20), size=18),
    axis.title.y = element_text(margin = margin(r = 20), size=18),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
  )+
  theme(text = element_text(size=23)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35)))
p_duration_acc

ggsave("p_duration_acc.png",p_duration_acc,  width = 6.5, height =6)

p_space_acc <- ggplot() +
  theme(text = element_text(size=25)) +
  ggthemes::theme_tufte() +
  geom_jitter(data = df_filtered, aes(x = Jittered_Accessibility, y = Average_Space_Normalized, color=Discourse),size = 2, alpha = 0.5, show.legend = TRUE) +
  geom_errorbar(data = df_summary_acc, aes(x = Accessibility, ymin = Mean_Space_Normalized - SE_Space_Normalized, ymax = Mean_Space_Normalized + SE_Space_Normalized), width = 0.2, color = "#F8766D") +
  geom_point(data = df_summary_acc, aes(x = Accessibility, y = Mean_Space_Normalized), size = 3, color = "red") +
  geom_smooth(data = df_filtered, aes(x = as.numeric(Accessibility), y = Average_Space_Normalized), method = "lm", se = TRUE, color = "black", linetype = "dashed",size=1.25) +
  geom_line(data = df_summary_acc, aes(x = as.numeric(Accessibility), y = Mean_Space_Normalized), alpha = 1, color = "red", size = 0.6) +
  geom_label(data = df_summary_acc, aes(x = Accessibility, y = Mean_Space_Normalized, label = sprintf("%.3f", Mean_Space_Normalized)), vjust = -0.5, fill = "white", color = "black", fontface = "bold", size = 5) +
  xlab("Accessibility Score") + ylab("Average Sign Space Use") +
  theme(
    plot.title = element_text(hjust = 0.25),
    axis.title.x = element_text(margin = margin(t = 20), size=18),
    axis.title.y = element_text(margin = margin(r = 20), size=18),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
  )+
  theme(text = element_text(size=23)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35)))
p_space_acc

ggsave("p_space_acc.png",p_space_acc,  width = 6.5, height =6)




