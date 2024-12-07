# Libraries
library(magrittr)
library(tidyverse) 
library(lme4)
library(patchwork)
library(lmerTest)
library(sjPlot)
#library("viridis")
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

# Data
df = read.csv("/Users/onurkeles/Documents/GitHub/MA_Thesis/Discourse_Cohesion_Analysis/df.csv")

# Recoding
df$ref_type <- ifelse(df$GTR1 %in% c("Bare noun", "Point noun", "Noun Point","Fingerspelled noun", "CL noun", "Noun CL"), "NOM",
                      ifelse(df$GTR1 %in% c("Person agreement verb"), "AGR",
                             ifelse(df$GTR1 %in% c("Plain verb"), "PLAIN",
                             ifelse(df$GTR1 %in% c("Constructed action", "Constructed Action"), "CA",
                                    ifelse(df$GTR1 %in% c("Whole entity CL"), "WECL",
                                    ifelse(df$GTR1 %in% c("Body part CL"), "BPCL",
                                           ifelse(df$GTR1 %in% c("SASS - Static"), "STATIC",
                                                  ifelse(df$GTR1 %in% c("SASS - Tracing"), "TRACE", "PRO"))))))))

# Recoding
df$ref_type2 <- ifelse(df$GTR1 %in% c("Bare noun", "Point noun", "Noun Point","Fingerspelled noun", "CL noun", "Noun CL"), "NOM",
                             ifelse(df$GTR1 %in% c("Plain verb","Person agreement verb"), "VERB",
                                    ifelse(df$GTR1 %in% c("Constructed action", "Constructed Action"), "CA",
                                           ifelse(df$GTR1 %in% c("Whole entity CL","Body part CL","SASS - Tracing","SASS - Static"), "CL", "PRO"))))


df$Nativeness <- ifelse(df$Nativeness == 1, "Native", "Late")

# Remove Participant 34:
df <- df %>% filter(Subject != 34)

#### PLOTTING ####

f <- function(x) {
  r <- quantile(x, probs = c(0.1, 0.9))
  names(r) <- c("ymin", "ymax")
  r
}

# Reordering factor for plotting
df$ref_type <- factor(df$ref_type, levels = c("NOM","TRACE", "PRO","STATIC", "WECL", "CA", "BPCL", "AGR", "PLAIN"))
df$Nativeness <- factor(df$Nativeness, levels = c("Native","Late"))

# Count plots 

# Ensure the dataframe has correct column names and the correct percentage calculation
sum1 <- df %>%
  group_by(Discourse, ref_type) %>%
  summarise(n = n()) %>%
  mutate(Perc = (n / sum(n)) * 100)

sum2 <- df %>%
  group_by(Nativeness, Discourse, ref_type) %>%
  summarise(n = n()) %>%
  mutate(Perc = (n / sum(n)) * 100)

sum2 <- df %>%
  group_by(Nativeness, Discourse) %>%  # Group by Nativeness and Discourse to calculate N
  mutate(N = n()) %>%  # Total number of RE types within each Nativeness and Discourse
  group_by(Nativeness, Discourse, ref_type) %>%  # Now, group by ref_type within the previous grouping
  summarise(
    n = n(),  # Count for each RE type
    N = first(N),  # Use N calculated earlier for the broader group
    p = n / N,  # Proportion of each RE type within the broader group
    se = sqrt(p * (1 - p) / N)  # Standard error of the proportion
  ) %>%
  mutate(Perc = p * 100)  # Convert proportion to percentage

sum2 <- df %>%
  group_by(Nativeness, Discourse, ref_type) %>%
  summarise(n = n()) %>%
  mutate(Perc = (n / sum(n)) * 100)

# Filter data for Native speakers
native_plot <- ggplot(sum2 %>% filter(Nativeness == "Native"), aes(x = Discourse, y = Perc, fill = ref_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  labs(
    x = "Discourse Type",
    y = "Percent RE Use",
    fill = "RE Type",
    title = "(a) Native Signers"  # Add title for the first plot
  ) +
  theme_tufte() +
  geom_text(
    aes(label = sprintf("%.1f", Perc), y = Perc),
    position = position_dodge(width = 0.8),
    size = 5,  # Increased text size
    vjust = -0.5
  ) +
  theme(
    legend.position = "none",  # Remove legend for Native Signers,
    text = element_text(size=25),
    plot.title = element_text(size = 25),  # Set title size
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25)),
    panel.spacing = unit(2, "lines"),
    strip.text = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

# Filter data for Late learners
late_plot <- ggplot(sum2 %>% filter(Nativeness == "Late"), aes(x = Discourse, y = Perc, fill = ref_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  labs(
    x = "Discourse Type",
    y = "Percent RE Use",
    fill = "RE Type",
    title = "(b) Late Signers"  # Add title for the second plot
  ) +
  theme_tufte() +
  geom_text(
    aes(label = sprintf("%.1f", Perc), y = Perc),
    position = position_dodge(width = 0.8),
    size = 5,
    vjust = -0.5
  ) +
  theme(
    legend.position = "bottom",  # Keep legend for Late Signers
    text = element_text(size=25),
    plot.title = element_text(size = 25),  # Set title size to match axis labels
    axis.title.x = element_text(margin = margin(t = 25)),
    axis.title.y = element_text(margin = margin(r = 25)),
    panel.spacing = unit(2, "lines"),
    strip.text = element_text(face = "bold")
  ) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0))

p_count <- native_plot / late_plot + 
  plot_layout(heights = c(1, 1.2))

ggsave("p_count",p_count,  width = 12, height = 13.5)

df$ref_type <- factor(df$ref_type, levels = c("NOM","PRO", "STATIC", "TRACE", "WECL", "BPCL", "CA", "PLAIN", "AGR"))

# Accessibility plots

# Accessibility score by re type
p_retype <- ggplot(df, aes(x = ref_type, y = Accessibility)) +
  theme(text = element_text(size=25)) +
  xlab("RE Type") + ylab("Accessibility Score") +
  ggthemes::theme_tufte() +
  geom_jitter(aes(color=ref_type), position = position_jitter(width = 0.2), size = 3, alpha = 0.1) +
  stat_summary(fun.data = f, geom = "errorbar", width = 0.2, color = "black", size = 0.2) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5, fontface = "bold", color = "black") +
  # Add mean labels
  stat_summary(
    fun.y = mean, 
    geom = "label", 
    aes(label = sprintf("%.2f", ..y..)), 
    vjust = 0.5, 
    hjust = 0.5,
    fontface = "bold", 
    size = 5.5,
    color = "black",
    fill = "white"  
    ) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-2.5, 5.5), breaks = seq(-2, 5, by = 1)) +
  theme(text = element_text(size=20)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), 
        axis.title.y = element_text(margin = margin(r = 35))) 

ggsave("p_retype.png",p_retype,  width = 10, height =4)

# Plot by nativeness, discourse, ref_type

# df for plot
interaction_data <- df %>%
  group_by(ref_type, Nativeness, Discourse) %>%
  summarise(AccessibilityMean = mean(Accessibility), .groups = 'drop')

interaction_data <- interaction_data %>%
  group_by(Discourse, ref_type) %>%
  mutate(
    text_position = ifelse(AccessibilityMean > median(AccessibilityMean), -1, 1)  # Adjust condition as needed
  ) %>%
  ungroup()

interaction_data <- interaction_data %>%
  mutate(
    AdjustedAccessibilityMean = ifelse(Discourse == "Introduction", pmin(pmax(AccessibilityMean, -2), 2), AccessibilityMean)
  )


df$ref_type2 <- factor(df$ref_type2, levels = c("NOM","PRO", "CL", "CA", "VERB"))

intro_plot <- ggplot(df %>% filter(Discourse == "Introduction"), #%>% filter(!ref_type %in% c("CA","PRO","PLAIN","AGR")), 
                     aes(x = ref_type, y = Accessibility, group = Nativeness, color= Nativeness, shape = Nativeness)) +
  geom_jitter(color = "#F8766D", position = position_jitter(width = 0.2), size = 3, alpha = 0.1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.2, size = 0.6, 
               position = position_dodge(width = 0.8)) +  # Add error bars for both groups
  stat_summary(fun = mean, geom = "point", size = 4, 
               position = position_dodge(width = 0.8)) +  # Add mean points with shape showing Nativeness
 stat_summary(fun = mean, geom = "label", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, hjust=0.5, size = 5.5, color="black",fontface = "bold", fill = "white", 
               label.size = 0.3, position = position_dodge(width = 0.8)) +  # Add mean labels above each shape
  labs(x = "RE Type", y = "Accessibility Score", shape = "Nativeness", title = "(a) Introduction") +
  theme_tufte() +
  scale_shape_manual(name = "Acquisition", labels = c("Native", "Late"), values = c(19, 17)) +  # Shape for Nativeness
  scale_color_manual(name = "Acquisition", labels = c("Native", "Late"), values = c("#619CFF","#00BA38")) +
  theme(legend.position = "none", 
        text = element_text(size = 25), 
        plot.title = element_text(size = 25),  # Set title size to match axis labels
        axis.title.x = element_text(margin = margin(25)),
        panel.spacing = unit(2, "lines"), 
        strip.text = element_text(face = "bold")) +
  scale_y_continuous(limits = c(-2.5, 5), breaks = seq(-2, 5, by = 1))
ggsave("introplot.png",intro_plot,  width = 14, height =7)

maintenance_plot <- ggplot(df %>% filter(Discourse == "Maintenance"), #%>% filter(!ref_type %in% c("CA","PRO","PLAIN","AGR")), 
                     aes(x = ref_type, y = Accessibility, group = Nativeness, color= Nativeness, shape = Nativeness)) +
  geom_jitter(color = "#F8766D", position = position_jitter(width = 0.2), size = 3, alpha = 0.1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.2, size = 0.6, 
               position = position_dodge(width = 0.8)) +  # Add error bars for both groups
  stat_summary(fun = mean, geom = "point", size = 4, 
               position = position_dodge(width = 0.8)) +  # Add mean points with shape showing Nativeness
  stat_summary(fun = mean, geom = "label", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, hjust=0.5, size = 5.5, color="black",fontface = "bold", fill = "white", 
               label.size = 0.3, position = position_dodge(width = 0.8)) +  # Add mean labels above each shape
  labs(x = "RE Type", y = "Accessibility Score", shape = "Nativeness", title = "(b) Maintenance") +
  theme_tufte() +
  scale_shape_manual(name = "Acquisition", labels = c("Native", "Late"), values = c(19, 17)) +  # Shape for Nativeness
  scale_color_manual(name = "Acquisition", labels = c("Native", "Late"), values = c("#619CFF","#00BA38")) +
  theme(legend.position = "none", 
        text = element_text(size = 25), 
        plot.title = element_text(size = 25),  # Set title size to match axis labels
        axis.title.x = element_text(margin = margin(25)),
        panel.spacing = unit(2, "lines"), 
        strip.text = element_text(face = "bold")) +
  scale_y_continuous(limits = c(-2, 5.5), breaks = seq(-2, 5, by = 1))
ggsave("maintenance.png",maintenance_plot,  width = 14, height =7)

reintro_plot <- ggplot(df %>% filter(Discourse == "Re-introduction"), #%>% filter(!ref_type %in% c("CA","PRO","PLAIN","AGR")), 
                           aes(x = ref_type, y = Accessibility, group = Nativeness, color=Nativeness, shape = Nativeness)) +
  geom_jitter(color = "#F8766D",position = position_jitter(width = 0.2), size = 3, alpha = 0.1) +
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.2, size = 0.6, 
               position = position_dodge(width = 0.8)) +  # Add error bars for both groups
  stat_summary(fun = mean, geom = "point", size = 4, 
               position = position_dodge(width = 0.8)) +  # Add mean points with shape showing Nativeness
  stat_summary(fun = mean, geom = "label", aes(label = sprintf("%.2f", ..y..)), 
               vjust = -0.5, hjust=0.5, size = 5.5, color="black",fontface = "bold", fill = "white", 
               label.size = 0.3, position = position_dodge(width = 0.8)) +  # Add mean labels above each shape
  labs(x = "RE Type", y = "Accessibility Score", shape = "Nativeness", title = "(c) Re-introduction") +
  theme_tufte() +
  scale_shape_manual(name = "Acquisition", labels = c("Native", "Late"), values = c(19, 17)) +  # Shape for Nativeness
  scale_color_manual(name = "Acquisition", labels = c("Native", "Late"), values = c("#619CFF","#00BA38")) +
  theme(legend.position = "bottom", 
        text = element_text(size = 25), 
        plot.title = element_text(size = 25),  # Set title size to match axis labels
        axis.title.x = element_text(margin = margin(25)),
        panel.spacing = unit(2, "lines"), 
        strip.text = element_text(face = "bold")) +
  scale_y_continuous(limits = c(-2, 5.5), breaks = seq(-2, 5, by = 1))
ggsave("reintro.png",reintro_plot,  width = 14, height =7)


combined_plot <- intro_plot / maintenance_plot / reintro_plot + 
  plot_layout(heights = c(1, 1, 1.2))

combined_plot

ggsave("nativenessplot.png",combined_plot,  width = 13, height =14.5)

# Discourse plot

p_discourse <- ggplot(df, aes(x = Discourse, y = Accessibility)) +
  theme(text = element_text(size=25)) +
  xlab("Discourse") + ylab("Accessibility Score") +
  ggthemes::theme_tufte() +
  geom_jitter(aes(color=Discourse), position = position_jitter(width = 0.2), size = 3, alpha = 0.10) +
  geom_segment(aes(x = Discourse, xend = Discourse, y = Accessibility, yend = Accessibility), 
               position = position_jitter(width = 0.2), size = 0.5, nudge_y = -0.4) +
  stat_summary(fun.data = f, geom = "errorbar", width = 0.2, color = "black", size = 0.2) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 5, color = "black") +
  # Add mean labels
  stat_summary(
    fun.y = mean, 
    geom = "label", 
    aes(label = sprintf("%.2f", ..y..)), 
    vjust = 0.5, 
    hjust = 0.5,
    fontface = "bold", 
    size = 5.5,
    color = "black",
    fill = "white"  
  ) +
  theme(legend.position = "none") + theme(text = element_text(size=20)) +
  theme(axis.title.x = element_text(margin = margin(t = 35)), # Increase top margin for x-axis title
        axis.title.y = element_text(margin = margin(r = 35))) +
  scale_color_manual(values=c("#F8766D","#00BA38","#619CFF"))+
  scale_y_continuous(limits = c(-2.5, 5.5), breaks = seq(-2, 5, by = 1))


ggsave("p_discourse.png",p_discourse,  width = 7, height =5)

  
# Drop pronouns for analysis
df_analysis <- df %>% dplyr::select(Subject, Nativeness, Competition_Score, Saliency_Score, Previous_Mention_Score, ref_type, ref_type2, Discourse, Accessibility, Narrative)  %>% subset(ref_type2 != "PRO")
df_analysis$ref_type2 = droplevels(df_analysis$ref_type2)



# Encode vector types
df_analysis$Narrative %<>% as.integer() 
df_analysis$Accessibility %<>% as.numeric() # for stats
df_analysis$Participant %<>% as.integer()
df_analysis$ref_type2 %<>% as.factor() 
df_analysis$Discourse %<>% as.factor()
df_analysis$interaction %<>% as.factor()
df_analysis$Nativeness %<>% as.factor() 

# Contrast coding
df_analysis$Nativeness <- factor(df_analysis$Nativeness, levels = c("Native","Late"))
df_analysis$Discourse <- factor(df_analysis$Discourse, levels = c("Introduction","Maintenance","Re-introduction"))
df_analysis$ref_type2 <- factor(df_analysis$ref_type2, levels = c("CL","NOM","CA","VERB"))

contrasts(df_analysis$Nativeness) <- contr.sum(2)
contrasts(df_analysis$ref_type2) <- contr.sum(4)
contrasts(df_analysis$Discourse) <- contr.sum(3)

m1 <- lmer((Accessibility) ~ ref_type2 + (1|Subject) + (1|Narrative), data = df_analysis)
m2 <- lmer(Accessibility ~ Nativeness + ref_type2 + (1|Subject) + (1|Narrative), data = df_analysis)
m3 <- lmer(Accessibility ~ Nativeness*ref_type2 + (1|Subject) + (1|Narrative), data = df_analysis)
m4 <- lmer((Accessibility) ~ Discourse + (1|Subject) + (1|Narrative), data = df_analysis)
m5 <- lmer((Accessibility) ~ Nativeness + Discourse  + (1|Subject) + (1|Narrative), data = df_analysis)
m6 <- lmer((Accessibility) ~ Discourse*Nativeness + (1|Subject) + (1|Narrative), data = df_analysis)
m7 <- lmer((Accessibility) ~ Discourse*re_type2 + Nativeness + (1|Subject) + (1|Narrative), data = df_analysis)
m8 <- lmer((Accessibility) ~ Discourse*re_type2*Nativeness + (1|Subject) + (1|Narrative), data = df_analysis)
m9 <- lmer((Accessibility) ~ Discourse*Nativeness + re_type2 + (1|Subject) + (1|Narrative), data = df_analysis)
m10 <- lmer((Accessibility) ~ re_type2*Nativeness + Discourse + (1|Subject) + (1|Narrative), data = df_analysis)


# Outlier analysis

# Plotting others vs. Participant 34

# Frequency of each GTR1 level for participant 34
data_34 <- df %>%
  filter(Subject == 34) %>%
  count(ref_type) %>%
  mutate(Group = "Participant 34")


# Frequency of each GTR1 level for all other participants (excluding 34 and 24)
data_others <- df %>%
  filter(!Subject %in% c(34)) %>%
  count(ref_type) %>%
  mutate(Group = "Others")

# Combine the data
combined_data <- bind_rows(data_34, data_others)
combined_data <- combined_data %>%
  group_by(Group) %>%
  mutate(Frequency = n / sum(n))


# Calculate percentage for labeling
combined_data$Percentage <- combined_data$Frequency * 100

# Reorder factor
combined_data$ref_type <- factor(combined_data$ref_type, levels = c("NOM", "TRACE", "PRO", "STATIC", "WECL", "CA", "BPCL", "AGR", "PLAIN"))

others_vs_34_plot <- ggplot(combined_data, aes(x = ref_type, y = Percentage, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  scale_color_manual(name="Participant", labels = c("Other Participants (average)","Participant 24", "Participant 34"), values = c("#F8766D", "#00BA38", "#619CFF")) + ggthemes::theme_tufte()+ 
  labs(
    x = "RE Type",
    y = "Proportion (%)",
  ) +
  theme_tufte() +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(
    aes(label = sprintf("%.1f%%", Percentage), y = Frequency + 0.02),
    position = position_dodge(width = 0.9),
    size = 3.5,
    vjust = -0.5
  ) +
  facet_wrap(~ Group, ncol = 1) +  # Adjusted to include three facets
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 13)
  )

print(plot)

# Encode vector types
df$Narrative %<>% as.integer() 
#df$Accessibility %<>% as.numeric() # for plotting purposes
df$Accessibility %<>% as.integer() # for stats
df$Subject %<>% as.integer()
df$GTR1 %<>% as.factor() 
df$Nativeness %<>% as.factor()
df$Discourse %<>% as.factor()
df$ref_type2 %<>% as.factor()

# Run a binary diagnostic model for outlier analysis # CA - NONCA
# çıkarmak yerine dahil et sonra bahset

binary_model <- glmer(ref_type3 ~ Nativeness + Discourse + (1|Subject) + (1|Narrative), family=binomial, data=df_analysis)

tab_model(binary_model, dv.labels = "Nominal Usage", pred.labels = c("Intercept", "Group [Native]", "Discourse [Introduction]", "Discourse [Maintenance]"))

# Influence analysis
influence_model <- influence(m3, "Subject")
dist <- cooks.distance(influence_model)

# Convert Cook's distances to a data frame
cooks_df <- data.frame(Participants = rownames(dist), CooksDistance = dist[,1])

# Order the data frame by Cook's distance for better visualization
cooks_df %<>% arrange(CooksDistance) 

# Creating the Cook's distance plot
library(ggplot2)

# Create the cook distance plot
cook_distance_plot <- ggplot(cooks_df, aes(x = CooksDistance, y = factor(Participants, levels = as.character(cooks_df$Participants)))) +
  geom_point(aes(color = ifelse(CooksDistance > 0.20312, "Above 0.5", "0.5 or Below")), size = 3) +  # Conditionally color the dots
  scale_color_manual(values = c("Above 0.5" = "#F8766D", "0.5 or Below" = "#00BA38")) +  # Specify the colors for each condition
  labs(    x = "Cook's Distance",
           y = "Participant",
           color = "Distance"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red", size = 1) +  # Add a vertical dashed line at Cook's Distance = 0.5
  ggthemes::theme_tufte() +
  theme(
    axis.text.y = element_text(size = 14),  # Adjust the size for readability
    axis.title = element_text(size = 14),
    title = element_text(size = 14),
    legend.position = "right"
  ) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_text(size = 13),
    legend.position = "bottom",
    plot.margin = margin(1, 1, 1, 1, "cm"),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 15),
    strip.text = element_text(size = 14))

ggsave("cooks_distance_colored_plot.png", cook_distance_plot, width = 7, height = 6.75)

cook_distance_plot + geom_segment(aes(x = 0, xend = CooksDistance, y = factor(Participants, levels = as.character(cooks_df$Participants)), yend = factor(Participants, levels = as.character(cooks_df$Participants))), color = "grey")

binary_model_without_34 <- exclude.influence(binary_model, "Subject", c("34"))

# Model comparison
anova(binary_model_without_34, binary_model)
tab_model(binary_model_without_34, dv.labels = "Nominal Usage", pred.labels = c("Intercept", "Group [Native]", "Discourse [Introduction]", "Discourse [Maintenance]"))
