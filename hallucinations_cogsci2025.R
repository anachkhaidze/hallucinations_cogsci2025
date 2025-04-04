# load libraries
suppressMessages(library(tidyverse))
library(ggplot2)
library(sjPlot)
library(patchwork)
library(mediation)


df_responses <- read.csv("hallucinations_preprocessed_assigned_ls_norms.csv")

df_responses <- df_responses %>%
  mutate(
    head_strength_scaled = as.numeric(scale(head_strength)),
    hand_strength_scaled = as.numeric(scale(hand_strength)),
    mouth_strength_scaled = as.numeric(scale(mouth_strength)),
    torso_strength_scaled = as.numeric(scale(torso_strength)),
    foot_strength_scaled = as.numeric(scale(foot_strength)),
    description_length_scaled = as.numeric(scale(description_length))
  )

df_responses <- df_responses %>%
  mutate(
    visual_strength_scaled = as.numeric(scale(visual_strength)),
    auditory_strength_scaled = as.numeric(scale(auditory_strength)),
    gustatory_strength_scaled = as.numeric(scale(gustatory_strength)),
    haptic_strength_scaled = as.numeric(scale(haptic_strength)),
    olfactory_strength_scaled = as.numeric(scale(olfactory_strength)),
    interoceptive_strength_scaled = as.numeric(scale(interoceptive_strength)),
    description_length_scaled = as.numeric(scale(description_length))  # Also scale the mediator
  )

df_responses <- df_responses %>%
  mutate(
    perceptual_strength_scaled = as.numeric(scale(perceptual_strength)),
    action_strength_scaled = as.numeric(scale(action_strength)),
    description_length_scaled = as.numeric(scale(description_length))  # Scale the mediator
  )



# Select relevant predictors
predictors <- df_responses[, c("visual_strength", "interoceptive_strength", "auditory_strength", 
                               "gustatory_strength", "haptic_strength", "olfactory_strength",
                               "head_strength", "hand_strength", "mouth_strength", 
                               "torso_strength", "foot_strength")]

# Convert to long format for ggplot2
df_long <- predictors %>%
  tidyr::gather(key = "Predictor", value = "Value")

# Create faceted histogram of LS dimensions
ggplot(df_long, aes(x = Value)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
  facet_wrap(~ Predictor, scales = "free_x") +  # Free scales allow different ranges
  theme_minimal() +
  labs(title = "Distribution of LS Dimensions",
       x = "Value",
       y = "Count")

# Look at predictor correlations
round(cor(df_responses[, c("head_strength", "hand_strength", "mouth_strength", 
                           "torso_strength", "foot_strength")]), 2)

round(cor(df_responses[, c("interoceptive_strength", "visual_strength", "auditory_strength", 
                           "gustatory_strength", "haptic_strength", "olfactory_strength")]), 2)

# Check variances
apply(df_responses[, c("visual_strength", "auditory_strength", "gustatory_strength", 
                       "haptic_strength", "olfactory_strength", "interoceptive_strength")], 2, var)


apply(df_responses[, c("head_strength", "hand_strength", "foot_strength", 
                       "mouth_strength", "torso_strength")], 2, var)


############################# Motor Model ############################# 

model_motor <- glm(visual_vividness ~ 
                   scale(head_strength) + scale(hand_strength) + scale(mouth_strength) + 
                   scale(torso_strength) + scale(foot_strength),
                   data = df_responses)

coeffs_motor <- as.data.frame(summary(model_motor)$coefficients)
coeffs_motor$Predictor <- rownames(coeffs_motor)
ggplot(coeffs_motor[-1,], aes(x = reorder(Predictor, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Standardized Coefficients (Effect Sizes)", x = "Predictor", y = "Standardized Estimate")

summary(model_motor)


# Fit Mediator Model (Predicting description_length)
model_mediator_motor <- glm(description_length_scaled ~ 
                              head_strength_scaled + hand_strength_scaled + 
                              mouth_strength_scaled + torso_strength_scaled + 
                              foot_strength_scaled, 
                            data = df_responses)

# Fit Outcome Model (Predicting visual_vividness, controlling for description_length)
model_outcome_motor <- glm(visual_vividness ~ 
                             head_strength_scaled + hand_strength_scaled + 
                             mouth_strength_scaled + torso_strength_scaled + 
                             foot_strength_scaled + 
                             description_length_scaled, 
                           data = df_responses)

summary(model_outcome_motor)


# Set number of bootstrapping simulations
num_sims <- 5000  # Higher = more accuracy, but takes longer

# Mediation for head strength
mediation_motor_head <- mediate(model_mediator_motor, model_outcome_motor, 
                                treat = "head_strength_scaled", mediator = "description_length_scaled", 
                                boot = TRUE, sims = num_sims)

# Mediation for hand strength
mediation_motor_hand <- mediate(model_mediator_motor, model_outcome_motor, 
                                treat = "hand_strength_scaled", mediator = "description_length_scaled", 
                                boot = TRUE, sims = num_sims)

# Mediation for mouth strength
mediation_motor_mouth <- mediate(model_mediator_motor, model_outcome_motor, 
                                 treat = "mouth_strength_scaled", mediator = "description_length_scaled", 
                                 boot = TRUE, sims = num_sims)

# Mediation for torso strength
mediation_motor_torso <- mediate(model_mediator_motor, model_outcome_motor, 
                                 treat = "torso_strength_scaled", mediator = "description_length_scaled", 
                                 boot = TRUE, sims = num_sims)

# Mediation for foot strength
mediation_motor_foot <- mediate(model_mediator_motor, model_outcome_motor, 
                                treat = "foot_strength_scaled", mediator = "description_length_scaled", 
                                boot = TRUE, sims = num_sims)

# Print mediation results
summary(mediation_motor_head)
summary(mediation_motor_hand)
summary(mediation_motor_mouth)
summary(mediation_motor_torso)
summary(mediation_motor_foot)

# Store results in a data frame for easier comparison
mediation_results <- data.frame(
  Predictor = c("Head Strength", "Hand Strength", "Mouth Strength", "Torso Strength", "Foot Strength"),
  ACME = c(mediation_motor_head$d0, mediation_motor_hand$d0, mediation_motor_mouth$d0, 
           mediation_motor_torso$d0, mediation_motor_foot$d0),  # Indirect effect (mediation)
  ADE = c(mediation_motor_head$z0, mediation_motor_hand$z0, mediation_motor_mouth$z0, 
          mediation_motor_torso$z0, mediation_motor_foot$z0),  # Direct effect
  TotalEffect = c(mediation_motor_head$tau.coef, mediation_motor_hand$tau.coef, mediation_motor_mouth$tau.coef, 
                  mediation_motor_torso$tau.coef, mediation_motor_foot$tau.coef),  # Total effect
  PropMediated = c(mediation_motor_head$n0, mediation_motor_hand$n0, mediation_motor_mouth$n0, 
                   mediation_motor_torso$n0, mediation_motor_foot$n0)  # % of effect mediated
)

# Print summary table
print(mediation_results)

# Sensory Model: Adjusted Effect Sizes (ADE)
adjusted_coeffs_sensory <- data.frame(
  Predictor = c("Visual Strength", "Auditory Strength", "Haptic Strength", "Interoceptive Strength"),
  Adjusted_Estimate = c(mediation_sensory_visual$z0, 
                        mediation_sensory_auditory$z0, 
                        mediation_sensory_haptic$z0, 
                        mediation_sensory_interoceptive$z0)
)

# Motor Model: Adjusted Effect Sizes (ADE)
adjusted_coeffs_motor <- data.frame(
  Predictor = c("Head Strength", "Hand Strength", "Mouth Strength", "Torso Strength", "Foot Strength"),
  Adjusted_Estimate = c(mediation_motor_head$z0, 
                        mediation_motor_hand$z0, 
                        mediation_motor_mouth$z0, 
                        mediation_motor_torso$z0, 
                        mediation_motor_foot$z0)
)



ggplot(adjusted_coeffs_motor, aes(x = reorder(Predictor, Adjusted_Estimate), y = Adjusted_Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Adjusted Standardized Coefficients (Motor Model, After Mediation)", 
       x = "Predictor", y = "Adjusted Standardized Estimate") +
  theme_minimal()


##################################################

############################# Sensory Model ############################# 

model_sensory <- glm(visual_vividness ~ 
                        visual_strength_scaled + auditory_strength_scaled + 
                        haptic_strength_scaled + interoceptive_strength_scaled, 
                      data = df_responses)
summary(model_sensory)

coeffs <- as.data.frame(summary(model_sensory)$coefficients)
coeffs$Predictor <- rownames(coeffs)
ggplot(coeffs[-1,], aes(x = reorder(Predictor, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Standardized Coefficients (Effect Sizes)", x = "Predictor", y = "Standardized Estimate")

## Mediation analysis: description length
# Fit Mediator Model (Predicting description_length)
model_mediator_sensory <- glm(description_length_scaled ~ 
                                visual_strength_scaled + auditory_strength_scaled + 
                                gustatory_strength_scaled + haptic_strength_scaled + 
                                olfactory_strength_scaled + interoceptive_strength_scaled, 
                              data = df_responses)



# Fit Outcome Model (Predicting visual_vividness, controlling for description_length)
model_outcome_sensory <- glm(visual_vividness ~ 
                               visual_strength_scaled + auditory_strength_scaled + 
                               gustatory_strength_scaled + haptic_strength_scaled + 
                               olfactory_strength_scaled + interoceptive_strength_scaled + 
                               description_length_scaled, 
                             data = df_responses)

summary(model_outcome_sensory)

###############

# Fit GLM model
model_outcome_sensory <- glm(visual_vividness ~ 
                               scale(visual_strength) + scale(auditory_strength) + 
                               scale(gustatory_strength) + scale(haptic_strength) + 
                               scale(olfactory_strength) + scale(interoceptive_strength) + 
                               scale(description_length), 
                              data = df_responses)

summary(model_outcome_sensory)

# Extract coefficients, confidence intervals, and p-values
coef_df <- as.data.frame(summary(model_outcome_sensory)$coefficients)
coef_df$Predictor <- rownames(coef_df)  # Add predictor names
colnames(coef_df) <- c("Estimate", "Std_Error", "Z_value", "P_value", "Predictor")


# Extract confidence intervals
conf_int <- confint(model_outcome_sensory)
coef_df$Lower_CI <- conf_int[,1]
coef_df$Upper_CI <- conf_int[,2]
coef_df


# Filter only visual_strength and interoceptive_strength
coef_df <- subset(coef_df, Predictor %in% c("scale(visual_strength)", "scale(interoceptive_strength)"))


# Determine significance and assign colors
coef_df$Color <- ifelse(coef_df$P_value < 0.05 & coef_df$Estimate > 0, "#c5c643", 
                        ifelse(coef_df$P_value < 0.05 & coef_df$Estimate < 0, "#FF7043", "#666666"))

library(stringr)

# Wrap predictor labels to a maximum width (adjust width as needed)
wrapped_labels <- c("scale(visual_strength)" = str_wrap("Perceptual Strength", width = 10),
                    "scale(interoceptive_strength)" = str_wrap("Interoceptive Strength", width = 10))

p <- ggplot(coef_df, aes(x = Estimate, y = reorder(Predictor, Estimate), color = Color)) +
  geom_point(size = 7) +  # Plot points
  geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0, size = 0.5) +  # Add error bars
  scale_color_identity() +  # Use manually assigned colors
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Reference line at 0
  labs(x = "Beta Coefficients with 95% CI", y = NULL) +  
  theme_classic(base_family = "Times New Roman", base_size = 18) +  # Font settings
  theme(legend.position = "none",  
        plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 18, angle = 90, hjust = 0.5, lineheight = 0.75),  # Reduce line spacing
        axis.ticks.y = element_line(size = 0.4),  # Ensure ticks match thinner axis lines
        axis.title.x = element_text(margin = margin(t = 5)),  # Decrease spacing between x-axis label and axis
        axis.line = element_line(size = 0.4))  # Decrease axis line thickness

# Apply wrapped y-axis labels
p <- p + scale_y_discrete(labels = wrapped_labels)


p

# Save the plot
ggsave("forest_plot_mediator_sensory.png", width = 4, height = 4, dpi = 300, units = "in", bg = "transparent")



###############

# Set number of bootstrapping simulations
num_sims <- 5000  # Higher = more accuracy, but takes longer

# Mediation for visual strength
mediation_sensory_visual <- mediate(model_mediator_sensory, model_outcome_sensory, 
                                    treat = "visual_strength_scaled", mediator = "description_length_scaled", 
                                    boot = TRUE, sims = num_sims)

# Mediation for auditory strength
mediation_sensory_auditory <- mediate(model_mediator_sensory, model_outcome_sensory, 
                                      treat = "auditory_strength_scaled", mediator = "description_length_scaled", 
                                      boot = TRUE, sims = num_sims)


# Mediation for haptic strength
mediation_sensory_haptic <- mediate(model_mediator_sensory, model_outcome_sensory, 
                                    treat = "haptic_strength_scaled", mediator = "description_length_scaled", 
                                    boot = TRUE, sims = num_sims)


# Mediation for interoceptive strength
mediation_sensory_interoceptive <- mediate(model_mediator_sensory, model_outcome_sensory, 
                                           treat = "interoceptive_strength_scaled", mediator = "description_length_scaled", 
                                           boot = TRUE, sims = num_sims)

# Print mediation results
summary(mediation_sensory_visual)
summary(mediation_sensory_auditory)
summary(mediation_sensory_haptic)
summary(mediation_sensory_interoceptive)

# # Store results in a data frame for easier comparison
# mediation_results_sensory <- data.frame(
#   Predictor = c("Visual Strength", "Auditory Strength", 
#                 "Haptic Strength", "Interoceptive Strength"),
#   ACME = c(mediation_sensory_visual$d0, mediation_sensory_auditory$d0,  
#            mediation_sensory_haptic$d0, mediation_sensory_interoceptive$d0),  # Indirect effect (mediation)
#   ADE = c(mediation_sensory_visual$z0, mediation_sensory_auditory$z0,
#           mediation_sensory_haptic$z0, mediation_sensory_interoceptive$z0),  # Direct effect
#   TotalEffect = c(mediation_sensory_visual$tau.coef, mediation_sensory_auditory$tau.coef, 
#                   mediation_sensory_haptic$tau.coef, mediation_sensory_interoceptive$tau.coef),  # Total effect
#   PropMediated = c(mediation_sensory_visual$n0, mediation_sensory_auditory$n0,
#                    mediation_sensory_haptic$n0, mediation_sensory_interoceptive$n0)  # % of effect mediated
# )

# Store results in a data frame for easier comparison
mediation_results_sensory <- data.frame(
  Predictor = c("Visual Strength", "Interoceptive Strength"),
  ACME = c(mediation_sensory_visual$d0, mediation_sensory_interoceptive$d0),  # Indirect effect (mediation)
  ADE = c(mediation_sensory_visual$z0, mediation_sensory_interoceptive$z0),  # Direct effect
  TotalEffect = c(mediation_sensory_visual$tau.coef, mediation_sensory_interoceptive$tau.coef),  # Total effect
  PropMediated = c(mediation_sensory_visual$n0, mediation_sensory_interoceptive$n0)  # % of effect mediated
)

# Create a data frame for adjusted effect sizes using ADE (Direct Effect)
# adjusted_coeffs <- data.frame(
#   Predictor = c("Visual Strength", "Auditory Strength", "Haptic Strength", "Interoceptive Strength"),
#   Adjusted_Estimate = c(mediation_sensory_visual$z0, 
#                         mediation_sensory_auditory$z0, 
#                         mediation_sensory_haptic$z0, 
#                         mediation_sensory_interoceptive$z0)
# )

# Create a data frame for adjusted effect sizes using ADE (Direct Effect)
adjusted_coeffs <- data.frame(
  Predictor = c("Visual Strength", "Interoceptive Strength"),
  Adjusted_Estimate = c(mediation_sensory_visual$z0, 
                        mediation_sensory_interoceptive$z0)
)

summary(mediation_sensory_visual)$z0.ci
summary(mediation_sensory_interoceptive)$z0.ci

library(ggplot2)

# Extract confidence intervals for ADE (Direct Effect)
visual_ci <- summary(mediation_sensory_visual)$z0.ci
interoceptive_ci <- summary(mediation_sensory_interoceptive)$z0.ci

# Create a data frame with confidence intervals
adjusted_coeffs <- data.frame(
  Predictor = c("Visual Strength", "Interoceptive Strength"),
  Adjusted_Estimate = c(mediation_sensory_visual$z0, mediation_sensory_interoceptive$z0),
  Lower_CI = c(visual_ci[1], interoceptive_ci[1]),  # Lower bound
  Upper_CI = c(visual_ci[2], interoceptive_ci[2])   # Upper bound
)

# Plot with error bars (confidence intervals)
p <- ggplot(adjusted_coeffs, aes(x = reorder(Predictor, Adjusted_Estimate), y = Adjusted_Estimate, fill = Predictor)) +
  geom_bar(stat = "identity", alpha = 0.7, width = 0.8) +  
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0, color = "black") +  
  scale_fill_manual(values = c("Visual Strength" = "#E8D5B7", "Interoceptive Strength" = "#E8D5B7")) +  
  coord_flip() +  
  labs(title = NULL, 
       x = NULL, 
       y = "Standardized Effect Sizes with 95% CI") +  
  theme_classic(base_family = "Times New Roman", base_size = 24) +  
  theme(legend.position = "none",  
        plot.title = element_text(hjust = 0.5),  
        axis.title.y = element_text(margin = margin(r = 24)),  
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 22, margin = margin(l = 10))) +  # Adjust y-axis spacing
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2)))  # Ensures space between labels

p
# Save with correct dimensions
ggsave("effect_size_plot.png", plot = p, width = 4.3, height = 4.3, dpi = 300, units = "in", bg = "transparent")



# Print summary table
print(mediation_results_sensory)



###################################### Overall Sensorimotor Model ######################

model_overal_sensorimotor <- glm(visual_vividness ~ 
                                 perceptual_strength,
                               data = df_responses)

summary(model_overal_sensorimotor)

# Fit Mediator Model (Predicting description_length)
model_mediator_sensorimotor <- glm(description_length_scaled ~ 
                                     perceptual_strength_scaled, 
                                   data = df_responses)

# Fit Outcome Model (Predicting visual_vividness, controlling for description_length)
model_outcome_sensorimotor <- glm(visual_vividness ~ 
                                    perceptual_strength_scaled + 
                                    description_length_scaled, 
                                  data = df_responses)

summary(model_outcome_sensorimotor)

# Set number of bootstrapping simulations
num_sims <- 5000  # Higher = more accuracy, but takes longer

# Mediation for perceptual strength
mediation_sensorimotor_perceptual <- mediate(model_mediator_sensorimotor, model_outcome_sensorimotor, 
                                             treat = "perceptual_strength_scaled", mediator = "description_length_scaled", 
                                             boot = TRUE, sims = num_sims)

# Mediation for action strength
mediation_sensorimotor_action <- mediate(model_mediator_sensorimotor, model_outcome_sensorimotor, 
                                         treat = "action_strength_scaled", mediator = "description_length_scaled", 
                                         boot = TRUE, sims = num_sims)


# Print mediation results
summary(mediation_sensorimotor_perceptual)
summary(mediation_sensorimotor_action)

# Store results in a data frame for easier comparison
mediation_results_sensorimotor <- data.frame(
  Predictor = c("Perceptual Strength", "Action Strength"),
  ACME = c(mediation_sensorimotor_perceptual$d0, mediation_sensorimotor_action$d0),  # Indirect effect (mediation)
  ADE = c(mediation_sensorimotor_perceptual$z0, mediation_sensorimotor_action$z0),  # Direct effect
  TotalEffect = c(mediation_sensorimotor_perceptual$tau.coef, mediation_sensorimotor_action$tau.coef),  # Total effect
  PropMediated = c(mediation_sensorimotor_perceptual$n0, mediation_sensorimotor_action$n0)  # % of effect mediated
)

# Print summary table
print(mediation_results_sensorimotor)

# Sensorimotor Model: Adjusted Effect Sizes (ADE)
adjusted_coeffs_sensorimotor <- data.frame(
  Predictor = c("Perceptual Strength", "Action Strength"),
  Adjusted_Estimate = c(mediation_sensorimotor_perceptual$z0, 
                        mediation_sensorimotor_action$z0)
)

ggplot(adjusted_coeffs_sensorimotor, aes(x = reorder(Predictor, Adjusted_Estimate), y = Adjusted_Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Adjusted Standardized Coefficients (Sensorimotor Model, After Mediation)", 
       x = "Predictor", y = "Adjusted Standardized Estimate") +
  theme_minimal()


################################### Visual and Interoceptive Model #############

###################################### Overall Sensorimotor Model ######################

model_visual_interoceptive <- glm(visual_vividness ~ 
                                   visual_strength_scaled + interoceptive_strength_scaled,
                                 data = df_responses)

model_visual_interoceptive <- glm(visual_vividness ~ 
                                    description_length,
                                  data = df_responses)

summary(model_visual_interoceptive)

# Fit Mediator Model (Predicting description_length)
model_mediator_visual_interoceptive <- glm(description_length_scaled ~ 
                                             visual_strength_scaled + interoceptive_strength_scaled, 
                                         data = df_responses)

# Fit Outcome Model (Predicting visual_vividness, controlling for description_length)
model_outcome_visual_interoceptive <- glm(visual_vividness ~ 
                                            visual_strength_scaled + interoceptive_strength_scaled + 
                                            description_length_scaled, 
                                          data = df_responses)

# Set number of bootstrapping simulations
num_sims <- 5000  # Higher = more accuracy, but takes longer

# Mediation for perceptual strength
mediation_visual <- mediate(model_mediator_visual_interoceptive, model_outcome_visual_interoceptive, 
                                             treat = "visual_strength_scaled", mediator = "description_length_scaled", 
                                             boot = TRUE, sims = num_sims)

# Mediation for action strength
mediation_interoceptive <- mediate(model_mediator_visual_interoceptive, model_outcome_visual_interoceptive, 
                                         treat = "interoceptive_strength_scaled", mediator = "description_length_scaled", 
                                         boot = TRUE, sims = num_sims)


# Print mediation results
summary(mediation_visual)
summary(mediation_interoceptive)

# Store results in a data frame for easier comparison
mediation_results_visual_interoceptive <- data.frame(
  Predictor = c("Visual Strength", "Interoceptive Strength"),
  ACME = c(mediation_visual$d0, mediation_interoceptive$d0),  # Indirect effect (mediation)
  ADE = c(mediation_visual$z0, mediation_interoceptive$z0),  # Direct effect
  TotalEffect = c(mediation_visual$tau.coef, mediation_interoceptive$tau.coef),  # Total effect
  PropMediated = c(mediation_visual$n0, mediation_interoceptive$n0)  # % of effect mediated
)

# Print summary table
print(mediation_results_visual_interoceptive)

# Sensorimotor Model: Adjusted Effect Sizes (ADE)
adjusted_coeffs_visual_interoception <- data.frame(
  Predictor = c("Perceptual Strength", "Action Strength"),
  Adjusted_Estimate = c(mediation_visual$z0, 
                        mediation_interoceptive$z0)
)

ggplot(adjusted_coeffs_visual_interoception, aes(x = reorder(Predictor, Adjusted_Estimate), y = Adjusted_Estimate)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Adjusted Standardized Coefficients (Visual / Interoception Model, After Mediation)", 
       x = "Predictor", y = "Adjusted Standardized Estimate") +
  theme_minimal()

#########################################

library(boot)
cv_glm <- cv.glm(df_responses, model_simple, K = 10)
cv_glm$delta


library(sjPlot)
library(ggplot2)
library(patchwork)

# Generate individual plots with specific customizations
p1 <- plot_model(model_motor, type = "pred", terms = "head_strength") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "#53b4a3", alpha = 0.5) +
  geom_line(aes(y = predicted), col = "#53b4a3") +
  scale_color_manual(values = "#53b4a3") +
  theme_linedraw(base_family = "Georgia") +
  theme(axis.title = element_text(size = 1),
  # theme(axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(size = 18),
        plot.title = element_blank(),
        legend.position = "none") +
  labs(x = "Head", y = "Visual Vividness")
p1

p2 <- plot_model(model, type = "pred", terms = "hand_strength") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "#53b4a3", alpha = 0.5) +
  geom_line(aes(y = predicted), col = "#53b4a3") +
  theme_linedraw(base_family = "Georgia") +
  theme(axis.title = element_text(size = 1),
  # theme(axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(size = 18),
        plot.title = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank()) +  # Remove y-axis ticks
  labs(x = "Hand")

p3 <- plot_model(model, type = "pred", terms = "interoceptive_strength") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "#ff1f1b", alpha = 0.5) +
  geom_line(aes(y = predicted), col = "#ff1f1b") +
  theme_linedraw(base_family = "Georgia") +
  theme(axis.title = element_text(size = 1),
  # theme(axis.title = element_text(face = "bold", size = 22),
        axis.text = element_text(size = 18),
        plot.title = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),  # Remove y-axis title
        axis.text.y = element_blank(),   # Remove y-axis text
        axis.ticks.y = element_blank()) +  # Remove y-axis ticks
  labs(x = "Interoceptive")

# Combine the plots side by side
combined_plot <- p1 + p2 + p3 + 
  plot_layout(ncol = 3)  # Set layout to have three columns

# Print the combined plot
print(combined_plot)

# Save the combined plot with specific dimensions
ggsave("head_hand_interoception_small.png", combined_plot, width = 9, height = 4.5)

