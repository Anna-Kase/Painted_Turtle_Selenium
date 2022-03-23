

library(brms)
library(tidyverse)
library(janitor)
setwd("~/Turtle_SE")


se_data <- read.csv("Turtle_Se_Data.csv", header=T)
se_data

t_w_data <- read.csv("T_W_Se.csv", header=T)

stuff_to_plot <- t_w_data %>% 
  group_by(Site, Sample_Type, Site_Type) %>% 
  summarise(
    mean = mean(Se),
    sd = sd(Se),
    n = length(Se),
    se = (sd/sqrt(n)),
    min = (mean-se),
    max = (mean+se)
  )

#Adding mean water [Se] to turtle dataset

water_descriptive_stats <- stuff_to_plot %>% 
  subset(Sample_Type == "Water")

se_data$Water_Se <- ifelse(se_data$Site == "Acheson", 0.278, ifelse(se_data$Site == "Beck", 0.082, ifelse(se_data$Site == "Buffalo_Lake", 0.142, ifelse(se_data$Site == "Habeger", 1.41, ifelse(se_data$Site == "Lost_Lake", 0.18, ifelse(se_data$Site == "Petri", 0.312, ifelse(se_data$Site == "Pettigrew", 0.071, ifelse(se_data$Site == "Volker", 0.808, "FUCK"))))))) )

se_data$Water_Se <- as.numeric(se_data$Water_Se)

se_data$Log_Se <- log(se_data$Se)
se_data$Log_Mass <- log(se_data$Mass)
se_data$Log_Water_Se <- log(se_data$Water_Se)

og_stuff_to_plot <- stuff_to_plot

ggplot(og_stuff_to_plot, aes(x=Site, y=mean)) +
  geom_errorbar(
    aes(ymin=min, ymax=max, color=Sample_Type),
    position = position_dodge(0.3), width=0.2, size=1
  ) +
  geom_point(aes(color=Sample_Type, shape=Site_Type), position = position_dodge(0.3), size=2) +
  scale_color_manual(values = c("aquamarine4", "royalblue4")) +
  labs(x="Wetland Site", y="Selenium Concentration (??g/L)")+
  scale_x_discrete(labels=c("Acheson", "Beck", "Buffalo Lake", "Habeger", "Lost Lake", "Petri", "Pettigrew", "Volker"))+
  guides(col=guide_legend("Sample Type"), shape=guide_legend("Site Type"))

#water vs blood with site type differentiation
#Water_And_Blood_By_Site_With_Type_Differentiation_and_Dividing_Line
stuff_to_plot %>% 
  mutate(Site = fct_relevel(Site, "Beck", "Buffalo_Lake", "Lost_Lake", "Pettigrew", "Acheson", "Habeger", "Petri", "Volker")) %>% 
  ggplot(aes(x=Site, y=mean)) +
  geom_errorbar(
    aes(ymin=min, ymax=max, color=Sample_Type),
    position = position_dodge(0.3), width=0.2, size=1) +
  geom_point(aes(color=Sample_Type, shape=Site_Type), position = position_dodge(0.3), size=2.25) +
  labs(x="Wetland Site", y="Selenium Concentration (??g/L)")+
  scale_x_discrete(labels=c("Beck", "Buffalo Lake", "Lost Lake", "Pettigrew", "Acheson", "Habeger", "Petri", "Volker"))+
  guides(col=guide_legend("Selenium Sample Type"), shape=guide_legend("Wetland Site Type"))+
  theme(text=element_text(size=21))+
  theme(axis.text.x.bottom = element_text(angle =45, vjust = 0.75 )) +
  scale_color_manual(name = "Sample Type", labels = c("Turtle Blood", "Water"), values = c("aquamarine4", "royalblue4")) +
  scale_shape_discrete(name = "Wetland Site Type", labels = c("Control Wetland", "Tile Wetland"))+
  geom_vline(xintercept = 4.5, linetype="solid", color="black", size = 1 )



#Bayesian models using log transformed data in a normal distirbution

B_water <- brm(formula = Log_Se ~ Log_Water_Se,
               family = gaussian(),
               data = se_data,
               prior = c(prior(normal(0,2), class = Intercept),
                         prior(normal(0,1), class = b),
                         prior(normal(0,1), class = sigma)),
               chains = 4, cores = 4)
summary(B_water)
water_WAIC <- waic(B_water)
water_WAIC

#Population-Level Effects: 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept        1.43      0.34     0.76     2.09 1.00     3169     2325
#Log_Water_Se     1.09      0.21     0.68     1.49 1.00     2885     2453

#Family Specific Parameters: 
#         Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#sigma     1.09      0.14     0.86     1.39 1.00     3541     2730

#WAIC: 102.0 

exp(1.09)

#Estimated slope is 2.97 ug/g - With every unit increase in the 
#water selenium concentration, we expect a 2.97 ug/g increase in turtle
#blood selenium concentration.

#Log_Turtle_Se_by_Log_Water_Se_with_Legend
ggplot(se_data, aes(Log_Water_Se, Log_Se))+
  geom_point(aes(col=Site_Type, size=4))+
  geom_smooth(method="lm", col="black")+
  guides(col=guide_legend(title="Wetland Site Type"), size=FALSE)+
  labs(x="Log Water Selenium Concentration (??g/L)", y="Log Turtle Blood Selenium Concentration (??g/g)")+
  theme(text=element_text(size=21))+
  scale_color_manual(name = "Wetland Site Type", labels = c("Control Wetland", "Tile Wetland"), values = c("mediumorchid4", "coral3"))


B_mass <- brm(formula = Log_Se ~ Log_Mass,
              family = gaussian(),
              data = se_data,
              prior = c(prior(normal(0,2), class = Intercept),
                        prior(normal(0,1), class = b),
                        prior(normal(0,1), class = sigma)),
              chains = 4, cores = 4)
summary(B_mass)
mass_WAIC <- waic(B_mass)
mass_WAIC

#Population-Level Effects: 
#           Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#Intercept    -6.80      2.10   -10.88    -2.58 1.00     3587     2794
#Log_Mass      1.18      0.36     0.46     1.88 1.00     3572     2675

#Family Specific Parameters: 
#        Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
#sigma     1.28      0.16     1.01     1.62 1.00     3213     2758

#WAIC: 110.1

exp(1.18)

#Estimated slope is 3.25 ug/g - With every unit increase in the 
#turtle mass, we expect a 3.25 ug/g increase in turtle
#blood selenium concentration.

#Log_Turtle_Se_by_Log_Turtle_Mass_with_Legend
ggplot(se_data, aes(Log_Mass, Log_Se))+
  geom_point(aes(col=Site_Type, size=4))+
  geom_smooth(method="lm", col="black")+
  scale_color_manual(name = "Wetland Site Type", labels = c("Control Wetland", "Tile Wetland"), values = c("mediumorchid4", "coral3"))+
  guides(col=guide_legend(title="Wetland Site Type"), size=FALSE)+
  labs(x="Log Turtle Mass (g)", y="Log Turtle Blood Selenium Concentration (??g/g)")+
  theme(text=element_text(size=21))
  

B_site <- brm(formula = Log_Se ~ Site_Type,
              family = gaussian(),
              data=se_data,
              prior=c(prior(normal(0,2), class=Intercept),
                      prior(normal(0,1), class=b),
                      prior(normal(0,1), class = sigma)),
              chains = 4, cores=4)

summary(B_site)
site_WAIC <- waic(B_site)
site_WAIC

#posterior extraction function
conditional_posts_fitted <- function(fit, effects, conditions = NULL){
  list_of_data <- conditional_effects(fit, effects, conditions)[[1]]
  new_names <- list_of_data %>%
    select(-names(fit$data[1])) %>%
    select(-cond__, -effect1__, -estimate__, -se__, -lower__, -upper__) %>%
    remove_empty("cols")
  
  as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NA, summary = FALSE))) %>%
    cbind(new_names) %>%
    pivot_longer(cols = contains("V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))
}

#extracting posterior
B_site_post <- conditional_posts_fitted(B_site, effects = "Site_Type")

View(B_site_post)


#subsetting
tile_log <- subset(B_site_post, Site_Type == "Tile")
View(tile_log)
control_log <- subset(B_site_post, Site_Type == "Control")

tile_join_log <- tile_log %>% 
  mutate(tile_value = value) %>%
  select(tile_value, iter)

control_join_log <- control_log %>% 
  mutate(control_value = value) %>% 
  select(control_value, iter)

B_full_post <- left_join(tile_join_log, control_join_log)

B_full_post %>% 
  mutate(diff = tile_value - control_value) %>% 
  mutate(diff_binary = ifelse(diff>0, 1, 0)) %>% 
  summarise(prob = sum(diff_binary)/4000)

#The probability that the tile Se is greater than the control Se is 99.8%

#Bayesian_Log_Turtle_Se_by_Wetland_Type_With_Legend
ggplot(B_site_post, aes(Site_Type, value, fill=Site_Type))+
  geom_violin()+
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_fill_manual(name = "Wetland Site Type", labels = c("Control Wetland", "Tile Wetland"),values = c("mediumorchid4", "coral3"))+
  guides(fill=guide_legend(title="Wetland Site Type"))+
  labs(x="Wetland Site Type", y="Log Turtle Blood Selenium Concentration (??g/g)")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text=element_text(size=21)) +
  scale_x_discrete(labels=c("Control Wetland", "Tile Wetland"))

#Bayesian_Log_Turtle_Se_by_Wetland_Type_No_Legend
ggplot(B_site_post, aes(Site_Type, value, fill=Site_Type))+
  geom_violin()+
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_fill_manual(name = "Wetland Site Type", labels = c("Control Wetland", "Tile Wetland"),values = c("mediumorchid4", "coral3"))+
  guides(fill=guide_legend(title="Wetland Site Type"))+
  labs(x="Wetland Site Type", y="Log Turtle Blood Selenium Concentration (??g/g)")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text=element_text(size=21), legend.position = "none")+
  scale_x_discrete(labels=c("Control Wetland", "Tile Wetland"))




B_water <- brm(formula = Log_Water_Se ~ Site_Type,
              family = gaussian(),
              data=se_data,
              prior=c(prior(normal(0,2), class=Intercept),
                      prior(normal(0,1), class=b),
                      prior(normal(0,1), class = sigma)),
              chains = 4, cores=4)

summary(B_water)
water_WAIC <- waic(B_water)
water_WAIC

#posterior extraction function
conditional_posts_fitted <- function(fit, effects, conditions = NULL){
  list_of_data <- conditional_effects(fit, effects, conditions)[[1]]
  new_names <- list_of_data %>%
    select(-names(fit$data[1])) %>%
    select(-cond__, -effect1__, -estimate__, -se__, -lower__, -upper__) %>%
    remove_empty("cols")
  
  as_tibble(t(fitted(fit, newdata = list_of_data, re_formula = NA, summary = FALSE))) %>%
    cbind(new_names) %>%
    pivot_longer(cols = contains("V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))
}

#extracting posterior
B_water_post <- conditional_posts_fitted(B_water, effects = "Site_Type")

View(B_water_post)


#subsetting
tile_water <- subset(B_water_post, Site_Type == "Tile")

control_water <- subset(B_water_post, Site_Type == "Control")

tile_join_water <- tile_water %>% 
  mutate(tile_value = value) %>%
  select(tile_value, iter)


control_join_water <- control_water %>% 
  mutate(control_value = value) %>% 
  select(control_value, iter)

B_post_water <- left_join(tile_join_water, control_join_water)

B_post_water %>% 
  mutate(diff = tile_value - control_value) %>% 
  mutate(diff_binary = ifelse(diff>0, 1, 0)) %>% 
  summarise(prob = sum(diff_binary)/4000)

#The probability that the tile Se is greater than the control Se is 100%

#Bayesian_Log_Water_Se_by_Wetland_Type_With_Legend
ggplot(B_water_post, aes(Site_Type, value, fill=Site_Type))+
  geom_violin()+
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_fill_manual(name = "Wetland Site Type", labels = c("Control Wetland", "Tile Wetland"),values = c("mediumorchid4", "coral3"))+
  guides(fill=guide_legend(title="Wetland Site Type"))+
  labs(x="Wetland Site Type", y="Log Water Selenium Concentration (??g/L)")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text=element_text(size=21))+
  scale_x_discrete(labels=c("Control Wetland", "Tile Wetland"))

#plot with no legend because we really don't need a legend here - Bayesian_Log_Water_Se_by_Wetland_Type_No_Legend
ggplot(B_water_post, aes(Site_Type, value, fill=Site_Type))+
  geom_violin()+
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_fill_manual(values = c("mediumorchid4", "coral3"))+
  labs(x="Wetland Site Type", y="Log Water Selenium Concentration (??g/L)")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text=element_text(size=21), legend.position = "none") +
  scale_x_discrete(labels=c("Control Wetland", "Tile Wetland"))



#Removing the log transformation
no_log_water_post <- B_water_post
no_log_water_post$value <- exp(no_log_water_post$value)


ggplot(no_log_water_post, aes(Site_Type, value, fill=Site_Type))+
  geom_violin()+
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_fill_manual(values = c("mediumorchid4", "coral3"))+
  guides(fill=guide_legend(title="Site Type"))+
  labs(x="Site Type", y="Water Selenium Concentration (??g/L)")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text=element_text(size=21))

no_log_blood_post <- B_site_post
no_log_blood_post$value <- exp(no_log_blood_post$value)

ggplot(no_log_blood_post, aes(Site_Type, value, fill=Site_Type))+
  geom_violin()+
  geom_boxplot(width=0.1, color="black", alpha=0.2)+
  scale_fill_manual(values = c("mediumorchid4", "coral3"))+
  guides(fill=guide_legend(title="Site Type"))+
  labs(x="Site Type", y="Turtle Blood Selenium Concentration (??g/g)")+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(text=element_text(size=21))
