

#load packages
library(tidyverse)
library(janitor)
library(brms)
library(tidybayes)

#load dataset
se <- read_csv("~/Turtle_SE/T_W_Se.csv")

#sub group just turtle [se]
se_turtle <- filter(se, Sample_Type == "Turtle")

#model - Se ~ Site_Type
N = 100  # number of simulations (change as needed)

# simulate priors
priors <- tibble(a = rnorm(N, 1.3, 0.3),
                 b = rnorm(N, 0, 1),
                 sigma = rexp(N, 1),
                 sim = 1:N)

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- se_turtle$Se - mean(se_turtle$Se)

# combine and simulate
prior_and_x <- priors %>% expand_grid(x = x) %>%    # combine priors and x's
  mutate(mu = a + b*x,                              # simulate regressions
         y = rnorm(nrow(.), mu, sigma))             # simulate data (e.g., y_rep)

# plot the priors
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim")


# --------------------------------------------------------------------------
#prior only model

# prior predicitve simulation
se_prior_brm <- brm(Se ~ Site_Type,
              family = Gamma(link="log"),
              data = se_turtle %>% mutate(se_c = (Se - mean(Se))/sd(Se)),
              prior = c(prior(normal(1.3,0.3), class="Intercept"),
                        prior(normal(0,1), class="b")),
              sample_prior = "only",
              cores = 4, chains = 1, iter=1000,
              file = "C:/Users/annak/OneDrive/Documents/Bayes_Topics_Class_F2021/Bayes_Topics_2021/se_prior_brm.rds")



# save the conditional effects plot as its own plot object
se_prior_cond <- plot(conditional_effects(se_prior_brm))

# priors plot with reference means and standard deviations on plot
se_prior_cond$Site_Type +
  theme_bw() +
  #scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = (37.1), linetype=1, size=1, color="red") +
  geom_hline(yintercept = (3.4), linetype=1, size=1, color="blue") +
  geom_hline(yintercept = c((38.7+10.9),(38.7-10.9)), linetype=2, color="red")+
  geom_hline(yintercept = c((3.4+0.72),(3.4-0.72)), linetype=2, color="blue")

# priors plot with reference means on plot 
# I like this one the best
se_prior_cond$Site_Type +
  theme_bw() +
  scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = c(37.1, 3.4)) +
  geom_text(aes(0.7, 45, label = "Mean Coal Ash Site [Se]"))+
  geom_text(aes(0.7, 4.3, label="Mean Reference Site [Se]"))


# ---------------------------------------------------------------------------
#model with data
# turtle blood [Se] as a function of wetland type
se_brm <- brm(Se ~ Site_Type,
              family = Gamma(link="log"),
              data = se_turtle %>% mutate(se_c = (Se - mean(Se))/sd(Se)),
              prior = c(prior(normal(1.3,0.3), class="Intercept"),
                        prior(normal(0,1), class="b")),
              cores = 4, chains = 1, iter=1000,
              file = "C:/Users/annak/OneDrive/Documents/Bayes_Topics_Class_F2021/Bayes_Topics_2021/se_brm.rds")


se_brm

#posterior plot with no original data points
plot_post_cond <- plot(conditional_effects(se_brm))
plot_post_cond$Site_Type +
  theme_bw() +
  scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = c(37.1, 3.4)) +
  geom_text(aes(0.7, 45, label = "Mean Coal Ash Site [Se]"))+
  geom_text(aes(0.7, 4.3, label="Mean Reference Site [Se]"))

#postrior plot with original data points
plot_post_cond <- plot(conditional_effects(se_brm), points=T)
plot_post_cond$Site_Type +
  theme_bw() +
  scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = c(37.1, 3.4)) +
  geom_text(aes(0.7, 45, label = "Mean Coal Ash Site [Se]"))+
  geom_text(aes(0.7, 4.3, label="Mean Reference Site [Se]"))+
  labs(x="Wetland Type", y="Selenium Concentration in Turtle Blood (ug/g)")

# Remaking the postior conditional effects plot by hand so the style can
# be more easily manipulated
plot_post_cond$Site_Type$data %>% 
  ggplot() + 
  geom_errorbar(aes(ymin = lower__, ymax = upper__, y = estimate__,
                      x = Site_Type), width=0.2) +
  geom_point(aes(Site_Type, estimate__), size=3)+
  geom_jitter(data = se_brm$data, aes(x = Site_Type, y = Se),alpha = 0.4, width=0.1) +
  theme_bw() +
  scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = c(37.1, 3.4)) +
  geom_text(aes(0.7, 45, label = "Mean Coal Ash Site [Se]"))+
  geom_text(aes(0.7, 4.3, label="Mean Reference Site [Se]"))+
  labs(x="Wetland Type", y="Selenium Concentration in Turtle Blood (ug/g)")


# postior predictive check plots
pp_check(se_brm, type = "stat_grouped", group = 'Site_Type')
pp_check(se_brm, type="scatter_avg_grouped", group='Site_Type')

# postior predictive check by modelling new data based off of model posterior
# means and standard deviations and making it a plottable data frame
simulated_control <- rgamma(25, (exp(-0.08)))
simulated_tile <- rgamma(25, (exp(-0.08+1.87)))

simulated_data <- as.data.frame(cbind(simulated_control, simulated_tile))
simulated_data <- gather(simulated_data, "site", "se")
simulated_data$site <- as.factor(simulated_data$site)

# Basic stripcharts of posterior predictive check simulated data and the
# original data used in model
stripchart(se_turtle$Se ~ se_turtle$Site_Type, method = "jitter",
           subset = se_turtle$Se <25, vertical = TRUE)
stripchart(simulated_data$se ~ simulated_data$site, method = "jitter",
           vertical = TRUE)


# Now using tidybayes

cond_data <- tibble(Site_Type = c("Control", "Tile"))

posts <- add_epred_draws(se_brm, newdata = cond_data)

# density plot
posts %>% 
  ggplot(aes(x=.epred))+
  geom_density(aes(fill=Site_Type))

# box plot
posts %>% 
  ggplot(aes(x=Site_Type, y=.epred))+
  geom_boxplot(aes(group=Site_Type, fill = Site_Type), outlier.shape = NA)+
  geom_jitter(data = se_brm$data, aes(x = Site_Type, y = Se),alpha = 0.4, width=0.1) +
  labs(x = "Wetland Type", y = "Selenium Concentration in Turtle Blood (ug/g)")+
  theme(legend.position = "none")+
  scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = c(37.1, 3.4)) +
  geom_text(aes(0.7, 45, label = "Mean Coal Ash Site [Se]"))+
  geom_text(aes(0.7, 4.3, label="Mean Reference Site [Se]"))

posts %>% 
  ggplot(aes(x=Site_Type, y=.epred))+
  geom_boxplot(aes(group=Site_Type, fill = Site_Type), outlier.shape = NA)+
  geom_jitter(data = se_brm$data, aes(x = Site_Type, y = Se),alpha = 0.4, width=0.1) +
  labs(x = "Wetland Type", y = "Selenium Concentration in Turtle Blood (ug/g)")+
  theme(legend.position = "none")+
  geom_hline(yintercept = c(37.1, 3.4)) +
  geom_text(aes(0.7, 38.3, label = "Mean Coal Ash Site [Se]"))+
  geom_text(aes(0.7, 4.7, label="Mean Reference Site [Se]"))
  

# poserior summary statistics
posts %>% 
  ungroup() %>% 
  select(.draw, Site_Type, .epred) %>% 
  pivot_wider(names_from = Site_Type, values_from = .epred) %>% 
  mutate(diff_tc = Tile - Control) %>% 
  ggplot(aes(x=diff_tc))+
  geom_density()+
  labs(x = "Difference Between Tile and Control [Se]", y="Density")

posts %>% 
  ungroup() %>% 
  select(.draw, Site_Type, .epred) %>% 
  pivot_wider(names_from = Site_Type, values_from = .epred) %>% 
  mutate(diff_tc = Tile - Control) %>% 
  summarize(mean_diff = mean(diff_tc),
            upper = quantile(diff_tc, probs = 0.975),
            lower = quantile(diff_tc, probs = 0.025)) 
  

# The mean difference in selenium concentrations in painted turtle blood
# between tile and control sites is 5.32 ug/g, and we are 95% confident
# that the true mean difference is between 2.62 and 9.56 ug/g ---
# I don't think that wording is 100% correct

# There is a 95% probability that the difference between tile and control
# turtle blood is between 2.62 and 9.56 ug/g




#buffer code


# --------------------------------------------------------------------------
# Running the model again but with no outlier (64 ug/g Se)

# Removing outlier from data set
se_turtle_NO <- se %>% 
  filter(Sample_Type == "Turtle") %>% 
  filter(Se < 50)

# Model with no outlier
se_no_out_brm <- brm(Se ~ Site_Type,
              family = Gamma(link="log"),
              data = se_turtle_NO %>% mutate(se_c = (Se - mean(Se))/sd(Se)),
              prior = c(prior(normal(2.5,1), class="Intercept"),
                        prior(normal(0,1), class="b")),
              cores = 4, chains = 1, iter=1000,
              file = "C:/Users/annak/OneDrive/Documents/Bayes_Topics_Class_F2021/Bayes_Topics_2021/se_no_out_brm.rds")


se_no_out_brm

# conditional effects plot of posterior
conditional_effects(se_no_out_brm)

# postior predictive check by modelling new data based off of model posterior
# means and standard deviations and making it a plottable data frame
simulated_control_NO <- rgamma(25, (exp(-0.26)))
simulated_tile_NO <- rgamma(25, (exp(-0.26+1.43)))

simulated_data_NO <- as.data.frame(cbind(simulated_control_NO, simulated_tile_NO))
simulated_data_NO <- gather(simulated_data_NO, "site", "se")
simulated_data_NO$site <- as.factor(simulated_data_NO$site)

# Basic stripcharts of posterior predictive check simulated data and the
# original data used in model
stripchart(se_turtle$Se ~ se_turtle$Site_Type, method = "jitter",
           subset = se_turtle$Se <25, vertical = TRUE)
stripchart(simulated_data_NO$se ~ simulated_data_NO$site, method = "jitter",
           vertical = TRUE)  


# ---------------------------------------------------------------------------

# Water model

# prior values from USFWS 2018
#SD tile 3 +/- 1 ug/L ------- log values : 1.1 +/- 0.1
#SD reference ~ 1 +/- 0.5 ug/L -------- log values: 0.1 +/- 0.69


#sub group just water [se]
se_water <- filter(se, Sample_Type == "Water")


# Priors

#model - Se ~ Site_Type
N = 100  # number of simulations (change as needed)

# simulate priors
priors <- tibble(a = rnorm(N, 1.1, 0.1),
                 b = rnorm(N, 0, 1),
                 sigma = rexp(N, 1),
                 sim = 1:N)

# data (only the x values, since we're simulating y and mu and pretending we don't have them yet)
x <- se_water$Se - mean(se_water$Se)

# combine and simulate
prior_and_x <- priors %>% expand_grid(x = x) %>%    # combine priors and x's
  mutate(mu = a + b*x,                              # simulate regressions
         y = rnorm(nrow(.), mu, sigma))             # simulate data (e.g., y_rep)

# plot
prior_and_x %>% 
  ggplot(aes(x = x, y = mu, group = sim)) + 
  geom_line() +
  geom_point(aes(y = y)) +
  labs(y = "sim")


# ---------------------------------------------------------------------------

#prior only model

# prior predicitve simulation
se_water_prior_brm <- brm(Se ~ Site_Type,
                    family = Gamma(link="log"),
                    data = se_water %>% mutate(se_c = (Se - mean(Se))/sd(Se)),
                    prior = c(prior(normal(0.3,0.1), class="Intercept"),
                              prior(normal(0,1), class="b")),
                    sample_prior = "only",
                    cores = 4, chains = 1, iter=1000,
                    file = "C:/Users/annak/OneDrive/Documents/Bayes_Topics_Class_F2021/Bayes_Topics_2021/se_water_prior_brm.rds")


# saving postior conditional effects plot as its own plot object
se_water_prior_cond <- plot(conditional_effects(se_water_prior_brm))

# tidying up the conditional effects plot and adding prior value reference
# lines to the plot
se_water_prior_cond$Site_Type +
  theme_bw() +
  #scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = c(1, 3)) +
  geom_text(aes(0.7, 1.2, label = "Mean Reference Site [Se]"))+
  geom_text(aes(0.7, 3.7, label="Mean Tile Site [Se]"))


# --------------------------------------------------------------------------

# Model with data
se_water_brm <- brm(Se ~ Site_Type,
                          family = Gamma(link="log"),
                          data = se_water %>% mutate(se_c = (Se - mean(Se))/sd(Se)),
                          prior = c(prior(normal(0.1,0.3), class="Intercept"),
                                    prior(normal(0,1), class="b")),
                          cores = 4, chains = 1, iter=1000,
                    file = "C:/Users/annak/OneDrive/Documents/Bayes_Topics_Class_F2021/Bayes_Topics_2021/se_water_brm.rds")

se_water_cond <- plot(conditional_effects(se_water_brm))

se_water_cond$Site_Type +
  theme_bw() +
  #scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = c(1, 3)) +
  geom_text(aes(0.7, 1.1, label = "Mean Reference Site [Se]"))+
  geom_text(aes(0.7, 3.1, label="Mean Tile Site [Se]"))


pp_check(se_water_brm, type = "stat_grouped", group = 'Site_Type')
pp_check(se_water_brm, type = "dens_overlay", nsamples = 500)

water_cond_data <- tibble(Site_Type = c("Control", "Tile"))

water_posts <- add_epred_draws(se_water_brm, newdata = water_cond_data)

water_posts %>% 
  ggplot(aes(x=.epred))+
  geom_density(aes(fill=Site_Type))

water_posts %>% 
  ggplot(aes(x=Site_Type, y=.epred))+
  geom_boxplot(aes(group=Site_Type, fill = Site_Type), outlier.shape = NA)+
  geom_jitter(data = se_water_brm$data, aes(x = Site_Type, y = Se),alpha = 0.4, width=0.1) +
  labs(x = "Wetland Type", y = "Selenium Concentration in Wetland Water (ug/L)")+
  theme(legend.position = "none")+
  scale_y_log10(limits = c(0.1, 300))  +
  geom_hline(yintercept = c(1, 3)) +
  geom_text(aes(0.7, 1.2, label = "Mean Reference Site [Se]"))+
  geom_text(aes(0.7, 3.6, label="Mean Tile Site [Se]"))

water_posts %>% 
  ggplot(aes(x=Site_Type, y=.epred))+
  geom_boxplot(aes(group=Site_Type, fill = Site_Type), outlier.shape = NA)+
  geom_jitter(data = se_water_brm$data, aes(x = Site_Type, y = Se),alpha = 0.4, width=0.1) +
  labs(x = "Wetland Type", y = "Selenium Concentration in Wetland Water (ug/L)")+
  theme(legend.position = "none")+
  geom_hline(yintercept = c(1, 3)) +
  geom_text(aes(0.67, 1.1, label = "Mean Reference Site [Se]"))+
  geom_text(aes(0.6, 3.1, label="Mean Tile Site [Se]"))



water_posts %>% 
  ungroup() %>% 
  select(.draw, Site_Type, .epred) %>% 
  pivot_wider(names_from = Site_Type, values_from = .epred) %>% 
  mutate(diff_tc = Tile - Control) %>% 
  summarize(mean_diff = mean(diff_tc),
            upper = quantile(diff_tc, probs = 0.975),
            lower = quantile(diff_tc, probs = 0.025)) 


# -------------------------------------------------------------------------

#plotting actual predictions
cond_data <- tibble(name = c("a", "b"))

posts <- add_epred_draws(modelname, newdata = cond_data)

posts %>% 
  ggplot(aes(x=.epred))+
  geom_density(aes(fill=name)) # can change this line to be any kind of plot

# to separate into a wide format and add a difference column
posts %>% 
  ungroup() %>% 
  select(.draw, name, .edpred) %>% 
  pivot_wider(names_from = name, values_from = .epred) %>% 
  mutate(diff_ab = a - b) %>% 
  summarize(mean_diff = mean(diff_ab),
            upper = quantile(diff_ab, probs = 0.975),
            lower = quantile(diff_ab, probs = 0.025))

posts %>% 
  ungroup() %>% 
  select(.draw, name, .edpred) %>% 
  pivot_wider(names_from = name, values_from = .epred) %>% 
  mutate(diff_ab = a - b) %>% 
  median_qi(diff_ab)

posts %>% 
  ungroup() %>% 
  select(.draw, name, .edpred) %>% 
  pivot_wider(names_from = name, values_from = .epred) %>% 
  mutate(diff_ab = a - b) %>%
  ggplot(aes(x=diff_ab))+
  geom_density()