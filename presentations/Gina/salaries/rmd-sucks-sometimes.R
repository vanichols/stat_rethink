# ISU Professor Salaries - gender disparities?
  
library(janitor)
library(dplyr)
library(tidyr)
library(tidytext)
library(tibble)
library(ggplot2)
library(scales)
library(stringr)
library(forcats)
library(emo)

library(CyChecks3)

data("professors")
profs <- professors %>% as_tibble()
head(profs)

profs %>%
  mutate(title_simp = as_factor(title_simp),
         title_simp = fct_reorder(title_simp, base_salary)) %>% 
  ggplot(aes(gender, base_salary, color = gender)) +
  geom_boxplot(outlier.colour = NA) +
  geom_hline(yintercept = 50000, linetype = "dashed") +
  geom_jitter(alpha = 0.4) +
  facet_grid(.~title_simp) +
  scale_y_continuous(labels = dollar_format()) +
  guides(color = F) +
  labs(title = "2019 ISU Salaries",
       y = "Reported Base Salary",
       x = NULL) +
  theme_bw()


mdat <- 
  profs %>%
  filter(dept_chair == "N") %>%
  filter(base50 == "Y")  %>%
  mutate(lsal = log(base_salary)) %>% #--because I'm not sure how brms works
  select(dept, gender, rank, lsal) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(rank = fct_reorder(rank, lsal))

mdat_sub <- 
  profs %>%
  filter(dept_chair == "N") %>%
  filter(base50 == "Y")  %>%
  mutate(lsal = log(base_salary)) %>% #--because I'm not sure how brms works
  filter(dept %in% c("animal science", "statistics", "agronomy")) %>% 
  select(dept, gender, rank, lsal) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(rank = fct_reorder(rank, lsal)) 
  


### Fitting models

library(lme4)
library(emmeans)
library(broom)

library(brms)
library(tidybayes)

options(mc.cores = parallel::detectCores())

m1 <- lm(lsal ~ dept*rank*gender, data = mdat_sub)

summary(m1) 

summary(m1)$coefficients %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  clean_names() %>% 
  mutate(rowname = str_remove_all(rowname, "dept|rank|gender")) %>% 
  separate(rowname, into = c("x1", "x2", "x3"), sep = ":") %>% 
  filter(!is.na(x2), !is.na(x3)) %>% 
  mutate(x2 = factor(x2, levels = c("assoc", "prof", "awarded"))) %>% 
  ggplot(aes(x2, estimate)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std_error, ymax = estimate + std_error)) + 
  facet_grid(.~x1)


#--let's try dummy coding
library(fastDummies)

mdat_sub_dum <- 
  mdat_sub %>% 
  fastDummies::dummy_cols(., remove_first_dummy = T) %>% 
  clean_names() %>% 
  select(-dept, -gender, -rank)

m2 <- lm(lsal ~ 0 + ., data = mdat_sub_dum)
summary(m2)

get_prior(lsal ~ 0 + dept*rank*gender, data = mdat_sub)

bm0priors <- 

bm0 <-
  brm(
    data = mdat_sub,
    family = gaussian,
    formula = lsal ~ 0 + dept*rank*gender,
    iter = 4000, chains = 4,
    seed = 7,
    file = "presentations/Gina/salaries/fits/bm0") 


summary(bm0)
plot(bm0)


#--try in brms w/o priors
bm1 <-
  brm(
    data = mdat_sub,
    family = gaussian,
    formula = lsal ~ 0 + gender + rank + gender:rank,
    iter = 2000, warmup = 1000, chains = 1,
    seed = 7,
    file = "presentations/Gina/salaries/fits/bm1") 


summary(bm1)
plot(bm1)


bm2 <-
  brm(
    data = mdat,
    family = gaussian,
    formula = lsal ~ gender + rank + gender:rank,
    iter = 3000, chains = 4,
    seed = 7,
    file = "presentations/Gina/salaries/fits/bm2") 

summary(bm2)
get_variables(bm2)
#--the effect of being male:

bm2 %>%
  gather_draws(`b_.*`, regex = TRUE) %>% 
  mutate(.variable = str_remove_all(.variable, "b_|gender|rank")) %>%
  ggplot(aes(x = .value, y = .variable)) +
  geom_vline(
    xintercept = 0,
    color = "gray50",
    size = 1.2,
    lty = 2,
    alpha = 0.5
  ) +
  geom_halfeyeh(aes(fill = .variable)) +
  stat_pointintervalh(.width = c(.66, .95), size = 2) +
  theme(legend.position = "none") +
  labs(title = "The effect of being male")

                                                                                                                 
                                                                                                                 
                                                                                                                 # dept as fixed -----------------------------------------------------------
                                                                                                                 
                                                                                                                 get_prior(
                                                                                                                   lsal ~  gender * title_simp * dept,
                                                                                                                   data = bsal_brms
                                                                                                                 ) %>%
                                                                                                                   parse_dist(prior) %>% #--a tidybayes function
                                                                                                                   ggplot(aes(y = prior, dist = .dist, args = .args, fill = class)) +
                                                                                                                   stat_dist_halfeye() +
                                                                                                                   theme(axis.text=element_text(size=15))
                                                                                                                 
                                                                                                                 
                                                                                                                 # m2 <- brm(
                                                                                                                 #   lsal ~  gender * title_simp * dept,
                                                                                                                 #   data = bsal_brms,
                                                                                                                 #   sample_prior = T
                                                                                                                 # )
                                                                                                                 
                                                                                                                 #--oof the intercept still had a hard time. I'll have to think about why that is
                                                                                                                 summary(m2)
                                                                                                                 
                                                                                                                 # try with 0 intercept ----------------------------------------------------
                                                                                                                 
                                                                                                                 #--mcelreath talks about how using the above assigns unequal uncertainty to m vs f
                                                                                                                 
                                                                                                                 #--I could probably constrain them. Try without defining them
                                                                                                                 get_prior(
                                                                                                                   lsal ~  0 + gender * title_simp + gender + (1 | dept),
                                                                                                                   data = bsal_brms
                                                                                                                 )
                                                                                                                 
                                                                                                                 #--these priors are for asst profs.
                                                                                                                 p2 <- prior(student_t(3, 11.5, 2), class = "b", coef = "genderM") +
                                                                                                                   prior(student_t(3, 11.5, 2), class = "b", coef = "genderF")
                                                                                                                 
                                                                                                                 m2 <- brm(
                                                                                                                   lsal ~  0 + gender * title_simp + gender + (1 | dept),
                                                                                                                   prior = p2,
                                                                                                                   data = bsal,
                                                                                                                   sample_prior = T
                                                                                                                 )
                                                                                                                 
                                                                                                                 #--man those intercepts!
                                                                                                                 summary(m2)
                                                                                                                 
                                                                                                                 #--I'm not sure how to interpret these results
                                                                                                                 
                                                                                                                 #--look at terms
                                                                                                                 #--b_Intercept is female-assistprof.
                                                                                                                 #--the r_dept are offsets from the intercept for each dept.
                                                                                                                 get_variables(m2)
                                                                                                                 
                                                                                                                 m2 %>%
                                                                                                                   gather_draws(`b_.*`, regex = TRUE) %>%
                                                                                                                   ungroup() %>%
                                                                                                                   mutate(
                                                                                                                     .variable = str_remove_all(.variable, "b_|gender|title_simp")) %>%
                                                                                                                   dplyr::filter(.variable %in% c("F", "M")) %>%
                                                                                                                   ggplot(aes(x = .value, y = .variable)) +
                                                                                                                   geom_halfeyeh(fill = "gray80") +
                                                                                                                   stat_pointintervalh(.width = c(.66, .95)) +
                                                                                                                   theme(legend.position = "none")
                                                                                                                 
                                                                                                                 
                                                                                                                 m2 %>%
                                                                                                                   gather_draws(`b_.*`, regex = TRUE) %>%
                                                                                                                   ungroup() %>%
                                                                                                                   mutate(
                                                                                                                     .variable = str_remove_all(.variable, "b_|gender|title_simp")) %>%
                                                                                                                   dplyr::filter(.variable != "Intercept",
                                                                                                                                 .variable != "Prof",
                                                                                                                                 .variable != "AssocProf",
                                                                                                                                 .variable != "AwardedProf") %>%
                                                                                                                   mutate(
                                                                                                                     .variable = ifelse(.variable == "M", "M:AsstProf", .variable),
                                                                                                                     .variable = fct_reorder(.variable, .value)) %>%
                                                                                                                   ggplot(aes(x = .value, y = .variable)) +
                                                                                                                   geom_vline(xintercept = 0, color = "gray50", size = 1.2, lty = 2, alpha = 0.5) +
                                                                                                                   geom_halfeyeh(fill = "gray80") +
                                                                                                                   stat_pointintervalh(.width = c(.66, .95)) +
                                                                                                                   theme(legend.position = "none")
                                                                                                                 