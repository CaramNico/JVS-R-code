#============ JOURNAL of VEGETATION SCIENCE ================== Caram et al. 2024
# ============== R Markdown ========================

# ========== Objective 1: Nested hierarchical structures from Messier et al., 2010 =============

trait <- lme(log(trait) ~ 1, random = ~ 1|species/season/treatment, data=dat, na.action = na.omit)
varcomp(trait, T, F)

# ========== Objective 2: Spearman coefficient correlation test rank ===========

trait_season1_vs_trait_season2 <- cor.test(season_1$trait, season_2$trait, method = "spearman")
trait_season1_vs_trait_season2

trait_season2_vs_trait_season3 <- cor.test(season_2$trait, season_3$trait, method = "spearman")
trait_season2_vs_trait_season3

trait_treatment1_vs_trait_treatment2 <- cor.test(treatment1$trait, treatment2$trait, method = "spearman")
trait_treatment1_vs_trait_treatment2

# =============== Objective 3: trait.flex.anova. Nested design analysis Dr. Petr Smilauer =================

CWMSpecFix <- trait.CWM(local_trait, mean_trait, community_trait)

specific= CWMSpecFix$specific.avg
fixed = CWMSpecFix$fixed.avg
intraspecific = spec-fix

# Trait flex
summary(aov(specific ~ 1, data = dat))
summary(aov(fixed ~ 1, data = dat))
summary(aov(intraspecific ~ 1, data = dat))

# Including explanatory variables
summary(aov(specific ~ main_plot_explanatory_variable  * sub_plot_explanatory_variable + Error(plotID), data = dat))
summary(aov(fixed ~ main_plot_explanatory_variable  * sub_plot_explanatory_variable + Error(plotID), data = dat))
summary(aov(intraspecific ~ main_plot_explanatory_variable  * sub_plot_explanatory_variable + Error(plotID), data = dat))
# This code is appropriate for split plot in time designs, i.e., a repeated measure analysis, 
# where responses are measured in each plotID through time. 

# The Error(plotID) denotes a split-plot design, where the main plot explanatory variable, say Treatment, is tested
# with the main plot Error (Error A), while the sub plot explanatory variable, say Season, 
# is tested with the subplot Error (Error B).

# =============== Objective 4: Community weighted mean analyses from SYNCSA and cwm.sig2 Duarte et al., 2018 ===========

# First analysis including only ITV across space. Typical studies where species are measured across spatial scales (say grazing treatments),
# but botanical composition is measured across the year.
# Community matrix includes temporal assessments, i.e., fixed quadrants measured in different season. 
# Trait matrix including species traits measured only in one season (typically in Spring), here referred as spatial_trait_matrix
# Treatment matrix including explanatory variables, such as aboveground biomass, season, treatment, etc, is the environmental_matrix

model_cwm_fixed_trait <- cwm.sig2(community_matrix, spatial_trait_matrix, environmental_matrix, formula = 
                          trait ~ treatment*season + (1|random_structure), 
                        FUN = FUN.LMER, PGLS = FALSE, runs = 999, # number of permutations
                        na.action="na.omit", control = controlLocal) 
summary(model_cwm_fixed_trait$model)
model_cwm_fixed_trait$p.site.shuffle
model_cwm_fixed_trait$p.taxa.shuffle

# Now the same analysis, but including the traits assessed for species in different seasons. Therefore, we now include the temporal ITV of species.
# Here, the trait matrix now includes spatial and temporal assessments, referred as temporal_spatial_trait_matrix

model_cwm_specific_trait <- cwm.sig2(community_matrix, temporal_spatial_trait_matrix, environmental_matrix, formula = 
                                    trait ~ treatment*season + (1|random_structure), 
                                  FUN = FUN.LMER, PGLS = FALSE, runs = 999, # number of permutations
                                  na.action="na.omit", control = controlLocal) 

summary(model_cwm_specific_trait$model)
model_cwm_specific_trait$p.site.shuffle
model_cwm_specific_trait$p.taxa.shuffle

















