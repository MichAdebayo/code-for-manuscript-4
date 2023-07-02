##***********************************************************************************************************************************##
## Written by Michael B. Adebayo, PhD Student in de Garidel-Thoron lab, (2019 – 2023)                                                ##
## Centre européen de recherche et d'enseignement de géosciences de l'environnement (CEREGE), Aix-en-Provence                        ##
## Aix-Marseille University, France                                                                                                  ##
## Code related to statistical analysis in Thesis Chapter 6                                                                          ##
##***********************************************************************************************************************************##

##***********************************************************************************************************************************##
## Manuscript Title: A first assessment of sea surface temperature reconstruction using planktonic foraminiferal assemblage size     ##
##                   in the tropical Indian Ocean                                                                                    ##
##***********************************************************************************************************************************##

##*************************************************************************
## Statistical Analysis (Factors driving Holocene assemblage size) --------
##*************************************************************************

## Models for Core MD90-0963

## fit model
Diversity_MD63 <- lm(formula = Size95 ~ Sp_richness, data = MD63_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Diversity_MD63)

## fit model
Productivity_MD63  <- lm(formula = Size95 ~ Perc_bulloides, data = MD63_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Productivity_MD63)

## fit model
Productivity_MD63_2  <- lm(formula = Size95 ~ Perc_bulloides_Cayre_et_al, data = MD63_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Productivity_MD63_2)

## fit model
Nutrient_regime_MD63 <- lm(formula = Size95 ~ Nut_regime, data = MD63_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Nutrient_regime_MD63)

## fit model
Dissolution_MD63 <- lm(formula = Size95 ~ Frag_rate, data = MD63_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Dissolution_MD63)

## fit model
Dissolution_MD63_2 <- lm(formula = Size95 ~ Frag_rate_Bassinot_et_al, data = MD63_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Dissolution_MD63_2)

## fit model
Temperature_MD63 <- lm(formula = Size95 ~ Delta_O_Ruber, data = MD63_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Temperature_MD63)

## fit model
Temperature_MD63_2 <- lm(formula = Size95 ~ MgCa_SST_Tachikawa_et_al, data = MD63_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Temperature_MD63_2)

## Models for Core MD96-2077

## fit model
Diversity_MD96 <- lm(formula = Size95 ~ Sp_richness, data = MD96_size95_vs_causation_Holocene)
## view R2 and p-value
summary(Diversity_MD96)

## fit model
Productivity_MD96 <- lm(formula = Size95 ~ Perc_bulloides, data = MD96_size95_vs_causation_Holocene)
## view R2 and p-value
summary(Productivity_MD96 )

## fit model
Nutrient_regime_MD96 <- lm(formula = Size95 ~ Nut_regime, data = MD96_size95_vs_causation_Holocene)
## view R2 and p-value
summary(Nutrient_regime_MD96)

## fit model
Dissolution_MD96 <- lm(formula = Size95 ~ Frag_rate, data = MD96_size95_vs_causation_Holocene)
## view R2 and p-value
summary(Dissolution_MD96)

## fit model
Temperature_MD96 <- lm(formula = Size95 ~ Delta_O_Inflata, data = MD96_size95_vs_causation_Holocene)
## view R2 and p-value
summary(Temperature_MD96)

## Models for Core ODP Site 722

## fit model
Diversity_ODP722 <- lm(formula = Size95 ~ Sp_richness, data = ODP_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Diversity_ODP722)

## fit model
Productivity_ODP722 <- lm(formula = Size95 ~ Perc_bulloides, data = ODP_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Productivity_ODP722)

## fit model
Nutrient_regime_ODP722  <- lm(formula = Size95 ~ Nut_regime, data = ODP_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Nutrient_regime_ODP722)

## fit model
Dissolution_ODP722 <- lm(formula = Size95 ~ Frag_rate, data = ODP_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Dissolution_ODP722)

## fit model
Temperature_ODP722 <- lm(formula = Size95 ~ Delta_O_Sac, data = ODP_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Temperature_ODP722)

## fit model
Temperature_ODP722_2 <- lm(formula = Size95 ~ Delta_O_Uvigerina, data = ODP_size_95_vs_causation_Holocene)
## view R2 and p-value
summary(Temperature_ODP722)

##****************************************************************************
## Statistical Analysis (Factors driving Pleistocene assemblage size) --------
##****************************************************************************

## Models for Core MD90-0963

## fit model
Diversity_MD63 <- lm(formula = Size95 ~ Sp_richness, data = MD63_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Diversity_MD63)

## fit model
Productivity_MD63 <- lm(formula = Size95 ~ Perc_bulloides, data = MD63_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Productivity_MD63)

## fit model
Nutrient_regime_MD63 <- lm(formula = Size95 ~ Nut_regime, data = MD63_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Nutrient_regime_MD63)

## fit model
Dissolution_MD63 <- lm(formula = Size95 ~ Frag_rate, data = MD63_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Dissolution_MD63)

## fit model
Temperature_MD63 <- lm(formula = Size95 ~ Delta_O_Ruber, data = MD63_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Temperature_MD63)

## Models for Core MD96-2077

## fit model
Diversity_MD96 <- lm(formula = Size95 ~ Sp_richness, data = MD96_size95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Diversity_MD96)

## fit model
Productivity_MD96 <- lm(formula = Size95 ~ Perc_bulloides, data = MD96_size95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Productivity_MD96)

## fit model
Nutrient_regime_MD96 <- lm(formula = Size95 ~ Nut_regime, data = MD96_size95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Nutrient_regime_MD96)

## fit model
Dissolution_MD96 <- lm(formula = Size95 ~ Frag_rate, data = MD96_size95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Dissolution_MD96)

## fit model
Temperature_MD96 <- lm(formula = Size95 ~ Delta_O_Inflata, data = MD96_size95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Temperature_MD96)

## Models for Core ODP Site 722

## fit model
Diversity_ODP722 <- lm(formula = Size95 ~ Sp_richness, data = ODP_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Diversity_ODP722)

## fit model
Productivity_ODP722  <- lm(formula = Size95 ~ Perc_bulloides, data = ODP_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Productivity_ODP722)

## fit model
Nutrient_regime_ODP722  <- lm(formula = Size95 ~ Nut_regime, data = ODP_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Nutrient_regime_ODP722)

## fit model
Dissolution_ODP722 <- lm(formula = Size95 ~ Frag_rate, data = ODP_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Dissolution_ODP722)

## fit model
Temperature_ODP722 <- lm(formula = Size95 ~ Delta_O_Sac, data = ODP_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Temperature_ODP722)

## fit model
Temperature_ODP722 <- lm(formula = Size95 ~ Delta_O_Uvigerina, data = ODP_size_95_vs_causation_Pleistocene)
## view R2 and p-value
summary(Temperature_ODP722)

##*************************************************************************
## Statistical Analysis (Factors driving Pliocene assemblage size) --------
##*************************************************************************

## Models for Core IODP 117-722

## fit model
Diversity_ODP722 <- lm(formula = Size95 ~ Sp_richness, data = ODP_size_95_vs_causation_Pliocene)
## view R2 and p-value
summary(Diversity_ODP722)

## fit model
Productivity_ODP722 <- lm(formula = Size95 ~ Perc_bulloides, data = ODP_size_95_vs_causation_Pliocene)
## view R2 and p-value
summary(Productivity_ODP722)

## fit model
Nutrient_regime_ODP722  <- lm(formula = Size95 ~ Nut_regime, data = ODP_size_95_vs_causation_Pliocene)
## view R2 and p-value
summary(Nutrient_regime_ODP722)

## fit model
Dissolution_ODP722 <- lm(formula = Size95 ~ Frag_rate, data = ODP_size_95_vs_causation_Pliocene)
## view R2 and p-value
summary(Dissolution_ODP722)

## fit model
Temperature_ODP722 <- lm(formula = Size95 ~ Delta_O_Uvigerina, data = ODP_size_95_vs_causation_Pliocene)
## view R2 and p-value
summary(Temperature_ODP722)

## fit model
Light_availability_ODP722 <- lm(formula = Size95 ~ poly(Ratio,2), data = ODP_size_95_vs_light_availability)
## view R2 and p-value
summary(Light_availability_ODP722)

##**********************************************************************************************************************************##
## End of Script --------
##**********************************************************************************************************************************##

