##***********************************************************************************************************************************##
## Written by Michael B. Adebayo, PhD Student in de Garidel-Thoron lab, (2019 – 2023)                                                ##
## Centre européen de recherche et d'enseignement de géosciences de l'environnement (CEREGE), Aix-en-Provence                        ##
## Aix-Marseille University, France                                                                                                  ##
## Code related to figures in Thesis Chapter 4                                                                                       ##
##***********************************************************************************************************************************##

##***********************************************************************************************************************************##
## Manuscript Title: Fragment Variance Index : A new proxy for estimating planktonic foraminifera dissolution in deep sea sediments  ##
##***********************************************************************************************************************************##

#********************************************
#Figure 1 (Range Bar Chart) -----------------
#*********************************************
#*
Range_bar_chart <- ggplot(data = Range_Bar_Chart, aes(x = as.factor(CoreType), y = Highest)) +
  geom_crossbar(aes(ymin = Lowest, ymax = Highest), width = 0.5, fill = (values = c("#D7595D","#DAA201", "#0532FF", "#099F74"))) +
  coord_flip() +
  theme_bw() + 
  theme(legend.position= "right") + 
  theme(axis.line  = element_line(colour = "black",size=0), panel.border = element_rect(colour = "black", fill=NA, size=1),panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.text = element_text(size = 12)) + 
  theme(axis.title.x = element_text(colour = "black",size = 14, face = "bold"), axis.title.y = element_text(colour = "black",size = 14, face = "bold")) + 
  xlab("Core Type") + 
  ylab("Fragmentation Index (%)")

png("~/Desktop/Range Bar Chart.png",width=4000,height=3500,units="px",res=600,bg="white", pointsize = 8)
ggarrange(Range_bar_chart)
dev.off()

#************************************************************************
#Statistical Analysis (Factors driving Holocene assemblage size) -----------------
#************************************************************************

# Linear Models for Core MD90-0963

Competition_MD63 <- lm(formula = Size95 ~ Sprichness, data = MD63_size_95_vs_causation_Holocene)
summary(Competition_MD63)

Productivity_MD63  <- lm(formula = Size95 ~ Percbull, data = MD63_size_95_vs_causation_Holocene)
summary(Productivity_MD63)

Productivity_MD63_2  <- lm(formula = Size95 ~ GbulCayre, data = MD63_size_95_vs_causation_Holocene)
summary(Productivity_MD63_2)

Species_replacement_MD63  <- lm(formula = Size95 ~ Spreplace, data = MD63_size_95_vs_causation_Holocene)
summary(Species_replacement_MD63)

Dissolution_MD63  <- lm(formula = Size95 ~ Fragrate, data = MD63_size_95_vs_causation_Holocene)
summary(Dissolution_MD63)

Dissolution_MD63_2  <- lm(formula = Size95 ~ DisB, data = MD63_size_95_vs_causation_Holocene)
summary(Dissolution_MD63_2)

Temperature_MD63 <- lm(formula = Size95 ~ DeltaO, data = MD63_size_95_vs_causation_Holocene)
summary(Temperature_MD63)

Temperature_MD63_2 <- lm(formula = Size95 ~ MgCaSST, data = MD63_size_95_vs_causation_Holocene)
summary(Temperature_MD63_2)

# MD90-0963 Correlation Plots

Size_exp_by_competition <- ggplot(MD63_size_95_vs_causation,
                                         aes(Sprichness,Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.24, p = 0.003", x = 32, y = 900, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Diversity (Species Richness)"))


Size_exp_by_productivity <- ggplot(MD63_size_95_vs_causation,
                                  aes(Percbull, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.18, p = 0.01", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("% G. bulloides (Paleoproductivity)"))

Size_exp_by_productivity_2 <- ggplot(MD63_size_95_vs_causation_Holocene,
                                   aes(GbulCayre, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.18, p = 0.01", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("% G. bulloides (Paleoproductivity)"))

Size_exp_by_species_replacement <- ggplot(MD63_size_95_vs_causation,
                                   aes(Spreplace, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.08, p = 0.11", x = 2, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Tropical/Upwelling Species (Species replacement)"))

Size_exp_by_dissolution <- ggplot(MD63_size_95_vs_causation,
                                          aes(Fragrate, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.0003, p = 0.93", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Fragmentation Index (Dissolution)"))

Size_exp_by_temperature <- ggplot(MD63_size_95_vs_causation,
                                  aes(DeltaO, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.0003, p = 0.93", x = -1, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("DeltaO"))

Size_exp_by_temperature_2 <- ggplot(MD63_size_95_vs_causation_Holocene,
                                    aes(MgCaSST, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.0003, p = 0.93", x = -1, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Mg/Ca SST"))

# Linear Models for Core MD96-2077

Competition_MD96 <- lm(formula = Size95 ~ Sprichness, data = MD96_size95_vs_causation_Holocene)
summary(Competition_MD96)

Productivity_MD96  <- lm(formula = Size95 ~ Percbull, data = MD96_size95_vs_causation_Holocene)
summary(Productivity_MD96 )

Species_replacement_MD96  <- lm(formula = Size95 ~ Spreplace, data = MD96_size95_vs_causation_Holocene)
summary(Species_replacement_MD96 )

Dissolution_MD96  <- lm(formula = Size95 ~ Fragrate, data = MD96_size95_vs_causation_Holocene)
summary(Dissolution_MD96)

Temperature_MD96  <- lm(formula = Size95 ~ DeltaO, data = MD96_size95_vs_causation_Holocene)
summary(Temperature_MD96)

# MD96-2077 Correlation Plots

Size_exp_by_competition_MD96 <- ggplot(MD96_size95_vs_causation,
                                  aes(Sprichness,Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.18, p = 0.0004", x = 32, y = 900, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Diversity (Species Richness)"))


Size_exp_by_productivity_MD96 <- ggplot(MD96_size95_vs_causation,
                                   aes(Percbull, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.01, p = 0.52", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("% G. bulloides (Paleoproductivity)"))


Size_exp_by_species_replacement_MD96 <- ggplot(MD96_size95_vs_causation,
                                          aes(Spreplace, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.10, p = 0.01", x = 2, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Tropical/Upwelling Species (Species replacement)"))

Size_exp_by_dissolution_MD96 <- ggplot(MD96_size95_vs_causation,
                                  aes(Fragrate, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.50, p = 1.531E-11", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Fragmentation Index (Dissolution)"))

Size_exp_by_temperature_MD96 <- ggplot(MD96_size95_vs_causation,
                                       aes(DeltaO, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.02, p = 0.23", x = 1, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Delta O"))


# Linear Models for Core IODP 117-722

Competition_IODP <- lm(formula = Size95 ~ Sprichness, data = IODP_size_95_vs_causation_Holocene)
summary(Competition_IODP)

Productivity_IODP  <- lm(formula = Size95 ~ Percbull, data = IODP_size_95_vs_causation_Holocene)
summary(Productivity_IODP)

Species_replacement_IODP  <- lm(formula = Size95 ~ Spreplace, data = IODP_size_95_vs_causation_Holocene)
summary(Species_replacement_IODP)

Dissolution_IODP <- lm(formula = Size95 ~ Fragrate, data = IODP_size_95_vs_causation_Holocene)
summary(Dissolution_IODP)

Temperature_IODP <- lm(formula = Size95 ~ C37, data = IODP_size_95_vs_causation_Holocene)
summary(Temperature_IODP)

Temperature_IODP <- lm(formula = Size95 ~ Sac, data = HoloceneODPnew)
summary(Temperature_IODP)

# IODP 117-722 Correlation Plots

Size_exp_by_competition_IODP <- ggplot(IODP_size_95_vs_causation,
                                       aes(Sprichness,Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.04, p = 0.02", x = 32, y = 900, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Diversity (Species Richness)"))


Size_exp_by_productivity_IODP <- ggplot(IODP_size_95_vs_causation,
                                        aes(Percbull, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.05, p = 0.01", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("% G. bulloides (Paleoproductivity)"))


Size_exp_by_species_replacement_IODP <- ggplot(IODP_size_95_vs_causation,
                                               aes(Spreplace, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.13, p = 6.39e-06", x = 2, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Tropical/Upwelling Species (Species replacement)"))

Size_exp_by_dissolution_IODP <- ggplot(IODP_size_95_vs_causation,
                                       aes(Fragrate, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.02, p = 0.07", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Fragmentation Index (Dissolution)"))

Size_exp_by_temperature_IODP <- ggplot(IODP_size_95_vs_causation,
                                       aes(DeltaOSac, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.24, p = 1.357e-08 or R = 0.32, = 2.57e-10 (2nd ord poly)", x = 4, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("DeltaO (Uvigerina)"))

#************************************************************************
#Statistical Analysis (Factors driving Pleistocene assemblage size) -----------------
#************************************************************************

# Linear Models for Core MD90-0963

Competition_MD63 <- lm(formula = Size95 ~ Sprichness, data = MD63_size_95_vs_causation_Pleistocene)
summary(Competition_MD63)

Productivity_MD63  <- lm(formula = Size95 ~ Percbull, data = MD63_size_95_vs_causation_Pleistocene)
summary(Productivity_MD63)

Species_replacement_MD63  <- lm(formula = Size95 ~ Spreplace, data = MD63_size_95_vs_causation_Pleistocene)
summary(Species_replacement_MD63)

Dissolution_MD63  <- lm(formula = Size95 ~ Fragrate, data = MD63_size_95_vs_causation_Pleistocene)
summary(Dissolution_MD63)

Temperature_MD63 <- lm(formula = Size95 ~ DeltaO, data = MD63_size_95_vs_causation_Pleistocene)
summary(Temperature_MD63)

# MD90-0963 Correlation Plots

Size_exp_by_competition <- ggplot(MD63_size_95_vs_causation,
                                  aes(Sprichness,Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.24, p = 0.003", x = 32, y = 900, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Diversity (Species Richness)"))


Size_exp_by_productivity <- ggplot(MD63_size_95_vs_causation,
                                   aes(Percbull, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.18, p = 0.01", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("% G. bulloides (Paleoproductivity)"))


Size_exp_by_species_replacement <- ggplot(MD63_size_95_vs_causation,
                                          aes(Spreplace, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.08, p = 0.11", x = 2, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Tropical/Upwelling Species (Species replacement)"))

Size_exp_by_dissolution <- ggplot(MD63_size_95_vs_causation,
                                  aes(Fragrate, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.0003, p = 0.93", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Fragmentation Index (Dissolution)"))

Size_exp_by_temperature <- ggplot(MD63_size_95_vs_causation,
                                  aes(DeltaO, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.0003, p = 0.93", x = -1, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("DeltaO"))

# Linear Models for Core MD96-2077

Competition_MD96 <- lm(formula = Size95 ~ Sprichness, data = MD96_size95_vs_causation_Pleistocene)
summary(Competition_MD96)

Productivity_MD96  <- lm(formula = Size95 ~ Percbull, data = MD96_size95_vs_causation_Pleistocene)
summary(Productivity_MD96 )

Species_replacement_MD96  <- lm(formula = Size95 ~ Spreplace, data = MD96_size95_vs_causation_Pleistocene)
summary(Species_replacement_MD96 )

Dissolution_MD96  <- lm(formula = Size95 ~ Fragrate, data = MD96_size95_vs_causation_Pleistocene)
summary(Dissolution_MD96)

Temperature_MD96  <- lm(formula = Size95 ~ DeltaO, data = MD96_size95_vs_causation_Pleistocene)
summary(Temperature_MD96)

# MD96-2077 Correlation Plots

Size_exp_by_competition_MD96 <- ggplot(MD96_size95_vs_causation,
                                       aes(Sprichness,Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.18, p = 0.0004", x = 32, y = 900, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Diversity (Species Richness)"))


Size_exp_by_productivity_MD96 <- ggplot(MD96_size95_vs_causation,
                                        aes(Percbull, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.01, p = 0.52", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("% G. bulloides (Paleoproductivity)"))


Size_exp_by_species_replacement_MD96 <- ggplot(MD96_size95_vs_causation,
                                               aes(Spreplace, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.10, p = 0.01", x = 2, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Tropical/Upwelling Species (Species replacement)"))

Size_exp_by_dissolution_MD96 <- ggplot(MD96_size95_vs_causation,
                                       aes(Fragrate, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.50, p = 1.531E-11", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Fragmentation Index (Dissolution)"))

Size_exp_by_temperature_MD96 <- ggplot(MD96_size95_vs_causation,
                                       aes(DeltaO, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.02, p = 0.23", x = 1, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Delta O"))


# Linear Models for Core IODP 117-722

Competition_IODP <- lm(formula = Size95 ~ Sprichness, data = IODP_size_95_vs_causation_Pleistocene)
summary(Competition_IODP)

Productivity_IODP  <- lm(formula = Size95 ~ Percbull, data = IODP_size_95_vs_causation_Pleistocene)
summary(Productivity_IODP)

Species_replacement_IODP  <- lm(formula = Size95 ~ Spreplace, data = IODP_size_95_vs_causation_Pleistocene)
summary(Species_replacement_IODP)

Dissolution_IODP <- lm(formula = Size95 ~ Fragrate, data = IODP_size_95_vs_causation_Pleistocene)
summary(Dissolution_IODP)

Temperature_IODP <- lm(formula = Size95 ~ DeltaOSac, data = IODP_size_95_vs_causation_Pleistocene)
summary(Temperature_IODP)

Temperature_IODP <- lm(formula = Size95 ~ Sac, data = PleistoceneODPnewdata)
summary(Temperature_IODP)

# IODP 117-722 Correlation Plots

Size_exp_by_competition_IODP <- ggplot(IODP_size_95_vs_causation,
                                       aes(Sprichness,Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.04, p = 0.02", x = 32, y = 900, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Diversity (Species Richness)"))


Size_exp_by_productivity_IODP <- ggplot(IODP_size_95_vs_causation,
                                        aes(Percbull, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.05, p = 0.01", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("% G. bulloides (Paleoproductivity)"))


Size_exp_by_species_replacement_IODP <- ggplot(IODP_size_95_vs_causation,
                                               aes(Spreplace, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.13, p = 6.39e-06", x = 2, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Tropical/Upwelling Species (Species replacement)"))

Size_exp_by_dissolution_IODP <- ggplot(IODP_size_95_vs_causation,
                                       aes(Fragrate, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.02, p = 0.07", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Fragmentation Index (Dissolution)"))

Size_exp_by_temperature_IODP <- ggplot(IODP_size_95_vs_causation_Pleistocene,
                                       aes(DeltaOSac, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.24, p = 1.357e-08 or R = 0.32, = 2.57e-10 (2nd ord poly)", x = 4, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("DeltaO (Uvigerina)"))


#************************************************************************
#Statistical Analysis (Factors driving Pliocene assemblage size) --------
#************************************************************************

# Linear Models for Core IODP 117-722

Competition_IODP <- lm(formula = Size95 ~ Sprichness, data = IODP_size_95_vs_causation_Pliocene)
summary(Competition_IODP)

Productivity_IODP  <- lm(formula = Size95 ~ Percbull, data = IODP_size_95_vs_causation_Pliocene)
summary(Productivity_IODP)

Species_replacement_IODP  <- lm(formula = Size95 ~ Spreplace, data = IODP_size_95_vs_causation_Pliocene)
summary(Species_replacement_IODP)

Dissolution_IODP <- lm(formula = Size95 ~ Fragrate, data = IODP_size_95_vs_causation_Pliocene)
summary(Dissolution_IODP)

Temperature_IODP <- lm(formula = Size95 ~ C37, data = IODP_size_95_vs_causation_Pliocene)
summary(Temperature_IODP)

Light_availability_IODP <- lm(formula = Size95 ~ poly(Ratio,2), data = IODP_size_95_vs_light_availability)
summary(Light_availability_IODP)
  
Temperature_IODP <- lm(formula = Size95 ~ Uvig, data = PlioceneODPnewdata)
summary(Temperature_IODP)
  
# IODP 117-722 Correlation Plots

Size_exp_by_competition_IODP <- ggplot(IODP_size_95_vs_causation,
                                       aes(Sprichness,Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.04, p = 0.02", x = 32, y = 900, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Diversity (Species Richness)"))


Size_exp_by_productivity_IODP <- ggplot(IODP_size_95_vs_causation,
                                        aes(Percbull, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.05, p = 0.01", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("% G. bulloides (Paleoproductivity)"))


Size_exp_by_species_replacement_IODP <- ggplot(IODP_size_95_vs_causation,
                                               aes(Spreplace, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.13, p = 6.39e-06", x = 2, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Tropical/Upwelling Species (Species replacement)"))

Size_exp_by_dissolution_IODP <- ggplot(IODP_size_95_vs_causation,
                                       aes(Fragrate, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.02, p = 0.07", x = 5, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("Fragmentation Index (Dissolution)"))

Size_exp_by_temperature_IODP <- ggplot(IODP_size_95_vs_causation_Pleistocene,
                                       aes(DeltaOSac, Size95)) + 
  geom_point(size = 4) +
  geom_smooth(method=lm,se=T, size = .45, color = 'black') +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text=element_text(size = 14, colour = "black"), legend.position = "right",
        axis.title=element_text(size=16, colour = "black", face = "bold")) +
  annotate("text", label = "R2 = 0.24, p = 1.357e-08 or R = 0.32, = 2.57e-10 (2nd ord poly)", x = 4, y = 950, size = 4.6, colour ="black") + 
  theme(legend.title = element_text(face = c(rep("bold", 5)), size = 15)) + 
  theme(legend.text = element_text(size = 13)) + 
  labs(y =  expression("Size 95/5 (µm)"), x = expression("DeltaO (Uvigerina)"))


