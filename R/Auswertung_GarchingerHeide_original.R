#####################################
#### Auswertung Garchinger Heide ####
####       Sina Appeltauer       ####
####      Stand: 25.09.2024      ####
#####################################

#Arbeitsverzeichnis setzen
setwd("~/Studium_Master/2. Semester/Projekt Garchinger Heide/R_Auswertung")

#Package laden
library(ggplot2)
library(dplyr)
library(vegan)
library(tidyr)
library(patchwork)                  
library(multcomp)
library(tibble)
library(gridExtra)
library(DHARMa)
library(GIFT)
library(TNRS)
library(rstatix)



#Tabellen einlesen
Data <- read.table("Arten und Kopfdaten.csv", header = TRUE, dec = ".", sep = ";")  #Rohtabelle Vegetationsaufnahme
Data <- data.frame(Data)

Data[Data == ""] <- 0   # Felder ohne Inhalt durch 0 ersetzen

#Tabelle nur mit Vegetation
Veg <- Data[c(1,12:161),]

Veg$Table.number <- as.character(Veg$Table.number)
Veg[,-1] <- lapply(Veg[,-1], as.numeric)
write.csv2(Veg, "Vegetationstabelle.csv")

Veg_pa <- decostand(Veg[,-1], method = "pa") #nur Art da oder nicht da (0 / 1)
Artenzahl <- colSums(Veg_pa)  #Berechnen Artenzahl pro Plot
Artenzahl <- c("Artenzahl", colSums(Veg_pa))

Veg <- rbind(Veg, Artenzahl)
Veg <- rbind(Veg, Data[2:3,]) # Bewirtschaftung und Mahdgut Datum hinzu
names(Veg) <- Veg[1,]
Veg <- Veg[-1,]

plant_names <- TNRS(Veg$Plot)
Veg[1:150,]$Plot <- plant_names[1:150,]$Accepted_name
rownames(Veg) <- Veg$Plot


##### Forschungsfrage 1 Vergleich Erweiterungsflächen ######

#### Vergleich Artenzahl Flaechennutzung ####
Veg <- Veg[,c(-1)]
Veg_t <- as.data.frame(t(Veg))

Veg_t$Artenzahl <- as.numeric(Veg_t$Artenzahl)

Veg_t %>%
  group_by(Bewirtsch) %>%
  summarise(mean_ab=mean(as.numeric(Artenzahl))) %>%
  ggplot() +
  geom_col(aes(x = Bewirtsch, y = mean_ab)) +
  labs(x = "Flächentyp", y = "Artenzahl") 

ggplot(Veg_t) +
  geom_boxplot(aes(x=Bewirtsch, y=as.numeric(Artenzahl), fill = Bewirtsch)) +
  labs(x = "Bewirtschaftung", y = "Artenzahl pro 4m²", fill="Bewirtschaftung") +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"), labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag") ) +
  ylim(0, NA) + 
  annotate("text",x= 1, y= max(Veg_t$Artenzahl)+2, label = "a", size = 5) +
  annotate("text",x= 2, y= max(Veg_t$Artenzahl)+2, label = "a", size = 5) +
  annotate("text",x= 3, y= max(Veg_t$Artenzahl)+2, label = "b", size = 5) +
  scale_x_discrete(labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  )


hist(Veg_t$Artenzahl) #normalverteilt
shapiro.test(Veg_t$Artenzahl) # nicht signifikant p = 0,067 damit normalverteilt

mod1 <- lm(Artenzahl ~ Bewirtsch, data=Veg_t)
plot(simulateResiduals(mod1))
anova(mod1)  #Signifikanz Unterschied zwischen Gruppen

TukeyHSD(aov(Artenzahl ~ Bewirtsch, data=Veg_t))
# Schnittzeitpunkte unterscheiden sich nicht signifikant, signifikant Weide - Schnitt 1.7. bzw. Schnitt 1.9.


#### Shannon-Index Plots ####
Veg_t_shannon <- Veg_t %>% 
  rownames_to_column(var="Plot")

Veg_t_shannon <- cbind(Veg_t_shannon$Plot, as.data.frame(lapply(Veg_t_shannon[2:151], as.numeric)))
Veg_t$Shannon <- diversity(Veg_t_shannon[2:151], index="shannon")

Veg_t$Evenness <- Veg_t$Shannon/log(specnumber(Veg_t[2:151]))

Veg_t %>%
  group_by(Bewirtsch) %>%
  reframe(mean_shannon=mean(Shannon),
          mean_evenness=mean(Evenness))

Veg_t %>%
  group_by(Bewirtsch) %>%
  summarise(mean_ab=mean(as.numeric(Shannon))) %>%
  ggplot() +
  geom_col(aes(x = Bewirtsch, y = mean_ab)) +
  labs(x = "Flächentyp", y = "Shannon-Index")


Veg_t %>%
  rownames_to_column(var="Plot") %>%
  select(Plot, Bewirtsch, Shannon, Evenness) %>%
  pivot_longer(cols=3:4) %>%
  ggplot() +
  geom_boxplot(aes(x=Bewirtsch, y=value, fill=Bewirtsch)) +
  labs(x = "Bewirtschaftung", y = "Wert", fill="Bewirtschaftung") +
  facet_wrap(.~name, scales="free") +
  scale_x_discrete(labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"), 
                    labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 14),
    strip.placement = "outside",  
    panel.spacing = unit(1.5, "lines") 
  )


hist(Veg_t$Shannon) #normalverteilt
shapiro.test(Veg_t$Shannon) # nicht signifikant p = 0,071 damit normalverteilt

mod2a <- lm(Shannon ~ Bewirtsch, data=Veg_t)
anova(mod2a)  #Signifikanz Unterschied zwischen Gruppen
plot(simulateResiduals(mod2a))
summary(glht(mod2a),linfct = mcp(Bewirtsch = "Tukey")) #Post-hoc testet Unterschied zwischen einzelnen Gruppen

TukeyHSD(aov(Shannon ~ Bewirtsch, data=Veg_t))
# nur ein signifikanter Unterschied zwischen Weide und Schnitt 1.7.


hist(Veg_t$Evenness) #normalverteilt
shapiro.test(Veg_t$Evenness) # nicht signifikant p = 0,071 damit normalverteilt

mod2b <- lm(Evenness ~ Bewirtsch, data=Veg_t)
plot(simulateResiduals(mod2b))
anova(mod2b)  #Signifikanz Unterschied zwischen Gruppen

TukeyHSD(aov(Evenness ~ Bewirtsch, data = Veg_t))

# kruskal.test(Evenness ~ Bewirtsch, data=Veg_t)
# dunn_test(Evenness ~ Bewirtsch, data=Veg_t, p.adjust.method = "bonferroni")



#### Tabellen mit Kennarten ####
Wiese <- read.table("Taxa Mähwiese.csv", header = FALSE, dec = ",", sep = ";")
Wiese <- as.data.frame(t(Wiese))
Wiese <- TNRS(Wiese$V1)
Wiese <- as.data.frame(Wiese$Accepted_name)
write.csv2(Wiese, file="Kennarten_FlachlandMähwiese.csv")

Magerrasen <- read.table("Taxa Kalkmagerrasen.csv", header = FALSE, dec = ",", sep = ";")
Magerrasen <- as.data.frame(t(Magerrasen))
Magerrasen <- TNRS(Magerrasen$V1)
Magerrasen <- as.data.frame(Magerrasen$Accepted_name)
write.csv2(Magerrasen, file="Kennarten_Magerrasen.csv")

# Kennarten markieren
Veg_k <- Veg[c(-151:-153),]
Veg_k$Kennart_Mager <- ifelse(rownames(Veg_k) %in% Magerrasen$`Magerrasen$Accepted_name`, "Kennart_Magerrasen", "")
Veg_k$Kennart_Wiese <- ifelse(rownames(Veg_k) %in% Wiese$`Wiese$Accepted_name`, "Kennart_Wiese", "")

Veg_k$Kennart_ges <- ifelse(Veg_k$Kennart_Mager == "Kennart_Magerrasen" & Veg_k$Kennart_Wiese == "", "Kennart_Magerrasen",
                            ifelse(Veg_k$Kennart_Wiese == "Kennart_Wiese" & Veg_k$Kennart_Mager == "", "Kennart_Wiese",
                                   ifelse(Veg_k$Kennart_Wiese == "Kennart_Wiese" & Veg_k$Kennart_Mager == "Kennart_Magerrasen", "Kennart_Misch",
                                          "Rest")))


# Balkendiagramm Artenzahl
Veg_k_neu <- Veg_k %>%
  rownames_to_column(var="Art") %>%
  pivot_longer(cols=2:91, names_to="Plot", values_to="Abundanz")

Veg_k_neu <- Veg_k_neu[,c(-2,-3)]
Veg_k_neu$pa_wert <- decostand(Veg_k_neu$Abundanz, method="pa")

Veg_k_neu <- Veg_k_neu %>%
  group_by(Plot, Kennart_ges) %>%
  reframe(Kennartenzahl=sum(pa_wert[,1]))

Veg_t_neu <- Veg_t %>%
  rownames_to_column(var="Plot") %>%
  dplyr::select(Plot, Bewirtsch)

Veg_k_neu <- merge(Veg_t_neu, Veg_k_neu, by="Plot")

# Vergleich der Kennarten mit der Bewirtschaftungsweise !nur mit Artenzahl

durchschnitt <- aggregate(Kennartenzahl ~ Kennart_ges + Bewirtsch, data = Veg_k_neu, FUN = mean)
write.csv2(durchschnitt, "durchschnitt.csv")

ggplot(Veg_k_neu) +
  geom_boxplot(aes(x=Kennart_ges, y=Kennartenzahl, fill=Bewirtsch)) +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"))

df <- Veg_k_neu %>%
  pivot_wider(values_from=Kennartenzahl, names_from=Kennart_ges) %>%
  mutate(Kennart_Wiese=Kennart_Wiese+Kennart_Misch,
         Kennart_Magerrasen=Kennart_Magerrasen+Kennart_Misch) %>%
  pivot_longer(cols=c(3, 5:6))

ggplot(df) +
  geom_boxplot(aes(x=name, y=value, fill=Bewirtsch)) +
  labs(x = "Kennart", y = "Anzahl Kennarten pro 4m²", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Kennarten für Magerrasen [84]", "Kennarten für Flachland-Mähwiese [66]", "Rest")) +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"), 
                    labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "bottom",
    legend.title = element_text(size=14),
    legend.text = element_text(size=12))


ggplot(filter(Veg_k_neu, Kennart_ges=="Kennart_Magerrasen"|Kennart_ges=="Kennart_Misch")) +
  geom_boxplot(aes(x=Bewirtsch, y=Kennartenzahl)) +
  labs(x="Bewirtschaftung", y="Kennartenzahl Magerrasen") +
  annotate("text",x= 1, y= 16, label = "a", size = 5) +
  annotate("text",x= 2, y= 16, label = "a", size = 5) +
  annotate("text",x= 3, y= 16, label = "b", size = 5)


mod3 <- lm(Kennartenzahl ~ Bewirtsch, data = filter(Veg_k_neu, Kennart_ges == "Kennart_Magerrasen"|Kennart_ges == "Kennart_Misch"))
plot(simulateResiduals(mod3))  #homogeneity of variance significant

kruskal.test(Kennartenzahl ~ Bewirtsch, data = filter(Veg_k_neu, Kennart_ges == "Kennart_Magerrasen"|Kennart_ges == "Kennart_Misch"))
dunn_test(Kennartenzahl ~ Bewirtsch, data = filter(Veg_k_neu, Kennart_ges == "Kennart_Magerrasen"|Kennart_ges == "Kennart_Misch"), p.adjust.method = "bonferroni")


ggplot(filter(Veg_k_neu, Kennart_ges=="Kennart_Wiese"|Kennart_ges=="Kennart_Misch")) +
  geom_boxplot(aes(x=Bewirtsch, y=Kennartenzahl)) +
  labs(x="Bewirtschaftung", y="Kennartenzahl Flachland-Mähwiese") +
  annotate("text",x= 1, y= 12, label = "a", size = 5) +
  annotate("text",x= 2, y= 12, label = "a", size = 5) +
  annotate("text",x= 3, y= 12, label = "b", size = 5)


mod4 <- lm(Kennartenzahl ~ Bewirtsch, data = filter(Veg_k_neu, Kennart_ges == "Kennart_Wiese"|Kennart_ges == "Kennart_Misch"))
plot(simulateResiduals(mod4))

kruskal.test(Kennartenzahl ~ Bewirtsch, data = filter(Veg_k_neu, Kennart_ges == "Kennart_Wiese"|Kennart_ges == "Kennart_Misch"))
dunn_test(Kennartenzahl ~ Bewirtsch, data = filter(Veg_k_neu, Kennart_ges == "Kennart_Wiese"|Kennart_ges == "Kennart_Misch"), p.adjust.method = "bonferroni")


ggplot(filter(Veg_k_neu, Kennart_ges=="Rest")) +
  geom_boxplot(aes(x=Bewirtsch, y=Kennartenzahl)) +
  labs(x="Bewirtschaftung", y="Rest") +
  annotate("text",x= 1, y= 16, label = "a", size = 5) +
  annotate("text",x= 2, y= 16, label = "b", size = 5) +
  annotate("text",x= 3, y= 16, label = "c", size = 5)


mod5 <- lm(Kennartenzahl ~ Bewirtsch, data = filter(Veg_k_neu, Kennart_ges == "Rest"))
plot(simulateResiduals(mod5))

anova(mod5)
TukeyHSD(aov(Kennartenzahl ~ Bewirtsch, data= filter(Veg_k_neu, Kennart_ges == "Rest")))


#### Artenzusammensetzung ####
### ORDINATION ###  
Veg_Ordi <- Veg_t[,c(1:150)] %>%
  rownames_to_column(var="Plot")
Veg_Ordi <- cbind(Veg_Ordi$Plot, as.data.frame(lapply(Veg_Ordi[2:151], as.numeric)))
Veg_Ordi <- Veg_Ordi %>%
  column_to_rownames(var="Veg_Ordi$Plot")

# wie viele Arten kommen nur 1x vor
sum(colSums(decostand(Veg_Ordi, method = "pa")) < 2)
# 39 Arten kommen nur 1x vor
Veg_Ordi2 <- Veg_Ordi[,colSums(decostand(Veg_Ordi, method = "pa")) > 1] 
sum(colSums(decostand(Veg_Ordi2, method = "pa")) < 2)  

Veg_Ordi.sqrt <- sqrt(Veg_Ordi2)         # Vegetationstabelle wurzeltransformieren
summary(Veg_Ordi.sqrt)                

# NMDS berechnen mit 3 Dimensionen Bray Curtis Distanz eine Einheit Achse Artenturnover von 50%
Vegtabelle.NMDS <- metaMDS(Veg_Ordi.sqrt, k = 2) 
plot(Vegtabelle.NMDS)                   # Ordinationsdiagramm anzeigen - nur Symbole
plot(Vegtabelle.NMDS, type = "t")       # Ord.diagramm mit Text: unuebersichtlich
# stress von 9,7% konnte nicht passend abgebildet werden 

# pruefen, welche Anzahl an Dimensionen gut geeignet ist
vegtab.nmds1 <- metaMDS(Veg_Ordi.sqrt, k = 1)
vegtab.nmds2 <- metaMDS(Veg_Ordi.sqrt, k = 2)
vegtab.nmds3 <- metaMDS(Veg_Ordi.sqrt, k = 3)
vegtab.nmds4 <- metaMDS(Veg_Ordi.sqrt, k = 4)
vegtab.nmds5 <- metaMDS(Veg_Ordi.sqrt, k = 5)
dimensionen <- 1:5
stress.dim <- c(vegtab.nmds1$stress, vegtab.nmds2$stress, vegtab.nmds3$stress,
                vegtab.nmds4$stress, vegtab.nmds5$stress)
plot(stress.dim ~ dimensionen, type = "b") 
# 2 Dimensionen

adonis2(Veg_Ordi.sqrt ~ Bewirtsch, data=Veg_t)

# Uebersichtsdiagramm mit Flaechenbezeichnung
plot(Vegtabelle.NMDS, type = "n")       
orditorp(Vegtabelle.NMDS, display = "sites", cex = 1.3, col = "navy") # Kreis bedeuten Ueberlappung
cnam <- make.cepnames(names(Veg_Ordi2)) # abgekuerzte Artnamen erzeugen jeweils 4 Zeichen Gattung Art
orditorp(Vegtabelle.NMDS, display = "species", labels = cnam, 
         col = "tomato")

# NUTZUNG #
plot(Vegtabelle.NMDS, type ="n")
orditorp(Vegtabelle.NMDS, display = "sites", col = "navy", labels = FALSE) 

Vegtabelle.NMDS <- scores(Vegtabelle.NMDS, display = "sites")
points(Vegtabelle.NMDS[Veg_t_neu$Bewirtsch == "VNP Schnitt 1.7.",], pch = 22, bg = "#66ff00", cex = 1.5)
points(Vegtabelle.NMDS[Veg_t_neu$Bewirtsch == "VNP Schnitt 1.9.",], pch = 21, bg = "#308400", cex = 1.5)
points(Vegtabelle.NMDS[Veg_t_neu$Bewirtsch == "VNP Weide",], pch = 23, bg = "#ff9100", cex = 1.5)
legend(0,0.3, legend = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag"), pch = c(22,21,23), 
       pt.bg = c("#66ff00", "#308400","#ff9100"), cex = 1.3, pt.cex = 1.5, title = "Bewirtschaftung")

ordiellipse(Vegtabelle.NMDS, Veg_t_neu$Bewirtsch, draw="polygon", label = TRUE)


#### Funktionale Pflanzeneigenschaften ####
traits_meta <- GIFT_traits_meta()
trait_values <- GIFT_traits(trait_IDs=c("1.6.3", "3.2.3", "4.1.3"))
trait_values <- filter(trait_values, work_species %in% rownames(Veg[1:150,])) %>%
  dplyr::select(c(2, 4:6))


Veg_traits_orginal <- Veg_t[,1:150] %>%
  rownames_to_column(var="Plot") %>%
  pivot_longer(cols=2:151, names_to="Art", values_to="Deckung") %>%
  merge(trait_values, by.x="Art", by.y="work_species")

#### Berechnung mit Deckung
Gesamtdeckung <- Veg_traits_orginal %>%
  group_by(Plot) %>%
  reframe(Gesamtdeckung=sum(as.numeric(Deckung)))

Veg_traits <- merge(Veg_traits_orginal, Gesamtdeckung)


Veg_traits$plant_height <- (as.numeric(Veg_traits$Deckung)*Veg_traits$trait_value_1.6.3)
Veg_traits$seed_mass <- (as.numeric(Veg_traits$Deckung)*Veg_traits$trait_value_3.2.3)
Veg_traits$SLA <- (as.numeric(Veg_traits$Deckung)*Veg_traits$trait_value_4.1.3)

Veg_traits <- Veg_traits %>%
  group_by(Plot) %>%
  na.omit() %>%
  reframe(mean_plant_height=mean(plant_height),
          mean_seed_mass=mean(seed_mass),
          mean_SLA=mean(SLA))

Veg_traits <- merge(Veg_traits, Veg_t_neu, by="Plot")

#### Berechnung mit Community weight mean

Veg_traits_CWM <- 
  Veg_traits_orginal %>%
  group_by(Plot) %>%
  summarize(Height_cwm = weighted.mean(trait_value_1.6.3, as.numeric(Deckung), na.rm = TRUE),
            Seed_cwm = weighted.mean(trait_value_3.2.3, as.numeric(Deckung), na.rm = TRUE),
            SLA_cwm = weighted.mean(trait_value_4.1.3, as.numeric(Deckung),na.rm = TRUE))

Veg_traits_CWM <- merge(Veg_traits_CWM, Veg_t_neu, by="Plot")

Veg_traits_CWM %>%
  pivot_longer(cols=2:4) %>%
  ggplot() +
  geom_boxplot(aes(x=Bewirtsch, y=value, fill=Bewirtsch), show.legend = F) +
  labs(x = "Bewirtschaftung", y = "Community-weighted mean (Abundanz)", fill="Bewirtschaftung") +
  facet_wrap(.~name, scales="free", labeller = labeller(
    name = c(Height_cwm = "Pflanzenhöhe [m]", 
             Seed_cwm = "Samenmasse [g]",
             SLA_cwm = "SLA [cm²/g]"))) +
  scale_x_discrete(labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"), 
                    labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 14),
    strip.placement = "outside",  
    panel.spacing = unit(1.5, "lines") 
  )


ggplot(Veg_traits_CWM) +
  geom_boxplot(aes(x=Bewirtsch, y=Height_cwm, fill= Bewirtsch)) +
  annotate("text",x= 1, y= 0.8, label = "a", size = 5) +
  annotate("text",x= 2, y= 0.8, label = "a", size = 5) +
  annotate("text",x= 3, y= 0.8, label = "b", size = 5) +
  labs(x = "Bewirtschaftung", y = "Mittlere Vegetationshöhe [m]", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"), 
                    labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag"))


mod6 <- lm(Height_cwm ~ Bewirtsch, data = Veg_traits_CWM)
plot(simulateResiduals(mod6))

kruskal.test(Height_cwm ~ Bewirtsch, data = Veg_traits_CWM)
dunn_test(Height_cwm ~ Bewirtsch, data = Veg_traits_CWM, p.adjust.method = "bonferroni")


ggplot(Veg_traits_CWM) +
  geom_boxplot(aes(x=Bewirtsch, y=Seed_cwm, fill = Bewirtsch)) +
  annotate("text",x= 1, y= 0.0045, label = "a", size = 5) +
  annotate("text",x= 2, y= 0.0045, label = "a", size = 5) +
  annotate("text",x= 3, y= 0.0045, label = "b", size = 5) +
  labs(x = "Bewirtschaftung", y = "Mittlere Samenmasse [g]", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"), 
                    labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag"))


mod7 <- lm(Seed_cwm ~ Bewirtsch, data = Veg_traits_CWM)
plot(simulateResiduals(mod7))

kruskal.test(Seed_cwm ~ Bewirtsch, data = Veg_traits_CWM)
dunn_test(Seed_cwm ~ Bewirtsch, data = Veg_traits_CWM, p.adjust.method = "bonferroni")


ggplot(Veg_traits_CWM) +
  geom_boxplot(aes(x=Bewirtsch, y=SLA_cwm, fill = Bewirtsch)) +
  annotate("text",x= 1, y= 252, label = "a", size = 5) +
  annotate("text",x= 2, y= 252, label = "a", size = 5) +
  annotate("text",x= 3, y= 252, label = "b", size = 5) +
  labs(x = "Bewirtschaftung", y = "Mittlere spezifische Blattfläche [cm²/g]", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"), 
                    labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag"))


mod8 <- lm(SLA_cwm ~ Bewirtsch, data = Veg_traits_CWM)
plot(simulateResiduals(mod8))

anova(mod8)
TukeyHSD(aov(SLA_cwm ~ Bewirtsch, data = Veg_traits_CWM))



#### Forschungsfrage 2 Vergleich Garchinger Heide ####
# Daten einlesen
Heide <- read.table("data_Bauer.csv", header = TRUE, dec = ",", sep = ";")
str(Heide)
summary(Heide)

plant_names_Heide <- TNRS(Heide$name)
Heide[1:108,]$name <- plant_names_Heide[1:108,]$Accepted_name
rownames(Heide) <- Heide$name
Heide[is.na(Heide)] <- 0

# Kennarten markieren
Heide_k <- Heide
Heide_k$Kennart_Mager <- ifelse(Heide_k$name %in% Magerrasen$`Magerrasen$Accepted_name`, "Kennart_Magerrasen", "")
Heide_k$Kennart_Wiese <- ifelse(Heide_k$name %in% Wiese$`Wiese$Accepted_name`, "Kennart_Wiese", "")
Heide_k$Kennart_ges <- ifelse(Heide_k$Kennart_Mager == "Kennart_Magerrasen" & Heide_k$Kennart_Wiese == "", "Kennart_Magerrasen",
                              ifelse(Heide_k$Kennart_Wiese == "Kennart_Wiese" & Heide_k$Kennart_Mager == "", "Kennart_Wiese",
                                     ifelse(Heide_k$Kennart_Wiese == "Kennart_Wiese" & Heide_k$Kennart_Mager == "Kennart_Magerrasen", "Kennart_Misch",
                                            "Rest")))

# Balkendiagramm Artenzahl
Heide_k_neu <- Heide_k %>%
  pivot_longer(cols=2:69, names_to="Plot", values_to="Abundanz")
Heide_k_neu[is.na(Heide_k_neu)] <- 0
Heide_k_neu <- Heide_k_neu[,c(-2,-3)]
Heide_k_neu$pa_wert <- decostand(Heide_k_neu$Abundanz, method="pa")
Heide_k_neu <- Heide_k_neu %>%
  group_by(Plot, Kennart_ges) %>%
  reframe(Kennartenzahl=sum(pa_wert[,1]))
Heide_k_neu$Bewirtsch <- "Heide"

# Vergleich der Kennarten mit der Bewirtschaftungsweise !nur mit Artenzahl
durchschnitt3 <- aggregate(Kennartenzahl ~ Kennart_ges + Bewirtsch, data = Heide_k_neu, FUN = mean)
ggplot(durchschnitt3) +
  geom_bar(aes(x=Bewirtsch,y=Kennartenzahl, fill=Kennart_ges), stat = "identity")

ggplot(Heide_k_neu) +
  geom_boxplot(aes(x=Kennart_ges, y=Kennartenzahl, fill=Bewirtsch)) +
  scale_fill_manual(values = c("#FF6A6A"))


#### Kennarten Vergleich Erweiterung Heide ####
# Tabellen Erweiterung und Heide zusammenfassen

Tab_Erweiterung_Heide <- rbind(Heide_k_neu, Veg_k_neu)

ggplot(Tab_Erweiterung_Heide) +
  geom_boxplot(aes(x=Kennart_ges, y=Kennartenzahl, fill=Bewirtsch)) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100")) 

df2 <- Tab_Erweiterung_Heide %>%
  pivot_wider(values_from = Kennartenzahl, names_from=Kennart_ges) %>%
  mutate(Kennart_Wiese = Kennart_Wiese + Kennart_Misch,
         Kennart_Magerrasen = Kennart_Magerrasen + Kennart_Misch) %>%
  pivot_longer(cols=c(3, 5:6))


ggplot(df2) +
  geom_boxplot(aes(x=name, y=value, fill=Bewirtsch)) +
  labs(x = "Kennart", y = "Anzahl Kennarten pro 4m²", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Kennarten für Magerrasen [84]", "Kennarten für Flachland-Mähwiese [66]", "Rest")) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"), 
                    labels = c("Garchinger Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    legend.position = "bottom",
    legend.title = element_text(size=14),
    legend.text = element_text(size=12)) 


ggplot(filter(Veg_k_neu, Kennart_ges=="Kennart_Magerrasen"|Kennart_ges=="Kennart_Misch")) +
  geom_boxplot(aes(x=Bewirtsch, y=Kennartenzahl)) +
  labs(x="Bewirtschaftung", y="Kennartenzahl Magerrasen") +
  annotate("text",x= 1, y= 16, label = "a", size = 5) +
  annotate("text",x= 2, y= 16, label = "a", size = 5) +
  annotate("text",x= 3, y= 16, label = "b", size = 5)


ggplot(filter(Tab_Erweiterung_Heide, Kennart_ges=="Kennart_Magerrasen"|Kennart_ges=="Kennart_Misch")) +
  geom_boxplot(aes(x=Bewirtsch, y=Kennartenzahl)) +
  labs(x="Bewirtschaftung", y="Kennartenzahl Magerrasen") +
  annotate("text",x= 1, y= 22, label = "a", size = 5) +
  annotate("text",x= 2, y= 22, label = "a", size = 5) +
  annotate("text",x= 3, y= 22, label = "a", size = 5) +
  annotate("text",x= 4, y= 22, label = "b", size = 5) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"))


mod10 <- lm(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Kennart_Magerrasen"|Kennart_ges == "Kennart_Misch"))
plot(simulateResiduals(mod10))

kruskal.test(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Kennart_Magerrasen"|Kennart_ges == "Kennart_Misch"))
dunn_test(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Kennart_Magerrasen"|Kennart_ges == "Kennart_Misch"), p.adjust.method = "bonferroni")


ggplot(filter(Tab_Erweiterung_Heide, Kennart_ges=="Kennart_Wiese"|Kennart_ges=="Kennart_Misch")) +
  geom_boxplot(aes(x=Bewirtsch, y=Kennartenzahl)) +
  labs(x="Bewirtschaftung", y="Kennartenzahl Flachland-Mähwiese") +
  annotate("text",x= 1, y= 12, label = "a", size = 5) +
  annotate("text",x= 2, y= 12, label = "b", size = 5) +
  annotate("text",x= 3, y= 12, label = "b", size = 5) +
  annotate("text",x= 4, y= 12, label = "a", size = 5) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"))

mod11 <- lm(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Kennart_Wiese"|Kennart_ges == "Kennart_Misch"))
plot(simulateResiduals(mod11))

kruskal.test(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Kennart_Wiese"|Kennart_ges == "Kennart_Misch"))
dunn_test(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Kennart_Wiese"|Kennart_ges == "Kennart_Misch"), p.adjust.method = "bonferroni")

mod11b <- lm(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Rest"))
plot(simulateResiduals(mod11b))

kruskal.test(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Rest"))
dunn_test(Kennartenzahl ~ Bewirtsch, data = filter(Tab_Erweiterung_Heide, Kennart_ges == "Rest"), p.adjust.method = "bonferroni")



#### Traits Vergleich Erweiterung Heide ####
traits_meta <- GIFT_traits_meta()
trait_values <- GIFT_traits(trait_IDs=c("1.6.3", "3.2.3", "4.1.3"))

trait_values2 <- filter(trait_values, work_species %in% rownames(Heide[1:108,])) %>%
  dplyr::select(c(2, 4:6))

Heide_traits_original <- Heide %>%
  pivot_longer(cols=2:69, names_to="Plot", values_to="Deckung") %>%
  merge(trait_values2, by.x="name", by.y="work_species")

Gesamtdeckung2 <- Heide_traits_original %>%
  group_by(Plot) %>%
  reframe(Gesamtdeckung=sum(as.numeric(Deckung)))

Heide_traits <- merge(Heide_traits_original, Gesamtdeckung2)

Heide_traits$plant_height <- (as.numeric(Heide_traits$Deckung)*Heide_traits$trait_value_1.6.3)
Heide_traits$seed_mass <- (as.numeric(Heide_traits$Deckung)*Heide_traits$trait_value_3.2.3)
Heide_traits$SLA <- (as.numeric(Heide_traits$Deckung)*Heide_traits$trait_value_4.1.3)

Heide_traits <- Heide_traits %>%
  group_by(Plot) %>%
  na.omit() %>%
  reframe(mean_plant_height=mean(plant_height),
          mean_seed_mass=mean(seed_mass),
          mean_SLA=mean(SLA))

Heide_traits$Bewirtsch <- "Heide"

Traits_Erweiterung_Heide <- rbind(Heide_traits, Veg_traits)

Heide_traits_original <- Heide_traits_original %>%
  rename(Art = name)
Heide_traits_original$Bewirtsch <- "Heide"
Veg_traits_orginal <- Veg_traits_orginal %>%
  left_join(Veg_t_neu, by = "Plot")

Traits_Erweiterung_Heide_original <- rbind(Heide_traits_original, Veg_traits_orginal)


### Traits mit CWM
#### Berechnung mit Community weight mean

Heide_traits_CWM <- 
  Traits_Erweiterung_Heide_original %>%
  group_by(Plot) %>%
  summarize(Height_cwm = weighted.mean(trait_value_1.6.3, as.numeric(Deckung), na.rm = TRUE),
            Seed_cwm = weighted.mean(trait_value_3.2.3, as.numeric(Deckung), na.rm = TRUE),
            SLA_cwm = weighted.mean(trait_value_4.1.3, as.numeric(Deckung),na.rm = TRUE))

Bewirtschaftung <- Traits_Erweiterung_Heide_original$Bewirtsch[match(Heide_traits_CWM$Plot, Traits_Erweiterung_Heide_original$Plot)]
Heide_traits_CWM <- cbind(Heide_traits_CWM, Bewirtsch = Bewirtschaftung)


Heide_traits_CWM %>%
  pivot_longer(cols=2:4) %>%
  ggplot() +
  geom_boxplot(aes(x=Bewirtsch, y=value, fill=Bewirtsch)) +
  labs(x = "Bewirtschaftung", y = "Community-weighted mean (Abundanz)", fill="Bewirtschaftung") +
  facet_wrap(.~name, scales="free", labeller = labeller(
    name = c(Height_cwm = "Pflanzenhöhe [m]", 
             Seed_cwm = "Samenmasse [g]",
             SLA_cwm = "SLA [cm²/g]"))) +
  scale_x_discrete(labels = c("Garchinger Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"), 
                    labels = c("Garchinger Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 14),
    strip.placement = "outside",  
    panel.spacing = unit(1.5, "lines"))



ggplot(Heide_traits_CWM) +
  geom_boxplot(aes(x=Bewirtsch, y=Height_cwm, fill= Bewirtsch)) +
  annotate("text",x= 1, y= 0.8, label = "a", size = 5) +
  annotate("text",x= 2, y= 0.8, label = "a", size = 5) +
  annotate("text",x= 3, y= 0.8, label = "b", size = 5) +
  labs(x = "Bewirtschaftung", y = "Mittlere Vegetationshöhe [m]", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"), 
                    labels = c("Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) 



mod20 <- lm(Height_cwm ~ Bewirtsch, data = Heide_traits_CWM)
plot(simulateResiduals(mod20))

kruskal.test(Height_cwm ~ Bewirtsch, data = Heide_traits_CWM)
dunn_test(Height_cwm ~ Bewirtsch, data = Heide_traits_CWM, p.adjust.method = "bonferroni")


ggplot(Heide_traits_CWM) +
  geom_boxplot(aes(x=Bewirtsch, y=Seed_cwm, fill = Bewirtsch)) +
  annotate("text",x= 1, y= 0.0045, label = "a", size = 5) +
  annotate("text",x= 2, y= 0.0045, label = "a", size = 5) +
  annotate("text",x= 3, y= 0.0045, label = "b", size = 5) +
  labs(x = "Bewirtschaftung", y = "Mittlere Samenmasse [g]", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"), 
                    labels = c("Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag"))


mod21 <- lm(Seed_cwm ~ Bewirtsch, data = Heide_traits_CWM)
plot(simulateResiduals(mod21))

kruskal.test(Seed_cwm ~ Bewirtsch, data = Heide_traits_CWM)
dunn_test(Seed_cwm ~ Bewirtsch, data = Heide_traits_CWM, p.adjust.method = "bonferroni")


ggplot(Heide_traits_CWM) +
  geom_boxplot(aes(x=Bewirtsch, y=SLA_cwm, fill = Bewirtsch)) +
  annotate("text",x= 1, y= 252, label = "a", size = 5) +
  annotate("text",x= 2, y= 252, label = "a", size = 5) +
  annotate("text",x= 3, y= 252, label = "b", size = 5) +
  labs(x = "Bewirtschaftung", y = "Mittlere spezifische Blattfläche [cm²/g]", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"), 
                    labels = c("Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag"))


mod22 <- lm(SLA_cwm ~ Bewirtsch, data = Heide_traits_CWM)
plot(simulateResiduals(mod22))

kruskal.test(SLA_cwm ~ Bewirtsch, data = Heide_traits_CWM)
dunn_test(SLA_cwm ~ Bewirtsch, data = Heide_traits_CWM, p.adjust.method = "bonferroni")


#### Ordination Erweiterung Heide ####
Heide_Ordi <- Heide[,-1]
Heide_Ordi <- as.data.frame(t(Heide_Ordi))

Veg_Ordi <- Veg_Ordi %>% rownames_to_column(var="Plot")
Heide_Ordi <- Heide_Ordi %>% rownames_to_column(var="Plot")

Heide_Ordi <- cbind(Heide_Ordi$Plot, as.data.frame(lapply(Heide_Ordi[2:108], as.numeric)))

#Heide_Ordi_x <- Heide_Ordi %>%
#mutate(across(-name, ~ as.numeric(.x))) 5>%+
#arrange(name)
#Heide_Ordi$plot %in% env$plot
#table(Heide_Ordi$plot == env$plot)


Ordi_Erweiterung_Heide <- full_join(Veg_Ordi, Heide_Ordi)


Ordi_Erweiterung_Heide <- Ordi_Erweiterung_Heide %>%    #Alle Plotbezeichnungen in eine Spalte
  mutate(Plot = coalesce(Plot, `Heide_Ordi$Plot`))

Ordi_Erweiterung_Heide <- Ordi_Erweiterung_Heide[,-152]  #entfernen Plotbezeichnungsspalte Heide

Ordi_Erweiterung_Heide <- Ordi_Erweiterung_Heide %>% column_to_rownames(var = "Plot")

Ordi_Erweiterung_Heide[is.na(Ordi_Erweiterung_Heide)]<-0

# wie viele Arten kommen nur 1x vor
sum(colSums(decostand(Ordi_Erweiterung_Heide, method = "pa")) < 2)
# 39 Arten kommen nur 1x vor
Ordi_Erweiterung_Heide2 <- Ordi_Erweiterung_Heide[,colSums(decostand(Ordi_Erweiterung_Heide, method = "pa")) > 1] 
sum(colSums(decostand(Ordi_Erweiterung_Heide2, method = "pa")) < 2)  

Ordi_Erweiterung_Heide.sqrt <- sqrt(Ordi_Erweiterung_Heide2)         # Vegetationstabelle wurzeltransformieren
summary(Ordi_Erweiterung_Heide.sqrt)                


Vegtabelle2.NMDS <- metaMDS(Ordi_Erweiterung_Heide.sqrt, k = 2) 
plot(Vegtabelle2.NMDS)                   # Ordinationsdiagramm anzeigen - nur Symbole
plot(Vegtabelle2.NMDS, type = "t")       # Ord.diagramm mit Text: unuebersichtlich
# stress von 12% konnte nicht passend abgebildet werden 

# pruefen, welche Anzahl an Dimensionen gut geeignet ist
vegtab.nmds1 <- metaMDS(Ordi_Erweiterung_Heide.sqrt, k = 1)
vegtab.nmds2 <- metaMDS(Ordi_Erweiterung_Heide.sqrt, k = 2)
vegtab.nmds3 <- metaMDS(Ordi_Erweiterung_Heide.sqrt, k = 3)
vegtab.nmds4 <- metaMDS(Ordi_Erweiterung_Heide.sqrt, k = 4)
vegtab.nmds5 <- metaMDS(Ordi_Erweiterung_Heide.sqrt, k = 5)
dimensionen <- 1:5
stress.dim <- c(vegtab.nmds1$stress, vegtab.nmds2$stress, vegtab.nmds3$stress,
                vegtab.nmds4$stress, vegtab.nmds5$stress)
plot(stress.dim ~ dimensionen, type = "b") 
# 2 Dimensionen

Ordi_Erweiterung_Heide.sqrt <- Ordi_Erweiterung_Heide.sqrt %>%
  rownames_to_column(var="Plot") %>%
  merge(Traits_Erweiterung_Heide[, c(1, 5)], by="Plot") %>%
  column_to_rownames(var="Plot")

adonis2(Ordi_Erweiterung_Heide.sqrt[1:148] ~ Bewirtsch, data=Ordi_Erweiterung_Heide.sqrt)

# Uebersichtsdiagramm mit Flaechenbezeichnung
plot(Vegtabelle2.NMDS, type = "n")       
orditorp(Vegtabelle2.NMDS, display = "sites", cex = 1.3, col = "navy") # Kreis bedeuten Ueberlappung
cnam <- make.cepnames(names(Ordi_Erweiterung_Heide2)) # abgekuerzte Artnamen erzeugen jeweils 4 Zeichen Gattung Art
orditorp(Vegtabelle2.NMDS, display = "species", labels = cnam, 
         col = "tomato")

# NUTZUNG #
plot(Vegtabelle2.NMDS, type ="n")
orditorp(Vegtabelle2.NMDS, display = "sites", col = "navy", labels = FALSE) 


Vegtabelle2.NMDS <- scores(Vegtabelle2.NMDS, display = "sites") 
Vegtabelle2.NMDS <- Vegtabelle2.NMDS %>%
  as.data.frame() %>%
  rownames_to_column(var = "Plot") %>%
  arrange(desc(Plot)) 


Vegtabelle2.NMDS_plot <- Vegtabelle2.NMDS %>%
  merge(Traits_Erweiterung_Heide[, c(1, 5)], by="Plot") %>%
  arrange(desc(Plot)) %>%
  column_to_rownames(var="Plot")

Vegtabelle2.NMDS <- Vegtabelle2.NMDS %>% 
  column_to_rownames(var="Plot")

plot(Vegtabelle2.NMDS, type ="n")
orditorp(Vegtabelle2.NMDS, display = "sites", col = "navy", labels = FALSE) 

points(Vegtabelle2.NMDS[Vegtabelle2.NMDS_plot$Bewirtsch == "VNP Schnitt 1.7.",], pch = 22, bg = "#66ff00", cex = 1.5)
points(Vegtabelle2.NMDS[Vegtabelle2.NMDS_plot$Bewirtsch == "VNP Schnitt 1.9.",], pch = 21, bg = "#308400", cex = 1.5)
points(Vegtabelle2.NMDS[Vegtabelle2.NMDS_plot$Bewirtsch == "VNP Weide",], pch = 23, bg = "#ff9100", cex = 1.5)
points(Vegtabelle2.NMDS[Vegtabelle2.NMDS_plot$Bewirtsch == "Heide",], pch = 24, bg = "#FF6A6A", cex = 1.5)
legend(-0.4,-0.3, legend = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag", "Garchinger Heide"), pch = c(22,21,23,24), 
       pt.bg = c("#66ff00", "#308400","#ff9100", "#FF6A6A"), cex = 1, pt.cex = 1, title = "Bewirtschaftung")

ordiellipse(Vegtabelle2.NMDS, Vegtabelle2.NMDS_plot$Bewirtsch, draw="polygon", label = TRUE)


#### Brachypodium pinnatum ####
Brachypodium <- Ordi_Erweiterung_Heide.sqrt %>%
  select(Bewirtsch, Brachypodium.pinnatum)
Brachypodium$Brachypodium.pinnatum <- Brachypodium$Brachypodium.pinnatum^2

ggplot(data = Brachypodium ) +
  geom_boxplot(aes(x=Bewirtsch , y= Brachypodium.pinnatum, fill = Bewirtsch)) +
  labs(x = "Bewirtschaftung", y = "Deckung Brachypodium pinnatum [%]", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Garchinger Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"), 
                    labels = c("Garchinger Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 14)) +
  annotate("text",x= 1, y= 45, label = "a", size = 5) +
  annotate("text",x= 2, y= 45, label = "a", size = 5) +
  annotate("text",x= 3, y= 45, label = "a", size = 5) +
  annotate("text",x= 4, y= 45, label = "b", size = 5) 




mod30 <- lm(Brachypodium.pinnatum ~ Bewirtsch, data = Brachypodium)
plot(simulateResiduals(mod30))

kruskal.test(Brachypodium.pinnatum ~ Bewirtsch, data = Brachypodium)
dunn_test(Brachypodium.pinnatum ~ Bewirtsch, data = Brachypodium, p.adjust.method = "bonferroni")


#### Brachypodium pinnatum nur Erweiterungsflaechen####
Brachypodium2 <- Veg_t %>%
  select(Bewirtsch, `Brachypodium pinnatum`)

str(Brachypodium2)

Brachypodium2$`Brachypodium pinnatum` <- as.numeric(Brachypodium2$`Brachypodium pinnatum`)

ggplot(data = Brachypodium2 ) +
  geom_boxplot(aes(x=Bewirtsch , y= `Brachypodium pinnatum`, fill = Bewirtsch)) +
  labs(x = "Bewirtschaftung", y = "Deckung Brachypodium pinnatum agg. [%]", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#66ff00","#308400","#ff9100"), 
                    labels = c("Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 14)) +
  annotate("text",x= 1, y= 45, label = "a", size = 5) +
  annotate("text",x= 2, y= 45, label = "a", size = 5) +
  annotate("text",x= 3, y= 45, label = "b", size = 5) 



mod31 <- lm(`Brachypodium pinnatum` ~ Bewirtsch, data = Brachypodium2)
plot(simulateResiduals(mod31))

kruskal.test(`Brachypodium pinnatum` ~ Bewirtsch, data = Brachypodium2)
dunn_test(`Brachypodium pinnatum` ~ Bewirtsch, data = Brachypodium2, p.adjust.method = "bonferroni")



##### Rote Liste Arten
RoteListe <- readxl::read_excel(
  "02_Datentabelle_RL_Farn-_und_Bluetenpflanzen_2018_Deutschland_20210317-1607.xlsx")
str(RoteListe)
summary(RoteListe)

RoteListe_plant_names <- TNRS(RoteListe$Name)


RoteListe[1:5312,]$Name <- RoteListe_plant_names[1:5312,]$Accepted_name

Vegetationstabelle_gesamt <- as.data.frame(t(Ordi_Erweiterung_Heide))
Vegetationstabelle_gesamt <- rownames_to_column(Vegetationstabelle_gesamt)

RoteListe_kurz <- RoteListe[, c("Name", "RL Kat.")]
RoteListe_kurz$Name <- gsub(" ", ".", RoteListe_kurz$Name)



Vegetation_RoteListe <- merge(RoteListe_kurz, Vegetationstabelle_gesamt, by.x = "Name", by.y = "rowname")
str(Vegetation_RoteListe)

Vegetation_RoteListe[,c(-1,-2)] <- ifelse(Vegetation_RoteListe[,c(-1,-2)] > 0, 1, 0)


Veg_RL <- Vegetation_RoteListe  %>%
  pivot_longer(cols=3:160, names_to="Plot", values_to="Abundanz")

Veg_RL <- Veg_RL %>%
  group_by(Plot, `RL Kat.`) %>%
  reframe(AnzahlRLArten = sum(Abundanz))

Vegtabelle2.NMDS_plot <- rownames_to_column(Vegtabelle2.NMDS_plot)
Bewirtschaftung_gesamt <- Vegtabelle2.NMDS_plot[,c(1,4)]

Veg_RL_Bewirtsch <- merge(Veg_RL, Bewirtschaftung_gesamt, by.x = "Plot", by.y = "rowname")


nichtRL <- c("*", "D", "nb", NA)
Veg_RL_Bewirtsch_nurRL <- Veg_RL_Bewirtsch %>% filter(!`RL Kat.` %in% nichtRL)


Veg_RL_Bewirtsch_nurRL <- Veg_RL_Bewirtsch_nurRL %>%
  group_by(Plot,Bewirtsch) %>%
  reframe(GesamtanzahlRLArten = sum(AnzahlRLArten))

x <- Veg_RL_Bewirtsch %>%
  group_by(Bewirtsch, `RL Kat.`) %>%
  summarise(Anzahl = round(mean(AnzahlRLArten), 2)) %>%
  pivot_wider(names_from=`RL Kat.`, values_from=Anzahl)

write.csv2(x,"Durchschnittliche Rote Liste Arten pro Bewirtschaftung.csv")


ggplot(Veg_RL_Bewirtsch) +
  geom_boxplot(aes(x = Bewirtsch, y = AnzahlRLArten, fill = `RL Kat.`))


ggplot(Veg_RL_Bewirtsch_nurRL) +
  geom_boxplot(aes(x = Bewirtsch, y = GesamtanzahlRLArten, fill = Bewirtsch)) +
  ylim(0, NA) +
  annotate("text",x= 1, y= 40, label = "a", size = 5) +
  annotate("text",x= 2, y= 40, label = "b", size = 5) +
  annotate("text",x= 3, y= 40, label = "b", size = 5) +
  annotate("text",x= 4, y= 40, label = "b", size = 5) +
  labs(x = "Bewirtschaftung", y = "Anzahl der Rote Liste Arten pro 4m²", fill="Bewirtschaftung") +
  scale_x_discrete(labels = c("Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  scale_fill_manual(values = c("#FF6A6A","#66ff00","#308400","#ff9100"), 
                    labels = c("Heide","Schnitt 1.7.", "Schnitt 1.9.", "Oberbodenabtrag")) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text = element_text(size = 14)) 

mod40 <- lm(GesamtanzahlRLArten ~ Bewirtsch, data = Veg_RL_Bewirtsch_nurRL)
plot(simulateResiduals(mod40))

anova(mod40)
TukeyHSD(aov(GesamtanzahlRLArten ~ Bewirtsch, data = Veg_RL_Bewirtsch_nurRL))

