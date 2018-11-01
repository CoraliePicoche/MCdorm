# Code required to make Box 3 Figure 1 
# Link to data is located in Supplement 1. 

require(tidyverse)
require(XML)
require(cowplot)
traits <- read_csv("trait_data.csv")
metadata <- xmlParse("bromeliad_invert_traits.xml")
md <- xmlToList(metadata)
md$dataset$dataTable$attributeList$attribute$measurementScale$nominal$nonNumericDomain$textDomain$definition

    
# DM1 = passive dispersal, DM2 = active dispersal, RF3 = diapause/dormancy

taxa.list <- traits %>% select(species_id, taxon_name, taxon_number, RF3, DM1, DM2) %>% 
  group_by(taxon_name) %>% count() %>% arrange(desc(n)) %>% filter(n > 3) %>% select(taxon_name)


# extract just these traits
dd.traits <- traits %>% filter(taxon_name %in% taxa.list$taxon_name) %>% 
  select(species_id, taxon_name, taxon_number, RF3, DM1, DM2) %>% na.omit()

# convert to factors
dd.traits$Dormancy <- factor(dd.traits$RF3, levels = c(0,1,2,3), 
       labels = c("None", "Low", "Intermediate", "High"), ordered = T)

dd.traits$Passive <- factor(dd.traits$DM1, levels = c(0,1,2,3), 
                             labels = c("None", "Low", "Intermediate", "High"), ordered = T)

dd.traits$Active <- factor(dd.traits$DM2, levels = c(0,1,2,3), 
                             labels = c("None", "Low", "Intermediate", "High"), ordered = T)

dd.traits <- select(dd.traits, -RF3, -DM1, -DM2)

# make figure for passive dispersal
passive.fig <- dd.traits %>%
  count(Dormancy, Passive) %>% complete(Dormancy, Passive, fill = list(n = 0)) %>% 
  mutate(Proportion = n/sum(n)) %>% 
  ggplot(aes(Dormancy, Passive)) + 
  geom_tile(aes(fill = Proportion), show.legend = F) + 
  geom_label(aes(label = round(Proportion,2)), alpha = 0.8) +
  scale_fill_continuous("", low = "white", high = "black") + 
  theme_minimal() +
  labs(x = "Dormancy Capacity", y = "Passive Dispersal") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(0,0,0,0), units = "in"))

# make figure for active dispersal
active.fig <- dd.traits %>%
  count(Dormancy, Active) %>% complete(Dormancy, Active, fill = list(n = 0)) %>% 
  mutate(Proportion = n/sum(n)) %>% 
  ggplot(aes(Dormancy, Active)) + 
  geom_tile(aes(fill = Proportion), show.legend = F) + 
  geom_label(aes(label = round(Proportion,2)), alpha = 0.8) +
  scale_fill_continuous("", low = "white", high = "black") + 
  theme_minimal() +
  labs(x = "Dormancy Capacity", y = "Active Dispersal") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.margin = unit(c(0,0,0,0), units = "in"))
  
# combine figures
plot_grid(passive.fig + 
            theme(axis.line.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.y = element_text(margin = margin(r = 10))),
          active.fig + 
            theme(axis.title.x = element_text(margin = margin(t = 10)),
                  axis.title.y = element_text(margin = margin(r = 10))), align = "hv", nrow = 2) +
  theme(plot.margin = unit(c(.2,.2,.2,.2), units = "in"))
  
