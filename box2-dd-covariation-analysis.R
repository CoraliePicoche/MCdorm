# Code to run simulations and make figures for
# NI Wisnoski, MA Leibold, JT Lennon. "Dormancy in Metacommunities". American Naturalist.
# Author: Nathan I Wisnoski (wisnoski@indiana.edu)
# Date: 21 March 2019
# Code required to make Box 2 Figure 
# Link to data is located in Appendix B.


require(tidyverse)
require(XML)
require(ggrepel)
library(cluster)
library(vegan)
library(scatterpie)
library(pander)

# Read in the data from the repository
all.traits <- read_csv("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A7d88ecb9-2e7a-42da-af74-3ab4273d8cad", 
                   col_types = cols(
                     "species_id" = col_character()
                   ))

metadata <- read_file("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/doi%3A10.5063%2FF1VD6WMF")
md <- xmlToList(metadata)

  
# Subset traits of interest
# DM1 = passive dispersal, DM2 = active dispersal, RF3 = diapause/dormancy
all.traits <- na.omit(all.traits)

taxa.list <- all.traits %>%
  select(species_id, taxon_name, taxon_number, RF3, DM1, DM2) %>%
  group_by(taxon_name) %>%
  count() %>%
  arrange(desc(n))
# 
# 

# Normalize by ranks
all.traits[,-c(1:3)] <- decostand(all.traits[,-c(1:3)], method = "rank", MARGIN = 2)

# # extract just dispersal and dormancy traits
dd.traits <- all.traits %>%
  filter(taxon_name %in% taxa.list$taxon_name) %>%
  select(species_id, taxon_name, taxon_number, RF3, DM1, DM2) %>% na.omit()


taxanames <- dd.traits$taxon_name
traits <- dd.traits %>% 
  column_to_rownames(var = "species_id") %>% 
  select(RF3, DM1, DM2)

# Performe Fuzzy Clustering
trait.sum <- traits %>% group_by(RF3, DM1, DM2) %>% count()

# Average silhouette width across range of k values
asw <- numeric(nrow(trait.sum))
for(k in 2:(.5*length(asw)-1)){
  asw[k] <- fanny(traits, k = k)$silinfo$avg.width
}
plot(asw, xlim = c(0,20))
k <- 3 # chosing k = 3 because high ASW yet a reasonable # of groups
traits.fuz <- fanny(scale(traits), k = k)
summary(traits.fuz)
traitsfuz.g <- traits.fuz$clustering

plot(
  silhouette(traits.fuz),
  main = "Silhouette plot - fuzzy clustering",
  cex.names = 0.8,
  col = traits.fuz$silinfo$widths + 1
)

# Make PCA to visualize taxa groups
cols <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
traits.pcoa <- rda(traits,  scale = T)
traits.scores <- scores(traits.pcoa, display = "sites")
traits.vecs <- scores(traits.pcoa, display = "species")
rownames(traits.vecs) <- c("Dormancy", "Passive dispersal", "Active dispersal")
(var1 <- round(eigenvals(traits.pcoa)[1] / sum(eigenvals(traits.pcoa)), 3) * 100)
(var2 <- round(eigenvals(traits.pcoa)[2] / sum(eigenvals(traits.pcoa)), 3) * 100)


# Avg. Silhouette width for our clusters
traits.fuz$silinfo$avg.width

# Count how many taxa in each position along PC1 and 2
clusters <- cbind.data.frame(traits.scores, traits.fuz$membership)
colnames(clusters) <- c("PC1", "PC2", paste0("Cluster ",1:k))
clusters$hard <- traits.fuz$clustering
cluster.counts <- clusters %>% round(., 3) %>% 
  group_by(PC1_fact = as.factor(PC1), PC2_fact = as.factor(PC2)) %>% 
  count() %>% ungroup() %>% 
  mutate(prop = n/sum(n)) %>% 
  mutate(PC1 = as.numeric(levels(PC1_fact))[PC1_fact], 
         PC2 = as.numeric(levels(PC2_fact))[PC2_fact])
clusters <- unique(round(clusters, 3))
clusters <- left_join(clusters, cluster.counts)

# Labels for vectors
trait.labs <- as.data.frame(traits.vecs) %>% 
  rownames_to_column(var = "trait") %>% 
  mutate(origin = 0)


# Make Figure
scale.arrows <- .06 # scaling for vectors
ggplot() +
  geom_hline(alpha = 0.2, linetype = "dashed", aes(yintercept = 0)) +
  geom_vline(alpha = 0.2, linetype = "dashed", aes(xintercept = 0)) +
  geom_scatterpie(data = clusters, 
                  aes(x = PC1, y = PC2, group = hard, r = .2*sqrt(prop/pi)), 
                  cols = paste0("Cluster ",1:k),
                  color = "gray90", size = .1,
                  alpha = 1) +
  scale_fill_manual(values = cols[c(4,5,1)]) +
  geom_segment(data = trait.labs, 
               aes(x = origin, y = origin, 
                   xend = scale.arrows*PC1, 
                   yend = scale.arrows*PC2),
               alpha = 1, color = "black",
               arrow = arrow(angle = 20,
                             length = unit(.1, "inches"),
                             type = "open")) +
  geom_text_repel(data = trait.labs,
                   aes(x = scale.arrows*PC1, 
                       y = scale.arrows*PC2, label = trait),
                   color = "black",
                   segment.alpha = 0, 
                  point.padding = .2, 
                  direction = "y", nudge_y = .01, nudge_x = -.01) +
  coord_fixed() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA),
        legend.position = "top",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  labs(x = paste0("PC1 (", var1,"%)"),
       y = paste0("PC2 (", var2,"%)"),
       fill = "") +
  ggsave("figures/box2.pdf", width = 5, height = 5) +
  ggsave("figures/box2.png", dpi = 1000, width = 5, height = 5)


# calculate rank correlations between dispersal and dormancy vals 
gr <- colorRampPalette(RColorBrewer::brewer.pal(3, "RdBu"))

all.trait.corr <- all.traits %>% 
  select(-species_id, -taxon_name, -taxon_number) %>% 
  na.omit() %>% 
  select(starts_with("RF3"), starts_with("DM")) %>% 
  psych::corr.test(., method = "spearman", adjust = "holm")
corrplot::corrplot(all.trait.corr$r, method = "ellipse", 
                   tl.col = "black", p.mat = all.trait.corr$p, 
                   col = gr(200), type = "lower", diag = TRUE, 
                   addCoef.col = "black", 
                   tl.srt = 0, tl.offset = 1, cl.pos = "n", tl.pos = "ld")


# print the taxa in each cluster
taxa.by.cluster <- tibble(Taxon = map_chr(taxanames, str_replace_all, pattern =  "_", replacement =  " "), 
  Cluster = traits.fuz$clustering) %>% 
  group_by(Cluster, Taxon) %>% count(Taxon) %>% ungroup()

taxa.by.cluster %>% 
  filter(Cluster == 1) %>% select(-Cluster) %>% 
  arrange(desc(n)) %>% pander()

taxa.by.cluster %>% 
  filter(Cluster == 2) %>% select(-Cluster) %>% 
  arrange(desc(n)) %>% pander()

taxa.by.cluster %>% 
  filter(Cluster == 3) %>% select(-Cluster) %>% 
  arrange(desc(n)) %>% pander()

