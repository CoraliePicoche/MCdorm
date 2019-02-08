# Code required to make Box 3 Figure 1 
# Link to data is located in Supplement 1. 

require(tidyverse)
require(XML)
require(ggrepel)
require(cowplot)
library(cluster)
library(vegan)
library(scatterpie)

traits <- read_csv("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A7d88ecb9-2e7a-42da-af74-3ab4273d8cad", 
                   col_types = cols(
                     "species_id" = col_character()
                   ))

metadata <- read_file("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/doi%3A10.5063%2FF1VD6WMF")

md <- xmlToList(metadata)
md$dataset$dataTable$attributeList$attribute$measurementScale$nominal$nonNumericDomain$textDomain$definition

    
# DM1 = passive dispersal, DM2 = active dispersal, RF3 = diapause/dormancy

taxa.list <- traits %>%
  select(species_id, taxon_name, taxon_number, RF3, DM1, DM2) %>%
  group_by(taxon_name) %>%
  count() %>%
  arrange(desc(n))
# 
# 
# # extract just these traits
dd.traits <- traits %>%
  filter(taxon_name %in% taxa.list$taxon_name) %>%
  select(species_id, taxon_name, taxon_number, RF3, DM1, DM2) %>% na.omit()

# 
# 
# # convert to factors
# dd.traits$Dormancy <- factor(dd.traits$RF3, levels = c(0,1,2,3),
#        labels = c("None", "Low", "Intermediate", "High"), ordered = T)
# 
# dd.traits$Passive <- factor(dd.traits$DM1, levels = c(0,1,2,3),
#                              labels = c("None", "Low", "Intermediate", "High"), ordered = T)
# 
# dd.traits$Active <- factor(dd.traits$DM2, levels = c(0,1,2,3),
#                              labels = c("None", "Low", "Intermediate", "High"), ordered = T)
# 
# dd.traits <- select(dd.traits, -RF3, -DM1, -DM2)
# # dd.traits <- dd.traits %>% filter(Dormancy != "None", Passive != "None", Active != "None")
# 
# dd.traits$Dormancy <- factor(dd.traits$RF3, levels = c(1,2,3), 
#                              labels = c("Low", "Intermediate", "High"), ordered = T)
# 
# dd.traits$Passive <- factor(dd.traits$DM1, levels = c(1,2,3), 
#                             labels = c("Low", "Intermediate", "High"), ordered = T)
# 
# dd.traits$Active <- factor(dd.traits$DM2, levels = c(1,2,3), 
#                            labels = c("Low", "Intermediate", "High"), ordered = T)
# 
# 
# # Spearman rank correlation
# cor.passive <- cor.test(x = as.numeric(dd.traits$Dormancy), y = as.numeric(dd.traits$Passive), method = "spearman")
# cor.active <- cor.test(x = as.numeric(dd.traits$Dormancy), y = as.numeric(dd.traits$Active), method = "spearman")
# cor.disp <- cor.test(x = as.numeric(dd.traits$Passive), y = as.numeric(dd.traits$Active), method = "spearman")
# 
# rho.passive <- as.character(round(cor.passive$estimate, 2))
# rho.active <- as.character(round(cor.active$estimate, 2))
# rho.disp <- as.character(round(cor.disp$estimate, 2))
# 
# # make figure for passive dispersal
# passive.fig <- dd.traits %>%
#   count(Dormancy, Passive) %>% 
#   complete(Dormancy, Passive, fill = list(n = 0)) %>% 
#   mutate(Proportion = n/sum(n)) %>% 
#   ggplot(aes(Dormancy, Passive)) + 
#   geom_tile(aes(fill = Proportion), show.legend = F) + 
#   geom_label(aes(label = round(Proportion,2)), alpha = 0.8) +
#   scale_fill_continuous("", low = "white", high = "black") + 
#   theme_minimal() +
#   labs(x = "Dormancy Capacity", y = "Passive Dispersal", 
#        title = paste("Spearman's ρ =", eval(rho.passive))) +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         plot.margin = unit(c(0,0,0,0), units = "in"))
# passive.fig
# 
# # make figure for active dispersal
# active.fig <- dd.traits %>%
#   count(Dormancy, Active) %>% 
#   complete(Dormancy, Active, fill = list(n = 0)) %>% 
#   mutate(Proportion = n/sum(n)) %>% 
#   ggplot(aes(Dormancy, Active)) + 
#   geom_tile(aes(fill = Proportion), show.legend = F) + 
#   geom_label(aes(label = round(Proportion,2)), alpha = 0.8) +
#   scale_fill_continuous("", low = "white", high = "black") + 
#   theme_minimal() +
#   labs(x = "Dormancy Capacity", y = "Active Dispersal",
#        title = paste("Spearman's ρ =", eval(rho.active))) +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         plot.margin = unit(c(0,0,0,0), units = "in"))
# active.fig  
# 
# dd.traits %>%
#   count(Passive, Active) %>% 
#   complete(Passive, Active, fill = list(n = 0)) %>% 
#   mutate(Proportion = n/sum(n)) %>% 
#   ggplot(aes(Passive, Active)) + 
#   geom_tile(aes(fill = Proportion), show.legend = F) + 
#   geom_label(aes(label = round(Proportion,2)), alpha = 0.8) +
#   scale_fill_continuous("", low = "white", high = "black") + 
#   theme_minimal() +
#   labs(x = "Passive Dispersal", y = "Active Dispersal") +
#   theme(axis.text = element_text(size = 14),
#         axis.title = element_text(size = 16),
#         plot.margin = unit(c(0,0,0,0), units = "in"))
# 
# # combine figures
# plot_grid(passive.fig + 
#             theme(axis.line.x = element_blank(),
#                   axis.title.x = element_blank(),
#                   axis.text.x = element_blank(),
#                   axis.title.y = element_text(margin = margin(r = 10))),
#           active.fig + 
#             theme(axis.title.x = element_text(margin = margin(t = 10)),
#                   axis.title.y = element_text(margin = margin(r = 10))), align = "hv", nrow = 2) +
#   theme(plot.margin = unit(c(.2,.2,.2,.2), units = "in"))
  
taxanames <- dd.traits$taxon_name
traits <- dd.traits %>% 
  column_to_rownames(var = "species_id") %>% 
  select(RF3, DM1, DM2)

# #library(vegan)
# ord <- cca(traits)
# point <- scores(ord, display = "site")
# labels <- as_tibble(point) %>% add_column(taxanames) %>% 
#   mutate(taxanames = str_extract(taxanames, "[A-Z][a-z]+")) %>% 
#   group_by(taxanames) %>% summarize(meanca1 = mean(CA1), meanca2 = mean(CA2))
# vec <- scores(ord, display = "species")
# rownames(vec) <- c("Dormancy", "Passive dispersal", "Active dispersal")
# vecs <- as.data.frame(vec) %>% rownames_to_column(var = "trait") %>% 
#   mutate(origin = 0)
# 
# as_tibble(point) %>% 
#   ggplot(aes(x = CA1, y = CA2)) + 
#   geom_hline(alpha = 0.2, linetype = "dashed", aes(yintercept = 0)) +
#   geom_vline(alpha = 0.2, linetype = "dashed", aes(xintercept = 0)) +
#   geom_point(alpha = 0.2) +
#   geom_segment(data = vecs, 
#                aes(x = origin, y = origin, xend = CA1, yend = CA2),
#                alpha = 0.5, color = "red",
#                arrow = arrow(angle = 20, length = unit(.1, "inches"), type = "open")) +
#   geom_text_repel(data = labels, 
#                   aes(x = meanca1, y = meanca2, label = taxanames), 
#                   alpha = 0.6, size = 2.5, segment.alpha = 0.3, max.iter = 10000) +
#   geom_label_repel(data = vecs, aes(x = CA1, y = CA2),
#                    label = vecs$trait, color = "red",
#                    segment.alpha = 1) +
#   coord_fixed() +
#   scale_x_continuous(limits = c(-4, 4)) +
#   scale_y_continuous(limits = c(-3, 3)) +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),
#         panel.border = element_rect(fill = NA)) +
#   ggsave("figures/box3.pdf", width = 8, height = 8, units = "in") +
#   ggsave("figures/box3.png", width = 8, height = 8, units = "in", dpi = 500)


# Fuzzy Clustering
trait.sum <- traits %>% group_by(RF3, DM1, DM2) %>% count()

asw <- numeric(nrow(trait.sum))
for(k in 2:(.5*length(asw)-1)){
  asw[k] <- fanny(traits, k = k)$silinfo$avg.width
}
plot(asw)
k =3
traits.fuz <- fanny(scale(traits), k = k)
summary(traits.fuz)
traitsfuz.g <- traits.fuz$clustering

plot(
  silhouette(traits.fuz),
  main = "Silhouette plot - fuzzy clustering",
  cex.names = 0.8,
  col = traits.fuz$silinfo$widths + 1
)

cols <- cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
traits.pcoa <- rda(traits)
traits.scores <- scores(traits.pcoa, display = "sites")
traits.vecs <- scores(traits.pcoa, display = "species")
rownames(traits.vecs) <- c("Dormancy", "Passive dispersers", "Active dispersers")
(var1 <- round(eigenvals(traits.pcoa)[1] / sum(eigenvals(traits.pcoa)), 3) * 100)
(var2 <- round(eigenvals(traits.pcoa)[2] / sum(eigenvals(traits.pcoa)), 3) * 100)


# pdf("figures/fuzzy-clusters.pdf")
# plot(traits.scores,
#      asp = 1, 
#      type = "n",
#      xlim = c(min(traits.scores[,1]) - 0.16, max(traits.scores[,1]) + 0.1),
#      ylim = c(min(traits.scores[,2]) - 0.1, max(traits.scores[,2]) + 0.1))
# abline(h = 0, lty = "dotted")
# abline(v = 0, lty = "dotted")
# box(lwd = 2)
# for(i in 1:k){
#   gg <- traits.scores[traitsfuz.g == i,]
#   hpts <- chull(gg)
#   hpts <- c(hpts, hpts[1])
#   lines(gg[hpts, ], col = cols[i])
# }
# stars(
#   traits.fuz$membership,
#   location = traits.scores,
#   # key.loc = c(0.6, 0.6),
#   # key.labels = 1:k,
#   draw.segments = TRUE,
#   add = TRUE,
#   len = 0.075, 
#   col.segments = cols,
#   labels = NULL
# )
# arrows(0,0, 0.1*traits.vecs[,1], 0.1*traits.vecs[,2], 
#        lwd = 2, lty = 1, length = 0.2, col = "black")
# text(0.1*traits.vecs[,1] + c(-.0,0.,0.15), 
#      0.1*traits.vecs[,2] + c(.05, .05, .05),
#      labels = rownames(traits.vecs))
# text(unique(round(traits.scores,3)), pos = 1,
#      labels = paste("n =", as.data.frame(trait.sum[,4])$n))
# dev.off()

traits.fuz$membership %>% as.data.frame() %>% 
  rownames_to_column(var = "species_id") %>% 
  as_tibble() %>% 
  left_join(dd.traits)


clusters <- cbind.data.frame(traits.scores, traits.fuz$membership)
colnames(clusters) <- c("PC1", "PC2", paste0("Group",1:k))
clusters$hard <- traits.fuz$clustering
clusters <- unique(round(clusters, 3))
clusters$n <- trait.sum$n
clusters <- clusters %>% mutate(n = n/sum(n))

# for(i in 1:k){
#   gg <- unique(round(traits.scores[traitsfuz.g == i,], 3))
#   hpts <- chull(gg)
#   hpts <- c(hpts, hpts[1])
#   gg[hpts]
# }

trait.labs <- as.data.frame(traits.vecs) %>% 
  rownames_to_column(var = "trait") %>% 
  mutate(origin = 0)


# Make Figure
scale.arrows <- .05
ggplot() +
  geom_hline(alpha = 0.2, linetype = "dashed", aes(yintercept = 0)) +
  geom_vline(alpha = 0.2, linetype = "dashed", aes(xintercept = 0)) +
  geom_scatterpie(data = clusters, 
                  aes(x = PC1, y = PC2, group = hard, r = .3*sqrt(n/pi)), 
                  cols = paste0("Group",1:k),
                  color = "gray90", size = .1,
                  alpha = 0.8) +
  scale_fill_manual(values = cols[c(4,5,3)]) +
  geom_segment(data = trait.labs, 
               aes(x = origin, y = origin, 
                   xend = scale.arrows*PC1, 
                   yend = scale.arrows*PC2),
               alpha = 0.5, color = "black",
               arrow = arrow(angle = 20,
                             length = unit(.1, "inches"),
                             type = "open")) +
  geom_text_repel(data = trait.labs,
                   aes(x = scale.arrows*PC1, 
                       y = scale.arrows*PC2, label = trait),
                   color = "black",
                   segment.alpha = 0, 
                  point.padding = .2, 
                  direction = "y", nudge_y = .01) +
  coord_fixed() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA),
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  labs(x = paste0("PC1 (", var1,"%)"),
       y = paste0("PC2 (", var2,"%)")) +
  ggsave("figures/fuzzy-clusters.pdf", width = 5, height = 5) +
  ggsave("figures/fuzzy-clusters.png", dpi = 500, width = 5, height = 5)


