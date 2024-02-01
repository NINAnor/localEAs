library(tidyverse)
library(vegan)
library(gridExtra)
library(tidyverse)
library(corrplot)
library(tidymodels)
library(tidytext)
library(ggpubr)
library(factoextra)#derive results from prcomp, pca
library(ggrepel)

Sys.setlocale("LC_CTYPE", "norwegian")
options(scipen=999)

Sys.setlocale("LC_CTYPE", "norwegian")

setwd("C:/Users/trond.simensen/OneDrive - NINA/Documents/R/lokale_naturregnskap")

data_orig <- read.csv("C:/Users/trond.simensen/OneDrive - NINA/Documents/R/lokale_naturregnskap/lok_eco_account4.csv", 
	header = TRUE, sep = ",") 

data_orig <- read.csv("C:/Users/trond.simensen/OneDrive - NINA/Documents/R/lokale_naturregnskap/lok_eco_account5.csv", 
	header = TRUE, sep = ",") 

data_orig <- read.csv("C:/Users/trond.simensen/OneDrive - NINA/Documents/R/lokale_naturregnskap/lok_eco_account7.csv", 
	header = TRUE, sep = ",") 


data_orig
glimpse(data_orig)

data <- data_orig

rownames(data) <- data[,1]
data[,1] <- NULL

data
glimpse(data)


# Histograms ---------------------------------------------------------

c_data <- data

c_data [,1:12] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)+
	theme_pubclean()


c_data [,1:18] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)+
	theme_pubclean()


p <- c_data [,1:20] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(bins = 20, color="white", fill="#0072B2", alpha=0.8)+
	theme_pubclean()

p

ggsave("C:/Users/trond.simensen/OneDrive - NINA/Documents/R/lokale_naturregnskap/plots/histograms.png", 
	p, scale = 2, bg = "white", width = 12, height = 12, units = "cm", dpi = 600)

# Multivariate


transformation_recipe <-
  recipe(~., data = data_orig) %>% 
  update_role(method, approach, new_role = "id") %>% 
	#step_impute_mean(all_numeric()) |> 
	# 1 impute or omit na
	step_naomit(all_numeric()) %>% 
	# mean impute numeric variables
  # step_impute_mean(all_numeric()) |> 
	# 2 Handle factor levels
  # convert specific variable(s) to dummy variables
  #step_dummy(kyst_f) %>%
	# 3 Individual transformations for skewness and other issues
	step_YeoJohnson(all_numeric()) |> 
	# 4 Create interactions
	# 5 Normalization steps (center, scale, range, etc)
#   step_center(all_numeric()) |> 
# 	step_scale(all_numeric()) |> 
	step_normalize(all_numeric()) %>%
	# rescale all numeric variables except ..., ... and ... to lie between 0 and 1
  # step_range(all_numeric(), min = 0, max = 1, -vanilla, -salt, -baking_powder) %>%
	step_range(all_numeric(), min = 0, max = 1) %>%
	# remove predictor variables that are almost the same for every entry
  step_nzv(all_predictors()) |> 
  #step_pca(all_numeric(), id = "pca") %>% 
  prep() # CRN: package for function "prep" missing?

#data_t <- data |> select(-region, -municipality, -county, -kyst_f, -hyttekomtype, -KDP_natur)
data_t <- data_orig |> dplyr::select(where(is.numeric)) 

transformation_recipe2 <- 
	recipe(~., data = data_orig) |> 
  update_role(method, approach, new_role = "id") %>% 
	step_naomit(all_numeric()) |> 
	step_YeoJohnson(all_numeric()) |> 
	#step_normalize(all_numeric())  |> 
	step_range(all_numeric(), min = 0, max = 1) %>%
  step_nzv(all_predictors()) |> 
	prep()

# fit recipe to data and preprocess
preprocessed_data <- bake(transformation_recipe2, new_data = NULL)
data <- as.data.frame(preprocessed_data)
rownames(data) <- data[,1]
data[,1] <- NULL
method.env <- data |> dplyr::select(method)
data <- data |> dplyr::select(-method, -doc)



# # fit recipe to data and preprocess
# preprocessed_data <- bake(transformation_recipe2, new_data = NULL)
# data <- as.data.frame(preprocessed_data)
# method.env <- data |> dplyr::select(method)
# rownames(data) <- data[,1]
# #data[,1] <- NULL
# #method.env <- data |> dplyr::select(method)
# #data <- data |> dplyr::select(-method, -doc)
# data <- data |> dplyr::select(-method, approach, -doc)


#The correlation matrix can be calculated as follows (returning only correlation coefficients):
round(cor(data,method="k"),2)
cor_vars <- round(cor(data, method ="kendall"), 2)
cor_vars
write.csv(cor_vars, "C:/Users/trond.simensen/OneDrive - NINA/Documents/R/lokale_naturregnskap/tables/cor_variabler.csv")

plot(data)

M = cor(data, method = "kendall")
corrplot(M, method = 'number') # colorful number
corrplot(M, method = 'number', order = 'hclust', addrect = 3)

## leave blank on non-significant coefficient
## add all correlation coefficients

testRes = cor.mtest(data, method = "kendall", conf.level = 0.95)

corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         order = 'AOE', diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))
#round(cor(nv,method="k"),2)

# removing strongly correlated data
data <- data |> select(-remote, -laeco, -proj, -csr, -compile, -specfield)
#data <- data |> select(-specfield)

M = cor(data, method = "kendall")
testRes = cor.mtest(data, method = "kendall", conf.level = 0.95)

corrplot(M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         order = 'AOE', diag = FALSE)$corrPos -> p1
text(p1$x, p1$y, round(p1$corr, 2))

# Shepards test/goodness of fit

#method.mds <- metaMDS(data, distance = "bray", autotransform = F)

# Running NMDS in vegan (metaMDS)
method.mds <-
  metaMDS(data,
          distance = "bray",
          k = 2,
          maxit = 999, 
          trymax = 500,
          wascores = TRUE)

method.mds

# As a rule of thumb literature has identified the following cut-off values for stress-level:
# 
# Higher than 0.2 is poor (risks for false interpretation).
# 0.1 - 0.2 is fair (some distances can be misleading for interpretation).
# 0.05 - 0.1 is good (can be confident in inferences from plot).
# Less than 0.05 is excellent (this can be rare).

goodness(method.mds) # Produces a results of test statistics for goodness of fit for each point

stressplot(method.mds) # Produces a Shepards diagram

# https://rstudio-pubs-static.s3.amazonaws.com/496936_2cda5f07e6044d7e9d521893e484a558.html

method.envfit <- envfit(method.mds, method.env, permutations = 999) # this fits environmental vectors
method.spp.fit <- envfit(method.mds, data, permutations = 999) # this fits species vectors

method.mds
method.spp.fit

# Plotting points in ordination space
plot(method.mds, "sites")   # Produces distance 
orditorp(method.mds, "sites")   # Gives points labels

site.scrs <- as.data.frame(scores(method.mds, display = "sites")) #save NMDS results into dataframe
site.scrs <- cbind(site.scrs, Method = method.env$method) #add grouping variable "Management" to dataframe
site.scrs <- cbind(site.scrs, Approach = method.env$method) #add grouping variable of cluster grouping to dataframe
#site.scrs <- cbind(site.scrs, Site = rownames(site.scrs)) #add site names as variable if you want to display on plot

head(site.scrs)

##***Akse 1**###
#Analysevariabler


cor.test(site.scrs$NMDS1, data$csr,method="k")
cor.test(site.scrs$NMDS1, data$plan,method="k")
cor.test(site.scrs$NMDS1, data$proj,method="k")


ax1<-site.scrs$NMDS1 #Bestem akse - husk ? skifte fra "akse1" til f.eks."akse2" hver gang en g?r over fra f.eks. gnmds1 til gnmds2
ax2<-site.scrs$NMDS2 #Bestem akse - husk ? skifte fra "akse1" til f.eks."akse2" hver gang en g?r over fra f.eks. gnmds1 til gnmds2
#ax3<-site.scrs$NMDS3 #Bestem akse - husk ? skifte fra "akse1" til f.eks."akse2" hver gang en g?r over fra f.eks. gnmds1 til gnmds2


la.var<-data #Bestem milj?variabelmatrise

# axis 1
p.val<-NULL #create empty objects for the p.values
tau.val<-NULL #creates empty object for the t-values.
p<-NULL

for(i in 1:length(ax1)) # A Loop that calculates Kendall's tau and p-values for all the correlations tests.NB! Husk ? endre antall faktorer som analyseres! 
{
  z<-cor.test(ax1,la.var[,i], method="k")
  p.val[i]<-round(z$p.value, digits=3)
  tau.val[i]<-round(z$estimate,digits=2)
  if (p.val[i] < 0.05) #prints a star when the p-value is less than 0.05
  {p[i]<-"***"
  }else{
    p[i]<-" "}
}

names(la.var)
p.val
tau.val
summary.ax1<-data.frame(cbind(names(la.var), tau.val, p.val))
summary.ax1

# axis 2

p.val<-NULL #create empty objects for the p.values
tau.val<-NULL #creates empty object for the t-values.
p<-NULL

for(i in 1:length(ax2)) # A Loop that calculates Kendall's tau and p-values for all the correlations tests.NB! Husk ? endre antall faktorer som analyseres! 
{
  z<-cor.test(ax2,la.var[,i], method="k")
  p.val[i]<-round(z$p.value, digits=3)
  tau.val[i]<-round(z$estimate,digits=2)
  if (p.val[i] < 0.05) #prints a star when the p-value is less than 0.05
  {p[i]<-"***"
  }else{
    p[i]<-" "}
}

names(la.var)
p.val
tau.val
summary.ax2<-data.frame(cbind(names(la.var), tau.val, p.val))
summary.ax2



# axis 3

# p.val<-NULL #create empty objects for the p.values
# tau.val<-NULL #creates empty object for the t-values.
# p<-NULL
# 
# for(i in 1:length(ax3)) # A Loop that calculates Kendall's tau and p-values for all the correlations tests.NB! Husk ? endre antall faktorer som analyseres! 
# {
#   z<-cor.test(ax3,la.var[,i], method="k")
#   p.val[i]<-round(z$p.value, digits=3)
#   tau.val[i]<-round(z$estimate,digits=2)
#   if (p.val[i] < 0.05) #prints a star when the p-value is less than 0.05
#   {p[i]<-"***"
#   }else{
#     p[i]<-" "}
# }
# 
# names(la.var)
# p.val
# tau.val
# summary.ax3<-data.frame(cbind(names(la.var), tau.val, p.val))
# summary.ax3

# Combine to one table
summary.ax1 <- summary.ax1 |> rename (Variable = V1, tau_ax1 = tau.val, p_ax1 = p.val)
summary.ax1

summary.ax2 <- summary.ax2 |> rename (Variable = V1, tau_ax2 = tau.val, p_ax2 = p.val)
summary.ax2

# summary.ax3 <- summary.ax3 |> rename (Variable = V1, tau_ax3 = tau.val, p_ax3 = p.val)
# summary.ax3

summary_table <- left_join(summary.ax1, summary.ax2, by = "Variable")
summary_table 
# summary_table <- left_join(summary_table, summary.ax3, by = "Variable")
# summary_table 

# Save to CSV
write.csv(summary_table , "tables/cor_nmds_axes_1_2.csv", row.names = FALSE)

spp.scrs <- as.data.frame(scores(method.spp.fit, display = "vectors")) #save species intrinsic values into dataframe
spp.scrs <- cbind(spp.scrs, Species = rownames(spp.scrs)) #add species names to dataframe
spp.scrs <- cbind(spp.scrs, pval = method.spp.fit$vectors$pvals) #add pvalues to dataframe so you can select species which are significant
#spp.scrs<- cbind(spp.scrs, abrev = abbreviate(spp.scrs$Species, minlength = 6)) #abbreviate species names
sig.spp.scrs <- subset(spp.scrs, pval <= 0.05) #subset data to show species significant at 0.05
all.spp.scrs <- subset(spp.scrs, pval > 0.05) #subset data, all variables


head(spp.scrs)
glimpse(spp.scrs)


env.scores.method <- as.data.frame(scores(method.envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.method <- cbind(env.scores.method, env.variables = rownames(env.scores.method)) #and then gives them their names

env.scores.method <- cbind(env.scores.method, pval = method.envfit$vectors$pvals) # add pvalues to dataframe
#sig.env.scrs <- subset(env.scores.method, pval<=0.05) #subset data to show variables significant at 0.05

#head(env.scores.method)



ggplot(site.scrs, aes(x=NMDS1, y=NMDS2))+ #sets up the plot
  geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Method), shape = factor(site.scrs$Method)), size = 2)+ #adds site points to plot, shape determined by Landuse, colour determined by Management
  coord_fixed()+
  theme_classic()+
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid"))+
  labs(colour = "Method", shape = "Method")+ # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) # add legend at right of plot

ggplot(site.scrs, aes(x=NMDS1, y=NMDS2, label = row.names(site.scrs))) + # sets up the plot
  coord_fixed() +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid")) +
  labs(colour = "Method", shape = "Method") + # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) + # add legend at right of plot
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + # add dashed line at y=0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
	geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Method), shape = factor(site.scrs$Method)), size = 2) + # adds site points to plot, shape determined by Landuse, colour determined by Management
  geom_text(color = "grey1", check_overlap = FALSE, vjust = "bottom", nudge_y = 0.1, family = "IBMPlexSans") # add dashed line at x=0

nmds.plot.a <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2, label = row.names(site.scrs))) + # sets up the plot
  coord_fixed() +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid")) +
  labs(colour = "Method", shape = "Method") + # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) + # add legend at right of plot
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + # add dashed line at y=0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
	geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Method), shape = factor(site.scrs$Method)), size = 2) + # adds site points to plot, shape determined by Landuse, colour determined by Management
  geom_text(color = "grey1", check_overlap = FALSE, vjust = "bottom", nudge_y = 0.05, family = "IBMPlexSans")+ # add dashed line at x=0
	labs(title = "a)")

nmds.plot.a

nmds.plot.b <- ggplot(site.scrs, aes(x=NMDS1, y=NMDS2)) + # sets up the plot
  coord_fixed() +
  theme_classic() +
  theme(panel.background = element_rect(fill = NA, colour = "black", size = 1, linetype = "solid")) +
  labs(colour = "Method", shape = "Method") + # add legend labels for Management and Landuse
  theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12), axis.text = element_text(size = 10)) + # add legend at right of plot
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") + # add dashed line at y=0
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
	geom_point(aes(NMDS1, NMDS2, colour = factor(site.scrs$Method), shape = factor(site.scrs$Method)), size = 2) + # adds site points to plot, shape determined by Landuse, colour determined by Management
	geom_segment(data = all.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey55", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = all.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3.5, direction = "both", segment.size = 0.25, colour = "grey55")+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
	geom_segment(data = sig.spp.scrs, aes(x = 0, xend=NMDS1, y=0, yend=NMDS2), arrow = arrow(length = unit(0.25, "cm")), colour = "grey10", lwd=0.3) + #add vector arrows of significant species
  ggrepel::geom_text_repel(data = sig.spp.scrs, aes(x=NMDS1, y=NMDS2, label = Species), cex = 3.5, direction = "both", segment.size = 0.25)+ #add labels for species, use ggrepel::geom_text_repel so that labels do not overlap
	labs(title = "b)")

nmds.plot.a
nmds.plot.b

nmds.plot.a <- nmds.plot.a +
  scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1))

nmds.plot.b <- nmds.plot.b +
  scale_x_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1))

# Plot side by side
grid.arrange(nmds.plot.a, nmds.plot.b, ncol = 2)
grid.arrange(nmds.plot.a, nmds.plot.b, nrow = 2)


# Save plot
plots <- grid.arrange(nmds.plot.a, nmds.plot.b, nrow = 2)

plots

ggsave("C:/Users/trond.simensen/OneDrive - NINA/Documents/R/lokale_naturregnskap/plots/fig_nmds_3_axes.png", 
	plots, scale = 2, bg = "white", width = 7.5, height = 13, units = "cm", dpi = 600)

# NMDS - what to report:

#http://stratigrafia.org/8370/lecturenotes/multidimensionalScaling.html

# Clustering


# glimpse(preprocessed_data)
# 
# dat <- as.data.frame(preprocessed_data)
# dat <- as.data.frame(data_t)

dat <- data_orig

# rownames(data) <- data[,1]
# data[,1] <- NULL

rownames(dat) <- dat[,1]
data[,1] <- NULL

dat
glimpse(dat)

dat <- dat |> select(-approach, - method, -doc)
dat

methods_dist <- vegdist(dat, method = "bray")

methods_hc <- hclust(methods_dist)
plot(methods_hc, main = "Gruppering av metoder")


#library(factoextra)
fviz_dend(methods_hc, cex = 0.5, 
          main = "Gruppering av metoder",
          xlab = "Objects", ylab = "Distance", sub = "")

#fviz_dend(methods_hc, cex = 0.5, horiz = TRUE)

fviz_dend(methods_hc, k = 4,                  # Cut in four groups
          cex = 0.5,                          # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"), # group colors
          color_labels_by_k = TRUE,           # color labels by groups
          rect = TRUE,                        # add colored rectangle for groups
          main = "Gruppering av metoder",     # main title
          xlab = "",                          # remove x-axis label
          ylab = "Distance",                  # y-axis label
          sub = "",                           # remove subtitle
          ggtheme = theme_void()              # use a blank theme
          )

# Assuming methods_hc is your hclust object and you've already performed clustering
plot(methods_hc, main = "Gruppering av metoder") # Plot the dendrogram

# Use rect.hclust to add rectangles. The border parameter specifies the color of the rectangle borders.
rect.hclust(methods_hc, k = 2, border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))

plot(methods_hc, main = "Gruppering av metoder", sub = "")
rect.hclust(methods_hc, k = 3, border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))

plot(methods_hc, main = "Gruppering av metoder", xlab = "", sub = "")
rect.hclust(methods_hc, k = 5, border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))


# Open a PNG device
png(filename = "C:/Users/trond.simensen/OneDrive - NINA/Documents/R/lokale_naturregnskap/plots/Gruppering_av_metoder.png", width = 8*600, height = 8*600, units = "px", res = 600, bg = "white")

# Plot the dendrogram
plot(methods_hc, main = "Gruppering av metoder", xlab = "", sub = "")
rect.hclust(methods_hc, k = 4, border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"))

# Turn off the device (this saves the file)
dev.off()



