library(factoextra)
library(cowplot)

# Create the cluster plot
cluster_plot <- fviz_cluster(kmeans_result,  
                             data = metrics,  
                             ellipse.type = "convex",  
                             geom = "point",  
                             main = "K-means Clusters",  
                             xlab = "",  # Set to NULL for shared labels
                             ylab = "") +  
  geom_text(aes(label = comp_data$data),  
            size = 5,  
            vjust = -1,  
            hjust = 0.5) +  
  theme(legend.position = "none") +  
  theme_bw() + 
  theme(axis.title.x = element_text(size = 18),  # Adjust x-axis label size
        axis.title.y = element_text(size = 18),
        title        = element_text(size = 18),
        axis.text.x  = element_text(size = 16),    # Adjust x-axis text size
        axis.text.y  = element_text(size = 16))    # Adjust y-axis text size

# Create the biplot
biplot <- fviz_pca_biplot(pca) +  
  geom_point(data = nx, aes(PC1, PC2, color = cluster), size = 3) +  
  theme_bw() +  
  labs(x = "", y = "") +  
  theme(legend.position = "none", 
        axis.title.x = element_text(size = 18),  
        axis.title.y = element_text(size = 18),  
        title        = element_text(size = 18),
        axis.text.x  = element_text(size = 16),  
        axis.text.y  = element_text(size = 16)) 

# Combine plots using cowplot
combined_plot <- plot_grid(biplot, cluster_plot, ncol = 2, align = 'hv')

# Add shared x and y axis labels
combined_plot <- combined_plot +  
  draw_label("Dimension 1 (39.2%)", x = 0.48, y = 0.02, size = 12) +  # Adjust y for placement
  draw_label("Dimension 2 (16.5%)", x = 0.01, y = 0.5, size = 12, angle = 90)  # Adjust x for placement

# Display combined plot
print(combined_plot)
