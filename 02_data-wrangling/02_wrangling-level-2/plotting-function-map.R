# install.packages("countrycode")
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("plotly")

# Load required packages
library(dplyr)
library(ggplot2)
library(ggiraph)
library(sf)
library(rlang)
library(patchwork)

# Create an interactive world map function 
interactive_combined_plot <- function(
    data, 
    var,
    geo_unit  # ✅ More flexible than "region" (can be country, continent, etc.)
) {
  
  vr_sym <- sym(var)  # Convert variable to symbol
  
  # 1. Scatter Plot 
  p1 <- ggplot(data, aes(
    x = !!vr_sym,
    y = reorder(!!sym(geo_unit), !!vr_sym),
    tooltip = !!sym(geo_unit),
    data_id = !!sym(geo_unit),
    color = !!sym(geo_unit)
  )) +
    geom_point_interactive(size = 6, alpha = 0.8) +
    labs(x = var, y = geo_unit, title = "Scatter Plot") +
    scale_x_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      axis.text.y = element_text_interactive(size = 12, angle = 15, hjust = 1, 
                                             tooltip = paste("Location:", data[[geo_unit]])),
      axis.text.x = element_text_interactive(size = 14, tooltip = paste(var, "values")),  
      plot.title = element_text_interactive(size = 16, face = "bold", 
                                            tooltip = "Hover over the points for details"),  
      legend.position = "none",
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # 2. Bar Plot 
  p2 <- ggplot(data, aes(
    x = !!vr_sym,
    y = reorder(!!sym(geo_unit), !!vr_sym),
    tooltip = !!sym(geo_unit),
    data_id = !!sym(geo_unit),
    fill = !!sym(geo_unit)
  )) +
    geom_col_interactive(width = 0.6, alpha = 0.8) +
    labs(x = var, y = geo_unit, title = "Bar Plot") +
    scale_x_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      axis.text.y = element_text_interactive(size = 12, angle = 15, hjust = 1, 
                                             tooltip = paste("Location:", data[[geo_unit]])),
      axis.text.x = element_text_interactive(size = 14, tooltip = paste(var, "values")),  
      plot.title = element_text_interactive(size = 16, face = "bold", 
                                            tooltip = "Hover over the bars for details"),
      legend.position = "none",
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # 3. Choropleth Map
  p3 <- ggplot() +
    geom_polygon(data = data, aes(
      x = long, 
      y = lat, 
      group = group
    ), fill = "lightgrey", color = "black") +
    
    geom_polygon_interactive(
      data = data,
      aes(
        x = long,
        y = lat,
        group = group, 
        fill = !!vr_sym,
        tooltip = !!sym(geo_unit),
        data_id = !!sym(geo_unit)
      )
    ) +
    scale_fill_viridis_c(option = "plasma", na.value = "grey50") +  
    labs(fill = var, title = "Choropleth Map") +  
    coord_fixed(1.3) +
    theme_void() +
    theme(
      legend.position = "right",
      legend.title = element_text_interactive(size = 14, tooltip = paste("Legend for", var)),  
      legend.text = element_text_interactive(size = 12, tooltip = "Click to highlight"),
      plot.title = element_text_interactive(size = 18, face = "bold", 
                                            tooltip = "Hover over the map for details")
    )
  
  # Combine the plots with patchwork
  combined_plot <- (p1 + p2) / p3 +
    plot_layout(heights = c(2, 3))  # Keep scatter/bar larger relative to the map
  
  # Convert to an interactive widget
  interactive_plot <- girafe(ggobj = combined_plot, width_svg = 14, height_svg = 14)
  
  # Add a hover effect
  interactive_plot <- girafe_options(
    interactive_plot,
    opts_hover(css = "fill:red;stroke:black;")
  )
  
  return(interactive_plot)
}
