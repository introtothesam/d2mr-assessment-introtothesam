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
    country_col,
    geometry_col
) {
  vr_sym <- ensym(var)
  
  # 1. Scatter Plot 
  # x-ais = the chosen variable (var)
  # y-axis = country 
  p1 <- ggplot(data, aes(
    x = !!var_sym,
    y = !!sym(country_col),
    tooltip = !!sym(country_col),
    data_id = !!sym(country_col),
    color = !!sym(country_col)
  )) +
    
    geom_point_interactive(size = 3) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  
  # 2. Bar Plot 
  # x-axis = the chosen variable
  # y-axis = reorder (country by chosen variable)

  p2 <- ggplot(data, aes(
    x = !!var_sym,
    y = reorder(!!sym(country_col), !!var_sym),
    tooltip = !!sym(country_col),
    data_id = !!sym(country_col),
    fill = !!sym(country_col)
  )) +
    geom_col_interactive() +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  
  # 3. Choropleth Map
  # fill = the chosen variable (instead of country name)
  # The base layer is a light grey world map
  # The interactive layer uses var for fill, and country name for tooltip
  p3 <- ggplot() +
    # Base map layer (light grey)
    geom_sf(data = data, fill = "lightgrey", color = "lightgrey") +
    # Interactive layer
    geom_sf_interactive(
      data = data,
      aes(
        fill = !!var_sym,
        tooltip = !!sym(country_col),
        data_id = !!sym(country_col)
      )
    ) +
    coord_sf(crs = st_crs(3857)) + # common Web Mercator projection
    theme_void() +
    theme(
      legend.position = "none"
    )
  
  # Combine the plots with patchwork
  # (p1 + p2) puts scatter and bar side by side
  # / p3     puts the map underneath
  combined_plot <- (p1 + p2) / p3 +
    plot_layout(heights = c(1, 2))  # make the map larger
  
  # Convert to an interactive widget
  interactive_plot <- girafe(ggobj = combined_plot)
  
  # Add a hover effect
  interactive_plot <- girafe_options(
    interactive_plot,
    opts_hover(css = "fill:red;stroke:black;")
  )
  
  return(interactive_plot)
}
  