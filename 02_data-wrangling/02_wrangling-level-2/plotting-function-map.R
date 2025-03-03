# ----- SETUP: Load Libraries -----
# Ensure required packages are installed before loading
# If any package is missing, install it using:
# install.packages(c("ggplot2", "maps", "dplyr", "ggiraph", "patchwork", "sf", "scales"))

# Load necessary libraries for visualization and data handling
library(ggplot2)      # For creating static and interactive plots
library(maps)         # For geographic map data
library(dplyr)        # For data manipulation and transformation
library(ggiraph)      # For adding interactive elements to ggplot2
library(patchwork)    # For combining multiple plots into one
library(sf)           # For working with spatial data
library(scales)       # For formatting numbers (e.g., commas in large numbers)

# ----- FUNCTION: interactive_combined_plot -----
#' Create an Interactive Combined Plot (Scatter, Bar, Choropleth)
#'
#' This function generates an interactive visualization combining a scatter plot, a bar plot, and a choropleth map using ggplot2 and ggiraph.
#'
#' @param data A data frame containing the dataset with geographic and numeric variables.
#' @param var A string representing the numeric variable to visualize (e.g., GDP per capita, population).
#' @param geo_unit A string representing the geographic unit (e.g., "country", "continent").
#' 
#' @return A combined interactive plot with three visualization types.
#' @export

interactive_combined_plot <- function(
    data, 
    var,
    geo_unit  
) {
  
  # Convert the variable name to a symbol for tidy evaluation
  vr_sym <- sym(var)  
  
  # ----- DATA PREPROCESSING -----
  # Create dynamic tooltip with location type for interactive visualization
  
  # Determine the geographic label based on geo_unit
  # This label will be used in the tooltip for better clarity
  geo_label <- if (geo_unit == "country") {
    "Country"
  } else if (geo_unit == "continent") {
    "Continent"
  } else if (geo_unit == "country_abbrev") {
    "Country Abbreviation"
  } else {
    "Region"
  }
  
  # Add the geographic label and tooltip to the dataset
  # The tooltip provides detailed information when hovering over elements in the plot
  data <- data %>%
    mutate(
      geo_label = geo_label,  # Assign the determined label to each row
      tooltip_text = paste0(
        geo_label, ": ", !!sym(geo_unit), "<br>",  # Include the geographic unit name
        var, ": ", scales::comma(!!vr_sym)  # Format variable values with commas
      )
    )
  
  # Ensure the selected variable is numeric before plotting
  # This prevents errors when using the variable for visualization
  if (!is.numeric(data[[var]])) {
    stop(paste("Error: The selected variable", var, "must be numeric."))
  }
  
  
  # ----- 1. Scatter Plot -----
  # Creates an interactive scatter plot with country/region names on the y-axis
  p1 <- ggplot(data, aes(
    x = !!vr_sym,
    y = reorder(!!sym(geo_unit), !!vr_sym),  # Order y-axis based on the variable value
    tooltip = tooltip_text,  # Show tooltip on hover
    data_id = factor(!!sym(geo_unit)),  # Unique identifier for interactivity
    color = !!sym(geo_unit)  # Assign colors based on geographic unit
  )) +
    geom_point_interactive(size = 5, alpha = 0.7) +  # Make points interactive
    labs(x = var, y = geo_unit, title = "Scatter Plot") +  # Add labels and title
    scale_x_continuous(labels = scales::comma) +  # Format x-axis labels
    theme_minimal() +  # Use a clean minimal theme
    theme(
      # Interactive axis labels and title with tooltips
      axis.text.y = element_text_interactive(size = 12, angle = 15, hjust = 1, 
                                             tooltip = paste("Location:", data[[geo_unit]])),
      axis.text.x = element_text_interactive(size = 14, tooltip = paste(var, "values")),  
      plot.title = element_text_interactive(size = 16, face = "bold", 
                                            tooltip = "Hover over the points for details"),  
      legend.position = "none",
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 12, 12, 12)
    )
  
  # ----- 2. Bar Plot -----
  # Creates an interactive bar plot showing the variable distribution across regions
  p2 <- ggplot(data, aes(
    x = !!vr_sym,
    y = reorder(!!sym(geo_unit), !!vr_sym),  # Order by variable value
    tooltip = tooltip_text,  
    data_id = factor(!!sym(geo_unit)),  # Interactive identifier
    fill = !!sym(geo_unit)  # Fill bars by geographic unit
  )) +
    geom_col_interactive(width = 0.6, alpha = 0.7) +  # Interactive bars
    labs(x = var, y = geo_unit, title = "Bar Plot") +  # Add labels and title
    scale_x_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      # Interactive axis labels and title with tooltips
      axis.text.y = element_text_interactive(size = 12, angle = 15, hjust = 1, 
                                             tooltip = paste("Location:", data[[geo_unit]])),
      axis.text.x = element_text_interactive(size = 14, tooltip = paste(var, "values")),  
      plot.title = element_text_interactive(size = 16, face = "bold", 
                                            tooltip = "Hover over the bars for details"),
      legend.position = "none",
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.margin = margin(12, 12, 12, 12)
    )
  
  # ----- 3. Choropleth Map -----
  # Creates an interactive map with geographic regions shaded by the variable
  p3 <- ggplot() +
    # Base map layer (light grey for all regions)
    geom_polygon(data = data, aes(
      x = long, 
      y = lat, 
      group = group
    ), fill = "lightgrey", color = "black") +
    
    # Overlay interactive colored regions based on variable value
    geom_polygon_interactive(
      data = data,
      aes(
        x = long,
        y = lat,
        group = group, 
        fill = !!vr_sym,
        tooltip = tooltip_text,
        data_id = factor(!!sym(geo_unit))
      )
    ) +
    scale_fill_viridis_c(option = "mako", na.value = "grey50") +  # Use color scale for better visualization
    labs(fill = var, title = "Choropleth Map") +  # Add title and legend label
    coord_fixed(1.3) +  # Keep aspect ratio consistent
    theme_void() +  # Remove default background and gridlines
    theme(
      legend.position = "right",
      legend.title = element_text_interactive(size = 14, tooltip = paste("Legend for", var)),  
      legend.text = element_text_interactive(size = 12, tooltip = "Click to highlight"),
      plot.title = element_text_interactive(size = 18, face = "bold", 
                                            tooltip = "Hover over the map for details")
    )
  
  # ----- COMBINE PLOTS -----
  # Arrange scatter, bar, and choropleth map using patchwork
  combined_plot <- (p1 + p2) / p3 +  # Keep scatter and bar larger than the map
    plot_layout(heights = c(3, 3))  # Adjust layout proportions
  
  # Convert to an interactive widget with girafe
  interactive_plot <- girafe(
    ggobj = combined_plot, 
    width_svg = 14, height_svg = 10,
    options = list(
      opts_selection(
        type = "single",  # Allow selecting one item at a time
        css = "fill: red; stroke: black; stroke-width: 2px;",  # Custom styling for selected elements
        selected = "data_id"
      )
    )
  )
  
  return(interactive_plot)  # Return the interactive visualization

}
