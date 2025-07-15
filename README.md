# README - Shiny Interactive mtcars Visualization

## Overview

This Shiny application provides an interactive visualization of the mtcars dataset, allowing users to explore and select data points through various interactions. The app features:

- A scatter plot of mpg (miles per gallon) vs wt (weight)
- Multiple interaction modes (hover, click, double-click, and brush selection)
- A responsive data table that displays selected points
- Color-coded selections for different interaction types

## Features

### Interactive Plot

- **Hover**: Points temporarily turn gray when hovered over
- **Click**: Toggle points to green (selected) or black (unselected)
- **Double-click**: Reset individual points to their original state
- **Brush selection**: Select multiple points by dragging - selected points turn blue

### Data Table

- Displays only the selected points (either clicked or brush-selected)
- Updates dynamically as selections change

## Installation

To run this application, you'll need:

1. R (version 4.0 or higher recommended)
2. The following R packages:
   - shiny
   - ggplot2
   - DT

Install the required packages with:
```R
install.packages(c("shiny", "ggplot2", "DT"))
```

## Usage

1. Save both `server.R` and `ui.R` files in the same directory
2. Run the application with:
```R
shiny::runApp()
```

## Interaction Guide

1. **Hover** over points to temporarily highlight them in gray
2. **Click** on points to permanently select them (green)
3. **Double-click** on points to deselect them
4. **Click and drag** to create a brush selection area (blue)
5. The table at the bottom will show all currently selected points

## Customization

The application can be easily modified to:

- Use a different dataset by changing the `vals$df` initialization
- Change colors by modifying the color values in the server logic
- Adjust plot aesthetics by modifying the ggplot code

## Troubleshooting

If the application doesn't run:
- Ensure all required packages are installed
- Check that both R files are in the same directory
- Verify there are no syntax errors in the R console when starting the app

