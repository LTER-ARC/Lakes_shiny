# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

## Required Packages & Data
library(tidyverse)
library(DT)
library(shiny)
library(markdown)
library(janitor)


lake_data <- read_rds("data/ARC_Lakes_Physchem_2010_2021.ver5.rds") #load dataframe 

## Useful Objects for Plotting 
 site_list <- unique(lake_data$Site)
 measure_list <- names(lake_data)[7:19]

# Define server logic required to draw plots
shinyServer(
  
  function(input, output, session) {


# Reactive Conductor Functions  -------------------------------------------
    ## (see https://shiny.rstudio.com/articles/reactivity-overview.html)
    
    #**************************************************************************   
    plot3D_lakes_select <- function(lake_data) {
      sites <- input$lake_site_3D
      measures <- unlist(measure_list[as.numeric(input$lake_measures)])
      
      sub_data <- lake_data %>%
        filter(Site %in% sites) %>% 
        filter(Year >= input$lake_year_3D) %>%
        mutate(Year = factor(Year)) %>% 
        filter(DOY >= input$dayofyear[1] & DOY <= input$dayofyear[2])
      
      
      return(sub_data)
    }
    #************************************************************************** 
    plot_lakes_select <- function(lake_data) {
      sites <- input$lake_sites
      measures <- unlist(measure_list[as.numeric(input$lake_measures)]) 
      
      sub_data <- lake_data %>%
        filter(Site %in% sites) %>% 
        filter(Year >= input$lake_years[1] & Year <= input$lake_years[2])  %>%
        mutate(Year = factor(Year)) 
      
      return(sub_data)
    }
    
# Reactive Dataframes -----------------------------------------------------
    data <- reactiveValues(lakes_sub_data = NULL)
    

# Datatable Output --------------------------------------------------------

    output$lakes_table <- DT::renderDataTable({
      DT::datatable(data$lakes_sub_data, options = list(orderClasses = TRUE))
    })
    
# 3D Plot Output -------------------------------------------------------------
    output$lakes_3D_plot <- renderPlotly({ ######## LAKES PHYSCHEM
      sub_data <- plot3D_lakes_select(lake_data) 
      which_measure <- input$measurement_3D
      
      ### Reactive Dataframe ###
      data$lakes_sub_data <- sub_data %>% 
        # pick measurement
        select(contains(which_measure), everything())
      
      ### Plot
      data_to_plot <- sub_data  %>%
         # Choose measurement to graph 
        rename_with(~sub(which_measure, 'measure', .),contains(which_measure)) %>% 
        clean_names() %>% 
        mutate(doy = as.factor(doy))
      plot_ly(data_to_plot, x = ~doy, z = ~hydrolab_depth, y = ~measure, split = ~doy, 
              color = ~doy, type = "scatter3d", mode = "lines+markers") %>% 
        layout(scene = list(xaxis = list(autorange = 'reversed', title = "Day of Year"),
                            yaxis = (list(title = which_measure)),
                zaxis = list(autorange = "reversed"))
               ) %>% 
        layout(
          scene = list(
            aspectmode = 'manual',  # Options: 'auto', 'cube', 'data', 'manual'
            aspectratio = list(x = 2, y = 1, z = 1)  # Adjust these values as needed
          )
        )

      # ggplot(data = data_to_plot, mapping = aes(x = `Hydrolab Depth`, y = measure)) +
      #   geom_point(aes(color=as.factor(DOY))) + 
      #   geom_path(aes(color=as.factor(DOY))) +
      #   
      #   labs(x = "Depth (m)", 
      #        y = which_measure) +
      #   scale_x_reverse() + 
      #   coord_flip () + 
      #   facet_grid(Site ~ Year, scales = "free")
      
    })
    # # Plot & Datatable Output -------------------------------------------------------------
    output$lakes_plot <- renderPlotly({ ######## LAKES PHYSCHEM
      sub_data <- plot_lakes_select(lake_data)
      which_measure <- input$measurement

      ### Reactive Dataframe ###
      data$lakes_sub_data <- sub_data %>%
        # pick measurement
        select(contains(which_measure), everything())

      ### Plot
      data_to_plot <- sub_data  %>% # Choose measurement to graph
        rename_with(~sub(which_measure, 'measure', .),contains(which_measure))

      plotly::ggplotly(
      ggplot(data = data_to_plot, mapping = aes(x = `Hydrolab Depth`, y = measure)) +
        geom_point(aes(color=as.factor(DOY))) +
        geom_path(aes(color=as.factor(DOY))) +
        labs(x = "Depth (m)",
             y = which_measure) +
        scale_x_reverse() +
        coord_flip () +
        facet_grid(Site ~ Year, scales = "free")
      ) %>% 
        layout(dragmode = "select") %>%
        event_register("plotly_selecting")

    })
    
  }
  
)
