#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)
library(markdown)
library(plotly)


# Useful Vectors ----------------------------------------------------------
lake_data <- read_rds("data/ARC_Lakes_Physchem_2010_2021.ver5.rds") #load dataframe 

## Useful Objects for Plotting 
site_list <- unique(lake_data$Site)
year_list <- seq(min(lake_data$Year), max(lake_data$Year), by = 1) 
measure_list <- names(lake_data)[7:19]
doy_list <- unique(lake_data$DOY)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  navbarPage("Arctic LTER Lakes Physchem Data",
             
             
             tabPanel("Physical Chemistry - 3D plot",
                      
                      # Fluid row layout with input and output definitions ----
                      fluidRow(
                      
                        column(width = 2,
                               
                               selectizeInput("lake_year_3D",
                                              h4("Year"),
                                              choices = year_list,
                                              selected = year_list[1])
                        ),
                         
                         column(width = 2, 
                                
                                # Input: Checkboxes for Site selection ----
                                selectizeInput("lake_site_3D", 
                                               h4("Site"), 
                                               choices = setNames(nm = site_list),
                                               selected = site_list[1])
                         ),
                        column(width = 3, 
                               
                               # Input: ordinary selectize input without option groups
                               selectizeInput('measurement_3D', 
                                              h4('Measurement Type'),
                                              choices = setNames(nm = measure_list))
                        ),
                        column(width = 4,
                               sliderInput("dayofyear",
                                           h4("Day of Year"),
                                           min = min(doy_list), max = max(doy_list),
                                           value = c(min(doy_list),max(doy_list)),
                                           step=1,
                                           sep=""))
                      ),
                      
                      # LAKE Plot Data ----
                      plotlyOutput('lakes_3D_plot', height = "900px")
             ),

             tabPanel("Physical Chemistry - plots and data table",
                      # LAKE Plot Data ----
                      plotlyOutput('lakes_plot', height = "500px"),
                      #textOutput('vector'),

                      hr(), # horizontal line break

                      # Fluid row layout with input and output definitions ----
                      fluidRow(

                        column(width = 2,

                               # Input: ordinary selectize input without option groups
                               selectizeInput('measurement',
                                              h4('Measurement Type'),
                                              choices = setNames(nm = measure_list)),

                               # Input: Specification of range within an interval ----
                               sliderInput("lake_years",
                                           h4("Years"),
                                           min = min(year_list), max = max(year_list),
                                           value = c(min(year_list),max(year_list)),
                                           step=1,
                                           sep=""),

                               # Input: Checkboxes for Site selection ----
                               checkboxGroupInput("lake_sites",
                                                  h4("Sites"),
                                                  choices = setNames(nm = site_list),
                                                  selected = site_list[1])
                        ),

                        column(width = 4,

                               DT::dataTableOutput("lakes_table")
                        )


                      )
             )
  )
  
))
