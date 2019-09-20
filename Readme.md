# Shiny App Experiment

Simply playing with Shiny to get a feel for how it creates apps. The Shiny app has tabs that have different apps on them. 

## Simple Walking App

This app has two buttons, Walk and Rest, and displays distance in steps and stamina in units. Clicking the Walk button allows the user to increase distance by 1 step at the expense of 1 stamina. Once stamina has run out, clicking the Walk button does nothing. Clicking the Rest button increases stamina by 1.

## File Import and Display App

This app takes in very specific files, data files and a color file. There can be multiple data files, but only one color file. Data files are expected to have the columns: SampleID, X, Y, where X and Y are numeric values. The color file is expected to have the columns: SampleID, Color, where Color is a color natively available in R. All the data is combined into a single table. A plot of ln(Y) versus X is plotted with the dots the same color as Color. A list of colors separated by spaces is then able to be entered into a text box. A table displaying statistics of column Y based on Color is displayed. 

