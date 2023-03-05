# Hrudhai_Umas_P6_WebApp

This Shiny application is called the CO2 Emissions Data Explorer. It allows users to analyze the average CO2, nitrous oxide, methane, oil CO2, and trade CO2 emissions for different countries and years. The data used in this app is sourced from the Our World in Data website.

The user interface (UI) of the application contains two panels. The first panel, called the sidebar panel, allows users to select a range of years and countries. It contains a slider input for selecting the range of years and a select input for choosing the countries. Additionally, there is a checkbox input for selecting all countries.

The second panel is the main panel, which displays the results of the user's selection. It consists of three tabs, namely, the "Welcome" tab, the "Plot" tab, and the "Table" tab. The "Welcome" tab contains general information about the app and the data, such as the number of rows and columns in the dataset. It also displays a random sample of the data.

The "Plot" tab displays a plot of CO2 emissions over time for selected countries. Not all countries have data, and those countries will have no visual lines. The user can select the variable they wish to display in the plot from a dropdown menu. This tab also displays the overall average CO2 emissions for the selected countries and years.

The "Table" tab displays tables of the selected subset of data. It contains five tables, one for each of the five emissions types analyzed by the app. The tables display the average emissions for each country in the selected subset of years.

The app is built using the following packages: shiny, tidyverse, dplyr, and DT.
