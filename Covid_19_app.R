############################################
##### COVID-19 death Interactive Plot  #####
############################################

#-----------------------------------
# Import the data
#-----------------------------------

# import the libraries needed
library(dplyr)
library(data.table)
library(ggplot2)
library(data.table)
library(magrittr)
library(readr)
library(scales)
library(shiny)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(ggthemes)

# Import the data set of "COVID-19 death"
coronaD <- read_csv("time_series_covid19_deaths_global.csv")

# Set the data frame "coronaD" as a data table
setDT(coronaD)

# Save all the date columns as a vector
Dates <- colnames(coronaD[, !1:4])

# Group by countries
coronaD <- aggregate(coronaD[, !1:4], by = list(coronaD$`Country/Region`) , FUN = sum, na.rm = TRUE)


# Rename the Country column after grouping
setnames(coronaD, old = 'Group.1', new = 'Country')

# Remove Eritrea's row
# as Eritrea has NA for population, which cannot be taken into account for Death per Capita Calculation
coronaD <- coronaD[!(coronaD$`Country`== 'Eritrea'),]


#--------------------------------------------------------
# Create a new row for 27 EU countries in the data table
#--------------------------------------------------------

EU27 = c("Austria", "Belgium", "Bulgaria", "Croatia", 
         "Cyprus", "Czech Republic", "Denmark", 
         "Estonia", "Finland", "France", "Germany", "Greece", 
         "Hungary", "Ireland", "Italy", "Latvia", "Lithuania", 
         "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", 
         "Romania", "Slovakia", "Slovenia", "Spain", "Sweden")
# Select EU27 countries from coronaD data table
EU27DT = subset(coronaD,subset = coronaD$Country %in% EU27 )


# Calculate the sums of each date column
colSums(EU27DT[,2:length(EU27DT)])

# Add the row of sums of death of EU27 to the coronaD (death table)
coronaD <- rbind(coronaD, c(Country='EU27', colSums(EU27DT[,2:length(EU27DT)])))

# set CoronaD as a data table
setDT(coronaD)


#---------------------------------------
# Change a wide table to a long table
#---------------------------------------

# Convert the coronaD table into a long table with the name "PlotDT"
PlotDT = melt(coronaD, id.vars = 1, 
              measure.vars = Dates,  
              variable.name = "Date",
              value.name = "Dead")

# Change the data type of the date columns in date format
PlotDT$Date = PlotDT$Date %>% as.Date(format="%m/%d/%y")
PlotDT$Dead = PlotDT$Dead %>% as.numeric()

# Rearrange the order of the table by Country name
PlotDT <- setorder(PlotDT,Country)


#-----------------------------------------------
# Create a data table of Death with population
#-----------------------------------------------

# Import the data of population
worldPop <- read.csv("worldPop.csv") 

# Rename the column names of "worldPop"
colnames(worldPop) <- c("Country Name", "population")

# Set "Country name" as character type from factor
worldPop$`Country Name` <- as.character(worldPop$`Country Name`)

# Set data frame to data table
setDT(worldPop)

# Rename the "Country Name" column of "coronaD" for merging
colnames(coronaD)[1] <-"Country Name"

# Merge the coronaD and worldPop tables
coronaDpC <- merge(worldPop,coronaD, by="Country Name")

# Convert data frame of corona death per capita into a data table
setDT(coronaDpC)


### For creating a new row "EU27" ###

# Select EU27 countries from coronaDpC data table
EU27DpCDT = coronaDpC[coronaDpC$`Country Name` %in% EU27]
setDT(EU27DpCDT)

# Convert all the date column from character to numeric for EU27DpCDT
EU27DpCDT <- cbind(EU27DpCDT[,1:2], sapply(EU27DpCDT[,!1:2], as.numeric))


# Add EU27 data into death per capita table
add_rowEU <- cbind(data.table(`Country Name`='EU27', population = colSums(EU27DpCDT[,2]), 
                  t(colSums(EU27DpCDT[,3:length(EU27DpCDT)]))))
coronaDpC <- rbind(coronaDpC,add_rowEU)

# Convert all the date column from character to numeric for coronaDpCDT
coronaDpC <- cbind(coronaDpC[,1:2], sapply(coronaDpC[,!1:2], as.numeric))


#----------------------------------------------
# Calculate death per capita for all dates
#----------------------------------------------

# Create a function to calculate the death per capita
DeathPerCapita <- function( population, days){
    
    deathOnThatdate <- (days/population)
    return (deathOnThatdate)
    
}


#-----------------------------------------------
# Create a data table of Death per Capita
#-----------------------------------------------

# Combine the information of each country and its death per capita each day into a data frame
calculatedDeathPerCapita <- sapply(coronaDpC[,!1:2], DeathPerCapita, population = coronaDpC$population)

# Form a new data frame with the calculated Death per Capita
coronaDpC <- data.frame(coronaDpC[,1:2], calculatedDeathPerCapita)

# Remove the 'x' of the date columns 
colnames(coronaDpC) =  colnames(coronaDpC) %>% 
    gsub("X","",.) %>%
    gsub("\\.","/",.) 

# Rename "Country Name" column without a "/"
names(coronaDpC)[names(coronaDpC)=='Country/Name'] <- 'Country'

# Convert data frame of Corona Death per Capita into a data table
setDT(coronaDpC)

# Gather all date columns and rename them
DatesOfDeath = colnames(coronaDpC[,!(1:2)])


#---------------------------------------
# Change a wide table to a long table
#---------------------------------------

# Melting the data table from wide to long
PlotPCDT <- melt(coronaDpC, id.vars = 1:2, 
                    measure.vars = DatesOfDeath,  
                    variable.name = "Date",
                    value.name = "Dead")

# Change the data type of the Date column to “Date”
PlotPCDT$Date = PlotPCDT$Date %>% as.Date(format="%m/%d/%y")

# Rename the coloumn names of PlotPCDT
colnames(PlotPCDT)=c("Country","population","Date","Dead")


#-------------------------------------------------------
# Create separate tables for lockdown start and end dates
#-------------------------------------------------------

# Load and set lockdown as a data table
lockdown <- read.csv("lockdown.csv")
setDT(lockdown)
lockdown$Country <- as.character(lockdown$Country)

# Clean data table and set start/end dates as date format
lockdown = lockdown[,1:3]
lockdown$Start = as.Date(lockdown$Start,format="%d/%m/%y" ) 
lockdown$End = as.Date(lockdown$End,format="%d/%m/%y")

# Merge the lockdown table with PlotDT for start date
lockDT = merge(x = lockdown,
               y = PlotDT,
               by.x = c("Country","Start"),
               by.y = c("Country","Date"), all = FALSE) %>%
  .[,c('Country','Start','Dead')]
colnames(lockDT) = c("Country","Lockdown Start Date","Death number on lockdown date")

# Merge the lockdown table with PlotPCDT for start date
lockDTPC = merge(x = lockdown,
                 y = PlotPCDT,
                 by.x = c("Country","Start"),
                 by.y = c("Country","Date"), all = FALSE) %>%
  .[,c('Country','Start','Dead')]
colnames(lockDTPC) = c("Country","Lockdown Start Date","Death number on lockdown date")


# Merge the lockdown end table with PlotDT for end date
lock_endDT = merge(x = lockdown,
                   y = PlotDT,
                   by.x = c("Country", "End"), 
                   by.y = c("Country", "Date"), all = FALSE) %>%
  .[,c('Country','End','Dead')]

colnames(lock_endDT) = c("Country","Lockdown End Date","Death number when lockdown is over")


# Merge the lockdown end table with PlotPCDT for end date
lock_endDTPC = merge(x = lockdown,
                     y = PlotPCDT,
                     by.x = c("Country","End"),
                     by.y = c("Country","Date"), all = FALSE) %>%
  .[,c('Country','End','Dead')]
colnames(lock_endDTPC) = c("Country","Lockdown End Date","Death number when lockdown is over")


#--------------------------------------------------------------
# Create the function for the plot of Death in absolute number
#--------------------------------------------------------------

# Create a function for showing Linear scale or Logarithmic scale of the Death
myPlot_date=function(countrylist, scale, date, Maxvalue){
    
    PlotDeath = ggplot(PlotDT, aes(x= Date, y = Dead, group = `Country`)) + 
        geom_line(colour ="grey") + # All other unselected countries are in grey color
        geom_line(data = PlotDT[`Country` %in% countrylist ], 
                         aes(colour = `Country`)) + # The selected countries are colored 
        scale_x_date(limits = as.Date(date)) + # Limit the scale on the x-axis to a selected date
        geom_point(data = lockDT[`Country` %in% countrylist], aes(x = `Lockdown Start Date`, y = `Death number on lockdown date`), 
                   size = 1)+ # Mark the start time of lockdown with the points  
        geom_point(data = lock_endDT[`Country` %in% countrylist], aes(x = `Lockdown End Date`, y = `Death number when lockdown is over`),
                   colour = "red", size = 1)+ # Mark the end time of lockdown with the points
        ggtitle("Deaths from Covid 19", subtitle = "Data: John Hopkins") + # Add a title and a subtitle
        theme_solarized() + # Add some of the layout-elements
        theme(plot.title  = element_text(family= "Arial", face = "bold.italic", colour = "black", size = 18))
    
    
    if (scale == "logarithmic") {
        PlotDeath = PlotDeath + # If logarithmic scale is selected, change the scale of y-axis to logarithmic
            scale_y_log10(name = "Dead (logarithmic scale)", 
                          limits = c(1,(Maxvalue*(max(PlotDT$Dead)))), breaks = waiver(), 
                          minor_breaks = waiver(), n.breaks = 8, labels =  format_format(scientific= FALSE))  
    }
    else if (scale == 'linear'){
      PlotDeath = PlotDeath + # If linear scale is selected, change the scale of y-axis to linear
            scale_y_continuous(limits = c(NA, (Maxvalue*(max(PlotDT$Dead)))),
                               breaks = waiver(), minor_breaks = waiver(), n.breaks = 8)  
    }
    return(PlotDeath)
}

# Test the function
date=c("2020-02-15","2020-04-07")
countrylist = c("Belgium","France")
Maxvalue = 0.4
PlotDeath = myPlot_date(countrylist, "linear",date, Maxvalue)
PlotDeath # Show the plot

PlotDeath = myPlot_date(countrylist, "logarithmic",date, Maxvalue)
PlotDeath # Show the plot


#-------------------------------------------------------
# Create the function for the plot of Death per Capita
#-------------------------------------------------------

# Create a function for showing Linear scale or Logarithmic scale of Death per Capita
myPlotPC_date=function(countrylist, scale, date, Maxvalue){
  PlotDeathPC = ggplot(PlotPCDT, aes(x= Date, y = Dead, group = `Country`)) + 
    geom_line(colour ="grey") + # All other unselected countries are in grey color
    geom_line(data = PlotPCDT[`Country` %in% countrylist ],
              aes(colour = `Country`)) + # The selected countries are colored
    scale_x_date(limits = as.Date(date)) + # Limit the scale on the x-axis to a selected date
    geom_point(data = lockDTPC[`Country` %in% countrylist], aes(x = `Lockdown Start Date`, y = `Death number on lockdown date`), 
               size = 1)+ # Mark the start time of lockdown with the points
    geom_point(data = lock_endDTPC[`Country` %in% countrylist], aes(x = `Lockdown End Date`, y = `Death number when lockdown is over`),
               colour = "red", size = 1)+ # Mark the end time of lockdown with the points
    ggtitle("Deaths per capita from Covid 19", subtitle = "Data: John Hopkins") +  # Add a title and a subtitle
    theme_solarized() + # Add some of the layout-elements
    theme(plot.title  = element_text(family= "Arial", face = "bold.italic", colour = "brown", size = 18))
  
  
    
  if (scale == "logarithmic") {
    PlotDeathPC = PlotDeathPC + # If logarithmic scale is selected, change the scale of y-axis to logarithmic
      scale_y_log10(name = "Dead (logarithmic scale)",
                    limits = c(NA,(Maxvalue*(max(PlotPCDT$Dead)))), breaks =  waiver(),
                    minor_breaks = waiver(), n.breaks = 8) 
    
    
    }
  else if (scale == 'linear'){
      PlotDeathPC = PlotDeathPC + # If linear scale is selected, change the scale of y-axis to linear
        scale_y_continuous(limits = c(NA, (Maxvalue*(max(PlotPCDT$Dead)))), breaks = waiver(), 
                           minor_breaks = waiver(), n.breaks = 8) 
    }
  return(PlotDeathPC)
}

# Test the function
date=c("2020-02-15","2020-04-07")
Maxvalue = 1
countrylist = c("China","France")
PlotDeathPC = myPlotPC_date(countrylist, "linear",date, Maxvalue)
PlotDeathPC # Show the plot

PlotDeathPC = myPlotPC_date(countrylist, "logarithmic",date, Maxvalue)
PlotDeathPC # Show the plot



#-------------------
# Shiny App -- UI
#-------------------

ui <- bootstrapPage(
    navbarPage(
        theme = shinytheme("sandstone"), collapsible = TRUE, # Set the theme of the website
        "COVID-19", # Main title of the website
        
        tabPanel("Country/Region plots", # Title of the first webpage tab 
                 sidebarLayout(  # side bar for input on the left
                     sidebarPanel(
                         pickerInput("region_select", "Country/Region:", # Choose the countries to show
                                     choices = as.character(unique(coronaD$`Country`)), # Set the range of selectable countries
                                     options = list('action-box' = TRUE, 'none-selected-text' = "please select at least one country!"),
                                     selected = as.character(c("Italy","US","China")), # Defaulted countries
                                     multiple = TRUE # Allow multiple choices of input
                         ),
                         pickerInput(
                             "outcome_select", "Outcome:", 
                             choices = c("Deaths per capita", "Deaths(total)"), # Choose either "Death" or " Death per Capita"
                             selected = c("Deaths(total)"), # Defaulted outcome
                             multiple = FALSE # Not allow multiple choices of input
                         ),
                         
                         sliderInput("select_yaxis",
                                     "Select the scale of y-axis:", # Choose a time frame to show
                                     min = 0.01,  # Set the range of time frame
                                     max = 1,
                                     value = 1,
                                     
                                     # Defaulted time frame
                         ),   
                         
                         sliderInput("select_date",
                                     "Select a date range:", # Choose a time frame to show
                                     min = as.Date(min(PlotDT$Date),"%Y-%m-%d"), # Set the range of time frame
                                     max = as.Date(max(PlotDT$Date),"%Y-%m-%d"),
                                     value = as.Date(c("2020-02-15","2020-04-07")), # Defaulted time frame
                                     timeFormat = "%d %b"), # Set the date format
                         "Select outcome, regions, y-axis scale and plotting start date from drop-down menus to update plots."
                     ),
                     mainPanel( # Main panel showing the plot
                         tabsetPanel( # 2 tabs for choosing Linear scale or Logarithmic scale
                             tabPanel("Linear", plotlyOutput("plot_death")), # Linear plot tab
                             tabPanel("Logarithmic(log10)", plotlyOutput("plot_death_log")) # Logarithmic plot tab
                         )
                     )
                 )
        )
    )
)


#-----------------------
# Shiny App -- Server
#-----------------------

server = function(input, output) {
    country_reactive_total = reactive({ # Create a reactive plot
        
        if(input$outcome_select=="Deaths(total)"){ # Determine the Linear plot shown: Death(total) or Death per Capita
            myPlot_date(input$region_select,"linear",input$select_date, input$select_yaxis)}
        else{
            myPlotPC_date(input$region_select,"linear",input$select_date, input$select_yaxis)}
    })
    
    country_reactive_total_log = reactive({
        if(input$outcome_select=="Deaths(total)"){ # Determine the Logarithmic plot shown: Death(total) or Death per Capita
            myPlot_date(input$region_select,"logarithmic",input$select_date, input$select_yaxis)
        }
        else{
            myPlotPC_date(input$region_select,"logarithmic",input$select_date, input$select_yaxis)
        }
    })
    
    output$plot_death <-renderPlotly({country_reactive_total()}) # Show the selected Linear plot
    output$plot_death_log <- renderPlotly({country_reactive_total_log()}) # show the selected Logarithmic plot
    
}


#----------------------------
# Run the COVID-19 App
#----------------------------

shinyApp(ui,server)




