library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dashboardthemes)
library(tidyverse)
library(DT)

source("theme.R")

va_table_long <- read_csv('data/va_table_long.csv')
vacounties <- unique(va_table_long$County)

ffu_va_value <- va_table_long %>% filter(str_detect(variable, "value"))
ffu_va_yrbuilt <- va_table_long %>% filter(str_detect(variable, "yrbuilt"))
ffu_va_housing <- va_table_long %>% filter(str_detect(variable, "housing|occ"))

ruca_def <- read_csv('data/ruca_def.csv')

shinyApp(
  ui = dashboardPagePlus(
    title = "DashboardPage",
    header = dashboardHeaderPlus(
      title = "DSPG 2020"
      ),

# SIDEBAR (LEFT) ----------------------------------------------------------
    sidebar = dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem(
          tabName = "overview",
          text = "Project Overview",
          icon = icon("info circle")
        ),
        menuItem(
          tabName = "data",
          text = "Data Sources & Methodology",
          icon = icon("database")
        ),
        menuItem(
          tabName = "profiling",
          text = "Data Profiling",
          icon = icon("chart-pie")
        ),
        menuItem(
          tabName = "vatable",
          text = "Virginia Counties",
          icon = icon("bar-chart")
        ),
        menuItem(
          tabName = "vamap",
          text = "Virginia Map",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "team",
          text = "Team",
          icon = icon("user-friends")
        )
      )
    ),

# BODY --------------------------------------------------------------------
    body = dashboardBody(
      customTheme,
      fluidPage(
      tabItems(
        tabItem(tabName = "overview",
                fluidRow(
                  boxPlus(
                    title = "Project Overview",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h1("2020 DSPG USDA-ERS: Evaluating Residential Property Data Quality"),
                    h2("Project Description"), 
                    p("To explore the influence of broadband access on rural property values, this project evaluates the quality of CoreLogic property data, which is aggregated commercial and residential property data based on county tax assessments and property deeds. We are comparing American Community Survey estimates to CoreLogic counts and estimates for relevant housing variables. Property-level comparisons at the national level would not be possible without access to the underlying local data. We additionally focus on two Virginia counties for which we have local property-level data, Fairfax County and New Kent County, as case studies for additional data quality evaluations. This project will enable the Social and Decision Analytics team and other CoreLogic data users to learn about the strengths and limitations of CoreLogic data."),
                    h2("Project Goals"),
                    p("The project sought to address the following questions:"), 
                    tags$ul(
                      tags$li("What is the coverage and representativeness of CoreLogic 2018 property data compared to benchmark datasets, particularly for rural areas?"), 
                      tags$li("How can we scale this meaningfully to the state level when we only have property-level data for certain counties?")
                    )
                    ),
                  
                  boxPlus(
                    title = "Our Approach",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    p("Our project process included the following elements of the data science framework:"),
                    h4("Data Discovery"),
                    p("We undertook a data discovery process to find datasets to benchmark against the proprietary CoreLogic housing dataset. We considered sample size, accessibility, geography, variables provided, and unit of analysis to ensure the datasets were appropriate and useful to the project. We narrowed down our findings to:"),
                      tags$ul(
                        tags$li("Property data from Fairfax County, Virginia, which is primarily affluent and urban"),
                        tags$li("Property data from New Kent County, Virginia, which is smaller and less affluent than Fairfax County"),
                        tags$li("Housing data from the American Community Survey (ACS), a national survey undertaken by the Census Bureau")),
                    p("We also evaluated the following sources, but determined they were not as comprehensive as ACS:"),
                    tags$ul(
                      tags$li("American Housing Survey"),
                      tags$li("Black Knight"),
                      tags$li("Multiple Listing Service"),
                      tags$li("Zillow"),
                      tags$li("USPS Vacancy Data"),
                      tags$li("Housing Assistance Council"),
                      tags$li("Housing Mortgage Disclosure Act")),
                    h4("Data Profiling"),
                    p("We profiled the CoreLogic data subset for Fairfax County and New Kent County in Virginia, in addition to the Fairfax County and New Kent County property data, to compare the completeness and variables included in each dataset."),
                    h4("Data Preparation and Linkage"),
                    p("We used the Census geocoder to geocode the subsets of the CoreLogic data for Fairfax and New Kent that did not have a property-level latitude and longitude. We also geocoded the New Kent County data as latitude and longitude was not provided in the original data. We attempted record linkage between the county and CoreLogic data and learned that linkage on latitude and longitude may not be possible given the way latitudes and longitudes in the CoreLogic data were generated."),
                    p("We additionally grouped the CoreLogic data by census tract and created equivalent variables to ACS estimates for total housing counts, vacancy status, year built, and assessed property value. We then linked these estimates at the census tract level and began exploring the differences between CoreLogic and ACS estimates."),
                    h4("Statistical Analysis"),
                    p("Using the linked CoreLogic and ACS data, we benchmarked the CoreLogic data using a \"fitness-for-use\" metric derived from a previous Census partnership. This is discussed more in the methodology section. The fitness-for-use metric takes into account both the ACS estimate and the ACS margin of error when comparing with the CoreLogic values.")
                  ),
                  boxPlus(
                    title = "Ethical Considerations and Limitations",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    p("This project will give enable researchers to better understand the coverage and representativeness of CoreLogic data. Many traditional components of an ethical analysis (for example, whether data is geographically representative) are actually inherently part of our project goals."),
                    p("However, we still made assumptions in the framing of our project that may affect the quality of our results. These assumptions included:"),
                    tags$ul(
                      tags$li("Results from just a few counties or states can be meaningfully generalized to a larger sample (while we only currently have results for Virginia, the data quality for a different state may look very different)"), 
                      tags$li("Housing units are defined similarly across counties and geographic areas (we had to make assumptions about which property types were residential in order to draw comparisons between CoreLogic and ACS data)"),
                      tags$li("Data taken directly from counties (such as the Fairfax County county-level data we accessed) can serve as \"truth\" in evaluating data quality (these data may have its own quality issues)"),
                      tags$li("It is meaningful to compare property-level data (i.e. CoreLogic data) and survey aggregations (e.g. ACS data) (we had to make several assumptions in order to directly compare geographic levels of these data types)")
                    ),
                    p("Finally, it is worth noting that aggregated data can pose a greater threat to privacy than its individual counterpart. When working with a large proprietary dataset with personally identifiable information, such as the CoreLogic data, we have an ethical responsibility to maintain privacy and anonymity of individuals in our presentation of results.")
                  )
                  
                )),
        tabItem(tabName = "vamap",
                fluidRow(
                  boxPlus(
                    title = "Virginia Map",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL
                  ),
                  p("Explanatory text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                  br()
                )),
        tabItem(tabName = "vatable",
                fluidRow(
                  boxPlus(
                    title = "Virginia Table",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    selectInput(
                      inputId = 'vacty',
                      label = 'Select a Virginia County',
                      choices = vacounties),
                    uiOutput("ruralText"),
                    p("If the plots and table are empty, CoreLogic did not have data for this county."),
                    dataTableOutput("countyTable"),
                    plotOutput('ctyvalue'),
                    plotOutput('ctyyrbuilt'),
                    plotOutput('ctyhousing')
                  ),
                  br()
                )),
        tabItem(tabName = "data",
                fluidRow(
                  boxPlus(
                    title = "Data & Methodology",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h2("Data Sources"),
                    h3("CoreLogic Property Data"),
                    p("CoreLogic is a commercial data aggregator focused on property data. We had access to several CoreLogic datasets for use in this project, but focused on tax assessment data purchased by SDAD."),
                    p("While multiple years of tax assessment data were available, we limited the data to properties assessed in 2018 for consistency across the other datasets we were using. While we profiled all the available CoreLogic variables initially, our final analysis and ACS comparisons focused on the number of bedrooms, the assessed total value, the year a property was built, and the property type. We additionally used the census tract and FIPS variables in the CoreLogic data to enable merging with ACS estimates. For our geocoding analysis, we also focused on the latitude and longitude of a property."),
                    h3("American Community Survey (ACS)"),
                    p("The American Community Survey (ACS) is an ongoing survey performed by the US Census Bureau that asks more detailed demographic questions about individuals than the decennial census. ACS data includes housing information as provided by individuals. Estimates are reported at varying levels of geography and accuracy; one-year ACS estimates are assumed to be less accurate than five-year ACS estimates, but are provided more often."),
                    p("Our analysis used the 2018 five-year ACS estimates at the census tract level. We used estimates of the number of housing units in the tract, whether that housing unit was vacant or occupied, and the year a housing unit was built, and the value of a housing unit for comparison with CoreLogic."),
                    h3("Fairfax County Data"),
                    p("Fairfax County provides property assessment data on its open data platform. We used data that had been collected from this platform in 2018 that included 18 property variables."),
                    p("While we profiled the full Fairfax data, for our analysis, we focused on the variables for property type, assessed value, year built, number of bedrooms, number of bathrooms, lot size, and square footage. The county also provided information on the latitude and longitude of a property, so geocoding this dataset was not necessary."),
                    h3("New Kent County Data"),
                    p("New Kent County provides property assessment data on their website as a downloadable Excel file. We downloaded the most recent version of this data, which was the 2020 property tax assessments for the county."),
                    p("While we profiled the full New Kent data, for our analysis, we focused on the variables for assessed value, year built, number of bedrooms, number of bathrooms, lot size, and square footage. New Kent County did not provide a property type variable or latitude and longitude."),
                    h3("Rural-Urban Commuting Area (RUCA) Codes"),
                    p("Rural-Urban Commuting Area (RUCA) Codes categorize census tracts based on their population density and commuting flows. They are a product of the United States Department of Agriculture Economic Research Service."),
                    p('We used RUCA codes to classify census tracts as urban or rural. While the codes range from one to ten (see table below for further definitions) we considered /"rural/" tracts as tracts with codes seven or higher.'),
                    p('ADD TABLE OUTPUT -- ASK'),
                    #tableOutput(rucatable),
                    h2("Methodology"),
                    h3("Geocoding"),
                    p("Elaborate on geocoding process and record linkage attempts. Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("ACS Linkage"),
                    p("ACS provides estimates of counts for a selected geography (in our case, counts in a given census tract), while CoreLogic provides property-level data. Some assumptions were therefore necessary in order to match counts of CoreLogic property characteristics to ACS estimates."),
                    p('To get the overall count of housing units and the occupancy status for each tract, we considered properties in CoreLogic that were coded as single family dwellings, condos, duplexes, apartments, or commercial condos to be "residential" properties. These residential properties were summed by census tract to get the "occupied" count, and properties coded in CoreLogic as "vacant" were summed to get the "vacant" count. "Total housing units" were considered to be the sum of the occupied and vacant counts.'),
                    p('To get the count of the year built variables, CoreLogic data was grouped based on its year built. The bins were constructed to match ACS bins; for example, a property constructed in 1975 would be grouped in the "1970-1979" year-built bin. The properties in each bin were then counted for each census tract'),
                    p('To get the count of the housing value variables, CoreLogic data was grouped based on its assessed total value variable. The bins were constructed to match ACS bins; for example, a property valued at $120,000 would be grouped in the "125,000 - 150,000" value bin. The properties in each bin were then counted for each census tract.'),
                    h3("Fitness-for-Use Metric"),
                    p("Explain fitness for use metric in greater detail."),
                    h3("References"),
                    p("Link at least the census paper, any package documentation, anything relevant from our literature review.")
                  )
                )),
    
        tabItem(tabName = "profiling",
                fluidRow(
                        boxPlus(title = "Data Profiling",
                                closable = FALSE,
                                width = NULL,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                p("We profiled the Fairfax and New Kent county subsets of the CoreLogic housing data, and the Fairfax county and New Kent county datasets, obtained direclty from the county websites.
                                   Our profiling process focused on the six variables that our sponsors at the USDA highlighted as having the greatest effect on property prices:"),
                                tags$ul(
                                    tags$li("Lot Size"),
                                    tags$li("Square Footage"),
                                    tags$li("Number of Bedrooms"),
                                    tags$li("Number of Bathrooms"),
                                    tags$li("Year Built"),
                                    tags$li("Assessed Value")),
                                    p("Our USDA sponsor highlighted these variables are important factors influencing property prices."),
                                    p("To profile these data, we considered the completeness of these variables and the distribution of property types.")
                                           ),
                       boxPlus(title = "Fairfax Profiling",
                               closable = FALSE,
                               width = 6,
                               status = "warning",
                               solidHeader = FALSE,
                               collapsible = FALSE,
                               p("Greater range of missing values in the CoreLogic dataset"),
                               img(src = "Missing_ffx.png", width = "360px", align = "left"),
                               img(src = "Missing_CL_ffx.png", width = "360px", align = "center"),
                               img(src = "fairfax_hist.png", width = "360px", align = "left")),
                       boxPlus(title = "New Kent Profiling",
                               closable = FALSE,
                               width = 6,
                               status = "warning",
                               solidHeader = FALSE,
                               collapsible = FALSE,
                               p("Greater range of missing values in the CoreLogic dataset"),
                               img(src = "Missing_nk.png", width = "360px", align = "left"),
                               img(src = "Missing_cl_nk.png", width = "360px", align = "right"),
                               img(src = "nk_hist.png", width = "360px", align = "left"))
                       ) #************
                ),
        tabItem(tabName = "team",
                fluidRow(
                  boxPlus(
                    title = "Meet the DSPG Team",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the Biocomplexity Institute’s Social and Decision Analytics division (SDAD). In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. 
                      DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy."),
                    h4(tags$a(href="https://github.com/mclaire19", "Madeline Pickens")),
                    (img(src = "maddie.jpg", width = 250, height = 270)),
                    p(""),
                    p("DSPG 2020 Fellow, Georgetown University, Data Science for Public Policy"),
                    h4(tags$a(href="https://github.com/v-ramanan", "Vatsala Ramanan")),
                    (img(src = "Vatsala_Ramanan.jpg", width = 250, height = 270)),
                    p(""),
                    p("DSPG 2020 Intern, Quantitative Economics and Government at Smith College"),
                    h4(tags$a(href="https://github.com/mklutzke", "Morgan Klutzke")),
                    (img(src = "UVA.intern.Morgan.Klutzke.jpg", width = 250, height = 270)),
                    p(""),
                    p("DSPG 2020 Intern, Indiana University, Psychology and Cognitive Science")),
                  boxPlus(
                    title = "UVA SDAD members",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    h4(tags$a(href="https://biocomplexity.virginia.edu/stephanie-shipp", "Stephanie Shipp")),
                    p("Project Lead, Deputy Division Director and Professor (economics)"),
                    h4(tags$a(href="https://biocomplexity.virginia.edu/joshua-goldstein", "Josh Goldstein")),
                    p("Research Assistant Professor, Statistics"),
                    h4(tags$a(href="https://biocomplexity.virginia.edu/neil-kattampallil", "Neil Kattampallil")),
                    p("Research Scientist"),
                    h4(tags$a(href="https://biocomplexity.virginia.edu/devika-mahoney-nair", "Devika Mahoney-Nair")),
                    p("Research Scientist")),
                  boxPlus(
                    title = "Sponsor",
                    closeable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    h4(tags$a(href="https://www.ers.usda.gov/authors/ers-staff-directory/john-pender/", "John Pender")),
                    p("Rural Liaison, Rural Economy Branch,  Economic Research Service, US  Department of Agriculture"),
                    (img(src = "ers.jpeg", width = 200, height = 140))
                  )
                ))
      )
    )) 
), 
  


# SERVER ------------------------------------------------------------------
  server = function(input, output) {
    
    # Render Housing Plot
    output$ctyhousing <- renderPlot({
      cty <- ffu_va_housing %>%
        filter(County == input$vacty) %>%
        filter(variable != 'percent_rural')
      cty %>%
        ggplot(aes(value, variable)) + 
        geom_point() + 
        geom_vline(xintercept = -1, linetype = "dashed", color = "red") + 
        geom_vline(xintercept = 1, linetype = "dashed", color = "red")
    })
    
    # Render Value Plot
    output$ctyvalue <- renderPlot({
      cty <- ffu_va_value %>%
        filter(County == input$vacty) %>%
        filter(variable != 'percent_rural')
      cty %>%
        ggplot(aes(value, variable)) + 
            geom_point() + 
            geom_vline(xintercept = -1, linetype = "dashed", color = "red") + 
            geom_vline(xintercept = 1, linetype = "dashed", color = "red")
    })
    
    # Render Year Built Plot
    output$ctyyrbuilt <- renderPlot({
      cty <- ffu_va_yrbuilt %>%
        filter(County == input$vacty) %>%
        filter(variable != 'percent_rural')
      cty %>%
        ggplot(aes(value, variable)) + 
        geom_point() + 
        geom_vline(xintercept = -1, linetype = "dashed", color = "red") + 
        geom_vline(xintercept = 1, linetype = "dashed", color = "red")
    })
    
    output$ruralText <- renderUI({
      cty <- va_table_long %>%
        filter(County == input$vacty) %>%
        filter(variable == 'percent_rural')
      pctrural <- round(cty$value*100,1)

      h4(paste(input$vacty, 
               " is ", pctrural, " percent rural based on the census tracts it contains.", 
               sep = ""))
    })
    
    output$countyTable <- DT::renderDataTable({
      cty <- va_table_long %>%
        filter(County == input$vacty) %>%
        dplyr::select(-County) %>%
        filter(variable != 'percent_rural') %>%
        mutate(good = ifelse(abs(value) <= 1, 1, 0)) %>%
        datatable() %>% 
        formatStyle(
          columns = 5,
          backgroundColor = styleEqual(c(0, 1), c('gray', 'yellow'))
        )
    })
    
    output$rucatable <- renderTable({
      ruca_def
    })
   
  }
) 