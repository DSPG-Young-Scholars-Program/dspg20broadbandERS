library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dashboardthemes)

source("theme.R")

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
          text = "Data & Methodology",
          icon = icon("database")
        ),
        menuItem(
          tabName = "findings",
          text = "Findings",
          icon = icon("chart-pie")
        ),
        menuItem(
          tabName = "vatable",
          text = "Virginia Table",
          icon = icon("map-marked-alt")
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
                      tags$li("What is the quality (coverage, representativeness) of CoreLogic 2018 property data compared to benchmark datasets, particularly for rural areas?"), 
                      tags$li("How can we scale this meaningfully to the national level when we only have property-level data for certain counties?")
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
                        tags$li("Property data from New Kent County, Virginia, which is more rural and less affluent than Fairfax County"),
                        tags$li("Housing data from the American Community Survey (ACS), a national survey undertaken by the Census Bureau")),
                    h4("Data Profiling"),
                    p("We profiled the CoreLogic data subset for Fairfax County and New Kent County in Virginia, in addition to the Fairfax County and New Kent County data sets described above, to compare the completeness and variables included in each dataset."),
                    h4("Data Preparation and Linkage"),
                    p("We used the Census geocoder to geocode the subsets of the CoreLogic data for Fairfax and New Kent that did not have a property-level latitude and longitude. We also geocoded the New Kent County data as latitude and longitude was not provided in the original data. We attempted record linkage between the county and CoreLogic data and learned that linkage on latitude and longitude may not be possible given the way latitudes and longitudes in the CoreLogic data were generated."),
                    p("We additionally grouped the CoreLogic data by census tract and created equivalent variables to ACS estimates for total housing counts, vacancy status, year built, and assessed property value. We then linked these estimates at the census tract level and began exploring the differences between CoreLogic and ACS estimates."),
                    h4("Statistical Analysis"),
                    p("Using the linked CoreLogic and ACS data, we benchmarked the CoreLogic data using a \"fitness-for-use\" metric derived from a previous Census partnership. Discussed more in the methodology section, the fitness-for-use metric takes into account both the ACS estimate and the ACS margin of error when comparing with the CoreLogic values.")
                  ),
                  boxPlus(
                    title = "Ethical Considerations and Limitations",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    p("This project will give enable researchers to better understand the quality of CoreLogic data, including its strengths and weaknesses. Many traditional components of an ethical analysis (for example, whether data is geographically representative) are actually inherently part of our project goals."),
                    p("However, we still made assumptions in the framing of our project that may impact the quality of our results. These assumptions included:"),
                    tags$ul(
                      tags$li("Results from just a few counties or states can be meaningfully generalized to a larger sample (while we only currently have results for Virginia, the data quality for a different state may look very different)"), 
                      tags$li("Housing units are defined similarly across counties and geographic areas (we had to make assumptions about which property types were residential in order to draw comparisons between CoreLogic and ACS data)"),
                      tags$li("Data taken directly from counties (such as the Fairfax County county-level data we accessed) can serve as \"truth\" in evaluating data quality (this data may have its own quality issues)"),
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
                    width = NULL
                  ),
                  p("Explanatory text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
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
                    p("Include years covered, variables used in analysis, and elaborate on the specific corelogic dataset. Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("American Community Survey"),
                    p("Include years covered (what survey was used) variables (tables) used, and explain geographic areas (tracts) and general ACS process. Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("Fairfax County Data"),
                    p("Include years covered, source of data, and variables included. Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("New Kent County Data"),
                    p("Include years covered, source of data, and variables included."),
                    h3("RUCA codes"),
                    p("Include years covered, source of data, and variables included."),
                    h2("Methodology"),
                    h3("Geocoding"),
                    p("Elaborate on geocoding process and record linkage attempts. Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("ACS Linkage"),
                    p("Elaborate on linking ACS data (assumptions made, variables chosen). Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("Fitness-for-Use Metric"),
                    p("Explain fitness for use metric in greater detail."),
                    h3("References"),
                    p("Link at least the census paper, any package documentation, anything relevant from our literature review.")
                  )
                )),
    
        tabItem(tabName = "findings",
               tabsetPanel(tabPanel("Profiling", 
                                    fluidRow(
                                      boxPlus(
                                        title = "Data Profiling",
                                        closable = FALSE,
                                        width = NULL,
                                        status = "warning",
                                        solidHeader = TRUE,
                                        collapsible = TRUE
                                        ,
                                        p("Data Profiling was the first task we undertook in determining the quality of the CoreLogic after performing the data discovery process. We profiled the Fairfax and New Kent county subsets of the CoreLogic housing data, in addition to the Fairfax county and New Kent county datasets. 
                                          Our profiling process focused on the six variables that our sponsors at the USDA highlighted as having the greatest effect on property prices: lot size, square footage, number of beds, number of baths, age, assessed value. To profile the data we considered the completeness of these variables, 
                                          the distribution of property types")
                                        ),
                                      boxPlus(
                                        title = "Fairfax Profiling",
                                        closable = FALSE,
                                        width = 6,
                                        status = "warning",
                                        solidHeader = FALSE,
                                        collapsible = FALSE,
                                        p("Greater range of missing values in the CoreLogic dataset"),
                                        img(src = "Missing_ffx.png", width = "360px", align = "left"),
                                        img(src = "Missing_CL_ffx.png", width = "360px", align = "center"),
                                        img(src = "fairfax_hist.png", width = "360px", align = "left")
                                      ),
                                      boxPlus(
                                        title = "New Kent Profiling",
                                        closable = FALSE,
                                        width = 6,
                                        status = "warning",
                                        solidHeader = FALSE,
                                        collapsible = FALSE,
                                        p("Greater range of missing values in the CoreLogic dataset"),
                                        img(src = "Missing_nk.png", width = "360px", align = "left"),
                                        img(src = "Missing_cl_nk.png", width = "360px", align = "right"),
                                        img(src = "nk_hist.png", width = "360px", align = "left"))
                                    
                                    )),
                           tabPanel("Geocoding",
                                    boxPlus(
                                      title = "Geocoding",
                                      closable = FALSE,
                                      width = NULL,
                                      status = "warning",
                                      solidHeader = TRUE,
                                      collapsible = TRUE
                                      ,
                                      p("Something Something")
                                    ),
                                    boxPlus(
                                      title = "Missing values in the latitude and longitude variables",
                                      closable = FALSE,
                                      width = NULL,
                                      status = "warning",
                                      solidHeader = FALSE,
                                      collapsible = FALSE,
                                      h3("Missing in CoreLogic data"),
                                      img(src = "lat_long_cl_ffx.png", width = "700px"),
                                      img(src = "lat_long_cl_nk.png", width = "700px"),
                                      h3("Missing in county data"),
                                      img(src = "ffx_lat_long.png", width = "700pxpx"),
                                      p("NOTE: The New kent county dataset does not have any latitude and longitude variables, and so the entire dataset had to be gecoded as compared to just some parts in the other datasets.")
                                    )),
                           tabPanel("Metrics")   
               )
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
                      DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy.")),
                  boxPlus(
                    title = "Vatsala Ramanan",
                    closable = FALSE,
                    width = 4,
                    status = "warning",
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    (img(src = "Vatsala_Ramanan.jpg", width = 250, height = 270)),
                    p("DSPG 2020 Intern, Quantitative Economics and Government at Smith College, Github: v-ramanan")), 
                  boxPlus(
                    title = "Morgan Klutzke",
                    closable = FALSE,
                    width = 4,
                    status = "warning",
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    (img(src = "UVA.intern.Morgan.Klutzke.jpg", width = 250, height = 270)),
                    p("DSPG 2020 intern, Indiana University, Psychology and Cognitive Science, Gihub: mklutzke")), 
                  boxPlus(
                    title = "Madeline Pickens",
                    closable = FALSE,
                    width = 4,
                    status = "warning",
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    (img(src = "maddie.jpg", width = 250, height = 270)),
                    p("DSPG 2020 Fellow, Georgetown University, Data Science for Public Policy, Github: mclaire19")),
             
                  boxPlus(
                    title = "UVA SDAD members",
                    closable = FALSE,
                    width = 12,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    p("[Photos go about here.]"),
                    h2("Project Sponsors"),
                    p("[Photos, information, and/or links about your sponsor go about here. You may want to use materials that your sponsors have already shared with you about their institution or coordinate with your stakeholders to include pertinent information here.]")
                  )
                ))
      )
    )) 
), 
  


# SERVER ------------------------------------------------------------------
  server = function(input, output) {
    
    # Render Plot 1
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
    # Render Plot 2
    output$distPlot2 <- renderPlot({
      hist(rnorm(input$obs2))
    })
    
   
  }
) 