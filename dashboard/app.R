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
          tabName = "map",
          text = "Interactive Map",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "graph",
          text = "Interactive Graph",
          icon = icon("map-marked-alt")
        ),
        menuItem(
          tabName = "both",
          text = "Mutiple Interactive",
          icon = icon("map-marked-alt")
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
                    h1("2020 DSPG USDA-ERS Broadband"),
                    h2("Project Description"), 
                    p("This DSPG project is a partnership with the USDA Economic Research Service. The project team worked with data from CoreLogic, a commercial data aggregator, to evaluate the quality of their national property data, particularly for rural areas."),
                    h2("Project Goals"),
                    p("The project sought to address the following questions:"), 

p("What is the quality (coverage, representativeness) of CoreLogic 2018 property data compared to benchmark datasets, particularly for rural areas?"),

p("How can we scale this meaningfully to the national level when we only have property-level data for certain counties? ")),
                  
                  boxPlus(
                    title = "Our Approach",
                    closable = FALSE,
                    width = 6,
                    status = "warning",
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    p("Undertook a data discovery process to find datasets to benchmark against the proprietary CoreLogic housing dataset. Narrowed down findings to the Fairfax county (affluent), New Kent (more rural) and American Community Survey datasets.  :"),
                    p("Profiled the CoreLogic data subset for Fairfax and New Kent county in Virginia, in addition to the Fairfax county and New Kent county data set. "),
                    p("Began geocoding the datasets to record link the county and CoreLogic data."),
                    p("Benchmarked the CoreLogic data against the American Community Survey dataset using the following metrices: fitness for use over bins (define more in the methods section) "),
                    p("Visualized the data on a dashboard  ")
                  ),
                  boxPlus(
                    title = "Ethical Consideration",
                    closable = FALSE,
                    width = 6,
                    status = "warning",
                    solidHeader = FALSE,
                    collapsible = FALSE,
                    h3("What are the benefits of this project?"),
                    p("If we are able to draw conclusions about the quality of CoreLogic data (particularly in rural areas), other projects will have a better sense of the risks/rewards of using this data. This could contribute to a larger movement of understanding the quality underlying private data aggregation companies. If we don't do a thorough job, or otherwise bias the quality analysis, this could have ripple effects on future projects."),
                    h3("Project Assumptions"),
                    p("Assumptions: CoreLogic has undertaken their collection in good faith (both to ensure quality and privacy)
Assumptions: generalizability of focusing on a few counties (particularly affluent ones)
Assumptions: all rural areas are the same or even similar across the US (AZ vs VA, for example)
Assumptions: we can make meaningful comparisons between property-level data (i.e. CoreLogic or Fairfax) and survey aggregations (e.g. ACS)
Assumptions: deed and/or property tax data is representative of actual property information 
Assumptions: housing units are defined similarly across counties and geographical areas"), 
                    h3("Dataset choice"),
                    p("The CoreLogic dataset was provided to the team by the sponsor. In undertaking the data discovery process, to find appropriate datasets to benchmark the CoreLogic data, we considered the following: sample size, accessibility, geography, 
                      unit of analysis. This process helped ensure the datasets were appropriate and useful to the project."),
                    h3("Data quality"),
                    p("From the data profiling process we found the following about data quality:

                      The CoreLogic Fairfax data has between 7683 and 144, 037 missing values for the variables highlighted as important by USDA
                      
                      Fairfax county has between 453 and 1217 missing values
                      
                      CoreLogic New Kent data has between 802 and 13, 458 missing values
                      
                      New Kent county data has between 0 and 4634 missing values.")
                  )
                  
                )),
        tabItem(tabName = "map",
                fluidRow(
                  boxPlus(
                    title = "Interactive Map",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    enable_sidebar = TRUE,
                    sidebar_width = 25,
                    sidebar_start_open = TRUE,
                    sidebar_content = tagList(p(),
                                              actionButton("recalc", "Click Me!")),
                    leafletOutput("mymap")
                  ),
                  p("Explanatory text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                  br()
                )),
        tabItem(tabName = "graph",
                fluidRow(
                  boxPlus(
                    title = "Interactive Graph",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = NULL,
                    enable_sidebar = TRUE,
                    sidebar_width = 25,
                    sidebar_start_open = TRUE,
                    sidebar_content = sliderInput(
                      "obs",
                      "Number of observations:",
                      min = 0,
                      max = 1000,
                      value = 500
                    ),
                    plotOutput("distPlot")
                  ),
                  p("Explanatory text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                  br()
                )),
        tabItem(tabName = "both",
                fluidRow(
                  boxPlus(
                    title = "Interactive Graph",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    enable_sidebar = TRUE,
                    sidebar_width = 25,
                    sidebar_start_open = TRUE,
                    sidebar_content = tagList(sliderInput(
                      "obs2",
                      "Number of observations:",
                      min = 0,
                      max = 1000,
                      value = 500
                    )
                    ),
                    plotOutput("distPlot2"),
                    footer = "Explanatory text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."
                  ),
                  boxPlus(
                    title = "Interactive Map",
                    closable = FALSE,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    width = 6,
                    enable_sidebar = TRUE,
                    sidebar_width = 25,
                    sidebar_start_open = TRUE,
                    sidebar_content = tagList(
                      p(),
                      actionButton("recalc2", "Click Me!")
                      ),
                    leafletOutput("mymap2"),
                    footer = "Explanatory text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."
                  )
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
                    img(src = "data_sets.png", width = "450px", align = "right"),
                    h3("Data Source 1"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("Data Source 2"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("Data Source 3"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h2("Methodology"),
                    h3("Data Preparation"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("Data Modeling"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante.")
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
                    p("[Photos, information, and/or links about your sponsor go about here. You may want to use materials that your sponsors have already shared with you about their institution or coordinate with your stakeholders to include pertinent information here.]"),
                    h2("Acknowledgements"),
                    p("[Optional: You can also include external collaborators in this section or a separate section.]")
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
    
    # Create Map Points 1
    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    # Render Map 1
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(data = points())
    })
    
    # Create Map Points 2
    points2 <- eventReactive(input$recalc2, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    # Render Map 2
    output$mymap2 <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(data = points2())
    })
    
   
  }
) 