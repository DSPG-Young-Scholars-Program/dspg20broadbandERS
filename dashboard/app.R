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
          tabName = "profiling",
          text = "Data Profiling",
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
                    p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the Biocomplexity Institute’s Social and Decision Analytics division (SDAD). In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy."),
                    p("This DSPG project is a partnership with the USDA Economic Research Service. The project team worked with data from CoreLogic, a commercial data aggregator, to evaluate the quality of their national property data, particularly for rural areas."),
                    h2("Project Goals"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex."),
                    h2("Our Approach"),
                    p("We took a two-pronged approach to evaluating data quality:"),
                    p("1. Since we did not have property-level data at a national level, we selected two counties in Virginia (Fairfax County and New Kent County) for which we did have property-level data. These counties served as case studies as we geocoded the address information we had for each property and made direct comparisons between the values given by the counties and their equivalent values in the CoreLogic data."),
                    p("2. In order to evaluate data quality on a national scale, we used American Community Survey (ACS) estimates for housing variables at the census tract level. We compared the CoreLogic estimates for each census tract to the ACS estimates using a fitness-for-use metric established in a previous partnership with the Census Burea."),
                    p("The exact methods of comparison are described in more detail in the Methodology tab."),
                    h2("Ethical Considerations"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Fusee justo nisi, suscipit a lacus et, posuere sagittis ex.")
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
        tabItem(tabName = "profiling",
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
                    solidHeader = TRUE,
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
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    p("Greater range of missing values in the CoreLogic dataset"),
                    img(src = "Missing_nk.png", width = "360px", align = "left"),
                    img(src = "Missing_cl_nk.png", width = "360px", align = "right"),
                    img(src = "nk_hist.png", width = "360px", align = "left"))
         
                )),
        tabItem(tabName = "findings",
                fluidRow(
                  boxPlus(
                    title = "Findings",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h2("Summary of Findings"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("Results Section One"),
                    img(src = "irrational_venn_diagram.png", width = "360px", align = "right"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("Results Section Two"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante."),
                    h3("Results Section Three"),
                    img(src = "food_reality_chart.png", width = "400px", align = "right"),
                    p("Example text: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aliquam in varius purus. Nullam ut sodales ante.")
                  )
                )),
        tabItem(tabName = "team",
                fluidRow(
                  boxPlus(
                    title = "Meet the Team",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h2("DSPG Team Members"),
                    h3("Morgan"),
                    img(src = "morgan.jfif",  width = "200px"),
                    h3("Vatsala "),
                    img(src = "Vatsala_Ramanan.jpg",  width = "200px"),
                    h3("Madeline"),
                    img(src = "maddie.jfif",  width = "200px"),
                    h2("UVA SDAD Team Members"),
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