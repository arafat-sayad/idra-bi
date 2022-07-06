life_test_title="Type of Reports"

life_company_side=list(
      sidebarMenu(id = "tabs",
        menuItem("Premium Summary", tabName = "premiumsummary", icon = icon("balance-scale-left"), selected = T),
        menuItem("E-Receipt Summary Trend", tabName = "e_receipt_summary", icon = icon("bacon")),
        menuItem("Policy Issue Trend", tabName = "homepage", icon = icon("bacon")),
        menuItem("Policy Status", tabName = "policysummary", icon = icon("calendar-check")),
        menuItem("Performance Comparison Report", tabName= "performance_comparison", icon = icon("table")),
        menuItem("Policyholders Summary Info", tabName = "policyholders_summary", icon = icon("balance-scale"))
        #menuItem("Advanced Analysis", tabName = "advanced_analysis", icon = icon("chart-line"))
        #menuItem("Map", tabName = "policy_map", icon = icon("map-marked"))
      ))

life_company_main=list(
  #style
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styleLife.css")
    ),
  titlePanel(
    # h1("Life Insurance BI", align = "center"),
    h1(textOutput('companyname_title_text'), align='center')
  ),
  tabItems(
    
    #premium summary dashboard
    tabItem( 
      tabName = "premiumsummary",
      h2("Premium Summary Report"),
      fluidRow(
        valueBoxOutput("companyname_p", width = 12),
        box(
          dateRangeInput("daterange_p", "Choose a date range","2021-10-01",today() ),
          width = 12),
        
        column(3, uiOutput("total_prem")),
        column(3, uiOutput("fpr_prem")),
        column(3, uiOutput("deferred_prem")),
        column(3, uiOutput("renewal_prem")),
        # valueBoxOutput("fpr_prem", width = 3),
        # valueBoxOutput("deferred_prem", width = 3),
        # valueBoxOutput("renewal_prem", width = 3),
        
        box(
          title = "Total Premium Collection Per Day", solidHeader = TRUE, background = "black",
          
          highchartOutput("linechart_p"),
          width = 12),
        
        box(
          title = "Total Premium Collection Per Month", status = "primary", solidHeader = TRUE, background = "teal",
          width = 6,
          selectInput("selectyear_p", "Select a Year", choices = life_or_year_choices, selected = 2021),
          highchartOutput("monthlychart_p")
        ),
        box(
          title = "Total Premium Collection Per Year", status = "primary", solidHeader = TRUE, background = "teal",
          width = 6,
          highchartOutput("yearlychart_p")
        )
        
        
      )),
    
    #E-Receipt Summary
    tabItem(
      tabName = "e_receipt_summary",
      h2("E-Receipt Summary"),
      fluidRow(
        valueBoxOutput("companyname_r", width = 12),
        box(
          dateRangeInput("daterange_r", "Choose a date range","2021-10-01",today() ),
          width = 12),
        
        
        column(3, uiOutput("total_e_receipt_issued")),
        column(3, uiOutput("fpr_e_receipt_issued")), 
        column(3, uiOutput("deferred_e_receipt_issued")), 
        column(3, uiOutput("renewal_e_receipt_issued")), 
        
        # valueBoxOutput("total_e_receipt_issued", width = 3),
        # valueBoxOutput("fpr_e_receipt_issued", width = 3),
        # valueBoxOutput("deferred_e_receipt_issued", width = 3),
        # valueBoxOutput("renewal_e_receipt_issued", width = 3),
        
        box(
          title = "Total E-Receipt Issued Per Day", status = "primary", solidHeader = TRUE, background = "black",
          
          highchartOutput("linechart_r"),
          width = 12),
        
        box(
          title = "Total E-Receipt Issued Per Month", status = "primary", solidHeader = TRUE,  background = "teal",
          width = 6,
          selectInput("selectyear_r", "Select a Year", choices = life_or_year_choices, selected = 2021),
          highchartOutput("monthlychart_r")
        ),
        box(
          title = "Total E-Receipt Issued Per Year", status = "primary", solidHeader = TRUE,
          width = 6,  
          highchartOutput("yearlychart_r")
        )
        
        
        
      )
    ),
    
    tabItem(
      tabName = "homepage",
      fluidRow(
        box(
          
          dateRangeInput("policydaterange","Choose a date range", start = "2021-10-01",end = today () ),
          # valueBoxOutput("totalpolicyvalue", width = 12),
          column(12, uiOutput("totalpolicyvalue")),
          width = 12),
        
        box(
          title = "Total Policies Issued Per Day", status = "primary", solidHeader = TRUE, background = "black",
          
          dateRangeInput("daterange", "Choose a date range","2021-10-01", today () ),
          highchartOutput("linechart"),
          width = 12),
        
        box(
          title = "Total Policies Issued Per Month", status = "primary", solidHeader = TRUE, background = "light-blue",
          width = 6,
          selectInput("selectyear", "Select a Year", choices = life_policy_year_choices, selected = 2021),
          highchartOutput("monthlychart")
        ),
        
        box(
          title = "Total Policies Issued Per Year", status = "success", solidHeader = TRUE,
          width = 6,
          sliderTextInput(
            inputId = "yearrange",
            label = "Choose a range:", 
            choices = life_policy_year_choices,
            selected = c(2015,2022)
          ),
          highchartOutput("yearlychart")
        )
        
        #   ,
        #   box(
        #     title = "No Match Between Policy Start Date & Risk Start Date Status", status = "primary", solidHeader = TRUE,
        #     plotlyOutput("nomatchpiechart"),
        #     width = 12
        # )
        
      )),
    
    tabItem(
      tabName = "performance_comparison",
      fluidRow(
        box(
          title = "Agentwise Policy Issue Comparison Report", status = "primary", solidHeader = TRUE,
          DTOutput("agent_policy_dt"),
          width = 12),
        
        box(
          title = "Branchwise Collection Comparison Report", status = "success", solidHeader = TRUE,
          DTOutput("branchdt"),
          width = 12),
        
        box(
          title = "Agentwise Collection Comparison Report", status = "primary", solidHeader = TRUE,
          DTOutput("agentdt"),
          width = 12)
      )),
    
    #Policy Holders Socio-Demographic Information
    tabItem(
      tabName = "policyholders_summary",
      fluidRow(
        
        box(
          
          title = "Policyholder's Age Distribution", status = "primary", solidHeader = TRUE, background = "aqua",
          # selectInput("selectgender", "Select Gender", choices = c("Overall", "Male", "Female", "Other"), selected = "Overall"),
          highchartOutput("age_dist"),
          width = 12),
        
        box(
          title = "Distribution of Gender", status = "primary", solidHeader = TRUE, background = "olive",
          dateRangeInput("daterange_gender", "Choose a date range", "2021-10-01", today()),
          plotlyOutput("genderchart"),
          width = 12)
      )),
    
    tabItem(
      tabName = "policysummary",
      h2("Policy Status At a Glance"),
      
      
      #number boxes in policy summary tab
      fluidRow(
        valueBoxOutput("companyname", width = 12),
        column(3, uiOutput("totalinforce")),
        column(3, uiOutput("totallapse")),
        column(3, uiOutput("totalpaidup")),
        column(3, uiOutput("totalpolicy")),
        column(4, uiOutput("totalsurrender")),
        column(4, uiOutput("totaldeath")),
        column(4, uiOutput("totalmatured")),
        
        # valueBoxOutput("totalinforce", width = 3),
        # valueBoxOutput("totallapse", width = 3),
        # valueBoxOutput("totalpaidup", width = 3),
        # valueBoxOutput("totalpolicy", width = 3),
        # valueBoxOutput("totalsurrender", width = 4),
        # valueBoxOutput("totaldeath", width = 4),
        # valueBoxOutput("totalmatured", width = 4),
        
        
        box(
          title = "Total Policy by Term", status = "primary", solidHeader = TRUE,
          textOutput("ploicyterm_companyname"),
          plotlyOutput("policytermplot"),
          width = 12
        )
      )
    )
    
    
    # tabItem(
    #   tabName = "advanced_analysis",
    #   
    #   tabsetPanel(
    #     tabPanel("Trend",
    #              column(12,
    #                     selectInput("trend_type_select", "Trend: ", 
    #                                 c("Policy", "Premium"))
    #              ),
    #              column(12,
    #                     plotlyOutput("trend_plot",height = "600px")
    #              )
    #     ),
    #     tabPanel("Forecast",
    #              fluidRow(
    #                
    #                column(width = 6, selectInput("select_period", "Period to forecast (Months)", c(6,12,18), 12)),
    #                column(width = 6, selectInput("forecast_product_type_select", "Forecast: ", 
    #                                              c("Policy", "Premium")),
    #                       actionButton("forecastButton","Run Forecast", class = "btn-warning")
    #                ),
    #                column(12, hr(style = "border-color: #cbcbcb; margin-top: 2px; margin-bottom: 2px;")),
    #                
    #                column(12,
    #                       plotlyOutput("forecast_plot",height = "600px") %>% shinycssloaders::withSpinner()
    #                )
    #              )
    #     )
    #   )
    #   
    # )
    
  ))