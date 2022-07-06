nonlife_company_sidebar <- list(
    
    # selectInput("selectinput", "Company", choices = select_options, selected = "All"),
    sidebarMenu( id = "tabs",
                 menuItem("Premium Collection Report", tabName = "premiumcollection", icon = icon("th"), selected = T),
                 menuItem("E-Receipt Summary Trend", tabName = "e_receipt_summary", icon = icon("bacon")),
                 menuItem("Policy + Covernote Issue Report", tabName = "policycovercount", icon = icon("th")),
                 menuItem("Policy Issue Report", tabName = "policycount", icon = icon("th")),
                 #menuItem("CoverNote Issue Report", tabName = "covercount_tab", icon = icon("th")),
                 menuItem("Analytical Report", tabName = "analyticalcomparison", icon = icon("th"))
                 #menuItem("Advanced Analysis", tabName = "nonlife_advanced_analysis", icon = icon("chart-line"))
    )
    
)






nonlife_company_body <- list(
    #style
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    titlePanel(
        h1("Non-Life Insurance BI for IDRA", align = "center", style={'background-color: #ffffff;'})
    ),
    tabItems(
        ###Premium Collection Report Sidebar Section
        tabItem( 
            tabName = "premiumcollection",
            
            fluidRow(
                valueBoxOutput("companyname_mr", width = 12),
                box(
                    status = "warning", solidHeader = TRUE, title = "Choose a date range",
                    dateRangeInput("daterange_2", "Choose a Date Range", "2021-10-01", today() ),
                    width = 12),
                column(6, uiOutput("total_prem")),
                # valueBoxOutput("total_prem", width = 6),
                
                box(
                    title = "Daywise Net Premium Collection", status = "primary", solidHeader = TRUE,
                    
                    highchartOutput("linechart_day"),
                    width = 12),
                
                box(
                    title = "Month wise Net Premium Collection", status = "primary", solidHeader = TRUE,
                    width = 6,
                    selectInput("selectyear_p", "Select a Year", choices = sort(year_choices, T), selected = 2021),
                    highchartOutput("linechart_month")
                ),
                
                box(
                    title = "Year wise Net Premium Collection", status = "primary", solidHeader = TRUE,
                    width = 6,
                    sliderTextInput(
                        inputId = "yearrange_p",
                        label = "Choose a range:", 
                        choices = sort(year_choices),
                        selected = c(2019,2022)
                    ),
                    highchartOutput("linechart_year")
                ),
                
                # box(
                #     background = "navy",
                #     width = 12,
                #     pickerInput(
                #         inputId = "linechart_company_picker",
                #         label = "Companies", 
                #         choices = company_picker,
                #         selected = company_picker[1:3],
                #         multiple = TRUE
                #     ),
                #     actionButton(
                #         inputId = "compare_button",
                #         label = "Compare", 
                #         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                #     )
                # ),
                # 
                # box(
                #     title = "Day wise Net Premium Collection Comparison", status = "primary", solidHeader = TRUE,
                #     dateRangeInput("daterange_p_comparison", "Choose a date range","2020-03-08", today () ),
                #     plotlyOutput("linechart_p_comparison"),
                #     width = 12),
                # 
                # box(
                #     title = "Month wise Net Premium Collection Comparison", status = "primary", solidHeader = TRUE,
                #     width = 12,
                #     selectInput("selectyear_premium_comparison", "Select a Year", choices = sort(unique(mr$year_r)), selected = 2020),
                #     plotlyOutput("monthlychart_premium_comparison")
                # ),
                
                box(
                    title = "Branchwise Performance", status = "primary", solidHeader = TRUE,
                    dateRangeInput("daterange_branch_dt", "Choose a date range","2021-10-01",today()),
                    DTOutput("branchdt"),
                    width = 12)
            )),
        
        #E-Receipt Summary
        tabItem(
            tabName = "e_receipt_summary",
            # h2("E-Receipt Summary", align='center'),
            
            fluidRow(
                valueBoxOutput("companyname_e_receipt", width = 12),
                box(
                    status = "warning", solidHeader = TRUE, title = "Choose a date range",
                    dateRangeInput("daterange_e_receipt", "Choose a Date Range","2021-10-01",today() ),
                    width = 12),
                column(6, uiOutput("total_e_receipt_issued")),
                # valueBoxOutput("total_e_receipt_issued", width = 6),
                # valueBoxOutput("fpr_e_receipt_issued", width = 3),
                # valueBoxOutput("deferred_e_receipt_issued", width = 3),
                # valueBoxOutput("renewal_e_receipt_issued", width = 3),
                
                box(
                    title = "Total E-Receipt Issued Per Day", status = "primary", solidHeader = TRUE, background = "aqua",
                    
                    highchartOutput("linechart_e_receipt"),
                    width = 12),
                
                box(
                    title = "Total E-Receipt Issued Per Month", status = "primary", solidHeader = TRUE,
                    width = 6,
                    selectInput("selectyear_e_receipt", "Select a Year", choices = c(2019:2022), selected = 2021),   #choices = sort(unique(or$year_r), T)
                    highchartOutput("monthlychart_e_receipt")
                ),
                
                box(
                    title = "Total E-Receipt Issued Per Year", status = "primary", solidHeader = TRUE,
                    width = 6,
                    highchartOutput("yearlychart_e_receipt")
                )
                
                # box(
                #     title = "Agent Performance", status = "primary", solidHeader = TRUE,
                #     dateRangeInput("daterange_agent_dt", "Choose a date range","2021-01-01",today()),
                #     DTOutput("agentdt_e_receipt"),
                #     width = 12)
                # box(
                #     title = "Branchwise Collection Comparison Report", status = "success", solidHeader = TRUE,
                #     DTOutput("branchdt"),
                #     width = 12),
                
                
                
            )
        ),
        
        ###Policy Count Report Sidebar Section
        tabItem(
            tabName = "policycount",
            
            fluidRow(
                
                valueBoxOutput("companyname_cp", width = 12),
                
                box(
                  status = "warning", solidHeader = TRUE, title = "Choose a date range (Policy issue date)",
                  dateRangeInput("daterange_1.2", "Choose a Date Range", "2021-10-01", today() ),
                  width = 12
                ),
                
                
                column(4, uiOutput("total_policy_count")),
                column(4, uiOutput("total_expired_policy_count")),
                column(4, uiOutput("total_cancelled_policy_count")),
                # valueBoxOutput("total_policy_count", width = 4),
                # # valueBoxOutput("total_active_policy_count", width = 3),
                # valueBoxOutput("total_expired_policy_count", width = 4),
                # valueBoxOutput("total_cancelled_policy_count", width = 4),
                box(
                    title = "Daily Issued Total Poilcy Count", status = "primary", solidHeader = TRUE,
                    
                    highchartOutput("linechart_policycount"),
                    width = 12),
                box(
                    title = "Month wise Policy Issued in Total", status = "primary", solidHeader = TRUE,
                    width = 6,
                    selectInput("selectyear_1.5", "Select a Year", choices = sort(year_choices, T), selected = 2021),
                    highchartOutput("barchart_monthly_policycount")
                ),
                box(
                    title = "Year wise Policy Issued in Total", status = "primary", solidHeader = TRUE,
                    width = 6,
                    sliderTextInput(
                        inputId = "yearrange_1.6",
                        label = "Choose a range:", 
                        choices = sort(year_choices),
                        selected = c(2019,2022)
                    ),
                    highchartOutput("barchart_yearly_policycount")
                ),
                
                box(
                    title = "Agentwise Policy Performance", status = "primary", solidHeader = TRUE,
                    DTOutput("agentdt_policy"),
                    width = 12
                )
                
            )),
        
        # tabItem(
        #   tabName = "covercount_tab",
        #   fluidRow(
        #       valueBoxOutput("companyname_cover_count", width = 12),
        #       box(
        #         status = "warning", solidHeader = TRUE, title = "Choose a date range (Covernote issue date)",
        #         dateRangeInput("daterange_1.1", "","2021-01-01",today()),
        #         width = 12
        #       ),
        #       
        #       valueBoxOutput("total_cover_count", width = 6),
        #       # valueBoxOutput("total_active_cover_count", width = 3),
        #       # valueBoxOutput("total_expired_cover_count", width = 3),
        #       valueBoxOutput("total_cancelled_cover_count", width = 6),
        #       
        #     box(
        #       title = "Daily Issued Total Covernote Count", status = "primary", solidHeader = TRUE,
        #       
        #       highchartOutput("linechart_day_1.1"),
        #       width = 12),
        #     box(
        #       title = "Month wise Covernote Issued in Total", status = "primary", solidHeader = TRUE,
        #       width = 6,
        #       selectInput("selectyear_1.2", "Select a Year", choices = sort(year_choices, T), selected = 2021),
        #       highchartOutput("linechart_month_1.2")
        #     ),
        #     box(
        #       title = "Year wise Covernote Issued in Total", status = "primary", solidHeader = TRUE,
        #       width = 6,
        #       sliderTextInput(
        #         inputId = "yearrange_1.3",
        #         label = "Choose a range:", 
        #         choices = sort(year_choices),
        #         selected = c(2019,2021)
        #       ),
        #       highchartOutput("linechart_year_1.3")
        #     ),
        #     
        #     box(
        #       title = "Agentwise Cover Policy Performance", status = "primary", solidHeader = TRUE,
        #       DTOutput("agentdt_note"),
        #       width = 12
        #     )
        #   )
        # ),
        
        
        
        
        ###Analytical Report Sidebar Section
        tabItem(
            tabName = "analyticalcomparison",
            
            fluidRow(
                valueBoxOutput("companyname_nonlife", width = 12),
                valueBoxOutput("bank_deposited_policy", width = 4),
                valueBoxOutput("valid_mobile_number", width = 4),
                valueBoxOutput("valid_email_address", width = 4),
                valueBoxOutput("policydate_vs_number_value", width = 4),
                valueBoxOutput("covernote_present_policy_absent", width = 4),
                valueBoxOutput("covernote_policy_both_present", width = 4),
                
                box(
                    title = "Non-life Insurance Type Chart", status = "primary", solidHeader = TRUE,
                    # dateRangeInput("daterange_insurancetype", "Choose a date range","2021-01-01",today()),
                    highchartOutput("insurancetypepiechart"),  ##its a bar chart now
                    width = 12),
                box(
                  title = "Non-life Insurance Type Data Table", status = "primary", solidHeader = TRUE,
                  dateRangeInput("daterange_insurancetype_dt", "Choose a date range","2021-10-01",today()),
                  DTOutput("insurancetype_dt"),
                  width = 12),
                
                box(
                    dateRangeInput("daterange_3.2", "Choose a date range","2021-10-01",today() ),
                    width = 12),
                
                box(
                    title = "Co-insurance Status", status = "primary", solidHeader = TRUE,
                    plotlyOutput("piechart_3.1"),
                    width = 12)
                # ,
                # 
                # box(
                #     title = "SMS Sending Status", status = "primary", solidHeader = TRUE,
                #     plotlyOutput("piechart_3.2"),
                #     width = 12)
            )),
        
        
        tabItem(
            tabName = "policycovercount",
            valueBoxOutput("info_policycovercount", width = 12),
            box(
                valueBoxOutput("policy_cover_active", width = 6),
                valueBoxOutput("policy_cover_total", width = 6),
                width = 12
            )
            
        )
        
        # tabItem(
        #     tabName = "nonlife_advanced_analysis",
        #     
        #     tabsetPanel(
        #         tabPanel("Trend",
        #                  column(12,
        #                         selectInput("nonlife_trend_type_select", "Trend: ", 
        #                                     c("Policy", "Premium"))
        #                  ),
        #                  column(12,
        #                         plotlyOutput("nonlife_trend_plot",height = "600px")
        #                  )
        #         ),
        #         tabPanel("Forecast",
        #                  fluidRow(
        #                      
        #                      column(width = 6, selectInput("nonlife_select_period", "Period to forecast (Months)", c(6,12,18), 12)),
        #                      column(width = 6, selectInput("nonlife_forecast_product_type_select", "Forecast: ", 
        #                                                    c("Policy", "Premium")),
        #                             actionButton("nonlife_forecastButton","Run Forecast", class = "btn-warning")
        #                      ),
        #                      column(12, hr(style = "border-color: #cbcbcb; margin-top: 2px; margin-bottom: 2px;")),
        #                      
        #                      column(12,
        #                             plotlyOutput("nonlife_forecast_plot",height = "600px") %>% shinycssloaders::withSpinner()
        #                      )
        #                  )
        #         )
        #     )
        #     
        # )
        
        
    )
)





