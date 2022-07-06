library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(tidyverse)
library(plotly)
library(shinyWidgets)
library(stringr)
library(tidyr)
library(DT)
library(shinymanager)
library(gotop)
library(highcharter)
library(glue)

####forecasting packages 
##library(tidyverse)
#library(tidymodels)
#library(modeltime)
# remotes::install_github("business-science/timetk")
#library(timetk)
# install.packages("modeltime.ensemble")
#library(modeltime.ensemble)
#library(lubridate)



#### cusom box with change
render_my_box_with_change <- function(value = "value", subtitle = "subtitle",changeText ="percent change", 
                                      bgcolor = "#023047", icon = NULL, textcolor = "white", changeTextColor = "white") {
    # changeTextColor <- ifelse(changeText > 0,"green", "red")
    tagList(
        
        div(class = paste0("small-box bg-", "custom"), style = glue("background-color: {bgcolor}!important; color: {textcolor};
                     height: 130px!important; fontweight : bold"), 
            div(class = "inner", h4(value), p(subtitle), 
                p(changeText, style = glue("color:{changeTextColor}; text-align: center;"))), if (!is.null(icon))
                    div(class = "icon-large", icon, style="font-size: 44px; color: #fff"))
    )
}



### color definitions 
COL_RED <- "#d62828"
COL_ORANGE <- "#f77f00"
COL_BLUE <- "#006d77"  
COL_PURPLE <- "#03045e"
COL_GREEN <- "#588157"
COL_DARK = "#023047"

## tidymodel packages messses up. fix:

observe <- shiny::observe
filter <- dplyr::filter



addResourcePath(prefix = "logo", directoryPath = "logo")
compress <- function(tx) {
    div <- findInterval(as.numeric(gsub("\\,", "", tx)),
                        c(0, 1e3, 1e6, 1e9, 1e12) )  # modify this if negative numbers are possible
    paste0(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2),
           c("","K","M","B","T")[div] )}


######### Non life items

load("data/nonlife_summarised_data.RData")
non_life_data_sum_table <- readRDS("data/non_life_data_sum_table.RDS")

source("nonlife_company_user_UI.R", local = TRUE)
source("nonlife_admin_UI.R", local = TRUE)



#### life items (needs more optimization like nonlife)


load("data/life_summarised_data.RData")

source("life_admin_UI.R", local = TRUE)
source("life_company_user_UI.R", local = TRUE)


#### life forecast data ##
# load("../data/life_policy_forecast.RData")
# load("../data/life_prem_forecast.RData")
# 
# ## nonlife forecast data
# load("../data/nonlife_prem_forecast.RData")
# load("../data/nonlife_policy_forecast.RData")

#################-----UI----#####################

set_labels(
    language = "en",
    "Please authenticate" = "Log In Information",
    "Username:" = "Username",
    "Password:" = "Password"
)



ui <- secure_app(
    tags_top =
        tags$div(
            tags$img(
                src = "logo/idra_logo.png", width = 150
            ),
            tags$h5("For your Service", style = "align:center")

        ),
    tags_bottom = tags$div(
        tags$p(
            "For any question, please contact ",
            tags$a(
                href = "http://www.idra.org.bd/",
                target="_top", "Insurance Development & Regulatory Authority"

            )
        )
    ),
    dashboardPage(
        skin = "green",
        dashboardHeader(title = "Type of Reports"),
        dashboardSidebar(uiOutput("side")),
        dashboardBody(
            uiOutput("page"),

            #https://www.quantargo.com/help/r/latest/packages/gotop/0.1.2/use_gotop
            use_gotop(
                src = "fas fa-chevron-circle-up", # css class from Font Awesome
                place="right",
                color = "tomato", # color
                opacity = 0.8, # transparency
                width = 40, # size
                appear = 50, # number of pixels before appearance
                marginY = 15,
                marginX = 1
            )
            )
    ),
     enable_admin = TRUE)

###############################------------------Server-----------------------#############################

server <- function(input, output, session) {


    res_auth <- secure_server(
        check_credentials = check_credentials(
            "ump_bi_credential.sqlite",
            # passphrase = key_get("R-shinymanager-key", "obiwankenobi")
            passphrase = "passphrase_wihtout_keyring"
        ),
        keep_token = T
    )
    loggedinas <- reactive({
        res_auth$user
    })


    loggedincompany <- reactive({
        # userslist$company[userslist$user == res_auth$user]
        res_auth$company
    })

    loggedin_usertype <- reactive({
        # userslist$role[userslist$user == res_auth$user]
        res_auth$role
    })




#### Life comapny filter data.. needs optimization in summarisation part life nonlife
    # ----------------------------------
    # company_policy_data <- reactive({
    #     policy %>% filter(company_name == loggedincompany())
    # })
    #
    linechart_policy_data <- reactive({
        linechart_policy %>% filter(company_name == loggedincompany())
    })

    # company_or_data <- reactive({
    #     or %>% filter(company_name == loggedincompany())
    # })

    agent_policy_dt_data <- reactive({
        agent_policy_dt %>% filter(company_name == loggedincompany())
    })

    branch_dt_data <- reactive({
        branch_dt %>% filter(company_name == loggedincompany())
    })

    agent_dt_data <- reactive({
        agent_dt %>% filter(company_name == loggedincompany())
    })
# ----------------------------------



### role based server

    observe({
        req(loggedin_usertype())

        if(loggedin_usertype() == "usernonlife"){  ## start pf nonlife company user server


            output$side <- renderUI({
                nonlife_company_sidebar
            })
            output$page <- renderUI({
                nonlife_company_body
            })

            #select a menu item as it doesnt select any at first load for some reasons
            updateTabItems(session, "tabs", "premiumcollection")


            source("non_life_company_user_server.R", local = TRUE)



        } #end of server (company version)


        if(loggedin_usertype() == "adminnonlife"){  ##nonlife admin server start
            output$side <- renderUI({
                nonlife_admin_sidebar
            })
            output$page <- renderUI({
                nonlife_admin_body
            })

            #select a menu item as it doesnt select any at first load for some reasons
            updateTabItems(session, "tabs", "premiumcollection")


            source("non_life_admin_server.R", local = TRUE)




        }#end of admin version server


        if(loggedin_usertype() == "userlife"){    ##life company user server code starts


            output$side <- renderUI({
                life_company_side
            })
            output$page <- renderUI({
                life_company_main
            })

            #select a menu item as it doesn't select any at first load for some reasons
            updateTabItems(session, "tabs", "premiumsummary")


            source("life_company_user_server.R", local = TRUE)


        }    ###end of life company user server codes


        if(loggedin_usertype() == "adminlife"){  ##life admin server
            output$side <- renderUI({
                life_admin_side
            })
            output$page <- renderUI({
                life_admin_main
            })

            #select a menu item as it doesnt select any at first load for some reasons
            updateTabItems(session, "tabs", "premiumsummary")

            source("life_admin_server.R", local = TRUE)





        }  ###end of life admin server codes





    })


    ############################------




}


shinyApp(ui, server)
