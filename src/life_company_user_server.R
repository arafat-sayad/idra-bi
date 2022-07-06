output$companyname_title_text <- renderText({
    paste("Life Insurance BI:",loggedincompany())
})

# output$companyname_title_text <- renderText({
#     paste("Life Insurance BI:", "XYZ")
# })





#Policyholders Age Distribution
output$age_dist <- renderHighchart({
    age_group_hc <- age_group_plotly %>%
                     filter(company_name == loggedincompany())
    hchart(age_group_hc,
           'column', hcaes(x = Age_Group, y = Frequecy, color = Age_Group)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = TRUE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    
    # plot_ly(age_group_plotly %>%
    #             filter(company_name == loggedincompany()),
    #         x = ~Age_Group, y = ~Frequecy, type = "bar", hoverinfo ="y") %>%
    #     
    #     layout(bargap = 0,
    #            annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
    #                             yref="paper",y=0.9,showarrow=FALSE
    #            ),
    #            xaxis = list(title = "Age Group"),
    #            yaxis = list(title = "Total Policy Issued"),
    #            title = "Policyholder's Age Distrubution")
    
    
    
    
}) 
#%>% bindCache(input$selectge   #whether or not to add filter(company_name == loggedincompany())


#total per day line chart

# totalpolicyperday_reactive <- reactive({
#   policy %>%
#     filter(policystartdate >= input$daterange[1], policystartdate < input$daterange[2]) %>%
#     group_by(policystartdate) %>% 
#     summarise(total = n())
# })

output$linechart <- renderHighchart({
    
    # data <- company_policy_data() %>%
    #     filter(policystartdate >= input$daterange[1], policystartdate < input$daterange[2], gender %in% c("Female", "Male", "Other")) %>%
    #     group_by(policystartdate) %>% 
    #     summarise(total = n())
    
    data <- linechart_policy %>%
        filter(company_name == loggedincompany(), policystartdate %in% seq(input$daterange[1],
                                                                           input$daterange[2], by = 'day')) %>% 
        group_by(policystartdate) %>% summarise(total = sum(total))
    
    
    hchart(data,
           'line', hcaes(x = policystartdate, y = total, color = total)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = TRUE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    # plot_ly(data, x = ~policystartdate, y = ~total, mode = "line", hoverinfo ="text", text = ~paste0("Date: ", policystartdate, " (",weekdays(as.Date(policystartdate)),")", "<br>", "Policy Issued: ", data$total)) %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Date"),
    #         yaxis = list(title = "Total Policy Issued"),
    #         title = "Policyholder's Age Distrubution"
    #     )
})

#total per month bar chart

output$monthlychart <- renderHighchart({
    
    
    
    # total_by_month <- company_policy_data() %>% 
    #     filter(year == input$selectyear) %>% group_by(month) %>% summarise(total = n())
    
    total_by_month <- monthlychart_data %>% 
        filter(company_name == loggedincompany(), year == input$selectyear) %>%
        group_by(month) %>%
        summarise(total = sum(total, na.rm = T))
    
    hchart(total_by_month,
           'column', hcaes(x = month, y = total, color = month)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = FALSE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    
    # plot_ly(total_by_month, x = ~month, y = ~total, hoverinfo ="y") %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Year: ", input$selectyear, "<br>", "Company:", loggedincompany()), xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Month"),
    #         yaxis = list(title = "Total Policies Issued")
    #     )
})

#total per year bar chart

output$yearlychart <- renderHighchart({
    
    # total_by_year <- totalpolicyperyear_reactive()
    
    
    total_by_year <- monthlychart_data %>% 
        filter(company_name == loggedincompany()) %>% 
        group_by(year) %>%
        summarise(total = sum(total, na.rm = T))
    
    hchart(total_by_year,
           'column', hcaes(x = year, y = total, color = year)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = FALSE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    # plot_ly(total_by_year %>% filter(year <= input$yearrange[2], year >= input$yearrange[1]), x = ~factor(year), y = ~total, type = "bar", hoverinfo ="y") %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Year"),
    #         yaxis = list(title = "Total Policy Issued")
    #     )
    
})





#value boxes appearing in policy summary (status)
output$companyname <- renderValueBox({
    
    name <- loggedincompany()
    #name <- "XYZ"
    
    valueBox(
        name,
        "Showing Results for", color = "navy"
    )
})


# total policy in date range valuebox
# totalpolicyvalue_reactive <- reactive({
#   nrow(policy %>%
#          filter(policystartdate >= input$policydaterange[1], policystartdate < input$policydaterange[2]))
# })
# totalpolicyvalue_filter_reactive <- reactive({
#     sum((company_policy_data() %>%
#              filter(policystartdate >= input$policydaterange[1], policystartdate < input$policydaterange[2]))$company_name == loggedincompany())
# })


# output$totalpolicyvalue <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   totalpolicy <- totalpolicyvalue_reactive()
#     # }
#     
#     totalpolicy <- linechart_policy %>% 
#         filter(company_name == loggedincompany()) %>%
#     filter(policystartdate %in% seq(input$policydaterange[1],
#                                     input$policydaterange[2], by = 'day')) %>%
#         pull(total) %>% sum()
#     
#     
#     valueBox(
#         totalpolicy,
#         paste(compress(totalpolicy),"Total Policy Issued between", input$policydaterange[1], "and", input$policydaterange[2]-1),color = "purple",
#         icon = tags$i(class = "fa fa-tags", style="font-size: 44px; color: #fff")
#     )
# })


output$totalpolicyvalue <- renderUI({
    
    totalpolicy <- linechart_policy %>% 
        filter(company_name == loggedincompany()) %>%
        filter(policystartdate %in% seq(input$policydaterange[1],
                                        input$policydaterange[2], by = 'day')) %>%
        pull(total) %>% sum()
    
    sub <- paste(compress(totalpolicy),"Total Policy Issued between", input$policydaterange[1], "and", input$policydaterange[2]-1)
    
    market_share <- scales::percent(totalpolicy/ (linechart_policy %>%
                                                      filter(policystartdate %in% seq(input$policydaterange[1],
                                                                                      input$policydaterange[2], by = 'day')) %>%
                                                      pull(total) %>% sum()), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(totalpolicy, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("tags"), bgcolor = COL_PURPLE, textcolor = "#fff")
})






# total inforce value box
# totalinforce_reactive <- reactive({
#   length(unique((policy %>% filter(policyenddate > today(),
#                                                 is.na(nextpremiumduedate) | nextpremiumduedate >= today() - days(30),
#                                                 (status == "" | is.na(status))))$uniqueid))     # note: null in database is saved as empty character in R
# })

totalinforce_filter_reactive <- reactive({
    policy_status$inforce[policy_status$company_name == loggedincompany()]
})

# output$totalinforce <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_inforce <- totalinforce_reactive()
#     # }
#     
#     total_inforce <- totalinforce_filter_reactive()
#     
#     
#     valueBox(
#         total_inforce,
#         paste(compress(total_inforce), "Total Inforce Policy") ,color = "green"
#     )
# })



output$totalinforce <- renderUI({
    
    total_inforce <- totalinforce_filter_reactive()
    
    sub <- paste(compress(total_inforce), "Total Inforce Policy")
    
    market_share <- scales::percent(total_inforce/ (sum(policy_status$inforce)), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_inforce, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("book"), bgcolor = COL_GREEN, textcolor = "#fff")
})



# total forfeit value box
# totalforfeited_reactive <- reactive({
#   length(unique((policy %>% filter(policyenddate > today(),
#                                                 nextpremiumduedate < today() - days(30),  #difference should be 30 days
#                                                 (nextpremiumduedate - policystartdate)<730,
#                                                 (status == "" | is.na(status))))$uniqueid))
# })

totallapse_filter_reactive <- reactive({
    policy_status$lapse[policy_status$company_name == loggedincompany()]
})

# output$totallapse <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_lapse <- totalforfeited_reactive()
#     # }
#     
#     total_lapse <- totallapse_filter_reactive()
#     
#     
#     
#     valueBox(
#         total_lapse,
#         paste(compress(total_lapse), "Total lapse Policy"),color = "orange",
#         icon = tags$i(class = "fa fa-cut", style="font-size: 44px; color: #fff")
#     )
# })


output$totallapse <- renderUI({
    
    total_lapse <- totallapse_filter_reactive()
    
    sub <- paste(compress(total_lapse), "Total lapse Policy")
    
    market_share <- scales::percent(total_lapse/ (sum(policy_status$lapse)), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_lapse, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("scissors"), bgcolor = COL_ORANGE, textcolor = "#fff")
})





totalpaidup_filter_reactive <- reactive({
    policy_status$paidup[policy_status$company_name == loggedincompany()]
})

# output$totalpaidup <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   
#     #   total_paidup <- totalpaidup_reactive()
#     # }
#     
#     total_paidup <- totalpaidup_filter_reactive()
#     
#     
#     
#     valueBox(
#         total_paidup,
#         paste(compress(total_paidup), "Total Paid-up Policy"),color = "light-blue",
#         icon = tags$i(class = "fa fa-power-off", style="font-size: 44px; color: #fff")
#     )
# })



output$totalpaidup <- renderUI({
    
    total_paidup <- totalpaidup_filter_reactive()
    
    sub <- paste(compress(total_paidup), "Total Paid-up Policy")
    
    market_share <- scales::percent(total_paidup/ (sum(policy_status$paidup)), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_paidup, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("power-off"), bgcolor = COL_BLUE, textcolor = "#fff")
})

#totalpolicy

totalpolicy_filter_reactive <- reactive({
    policy_status$total[policy_status$company_name == loggedincompany()]
})

# output$totalpolicy <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   
#     #   total_paidup <- totalpaidup_reactive()
#     # }
#     
#     total_policy <- totalpolicy_filter_reactive()
#     
#     
#     
#     valueBox(
#         total_policy,
#         paste(compress(total_policy), "Total Policy"),color = "green",
#         icon = tags$i(class = "fa fa-archive", style="font-size: 44px; color: #fff")
#     )
# })


output$totalpolicy <- renderUI({
    
    total_policy <- totalpolicy_filter_reactive()
    
    sub <- paste(compress(total_policy), "Total Policy")
    
    market_share <- scales::percent(total_policy/ (sum(policy_status$total)), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_policy, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("archive"), bgcolor = COL_GREEN, textcolor = "#fff")
})



# total surrender value box
# totalsurrender_reactive <- reactive({
#   sum(str_detect((policy)$status, regex("^s", ignore_case = T)), na.rm = T)
# })
totalsurrender_filter_reactive <- reactive({
    # sum(str_detect((company_policy_data())$status, regex("^s", ignore_case = T)), na.rm = T)
    policy_status$surrender[policy_status$company_name == loggedincompany()]
})

# output$totalsurrender <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   surrender <- totalsurrender_reactive()
#     # }
#     
#     surrender <- totalsurrender_filter_reactive()
#     
#     
#     valueBox(
#         surrender,
#         paste(compress(surrender),  "Total Surrender Policy"),color = "yellow",
#         icon = tags$i(class = "fa fa-diagnoses", style="font-size: 44px; color: #fff")
#     )
# })

output$totalsurrender <- renderUI({
    
    surrender <- totalsurrender_filter_reactive()
    
    sub <- paste(compress(surrender),  "Total Surrender Policy")
    
    market_share <- scales::percent(surrender/ (sum(policy_status$surrender)), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(surrender, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("diagnoses"), bgcolor = COL_ORANGE, textcolor = "#fff")
})


# total death value box
# totaldeath_reactive <- reactive({
#   sum(str_detect((policy)$status, regex("^d", ignore_case = T)), na.rm = T)
# })
totaldeath_filter_reactive <- reactive({
    # sum(str_detect((company_policy_data())$status, regex("^d", ignore_case = T)), na.rm = T)
    policy_status$death[policy_status$company_name == loggedincompany()]
})

# output$totaldeath <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   death <- totaldeath_reactive()
#     # }
#     
#     death <- totaldeath_filter_reactive()
#     
#     
#     valueBox(
#         death,
#         paste(compress(death), "Total Death Policy"),color = "red",
#         icon = tags$i(class = "fa fa-book-dead", style="font-size: 44px; color: #fff")
#     )
# })



output$totaldeath <- renderUI({
    
    death <- totaldeath_filter_reactive()
    
    sub <- paste(compress(death), "Total Death Policy")
    
    market_share <- scales::percent(death/ (sum(policy_status$death)), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(death, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("book-dead"), bgcolor = COL_RED, textcolor = "#fff")
})





# total maured value box:
# totalmatured_reactive <- reactive({
#   policy %>% filter(
#     tolower(status) != "death" & tolower(status) != "surrender" | is.na(status),
#     policyenddate < today()
#   ) %>%
#     summarise(n())
# })
# 
totalmatured_filter_reactive <- reactive({
    policy_status$matured[policy_status$company_name == loggedincompany()]
})

# output$totalmatured <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   totalmatured <- totalmatured_reactive()
#     # }
#     
#     totalmatured <- totalmatured_filter_reactive()
#     
#     
#     valueBox(
#         totalmatured,
#         paste(compress(totalmatured),"Total Matured Policy" ),color = "aqua",
#         icon = tags$i(class = "fa fa-balance-scale", style="font-size: 44px; color: #fff")
#     )
# })


output$totalmatured <- renderUI({
    
    totalmatured <- totalmatured_filter_reactive()
    
    sub <- paste(compress(totalmatured),"Total Matured Policy")
    
    market_share <- scales::percent(totalmatured/ (sum(policy_status$matured)), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(totalmatured, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("balance-scale"), bgcolor = COL_BLUE, textcolor = "#fff")
})



policyterm_reactive_filter <- reactive({
    policy_by_term_sum %>% filter(company_name == loggedincompany()) %>% 
        group_by(term) %>% summarise(n = sum(n))
})

output$policytermplot <- renderPlotly({
    
    
    
    plot_ly(
        data = policyterm_reactive_filter(),
        type = "treemap",
        labels = ~term,
        values = ~n,
        parents = "Term",
        textinfo="label+value"
    )
    
    
    
})

output$ploicyterm_companyname <- renderText({
    paste("Company:", loggedincompany())
})



#e receipt summary starts

# ereceiptperday_reactive <- reactive({
#   or %>%
#     filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]) %>%
#     group_by(ordate) %>% 
#     summarise(total = n())
# })

output$linechart_r <- renderHighchart({
    
    # if(input$selectinput == "All"){
    #   data <- ereceiptperday_reactive()
    #   
    # }
    
    # data <- company_or_data() %>%
    #     filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]) %>%
    #     group_by(ordate) %>% 
    #     summarise(total = n())
    
    data <- linechart_r_data %>%
        filter(company_name == loggedincompany(), ordate %in% seq(input$daterange_r[1],
                                                                  input$daterange_r[2], by = 'day')) %>%
        group_by(ordate) %>% 
        summarise(total = sum(total, na.rm = T))
    
    
    hchart(data,
           'line', hcaes(x = ordate, y = total, color = total)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = TRUE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    # plot_ly(data, x = ~ordate, y = ~total, mode = "line", hoverinfo = "text", text = ~paste0("Date: ", ordate," (",weekdays(as.Date(ordate)),")", "<br>", "E-Receipts Issued: ", data$total)) %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Date"),
    #         yaxis = list(title = "Total E-Receipt Issued")
    #     )
})


#total per month bar chart

# ereceiptpermonth_reactive <- reactive({
#   or %>% filter(year_r == input$selectyear_r) %>% 
#     group_by(month_r) %>% summarise(total = n())
# })

output$monthlychart_r <- renderHighchart({
    
    # if(input$selectinput == "All"){
    #   total_by_month <- ereceiptpermonth_reactive()
    # }
    
    total_by_month <- monthlychart_r %>% filter(company_name == loggedincompany()) %>% 
        filter(year_r == input$selectyear_r) %>% 
        group_by(month_r) %>% summarise(total = sum(total, na.rm = T))
    
    hchart(total_by_month,
           'column', hcaes(x = month_r, y = total, color = month_r)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = FALSE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    
    # plot_ly(total_by_month, x = ~month_r, y = ~total, hoverinfo ="y") %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Year: ", input$selectyear_r, "<br>", "Company:", loggedincompany()),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Month"),
    #         yaxis = list(title = "Total")
    #     )
})


output$yearlychart_r <- renderHighchart({
    
    
    
    total_by_month <- monthlychart_r %>% filter(company_name == loggedincompany()) %>%
        group_by(year_r) %>% summarise(total = sum(total, na.rm = T))
    
    
    hchart(total_by_month,
           'column', hcaes(x = year_r, y = total, color = year_r)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = FALSE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    # plot_ly(total_by_month, x = ~factor(year_r), y = ~total, hoverinfo ="y") %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Year"),
    #         yaxis = list(title = "Total")
    #     )
})






#E-receipt Value Boxes
output$companyname_r <- renderValueBox({
    # if(input$selectinput == "All"){
    #   name <- "All Company"
    # }
    
    name <- loggedincompany()
    #name <- "XYZ"
    
    valueBox(
        name,
        "showing result for", color = "navy"
    )
})

#total_e_receipt_issued value

# total_e_receipt_reactive <- reactive({
#   nrow(or %>%
#          filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]))
# })

# output$total_e_receipt_issued <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_e_receipt <- total_e_receipt_reactive()
#     # }
#     
#     total_e_receipt <- premium_by_ortype_count %>%
#         filter(company_name == loggedincompany()) %>%
#         filter(ordate %in% seq(input$daterange_r[1],
#                                input$daterange_r[2], by = 'day')) %>%
#         pull(total) %>% sum()
#     
#     
#     
#     
#     valueBox(
#         format(total_e_receipt, big.mark = ",", scientific = F),
#         paste(compress(total_e_receipt),"Total E-Receipt" ) ,color = "maroon",
#         icon = tags$i(class = "fa fa-reciept", style="font-size: 44px; color: #fff")
#     )
# })



output$total_e_receipt_issued <- renderUI({
    
    total_e_receipt <- premium_by_ortype_count %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_r[1],
                               input$daterange_r[2], by = 'day')) %>%
        pull(total) %>% sum()
    
    sub <-paste(compress(total_e_receipt),"Total E-Receipt" )
    
    market_share <- scales::percent(total_e_receipt/ (premium_by_ortype_count %>%
                                                          filter(ordate %in% seq(input$daterange_r[1],
                                                                                 input$daterange_r[2], by = 'day')) %>%
                                                          pull(total) %>% sum()), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_e_receipt, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("receipt"), bgcolor = COL_RED, textcolor = "#fff")
})




### interactive datatable for agent and policy

output$agent_policy_dt <- renderDataTable(
    agent_policy_dt_data() ,
    filter = "top",
    #server = FALSE,
    extensions = c('Buttons', 'FixedHeader'),
    options = list(columnDefs = list(list(visible=FALSE, targets= 1)), 
                   order = list(list(3, 'desc')), dom= 'Blfrtip', 
                   lengthMenu = c(10, 20, 100, nrow(agent_policy_dt_data ())), pageLength = 10,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), fixedHeader =TRUE,
                   initComplete = JS(
                       "function(settings, json) {",
                       # "$()"
                       #"$('th').css({'background': '#F1C40F','color':'white','transition': '1s ease','border':'none','borderRadius':'10px','textAlign':'center'});",
                       "$('.dt-button').css({'background': '#2ECC71','color':'#fff','border':'none', 'borderRadius':'10px','fontWeight':'700'});",
                       "$('input[type=search]').css({'border':'1.5px solid #F1C40F', 'borderRadius':'10px'});",
                       "$('.box-header').css({'textAlign': 'center'});",
                       "$('h3').css({'fontWeight':'700'});",
                       "$('label').css({'color':'red'});",
                       "$('td').css({'border':'none'});",
                       "}"))
)


# interactive datatable for branch

output$branchdt <- renderDataTable(
    branch_dt_data(),
    filter = "top",
    #server = FALSE,
    extensions = c('Buttons', 'FixedHeader'),
    options = list(columnDefs = list(list(visible=FALSE, targets= 1)),
                   dom= 'Blfrtip', 
                   lengthMenu = c(10, 20, nrow(branch_dt_data ())), pageLength = 10,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), fixedHeader =TRUE,
                   initComplete = JS(
                       "function(settings, json) {",
                       # "$()"
                       #"$('th').css({'background': '#F1C40F','color':'white','transition': '1s ease','border':'none','borderRadius':'10px','textAlign':'center'});",
                       "$('.dt-button').css({'background': '#2ECC71','color':'#fff','border':'none', 'borderRadius':'10px','fontWeight':'700'});",
                       "$('input[type=search]').css({'border':'1.5px solid #F1C40F', 'borderRadius':'10px'});",
                       "$('.box-header').css({'textAlign': 'center'});",
                       "$('h3').css({'fontWeight':'700'});",
                       "$('label').css({'color':'red'});",
                       "$('td').css({'border':'none'});",
                       "}"))
    
)

#interactive datatable for agent e_receipt

output$agentdt <- renderDataTable(
    agent_dt_data() ,
    filter = "top",
    #server = FALSE,
    extensions = c('Buttons', 'FixedHeader'),
    options = list(columnDefs = list(list(visible=FALSE, targets= 1)),
                   order = list(list(4, 'desc')), dom= 'Blfrtip', 
                   lengthMenu = c(10, 20, 100, nrow(agent_dt_data ())), pageLength = 10,
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), fixedHeader =TRUE,
                   initComplete = JS(
                       "function(settings, json) {",
                       # "$()"
                       #"$('th').css({'background': '#F1C40F','color':'white','transition': '1s ease','border':'none','borderRadius':'10px','textAlign':'center'});",
                       "$('.dt-button').css({'background': '#2ECC71','color':'#fff','border':'none', 'borderRadius':'10px','fontWeight':'700'});",
                       "$('input[type=search]').css({'border':'1.5px solid #F1C40F', 'borderRadius':'10px'});",
                       "$('.box-header').css({'textAlign': 'center'});",
                       "$('h3').css({'fontWeight':'700'});",
                       "$('label').css({'color':'red'});",
                       "$('td').css({'border':'none'});",
                       "}"))
)


#First Premium Receipt

# total_fpr_reactive <- reactive({
#   sum((or %>%
#          filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]))$ortype == "F", na.rm = T)
# })

# output$fpr_e_receipt_issued <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_fpr <- total_fpr_reactive()
#     # }
#     
#     total_fpr <- premium_by_ortype_count %>%
#         filter(company_name == loggedincompany()) %>%
#         filter(ordate %in% seq(input$daterange_r[1],
#                                input$daterange_r[2], by = 'day'), ortype == "F") %>%
#         pull(total) %>% sum()
#     
#     
#     valueBox(
#         format(total_fpr, big.mark = ",", scientific = F),
#         paste(compress(total_fpr), "Total First Premium Receipt (FPR)") ,color = "purple",
#         icon = tags$i(class = "fa fa-cash-register", style="font-size: 44px; color: #fff")
#     )
# })

output$fpr_e_receipt_issued <- renderUI({
    
    total_fpr <- premium_by_ortype_count %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_r[1],
                               input$daterange_r[2], by = 'day'), ortype == "F") %>%
        pull(total) %>% sum()
    
    sub <- paste(compress(total_fpr), "Total First Premium Receipt (FPR)")
    
    market_share <- scales::percent(total_fpr/ (premium_by_ortype_count %>%
                                                    filter(ordate %in% seq(input$daterange_r[1],
                                                                           input$daterange_r[2], by = 'day'), ortype == "F") %>%
                                                    pull(total) %>% sum()), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_fpr, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("cash-register"), bgcolor = COL_PURPLE, textcolor = "#fff")
})



#Deferred Premium Receipt

# total_deferred_reactive <- reactive({
#   sum((or %>%
#          filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]))$ortype == "D", na.rm = T)
# })

# output$deferred_e_receipt_issued <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_deferred <- total_deferred_reactive()
#     # }
#     
#     total_deferred <- premium_by_ortype_count %>%
#         filter(company_name == loggedincompany()) %>%
#         filter(ordate %in% seq(input$daterange_r[1],
#                                input$daterange_r[2], by = 'day'), ortype == "D") %>%
#         pull(total) %>% sum()
#     
#     
#     valueBox(
#         format(total_deferred, big.mark = ",", scientific = F),
#         paste(compress(total_deferred), "Total Deferred Premium Receipt (D)"),color = "purple",
#         icon = tags$i(class = "fa fa-truck-loading", style="font-size: 44px; color: #fff")
#     )
# })



output$deferred_e_receipt_issued <- renderUI({
    
    total_deferred <- premium_by_ortype_count %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_r[1],
                               input$daterange_r[2], by = 'day'), ortype == "D") %>%
        pull(total) %>% sum()
    
    sub <- paste(compress(total_deferred), "Total Deferred Premium Receipt (D)")
    
    market_share <- scales::percent(total_deferred/ (premium_by_ortype_count %>%
                                                         filter(ordate %in% seq(input$daterange_r[1],
                                                                                input$daterange_r[2], by = 'day'), ortype == "D") %>%
                                                         pull(total) %>% sum()), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_deferred, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("truck-loading"), bgcolor = COL_PURPLE, textcolor = "#fff")
})


#renewal Receipt

# total_renewal_reactive <- reactive({
#   sum((or %>%
#          filter(ordate >= input$daterange_r[1], 
#                 ordate < input$daterange_r[2]))$ortype == "R", na.rm = T)
# })

# output$renewal_e_receipt_issued <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_renewal <- total_renewal_reactive()
#     # }
#     
#     total_renewal <- premium_by_ortype_count %>%
#         filter(company_name == loggedincompany()) %>%
#         filter(ordate %in% seq(input$daterange_r[1],
#                                input$daterange_r[2], by = 'day'), ortype == "R") %>%
#         pull(total) %>% sum()
#     
#     
#     valueBox(
#         format(total_renewal, big.mark = ",", scientific = F),
#         paste(compress(total_renewal), "Total Renewal Premium Receipt (R)"),color = "green",
#         icon = tags$i(class = "fa fa-glass-cheers", style="font-size: 44px; color: #fff")
#     )
# })

output$renewal_e_receipt_issued <- renderUI({
    
    total_renewal <- premium_by_ortype_count %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_r[1],
                               input$daterange_r[2], by = 'day'), ortype == "R") %>%
        pull(total) %>% sum()
    
    sub <- paste(compress(total_renewal), "Total Renewal Premium Receipt (R)")
    
    market_share <- scales::percent(total_renewal/ (premium_by_ortype_count %>%
                                                        filter(ordate %in% seq(input$daterange_r[1],
                                                                               input$daterange_r[2], by = 'day'), ortype == "R") %>%
                                                        pull(total) %>% sum()), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_renewal, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("glass-cheers"), bgcolor = COL_GREEN, textcolor = "#fff")
})


#premium collection value boxes

output$companyname_p <- renderValueBox({
    
    
    name <- loggedincompany()
    #name <- "XYZ"
    
    valueBox(
        name,
        "showing result for", color = "navy"
    )
})

#Total Premium Collection

# total_prem_reactive <- reactive({
#   sum((or %>%
#          filter(ordate >= input$daterange_p[1], 
#                 ordate < input$daterange_p[2]))$totalpayableamount, na.rm = T)
# })

# output$total_prem <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_prem <- total_prem_reactive()
#     # }
#     
#     total_prem <- premium_by_ortype_sum %>%
#         filter(company_name == loggedincompany()) %>%
#         filter(ordate %in% seq(input$daterange_p[1],
#                                input$daterange_p[2], by = 'day')) %>%
#         pull(total) %>% sum()
#     
#     
#     valueBox(
#         format(total_prem, big.mark = ",", scientific = F),
#         paste(compress(total_prem), "Total Premium Collection"),color = "maroon",
#         icon = tags$i(class = "fa fa-hand-holding-usd", style="font-size: 44px; color: #fff")
#     )
#     
#     
# })


### custom valuebox with change total premium

output$total_prem <- renderUI({
    
    total_prem <- premium_by_ortype_sum %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_p[1],
                               input$daterange_p[2], by = 'day')) %>%
        pull(total) %>% sum()
    
    sub <- paste(compress(total_prem), "Total Premium Collection")
    
    market_share <- scales::percent(total_prem/ (premium_by_ortype_sum %>%
                                                     filter(ordate %in% seq(input$daterange_p[1],
                                                                            input$daterange_p[2], by = 'day')) %>%
                                                     pull(total) %>% sum()), accuracy = 0.01)
    
   
    
    
    
    render_my_box_with_change(format(total_prem, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("hand-holding-usd"), bgcolor = COL_GREEN, textcolor = "#fff")
})





#First Premium Collection

# total_fpr_p_reactive <- reactive({
#   sum((or %>%
#          filter(ordate >= input$daterange_p[1], ordate < input$daterange_p[2], 
#                 ortype == "F"))$totalpayableamount, na.rm = T)
# })




# output$fpr_prem <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_fpr_p <- total_fpr_p_reactive()
#     # }
#     
#     total_fpr_p <- premium_by_ortype_sum %>%
#         filter(company_name == loggedincompany()) %>%
#         filter(ordate %in% seq(input$daterange_p[1],
#                                input$daterange_p[2], by = 'day'), ortype == "F") %>%
#         pull(total) %>% sum()
#     
#     
#     valueBox(
#         format(total_fpr_p, big.mark = ",", scientific = F),
#         paste(compress(total_fpr_p), "Total First Premium Collection"),color = "navy",
#         icon = tags$i(class = "fa fa-money-check-alt", style="font-size: 44px; color: #fff")
#     )
# })




output$fpr_prem <- renderUI({
    
    total_fpr_p <- premium_by_ortype_sum %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_p[1],
                               input$daterange_p[2], by = 'day'), ortype == "F") %>%
        pull(total) %>% sum()
    
    sub <- paste(compress(total_fpr_p), "Total First Premium Collection")
    
    market_share <- scales::percent(total_fpr_p/ (premium_by_ortype_sum %>%
                                                     filter(ordate %in% seq(input$daterange_p[1],
                                                                            input$daterange_p[2], by = 'day'), ortype == "F") %>%
                                                     pull(total) %>% sum()), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_fpr_p, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("money-check-alt"), bgcolor = COL_DARK, textcolor = "#fff")
})



#Deferred Premium Collection

# total_deferred_p_reactive <- reactive({
#   sum((or %>%
#          filter(ordate >= input$daterange_p[1], 
#                 ordate < input$daterange_p[2], ortype == "D"))$totalpayableamount, na.rm = T)
# })
# 
# output$deferred_prem <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_deferred_p <- total_deferred_p_reactive()
#     # }
#     
#     total_deferred_p <- premium_by_ortype_sum %>%
#         filter(company_name == loggedincompany()) %>%
#         filter(ordate %in% seq(input$daterange_p[1],
#                                input$daterange_p[2], by = 'day'), ortype == "D") %>%
#         pull(total) %>% sum()
#     
#     
#     valueBox(
#         format(total_deferred_p, big.mark = ",", scientific = F),
#         paste(compress(total_deferred_p), "Total Deferred Premium Collection"),color = "navy",
#         icon = tags$i(class = "fa fa-funnel-dollar", style="font-size: 44px; color: #fff")
#     )
# })
# 


output$deferred_prem <- renderUI({
    
    total_deferred_p <- premium_by_ortype_sum %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_p[1],
                               input$daterange_p[2], by = 'day'), ortype == "D") %>%
        pull(total) %>% sum()
    
    sub <- paste(compress(total_deferred_p), "Total Deferred Premium Collection")
    
    market_share <- scales::percent(total_deferred_p/ (premium_by_ortype_sum %>%
                                                           filter(ordate %in% seq(input$daterange_p[1],
                                                                                  input$daterange_p[2], by = 'day'), ortype == "D") %>%
                                                           pull(total) %>% sum()), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_deferred_p, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("funnel-dollar"), bgcolor = COL_DARK, textcolor = "#fff")
})







#renewal Collection

# total_ren_p_reactive <- reactive({
#   sum((or %>%
#          filter(ordate >= input$daterange_p[1], 
#                 ordate < input$daterange_p[2], ortype == "R"))$totalpayableamount, na.rm = T)
# })

# output$renewal_prem <- renderValueBox({
#     # if(input$selectinput == "All"){
#     #   total_ren_p <- total_ren_p_reactive()
#     # }
#     
#     # total_ren_p <- sum((company_or_data() %>%
#     #                         filter(ordate >= input$daterange_p[1], ordate < input$daterange_p[2], 
#     #                                ortype == "R"))$totalpayableamount, na.rm = T)
#     
#     
#     total_ren_p <- premium_by_ortype_sum %>%
#         filter(company_name == loggedincompany()) %>%
#         filter(ordate %in% seq(input$daterange_p[1],
#                                input$daterange_p[2], by = 'day'), ortype == "R") %>%
#         pull(total) %>% sum()
#     
#     
#     valueBox(
#         format(total_ren_p, big.mark = ",", scientific = F),
#         paste(compress(total_ren_p),"Total Renewal Premium Collection" ),color = "purple",
#         icon = tags$i(class = "fa fa-credit-card", style="font-size: 44px; color: #fff")
#     )
# })


output$renewal_prem <- renderUI({
    
    total_ren_p <- premium_by_ortype_sum %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_p[1],
                               input$daterange_p[2], by = 'day'), ortype == "R") %>%
        pull(total) %>% sum()
    
    sub <- paste(compress(total_ren_p),"Total Renewal Premium Collection" )
    
    market_share <- scales::percent(total_ren_p/ (premium_by_ortype_sum %>%
                                                      filter(ordate %in% seq(input$daterange_p[1],
                                                                             input$daterange_p[2], by = 'day'), ortype == "R") %>%
                                                      pull(total) %>% sum()), accuracy = 0.01)
    
    
    
    
    
    render_my_box_with_change(format(total_ren_p, big.mark = ",", scientific = F), subtitle = sub,
                              changeText = paste(market_share, "of total market"),
                              icon = icon("credit-card"), bgcolor = COL_PURPLE, textcolor = "#fff")
})



#Total Premium Collection Charts

output$linechart_p <- renderHighchart({
    
    # if(input$selectinput == "All"){
    #   data <- totalpremperday_reactive()
    #   
    # }
    
    data <- linechart_p_or %>%
        filter(company_name == loggedincompany()) %>%
        filter(ordate %in% seq(input$daterange_p[1],
                               input$daterange_p[2], by = 'day')) %>%
        group_by(ordate) %>% 
        summarise(total = sum(total, na.rm = T))
    
    hchart(data,
           'line', hcaes(x = ordate, y = total, color = total)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = TRUE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    # plot_ly(data, x = ~ordate, y = ~total, mode = "line", hoverinfo= "text", text = ~paste0("Date: ", data$ordate," (",weekdays(as.Date(ordate)),")", "<br>", "Premium Collection:", data$total, "Taka")) %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Date"),
    #         yaxis = list(title = "Total Premium (in Taka)")
    #     )
})


#total premium collection per month bar chart


output$monthlychart_p <- renderHighchart({
    
    # if(input$selectinput == "All"){
    #   total_by_month_p <- totalprempermonth_reactive()
    # }
    
    total_by_month_p <- monthlychart_p_or %>% 
        filter(company_name == loggedincompany()) %>%
        filter(year_r == input$selectyear_p) %>% 
        group_by(month_r) %>% summarise(total = sum(total, na.rm = T))
    
    
    hchart(total_by_month_p,
           'column', hcaes(x = month_r, y = total, color = month_r)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = FALSE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    # plot_ly(total_by_month_p, x = ~month_r, y = ~total, hoverinfo= "text", text = ~paste(total_by_month_p$total, "Taka")) %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Year: ", input$selectyear_p, "<br>", "Company:", loggedincompany()),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Month"),
    #         yaxis = list(title = "Total (in Taka)")
    #         
    #     )
})




output$yearlychart_p <- renderHighchart({
    
    
    total_by_year_p <- monthlychart_p_or %>% 
        filter(company_name == loggedincompany()) %>%
        group_by(year_r) %>% summarise(total = sum(total, na.rm = T)) 

    
    hchart(total_by_year_p,
           'column', hcaes(x = year_r, y = total, color = year_r)
    ) %>%
        hc_plotOptions(
            column = list(
                dataLabels = list(
                    enabled = FALSE
                ),
                dataSorting = {
                    enabled = TRUE
                }
            )
        ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    # plot_ly(total_by_year_p, x = ~factor(year_r), y = ~total, hoverinfo= "text", text = ~paste(total_by_year_p$total, "Taka")) %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Year"),
    #         yaxis = list(title = "Total (in Taka)")
    #         
    #     )
})







output$genderchart <- renderPlotly({
    
    # if(input$selectinput == "All"){
    #   genderdata <- genderdata_reactive()
    # }
    
    genderdata <- data.frame(genderdata_sum) %>% 
        filter(company_name == loggedincompany()) %>%
        filter(policystartdate %in% seq(input$daterange_gender[1],
                                        input$daterange_gender[2], by = 'day')) %>%
        filter(gender %in% c("Female", "Male", "Other")) %>% 
        group_by(gender) %>% summarize(counts = sum(counts))
    
    
    plot_ly(genderdata, labels= ~gender, values= ~counts, type='pie') %>% 
        layout(title = "Policyholder's Gender Distribution",
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
})




################################################
############ FORECAST ##########################
################################################


# output$trend_plot <- renderPlotly({
#     
#     switch (input$trend_type_select,
#             "Policy" = {
#                 life_policy_forecast_data[[loggedincompany()]] %>% plot_time_series(.date_var = date, .value = cnt,
#                                                                       .title = "Monlty Policy Issued")
#             },
#             "Premium" = {
#                 life_prem_forecast_data[[loggedincompany()]] %>% plot_time_series(.date_var = date, .value = cnt,
#                                                                                   .title = "Weekly Premium Collection Trend")
#             }
#     )
# }) %>% bindCache(loggedinas(), input$trend_type_select)


#### life  forecast button
# forecast_button_apply <- reactive(switch (input$forecast_product_type_select,
#                                                    "Policy" = {
#                                                        monthly_policy <- life_policy_forecast_data[[loggedincompany()]]
#                                                        policy_forecast_object <- life_policy_forecastObjects[[loggedincompany()]]
#                                                        
#                                                        FORECAST_PERIOD <- as.numeric(input$select_period)
#                                                        
#                                                        full_data <- monthly_policy %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)
#                                                        
#                                                        future_data <- full_data %>% filter(
#                                                            is.na(cnt)
#                                                        )
#                                                        
#                                                        policy_forecast_object %>% 
#                                                            modeltime_calibrate(monthly_policy)%>%
#                                                            modeltime_forecast(new_data = future_data, 
#                                                                               actual_data = monthly_policy, 
#                                                                               keep_data = T
#                                                            ) %>%
#                                                            plot_modeltime_forecast(.title = "Monthly Policy Forecast Plot")
#                                                    },
#                                                    "Premium" = {
#                                                        
#                                                        weekly_premium <- life_prem_forecast_data[[loggedincompany()]]
#                                                        prem_forecast_obj <- life_prem_forecast_object[[loggedincompany()]]
#                                                        FORECAST_PERIOD <- as.numeric(input$select_period)
#                                                        full_data <- weekly_premium %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)
#                                                        future_data <- full_data %>% filter(
#                                                            is.na(cnt)
#                                                        )
#                                                        
#                                                        prem_forecast_obj %>% 
#                                                            modeltime_calibrate(weekly_premium)%>%
#                                                            modeltime_forecast(new_data = future_data, 
#                                                                               actual_data = weekly_premium, 
#                                                                               keep_data = T
#                                                            ) %>%
#                                                            plot_modeltime_forecast(.title = "Weekly Premium Collection Forecast Plot")
#                                                        
#                                                    }
#                                            )) %>% bindCache(loggedinas(), input$forecast_product_type_select, input$select_period ) %>% 
#                                             bindEvent(input$forecastButton)
#     
# 
# 
# output$forecast_plot <- renderPlotly({
#     
#     forecast_button_apply()
#     
# })







