output$companyname_cp <- renderValueBox({
  
  name <- loggedincompany()
  
  
  valueBox(
    paste("Showing Results For:", name),  #name
    ""
  )
})

## total policy count 

#for both active and total count
# x_coverpolicy_reactive <- reactive({
#     company_coverpolicy_data() %>% select(covernotenumber, policynumber, coverageenddate, company_name)
# })

# output$total_policy_count <- renderValueBox({
#   
#   # x_coverpolicy <- x_coverpolicy_reactive() 
#   
#   totalpolicycount <- daywisetotal_p_count %>%
#     filter(company_name == loggedincompany(), policyissuedate %in% seq(input$daterange_1.2[1],
#                                                             input$daterange_1.2[2], by = 'day')) %>%
#     ungroup() %>%
#     pull(total) %>% sum()
#   
#   
#   
#   valueBox(
#     totalpolicycount %>% format(big.mark = ",", scientific = F),
#     paste(compress(totalpolicycount),"Total Policy Count"),
#     color = "purple",
#     icon = tags$i(class = "fa fa-pencil-ruler", style="font-size: 44px; color: #fff")
#   )
# })

output$total_policy_count <- renderUI({
  
  totalpolicycount <- daywisetotal_p_count %>%
    filter(company_name == loggedincompany(), policyissuedate %in% seq(input$daterange_1.2[1],
                                                                       input$daterange_1.2[2], by = 'day')) %>%
    ungroup() %>%
    pull(total) %>% sum()
  
  sub <- paste(compress(totalpolicycount),"Total Policy Count")
  
  market_share <- scales::percent(totalpolicycount/ (daywisetotal_p_count %>%
                                                       filter(policyissuedate %in% seq(input$daterange_1.2[1],
                                                                                       input$daterange_1.2[2], by = 'day')) %>%
                                                       ungroup() %>%
                                                       pull(total) %>% sum()), accuracy = 0.01)
  
  
  
  
  
  render_my_box_with_change(format(totalpolicycount, big.mark = ",", scientific = F), subtitle = sub,
                            changeText = paste(market_share, "of total market"),
                            icon = icon("pencil-ruler"), bgcolor = COL_PURPLE, textcolor = "#fff")
})

## total active policy 
# 
# output$total_active_policy_count <- renderValueBox({
#     
#     
#     # x_coverpolicy <- x_coverpolicy_reactive() 
#     
#     totalactive <- total_active_policy_count_sum %>%
#         filter(company_name == loggedincompany()) %>%
#         summarise(sum(total))
#     
#     
#     valueBox(
#         totalactive,
#         paste(compress(totalactive),"Total Active Policy Count"),color = "purple"
#     )
# }
# )


#Total Expired Policy Count:
# totalexpired_reactive <- reactive({
#     coverpolicy %>% filter (coverageenddate < today()) 
# })

# output$total_expired_policy_count <- renderValueBox({
#   
#   totalexpired <- totalexpired_sum %>%
#     filter(company_name == loggedincompany(), 
#            policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>% 
#     ungroup() %>%
#     pull(total) %>% sum()
#   
#   
#   valueBox(
#     totalexpired  %>% format(big.mark = ",", scientific = F),
#     paste(compress(totalexpired), "Total Expired Policy Count"),color = "purple",
#     icon = tags$i(class = "fa fa-hourglass-end", style="font-size: 44px; color: #fff")
#   )
#   
# })



output$total_expired_policy_count <- renderUI({
  
  totalexpired <- totalexpired_sum %>%
    filter(company_name == loggedincompany(), 
           policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>% 
    ungroup() %>%
    pull(total) %>% sum()
  
  sub <-  paste(compress(totalexpired), "Total Expired Policy Count")
  
  market_share <- scales::percent(totalexpired/ (totalexpired_sum %>%
                                                   filter(policyissuedate >= input$daterange_1.2[1], 
                                                          policyissuedate <= input$daterange_1.2[2]) %>% 
                                                   ungroup() %>%
                                                   pull(total) %>% sum()
  ), accuracy = 0.01)
  
  
  
  
  
  render_my_box_with_change(format(totalexpired, big.mark = ",", scientific = F), subtitle = sub,
                            changeText = paste(market_share, "of total market"),
                            icon = icon("hourglass-end"), bgcolor = COL_PURPLE, textcolor = "#fff")
})



#Total Cancelled Policy Count:
# totalcancelled_reactive <- reactive({
#     company_coverpolicy_data() %>% filter (!is.na(cancelledcover) | !is.na(cancelledpolicy)) 
# })

# output$total_cancelled_policy_count <- renderValueBox({
#   
#   totalcancelled <- totalcancelled_sum %>%
#     filter(company_name == loggedincompany(), 
#            policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
#     ungroup() %>%
#     summarise(sum(total)) 
#   
#   
#   valueBox(
#     totalcancelled,
#     paste(compress(totalcancelled), "Total Cancelled Policy Count"),color = "purple",
#     icon = tags$i(class = "fa fa-cut", style="font-size: 44px; color: #fff")
#   )
#   
# })


output$total_cancelled_policy_count <- renderUI({
  
  totalcancelled <- totalcancelled_sum %>%
    filter(company_name == loggedincompany(), 
           policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
    ungroup() %>% pull(total) %>% sum()
  
  sub <-  paste(compress(totalcancelled), "Total Cancelled Policy Count")
  
  market_share <- scales::percent(totalcancelled/ (totalcancelled_sum %>%
                                                     filter(
                                                            policyissuedate >= input$daterange_1.2[1], 
                                                            policyissuedate <= input$daterange_1.2[2]) %>%
                                                     ungroup() %>% pull(total) %>% sum()), accuracy = 0.01)
  
  
  
  
  
  render_my_box_with_change(format(totalcancelled, big.mark = ",", scientific = F), subtitle = sub,
                            changeText = paste(market_share, "of total market"),
                            icon = icon("cut"), bgcolor = COL_PURPLE, textcolor = "#fff")
})


#Daywise Total Covernote Issue Chart

# daywisetotal_c_count_reactive <- reactive({
#     company_coverpolicy_note() %>%
#         filter(covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>%
#         group_by(covernoteissuedate) %>% 
#         summarise(total = n())
# })


# output$companyname_cover_count <- renderValueBox({
#   
#   name <- loggedincompany()
#   
#   
#   valueBox(
#     paste("Showing Results For:", name),
#     ""
#   )
# })
# 
# 
# #total cover count
# output$total_cover_count <- renderValueBox({
#   
#   
#   totalcovercount <- daywisetotal_c_count %>% 
#     filter(company_name == loggedincompany(),
#            covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>% 
#     pull(total) %>% sum()
#   
#   
#   
#   valueBox(
#     totalcovercount %>% format(big.mark = ",", scientific = F),
#     paste(compress(totalcovercount),"Total Covernote Count"),color = "purple",
#     icon = tags$i(class = "fa fa-book-open", style="font-size: 44px; color: #fff")
#   )
# })
# 
# #total cancelled cover
# output$total_cancelled_cover_count <- renderValueBox({
#   
#   totalcancelled <- totalcancelled_cover_sum %>%
#     filter(company_name == loggedincompany(), covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>%
#     ungroup() %>%
#     pull(total) %>% sum() 
#   
#   
#   valueBox(
#     totalcancelled %>% format(big.mark = ",", scientific = F),
#     paste(compress(totalcancelled),"Total Cancelled Covernote"),color = "purple",
#     icon = tags$i(class = "fa fa-cut", style="font-size: 44px; color: #fff")
#   )
# })
# 
# 
# output$linechart_day_1.1 <- renderHighchart({
#   
#   
#   data <- reactive({ daywisetotal_c_count %>%
#     filter(company_name == loggedincompany(), covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>%
#     group_by(covernoteissuedate) %>% 
#     summarise(total = sum(total, na.rm = T))
#   })
#   
#   hchart(data(),
#     'line', hcaes(x = covernoteissuedate, y = total, color = covernoteissuedate)
#   ) %>%
#     hc_plotOptions(
#       column = list(
#         dataLabels = list(
#           enabled = TRUE
#         ),
#         dataSorting = {
#           enabled = TRUE
#         }
#       )
#     ) %>% 
#     hc_exporting(
#       enabled = TRUE, # always enabled
#       filename = "custom-file-name"
#     )
#   
#   # plot_ly(data, x = ~covernoteissuedate, y = ~total, mode = "line", hoverinfo= "text", 
#   #         text = ~paste0("Date: ", data$covernoteissuedate," (", weekdays(as.Date(covernoteissuedate)),")", "<br>", "Covernote Issued: ", data$total)) %>%
#   #   layout(
#   #     annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
#   #                      yref="paper",y=0.9,showarrow=FALSE),
#   #     xaxis = list(title = "Date"),
#   #     yaxis = list(title = "Total Covernote Issued")
#   #   )
# })
# 
# 
# #Monthwise Total Covernote Issue Chart
# 
# # monthwisetotal_c_count_reactive <-reactive({
# #     company_coverpolicy_note() %>% filter(year_c == input$selectyear_1.2) %>% 
# #         group_by(month_c) %>% 
# #         summarise(total = n())
# # })
# 
# output$linechart_month_1.2 <- renderHighchart({
#   
#   
#   total_by_month <- reactive({ monthwisetotal_c_count %>% 
#     filter(year_c == input$selectyear_1.2, company_name == loggedincompany()) %>% 
#     group_by(month_c) %>%
#     summarise(total = sum(total, na.rm = T))
#   })
#   
#   hchart(total_by_month(),
#     'column', hcaes(x = month_c, y = total, color = month_c)
#   ) %>%
#     hc_plotOptions(
#       column = list(
#         dataLabels = list(
#           enabled = TRUE
#         ),
#         dataSorting = {
#           enabled = TRUE
#         }
#       )
#     ) %>% 
#     hc_exporting(
#       enabled = TRUE, # always enabled
#       filename = "custom-file-name"
#     )
#   
#   # plot_ly(total_by_month, x = ~month_c, y = ~total, hoverinfo ="y") %>%
#   #   layout(
#   #     
#   #     annotations=list(text=paste("Year: ", input$selectyear_1.2, "<br>", "Company:", loggedincompany()), xref="paper",x=0.5,
#   #                      yref="paper",y=0.9,showarrow=FALSE
#   #     ),
#   #     xaxis = list(title = "Month"),
#   #     yaxis = list(title = "Total Covernote Issued")
#   #   )
# })
# 
# #Year wise Total Covernote Issue Chart
# 
# # yearwisetotal_c_count_reactive <- reactive({
# #     company_coverpolicy_note() %>% group_by(year_c) %>% summarise(total = n())
# # })
# 
# output$linechart_year_1.3 <- renderHighchart({
#   
#   total_by_year <- reactive({ monthwisetotal_c_count %>% 
#     filter(company_name == loggedincompany(), year_c <= input$yearrange_1.3[2], year_c >= input$yearrange_1.3[1]) %>% 
#     group_by(year_c) %>% 
#     summarise(total = sum(total, na.rm = T))
#   })
#   
#   hchart(total_by_year(),
#     'column', hcaes(x = year_c, y = total, color = year_c)
#   ) %>%
#     hc_plotOptions(
#       column = list(
#         dataLabels = list(
#           enabled = TRUE
#         ),
#         dataSorting = {
#           enabled = TRUE
#         }
#       )
#     ) %>% 
#     hc_exporting(
#       enabled = TRUE, # always enabled
#       filename = "custom-file-name"
#     )
#   
#   # plot_ly(total_by_year %>% filter(year_c <= input$yearrange_1.3[2], year_c >= input$yearrange_1.3[1]), x = ~factor(year_c), y = ~total, type = "bar", hoverinfo ="y") %>%
#   #   layout(
#   #     
#   #     annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
#   #                      yref="paper",y=0.9,showarrow=FALSE),
#   #     xaxis = list(title = "Year"),
#   #     yaxis = list(title = "Total Covernote Issued")
#   #   )
#   
# })
# 
# 
# #interactive datatable for agentwise cover performance
# # agentwisedt_note_reactive <- reactive({
# #     company_coverpolicy_note() %>% group_by(company_name, agentid) %>%
# #         filter(!is.na(agentid), agentid != "N/A") %>% 
# #         summarise(Total_Covernote = n())
# # })
# # 
# output$agentdt_note <- renderDataTable(
#   agentwisedt_note %>% filter(company_name == loggedincompany()),
#   server = FALSE,
#   extensions = 'Buttons',
#   options = list(order = list(list(3, 'desc')), dom= 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
#                  columnDefs = list (list(visible = FALSE, targets =1)),
#                  initComplete = JS(
#                    "function(settings, json) {",
#                    "$('.dt-button').css({'background': '#2ECC71','color':'#fff','border':'none', 'borderRadius':'10px','fontWeight':'700'});",
#                    "$('input[type=search]').css({'border':'1.5px solid #F1C40F', 'borderRadius':'10px'});",
#                    "$('.box-header').css({'textAlign': 'center'});",
#                    "$('h3').css({'fontWeight':'700'});",
#                    "$('label').css({'color':'red'});",
#                    "$('td').css({'border':'none'});",
#                    "}"))
# )


#Day wise Total Policy Issue Chart

# daywisetotal_p_count_reactive <- reactive({
#     company_coverpolicy_policy() %>%
#         filter(policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
#         group_by(policyissuedate) %>% 
#         summarise(total = n())
# })



output$linechart_policycount <- renderHighchart({
  
  data <- daywisetotal_p_count %>%
    filter(company_name == loggedincompany(), policyissuedate %in% seq(input$daterange_1.2[1],  input$daterange_1.2[2], by = 'day')) %>%
    group_by(policyissuedate) %>% 
    summarise(total = sum(total, na.rm = T))
  
  hchart(data,
         'line', hcaes(x = policyissuedate, y = total, color = total)) %>%
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
      enabled = TRUE,
      filename = "custom-file-name"
    )
  
  # plot_ly(data, x = ~policyissuedate, y = ~total, mode = "line", hoverinfo= "text", text = ~paste0("Date: ", data$policyissuedate," (", weekdays(as.Date(policyissuedate)),")", "<br>", "Policy Issued: ", data$total)) %>%
  #   layout(
  #     annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
  #                      yref="paper",y=1.0,showarrow=FALSE),
  #     xaxis = list(title = "Date"),
  #     yaxis = list(title = "Total Policy Issued")
  #   )
})

#Month wise Total Policy Issue Chart

# monthwisetotal_p_count_reactive <-reactive({
#     company_coverpolicy_policy() %>% filter(year_p == input$selectyear_1.5) %>% 
#         group_by(month_p) %>% 
#         summarise(total = n())
# })

output$barchart_monthly_policycount <- renderHighchart({
  
  monthwisetotal_p_count %>% 
    filter(year_p == input$selectyear_1.5, company_name == loggedincompany()) %>% 
    group_by(month_p) %>% 
    summarise(total = sum(total, na.rm = T)) %>% 
  
  hchart(
    'column', hcaes(x = month_p, y = total, color = month_p)
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
  
  # plot_ly(total_by_month, x = ~month_p, y = ~total, hoverinfo ="y") %>%
  #   layout(
  #     annotations=list(text=paste("Year: ", input$selectyear_1.5, "<br>", "Company:", loggedincompany()), xref="paper",x=0.5,
  #                      yref="paper",y=1.0,showarrow=FALSE),
  #     xaxis = list(title = "Month"),
  #     yaxis = list(title = "Total Policy Issued")
  #   )
})

#Year wise Total Policy Issue Chart

# yearwisetotal_p_count_reactive <- reactive({
#     company_coverpolicy_policy() %>% group_by(year_p) %>% summarise(total = n())
# })




output$barchart_yearly_policycount <- renderHighchart({
  monthwisetotal_p_count %>% 
    filter(company_name == loggedincompany(), year_p <= input$yearrange_1.6[2], year_p >= input$yearrange_1.6[1]) %>% 
    group_by(year_p) %>% 
    summarise(total = sum(total, na.rm = T)) %>% 
 
  hchart(
    'column', hcaes(x = year_p, y = total, color = year_p)
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
  
  # plot_ly(total_by_year %>% filter(year_p <= input$yearrange_1.6[2], year_p >= input$yearrange_1.6[1]), x = ~factor(year_p), y = ~total, type = "bar", hoverinfo ="y") %>%
  #   layout(
  #     annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
  #                      yref="paper",y=1.0,showarrow=FALSE),
  #     xaxis = list(title = "Year"),
  #     yaxis = list(title = "Total Policy Issued")
  #   )
})

#interactive datatable for agentwise policy performance
# agentwisedt_policy_reactive <- reactive({
#     company_coverpolicy_policy() %>% group_by(company_name, agentid) %>%
#         filter(!is.na(agentid), agentid != "N/A") %>% 
#         summarise(Total_Policy = n())
# })
# 

output$agentdt_policy <- renderDataTable(
  agentwisedt_policy %>% filter(company_name == loggedincompany()),
  server = FALSE,
  extensions = 'Buttons',
  options = list(order = list(list(3, 'desc')), dom= 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                 columnDefs = list (list(visible = FALSE, targets =1)),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$('.dt-button').css({'background': '#2ECC71','color':'#fff','border':'none', 'borderRadius':'10px','fontWeight':'700'});",
                   "$('input[type=search]').css({'border':'1.5px solid #F1C40F', 'borderRadius':'10px'});",
                   "$('.box-header').css({'textAlign': 'center'});",
                   "$('h3').css({'fontWeight':'700'});",
                   "$('label').css({'color':'red'});",
                   "$('td').css({'border':'none'});",
                   "}"))
)


########################               Sidebar 2 Server Code              ##################################

###Net Premium Collection Value Box
output$companyname_mr <- renderValueBox({
  
  
  name <- loggedincompany()
  
  
  valueBox(
    paste("Showing Results For:", name),  #name
    ""
  )
})

###Premium Collection Report
#Net Premium Collection in Amount
# total_prem_reactive <- reactive({
#     sum((company_mr() %>%
#              filter(mrdate >= input$daterange_2[1], 
#                     mrdate <= input$daterange_2[2]))$totalpremium, na.rm = T)
# })

# output$total_prem <- renderValueBox({
#   
#   total_prem <- daywisetotalprem %>% 
#     filter(company_name == loggedincompany(), mrdate %in% seq(input$daterange_2[1],
#                                                             input$daterange_2[2], by = 'day')) %>%
#     pull(total) %>% 
#     sum() 
#   
#   
#   valueBox(
#     format(total_prem, big.mark = ",", scientific = F),
#     paste(compress(total_prem),"Net Premium Collection in Amount"),color = "maroon",
#     icon = tags$i(class = "fa fa-hand-holding-usd", style="font-size: 44px; color: #fff")
#   )
# })



output$total_prem <- renderUI({
  
  total_prem <- daywisetotalprem %>% 
    filter(company_name == loggedincompany(), mrdate %in% seq(input$daterange_2[1],
                                                              input$daterange_2[2], by = 'day')) %>%
    pull(total) %>% 
    sum()
  
  sub <- paste(compress(total_prem),"Net Premium Collection in Amount")
  
  market_share <- scales::percent(total_prem/ (daywisetotalprem %>% 
                                                 filter(mrdate %in% seq(input$daterange_2[1],
                                                                        input$daterange_2[2], by = 'day')) %>%
                                                 pull(total) %>% 
                                                 sum()), accuracy = 0.01)
  
  
  
  
  
  render_my_box_with_change(format(total_prem, big.mark = ",", scientific = F), subtitle = sub,
                            changeText = paste(market_share, "of total market"),
                            icon = icon("hand-holding-usd"), bgcolor = COL_RED, textcolor = "#fff")
})


#Daywise Net Premium Collection Chart

# daywisetotalprem_reactive <- reactive({
#     company_mr() %>%
#         filter(mrdate >= input$daterange_2[1], mrdate <= input$daterange_2[2]) %>%
#         group_by(mrdate) %>% 
#         summarise(total = sum(totalpremium, na.rm = T))
# })

output$linechart_day <- renderHighchart({
  
  
  data <- daywisetotalprem %>%
    filter(company_name == loggedincompany(), mrdate >= input$daterange_2[1], mrdate <= input$daterange_2[2]) %>%
    group_by(mrdate) %>% 
    summarise(total = sum(total, na.rm = T))
  
  hchart(data,
    'line', hcaes(x = mrdate, y = round(total, 0), color = total)
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
  
  
  # plot_ly(data, x = ~mrdate, y = ~total, mode = "line", hoverinfo= "text", 
  #         text = ~paste0("Date: ", data$mrdate," (", weekdays(as.Date(mrdate)),")", "<br>", "Premium Collection: ", data$total, "Taka")) %>%
  #   layout(
  #     
  #     annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Date"),
  #     yaxis = list(title = "Net Premium (in Taka)")
  #   )
})

#Month wise Net Premium Collection Chart

# monthwisetotalprem_reactive <- reactive({
#     company_mr() %>% 
#         filter(year_r == input$selectyear_p) %>% 
#         group_by(month_r) %>% summarise(total = sum(totalpremium, na.rm = T))
# })

output$linechart_month <- renderHighchart({
  
  
  totalprem_by_month <- monthwisetotalprem %>% 
    filter(year_r == input$selectyear_p, company_name == loggedincompany()) %>% 
    group_by(month_r) %>% 
    summarise(total = sum(total, na.rm = T))
    
  hchart(totalprem_by_month,
    'column', hcaes(x = month_r, y = round(total, 0), color = month_r)
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
  
    
  # plot_ly(totalprem_by_month, x = ~month_r, y = ~total, hoverinfo= "text", text = ~paste(totalprem_by_month$total, "Taka")) %>%
  #   layout(
  #     
  #     annotations=list(text=paste("Year: ", input$selectyear_p, "<br>", "Company:", loggedincompany()),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Month"),
  #     yaxis = list(title = "Total (in Taka)")
  #     
  #   )
})

#Year wise Net Premium Collection Chart

# yearwisetotalprem_reactive <- reactive({
#     company_mr() %>% group_by(year_r) %>% summarise(total = sum(totalpremium, na.rm = T))
# })

output$linechart_year <- renderHighchart({
  
  totalprem_by_year <- monthwisetotalprem %>% 
    filter(company_name == loggedincompany(), year_r <= input$yearrange_p[2], year_r >= input$yearrange_p[1]) %>% 
    group_by(year_r) %>% 
    summarise(total = sum(total, na.rm = T))
     
  hchart(totalprem_by_year,
    'column', hcaes(x = year_r, y = round(total, 0), color = year_r)
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
  
  # plot_ly(totalprem_by_year %>% filter(year_r <= input$yearrange_p[2], year_r >= input$yearrange_p[1]), x = ~factor(year_r), y = ~total, type = "bar", hoverinfo= "text", text = ~paste(total, "Taka")) %>%
  #   layout(
  #     
  #     annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Year"),
  #     yaxis = list(title = "Total (in Taka)")
  #   )
  
})


# interactive datatable for branch from 'mr' data
branchwisedt_reactive <- reactive({
  branchwisetotalprem %>% filter(company_name == loggedincompany(),
                                 mrdate %in% seq(input$daterange_branch_dt[1],
                                                 input$daterange_branch_dt[2], by = 'day')) %>%
    group_by(company_name, officebranchcode) %>% 
    summarise(Total_Transaction= sum(Total_Transaction, na.rm = T), Total_Premium = sum(total, na.rm = T))
})

output$branchdt <- renderDataTable(
  datatable(branchwisedt_reactive()) %>% formatCurrency(columns = 4,currency = "BDT ", mark = ","),
  server = FALSE,
  extensions = 'Buttons',
  options = list(order = list(list(4, 'desc')), dom= 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                 #columnDefs = list (list(visible = FALSE, targets = 1)),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$('.dt-button').css({'background': '#2ECC71','color':'#fff','border':'none', 'borderRadius':'10px','fontWeight':'700'});",
                   "$('input[type=search]').css({'border':'1.5px solid #F1C40F', 'borderRadius':'10px'});",
                   "$('.box-header').css({'textAlign': 'center'});",
                   "$('h3').css({'fontWeight':'700'});",
                   "$('label').css({'color':'red'});",
                   "$('td').css({'border':'none'});",
                   "}"))
)

#####################################  Sidebar 3 Server Code   #####################################
###Analytical Comparison Report

output$companyname_nonlife <- renderValueBox({
  
  name <- loggedincompany()
  
  
  valueBox(
    paste("Showing Results For:", name),  #name
    ""
  )
})

#Total Bank Deposited Policy Count:
# totalbankdep_reactive <- reactive({
#     company_mr() %>% mutate (modeofpayment_updated = ifelse (grepl("^P|ORDER|^ON|^I|BANK|^D|^TRANS|DRAFT|^CH|CHQ|CEQ|CHA|CREDIT|D.D", modeofpayment, ignore.case = TRUE),
#                                                              "Bank Deposited",
#                                                              "Others")) %>% 
#         filter (modeofpayment_updated == "Bank Deposited", mrdate >= input$daterange_3.1[1], mrdate <= input$daterange_3.1[2]) 
# })

output$bank_deposited_policy <- renderValueBox({
  
  totalbankdep <- totalbankdep_sum %>%
    filter (company_name == loggedincompany(), modeofpayment_updated == "Bank Deposited") %>%
    ungroup() %>%
    summarise(sum(total))
  
  
  valueBox(
    totalbankdep,
    paste(compress(totalbankdep),"Total Bank Deposited Policy Count"),color = "purple",
    icon = tags$i(class = "fa fa-layer-group", style="font-size: 44px; color: #fff")
  )
  
})

## valid mobile

# totalvalidmobile_reactive <- reactive({
#   mobile <- coverpolicy %>% pull(mobilenumber)
#   valid_mobile <- str_match(mobile, regex("01[3-9][0-9]{8}"))
#   length(valid_mobile[!is.na(valid_mobile)])
# })

output$valid_mobile_number <- renderValueBox({
  
  # mobile <- company_coverpolicy_data()  %>% pull(mobilenumber)
  # valid_mobile <- str_match(mobile, regex("01[3-9][0-9]{8}"))
  # totalvalidmobile <- length(valid_mobile[!is.na(valid_mobile)])
  # validmobile_percent <- paste0(round(totalvalidmobile / length(mobile) * 100 , 2),"%")
  
  totalvalidmobile <- totalvalidmobile_sum[totalvalidmobile_sum$company_name == loggedincompany(),]$totalvalidmobile
  validmobile_percent <- totalvalidmobile_sum[totalvalidmobile_sum$company_name == loggedincompany(),]$validmobile_percent
  
  
  valueBox(
    paste0(totalvalidmobile, " (", validmobile_percent, ")"),
    "Total Valid Mobile Number",
    icon = tags$i(class = "fa fa-phone-square", style="font-size: 44px; color: #fff")
  )
  
})


## valid email value box

output$valid_email_address <- renderValueBox({
  
  
  # email <- company_coverpolicy_data()  %>% pull(email)
  # valid_email <- str_match(email, regex("\\S+@\\S+\\.\\S+"))
  # totalvalidemail <- length(valid_email[!is.na(valid_email)])
  # validemail_percent <- paste0(round(totalvalidemail / length(email) * 100 , 2),"%")
  totalvalidemail <- valid_email_address_sum[valid_email_address_sum$company_name == loggedincompany(),]$totalvalidemail
  validemail_percent <- valid_email_address_sum[valid_email_address_sum$company_name == loggedincompany(),]$validemail_percent
  
  
  valueBox(
    paste0(totalvalidemail, " (", validemail_percent, ")"),
    "Total Valid Email Address",
    icon = tags$i(class = "fa fa-mail-bulk", style="font-size: 44px; color: #fff")
  )
  
})


output$policydate_vs_number_value <- renderValueBox({
  
  t <- policynumber_vs_issuedate %>%
    filter (company_name == loggedincompany()) %>%
    ungroup() %>%
    summarise(n = sum(total)) %>% pull(n)
  
  
  valueBox(
    t,
    paste(compress(t),"Policy Issue Dates Missing but Policynumber Present"),color = "maroon"
  )
  
})

output$covernote_present_policy_absent <- renderValueBox({
  
  t <- covernote_present_policy_absent %>%
    filter (company_name == loggedincompany()) %>%
    ungroup() %>%
    summarise(n = sum(total)) %>% pull(n)
  
  
  valueBox(
    t,
    paste(compress(t),"Covernote Present but Policy issuedate Absent"),color = "maroon"
  )
  
})

output$covernote_policy_both_present <- renderValueBox({
  
  t <- covernote_policy_both_present %>%
    filter (company_name == loggedincompany()) %>%
    ungroup() %>%
    summarise(n = sum(total)) %>% pull(n)
  
  
  valueBox(
    t,
    paste(compress(t),"Both Covernote and Policy issuedate Present")
  )
  
})



###'insurancetype' Pie Chart

# insurancetype_reactive <- reactive({
#     company_coverpolicy_data() %>% filter(insurancetype !="", covernoteissuedate >= input$daterange_insurancetype[1], covernoteissuedate <= input$daterange_insurancetype[2]) %>%
#         group_by(insurancetype) %>% summarize(counts = n(), percentage = n()/nrow(coverpolicy))
# })

output$insurancetypepiechart <- renderHighchart({
  
  
  # insurancetypedata <- insurancetype_sum %>%
  #     filter(company_name == loggedincompany(), policyissuedate >= input$daterange_insurancetype[1], 
  #            policyissuedate <= input$daterange_insurancetype[2]) %>%
  #   group_by(insurancetype) %>%
  #   summarise(counts = sum(counts, na.rm = T),percentage = sum(counts)/nrow(insurancetype_sum))
  insurancetypedata <- insurancetype_sum_bar %>%
    filter(company_name == loggedincompany()) %>%
    group_by(year_p, insurancetype) %>%
    summarise(counts = sum(counts, na.rm = T)) %>% mutate(percentage = prop.table(counts))
  
  hchart(insurancetypedata,
         'column', hcaes(x = year_p, y = counts, group = insurancetype), stacking = "normal"
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
  
  # plot_ly(insurancetypedata, labels= ~insurancetype, values= ~percentage, type='pie',
  #         textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
  #         hoverinfo = 'text', text = ~paste(insurancetypedata$counts),
  #         marker = list(colors = colors,
  #                       line = list(color = '#FFFFFF', width = 1))) %>%
  #     layout(
  #         annotations=list(text=paste("Company:", loggedincompany()), xref="paper",x=0.9,
  #                          yref="paper",y=1,showarrow=FALSE))
  
  # plot_ly(insurancetypedata, x = ~factor(year_p), y = ~ counts,
  #         hoverinfo = 'text', text = ~paste(insurancetypedata$counts,
  #                                           insurancetypedata$insurancetype,"(", scales::percent(insurancetypedata$percentage, 0.01) ,") policy")) %>% 
  #   add_bars(color = ~insurancetype,  orientation = 'v') %>% layout(barmode = "stack") %>%
  #   layout(
  #     xaxis = list(title = "Policy Issued"),
  #     yaxis = list(title = "")
  #   )
})


# policytype datatable
output$insurancetype_dt <- renderDataTable(
  
  (insurancetype_sum %>%
     filter(company_name == loggedincompany(), covernoteissuedate %in% seq(input$daterange_insurancetype_dt[1],
                                                             input$daterange_insurancetype_dt[2], by = 'day')) %>% 
     group_by(insurancetype) %>%
     summarize(Covernote_Issued = sum(counts)))
  ,
  server = FALSE,
  extensions = 'Buttons',
  options = list(order = list(list(2, 'desc')), dom= 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                 
                 initComplete = JS(
                   "function(settings, json) {",
                   "$('.dt-button').css({'background': '#2ECC71','color':'#fff','border':'none', 'borderRadius':'10px','fontWeight':'700'});",
                   "$('input[type=search]').css({'border':'1.5px solid #F1C40F', 'borderRadius':'10px'});",
                   "$('.box-header').css({'textAlign': 'center'});",
                   "$('h3').css({'fontWeight':'700'});",
                   "$('label').css({'color':'red'});",
                   "$('td').css({'border':'none'});",
                   "}"))
  
)

#Co-insurance Status Pie Chart 3.1

# coinsurance_reactive <- reactive({
#     company_mr() %>% filter(iscoinsurance !="", mrdate >= input$daterange_3.2[1], mrdate <= input$daterange_3.2[2]) %>%
#         group_by(iscoinsurance) %>% summarize(counts = n(), percentage = n()/nrow(mr))
# })

output$piechart_3.1 <- renderPlotly({
  
  
  coinsurancedata <- coinsurancedata_sum %>% filter(company_name == loggedincompany(),
                                                    mrdate %in% seq(input$daterange_3.2[1],
                                                            input$daterange_3.2[2], by = 'day')) %>% 
    group_by(iscoinsurance) %>% summarize(counts = sum(counts), percentage = sum(counts)/nrow_mr)
  
  
  plot_ly(coinsurancedata, labels= ~iscoinsurance, values= ~percentage, type='pie',
          textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text', text = ~paste(coinsurancedata$counts),
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1))) %>%
    layout(
      annotations=list(text=paste("Company:", loggedincompany()), xref="paper",x=0.9,
                       yref="paper",y=1,showarrow=FALSE))
})

#SMS Sending Status Pie Chart 3.2
# smsdata_reactive <- reactive({
#     company_mr() %>% filter(smsstatus !="", mrdate >= input$daterange_3.2[1], mrdate <= input$daterange_3.2[2]) %>%
#         group_by(smsstatus) %>% summarize(counts = n(), percentage = n()/nrow(mr))
# })

# output$piechart_3.2 <- renderPlotly({
#     
#     
#     smsdata <- smsdata_sum %>% filter(company_name == loggedincompany(), mrdate >= input$daterange_3.2[1], mrdate <= input$daterange_3.2[2]) %>% 
#         group_by(smsstatus) %>% summarize(counts = sum(counts), percentage = sum(counts)/nrow(mr))
#     
#     
#     
#     plot_ly(smsdata, labels= ~smsstatus, values= ~percentage, type='pie',
#             textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
#             hoverinfo = 'text', text = ~paste(smsdata$counts),
#             marker = list(colors = colors,
#                           line = list(color = '#FFFFFF', width = 1))) %>%
#         layout(
#             annotations=list(text=paste("Company:", loggedincompany()), xref="paper",x=0.9,
#                              yref="paper",y=1,showarrow=FALSE))
# })


###### e reciept tab 

output$companyname_e_receipt <- renderValueBox({
  
  
  name <- loggedincompany()
  
  
  valueBox(
    paste("Showing Results For:", name),  #name
    ""
  )
})

# output$total_e_receipt_issued <- renderValueBox({
# 
#   total_e_receipt <- e_receipt_daily %>%
#     filter(company_name == loggedincompany(), mrdate %in% seq(input$daterange_e_receipt[1],
#                                                             input$daterange_e_receipt[2], by = 'day')) %>%
#     ungroup() %>%
#     summarise(n = sum(total)) %>% pull(n)
# 
# 
#   valueBox(
#     #compress(total_e_receipt),
#     format(total_e_receipt, big.mark = ",", scientific = F),
#     paste(compress(total_e_receipt),"Total E-Receipt" ) ,
#     color = "green",
#     icon = tags$i(class = "fa fa-reciept", style="font-size: 44px; color: #fff")
#   )
# })


output$total_e_receipt_issued <- renderUI({

  total_e_receipt <- e_receipt_daily %>%
    filter(company_name == loggedincompany(), mrdate %in% seq(input$daterange_e_receipt[1],
                                                              input$daterange_e_receipt[2], by = 'day')) %>%
    ungroup() %>%
    summarise(n = sum(total)) %>% pull(n)

  sub <- paste(compress(total_e_receipt),"Total E-Receipt")

  market_share <- scales::percent(total_e_receipt/ (e_receipt_daily %>%
                                                 filter(mrdate %in% seq(input$daterange_e_receipt[1],
                                                                        input$daterange_e_receipt[2], by = 'day')) %>%
                                                 ungroup() %>%
                                                 summarise(n = sum(total)) %>% pull(n)), accuracy = 0.01)





  render_my_box_with_change(format(total_e_receipt, big.mark = ",", scientific = F), subtitle = sub,
                            changeText = paste(market_share, "of total market"),
                            icon = icon("hand-holding-usd"), bgcolor = COL_GREEN, textcolor = "#fff")
})


output$linechart_e_receipt <- renderHighchart({
  
  
   e_receipt_daily %>%
    filter(company_name == loggedincompany(), mrdate >= input$daterange_e_receipt[1], mrdate < input$daterange_e_receipt[2]) %>% 
  
  
  hchart(
    'line', hcaes(x = mrdate, y = total, color = total)
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
  
  # plot_ly(data, x = ~mrdate, y = ~total, mode = "line", hoverinfo = "text", text = ~paste("Date:", mrdate,"(", weekdays(as.Date(mrdate)),")", "<br>", "E-Receipts Issued:", data$total)) %>%
  #   layout(
  #     
  #     annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Date"),
  #     yaxis = list(title = "Total E-Receipt Issued")
  #   )
})


output$monthlychart_e_receipt <- renderHighchart({
  
  
  total_by_month <- e_receipt_monthly %>% filter(company_name == loggedincompany(), year_r == input$selectyear_e_receipt)
  
  
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
  #   layout(
  #     
  #     annotations=list(text=paste("Year: ", input$selectyear_e_receipt, "<br>", "Company:", loggedincompany()),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Month"),
  #     yaxis = list(title = "Total")
  #   )
})

output$yearlychart_e_receipt <- renderHighchart({
  
  
  e_receipt_monthly %>% filter(company_name == loggedincompany()) %>%
    group_by(year_r) %>% summarise(total = sum(total, na.rm = T)) %>% 
  
  hchart(
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
  
  # plot_ly(total_by_month, x = ~factor(year_r), y = ~total, type = "bar", hoverinfo ="y") %>%
  #   layout(
  #     
  #     annotations=list(text=paste("Company:", loggedincompany()),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Year"),
  #     yaxis = list(title = "Total")
  #   )
})


# interactive datatable for branch from 'mr' data
agenthwisedt_reactive <- reactive({
  agentwisetotalprem %>% filter(company_name == loggedincompany(), 
                                mrdate >= input$daterange_agent_dt[1], mrdate <= input$daterange_agent_dt[2]) %>%
    group_by(company_name, agentid) %>% 
    summarise(Total_E_Receipt= sum(Total_E_Receipt, na.rm = T), Total_Premium = sum(total, na.rm = T))
})

output$agentdt_e_receipt <- renderDataTable(
  datatable(agenthwisedt_reactive()) %>% formatCurrency(columns = 4,currency = "BDT ", mark = ","),
  server = FALSE,
  extensions = 'Buttons',
  options = list(order = list(list(4, 'desc')), dom= 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                 
                 initComplete = JS(
                   "function(settings, json) {",
                   "$('.dt-button').css({'background': '#2ECC71','color':'#fff','border':'none', 'borderRadius':'10px','fontWeight':'700'});",
                   "$('input[type=search]').css({'border':'1.5px solid #F1C40F', 'borderRadius':'10px'});",
                   "$('.box-header').css({'textAlign': 'center'});",
                   "$('h3').css({'fontWeight':'700'});",
                   "$('label').css({'color':'red'});",
                   "$('td').css({'border':'none'});",
                   "}"))
)





## policy + covernote tab 

output$info_policycovercount <- renderValueBox({
  
  
  valueBox(
    paste0("Non-Life Insurance Industry Status on: ", today()),
    ""
  )
})

# output$policy_cover_active <- renderValueBox({
#   
#   active_policy <- non_life_data_sum_table %>% filter(company_name == loggedincompany()) %>%
#     pull(activepolicy)
#   
#   valueBox(
#     format(active_policy, big.mark = ",", scientific = F),
#     paste(compress(active_policy),"Active Policy" ) ,
#     color = "green"
#     
#     
#   )
# })

output$policy_cover_active <- renderUI({
  
  active_policy <- non_life_data_sum_table %>% filter(company_name == loggedincompany()) %>%
    pull(activepolicy)
  
  sub <-  paste(compress(active_policy),"Active Policy" )
  
  market_share <- scales::percent(active_policy/ (non_life_data_sum_table %>%
                                                    pull(activepolicy) %>% sum()), accuracy = 0.01)
  
  
  
  
  
  render_my_box_with_change(format(active_policy, big.mark = ",", scientific = F), subtitle = sub,
                            changeText = paste(market_share, "of total market"),
                            icon = icon("pencil-ruler"), bgcolor = COL_GREEN, textcolor = "#fff")
})


# output$policy_cover_total <- renderValueBox({
#   
#   total_policy <- non_life_data_sum_table %>% filter(company_name == loggedincompany()) %>%
#     pull(totalpolicy)
#   
#   valueBox(
#     format(total_policy, big.mark = ",", scientific = F),
#     paste(compress(total_policy),"Total Policy" ) ,
#     color = "green",
#     icon = tags$i(class = "fa fa-pencil-ruler", style="font-size: 44px; color: #fff")
#     
#   )
# })


output$policy_cover_total <- renderUI({
  
  total_policy <- non_life_data_sum_table %>% filter(company_name == loggedincompany()) %>%
    pull(totalpolicy)
  
  sub <-  paste(compress(total_policy),"Total Policy" )
  
  market_share <- scales::percent(total_policy/ (non_life_data_sum_table %>%
                                                   pull(totalpolicy) %>% sum()
  ), accuracy = 0.01)
  
  
  
  
  
  render_my_box_with_change(format(total_policy, big.mark = ",", scientific = F), subtitle = sub,
                            changeText = paste(market_share, "of total market"),
                            icon = icon("pencil-ruler"), bgcolor = COL_GREEN, textcolor = "#fff")
})






################################################
############ FORECAST ##########################
################################################



#### nonlife  forecast button
# forecast_button_apply <- reactive(switch (input$nonlife_forecast_product_type_select,
#                                                  "Policy" = {
#                                                    monthly_policy <- nonlife_policy_forecast_data[[loggedincompany()]]
#                                                    policy_forecast_object <- nonlife_policy_forecast_object[[loggedincompany()]]
# 
#                                                    FORECAST_PERIOD <- as.numeric(input$nonlife_select_period)
# 
#                                                    full_data <- monthly_policy %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)
# 
#                                                    future_data <- full_data %>% filter(
#                                                      is.na(cnt)
#                                                    )
# 
#                                                    policy_forecast_object %>%
#                                                      modeltime_calibrate(monthly_policy)%>%
#                                                      modeltime_forecast(new_data = future_data,
#                                                                         actual_data = monthly_policy,
#                                                                         keep_data = T
#                                                      ) %>%
#                                                      plot_modeltime_forecast(.title = "Monthly Policy Forecast Plot")
#                                                  },
#                                                  "Premium" = {
#                                                    
#                                                    monthly_premium <- nonlife_prem_forecast_data[[loggedincompany()]]
#                                                    prem_forecast_obj <- nonlife_prem_forecast_object[[loggedincompany()]]
#                                                    FORECAST_PERIOD <- as.numeric(input$nonlife_select_period)
#                                                    full_data <- monthly_premium %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)
#                                                    future_data <- full_data %>% filter(
#                                                      is.na(cnt)
#                                                    )
#                                                    
#                                                    prem_forecast_obj %>% 
#                                                      modeltime_calibrate(monthly_premium)%>%
#                                                      modeltime_forecast(new_data = future_data, 
#                                                                         actual_data = monthly_premium, 
#                                                                         keep_data = T
#                                                      ) %>%
#                                                      plot_modeltime_forecast(.title = "Monthly Premium Collection Forecast Plot")
#                                                    
#                                                  }
#                                          )) %>% bindCache(loggedinas(), input$nonlife_forecast_product_type_select, input$nonlife_select_period) %>% 
#                                               bindEvent(input$nonlife_forecastButton)
# 
# 
# output$nonlife_forecast_plot <- renderPlotly({
#   
#   forecast_button_apply()
#   
# })





# output$nonlife_trend_plot <- renderPlotly({
#   
#   switch (input$nonlife_trend_type_select,
#           "Policy" = {
#             nonlife_policy_forecast_data[[loggedincompany()]] %>% plot_time_series(.date_var = date, .value = cnt,
#                                                                                 .title = "Monlty Policy Issue Trend")
#           },
#           "Premium" = {
#             nonlife_prem_forecast_data[[loggedincompany()]] %>% plot_time_series(.date_var = date, .value = cnt,
#                                                                                    .title = "Monlty Premium Collection Trend")
#           }
#   )
# }) %>% bindCache(loggedinas(), input$nonlife_trend_type_select)









