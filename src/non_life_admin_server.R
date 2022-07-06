output$companyname_cp <- renderValueBox({
  if(input$selectinput == "All"){
    name <- "All Company"
  }
  else {
    name <- input$selectinput
  }
  
  valueBox(
    paste("Showing Results For:", name),
    ""
  )
})

## total policy count 

#for both active and total count
# x_coverpolicy_reactive <- reactive({
#     coverpolicy %>% select(covernotenumber, policynumber, coverageenddate, company_name)
# })
# 
output$total_policy_count <- renderUI({
  
  totalpolicycount <- if(input$selectinput == "All") {
    
    # x_coverpolicy <- x_coverpolicy_reactive()
    
    totalpolicycount <- daywisetotal_p_count %>% 
      filter( policyissuedate %in% seq(input$daterange_1.2[1],
                                       input$daterange_1.2[2], by = 'day')) %>%
      pull(total) %>% sum()
  }
  else {
    # x_coverpolicy <- x_coverpolicy_reactive() %>% filter(company_name == input$selectinput)
    
    totalpolicycount <- daywisetotal_p_count %>%
      filter(company_name == input$selectinput, policyissuedate %in% seq(input$daterange_1.2[1],
                                                                         input$daterange_1.2[2], by = 'day')) %>%
      pull(total) %>% sum()
  }
  
  
  sub <- (  paste(compress(totalpolicycount), "Total Policy Count"))
  
  render_my_box(value = format(totalpolicycount, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #780206, #061161)", textcolor = "#fff", icon = "total_policy.jpg")
  
  # valueBox(
  #   totalpolicycount %>% format(big.mark = ",", scientific = F),
  #   paste(compress(totalpolicycount),"Total Policy Count"),color = "purple",
  #   icon = tags$i(class = "fa fa-pencil-ruler", style="font-size: 44px; color: #fff")
  # )
  
})



# ## total active policy 
# 
# output$total_active_policy_count <- renderValueBox({
#     
#     if(input$selectinput == "All") {
#         
#         # x_coverpolicy <- x_coverpolicy_reactive() 
#         totalactive <- total_active_policy_count_sum %>%
#             summarise(sum(total))
#         
#         # totalactive <- x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% filter(coverageenddate > today()) %>% pull(policynumber) %>% unique() %>% length() +
#         #     x_coverpolicy %>% filter(!is.na(covernotenumber), is.na(policynumber), coverageenddate > today(), !covernotenumber %in% (x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% pull(covernotenumber))) %>%
#         #     pull(covernotenumber) %>% tolower() %>% unique() %>% length() +
#         #     
#         #     x_coverpolicy %>% filter(is.na(covernotenumber), !is.na(policynumber), !policynumber %in% (x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% pull(policynumber)), coverageenddate > today()) %>%
#         #     pull(policynumber) %>% unique() %>% length()
#         
#     }
#     else {
#         
#         # x_coverpolicy <- x_coverpolicy_reactive() %>% filter(company_name == input$selectinput)
#         
#         totalactive <- total_active_policy_count_sum %>%
#             filter(company_name == input$selectinput) %>%
#             summarise(sum(total))
#     }
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

output$total_expired_policy_count <- renderUI({
  totalexpired <- if(input$selectinput == "All"){
    totalexpired <- totalexpired_sum %>% 
      filter(policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
      ungroup() %>%
      pull(total) %>% sum()
  }
  else {
    totalexpired <- totalexpired_sum %>%
      filter(company_name == input$selectinput, 
             policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>% 
      ungroup() %>%
      pull(total) %>% sum()
  }
  
  sub <- (  paste(compress(totalexpired), "Total Expired Policy Count"))
  
  render_my_box(value = format(totalexpired, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #000000, #e74c3c)", textcolor = "#fff", icon = "expired.jpg")
  
  # valueBox(
  #   totalexpired %>% format(big.mark = ",", scientific = F),
  #   paste(compress(totalexpired),"Total Expired Policy Count"),color = "purple",
  #   icon = tags$i(class = "fa fa-hourglass-end", style="font-size: 44px; color: #fff")
  # )
  
})

#Total Cancelled Policy Count:
# totalcancelled_reactive <- reactive({
#     coverpolicy %>% filter (!is.na(coverpolicy$cancelledcover) | !is.na(coverpolicy$cancelledpolicy)) 
# })

output$total_cancelled_policy_count <- renderUI({
  totalcancelled <- if(input$selectinput == "All"){
    totalcancelled <- totalcancelled_sum %>% 
      filter(policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
      ungroup() %>%
      pull(total) %>% sum() 
  }
  else {
    totalcancelled <- totalcancelled_sum %>%
      filter(company_name == input$selectinput, 
             policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
      ungroup() %>%
      pull(total) %>% sum() 
  }
  
  sub <- (  paste(compress(totalcancelled), "Total Cancelled Policy Count"))
  
  render_my_box(value = format(totalcancelled, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #4b1248, #f0c27b)", textcolor = "#fff", icon = "cancelled.jpg")
  
  # valueBox(
  #   totalcancelled %>% format(big.mark = ",", scientific = F),
  #   paste(compress(totalcancelled),"Total Cancelled Policy Count"),color = "purple",
  #   icon = tags$i(class = "fa fa-cut", style="font-size: 44px; color: #fff")
  # )
  
})

# # output$companyname_cover_count <- renderValueBox({
# #   if(input$selectinput == "All"){
# #     name <- "All Company"
# #   }
# #   else {
# #     name <- input$selectinput
# #   }
# #   
# #   valueBox(
# #     paste("Showing Results For:", name),
# #     ""
# #   )
# # })
# # 
# # 
# # #total cover count
# # output$total_cover_count <- renderValueBox({
# #   
# #   if(input$selectinput == "All") {
# #     
# #     
# #     
# #     totalcovercount <- daywisetotal_c_count %>% filter(covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>% pull(total) %>% sum()
# #   }
# #   else {
# #     totalcovercount <- daywisetotal_c_count %>% 
# #       filter(company_name == input$selectinput, covernoteissuedate >= input$daterange_1.1[1], 
# #              covernoteissuedate <= input$daterange_1.1[2]) %>% pull(total) %>% sum()
# #   }
#   
#   
#   valueBox(
#     totalcovercount %>% format(big.mark = ",", scientific = F),
#     paste(compress(totalcovercount),"Total Covernote Count"),color = "purple",
#     icon = tags$i(class = "fa fa-book-open", style="font-size: 44px; color: #fff")
#   )
# })

#total cancelled cover
# output$total_cancelled_cover_count <- renderValueBox({
#   if(input$selectinput == "All"){
#     totalcancelled <- totalcancelled_cover_sum %>%
#       filter(covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>%
#       ungroup() %>%
#       pull(total) %>% sum()
#   }
#   else {
#     totalcancelled <- totalcancelled_cover_sum %>%
#       filter(company_name == input$selectinput, covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>%
#       ungroup() %>%
#       pull(total) %>% sum() 
#   }
#   
#   valueBox(
#     totalcancelled %>% format(big.mark = ",", scientific = F),
#     paste(compress(totalcancelled),"Total Cancelled Covernote"),color = "purple",
#     icon = tags$i(class = "fa fa-cut", style="font-size: 44px; color: #fff")
#   )
# })
# 
# 
# ## total cover policy 
# 
# # output$total_active_cover_count <- renderValueBox({
# #     
# #     if(input$selectinput == "All") {
# #         
# #         totalactive <- x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% filter(coverageenddate > today()) %>% pull(policynumber) %>% unique() %>% length() +
# #             x_coverpolicy %>% filter(!is.na(covernotenumber), is.na(policynumber), coverageenddate > today(), !covernotenumber %in% (x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% pull(covernotenumber))) %>%
# #             pull(covernotenumber) %>% tolower() %>% unique() %>% length() +
# #             
# #             x_coverpolicy %>% filter(is.na(covernotenumber), !is.na(policynumber), !policynumber %in% (x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% pull(policynumber)), coverageenddate > today()) %>%
# #             pull(policynumber) %>% unique() %>% length()
# #         
# #     }
# #     else {
# #         
# #         x_coverpolicy <- x_coverpolicy_reactive() %>% filter(company_name == input$selectinput)
# #         
# #         totalactive <- x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% filter(coverageenddate > today()) %>% pull(policynumber) %>% unique() %>% length() +
# #             x_coverpolicy %>% filter(!is.na(covernotenumber), is.na(policynumber), coverageenddate > today(), !covernotenumber %in% (x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% pull(covernotenumber))) %>%
# #             pull(covernotenumber) %>% tolower() %>% unique() %>% length() +
# #             
# #             x_coverpolicy %>% filter(is.na(covernotenumber), !is.na(policynumber), !policynumber %in% (x_coverpolicy %>% filter(!is.na(covernotenumber), !is.na(policynumber)) %>% pull(policynumber)), coverageenddate > today()) %>%
# #             pull(policynumber) %>% unique() %>% length()
# #     }
# #     
# #     valueBox(
# #         totalactive,
# #         paste(compress(totalactive),"Total Active Covernote Count"),color = "purple"
# #     )
# # }
# # )
# 
# 
# 
# 
# 
# #Daywise Total Covernote Issue Chart
# 
# # daywisetotal_c_count_reactive <- reactive({
# #     coverpolicy_note %>%
# #         filter(covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>%
# #         group_by(covernoteissuedate) %>% 
# #         summarise(total = n())
# # })
# 
# output$linechart_day_1.1 <- renderPlotly({
#   
#   if(input$selectinput == "All"){
#     data <- daywisetotal_c_count %>% filter(covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>%
#       group_by(covernoteissuedate) %>% 
#       summarise(total = sum(total, na.rm = T))
#     
#   }
#   else {
#     data <- daywisetotal_c_count %>%
#       filter(company_name == input$selectinput, covernoteissuedate >= input$daterange_1.1[1], covernoteissuedate <= input$daterange_1.1[2]) %>%
#       group_by(covernoteissuedate) %>% 
#       summarise(total = sum(total, na.rm = T))
#   }
#   
#   plot_ly(data, x = ~covernoteissuedate, y = ~total, mode = "line", hoverinfo= "text", text = ~paste0("Date: ", data$covernoteissuedate," (", weekdays(as.Date(covernoteissuedate)),")", "<br>", "Covernote Issued: ", data$total)) %>%
#     layout(
#       annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
#                        yref="paper",y=0.9,showarrow=FALSE),
#       xaxis = list(title = "Date"),
#       yaxis = list(title = "Total Covernote Issued")
#     )
# })
# 
# 
# #Monthwise Total Covernote Issue Chart
# 
# # monthwisetotal_c_count_reactive <-reactive({
# #     coverpolicy_note %>% filter(year_c == input$selectyear_1.2) %>% 
# #         group_by(month_c) %>% 
# #         summarise(total = n())
# # })
# 
# output$linechart_month_1.2 <- renderPlotly({
#   
#   if(input$selectinput == "All"){
#     total_by_month <- monthwisetotal_c_count %>% filter(year_c == input$selectyear_1.2) %>%
#       group_by(month_c) %>% 
#       summarise(total = sum(total, na.rm = T))
#   }
#   else {
#     total_by_month <- monthwisetotal_c_count %>% 
#       filter(year_c == input$selectyear_1.2, company_name == input$selectinput) %>% 
#       group_by(month_c) %>%
#       summarise(total = sum(total, na.rm = T))
#   }
#   
#   
#   plot_ly(total_by_month, x = ~month_c, y = ~total, hoverinfo ="y") %>%
#     layout(
#       
#       annotations=list(text=paste("Year: ", input$selectyear_1.2, "<br>", "Company:", input$selectinput), xref="paper",x=0.5,
#                        yref="paper",y=0.9,showarrow=FALSE
#       ),
#       xaxis = list(title = "Month"),
#       yaxis = list(title = "Total Covernote Issued")
#     )
# })
# 
# #Year wise Total Covernote Issue Chart
# 
# # yearwisetotal_c_count_reactive <- reactive({
# #     coverpolicy_note %>% group_by(year_c) %>% summarise(total = n())
# # })
# # 
# output$linechart_year_1.3 <- renderPlotly({
#   if(input$selectinput == "All"){
#     total_by_year <- monthwisetotal_c_count %>% 
#       group_by(year_c) %>% 
#       summarise(total = sum(total, na.rm = T))
#   }
#   else {
#     total_by_year <- monthwisetotal_c_count %>% 
#       filter(company_name == input$selectinput) %>% group_by(year_c) %>% 
#       summarise(total = sum(total, na.rm = T))
#   }
#   
#   plot_ly(total_by_year %>% filter(year_c <= input$yearrange_1.3[2], year_c >= input$yearrange_1.3[1]), x = ~factor(year_c), y = ~total, type = "bar", hoverinfo ="y") %>%
#     layout(
#       
#       annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
#                        yref="paper",y=0.9,showarrow=FALSE),
#       xaxis = list(title = "Year"),
#       yaxis = list(title = "Total Covernote Issued")
#     )
#   
# })


#interactive datatable for agentwise cover performance
# agentwisedt_note_reactive <- reactive({
#     coverpolicy_note %>% group_by(company_name, agentid) %>%
#         filter(!is.na(agentid), agentid != "N/A") %>% 
#         summarise(Total_Covernote = n())
# })
# 
# output$agentdt_note <- renderDataTable(
#     agentwisedt_note_reactive(),
#     server = FALSE,
#     extensions = 'Buttons',
#     options = list(order = list(list(3, 'desc')), dom= 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
# )
# 

#Day wise Total Policy Issue Chart

# daywisetotal_p_count_reactive <- reactive({
#     coverpolicy_policy %>%
#         filter(policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
#         group_by(policyissuedate) %>% 
#         summarise(total = n())
# })
# 

output$barchart_monthly_policycount <- renderHighchart({

  if(input$selectinput == "All"){
    total_by_month <- monthwisetotal_p_count %>% filter(year_p == input$selectyear_1.5) %>%
      group_by(month_p) %>%
      summarise(total = sum(total, na.rm = T))
  }
  else {
    total_by_month <- monthwisetotal_p_count %>%
      filter(year_p == input$selectyear_1.5, company_name == input$selectinput) %>%
      group_by(month_p) %>%
      summarise(total = sum(total, na.rm = T))
  }

  hchart(total_by_month,
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
  #     annotations=list(text=paste("Year: ", input$selectyear_1.5, "<br>", "Company:", input$selectinput), xref="paper",x=0.5,
  #                      yref="paper",y=1.0,showarrow=FALSE),
  #     xaxis = list(title = "Month"),
  #     yaxis = list(title = "Total Policy Issued")
  #   )
})

#Year wise Total Policy Issue Chart

# yearwisetotal_p_count_reactive <- reactive({
#     coverpolicy_policy %>% group_by(year_p) %>% summarise(total = n())
# })
# 
output$barchart_yearly_policycount <- renderHighchart({
  if(input$selectinput == "All"){
    total_by_year <- monthwisetotal_p_count %>% filter(year_p <= input$yearrange_1.6[2], year_p >= input$yearrange_1.6[1]) %>% 
      group_by(year_p) %>% 
      summarise(total = sum(total, na.rm = T))
  }
  else {
    total_by_year <- monthwisetotal_p_count %>% 
      filter(company_name == input$selectinput, year_p <= input$yearrange_1.6[2], year_p >= input$yearrange_1.6[1]) %>% 
      group_by(year_p) %>% 
      summarise(total = sum(total, na.rm = T))
  }
  
  hchart(total_by_year,
    'column', hcaes(x = year_p, y = total, color = total)
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
  #     annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
  #                      yref="paper",y=1.0,showarrow=FALSE),
  #     xaxis = list(title = "Year"),
  #     yaxis = list(title = "Total Policy Issued")
  #   )
})

#interactive datatable for agentwise policy performance
# agentwisedt_policy_reactive <- reactive({
#     coverpolicy_policy %>% group_by(company_name, agentid) %>%
#         filter(!is.na(agentid), agentid != "N/A") %>% 
#         summarise(Total_Policy = n())
# })
# 
# 
# output$agentdt_policy <- renderDataTable(
#     agentwisedt_policy_reactive(),
#     server = FALSE,
#     extensions = 'Buttons',
#     options = list(order = list(list(3, 'desc')), dom= 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
# )

########################               Sidebar 2 Server Code              ##################################

###Net Premium Collection Value Box
output$companyname_mr <- renderValueBox({
  if(input$selectinput == "All"){
    name <- "All Company"
  }
  else {
    name <- input$selectinput
  }
  
  valueBox(
    paste("Showing Results For:", name),
    ""
  )
})

###Premium Collection Report
#Net Premium Collection in Amount
# total_prem_reactive <- reactive({
#     sum((mr %>%
#              filter(mrdate >= input$daterange_2[1], 
#                     mrdate <= input$daterange_2[2]))$totalpremium, na.rm = T)
# })

output$total_prem <- renderUI({
  total_prem <- if(input$selectinput == "All"){
    total_prem <- daywisetotalprem %>% 
      filter(mrdate %in% seq(input$daterange_2[1],
                             input$daterange_2[2], by = 'day')) %>%
      pull(total) %>% 
      sum()
  }
  else {
    total_prem <- daywisetotalprem %>% 
      filter(company_name == input$selectinput, mrdate %in% seq(input$daterange_2[1],
                                                                input$daterange_2[2], by = 'day')) %>%
      pull(total) %>% 
      sum() 
    
    
  }
  
  sub <- (  paste(compress(total_prem), "Net Premium Collection in Amount"))
  
  render_my_box(value = format(total_prem, big.mark = ",", scientific = F), subtitle = sub, class = "col-12 p-2",
                bgcolor = "linear-gradient(to right, #556270, #ff6b6b)", textcolor = "#fff", icon = "first_prem_reciept.jpg")
  
  # valueBox(
  #   format(total_prem, big.mark = ",", scientific = F),
  #   paste(compress(total_prem), "Net Premium Collection in Amount"),color = "maroon",
  #   icon = tags$i(class = "fa fa-hand-holding-usd", style="font-size: 44px; color: #fff")
  # )
})

#Daywise Net Premium Collection Chart

# daywisetotalprem_reactive <- reactive({
#     mr %>%
#         filter(mrdate >= input$daterange_2[1], mrdate <= input$daterange_2[2]) %>%
#         group_by(mrdate) %>% 
#         summarise(total = sum(totalpremium, na.rm = T))
# })

output$linechart_day <- renderHighchart({
  
  if(input$selectinput == "All"){
    data <- daywisetotalprem %>% 
      filter(mrdate >= input$daterange_2[1], mrdate <= input$daterange_2[2]) %>%
      group_by(mrdate) %>% 
      summarise(total = sum(total, na.rm = T))
    
  }
  else {
    data <- daywisetotalprem %>%
      filter(company_name == input$selectinput, mrdate >= input$daterange_2[1], mrdate <= input$daterange_2[2]) %>%
      group_by(mrdate) %>% 
      summarise(total = sum(total, na.rm = T))
  }
  
  hchart(data,
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
  # plot_ly(data, x = ~mrdate, y = ~total, mode = "line", hoverinfo= "text", text = ~paste0("Date: ", data$mrdate," (", weekdays(as.Date(mrdate)),")", "<br>", "Premium Collection: ", data$total, "Taka")) %>%
  #   layout(
  #     
  #     annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Date"),
  #     yaxis = list(title = "Net Premium (in Taka)")
  #   )
})

output$linechart_policycount <- renderHighchart({
  if(input$selectinput == "All"){
    data <- daywisetotal_p_count %>%
      filter(policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
      group_by(policyissuedate) %>%
      summarise(total = sum(total, na.rm = T))
    
  }
  else {
    data <- daywisetotal_p_count %>%
      filter(company_name == input$selectinput, policyissuedate >= input$daterange_1.2[1], policyissuedate <= input$daterange_1.2[2]) %>%
      group_by(policyissuedate) %>%
      summarise(total = sum(total, na.rm = T))
  }
  
  
  hchart(data,
         'line', hcaes(x = policyissuedate, y = total, color = total)) %>%
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
})


output$linechart_month <- renderHighchart({
  
  if(input$selectinput == "All"){
    totalprem_by_month <- monthwisetotalprem %>% 
      filter(year_r == input$selectyear_p) %>%
      group_by(month_r) %>% 
      summarise(total = sum(total, na.rm = T))
  }
  else {
    totalprem_by_month <- monthwisetotalprem %>% 
      filter(year_r == input$selectyear_p, company_name == input$selectinput) %>% 
      group_by(month_r) %>% 
      summarise(total = sum(total, na.rm = T))
  }
  
  hchart(totalprem_by_month,
    'column', hcaes(x = month_r, y = total, color = total)
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
  #     annotations=list(text=paste("Year: ", input$selectyear_p, "<br>", "Company:", input$selectinput),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Month"),
  #     yaxis = list(title = "Total (in Taka)")
  #     
  #   )
})


output$linechart_year <- renderHighchart({
  if(input$selectinput == "All"){
    totalprem_by_year <- monthwisetotalprem %>% filter(year_r <= input$yearrange_p[2], year_r >= input$yearrange_p[1]) %>% 
      group_by(year_r) %>% 
      summarise(total = sum(total, na.rm = T))
  }
  else {
    totalprem_by_year <- monthwisetotalprem %>% 
      filter(company_name == input$selectinput, year_r <= input$yearrange_p[2], year_r >= input$yearrange_p[1]) %>% 
      group_by(year_r) %>% 
      summarise(total = sum(total, na.rm = T))
  }
  
  hchart(totalprem_by_year,
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
  
  # plot_ly(totalprem_by_year %>% filter(year_r <= input$yearrange_p[2], year_r >= input$yearrange_p[1]), x = ~factor(year_r), y = ~total, type = "bar", hoverinfo= "text", text = ~paste(total, "Taka")) %>%
  #   layout(
  #     
  #     annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Year"),
  #     yaxis = list(title = "Total (in Taka)")
  #   )
  
})

### Interactive Comparison of the Companies

# reactive filter data for comparison charts that changes on button click

compare_reactive <- eventReactive(input$compare_button, {daywisetotalprem %>%
    filter(company_name %in% input$linechart_company_picker)})

#Day wise Premium Collection Comparison
output$linechart_p_comparison <- renderPlotly({
  data <- data.frame(compare_reactive() %>%
                       filter(mrdate %in% seq(input$daterange_p_comparison[1],
                                              input$daterange_p_comparison[2], by = 'day')) %>%
                       group_by(mrdate, company_name) %>% 
                       summarise(total = sum(total, na.rm = T)))
  
  plot_ly(data, x = ~mrdate, y = ~total) %>%
    add_lines(text = ~company_name, hovertemplate = '%{text}: %{y:.2f} Taka<extra></extra>', color = ~company_name) %>%
    layout(
      hovermode = "x unified",
      xaxis = list(title = "Date"),
      yaxis = list(title = "Net Premium (in Taka)")
    )
  
})

#Month wise Premium Collection Comparison
compare_reactive_month <- eventReactive(input$compare_button, {monthwisetotalprem %>%
    filter(company_name %in% input$linechart_company_picker)})

output$monthlychart_premium_comparison <- renderPlotly({
  total_by_month <- data.frame(compare_reactive_month() %>% 
                                 filter(year_r == input$selectyear_premium_comparison) %>% 
                                 group_by(month_r, company_name) %>% summarise(total = sum(total, na.rm = T)))
  
  plot_ly(total_by_month, x = ~month_r, y = ~total) %>% 
    add_lines(text = ~company_name, hovertemplate = '%{text}: %{y:.2f} Taka<extra></extra>', color = ~company_name) %>% add_markers(hoverinfo = "none", color = ~company_name, showlegend = FALSE) %>%
    layout(
      hovermode = "x unified",
      xaxis = list(title = "Month"),
      yaxis = list(title = "Total (in Taka)")
      
    )
  
})

# interactive datatable for branch from 'mr' data
# branchwisedt_reactive <- reactive({
#     mr %>% group_by(company_name, officebranchcode) %>% 
#         summarise(Total_Transaction= n(), Total_Premium = sum(totalpremium, na.rm = T))
# })
# 
# output$branchdt <- renderDataTable(
#     branchwisedt_reactive(),
#     server = FALSE,
#     extensions = 'Buttons',
#     options = list(order = list(list(4, 'desc')), dom= 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
# )
# 


#####################################  Sidebar 3 Server Code   #####################################
###Analytical Comparison Report

output$companyname_nonlife <- renderValueBox({
  if(input$selectinput == "All"){
    name <- "All Company"
  }
  else {
    name <- input$selectinput
  }
  
  valueBox(
    paste("Showing Results For:", name),
    ""
  )
})

#Total Bank Deposited Policy Count:
# totalbankdep_reactive <- reactive({
#     mr %>% mutate (modeofpayment_updated = ifelse (grepl("^P|ORDER|^ON|^I|BANK|^D|^TRANS|DRAFT|^CH|CHQ|CEQ|CHA|CREDIT|D.D", modeofpayment, ignore.case = TRUE),
#                                                    "Bank Deposited",
#                                                    "Others")) %>% 
#         filter (modeofpayment_updated == "Bank Deposited", mrdate >= input$daterange_3.1[1], mrdate <= input$daterange_3.1[2]) 
# })

output$bank_deposited_policy <- renderUI({
  totalbankdep <- if(input$selectinput == "All"){
    totalbankdep <- totalbankdep_sum %>%
      filter (modeofpayment_updated == "Bank Deposited") %>%
      ungroup() %>%
      pull(total) %>% sum()
  }
  else {
    totalbankdep <- totalbankdep_sum %>%
      filter (company_name == input$selectinput, modeofpayment_updated == "Bank Deposited") %>%
      ungroup() %>%
      pull(total) %>% sum()
  }
  
  sub <- (  paste(compress(totalbankdep), "Total Bank Deposited Policy Count"))
  
  render_my_box(value = format(totalbankdep, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #000000, #e74c3c)", textcolor = "#fff", icon = "bank_deposit_policy.jpg")
  
  # valueBox(
  #   totalbankdep %>% format(big.mark = ",", scientific = F),
  #   paste(compress(totalbankdep),"Total Bank Deposited Policy Count"),color = "purple",
  #   icon = tags$i(class = "fa fa-layer-group", style="font-size: 44px; color: #fff")
  # )
  
})

## valid mobile

# totalvalidmobile_reactive <- reactive({
#   mobile <- coverpolicy %>% pull(mobilenumber)
#   valid_mobile <- str_match(mobile, regex("01[3-9][0-9]{8}"))
#   length(valid_mobile[!is.na(valid_mobile)])
# })

output$valid_mobile_number <- renderUI({
  totalvalidmobile <- if(input$selectinput == "All"){
    # mobile <- coverpolicy  %>% pull(mobilenumber)
    # valid_mobile <- str_match(mobile, regex("01[3-9][0-9]{2}[-]?[0-9]{6}"))
    totalvalidmobile <- totalvalidmobile_sum_all
    validmobile_percent <- validmobile_percent_sum_all
    
    
  }
  else {
    # mobile <- coverpolicy %>% filter(company_name == input$selectinput) %>% pull(mobilenumber)
    # valid_mobile <- str_match(mobile, regex("01[3-9][0-9]{8}"))
    totalvalidmobile <- totalvalidmobile_sum[totalvalidmobile_sum$company_name == input$selectinput,]$totalvalidmobile
    validmobile_percent <- totalvalidmobile_sum[totalvalidmobile_sum$company_name == input$selectinput,]$validmobile_percent
  }
  
  sub <- (  paste(totalvalidmobile, "Total Valid Mobile Number"))
  
  render_my_box(value = format(totalvalidmobile, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #5d4157, #a8caba)", textcolor = "#fff", icon = "valid_mobile.jpg")
  
  # valueBox(
  #   paste0(totalvalidmobile, " (", validmobile_percent, ")"),
  #   "Total Valid Mobile Number",
  #   icon = tags$i(class = "fa fa-phone-square", style="font-size: 44px; color: #fff")
  # )
  
})


## valid email value box

output$valid_email_address <- renderUI({
  
  totalvalidemail <- if(input$selectinput == "All"){
    # email <- coverpolicy %>% pull(email)
    # valid_email <- str_match(email, regex("\\S+@\\S+\\.\\S+"))
    # totalvalidemail <- length(valid_email[!is.na(valid_email)])
    # validemail_percent <- paste0(round(totalvalidemail / length(email) * 100 , 2),"%")
    totalvalidemail <- totalvalidemail_sum_all
    validemail_percent <- validemail_percent_sum_all
  }
  else {
    # email <- coverpolicy %>% filter(company_name == input$selectinput) %>% pull(email)
    # valid_email <- str_match(email, regex("\\S+@\\S+\\.\\S+"))
    # totalvalidemail <- length(valid_email[!is.na(valid_email)])
    # validemail_percent <- paste0(round(totalvalidemail / length(email) * 100 , 2),"%")
    totalvalidemail <- valid_email_address_sum[valid_email_address_sum$company_name == input$selectinput,]$totalvalidemail
    validemail_percent <- valid_email_address_sum[valid_email_address_sum$company_name == input$selectinput,]$validemail_percent
  }
  
  sub <- (  paste(totalvalidemail, "Total Valid Email Address"))
  
  render_my_box(value = format(totalvalidemail, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #4b1248, #f0c27b)", textcolor = "#fff", icon = "valid_email.jpg")
  
  # valueBox(
  #   paste0(totalvalidemail, " (", validemail_percent, ")"),
  #   "Total Valid Email Address",
  #   icon = tags$i(class = "fa fa-mail-bulk", style="font-size: 44px; color: #fff")
  # )
  
})


output$policydate_vs_number_value <- renderUI({
  t <- if(input$selectinput == "All"){
    t <- policynumber_vs_issuedate %>% 
      ungroup() %>%
      summarise(n = sum(total)) %>% pull(n)
  }
  else {
    t <- policynumber_vs_issuedate %>%
      filter (company_name == input$selectinput) %>%
      ungroup() %>%
      summarise(n = sum(total)) %>% pull(n)
  }
  
  sub <- (  paste(compress(t), "Policy Issue Dates Missing but Policynumber Present"))
  
  render_my_box(value = format(t, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #061700, #52c234)", textcolor = "#fff", icon = NULL)
  
  # valueBox(
  #   t,
  #   paste(compress(t),"Policy Issue Dates Missing but Policynumber Present"),color = "maroon",
  # 
  # )
  
})

output$covernote_present_policy_absent <- renderUI({
  t <- if(input$selectinput == "All"){
    t <- covernote_present_policy_absent %>% 
      ungroup() %>%
      summarise(n = sum(total)) %>% pull(n)
  }
  else {
    t <- covernote_present_policy_absent %>%
      filter (company_name == input$selectinput) %>%
      ungroup() %>%
      summarise(n = sum(total)) %>% pull(n)
  }
  
  sub <- (  paste(compress(t), "Covernote Present but Policy issuedate Absent"))
  
  render_my_box(value = format(t, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #556270, #ff6b6b)", textcolor = "#fff", icon = "covernote.jpg")
  
  # valueBox(
  #   t,
  #   paste(compress(t),"Covernote Present but Policy issuedate Absent"),color = "maroon"
  # )
  
})

output$covernote_policy_both_present <- renderUI({
  t <- if(input$selectinput == "All"){
    t <- covernote_policy_both_present %>% 
      ungroup() %>%
      summarise(n = sum(total)) %>% pull(n)
  }
  else {
    t <- covernote_policy_both_present %>%
      filter (company_name == input$selectinput) %>%
      ungroup() %>%
      summarise(n = sum(total)) %>% pull(n)
  }
  
  
  sub <- (  paste(compress(t), "Both Covernote and Policy issuedate Present"))
  
  render_my_box(value = format(t, big.mark = ",", scientific = F), subtitle = sub, class = "col-md-4 p-2",
                bgcolor = "linear-gradient(to right, #780206, #061161)", textcolor = "#fff", icon = NULL)
  
  # valueBox(
  #   t,
  #   paste(compress(t),"Both Covernote and Policy issuedate Present")
  # )
  
})




###'insurancetype' Pie Chart

# insurancetype_reactive <- reactive({
#     coverpolicy %>% filter(insurancetype !="", covernoteissuedate >= input$daterange_insurancetype[1], covernoteissuedate <= input$daterange_insurancetype[2]) %>%
#         group_by(insurancetype) %>% summarize(counts = n(), percentage = n()/nrow(coverpolicy))
# })

output$insurancetypepiechart <- renderHighchart({
  
  if(input$selectinput == "All"){
    # insurancetypedata <- insurancetype_sum %>%
    #     filter(policyissuedate >= input$daterange_insurancetype[1], policyissuedate <= input$daterange_insurancetype[2]) %>% 
    #   group_by(insurancetype) %>%
    #   summarise(counts = sum(counts, na.rm = T),percentage = sum(counts, na.rm = T)/nrow(insurancetype_sum))
    
    insurancetypedata <- insurancetype_sum_bar %>%
      group_by(year_p, insurancetype) %>%
      summarise(counts = sum(counts, na.rm = T)) %>% mutate(percentage = prop.table(counts))
  }
  else {
    # insurancetypedata <- insurancetype_sum %>%
    #     filter(company_name == input$selectinput, policyissuedate >= input$daterange_insurancetype[1], 
    #            policyissuedate <= input$daterange_insurancetype[2]) %>%
    #   group_by(insurancetype) %>%
    #   summarise(counts = sum(counts, na.rm = T),percentage = sum(counts, na.rm = T)/nrow(insurancetype_sum))
    
    insurancetypedata <- insurancetype_sum_bar %>%
      filter(company_name == input$selectinput) %>%
      group_by(year_p, insurancetype) %>%
      summarise(counts = sum(counts, na.rm = T)) %>% mutate(percentage = prop.table(counts))
    
    
  }
  
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
  #         annotations=list(text=paste("Company:", input$selectinput), xref="paper",x=0.9,
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

# covernotetype datatable
output$insurancetype_dt <- renderDataTable(
  if(input$selectinput == "All"){
    insurancetype_sum %>%
      filter(covernoteissuedate %in% seq(input$daterange_insurancetype_dt[1],
                                         input$daterange_insurancetype_dt[2], by = 'day')) %>%
      group_by(insurancetype) %>%
      summarize(Covernote_Issued = sum(counts))
  }
  else {
    insurancetype_sum %>%
      filter(company_name == input$selectinput, covernoteissuedate %in% seq(input$daterange_insurancetype_dt[1],
                                                                            input$daterange_insurancetype_dt[2], by = 'day')) %>% 
      group_by(insurancetype) %>%
      summarize(Covernote_Issued = sum(counts))
  },
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
#     mr %>% filter(iscoinsurance !="", mrdate >= input$daterange_3.2[1], mrdate <= input$daterange_3.2[2]) %>%
#         group_by(iscoinsurance) %>% summarize(counts = n(), percentage = n()/nrow(mr))
# })

output$piechart_3.1 <- renderPlotly({
  
  if(input$selectinput == "All"){
    coinsurancedata <- coinsurancedata_sum %>% filter(mrdate %in% seq(input$daterange_3.2[1],
                                                                      input$daterange_3.2[2], by = 'day')) %>%
      group_by(iscoinsurance) %>% summarize(counts = sum(counts), percentage = sum(counts)/nrow_mr)
  }
  else {
    coinsurancedata <- coinsurancedata_sum %>% filter(company_name == input$selectinput,
                                                      mrdate %in% seq(input$daterange_3.2[1],
                                                                      input$daterange_3.2[2], by = 'day')) %>% 
      group_by(iscoinsurance) %>% summarize(counts = sum(counts), percentage = sum(counts)/nrow_mr)
  }
  
  plot_ly(coinsurancedata, labels= ~iscoinsurance, values= ~percentage, type='pie',
          textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
          hoverinfo = 'text', text = ~paste(coinsurancedata$counts),
          marker = list(colors = colors,
                        line = list(color = '#FFFFFF', width = 1))) %>%
    layout(
      annotations=list(text=paste("Company:", input$selectinput), xref="paper",x=0.9,
                       yref="paper",y=1,showarrow=FALSE))
})

#SMS Sending Status Pie Chart 3.2
# smsdata_reactive <- reactive({
#     mr %>% filter(smsstatus !="", mrdate >= input$daterange_3.2[1], mrdate <= input$daterange_3.2[2]) %>%
#         group_by(smsstatus) %>% summarize(counts = n(), percentage = n()/nrow(mr))
# })

# output$piechart_3.2 <- renderPlotly({
#     
#     if(input$selectinput == "All"){
#         smsdata <- smsdata_sum %>% filter(mrdate >= input$daterange_3.2[1], mrdate <= input$daterange_3.2[2]) %>%
#             group_by(smsstatus) %>% summarize(counts = sum(counts), percentage = sum(counts)/nrow(mr))
#     }
#     else {
#         smsdata <- smsdata_sum %>% filter(company_name == input$selectinput, mrdate >= input$daterange_3.2[1], mrdate <= input$daterange_3.2[2]) %>% 
#             group_by(smsstatus) %>% summarize(counts = sum(counts), percentage = sum(counts)/nrow(mr))
#     }
#     
#     plot_ly(smsdata, labels= ~smsstatus, values= ~percentage, type='pie',
#             textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
#             hoverinfo = 'text', text = ~paste(smsdata$counts),
#             marker = list(colors = colors,
#                           line = list(color = '#FFFFFF', width = 1))) %>%
#         layout(
#             annotations=list(text=paste("Company:", input$selectinput), xref="paper",x=0.9,
#                              yref="paper",y=1,showarrow=FALSE))
# })
# 



###### e reciept tab 
output$companyname_e_receipt <- renderValueBox({
  if(input$selectinput == "All"){
    name <- "All Company"
  }
  else {
    name <- input$selectinput
  }
  
  valueBox(
    paste("Showing Results For:", name),
    ""
  )
})

output$total_e_receipt_issued <- renderUI({
  total_e_receipt <- if(input$selectinput == "All"){
    total_e_receipt <- e_receipt_daily %>% 
      filter(mrdate %in% seq(input$daterange_e_receipt[1],
                             input$daterange_e_receipt[2], by = 'day')) %>%
      ungroup() %>%
      summarise(n = sum(total)) %>% pull(n) 
  }
  else {
    total_e_receipt <- e_receipt_daily %>% 
      filter(company_name == input$selectinput, mrdate %in% seq(input$daterange_e_receipt[1],
                                                                input$daterange_e_receipt[2], by = 'day')) %>% 
      ungroup() %>% 
      summarise(n = sum(total)) %>% pull(n)
  }
  
  sub <- (  paste(compress(total_e_receipt), "Total E-Receipt"))
  
  render_my_box(value = format(total_e_receipt, big.mark = ",", scientific = F), subtitle = sub, class = "col-12 p-2",
                bgcolor = "linear-gradient(to right,  #4b1248, #f0c27b)", textcolor = "#fff", icon = "inforce.jpg")
  
  # valueBox(
  #   #compress(total_e_receipt),
  #   format(total_e_receipt, big.mark = ",", scientific = F),
  #   paste(compress(total_e_receipt),"Total E-Receipt" ) ,
  #   color = "green",
  #   icon = tags$i(class = "fa fa-reciept", style="font-size: 44px; color: #fff")
  # )
})



output$linechart_e_receipt <- renderHighchart({
  
  if(input$selectinput == "All"){
    data <- e_receipt_daily %>% 
      filter(mrdate %in% seq(input$daterange_e_receipt[1],
                             input$daterange_e_receipt[2], by = 'day')) %>%
      group_by(mrdate) %>% 
      summarise(total = sum(total, na.rm = T))
  }
  else {
    data <- e_receipt_daily %>%
      filter(company_name == input$selectinput, mrdate %in% seq(input$daterange_e_receipt[1],
                                                                input$daterange_e_receipt[2], by = 'day'))
  }
  
  hchart(data,
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
  #     annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Date"),
  #     yaxis = list(title = "Total E-Receipt Issued")
  #   )
})


output$monthlychart_e_receipt <- renderHighchart({
  
  if(input$selectinput == "All"){
    total_by_month <- e_receipt_monthly %>%
      filter(year_r == input$selectyear_e_receipt) %>%
      group_by(month_r) %>% summarise(total = sum(total, na.rm = T))
  }
  else {
    total_by_month <- e_receipt_monthly %>% filter(company_name == input$selectinput, year_r == input$selectyear_e_receipt)
  }
  
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
  #     annotations=list(text=paste("Year: ", input$selectyear_e_receipt, "<br>", "Company:", input$selectinput),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Month"),
  #     yaxis = list(title = "Total")
  #   )
})


output$yearlychart_e_receipt <- renderHighchart({
  
  if(input$selectinput == "All"){
    total_by_month <- e_receipt_monthly %>%
      
      group_by(year_r) %>% summarise(total = sum(total, na.rm = T))
  }
  else {
    total_by_month <- e_receipt_monthly %>% filter(company_name == input$selectinput) %>%
      group_by(year_r) %>% summarise(total = sum(total, na.rm = T))
  }
  
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
  
  # plot_ly(total_by_month, x = ~factor(year_r), y = ~total, type = "bar", hoverinfo ="y") %>%
  #   layout(
  #     
  #     annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
  #                      yref="paper",y=0.9,showarrow=FALSE
  #     ),
  #     xaxis = list(title = "Year"),
  #     yaxis = list(title = "Total")
  #   )
})




## policy + covernote tab 

output$info_policycovercount <- renderValueBox({
  
  
  valueBox(
    paste0("Non-Life Insurance Industry Status on: ", today()),
    ""
  )
})

output$policycover_datatable <- renderDataTable(
  
  non_life_data_sum_table %>% select(company_name, activepolicy, totalpolicy), 
  server = FALSE, extensions = 'Buttons',
  options = list(order = list(list(3, 'desc')), dom= 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                 
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




################################################
############ FORECAST ##########################
################################################



#### nonlife  forecast button
# nonlife_admin_forecast_button_apply <- reactive(switch (input$nonlife_admin_forecast_product_type_select,
#                                           "Policy" = {
#                                             # monthly_policy <- nonlife_admin_policy_forecast_data[[loggedincompany()]]
#                                             # policy_forecast_object <- [[loggedincompany()]]
#                                             
#                                             FORECAST_PERIOD <- as.numeric(input$nonlife_admin_select_period)
#                                             
#                                             full_data <- nonlife_admin_policy_forecast_data %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)
#                                             
#                                             future_data <- full_data %>% filter(
#                                               is.na(cnt)
#                                             )
#                                             
#                                             nonlife_admin_policy_forecast_object %>%
#                                               modeltime_calibrate(nonlife_admin_policy_forecast_data)%>%
#                                               modeltime_forecast(new_data = future_data,
#                                                                  actual_data = nonlife_admin_policy_forecast_data,
#                                                                  keep_data = T
#                                               ) %>%
#                                               plot_modeltime_forecast(.title = "Monthly Policy Forecast Plot")
#                                           },
#                                           "Premium" = {
#                                             
#                                             # monthly_premium <- nonlife_admin_prem_forecast_data[
#                                             # prem_forecast_obj <- nonlife_admin_prem_forecast_object
#                                             
#                                             FORECAST_PERIOD <- as.numeric(input$nonlife_admin_select_period)
#                                             full_data <- nonlife_admin_prem_forecast_data %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)
#                                             future_data <- full_data %>% filter(
#                                               is.na(cnt)
#                                             )
#                                             
#                                             nonlife_admin_prem_forecast_object %>% 
#                                               modeltime_calibrate(nonlife_admin_prem_forecast_data)%>%
#                                               modeltime_forecast(new_data = future_data, 
#                                                                  actual_data = nonlife_admin_prem_forecast_data, 
#                                                                  keep_data = T
#                                               ) %>%
#                                               plot_modeltime_forecast(.title = "Monthly Premium Collection Forecast Plot")
#                                             
#                                           }
# )) %>% bindCache(loggedinas(), input$nonlife_admin_forecast_product_type_select, input$nonlife_admin_select_period) %>% 
#   bindEvent(input$nonlife_admin_forecastButton)
# 
# 
# output$nonlife_admin_forecast_plot <- renderPlotly({
#   
#   nonlife_admin_forecast_button_apply()
#   
# })





# output$nonlife_admin_trend_plot <- renderPlotly({
#   
#   switch (input$nonlife_admin_trend_type_select,
#           "Policy" = {
#             nonlife_admin_policy_forecast_data %>% plot_time_series(.date_var = date, .value = cnt,
#                                                                                    .title = "Monlty Policy Issue Trend")
#           },
#           "Premium" = {
#             nonlife_admin_prem_forecast_data %>% plot_time_series(.date_var = date, .value = cnt,
#                                                                                  .title = "Monlty Premium Collection Trend")
#           }
#   )
# }) %>% bindCache(loggedinas(), input$nonlife_admin_trend_type_select)















