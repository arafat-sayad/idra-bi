# linear-gradient(to right, #52c234, #061700)
# linear-gradient(to right, #fe8c00, #f83600)
# linear-gradient(to right, #556270, #ff6b6b)
# linear-gradient(to right, #780206, #061161)
# linear-gradient(to right, #000000, #e74c3c)
# linear-gradient(to right, #f0c27b, #4b1248)
# linear-gradient(to right, #5d4157, #a8caba)


source('custom_box.R')

#Policyholders Age Distribution
output$age_dist <- renderHighchart({
    
    if(input$selectinput == "All"){
        age_group_hc <- age_group_plotly %>% group_by(Age_Group) %>% summarise(Frequecy = sum(Frequecy, na.rm = T))
        # plot_ly(age_group_plotly %>% group_by(Age_Group) %>% summarise(Frequecy = sum(Frequecy, na.rm = T)),
        #         x = ~Age_Group, y = ~Frequecy, type = "bar", hoverinfo ="y") %>%
        #     
        #     layout(bargap = 0,
        #            annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
        #                             yref="paper",y=0.9,showarrow=FALSE
        #            ),
        #            xaxis = list(title = "Age Group"),
        #            yaxis = list(title = "Total Policy Issued"),
        #            title = "Policyholder's Age Distrubution")
    }
    
    else {
        age_group_hc <- age_group_plotly %>%
            filter(company_name == input$selectinput)
        # plot_ly(age_group_plotly %>%
        #             filter(company_name == input$selectinput),
        #         x = ~Age_Group, y = ~Frequecy, type = "bar", hoverinfo ="y") %>%
        #     
        #     layout(bargap = 0,
        #            annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
        #                             yref="paper",y=0.9,showarrow=FALSE
        #            ),
        #            xaxis = list(title = "Age Group"),
        #            yaxis = list(title = "Total Policy Issued"),
        #            title = "Policyholder's Age Distrubution")
    }
    
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
    
})




# pie chart for no match policy
# nomatchpolicy_reactive <- policy %>% mutate (no_match = policystartdate != riskstartdate) %>%
#     filter (no_match) %>% group_by(company_name) %>% summarize(counts = n())   #, percentage = n()/nrow(policy)

output$nomatchpiechart <- renderPlotly({
    
    # if(input$selectinput == "All"){
    #   companywise_nomatch <- nomatchpolicy_reactive()
    # }
    # else {
    #   companywise_nomatch = policy %>% mutate (no_match= policy$policystartdate != policy$riskstartdate) %>%
    #     filter (no_match, company_name == input$selectinput) %>% group_by(company_name) %>% summarize(counts = n(), percentage = n()/nrow(policy))
    # }
    
    plot_ly(nomatchpolicy_sum, labels= ~company_name, values= ~counts, type='pie',
            textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text', text = ~paste(nomatchpolicy_sum$counts),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1))) %>%
        layout(
            annotations=list(text=paste("Company:", "All"), xref="paper",x=0.9,
                             yref="paper",y=1,showarrow=FALSE))
})



#value boxes appearing in policy summary (status)
output$companyname <- renderValueBox({
    if(input$selectinput == "All"){
        name <- "All Company"
    }
    else {
        name <- input$selectinput
    }
    
    valueBox(
        name,
        paste("showing result for ", name), color = "navy"
    )
})


# total policy in date range valuebox

output$totalpolicyvalue <- renderValueBox({
    if(input$selectinput == "All"){
        totalpolicy <- linechart_policy %>% filter(policystartdate %in% seq(input$policydaterange[1],
                                                                 input$policydaterange[2], by = 'day')) %>%
            pull(total) %>% sum()
    
    }
    else {
        totalpolicy <- linechart_policy %>% 
            filter(company_name == input$selectinput) %>%
            filter(policystartdate %in% seq(input$policydaterange[1],
                                            input$policydaterange[2], by = 'day')) %>%
                pull(total) %>% sum()
    }
    
    valueBox(
        totalpolicy,
        paste("Total Policy Issued between", input$policydaterange[1], "and", input$policydaterange[2]-1),color = "olive",
        icon = tags$i(class = "fa fa-tags", style="font-size: 44px; color: #fff ")
    )
})
output$linechart <- renderHighchart({
    
    if(input$selectinput == "All"){
        data <- linechart_policy %>%  
            filter(policystartdate %in% seq(input$daterange[1],
                                 input$daterange[2], by = 'day')) %>% 
            group_by(policystartdate) %>% summarise(total = sum(total))
        
    }
    else {
        data <- linechart_policy %>%
            filter(company_name == input$selectinput, policystartdate %in% seq(input$daterange[1],
                                                                               input$daterange[2], by = 'day')) %>% 
            group_by(policystartdate) %>% summarise(total = sum(total))
    } 
    
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
    #         annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Date"),
    #         yaxis = list(title = "Total Policy Issued")
    #     )
}) %>% bindCache(input$selectinput, input$daterange)

#total per month bar chart

# totalpolicypermonth_reactive <-reactive({
#   policy %>% filter(year == input$selectyear) %>%
#     group_by(month) %>%
#     summarise(total = n())
# })

output$monthlychart <- renderHighchart({
    
    if(input$selectinput == "All"){
        total_by_month <- monthlychart_data %>% 
            filter(year == input$selectyear) %>%
            group_by(month) %>%
            summarise(total = sum(total, na.rm = T))
    }
    else {
        total_by_month <- monthlychart_data %>% 
            filter(company_name == input$selectinput, year == input$selectyear) %>%
            group_by(month) %>%
            summarise(total = sum(total, na.rm = T))
    }
    
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
    #         annotations=list(text=paste("Year: ", input$selectyear, "<br>", "Company:", input$selectinput), xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Month"),
    #         yaxis = list(title = "Total Policies Issued")
    #     )
}) %>% bindCache(input$selectinput, input$selectyear)

# #total per year bar chart
# totalpolicyperyear_reactive <- reactive({
#     policy %>% group_by(year) %>% summarise(total = n())
# })

output$yearlychart <- renderHighchart({
    if(input$selectinput == "All"){
        total_by_year <- monthlychart_data %>% filter(year <= input$yearrange[2], year >= input$yearrange[1]) %>% 
            group_by(year) %>%
            summarise(total = sum(total, na.rm = T))
    }
    else {
        total_by_year <- monthlychart_data %>% 
            filter(company_name == input$selectinput) %>% filter(year <= input$yearrange[2], year >= input$yearrange[1]) %>%
            group_by(year) %>%
            summarise(total = sum(total, na.rm = T))
    }
    
    hchart(total_by_year,
           'column', hcaes(x = year, y = total, color = year)
    ) %>%
        # hc_plotOptions(
        #     column = list(
        #         dataLabels = list(
        #             enabled = FALSE
        #         ),
        #         dataSorting = {
        #             enabled = TRUE
        #         }
        #     )
        # ) %>%
        hc_exporting(
            enabled = TRUE, # always enabled
            filename = "custom-file-name"
        )
    
    # plot_ly(total_by_year %>% filter(year <= input$yearrange[2], year >= input$yearrange[1]), 
    #         x = ~factor(year), y = ~total, type = "bar", hoverinfo ="y") %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Year"),
    #         yaxis = list(title = "Total Policy Issued")
    #     )
    
})







# Total inforce value box
totalinforce_reactive <- reactive({
    sum(policy_status$inforce)
})
totalinforce_filter_reactive <- reactive({
    policy_status$inforce[policy_status$company_name == input$selectinput]
})

output$totalinforce <- renderUI({
  total_inforce <- if(input$selectinput == "All"){
        total_inforce <- totalinforce_reactive()
    }
    else {
        total_inforce <- totalinforce_filter_reactive()
    }
  
  sub <- (  paste(compress(total_inforce), "Total Inforce Policy"))
  
  render_my_box(value = format(total_inforce, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #fe8c00, #f83600)", textcolor = "#fff", icon = "inforce.jpg")
  

    
    # valueBox(
    #     #compress(total_inforce),
    #     #paste(format(round(total_inforce / 1e5, 1), trim = TRUE), "lac"),
    #     total_inforce,
    #     paste(compress(total_inforce), "Total Inforce Policy") ,color = "green",
    #     icon = tags$i(class = "fa fa-sticky-note", style="font-size: 44px; color: #fff")
    # )
})


# total forfeit value box
totallapse_reactive <- reactive({
    sum(policy_status$lapse)
})

totallapse_filter_reactive <- reactive({
    policy_status$lapse[policy_status$company_name == input$selectinput]
})

output$totallapse <- renderUI({
  total_lapse <- if(input$selectinput == "All"){
        total_lapse <- totallapse_reactive()
    }
    else {
        total_lapse <- totallapse_filter_reactive()
        
    }
    
  sub <- ( paste(compress(total_lapse), "Total Lapse Policy"))
  
  render_my_box(value = format(total_lapse, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #556270, #ff6b6b)", textcolor = "#fff", icon = "lapse.jpg")
  
    # valueBox(
    #     #compress(total_lapse),
    #     #paste(format(round(total_lapse / 1e5, 1), trim = TRUE), "lac"),
    #     total_lapse,
    #     paste(compress(total_lapse), "Total lapse Policy") ,color = "orange",
    #     icon = tags$i(class = "fa fa-cut", style="font-size: 44px; color: #fff")
    # )
})

# total paidup value box
totalpaidup_reactive <- reactive({
    
    sum(policy_status$paidup)
})
totalpaidup_filter_reactive <- reactive({
    
    policy_status$paidup[policy_status$company_name == input$selectinput]
})

output$totalpaidup <- renderUI({
  totalpaidup <- if(input$selectinput == "All"){
        
        total_paidup <- totalpaidup_reactive()
    }
    else {
        total_paidup <- totalpaidup_filter_reactive()
        
    }
  
  sub <- ( paste(compress(totalpaidup), "Total PaidUp Policy"))
  
  render_my_box(value = format(totalpaidup, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #780206, #061161)", textcolor = "#fff", icon = "paidup.jpg")
    
    # valueBox(
    #     #compress(total_paidup),
    #     #paste(format(round(total_paidup / 1e5, 1), trim = TRUE), "lac"),
    #     total_paidup,
    #     paste(compress(total_paidup), "Total Paid-up Policy") ,color = "light-blue",
    #     icon = tags$i(class = "fa fa-power-off", style="font-size: 44px; color: #fff")
    # )
})


# total policy value box
totalpolicy_reactive <- reactive({
    
    sum(policy_status$total)
})
totalpolicy_filter_reactive <- reactive({
    
    policy_status$total[policy_status$company_name == input$selectinput]
})

output$totalpolicy <- renderUI({
  total_policy <- if(input$selectinput == "All"){
        
        total_policy <- totalpolicy_reactive()
    }
    else {
        total_policy <- totalpolicy_filter_reactive()
        
    }
    
  sub <- ( paste(compress(total_policy), "Total Policy"))
  
  render_my_box(value = format(total_policy, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #000000, #e74c3c)", textcolor = "#fff", icon = "total_policy.jpg")
  
    # valueBox(
    #     #compress(total_policy),
    #     #paste(format(round(total_policy / 1e5, 1), trim = TRUE), "lac"),
    #     total_policy,
    #     paste(compress(total_policy), "Total Policy") ,color = "green",
    #     icon = tags$i(class = "fa fa-archive", style="font-size: 44px; color: #fff")
    # )
})




# total surrender value box
totalsurrender_reactive <- reactive({
    sum(policy_status$surrender)
})
totalsurrender_filter_reactive <- reactive({
    policy_status$surrender[policy_status$company_name == input$selectinput]
})

output$totalsurrender <- renderUI({
  surrender <- if(input$selectinput == "All"){
        surrender <- totalsurrender_reactive()
    }
    else {
        surrender <- totalsurrender_filter_reactive()
    }
  
  sub <- ( paste(compress(surrender), "Total Surrender Policy"))
  
  render_my_box(value = format(surrender, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #f0c27b, #4b1248)", textcolor = "#fff", icon = "surrender.jpg")
    
    # valueBox(
    #     #compress(surrender),
    #     #paste(format(round(surrender / 1e5, 1), trim = TRUE), "lac"),
    #     surrender,
    #     paste(compress(surrender),  "Total Surrender Policy"),color = "yellow",
    #     icon = tags$i(class = "fa fa-diagnoses", style="font-size: 44px; color: #fff")
    # )
})


# total death value box
totaldeath_reactive <- reactive({
    sum(policy_status$death)
})
totaldeath_filter_reactive <- reactive({
    policy_status$death[policy_status$company_name == input$selectinput]
})

output$totaldeath <- renderUI({
  death <- if(input$selectinput == "All"){
        death <- totaldeath_reactive()
    }
    else {
        death <- totaldeath_filter_reactive()
    }
  
  sub <- ( paste(compress(death), "Total Death Policy"))
  
  render_my_box(value = format(death, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #5d4157, #a8caba)", textcolor = "#fff", icon = "death.jpg")
    
    # valueBox(
    #     #compress(death),
    #     #paste(format(round(death / 1e5, 1), trim = TRUE), "lac"),
    #     death,
    #     paste(compress(death), "Total Death Policy") ,color = "red",
    #     icon = tags$i(class = "fa fa-book-dead", style="font-size: 44px; color: #fff")
    # )
})

# total matured value box:
totalmatured_reactive <- reactive({
    sum(policy_status$matured)
})

totalmatured_filter_reactive <- reactive({
    policy_status$matured[policy_status$company_name == input$selectinput]
})

output$totalmatured <- renderUI({
  totalmatured <- if(input$selectinput == "All"){
        totalmatured <- totalmatured_reactive()
    }
    else {
        totalmatured <- totalmatured_filter_reactive()
    }
  
  sub <- ( paste(compress(totalmatured), "Total Matured Policy"))
  
  render_my_box(value = format(totalmatured, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "#e63946", textcolor = "#fff", icon = "matured.jpg")
  
    
    # valueBox(
    #     #compress(totalmatured),
    #     #paste(format(round(totalmatured / 1e5, 1), trim = TRUE), "lac"),
    #     totalmatured,
    #     paste(compress(totalmatured),"Total Matured Policy" ) ,color = "aqua",
    #     icon = tags$i(class = "fa fa-balance-scale", style="font-size: 44px; color: #fff")
    # )
})





### policy status by company stacked bar plot

company_wise_summary_reactive <- reactive({
    policy_status
})


output$policystatusbycompany <- renderPlotly({
    
    company_wise_summary <- pivot_longer(company_wise_summary_reactive(), 2:7)
    
    plot_ly(company_wise_summary, x = ~value, y = ~ company_name) %>% 
        add_bars(color = ~name,  orientation = 'h') %>% layout(barmode = "stack") %>%
        layout(
            xaxis = list(title = "Policy Issued"),
            yaxis = list(title = "")
        )
    
})


## company wise policy status percentage

company_wise_summary_percent_reactive <- reactive({
    company_wise_summary <- company_wise_summary_reactive()
    company_wise_summary <- data.frame(company_name = company_wise_summary$company_name, round((company_wise_summary[2:7] / rowSums(company_wise_summary[2:7]))*100, 2))
    pivot_longer(company_wise_summary, 2:7)
})

output$policystatusbycompanypercent <- renderPlotly({
    
    plot_ly(company_wise_summary_percent_reactive(), x = ~value, y = ~ company_name) %>% 
        add_bars(color = ~name,  orientation = 'h') %>% layout(barmode = "stack") %>%
        layout(
            xaxis = list(title = "Percentage Policy Issued"),
            yaxis = list(title = "")
        )
    
})


## policy by term tree plot

policyterm_reactive <- reactive({
    # policy %>% group_by(term) %>% count()
    policy_by_term_sum %>% group_by(term) %>% summarise(n = sum(n))
})
policyterm_reactive_filter <- reactive({
    policy_by_term_sum %>% filter(company_name == input$selectinput) %>% 
        group_by(term) %>% summarise(n = sum(n))
})

output$policytermplot <- renderPlotly({
    
    if(input$selectinput == "All"){
        plot_ly(
            data = policyterm_reactive(),
            type = "treemap",
            labels = ~term,
            values = ~n,
            parents = "Term",
            textinfo="label+value"
        )
    }
    else{
        plot_ly(
            data = policyterm_reactive_filter(),
            type = "treemap",
            labels = ~term,
            values = ~n,
            parents = "Term",
            textinfo="label+value"
        )
    }
    
    
})

output$ploicyterm_companyname <- renderText({
    paste("Company:",input$selectinput)
})








#e receipt summary starts

# ereceiptperday_reactive <- reactive({
#   or %>%
#     filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]) %>%
#     group_by(ordate) %>% 
#     summarise(total = n())
# })

output$linechart_r <- renderHighchart({
    
    if(input$selectinput == "All"){
        data <- linechart_r_data %>% 
            filter(ordate %in% seq(input$daterange_r[1],
                                 input$daterange_r[2], by = 'day')) %>%
            group_by(ordate) %>% 
            summarise(total = sum(total, na.rm = T))
    }
    else {
        data <- linechart_r_data %>%
            filter(company_name == input$selectinput, ordate %in% seq(input$daterange_r[1],
                                                                      input$daterange_r[2], by = 'day')) %>%
            group_by(ordate) %>% 
            summarise(total = sum(total, na.rm = T))
    }
    
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
    #         annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Date"),
    #         yaxis = list(title = "Total E-Receipt Issued")
    #     )
}) %>% bindCache(input$selectinput, input$daterange_r)


#total per month bar chart

# ereceiptpermonth_reactive <- reactive({
#   or %>% filter(year_r == input$selectyear_r) %>% 
#     group_by(month_r) %>% summarise(total = n())
# })

output$monthlychart_r <- renderHighchart({
    
    if(input$selectinput == "All"){
        total_by_month <- monthlychart_r %>%
            filter(year_r == input$selectyear_r) %>%
            group_by(month_r) %>% summarise(total = sum(total, na.rm = T))
    }
    else {
        total_by_month <- monthlychart_r %>% 
            filter(company_name == input$selectinput, year_r == input$selectyear_r) %>%
            group_by(month_r) %>% summarise(total = sum(total, na.rm = T))
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
    
    # plot_ly(monthlychart_r, x = ~month_r, y = ~total, hoverinfo ="y") %>%
    #     layout(
    # 
    #         annotations=list(text=paste("Year: ", input$selectyear_r, "<br>", "Company:", input$selectinput),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Month"),
    #         yaxis = list(title = "Total")
    # )
}) %>% bindCache(input$selectinput, input$selectyear_r)

output$yearlychart_r <- renderHighchart({
    
    if(input$selectinput == "All"){
        total_by_month <- monthlychart_r %>%
            group_by(year_r) %>% summarise(total = sum(total, na.rm = T))
    }
    else {
        total_by_month <- monthlychart_r %>% filter(company_name == input$selectinput) %>%
            group_by(year_r) %>% summarise(total = sum(total, na.rm = T))
    }
    
    hchart(monthlychart_r,
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
    #         annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Year"),
    #         yaxis = list(title = "Total")
    #     )
})

#E-receipt Value Boxes
output$companyname_r <- renderValueBox({
    if(input$selectinput == "All"){
        name <- "All Company"
    }
    else {
        name <- input$selectinput
    }
    
    valueBox(
        name,
        paste("showing result for ", name), color = "navy"
    )
})

# #total_e_receipt_issued value
# compress <- function(tx) { 
#   div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
#                       c(0, 1e3, 1e5, 1e8, 1e10) )  # modify this if negative numbers are possible
#   paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
#         c("","K","Lac","Crore", "Billion")[div] )}
# 
# total_e_receipt_reactive <- reactive({
#     nrow(or %>%
#              filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]))
# })

output$total_e_receipt_issued <- renderUI({
  total_e_receipt <- if(input$selectinput == "All"){
        total_e_receipt <- premium_by_ortype_count %>%
            filter(ordate %in% seq(input$daterange_r[1],
                                   input$daterange_r[2], by = 'day')) %>%
            pull(total) %>% sum()
    }
    else {
        total_e_receipt <- premium_by_ortype_count %>%
            filter(company_name == input$selectinput) %>%
            filter(ordate %in% seq(input$daterange_r[1],
                                   input$daterange_r[2], by = 'day')) %>%
            pull(total) %>% sum()
        
        
    }
  
  sub <- ( paste(compress(total_e_receipt), "Total E-Receipt collected"))
  
  render_my_box(value = format(total_e_receipt, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "#e63946", textcolor = "#fff", icon = "total_reciept.jpg")
  
    
    # valueBox(
    #     #compress(total_e_receipt),
    #     format(total_e_receipt, big.mark = ",", scientific = F),
    #     paste(compress(total_e_receipt),"Total E-Receipt" ) ,
    #     color = "green",
    #     icon = tags$i(class = "fa fa-reciept", style="font-size: 44px; color: #fff")
    # )
})




#First Premium Receipt

# total_fpr_reactive <- reactive({
#     sum((or %>%
#              filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]))$ortype == "F", na.rm = T)
# })

output$fpr_e_receipt_issued <- renderUI({
  total_fpr <- if(input$selectinput == "All"){
        total_fpr <- premium_by_ortype_count %>%
            filter(ordate %in% seq(input$daterange_r[1],
                                   input$daterange_r[2], by = 'day'), ortype == "F") %>%
            pull(total) %>% sum()
    }
    else {
        total_fpr <- premium_by_ortype_count %>%
            filter(company_name == input$selectinput) %>%
            filter(ordate %in% seq(input$daterange_r[1],
                                   input$daterange_r[2], by = 'day'), ortype == "F") %>%
            pull(total) %>% sum()
    }
  
  sub <- ( paste(compress(total_fpr), "Total First Premium Receipt (FPR)"))
  
  render_my_box(value = format(total_fpr, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #fe8c00, #f83600)", textcolor = "#fff", icon = "first_prem_reciept.jpg")
    
    # valueBox(
    #     #compress(total_fpr),
    #     #paste(format(round(total_fpr / 1e5, 1), trim = TRUE), "lac"),
    #     format(total_fpr, big.mark = ",", scientific = F),
    #     paste(compress(total_fpr), "Total First Premium Receipt (FPR)") ,color = "navy",
    #     icon = tags$i(class = "fa fa-cash-register", style="font-size: 44px; color: #fff")
    # )
})

#Deferred Premium Receipt

# total_deferred_reactive <- reactive({
#     sum((or %>%
#              filter(ordate >= input$daterange_r[1], ordate < input$daterange_r[2]))$ortype == "D", na.rm = T)
# })

output$deferred_e_receipt_issued <- renderUI({
  total_deferred <-  if(input$selectinput == "All"){
        total_deferred <- premium_by_ortype_count %>%
            filter(ordate %in% seq(input$daterange_r[1],
                                   input$daterange_r[2], by = 'day'), ortype == "D") %>%
            pull(total) %>% sum()
    }
    else {
        total_deferred <- premium_by_ortype_count %>%
            filter(company_name == input$selectinput) %>%
            filter(ordate %in% seq(input$daterange_r[1],
                                   input$daterange_r[2], by = 'day'), ortype == "D") %>%
            pull(total) %>% sum()
    }
    
  sub <- ( paste(compress(total_deferred), "Total Deferred Premium Receipt (D)"))
  
  render_my_box(value = format(total_deferred, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #780206, #061161)", textcolor = "#fff", icon = "deffered_reciept.jpg")
  
    # valueBox(
    #     #compress(total_deferred),
    #     #paste(format(round(total_deferred / 1e5, 1), trim = TRUE), "lac"),
    #     format(total_deferred, big.mark = ",", scientific = F),
    #     paste(compress(total_deferred), "Total Deferred Premium Receipt (D)") ,color = "orange",
    #     icon = tags$i(class = "fa fa-truck-loading", style="font-size: 44px; color: #fff")
    # )
})

#renewal Receipt

# total_renewal_reactive <- reactive({
#     sum((or %>%
#              filter(ordate >= input$daterange_r[1], 
#                     ordate < input$daterange_r[2]))$ortype == "R", na.rm = T)
# })

output$renewal_e_receipt_issued <- renderUI({
  total_renewal <- if(input$selectinput == "All"){
        total_renewal <- premium_by_ortype_count %>%
            filter(ordate %in% seq(input$daterange_r[1],
                                   input$daterange_r[2], by = 'day'), ortype == "R") %>%
            pull(total) %>% sum()
    }
    else {
        total_renewal <- premium_by_ortype_count %>%
            filter(company_name == input$selectinput) %>%
            filter(ordate %in% seq(input$daterange_r[1],
                                   input$daterange_r[2], by = 'day'), ortype == "R") %>%
            pull(total) %>% sum()
        
        
        
        
    }
  
  sub <- ( paste(compress(total_renewal), "Total Renewal Premium Receipt (R)"))
  
  render_my_box(value = format(total_renewal, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "linear-gradient(to right, #556270, #ff6b6b)", textcolor = "#fff", icon = "renewal_reciept.jpg")
  
    
    # valueBox(
    #     #compress(total_renewal),
    #     #paste(format(round(total_renewal / 1e5, 1), trim = TRUE), "lac"),
    #     format(total_renewal, big.mark = ",", scientific = F),
    #     paste(compress(total_renewal), "Total Renewal Premium Receipt (R)") ,color = "purple",
    #     icon = tags$i(class = "fa fa-glass-cheers", style="font-size: 44px; color: #fff")
    # )
})




#premium collection value boxes

output$companyname_p <- renderValueBox({
    if(input$selectinput == "All"){
        name <- "All Company"
    }
    else {
        name <- input$selectinput
    }
    
    valueBox(
        name,
        paste("showing result for ", name), color = "navy"
    )
})

#Total Premium Collection

# total_prem_reactive <- reactive({
#     sum((or %>%
#              filter(ordate >= input$daterange_p[1], 
#                     ordate < input$daterange_p[2]))$totalpayableamount, na.rm = T)
# })

output$total_prem <- renderUI({
  total_prem <- if(input$selectinput == "All"){
        total_prem <- premium_by_ortype_sum %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day')) %>%
            pull(total) %>% sum()
    }
    else {
        total_prem <- premium_by_ortype_sum %>%
            filter(company_name == input$selectinput) %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                 input$daterange_p[2], by = 'day')) %>%
            pull(total) %>% sum()
    }

  sub <- ( paste(compress(total_prem), "Total Premium Collected"))

  render_my_box(value = format(total_prem, big.mark = ",", scientific = F), subtitle = sub,
                 bgcolor = "#e63946", textcolor = "#fff", icon = "taka.png")

    # valueBox(
    #     format(total_prem, big.mark = ",", scientific = F),
    # 
    #     # compress(total_prem),
    #     # paste(format(total_prem, big.mark = ",", scientific = F),"(", compress(total_prem), ")"),
    #     #paste(format(round(total_prem / 1e7, 1), trim = TRUE), "Crore"),
    #     paste(compress(total_prem), "Total Premium Collection"),
    #     color = "black",
    #     icon = tags$i(class = "fa fa-hand-holding-usd", style="font-size: 44px; color: #fff")
    # )
})

#First Premium Collection

# total_fpr_p_reactive <- reactive({
#     sum((or %>%
#              filter(ordate >= input$daterange_p[1], ordate < input$daterange_p[2], 
#                     ortype == "F"))$totalpayableamount, na.rm = T)
# })

output$fpr_prem <- renderUI({
  total_fpr_p <- if(input$selectinput == "All"){
        total_fpr_p <- premium_by_ortype_sum %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day'), ortype == "F") %>%
            pull(total) %>% sum()
    }
    else {
        total_fpr_p <- premium_by_ortype_sum %>%
            filter(company_name == input$selectinput) %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day'), ortype == "F") %>%
            pull(total) %>% sum()
    }
  
  sub <- ( paste(compress(total_fpr_p), "Total First Premium Collected"))
  
  render_my_box(value = format(total_fpr_p, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "#023047", textcolor = "#fff", icon = "taka.png")
  
    
    # valueBox(
    #     #compress(total_fpr_p),
    #     #paste(format(round(total_fpr_p / 1e7, 1), trim = TRUE), "Crore"),
    #     format(total_fpr_p, big.mark = ",", scientific = F),
    #     paste(compress(total_fpr_p), "Total First Premium Collection"),
    #     color = "green",
    #     icon = tags$i(class = "fa fa-money-check-alt", style="font-size: 44px; color: #fff")
    # )
})

#Deferred Premium Collection

# total_deferred_p_reactive <- reactive({
#     sum((or %>%
#              filter(ordate >= input$daterange_p[1], 
#                     ordate < input$daterange_p[2], ortype == "D"))$totalpayableamount, na.rm = T)
# })

output$deferred_prem <- renderUI({
  total_deferred_p <-  if(input$selectinput == "All"){
        total_deferred_p <- premium_by_ortype_sum %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day'), ortype == "D") %>%
            pull(total) %>% sum()
    }
    else {
        total_deferred_p <- premium_by_ortype_sum %>%
            filter(company_name == input$selectinput) %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day'), ortype == "D") %>%
            pull(total) %>% sum()
    }
    
  sub <- ( paste(compress(total_deferred_p), "Total Deferred Premium Collection"))
  
  render_my_box(value = format(total_deferred_p, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "#588157", textcolor = "#fff", icon = "taka.png")
  
    # valueBox(
    #     #compress(total_deferred_p),
    #     #paste(format(round(total_deferred_p / 1e7, 1), trim = TRUE), "Crore"),
    #     format(total_deferred_p, big.mark = ",", scientific = F),
    #     paste(compress(total_deferred_p), "Total Deferred Premium Collection"),
    #     color = "blue",
    #     icon = tags$i(class = "fa fa-funnel-dollar", style="font-size: 44px; color: #fff")
    # )
})

#renewal Collection

# total_ren_p_reactive <- reactive({
#     sum((or %>%
#              filter(ordate >= input$daterange_p[1], 
#                     ordate < input$daterange_p[2], ortype == "R"))$totalpayableamount, na.rm = T)
# })

output$renewal_prem <- renderUI({
  total_ren_p <- if(input$selectinput == "All"){
        total_ren_p <- premium_by_ortype_sum %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day'), ortype == "R") %>%
            pull(total) %>% sum()
    }
    else {
        
        total_ren_p <- premium_by_ortype_sum %>%
            filter(company_name == input$selectinput) %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day'), ortype == "R") %>%
            pull(total) %>% sum()
    }
  
  sub <- ( paste(compress(total_ren_p), "Total Renewal Premium Collection"))
  
  render_my_box(value = format(total_ren_p, big.mark = ",", scientific = F), subtitle = sub,
                bgcolor = "#006d77", textcolor = "#fff", icon = "taka.png")
  
    
    # valueBox(
    #     #paste(format(round(total_ren_p / 1e7, 1), trim = TRUE), "Crore"),
    #     format(total_ren_p, big.mark = ",", scientific = F),
    #     paste(compress(total_ren_p),"Total Renewal Premium Collection" ),
    #     color = "teal", tags$style("background: red"),
    #     icon = tags$i(class = "fa fa-credit-card", style="font-size: 44px; color: #fff")
    # )
})


#Total Premium Collection Charts

output$linechart_p <- renderHighchart({
    
    if(input$selectinput == "All"){
        data <- linechart_p_or %>% 
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day')) %>%
            group_by(ordate) %>% summarise(total = sum(total, na.rm = T))
    }
    else {
        data <- linechart_p_or %>%
            filter(ordate %in% seq(input$daterange_p[1],
                                   input$daterange_p[2], by = 'day'), company_name == input$selectinput)
    }
    
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
    
    # plot_ly(data, x = ~ordate, y = ~total, mode = "line", hoverinfo= "text", text = ~paste0("Date: ", data$ordate," (",weekdays(as.Date(ordate)),")", "<br>", "Premium Collection:", format(round(data$total / 1e5, 1), trim = TRUE), "lac")) %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Date"),
    #         yaxis = list(title = "Total Premium (in Taka)")
    #     )
}) %>% bindCache(input$selectinput, input$daterange_p)


#total premium collection per month bar chart



output$monthlychart_p <- renderHighchart({
    
    if(input$selectinput == "All"){
        total_by_month_p <- monthlychart_p_or %>% 
            filter(year_r == input$selectyear_p) %>% 
            group_by(month_r) %>% summarise(total = sum(total, na.rm = T)) 
    }
    else {
        total_by_month_p <- monthlychart_p_or %>% 
            filter(company_name == input$selectinput, year_r == input$selectyear_p)
    }
    
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
    #         annotations=list(text=paste("Year: ", input$selectyear_p, "<br>", "Company:", input$selectinput),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Month"),
    #         yaxis = list(title = "Total (in Taka)")
    #         
    #     )
}) %>% bindCache(input$selectinput, input$selectyear_p)





output$yearlychart_p <- renderHighchart({
    
    if(input$selectinput == "All"){
        total_by_year_p <- monthlychart_p_or %>% 
            group_by(year_r) %>% summarise(total = sum(total, na.rm = T)) 
    }
    else {
        total_by_year_p <- monthlychart_p_or %>% 
            filter(company_name == input$selectinput) %>%
            group_by(year_r) %>% summarise(total = sum(total, na.rm = T)) 
    }
    
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
    
    # plot_ly(total_by_year_p, x = ~year_r, y = ~total, hoverinfo= "text", text = ~paste(total_by_year_p$total, "Taka")) %>%
    #     layout(
    #         
    #         annotations=list(text=paste("Company:", input$selectinput),xref="paper",x=0.5,
    #                          yref="paper",y=0.9,showarrow=FALSE
    #         ),
    #         xaxis = list(title = "Year"),
    #         yaxis = list(title = "Total (in Taka)")
    #         
    #     )
})




### interactive comparison tab items below

# reactive filter data for comparison charts that changes on button click
# compare_policy_reactive <- eventReactive(input$compare_button, {policy %>%
#         filter(company_name %in% input$linechart_company_picker)})

compare_policy_reactive <- eventReactive(input$compare_button, {life_compare_policy_data %>% ungroup() %>%
        filter(company_name %in% input$linechart_company_picker)})




# compare_or_reactive <- eventReactive(input$compare_button, {or %>%
#         filter(company_name %in% input$linechart_company_picker)})

compare_or_reactive <- eventReactive(input$compare_button, {life_compare_or_data %>%
        filter(company_name %in% input$linechart_company_picker)})

#total per day line chart comparison
output$linechart_comparison <- renderPlotly({
    
    # data <- compare_policy_reactive() %>%
    #     filter(policystartdate >= input$daterange_comparison[1], 
    #            policystartdate < input$daterange_comparison[2]) %>%
    #     count(policystartdate, company_name)
    
    data <- compare_policy_reactive() %>%
                filter(policystartdate %in% seq(input$daterange_comparison[1],
                                     input$daterange_comparison[2], by = 'day')) %>%
        group_by(policystartdate, company_name) %>%
        summarise(n = sum(total))
    data <- data.frame(data)
    
    
    plot_ly(data, x = ~policystartdate, y = ~n) %>% add_lines(color = ~company_name) %>%
        layout(
            hovermode = "x unified",
            xaxis = list(title = "Date"), 
            yaxis = list(title = "Total")
        )
    
}) 
# %>% bindCache(compare_policy_reactive())



#monthly policy issued comparison 

output$monthlychart_policy_comparison <- renderPlotly({
    # total_by_month <- compare_policy_reactive() %>% 
    #     filter(year == input$selectyear_policy_comparison) %>% 
    #     count(month, company_name)
    
    total_by_month <- compare_policy_reactive() %>% 
        filter(year == input$selectyear_policy_comparison) %>%
        group_by(month, company_name) %>%
        summarise(n = sum(total))
    total_by_month <- data.frame(total_by_month)
    
    plot_ly(total_by_month, x = ~month, y = ~n) %>% add_lines(color = ~company_name) %>% add_markers(hoverinfo = "none", color = ~company_name, showlegend = FALSE) %>%
        layout(
            hovermode = "x unified",
            xaxis = list(title = "Month"),
            yaxis = list(title = "Total Policies Issued")
        )
})


#daily prem collection comparison
output$linechart_p_comparison <- renderPlotly({
    # data <- data.frame(compare_or_reactive() %>%
    #                        filter(ordate >= input$daterange_p_comparison[1], ordate < input$daterange_p[2]) %>%
    #                        group_by(ordate, company_name) %>% 
    #                        summarise(total = sum(totalpayableamount, na.rm = T)))
    
    data <- data.frame(compare_or_reactive() %>%
                           filter(ordate %in% seq(input$daterange_p_comparison[1],
                                                input$daterange_p_comparison[2], by = 'day')) %>%
                           group_by(ordate, company_name) %>% 
                           summarise(total = sum(total)))
    
    
    
    plot_ly(data, x = ~ordate, y = ~total) %>%
        add_lines(text = ~company_name, hovertemplate = '%{text}: %{y:.2f} Taka<extra></extra>', color = ~company_name) %>%
        layout(
            hovermode = "x unified",
            xaxis = list(title = "Date"),
            yaxis = list(title = "Total Premium (in Taka)")
        )
    
})

#monthly premium collection comparison

output$monthlychart_premium_comparison <- renderPlotly({
    total_by_month_p <- data.frame(compare_or_reactive() %>% 
                                       filter(year_r == input$selectyear_premium_comparison) %>% 
                                       group_by(month_r, company_name) %>% summarise(total = sum(total)))
    
    plot_ly(total_by_month_p, x = ~month_r, y = ~total) %>% 
        add_lines(text = ~company_name, hovertemplate = '%{text}: %{y:.2f} Taka<extra></extra>', color = ~company_name) %>% add_markers(hoverinfo = "none", color = ~company_name, showlegend = FALSE) %>%
        layout(
            hovermode = "x unified",
            xaxis = list(title = "Month"),
            yaxis = list(title = "Total (in Taka)")
            
        )
    
})

#Distribution of Gender Pie Chart

# genderdata_reactive <- reactive({
#     policy %>% filter(policystartdate >= input$daterange_gender[1], policystartdate < input$daterange_gender[2], gender %in% c("Female", "Male")) %>%
#         group_by(gender) %>% summarize(counts = n())
# })

output$genderchart <- renderPlotly({
    
    if(input$selectinput == "All"){
        genderdata <- genderdata_sum %>% ungroup() %>%
            filter(policystartdate %in% seq(input$daterange_gender[1],
                                 input$daterange_gender[2], by = 'day')) %>%
            filter(gender %in% c("Female", "Male", "Other")) %>%
            group_by(gender) %>% summarize(counts = sum(counts))
        
    }
    else {
        genderdata <- genderdata_sum %>% ungroup() %>%
            filter(company_name == input$selectinput) %>%
            filter(policystartdate %in% seq(input$daterange_gender[1],
                                            input$daterange_gender[2], by = 'day')) %>%
            filter(gender %in% c("Female", "Male", "Other")) %>% 
            group_by(gender) %>% summarize(counts = sum(counts))
        genderdata <- data.frame(genderdata)
    }
    
    plot_ly(genderdata, labels= ~gender, values= ~counts, type='pie',
            textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text', text = ~paste(genderdata$counts),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1))) %>%
        layout(title = "Policyholder's Gender Distribution",
               annotations=list(text=paste("Company:", input$selectinput), xref="paper",x=0.7,
                                yref="paper",y=0.9, showarrow=FALSE))
})  %>% bindCache (input$selectinput, input$daterange_gender)







################################################
############ FORECAST ##########################
################################################


# output$life_admin_trend_plot <- renderPlotly({
#     
#     switch (input$life_admin_trend_type_select,
#             "Policy" = {
#                 life_admin_policy_forecast_data %>% plot_time_series(.date_var = date, .value = cnt,
#                                                                                     .title = "Monlty Policy Issued")
#             },
#             "Premium" = {
#                 life_admin_prem_forecast_data %>% plot_time_series(.date_var = date, .value = cnt,
#                                                                                   .title = "Weekly Premium Collection Trend")
#             }
#     )
# }) %>% bindCache(loggedinas(), input$life_admin_trend_type_select)





#### life  forecast button
# life_admin_forecast_button_apply <- reactive(switch (input$life_admin_forecast_product_type_select,
#                                           "Policy" = {
#                                               
#                                               FORECAST_PERIOD <- as.numeric(input$life_admin_select_period)
#                                               
#                                               full_data <- life_admin_policy_forecast_data %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)
#                                               
#                                               future_data <- full_data %>% filter(
#                                                   is.na(cnt)
#                                               )
#                                               
#                                               life_admin_policy_forecastObjects %>% 
#                                                   modeltime_calibrate(life_admin_policy_forecast_data)%>%
#                                                   modeltime_forecast(new_data = future_data, 
#                                                                      actual_data = life_admin_policy_forecast_data, 
#                                                                      keep_data = T
#                                                   ) %>%
#                                                   plot_modeltime_forecast(.title = "Monthly Policy Forecast Plot")
#                                           },
#                                           "Premium" = {
#                                               
#                                               FORECAST_PERIOD <- as.numeric(input$life_admin_select_period)
#                                               full_data <- life_admin_prem_forecast_data %>% future_frame(.date_var = date, .length_out = FORECAST_PERIOD, .bind_data = T)
#                                               future_data <- full_data %>% filter(
#                                                   is.na(cnt)
#                                               )
#                                               
#                                               life_admin_prem_forecast_object %>% 
#                                                   modeltime_calibrate(life_admin_prem_forecast_data)%>%
#                                                   modeltime_forecast(new_data = future_data, 
#                                                                      actual_data = life_admin_prem_forecast_data, 
#                                                                      keep_data = T
#                                                   ) %>%
#                                                   plot_modeltime_forecast(.title = "Weekly Premium Collection Forecast Plot")
#                                               
#                                           }
# )) %>% bindCache(loggedinas(), input$life_admin_forecast_product_type_select, input$life_admin_select_period ) %>% 
#     bindEvent(input$life_admin_forecastButton)
# 
# 
# 
# output$life_admin_forecast_plot <- renderPlotly({
#     
#     life_admin_forecast_button_apply()
#     
# })











