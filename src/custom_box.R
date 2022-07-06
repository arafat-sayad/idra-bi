library(tidyverse)
library(shiny)
library(htmlwidgets)
library(glue)



# my_box_ui <- tagList(
#     div( class="panel-metric",
#          selectInput(
#              "custom_select", "",
#              c("a","b","c"),
#              width = NULL,
#              selectize = TRUE,
#              selected = "a"
#          )),
#     # tags$div(
#     #          style = "background-color: #fff; width: 100%; font-size: 3rem;",
#     #          # class = "shiny-html-output col-sm-2",
#     #          class="shiny-text-output",
#     #          # id = "cv1"
#     #          id = "text1"
#     #        )
#     
#     # htmlOutput("htmlout")
#     
#     # tags$div(
#     #   class = "custom-html-output col-sm-3",
#     #   id = "my_custom_valuebox"
#     # )
#     
#     
#     uiOutput("my_custom_valuebox")
#     
# )



my_box_ui_with_input <- function(id = "x", choices = c("a","b","c")) {
    tagList(
            div( class="panel-metric",
                 selectInput(
                     paste0("custom_select_", id), "",
                     choices,
                     width = NULL,
                     selectize = TRUE,
                     selected = choices[1]
                 )),
            # tags$div(
            #          style = "background-color: #fff; width: 100%; font-size: 3rem;",
            #          # class = "shiny-html-output col-sm-2",
            #          class="shiny-text-output",
            #          # id = "cv1"
            #          id = "text1"
            #        )
            
            # htmlOutput("htmlout")
            
            # tags$div(
            #   class = "custom-html-output col-sm-3",
            #   id = "my_custom_valuebox"
            # )
            
          
            uiOutput(paste0("my_custom_valuebox_", id))
        
    )
}




render_my_box <- function(value = "value", subtitle = "subtitle", bgcolor = "#faedcd", icon = NULL, textcolor = "black", class = "col-md-3", padding = "15px" ) {
    tagList(
        
        div(class = paste0(class, " small-box bg-", "custom"), style = glue("text-align: center!important; padding: {padding}!important; background: {bgcolor}!important; color: {textcolor};
                     height: auto!important; fontweight : bold"),
            div(
              class = "row", style = "margin: 0px auto 10px auto", 
              div(class = "col-md-10 inner", style = "padding: 0px", h4(value , style = "font-weight: bold; margin-top: 10px; font-size: 20px"), p(subtitle)), 
              if (!is.null(icon))
                div(tags$img(src = icon, style = "height: 60px; width: 60px; box-shadow: 0 5px 10px 0px darkslategrey; border-radius: 100%!important") , style = "padding: 0px", class = "col-md-2 text-center"))
            )
            
    )
}



render_my_box_with_change <- function(value = "value", subtitle = "subtitle",changeText ="percent change", 
                                      bgcolor = "#faedcd", icon = NULL, textcolor = "black", changeTextColor = "black") {
    # changeTextColor <- ifelse(changeText > 0,"green", "red")
    tagList(
        
        div(class = paste0("small-box bg-", "custom"), style = glue("background-color: {bgcolor}!important; color: {textcolor};
                     height: 100px!important; width: 300px!important; fontweight : bold"), 
            div(class = "inner", h4(value), p(subtitle), 
                p(changeText, style = glue("color:{changeTextColor}; text-align: right;"))), if (!is.null(icon))
                div(class = "icon-large", icon))
    )
}



# ui <- tagList(
#     div( class="panel panel-metric",
#          selectInput(
#              "sales-summary_metric", "",
#              c("a","b","c"),
#              width = NULL,
#              selectize = TRUE,
#              selected = "a"
#          ),
#          tags$div(
#              style = "background-color: #fff; width: 100%; font-size: 3rem;",
#              # class = "shiny-html-output col-sm-2",
#              class="shiny-text-output",
#              # id = "cv1"
#              id = "text1"
#          ),
#          
#          tags$div(
#              style = "background-color: #fff; width: 100%; font-size: 1rem;",
#              # class = "shiny-html-output col-sm-2",
#              class="shiny-text-output",
#              # id = "cv1"
#              id = "subtext"
#          )
#          
#          # uiOutput("sales-summary")
#     )
# )
# 
# 
