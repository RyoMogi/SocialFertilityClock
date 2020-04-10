library(shiny)
library(markdown)
library(readstata13)
library(tidyverse)
library(ggpubr)
library(shinycssloaders)
library(highcharter)
library(magrittr)
library(cowplot)
#library(svgPanZoom)
#library(svglite)
library(plotly)
`%out%` = Negate(`%in%`)

#### ---- UI ----
shinyUI(
  navbarPage(title = "",
             windowTitle = "Social Fertility Clock", collapsible = TRUE, fluid = FALSE,
             position = "fixed-top",
             footer = tags$div(includeHTML("md/footer.html")),
             #footer = "a",
             id = 'page',
             
             tabPanel(
               title = "Home",
               id = 'home_tabs',
               fluidRow(
                 tags$style(type = "text/css", "body {padding-top: 70px;}"),
                 column(width = 12, includeMarkdown("md/title.Rmd")),
                 #column(width = 8, includeMarkdown("md/tagline.Rmd")),
                 #column(width = 4, includeMarkdown("md/photo.Rmd")),
                 column(width = 12, includeMarkdown("md/about.Rmd"))
                 #column(width = 12, includeHTML("md/about.html"))
               )
             ),
             tabPanel(
               title = "Graphs",
               id = 'graph_tabs',
               fluidRow(
                 column(width = 12, includeMarkdown("md/title.Rmd")),
                 column(width = 12, includeMarkdown("md/tagline.Rmd"))
                 ),
               br(),
               tabsetPanel(
                 type = "tabs",
                 #tabPanel("About", includeMarkdown("md/about.Rmd")),
                 tabPanel("Fig1", source("ui/Fig1.R", local = T)$value),
                 tabPanel("Fig2", source("ui/Fig2.R", local = T)$value),
                 tabPanel("Fig3", source("ui/Fig3.R", local = T)$value),
                 tabPanel("Data and Methods", withMathJax((includeMarkdown("md/Data_Methods.Rmd"))))
                 )
             ),
             #tags$div(class = "footer",
             #                  includeHTML("footer.html")),
             includeCSS("style.css"),
             tags$script(includeHTML("sns-share.html"))
  )
)
