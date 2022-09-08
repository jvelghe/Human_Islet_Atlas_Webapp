library(shiny) 
library(shinyhelper) 
library(data.table) 
library(Matrix) 
library(DT) 
library(magrittr) 
library(shinydashboard)

hisletconf = readRDS("hisletconf.rds")
hisletdef  = readRDS("hisletdef.rds")



endoconf = readRDS("endoconf.rds")
endodef  = readRDS("endodef.rds")



alphaconf = readRDS("alphaconf.rds")
alphadef  = readRDS("alphadef.rds")



betaconf = readRDS("betaconf.rds")
betadef  = readRDS("betadef.rds")



deltaconf = readRDS("deltaconf.rds")
deltadef  = readRDS("deltadef.rds")



dpconf = readRDS("dpconf.rds")
dpdef  = readRDS("dpdef.rds")



### Start server code 
shinyUI(fluidPage( 
### HTML formatting of error messages 
 
tags$head(tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}"))), 
list(tags$style(HTML(".navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }"))), 
 
   
### Page title 
titlePanel("Lynn Lab Adult Human Islet Single Cell Transcriptome Atlas"),  
navbarPage(id = "page", 
  NULL, 
  navbarMenu("Home",### Tab0
             tabPanel( 
               HTML("About"), 
               fluidRow(
                 column(12,
                        h3("As published in ", a("JOURNAL: LINK_TO_PAPER", href = "",target="_blank"), offset = 0)
                 ),
               ),
               fluidRow(
                 column(12,
                        h3("Please enjoy the range of tools for visualizing gene expression across versions of this dataset, including:"), offset = 0), #end of column
                 fluidRow(
                   column(12, 
                          h3(actionLink("link_to_navbar_hislet", "Hislet"),": 68,650 cells across all islet cell types;"), offset = 4)
                 ),
                  fluidRow(
                    column(12,
                           h3(actionLink("link_to_navbar_endo", "Endo"),": 59,373 endocrine cells only;"), offset = 4)
                  ),
                 fluidRow(
                   column(12,
                          h3("and each major endocrine cell type."), offset = 4)
                 ),
               ),
               br(), 
               fluidRow(
                 column(12, 
                        h3("This dataset was generated using islets from 3 male non-diabetic donors. Islets were exposed to 3 different experimental 
                conditions for 1 hour before dissociation and sequencing. Low (2.8mM glucose), Positive (25mM glucose + 40mM KCl), and 
                Negative (25mM glucose + 40mM KCl + 5mM EGTA). More information can be found in our paper. Please cite if you plan to use 
                figures generated with this website."), offset = 0
                 ),
               ),
             br(), 
             fluidRow(
               column(3,
                      h3("Human Islet Donor Info available", 
                         a("here.", href = "https://www.epicore.ualberta.ca/IsletCore/",target="_blank")),
                        # fluidRow(
                        #   column(10, tags$img(src='isletcore.png', align = "right", height = '200', width = '200')
                        #   )),
               ),
               column(4,
                      h3("Interested in the Lynn Lab?", 
                         a("Visit us!", href = "http://www.lynnlab.com/index.html", target = "_blank")), 
                        # fluidRow(
                        #   column(6, tags$img(src='bcchr.png', align = "top", height = '150', width = '175')), br(),
                        #   column(6, tags$img(src='ubc.png', align = "right", height = '100', width = '300'),
                        # )),
               ),
               column(4, 
                      h3("Check out our embryonic mouse pancreas or stem cell-derived beta cells datasets", 
                         a("here.", href = "https://www.shinyapps.io/admin/#/application/297133", target="_blank")), 
                      # fluidRow(
                      #   column(4, tags$img(src='sbmelogo.png', align = "bottom", height = '150', width = '600'),
                      #   column(3, tags$img(src='ubc.png', align = "bottom", height = '100', width = '300'),
                      #   ),
                      #   br(),
                      #   column(6, tags$img(src='CIHR.png', align = "right", height = '200', width = '350'),
                      #     ),
                      # )
               )
             ),
             fluidRow(
               column(3, tags$img(src='isletcore.png', align = "top", align = "middle", height = '200', width = '200')), 
                      # column(3, tags$img(src='isletcore.png', align = "right", height = '200', width = '200')
                      column(3, tags$img(src='bcchr.png', align = "top", align = "middle", height = '150', width = '175')),
                             column(3,  tags$img(src='ubc.png', align = "top", align = "middle", height = '100', width = '300')),
                                    column(3, tags$img(src='CIHR.png', align = "top", align = "middle", height = '200', width = '350')), 
             ),
             )
  ),
  
  
  
navbarMenu("Hislet",### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo vs GeneExpr"), 
    h4("Cell information vs gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("hisleta1drX", "X-axis:", choices = hisletconf[dimred == TRUE]$UI[6:9], 
                           selected = hisletdef$dimred[1]), 
            selectInput("hisleta1drY", "Y-axis:", choices = hisletconf[dimred == TRUE]$UI[6:9], 
                        selected = hisletdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("hisleta1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.hisleta1togL % 2 == 1", 
          selectInput("hisleta1sub1", "Cell information to subset:", 
                      choices = hisletconf[grp == TRUE]$UI, 
                      selected = hisletdef$grp1), 
          uiOutput("hisleta1sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("hisleta1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.hisleta1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("hisleta1siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("hisleta1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("hisleta1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("hisleta1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("hisleta1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("hisleta1inp1", "Cell information:", 
                           choices = hisletconf$UI[1:6], 
                           selected = hisletdef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("hisleta1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.hisleta1tog1 % 2 == 1", 
              radioButtons("hisleta1col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("hisleta1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("hisleta1lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("hisleta1oup1.ui"))), 
        downloadButton("hisleta1oup1.pdf", "Download PDF"), 
        downloadButton("hisleta1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("hisleta1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("hisleta1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("hisleta1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.hisleta1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("hisleta1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("hisleta1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("hisleta1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("hisleta1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.hisleta1tog2 % 2 == 1", 
              radioButtons("hisleta1col2", "Colour:", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("hisleta1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("hisleta1oup2.ui"))), 
        downloadButton("hisleta1oup2.pdf", "Download PDF"), 
        downloadButton("hisleta1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("hisleta1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("hisleta1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
  ### Tab1.a2: cellInfo vs cellInfo on dimRed 
  tabPanel( 
    HTML("CellInfo vs CellInfo"), 
    h4("Cell information vs cell information on dimension reduction"), 
    "In this tab, users can visualise two cell informations side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("hisleta2drX", "X-axis:", choices = hisletconf[dimred == TRUE]$UI[6:9], 
                           selected = hisletdef$dimred[1]), 
            selectInput("hisleta2drY", "Y-axis:", choices = hisletconf[dimred == TRUE]$UI[6:9], 
                        selected = hisletdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("hisleta2togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.hisleta2togL % 2 == 1", 
          selectInput("hisleta2sub1", "Cell information to subset:", 
                      choices = hisletconf[grp == TRUE]$UI, 
                      selected = hisletdef$grp1), 
          uiOutput("hisleta2sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("hisleta2tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.hisleta2tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("hisleta2siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("hisleta2psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("hisleta2fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("hisleta2asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("hisleta2txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information 1"), 
        fluidRow( 
          column( 
            6, selectInput("hisleta2inp1", "Cell information:", 
                           choices = hisletconf$UI[1:6], 
                           selected = hisletdef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("hisleta2tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.hisleta2tog1 % 2 == 1", 
              radioButtons("hisleta2col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("hisleta2ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("hisleta2lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("hisleta2oup1.ui"))), 
        downloadButton("hisleta2oup1.pdf", "Download PDF"), 
        downloadButton("hisleta2oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("hisleta2oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("hisleta2oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Cell information 2"), 
        fluidRow( 
          column( 
            6, selectInput("hisleta2inp2", "Cell information:", 
                           choices = hisletconf$UI[1:6], 
                           selected = hisletdef$meta2) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("hisleta2tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.hisleta2tog2 % 2 == 1", 
              radioButtons("hisleta2col2", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("hisleta2ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("hisleta2lab2", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("hisleta2oup2.ui"))), 
        downloadButton("hisleta2oup2.pdf", "Download PDF"), 
        downloadButton("hisleta2oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("hisleta2oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("hisleta2oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
   
  ### Tab1.a3: geneExpr vs geneExpr on dimRed 
  tabPanel( 
    HTML("GeneExpr vs GeneExpr"), 
    h4("Gene expression vs gene expression on dimension reduction"), 
    "In this tab, users can visualise two gene expressions side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("hisleta3drX", "X-axis:", choices = hisletconf[dimred == TRUE]$UI[6:9], 
                           selected = hisletdef$dimred[1]), 
            selectInput("hisleta3drY", "Y-axis:", choices = hisletconf[dimred == TRUE]$UI[6:9], 
                        selected = hisletdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("hisleta3togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.hisleta3togL % 2 == 1", 
          selectInput("hisleta3sub1", "Cell information to subset:", 
                      choices = hisletconf[grp == TRUE]$UI, 
                      selected = hisletdef$grp1), 
          uiOutput("hisleta3sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("hisleta3tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.hisleta3tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("hisleta3siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("hisleta3psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("hisleta3fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("hisleta3asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("hisleta3txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Gene expression 1"), 
        fluidRow( 
          column( 
            6, selectInput("hisleta3inp1", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("hisleta3tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.hisleta3tog1 % 2 == 1", 
              radioButtons("hisleta3col1", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("hisleta3ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("hisleta3oup1.ui"))), 
        downloadButton("hisleta3oup1.pdf", "Download PDF"), 
        downloadButton("hisleta3oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("hisleta3oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("hisleta3oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression 2"), 
        fluidRow( 
          column( 
            6, selectInput("hisleta3inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("hisleta3tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.hisleta3tog2 % 2 == 1", 
              radioButtons("hisleta3col2", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("hisleta3ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("hisleta3oup2.ui"))), 
        downloadButton("hisleta3oup2.pdf", "Download PDF"), 
        downloadButton("hisleta3oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("hisleta3oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("hisleta3oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("hisletb2drX", "X-axis:", choices = hisletconf[dimred == TRUE]$UI[6:9], 
                           selected = hisletdef$dimred[1]), 
           selectInput("hisletb2drY", "Y-axis:", choices = hisletconf[dimred == TRUE]$UI[6:9], 
                       selected = hisletdef$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("hisletb2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.hisletb2togL % 2 == 1", 
         selectInput("hisletb2sub1", "Cell information to subset:", 
                     choices = hisletconf[grp == TRUE]$UI, 
                    selected = hisletdef$grp1), 
         uiOutput("hisletb2sub1.ui") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("hisletb2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.hisletb2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("hisletb2siz", "Point size:", 
                            min = 0, max = 4, value = 1.5, step = 0.25), 
             radioButtons("hisletb2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Large", inline = TRUE), 
             radioButtons("hisletb2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Small", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("hisletb2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("hisletb2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("hisletb2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "Red-Blue colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("hisletb2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "Red-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("hisletb2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.hisletb2tog1 % 2 == 1", 
         radioButtons("hisletb2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("hisletb2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("hisletb2oup1.ui"), 
       downloadButton("hisletb2oup1.pdf", "Download PDF"), 
       downloadButton("hisletb2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("hisletb2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("hisletb2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("hisletb2oup2.ui"), 
       downloadButton("hisletb2oup2.pdf", "Download PDF"), 
       downloadButton("hisletb2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("hisletb2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.c1: violinplot / boxplot 
 tabPanel( 
    HTML("Violinplot / Boxplot"),  
   h4("Cell information / gene expression violin plot / box plot"), 
   "In this tab, users can visualise the gene expression or continuous cell information ",  
   "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
   br(),br(), 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", 
       selectInput("hisletc1inp1", "Cell information (X-axis):", 
                   choices = hisletconf[grp == TRUE]$UI, 
                   selected = hisletdef$grp1) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell information to group cells by",  
                content = c("Select categorical cell information to group cells by",  
                            "- Single cells are grouped by this categorical covariate",  
                            "- Plotted as the X-axis of the violin plot / box plot")),  
       selectInput("hisletc1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell Info / Gene to plot", 
                content = c("Select cell info / gene to plot on Y-axis", 
                            "- Can be continuous cell information (e.g. nUMIs / scores)", 
                            "- Can also be gene expression")), 
       radioButtons("hisletc1typ", "Plot type:", 
                    choices = c("violin", "boxplot"), 
                    selected = "violin", inline = TRUE), 
       checkboxInput("hisletc1pts", "Show data points", value = FALSE), 
       actionButton("hisletc1tog", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.hisletc1tog % 2 == 1", 
         sliderInput("hisletc1siz", "Data point size:",  
                     min = 0, max = 4, value = .4, step = 0.1),  
         radioButtons("hisletc1psz", "Plot size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Large", inline = TRUE), 
         radioButtons("hisletc1fsz", "Font size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Small", inline = TRUE)) 
     ), # End of column (6 space) 
     column(9, uiOutput("hisletc1oup.ui"),  
            downloadButton("hisletc1oup.pdf", "Download PDF"),  
            downloadButton("hisletc1oup.png", "Download PNG"), br(), 
            div(style="display:inline-block", 
                numericInput("hisletc1oup.h", "PDF / PNG height:", width = "138px", 
                             min = 4, max = 20, value = 8, step = 0.5)), 
            div(style="display:inline-block", 
                numericInput("hisletc1oup.w", "PDF / PNG width:", width = "138px", 
                             min = 4, max = 20, value = 10, step = 0.5)) 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
### Tab1.c2: Proportion plot 
tabPanel( 
  HTML("Proportion plot"), 
  h4("Proportion / cell numbers across different cell information"), 
  "In this tab, users can visualise the composition of single cells based on one discrete ", 
  "cell information across another discrete cell information. ",  
  "Usage examples include the library or cellcycle composition across clusters.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("hisletc2inp1", "Cell information to plot (X-axis):", 
                  choices = hisletconf[grp == TRUE]$UI, 
                  selected = hisletdef$grp2) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to plot cells by",  
               content = c("Select categorical cell information to plot cells by", 
                           "- Plotted as the X-axis of the proportion plot")), 
      selectInput("hisletc2inp2", "Cell information to group / colour by:", 
                  choices = hisletconf[grp == TRUE]$UI, 
                  selected = hisletdef$grp1) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group / colour cells by", 
               content = c("Select categorical cell information to group / colour cells by", 
                           "- Proportion / cell numbers are shown in different colours")), 
      radioButtons("hisletc2typ", "Plot value:", 
                   choices = c("Proportion", "CellNumbers"), 
                   selected = "Proportion", inline = TRUE), 
      checkboxInput("hisletc2flp", "Flip X/Y", value = FALSE), 
      actionButton("hisletc2tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.hisletc2tog % 2 == 1", 
        radioButtons("hisletc2psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Large", inline = TRUE), 
        radioButtons("hisletc2fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Small", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("hisletc2oup.ui"),  
           downloadButton("hisletc2oup.pdf", "Download PDF"),  
           downloadButton("hisletc2oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("hisletc2oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("hisletc2oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
),     # End of tab (2 space) 
 
  ### Tab1.d1: Multiple gene expr 
  tabPanel( 
    HTML("Bubbleplot / Heatmap"), 
    h4("Gene expression bubbleplot / heatmap"), 
    "In this tab, users can visualise the gene expression patterns of ", 
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
    "The normalised expression are averaged, log-transformed and then plotted.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, style="border-right: 2px solid black", 
        textAreaInput("hisletd1inp", HTML("List of gene names <br /> 
                                          (Max 50 genes, separated <br /> 
                                           by , or ; or newline):"), 
                      height = "200px", 
                      value = paste0(hisletdef$genes, collapse = ", ")) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "List of genes to plot on bubbleplot / heatmap", 
                 content = c("Input genes to plot", 
                             "- Maximum 50 genes (due to ploting space limitations)", 
                             "- Genes should be separated by comma, semicolon or newline")), 
        selectInput("hisletd1grp", "Group by:", 
                    choices = hisletconf[grp == TRUE]$UI, 
                    selected = hisletconf[grp == TRUE]$UI[1]) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "Cell information to group cells by", 
                 content = c("Select categorical cell information to group cells by", 
                             "- Single cells are grouped by this categorical covariate", 
                             "- Plotted as the X-axis of the bubbleplot / heatmap")), 
        radioButtons("hisletd1plt", "Plot type:", 
                     choices = c("Bubbleplot", "Heatmap"), 
                     selected = "Bubbleplot", inline = TRUE), 
        checkboxInput("hisletd1scl", "Scale gene expression", value = TRUE), 
        checkboxInput("hisletd1row", "Cluster rows (genes)", value = TRUE), 
        checkboxInput("hisletd1col", "Cluster columns (samples)", value = FALSE), 
        br(), 
        actionButton("hisletd1tog", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.hisletd1tog % 2 == 1", 
          radioButtons("hisletd1cols", "Colour scheme:", 
                       choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                   "Yellow-Green-Purple"), 
                       selected = "Blue-Yellow-Red"), 
          radioButtons("hisletd1psz", "Plot size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Large", inline = TRUE), 
          radioButtons("hisletd1fsz", "Font size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Small", inline = TRUE)) 
      ), # End of column (6 space) 
      column(9, h4(htmlOutput("hisletd1oupTxt")), 
             uiOutput("hisletd1oup.ui"), 
             downloadButton("hisletd1oup.pdf", "Download PDF"), 
             downloadButton("hisletd1oup.png", "Download PNG"), br(), 
             div(style="display:inline-block", 
                 numericInput("hisletd1oup.h", "PDF / PNG height:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)), 
             div(style="display:inline-block", 
                 numericInput("hisletd1oup.w", "PDF / PNG width:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  )      # End of tab (2 space) 
   ), 

navbarMenu("Endo",### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo vs GeneExpr"), 
    h4("Cell information vs gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("endoa1drX", "X-axis:", choices = endoconf[dimred == TRUE]$UI, 
                           selected = endodef$dimred[1]), 
            selectInput("endoa1drY", "Y-axis:", choices = endoconf[dimred == TRUE]$UI, 
                        selected = endodef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("endoa1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.endoa1togL % 2 == 1", 
          selectInput("endoa1sub1", "Cell information to subset:", 
                      choices = endoconf[grp == TRUE]$UI, 
                      selected = endodef$grp1), 
          uiOutput("endoa1sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("endoa1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.endoa1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("endoa1siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("endoa1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("endoa1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("endoa1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("endoa1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("endoa1inp1", "Cell information:", 
                           choices = endoconf$UI[1:6], 
                           selected = endodef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("endoa1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.endoa1tog1 % 2 == 1", 
              radioButtons("endoa1col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("endoa1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("endoa1lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("endoa1oup1.ui"))), 
        downloadButton("endoa1oup1.pdf", "Download PDF"), 
        downloadButton("endoa1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("endoa1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("endoa1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("endoa1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.endoa1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("endoa1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("endoa1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("endoa1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("endoa1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.endoa1tog2 % 2 == 1", 
              radioButtons("endoa1col2", "Colour:", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("endoa1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("endoa1oup2.ui"))), 
        downloadButton("endoa1oup2.pdf", "Download PDF"), 
        downloadButton("endoa1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("endoa1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("endoa1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
  ### Tab1.a2: cellInfo vs cellInfo on dimRed 
  tabPanel( 
    HTML("CellInfo vs CellInfo"), 
    h4("Cell information vs cell information on dimension reduction"), 
    "In this tab, users can visualise two cell informations side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("endoa2drX", "X-axis:", choices = endoconf[dimred == TRUE]$UI, 
                           selected = endodef$dimred[1]), 
            selectInput("endoa2drY", "Y-axis:", choices = endoconf[dimred == TRUE]$UI, 
                        selected = endodef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("endoa2togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.endoa2togL % 2 == 1", 
          selectInput("endoa2sub1", "Cell information to subset:", 
                      choices = endoconf[grp == TRUE]$UI, 
                      selected = endodef$grp1), 
          uiOutput("endoa2sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("endoa2tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.endoa2tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("endoa2siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("endoa2psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("endoa2fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("endoa2asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("endoa2txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information 1"), 
        fluidRow( 
          column( 
            6, selectInput("endoa2inp1", "Cell information:", 
                           choices = endoconf$UI[1:6], 
                           selected = endodef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("endoa2tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.endoa2tog1 % 2 == 1", 
              radioButtons("endoa2col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("endoa2ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("endoa2lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("endoa2oup1.ui"))), 
        downloadButton("endoa2oup1.pdf", "Download PDF"), 
        downloadButton("endoa2oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("endoa2oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("endoa2oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Cell information 2"), 
        fluidRow( 
          column( 
            6, selectInput("endoa2inp2", "Cell information:", 
                           choices = endoconf$UI[1:6], 
                           selected = endodef$meta2) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("endoa2tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.endoa2tog2 % 2 == 1", 
              radioButtons("endoa2col2", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("endoa2ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("endoa2lab2", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("endoa2oup2.ui"))), 
        downloadButton("endoa2oup2.pdf", "Download PDF"), 
        downloadButton("endoa2oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("endoa2oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("endoa2oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
   
  ### Tab1.a3: geneExpr vs geneExpr on dimRed 
  tabPanel( 
    HTML("GeneExpr vs GeneExpr"), 
    h4("Gene expression vs gene expression on dimension reduction"), 
    "In this tab, users can visualise two gene expressions side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("endoa3drX", "X-axis:", choices = endoconf[dimred == TRUE]$UI, 
                           selected = endodef$dimred[1]), 
            selectInput("endoa3drY", "Y-axis:", choices = endoconf[dimred == TRUE]$UI, 
                        selected = endodef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("endoa3togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.endoa3togL % 2 == 1", 
          selectInput("endoa3sub1", "Cell information to subset:", 
                      choices = endoconf[grp == TRUE]$UI, 
                      selected = endodef$grp1), 
          uiOutput("endoa3sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("endoa3tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.endoa3tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("endoa3siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("endoa3psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("endoa3fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("endoa3asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("endoa3txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Gene expression 1"), 
        fluidRow( 
          column( 
            6, selectInput("endoa3inp1", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("endoa3tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.endoa3tog1 % 2 == 1", 
              radioButtons("endoa3col1", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("endoa3ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("endoa3oup1.ui"))), 
        downloadButton("endoa3oup1.pdf", "Download PDF"), 
        downloadButton("endoa3oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("endoa3oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("endoa3oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression 2"), 
        fluidRow( 
          column( 
            6, selectInput("endoa3inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("endoa3tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.endoa3tog2 % 2 == 1", 
              radioButtons("endoa3col2", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("endoa3ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("endoa3oup2.ui"))), 
        downloadButton("endoa3oup2.pdf", "Download PDF"), 
        downloadButton("endoa3oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("endoa3oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("endoa3oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("endob2drX", "X-axis:", choices = endoconf[dimred == TRUE]$UI, 
                           selected = endodef$dimred[1]), 
           selectInput("endob2drY", "Y-axis:", choices = endoconf[dimred == TRUE]$UI, 
                       selected = endodef$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("endob2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.endob2togL % 2 == 1", 
         selectInput("endob2sub1", "Cell information to subset:", 
                     choices = endoconf[grp == TRUE]$UI, 
                    selected = endodef$grp1), 
         uiOutput("endob2sub1.ui") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("endob2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.endob2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("endob2siz", "Point size:", 
                            min = 0, max = 4, value = 1.5, step = 0.25), 
             radioButtons("endob2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Large", inline = TRUE), 
             radioButtons("endob2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Small", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("endob2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("endob2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("endob2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "White-Red colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("endob2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "Red-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("endob2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.endob2tog1 % 2 == 1", 
         radioButtons("endob2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("endob2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("endob2oup1.ui"), 
       downloadButton("endob2oup1.pdf", "Download PDF"), 
       downloadButton("endob2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("endob2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("endob2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("endob2oup2.ui"), 
       downloadButton("endob2oup2.pdf", "Download PDF"), 
       downloadButton("endob2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("endob2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.c1: violinplot / boxplot 
 tabPanel( 
    HTML("Violinplot / Boxplot"),  
   h4("Cell information / gene expression violin plot / box plot"), 
   "In this tab, users can visualise the gene expression or continuous cell information ",  
   "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
   br(),br(), 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", 
       selectInput("endoc1inp1", "Cell information (X-axis):", 
                   choices = endoconf[grp == TRUE]$UI, 
                   selected = endodef$grp1) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell information to group cells by",  
                content = c("Select categorical cell information to group cells by",  
                            "- Single cells are grouped by this categorical covariate",  
                            "- Plotted as the X-axis of the violin plot / box plot")),  
       selectInput("endoc1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell Info / Gene to plot", 
                content = c("Select cell info / gene to plot on Y-axis", 
                            "- Can be continuous cell information (e.g. nUMIs / scores)", 
                            "- Can also be gene expression")), 
       radioButtons("endoc1typ", "Plot type:", 
                    choices = c("violin", "boxplot"), 
                    selected = "violin", inline = TRUE), 
       checkboxInput("endoc1pts", "Show data points", value = FALSE), 
       actionButton("endoc1tog", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.endoc1tog % 2 == 1", 
         sliderInput("endoc1siz", "Data point size:",  
                     min = 0, max = 4, value = .4, step = 0.1),  
         radioButtons("endoc1psz", "Plot size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Large", inline = TRUE), 
         radioButtons("endoc1fsz", "Font size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Small", inline = TRUE)) 
     ), # End of column (6 space) 
     column(9, uiOutput("endoc1oup.ui"),  
            downloadButton("endoc1oup.pdf", "Download PDF"),  
            downloadButton("endoc1oup.png", "Download PNG"), br(), 
            div(style="display:inline-block", 
                numericInput("endoc1oup.h", "PDF / PNG height:", width = "138px", 
                             min = 4, max = 20, value = 8, step = 0.5)), 
            div(style="display:inline-block", 
                numericInput("endoc1oup.w", "PDF / PNG width:", width = "138px", 
                             min = 4, max = 20, value = 10, step = 0.5)) 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
### Tab1.c2: Proportion plot 
tabPanel( 
  HTML("Proportion plot"), 
  h4("Proportion / cell numbers across different cell information"), 
  "In this tab, users can visualise the composition of single cells based on one discrete ", 
  "cell information across another discrete cell information. ",  
  "Usage examples include the library or cellcycle composition across clusters.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("endoc2inp1", "Cell information to plot (X-axis):", 
                  choices = endoconf[grp == TRUE]$UI, 
                  selected = endodef$grp2) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to plot cells by",  
               content = c("Select categorical cell information to plot cells by", 
                           "- Plotted as the X-axis of the proportion plot")), 
      selectInput("endoc2inp2", "Cell information to group / colour by:", 
                  choices = endoconf[grp == TRUE]$UI, 
                  selected = endodef$grp1) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group / colour cells by", 
               content = c("Select categorical cell information to group / colour cells by", 
                           "- Proportion / cell numbers are shown in different colours")), 
      radioButtons("endoc2typ", "Plot value:", 
                   choices = c("Proportion", "CellNumbers"), 
                   selected = "Proportion", inline = TRUE), 
      checkboxInput("endoc2flp", "Flip X/Y", value = FALSE), 
      actionButton("endoc2tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.endoc2tog % 2 == 1", 
        radioButtons("endoc2psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Large", inline = TRUE), 
        radioButtons("endoc2fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Small", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("endoc2oup.ui"),  
           downloadButton("endoc2oup.pdf", "Download PDF"),  
           downloadButton("endoc2oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("endoc2oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("endoc2oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
),     # End of tab (2 space) 
 
  ### Tab1.d1: Multiple gene expr 
  tabPanel( 
    HTML("Bubbleplot / Heatmap"), 
    h4("Gene expression bubbleplot / heatmap"), 
    "In this tab, users can visualise the gene expression patterns of ", 
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
    "The normalised expression are averaged, log-transformed and then plotted.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, style="border-right: 2px solid black", 
        textAreaInput("endod1inp", HTML("List of gene names <br /> 
                                          (Max 50 genes, separated <br /> 
                                           by , or ; or newline):"), 
                      height = "200px", 
                      value = paste0(endodef$genes, collapse = ", ")) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "List of genes to plot on bubbleplot / heatmap", 
                 content = c("Input genes to plot", 
                             "- Maximum 50 genes (due to ploting space limitations)", 
                             "- Genes should be separated by comma, semicolon or newline")), 
        selectInput("endod1grp", "Group by:", 
                    choices = endoconf[grp == TRUE]$UI, 
                    selected = endoconf[grp == TRUE]$UI[1]) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "Cell information to group cells by", 
                 content = c("Select categorical cell information to group cells by", 
                             "- Single cells are grouped by this categorical covariate", 
                             "- Plotted as the X-axis of the bubbleplot / heatmap")), 
        radioButtons("endod1plt", "Plot type:", 
                     choices = c("Bubbleplot", "Heatmap"), 
                     selected = "Bubbleplot", inline = TRUE), 
        checkboxInput("endod1scl", "Scale gene expression", value = TRUE), 
        checkboxInput("endod1row", "Cluster rows (genes)", value = TRUE), 
        checkboxInput("endod1col", "Cluster columns (samples)", value = FALSE), 
        br(), 
        actionButton("endod1tog", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.endod1tog % 2 == 1", 
          radioButtons("endod1cols", "Colour scheme:", 
                       choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                   "Yellow-Green-Purple"), 
                       selected = "Blue-Yellow-Red"), 
          radioButtons("endod1psz", "Plot size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Large", inline = TRUE), 
          radioButtons("endod1fsz", "Font size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Small", inline = TRUE)) 
      ), # End of column (6 space) 
      column(9, h4(htmlOutput("endod1oupTxt")), 
             uiOutput("endod1oup.ui"), 
             downloadButton("endod1oup.pdf", "Download PDF"), 
             downloadButton("endod1oup.png", "Download PNG"), br(), 
             div(style="display:inline-block", 
                 numericInput("endod1oup.h", "PDF / PNG height:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)), 
             div(style="display:inline-block", 
                 numericInput("endod1oup.w", "PDF / PNG width:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  )      # End of tab (2 space) 
   ), 

navbarMenu("Alpha",### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo vs GeneExpr"), 
    h4("Cell information vs gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("alphaa1drX", "X-axis:", choices = alphaconf[dimred == TRUE]$UI, 
                           selected = alphadef$dimred[1]), 
            selectInput("alphaa1drY", "Y-axis:", choices = alphaconf[dimred == TRUE]$UI, 
                        selected = alphadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("alphaa1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.alphaa1togL % 2 == 1", 
          selectInput("alphaa1sub1", "Cell information to subset:", 
                      choices = alphaconf[grp == TRUE]$UI, 
                      selected = alphadef$grp1), 
          uiOutput("alphaa1sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("alphaa1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.alphaa1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("alphaa1siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("alphaa1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("alphaa1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("alphaa1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("alphaa1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("alphaa1inp1", "Cell information:", 
                           choices = alphaconf$UI[1:6], 
                           selected = alphadef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("alphaa1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.alphaa1tog1 % 2 == 1", 
              radioButtons("alphaa1col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("alphaa1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("alphaa1lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("alphaa1oup1.ui"))), 
        downloadButton("alphaa1oup1.pdf", "Download PDF"), 
        downloadButton("alphaa1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("alphaa1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("alphaa1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("alphaa1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.alphaa1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("alphaa1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("alphaa1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("alphaa1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("alphaa1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.alphaa1tog2 % 2 == 1", 
              radioButtons("alphaa1col2", "Colour:", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("alphaa1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("alphaa1oup2.ui"))), 
        downloadButton("alphaa1oup2.pdf", "Download PDF"), 
        downloadButton("alphaa1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("alphaa1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("alphaa1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
  ### Tab1.a2: cellInfo vs cellInfo on dimRed 
  tabPanel( 
    HTML("CellInfo vs CellInfo"), 
    h4("Cell information vs cell information on dimension reduction"), 
    "In this tab, users can visualise two cell informations side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("alphaa2drX", "X-axis:", choices = alphaconf[dimred == TRUE]$UI, 
                           selected = alphadef$dimred[1]), 
            selectInput("alphaa2drY", "Y-axis:", choices = alphaconf[dimred == TRUE]$UI, 
                        selected = alphadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("alphaa2togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.alphaa2togL % 2 == 1", 
          selectInput("alphaa2sub1", "Cell information to subset:", 
                      choices = alphaconf[grp == TRUE]$UI, 
                      selected = alphadef$grp1), 
          uiOutput("alphaa2sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("alphaa2tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.alphaa2tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("alphaa2siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("alphaa2psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("alphaa2fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("alphaa2asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("alphaa2txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information 1"), 
        fluidRow( 
          column( 
            6, selectInput("alphaa2inp1", "Cell information:", 
                           choices = alphaconf$UI[1:6], 
                           selected = alphadef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("alphaa2tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.alphaa2tog1 % 2 == 1", 
              radioButtons("alphaa2col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("alphaa2ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("alphaa2lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("alphaa2oup1.ui"))), 
        downloadButton("alphaa2oup1.pdf", "Download PDF"), 
        downloadButton("alphaa2oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("alphaa2oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("alphaa2oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Cell information 2"), 
        fluidRow( 
          column( 
            6, selectInput("alphaa2inp2", "Cell information:", 
                           choices = alphaconf$UI[1:6], 
                           selected = alphadef$meta2) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("alphaa2tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.alphaa2tog2 % 2 == 1", 
              radioButtons("alphaa2col2", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("alphaa2ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("alphaa2lab2", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("alphaa2oup2.ui"))), 
        downloadButton("alphaa2oup2.pdf", "Download PDF"), 
        downloadButton("alphaa2oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("alphaa2oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("alphaa2oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
   
  ### Tab1.a3: geneExpr vs geneExpr on dimRed 
  tabPanel( 
    HTML("GeneExpr vs GeneExpr"), 
    h4("Gene expression vs gene expression on dimension reduction"), 
    "In this tab, users can visualise two gene expressions side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("alphaa3drX", "X-axis:", choices = alphaconf[dimred == TRUE]$UI, 
                           selected = alphadef$dimred[1]), 
            selectInput("alphaa3drY", "Y-axis:", choices = alphaconf[dimred == TRUE]$UI, 
                        selected = alphadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("alphaa3togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.alphaa3togL % 2 == 1", 
          selectInput("alphaa3sub1", "Cell information to subset:", 
                      choices = alphaconf[grp == TRUE]$UI, 
                      selected = alphadef$grp1), 
          uiOutput("alphaa3sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("alphaa3tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.alphaa3tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("alphaa3siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("alphaa3psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("alphaa3fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("alphaa3asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("alphaa3txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Gene expression 1"), 
        fluidRow( 
          column( 
            6, selectInput("alphaa3inp1", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("alphaa3tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.alphaa3tog1 % 2 == 1", 
              radioButtons("alphaa3col1", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("alphaa3ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("alphaa3oup1.ui"))), 
        downloadButton("alphaa3oup1.pdf", "Download PDF"), 
        downloadButton("alphaa3oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("alphaa3oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("alphaa3oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression 2"), 
        fluidRow( 
          column( 
            6, selectInput("alphaa3inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("alphaa3tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.alphaa3tog2 % 2 == 1", 
              radioButtons("alphaa3col2", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("alphaa3ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("alphaa3oup2.ui"))), 
        downloadButton("alphaa3oup2.pdf", "Download PDF"), 
        downloadButton("alphaa3oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("alphaa3oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("alphaa3oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("alphab2drX", "X-axis:", choices = alphaconf[dimred == TRUE]$UI, 
                           selected = alphadef$dimred[1]), 
           selectInput("alphab2drY", "Y-axis:", choices = alphaconf[dimred == TRUE]$UI, 
                       selected = alphadef$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("alphab2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.alphab2togL % 2 == 1", 
         selectInput("alphab2sub1", "Cell information to subset:", 
                     choices = alphaconf[grp == TRUE]$UI, 
                    selected = alphadef$grp1), 
         uiOutput("alphab2sub1.ui") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("alphab2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.alphab2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("alphab2siz", "Point size:", 
                            min = 0, max = 4, value = 1.5, step = 0.25), 
             radioButtons("alphab2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Large", inline = TRUE), 
             radioButtons("alphab2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Small", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("alphab2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("alphab2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("alphab2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "White-Red colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("alphab2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "Red-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("alphab2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.alphab2tog1 % 2 == 1", 
         radioButtons("alphab2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("alphab2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("alphab2oup1.ui"), 
       downloadButton("alphab2oup1.pdf", "Download PDF"), 
       downloadButton("alphab2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("alphab2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("alphab2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("alphab2oup2.ui"), 
       downloadButton("alphab2oup2.pdf", "Download PDF"), 
       downloadButton("alphab2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("alphab2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.c1: violinplot / boxplot 
 tabPanel( 
    HTML("Violinplot / Boxplot"),  
   h4("Cell information / gene expression violin plot / box plot"), 
   "In this tab, users can visualise the gene expression or continuous cell information ",  
   "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
   br(),br(), 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", 
       selectInput("alphac1inp1", "Cell information (X-axis):", 
                   choices = alphaconf[grp == TRUE]$UI, 
                   selected = alphadef$grp1) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell information to group cells by",  
                content = c("Select categorical cell information to group cells by",  
                            "- Single cells are grouped by this categorical covariate",  
                            "- Plotted as the X-axis of the violin plot / box plot")),  
       selectInput("alphac1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell Info / Gene to plot", 
                content = c("Select cell info / gene to plot on Y-axis", 
                            "- Can be continuous cell information (e.g. nUMIs / scores)", 
                            "- Can also be gene expression")), 
       radioButtons("alphac1typ", "Plot type:", 
                    choices = c("violin", "boxplot"), 
                    selected = "violin", inline = TRUE), 
       checkboxInput("alphac1pts", "Show data points", value = FALSE), 
       actionButton("alphac1tog", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.alphac1tog % 2 == 1", 
         sliderInput("alphac1siz", "Data point size:",  
                     min = 0, max = 4, value = .4, step = 0.1),  
         radioButtons("alphac1psz", "Plot size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Large", inline = TRUE), 
         radioButtons("alphac1fsz", "Font size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Small", inline = TRUE)) 
     ), # End of column (6 space) 
     column(9, uiOutput("alphac1oup.ui"),  
            downloadButton("alphac1oup.pdf", "Download PDF"),  
            downloadButton("alphac1oup.png", "Download PNG"), br(), 
            div(style="display:inline-block", 
                numericInput("alphac1oup.h", "PDF / PNG height:", width = "138px", 
                             min = 4, max = 20, value = 8, step = 0.5)), 
            div(style="display:inline-block", 
                numericInput("alphac1oup.w", "PDF / PNG width:", width = "138px", 
                             min = 4, max = 20, value = 10, step = 0.5)) 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
### Tab1.c2: Proportion plot 
tabPanel( 
  HTML("Proportion plot"), 
  h4("Proportion / cell numbers across different cell information"), 
  "In this tab, users can visualise the composition of single cells based on one discrete ", 
  "cell information across another discrete cell information. ",  
  "Usage examples include the library or cellcycle composition across clusters.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("alphac2inp1", "Cell information to plot (X-axis):", 
                  choices = alphaconf[grp == TRUE]$UI, 
                  selected = alphadef$grp2) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to plot cells by",  
               content = c("Select categorical cell information to plot cells by", 
                           "- Plotted as the X-axis of the proportion plot")), 
      selectInput("alphac2inp2", "Cell information to group / colour by:", 
                  choices = alphaconf[grp == TRUE]$UI, 
                  selected = alphadef$grp1) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group / colour cells by", 
               content = c("Select categorical cell information to group / colour cells by", 
                           "- Proportion / cell numbers are shown in different colours")), 
      radioButtons("alphac2typ", "Plot value:", 
                   choices = c("Proportion", "CellNumbers"), 
                   selected = "Proportion", inline = TRUE), 
      checkboxInput("alphac2flp", "Flip X/Y", value = FALSE), 
      actionButton("alphac2tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.alphac2tog % 2 == 1", 
        radioButtons("alphac2psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Large", inline = TRUE), 
        radioButtons("alphac2fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Small", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("alphac2oup.ui"),  
           downloadButton("alphac2oup.pdf", "Download PDF"),  
           downloadButton("alphac2oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("alphac2oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("alphac2oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
),     # End of tab (2 space) 
 
  ### Tab1.d1: Multiple gene expr 
  tabPanel( 
    HTML("Bubbleplot / Heatmap"), 
    h4("Gene expression bubbleplot / heatmap"), 
    "In this tab, users can visualise the gene expression patterns of ", 
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
    "The normalised expression are averaged, log-transformed and then plotted.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, style="border-right: 2px solid black", 
        textAreaInput("alphad1inp", HTML("List of gene names <br /> 
                                          (Max 50 genes, separated <br /> 
                                           by , or ; or newline):"), 
                      height = "200px", 
                      value = paste0(alphadef$genes, collapse = ", ")) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "List of genes to plot on bubbleplot / heatmap", 
                 content = c("Input genes to plot", 
                             "- Maximum 50 genes (due to ploting space limitations)", 
                             "- Genes should be separated by comma, semicolon or newline")), 
        selectInput("alphad1grp", "Group by:", 
                    choices = alphaconf[grp == TRUE]$UI, 
                    selected = alphaconf[grp == TRUE]$UI[1]) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "Cell information to group cells by", 
                 content = c("Select categorical cell information to group cells by", 
                             "- Single cells are grouped by this categorical covariate", 
                             "- Plotted as the X-axis of the bubbleplot / heatmap")), 
        radioButtons("alphad1plt", "Plot type:", 
                     choices = c("Bubbleplot", "Heatmap"), 
                     selected = "Bubbleplot", inline = TRUE), 
        checkboxInput("alphad1scl", "Scale gene expression", value = TRUE), 
        checkboxInput("alphad1row", "Cluster rows (genes)", value = TRUE), 
        checkboxInput("alphad1col", "Cluster columns (samples)", value = FALSE), 
        br(), 
        actionButton("alphad1tog", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.alphad1tog % 2 == 1", 
          radioButtons("alphad1cols", "Colour scheme:", 
                       choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                   "Yellow-Green-Purple"), 
                       selected = "Blue-Yellow-Red"), 
          radioButtons("alphad1psz", "Plot size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Large", inline = TRUE), 
          radioButtons("alphad1fsz", "Font size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Small", inline = TRUE)) 
      ), # End of column (6 space) 
      column(9, h4(htmlOutput("alphad1oupTxt")), 
             uiOutput("alphad1oup.ui"), 
             downloadButton("alphad1oup.pdf", "Download PDF"), 
             downloadButton("alphad1oup.png", "Download PNG"), br(), 
             div(style="display:inline-block", 
                 numericInput("alphad1oup.h", "PDF / PNG height:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)), 
             div(style="display:inline-block", 
                 numericInput("alphad1oup.w", "PDF / PNG width:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  )      # End of tab (2 space) 
   ), 

navbarMenu("Beta",### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo vs GeneExpr"), 
    h4("Cell information vs gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("betaa1drX", "X-axis:", choices = betaconf[dimred == TRUE]$UI, 
                           selected = betadef$dimred[1]), 
            selectInput("betaa1drY", "Y-axis:", choices = betaconf[dimred == TRUE]$UI, 
                        selected = betadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("betaa1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.betaa1togL % 2 == 1", 
          selectInput("betaa1sub1", "Cell information to subset:", 
                      choices = betaconf[grp == TRUE]$UI, 
                      selected = betadef$grp1), 
          uiOutput("betaa1sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("betaa1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.betaa1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("betaa1siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("betaa1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("betaa1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("betaa1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("betaa1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("betaa1inp1", "Cell information:", 
                           choices = betaconf$UI[1:6], 
                           selected = betadef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("betaa1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.betaa1tog1 % 2 == 1", 
              radioButtons("betaa1col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("betaa1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("betaa1lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("betaa1oup1.ui"))), 
        downloadButton("betaa1oup1.pdf", "Download PDF"), 
        downloadButton("betaa1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("betaa1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("betaa1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("betaa1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.betaa1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("betaa1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("betaa1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("betaa1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("betaa1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.betaa1tog2 % 2 == 1", 
              radioButtons("betaa1col2", "Colour:", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("betaa1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("betaa1oup2.ui"))), 
        downloadButton("betaa1oup2.pdf", "Download PDF"), 
        downloadButton("betaa1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("betaa1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("betaa1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
  ### Tab1.a2: cellInfo vs cellInfo on dimRed 
  tabPanel( 
    HTML("CellInfo vs CellInfo"), 
    h4("Cell information vs cell information on dimension reduction"), 
    "In this tab, users can visualise two cell informations side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("betaa2drX", "X-axis:", choices = betaconf[dimred == TRUE]$UI, 
                           selected = betadef$dimred[1]), 
            selectInput("betaa2drY", "Y-axis:", choices = betaconf[dimred == TRUE]$UI, 
                        selected = betadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("betaa2togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.betaa2togL % 2 == 1", 
          selectInput("betaa2sub1", "Cell information to subset:", 
                      choices = betaconf[grp == TRUE]$UI, 
                      selected = betadef$grp1), 
          uiOutput("betaa2sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("betaa2tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.betaa2tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("betaa2siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("betaa2psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("betaa2fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("betaa2asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("betaa2txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information 1"), 
        fluidRow( 
          column( 
            6, selectInput("betaa2inp1", "Cell information:", 
                           choices = betaconf$UI[1:6], 
                           selected = betadef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("betaa2tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.betaa2tog1 % 2 == 1", 
              radioButtons("betaa2col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("betaa2ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("betaa2lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("betaa2oup1.ui"))), 
        downloadButton("betaa2oup1.pdf", "Download PDF"), 
        downloadButton("betaa2oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("betaa2oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("betaa2oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Cell information 2"), 
        fluidRow( 
          column( 
            6, selectInput("betaa2inp2", "Cell information:", 
                           choices = betaconf$UI[1:6], 
                           selected = betadef$meta2) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("betaa2tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.betaa2tog2 % 2 == 1", 
              radioButtons("betaa2col2", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("betaa2ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("betaa2lab2", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("betaa2oup2.ui"))), 
        downloadButton("betaa2oup2.pdf", "Download PDF"), 
        downloadButton("betaa2oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("betaa2oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("betaa2oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
   
  ### Tab1.a3: geneExpr vs geneExpr on dimRed 
  tabPanel( 
    HTML("GeneExpr vs GeneExpr"), 
    h4("Gene expression vs gene expression on dimension reduction"), 
    "In this tab, users can visualise two gene expressions side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("betaa3drX", "X-axis:", choices = betaconf[dimred == TRUE]$UI, 
                           selected = betadef$dimred[1]), 
            selectInput("betaa3drY", "Y-axis:", choices = betaconf[dimred == TRUE]$UI, 
                        selected = betadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("betaa3togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.betaa3togL % 2 == 1", 
          selectInput("betaa3sub1", "Cell information to subset:", 
                      choices = betaconf[grp == TRUE]$UI, 
                      selected = betadef$grp1), 
          uiOutput("betaa3sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("betaa3tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.betaa3tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("betaa3siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("betaa3psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("betaa3fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("betaa3asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("betaa3txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Gene expression 1"), 
        fluidRow( 
          column( 
            6, selectInput("betaa3inp1", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("betaa3tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.betaa3tog1 % 2 == 1", 
              radioButtons("betaa3col1", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("betaa3ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("betaa3oup1.ui"))), 
        downloadButton("betaa3oup1.pdf", "Download PDF"), 
        downloadButton("betaa3oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("betaa3oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("betaa3oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression 2"), 
        fluidRow( 
          column( 
            6, selectInput("betaa3inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("betaa3tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.betaa3tog2 % 2 == 1", 
              radioButtons("betaa3col2", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("betaa3ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("betaa3oup2.ui"))), 
        downloadButton("betaa3oup2.pdf", "Download PDF"), 
        downloadButton("betaa3oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("betaa3oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("betaa3oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("betab2drX", "X-axis:", choices = betaconf[dimred == TRUE]$UI, 
                           selected = betadef$dimred[1]), 
           selectInput("betab2drY", "Y-axis:", choices = betaconf[dimred == TRUE]$UI, 
                       selected = betadef$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("betab2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.betab2togL % 2 == 1", 
         selectInput("betab2sub1", "Cell information to subset:", 
                     choices = betaconf[grp == TRUE]$UI, 
                    selected = betadef$grp1), 
         uiOutput("betab2sub1.ui") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("betab2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.betab2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("betab2siz", "Point size:", 
                            min = 0, max = 4, value = 1.5, step = 0.25), 
             radioButtons("betab2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Large", inline = TRUE), 
             radioButtons("betab2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Small", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("betab2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("betab2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("betab2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "White-Red colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("betab2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "Red-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("betab2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.betab2tog1 % 2 == 1", 
         radioButtons("betab2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("betab2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("betab2oup1.ui"), 
       downloadButton("betab2oup1.pdf", "Download PDF"), 
       downloadButton("betab2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("betab2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("betab2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("betab2oup2.ui"), 
       downloadButton("betab2oup2.pdf", "Download PDF"), 
       downloadButton("betab2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("betab2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.c1: violinplot / boxplot 
 tabPanel( 
    HTML("Violinplot / Boxplot"),  
   h4("Cell information / gene expression violin plot / box plot"), 
   "In this tab, users can visualise the gene expression or continuous cell information ",  
   "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
   br(),br(), 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", 
       selectInput("betac1inp1", "Cell information (X-axis):", 
                   choices = betaconf[grp == TRUE]$UI, 
                   selected = betadef$grp1) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell information to group cells by",  
                content = c("Select categorical cell information to group cells by",  
                            "- Single cells are grouped by this categorical covariate",  
                            "- Plotted as the X-axis of the violin plot / box plot")),  
       selectInput("betac1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell Info / Gene to plot", 
                content = c("Select cell info / gene to plot on Y-axis", 
                            "- Can be continuous cell information (e.g. nUMIs / scores)", 
                            "- Can also be gene expression")), 
       radioButtons("betac1typ", "Plot type:", 
                    choices = c("violin", "boxplot"), 
                    selected = "violin", inline = TRUE), 
       checkboxInput("betac1pts", "Show data points", value = FALSE), 
       actionButton("betac1tog", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.betac1tog % 2 == 1", 
         sliderInput("betac1siz", "Data point size:",  
                     min = 0, max = 4, value = .4, step = 0.1),  
         radioButtons("betac1psz", "Plot size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Large", inline = TRUE), 
         radioButtons("betac1fsz", "Font size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Small", inline = TRUE)) 
     ), # End of column (6 space) 
     column(9, uiOutput("betac1oup.ui"),  
            downloadButton("betac1oup.pdf", "Download PDF"),  
            downloadButton("betac1oup.png", "Download PNG"), br(), 
            div(style="display:inline-block", 
                numericInput("betac1oup.h", "PDF / PNG height:", width = "138px", 
                             min = 4, max = 20, value = 8, step = 0.5)), 
            div(style="display:inline-block", 
                numericInput("betac1oup.w", "PDF / PNG width:", width = "138px", 
                             min = 4, max = 20, value = 10, step = 0.5)) 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
### Tab1.c2: Proportion plot 
tabPanel( 
  HTML("Proportion plot"), 
  h4("Proportion / cell numbers across different cell information"), 
  "In this tab, users can visualise the composition of single cells based on one discrete ", 
  "cell information across another discrete cell information. ",  
  "Usage examples include the library or cellcycle composition across clusters.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("betac2inp1", "Cell information to plot (X-axis):", 
                  choices = betaconf[grp == TRUE]$UI, 
                  selected = betadef$grp2) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to plot cells by",  
               content = c("Select categorical cell information to plot cells by", 
                           "- Plotted as the X-axis of the proportion plot")), 
      selectInput("betac2inp2", "Cell information to group / colour by:", 
                  choices = betaconf[grp == TRUE]$UI, 
                  selected = betadef$grp1) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group / colour cells by", 
               content = c("Select categorical cell information to group / colour cells by", 
                           "- Proportion / cell numbers are shown in different colours")), 
      radioButtons("betac2typ", "Plot value:", 
                   choices = c("Proportion", "CellNumbers"), 
                   selected = "Proportion", inline = TRUE), 
      checkboxInput("betac2flp", "Flip X/Y", value = FALSE), 
      actionButton("betac2tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.betac2tog % 2 == 1", 
        radioButtons("betac2psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Large", inline = TRUE), 
        radioButtons("betac2fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Small", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("betac2oup.ui"),  
           downloadButton("betac2oup.pdf", "Download PDF"),  
           downloadButton("betac2oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("betac2oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("betac2oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
),     # End of tab (2 space) 
 
  ### Tab1.d1: Multiple gene expr 
  tabPanel( 
    HTML("Bubbleplot / Heatmap"), 
    h4("Gene expression bubbleplot / heatmap"), 
    "In this tab, users can visualise the gene expression patterns of ", 
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
    "The normalised expression are averaged, log-transformed and then plotted.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, style="border-right: 2px solid black", 
        textAreaInput("betad1inp", HTML("List of gene names <br /> 
                                          (Max 50 genes, separated <br /> 
                                           by , or ; or newline):"), 
                      height = "200px", 
                      value = paste0(betadef$genes, collapse = ", ")) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "List of genes to plot on bubbleplot / heatmap", 
                 content = c("Input genes to plot", 
                             "- Maximum 50 genes (due to ploting space limitations)", 
                             "- Genes should be separated by comma, semicolon or newline")), 
        selectInput("betad1grp", "Group by:", 
                    choices = betaconf[grp == TRUE]$UI, 
                    selected = betaconf[grp == TRUE]$UI[1]) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "Cell information to group cells by", 
                 content = c("Select categorical cell information to group cells by", 
                             "- Single cells are grouped by this categorical covariate", 
                             "- Plotted as the X-axis of the bubbleplot / heatmap")), 
        radioButtons("betad1plt", "Plot type:", 
                     choices = c("Bubbleplot", "Heatmap"), 
                     selected = "Bubbleplot", inline = TRUE), 
        checkboxInput("betad1scl", "Scale gene expression", value = TRUE), 
        checkboxInput("betad1row", "Cluster rows (genes)", value = TRUE), 
        checkboxInput("betad1col", "Cluster columns (samples)", value = FALSE), 
        br(), 
        actionButton("betad1tog", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.betad1tog % 2 == 1", 
          radioButtons("betad1cols", "Colour scheme:", 
                       choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                   "Yellow-Green-Purple"), 
                       selected = "Blue-Yellow-Red"), 
          radioButtons("betad1psz", "Plot size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Large", inline = TRUE), 
          radioButtons("betad1fsz", "Font size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Small", inline = TRUE)) 
      ), # End of column (6 space) 
      column(9, h4(htmlOutput("betad1oupTxt")), 
             uiOutput("betad1oup.ui"), 
             downloadButton("betad1oup.pdf", "Download PDF"), 
             downloadButton("betad1oup.png", "Download PNG"), br(), 
             div(style="display:inline-block", 
                 numericInput("betad1oup.h", "PDF / PNG height:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)), 
             div(style="display:inline-block", 
                 numericInput("betad1oup.w", "PDF / PNG width:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  )      # End of tab (2 space) 
   ), 

navbarMenu("Delta",### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo vs GeneExpr"), 
    h4("Cell information vs gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("deltaa1drX", "X-axis:", choices = deltaconf[dimred == TRUE]$UI, 
                           selected = deltadef$dimred[1]), 
            selectInput("deltaa1drY", "Y-axis:", choices = deltaconf[dimred == TRUE]$UI, 
                        selected = deltadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("deltaa1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.deltaa1togL % 2 == 1", 
          selectInput("deltaa1sub1", "Cell information to subset:", 
                      choices = deltaconf[grp == TRUE]$UI, 
                      selected = deltadef$grp1), 
          uiOutput("deltaa1sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("deltaa1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.deltaa1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("deltaa1siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("deltaa1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("deltaa1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("deltaa1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("deltaa1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("deltaa1inp1", "Cell information:", 
                           choices = deltaconf$UI[1:6], 
                           selected = deltadef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("deltaa1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.deltaa1tog1 % 2 == 1", 
              radioButtons("deltaa1col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("deltaa1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("deltaa1lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("deltaa1oup1.ui"))), 
        downloadButton("deltaa1oup1.pdf", "Download PDF"), 
        downloadButton("deltaa1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("deltaa1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("deltaa1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("deltaa1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.deltaa1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("deltaa1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("deltaa1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("deltaa1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("deltaa1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.deltaa1tog2 % 2 == 1", 
              radioButtons("deltaa1col2", "Colour:", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("deltaa1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("deltaa1oup2.ui"))), 
        downloadButton("deltaa1oup2.pdf", "Download PDF"), 
        downloadButton("deltaa1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("deltaa1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("deltaa1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
  ### Tab1.a2: cellInfo vs cellInfo on dimRed 
  tabPanel( 
    HTML("CellInfo vs CellInfo"), 
    h4("Cell information vs cell information on dimension reduction"), 
    "In this tab, users can visualise two cell informations side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("deltaa2drX", "X-axis:", choices = deltaconf[dimred == TRUE]$UI, 
                           selected = deltadef$dimred[1]), 
            selectInput("deltaa2drY", "Y-axis:", choices = deltaconf[dimred == TRUE]$UI, 
                        selected = deltadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("deltaa2togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.deltaa2togL % 2 == 1", 
          selectInput("deltaa2sub1", "Cell information to subset:", 
                      choices = deltaconf[grp == TRUE]$UI, 
                      selected = deltadef$grp1), 
          uiOutput("deltaa2sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("deltaa2tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.deltaa2tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("deltaa2siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("deltaa2psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("deltaa2fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("deltaa2asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("deltaa2txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information 1"), 
        fluidRow( 
          column( 
            6, selectInput("deltaa2inp1", "Cell information:", 
                           choices = deltaconf$UI[1:6], 
                           selected = deltadef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("deltaa2tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.deltaa2tog1 % 2 == 1", 
              radioButtons("deltaa2col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("deltaa2ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("deltaa2lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("deltaa2oup1.ui"))), 
        downloadButton("deltaa2oup1.pdf", "Download PDF"), 
        downloadButton("deltaa2oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("deltaa2oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("deltaa2oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Cell information 2"), 
        fluidRow( 
          column( 
            6, selectInput("deltaa2inp2", "Cell information:", 
                           choices = deltaconf$UI[1:6], 
                           selected = deltadef$meta2) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("deltaa2tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.deltaa2tog2 % 2 == 1", 
              radioButtons("deltaa2col2", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("deltaa2ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("deltaa2lab2", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("deltaa2oup2.ui"))), 
        downloadButton("deltaa2oup2.pdf", "Download PDF"), 
        downloadButton("deltaa2oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("deltaa2oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("deltaa2oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
   
  ### Tab1.a3: geneExpr vs geneExpr on dimRed 
  tabPanel( 
    HTML("GeneExpr vs GeneExpr"), 
    h4("Gene expression vs gene expression on dimension reduction"), 
    "In this tab, users can visualise two gene expressions side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("deltaa3drX", "X-axis:", choices = deltaconf[dimred == TRUE]$UI, 
                           selected = deltadef$dimred[1]), 
            selectInput("deltaa3drY", "Y-axis:", choices = deltaconf[dimred == TRUE]$UI, 
                        selected = deltadef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("deltaa3togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.deltaa3togL % 2 == 1", 
          selectInput("deltaa3sub1", "Cell information to subset:", 
                      choices = deltaconf[grp == TRUE]$UI, 
                      selected = deltadef$grp1), 
          uiOutput("deltaa3sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("deltaa3tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.deltaa3tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("deltaa3siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("deltaa3psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("deltaa3fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("deltaa3asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("deltaa3txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Gene expression 1"), 
        fluidRow( 
          column( 
            6, selectInput("deltaa3inp1", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("deltaa3tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.deltaa3tog1 % 2 == 1", 
              radioButtons("deltaa3col1", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("deltaa3ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("deltaa3oup1.ui"))), 
        downloadButton("deltaa3oup1.pdf", "Download PDF"), 
        downloadButton("deltaa3oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("deltaa3oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("deltaa3oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression 2"), 
        fluidRow( 
          column( 
            6, selectInput("deltaa3inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("deltaa3tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.deltaa3tog2 % 2 == 1", 
              radioButtons("deltaa3col2", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("deltaa3ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("deltaa3oup2.ui"))), 
        downloadButton("deltaa3oup2.pdf", "Download PDF"), 
        downloadButton("deltaa3oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("deltaa3oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("deltaa3oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("deltab2drX", "X-axis:", choices = deltaconf[dimred == TRUE]$UI, 
                           selected = deltadef$dimred[1]), 
           selectInput("deltab2drY", "Y-axis:", choices = deltaconf[dimred == TRUE]$UI, 
                       selected = deltadef$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("deltab2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.deltab2togL % 2 == 1", 
         selectInput("deltab2sub1", "Cell information to subset:", 
                     choices = deltaconf[grp == TRUE]$UI, 
                    selected = deltadef$grp1), 
         uiOutput("deltab2sub1.ui") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("deltab2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.deltab2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("deltab2siz", "Point size:", 
                            min = 0, max = 4, value = 1.5, step = 0.25), 
             radioButtons("deltab2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Large", inline = TRUE), 
             radioButtons("deltab2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Small", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("deltab2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("deltab2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("deltab2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "White-Red colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("deltab2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "Red-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("deltab2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.deltab2tog1 % 2 == 1", 
         radioButtons("deltab2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("deltab2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("deltab2oup1.ui"), 
       downloadButton("deltab2oup1.pdf", "Download PDF"), 
       downloadButton("deltab2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("deltab2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("deltab2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("deltab2oup2.ui"), 
       downloadButton("deltab2oup2.pdf", "Download PDF"), 
       downloadButton("deltab2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("deltab2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.c1: violinplot / boxplot 
 tabPanel( 
    HTML("Violinplot / Boxplot"),  
   h4("Cell information / gene expression violin plot / box plot"), 
   "In this tab, users can visualise the gene expression or continuous cell information ",  
   "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
   br(),br(), 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", 
       selectInput("deltac1inp1", "Cell information (X-axis):", 
                   choices = deltaconf[grp == TRUE]$UI, 
                   selected = deltadef$grp1) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell information to group cells by",  
                content = c("Select categorical cell information to group cells by",  
                            "- Single cells are grouped by this categorical covariate",  
                            "- Plotted as the X-axis of the violin plot / box plot")),  
       selectInput("deltac1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell Info / Gene to plot", 
                content = c("Select cell info / gene to plot on Y-axis", 
                            "- Can be continuous cell information (e.g. nUMIs / scores)", 
                            "- Can also be gene expression")), 
       radioButtons("deltac1typ", "Plot type:", 
                    choices = c("violin", "boxplot"), 
                    selected = "violin", inline = TRUE), 
       checkboxInput("deltac1pts", "Show data points", value = FALSE), 
       actionButton("deltac1tog", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.deltac1tog % 2 == 1", 
         sliderInput("deltac1siz", "Data point size:",  
                     min = 0, max = 4, value = .4, step = 0.1),  
         radioButtons("deltac1psz", "Plot size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Large", inline = TRUE), 
         radioButtons("deltac1fsz", "Font size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Small", inline = TRUE)) 
     ), # End of column (6 space) 
     column(9, uiOutput("deltac1oup.ui"),  
            downloadButton("deltac1oup.pdf", "Download PDF"),  
            downloadButton("deltac1oup.png", "Download PNG"), br(), 
            div(style="display:inline-block", 
                numericInput("deltac1oup.h", "PDF / PNG height:", width = "138px", 
                             min = 4, max = 20, value = 8, step = 0.5)), 
            div(style="display:inline-block", 
                numericInput("deltac1oup.w", "PDF / PNG width:", width = "138px", 
                             min = 4, max = 20, value = 10, step = 0.5)) 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
### Tab1.c2: Proportion plot 
tabPanel( 
  HTML("Proportion plot"), 
  h4("Proportion / cell numbers across different cell information"), 
  "In this tab, users can visualise the composition of single cells based on one discrete ", 
  "cell information across another discrete cell information. ",  
  "Usage examples include the library or cellcycle composition across clusters.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("deltac2inp1", "Cell information to plot (X-axis):", 
                  choices = deltaconf[grp == TRUE]$UI, 
                  selected = deltadef$grp2) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to plot cells by",  
               content = c("Select categorical cell information to plot cells by", 
                           "- Plotted as the X-axis of the proportion plot")), 
      selectInput("deltac2inp2", "Cell information to group / colour by:", 
                  choices = deltaconf[grp == TRUE]$UI, 
                  selected = deltadef$grp1) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group / colour cells by", 
               content = c("Select categorical cell information to group / colour cells by", 
                           "- Proportion / cell numbers are shown in different colours")), 
      radioButtons("deltac2typ", "Plot value:", 
                   choices = c("Proportion", "CellNumbers"), 
                   selected = "Proportion", inline = TRUE), 
      checkboxInput("deltac2flp", "Flip X/Y", value = FALSE), 
      actionButton("deltac2tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.deltac2tog % 2 == 1", 
        radioButtons("deltac2psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Large", inline = TRUE), 
        radioButtons("deltac2fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Small", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("deltac2oup.ui"),  
           downloadButton("deltac2oup.pdf", "Download PDF"),  
           downloadButton("deltac2oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("deltac2oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("deltac2oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
),     # End of tab (2 space) 
 
  ### Tab1.d1: Multiple gene expr 
  tabPanel( 
    HTML("Bubbleplot / Heatmap"), 
    h4("Gene expression bubbleplot / heatmap"), 
    "In this tab, users can visualise the gene expression patterns of ", 
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
    "The normalised expression are averaged, log-transformed and then plotted.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, style="border-right: 2px solid black", 
        textAreaInput("deltad1inp", HTML("List of gene names <br /> 
                                          (Max 50 genes, separated <br /> 
                                           by , or ; or newline):"), 
                      height = "200px", 
                      value = paste0(deltadef$genes, collapse = ", ")) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "List of genes to plot on bubbleplot / heatmap", 
                 content = c("Input genes to plot", 
                             "- Maximum 50 genes (due to ploting space limitations)", 
                             "- Genes should be separated by comma, semicolon or newline")), 
        selectInput("deltad1grp", "Group by:", 
                    choices = deltaconf[grp == TRUE]$UI, 
                    selected = deltaconf[grp == TRUE]$UI[1]) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "Cell information to group cells by", 
                 content = c("Select categorical cell information to group cells by", 
                             "- Single cells are grouped by this categorical covariate", 
                             "- Plotted as the X-axis of the bubbleplot / heatmap")), 
        radioButtons("deltad1plt", "Plot type:", 
                     choices = c("Bubbleplot", "Heatmap"), 
                     selected = "Bubbleplot", inline = TRUE), 
        checkboxInput("deltad1scl", "Scale gene expression", value = TRUE), 
        checkboxInput("deltad1row", "Cluster rows (genes)", value = TRUE), 
        checkboxInput("deltad1col", "Cluster columns (samples)", value = FALSE), 
        br(), 
        actionButton("deltad1tog", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.deltad1tog % 2 == 1", 
          radioButtons("deltad1cols", "Colour scheme:", 
                       choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                   "Yellow-Green-Purple"), 
                       selected = "Blue-Yellow-Red"), 
          radioButtons("deltad1psz", "Plot size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Large", inline = TRUE), 
          radioButtons("deltad1fsz", "Font size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Small", inline = TRUE)) 
      ), # End of column (6 space) 
      column(9, h4(htmlOutput("deltad1oupTxt")), 
             uiOutput("deltad1oup.ui"), 
             downloadButton("deltad1oup.pdf", "Download PDF"), 
             downloadButton("deltad1oup.png", "Download PNG"), br(), 
             div(style="display:inline-block", 
                 numericInput("deltad1oup.h", "PDF / PNG height:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)), 
             div(style="display:inline-block", 
                 numericInput("deltad1oup.w", "PDF / PNG width:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  )      # End of tab (2 space) 
   ), 

navbarMenu("Multihormonal",### Tab1.a1: cellInfo vs geneExpr on dimRed 
  tabPanel( 
    HTML("CellInfo vs GeneExpr"), 
    h4("Cell information vs gene expression on reduced dimensions"), 
    "In this tab, users can visualise both cell information and gene ",  
    "expression side-by-side on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("dpa1drX", "X-axis:", choices = dpconf[dimred == TRUE]$UI, 
                           selected = dpdef$dimred[1]), 
            selectInput("dpa1drY", "Y-axis:", choices = dpconf[dimred == TRUE]$UI, 
                        selected = dpdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("dpa1togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.dpa1togL % 2 == 1", 
          selectInput("dpa1sub1", "Cell information to subset:", 
                      choices = dpconf[grp == TRUE]$UI, 
                      selected = dpdef$grp1), 
          uiOutput("dpa1sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("dpa1tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.dpa1tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("dpa1siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("dpa1psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("dpa1fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("dpa1asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("dpa1txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information"), 
        fluidRow( 
          column( 
            6, selectInput("dpa1inp1", "Cell information:", 
                           choices = dpconf$UI[1:6], 
                           selected = dpdef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("dpa1tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.dpa1tog1 % 2 == 1", 
              radioButtons("dpa1col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("dpa1ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("dpa1lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("dpa1oup1.ui"))), 
        downloadButton("dpa1oup1.pdf", "Download PDF"), 
        downloadButton("dpa1oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("dpa1oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("dpa1oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)), br(), 
        actionButton("dpa1tog9", "Toggle to show cell numbers / statistics"), 
        conditionalPanel( 
          condition = "input.dpa1tog9 % 2 == 1", 
          h4("Cell numbers / statistics"), 
          radioButtons("dpa1splt", "Split continuous cell info into:", 
                       choices = c("Quartile", "Decile"), 
                       selected = "Decile", inline = TRUE), 
          dataTableOutput("dpa1.dt") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression"), 
        fluidRow( 
          column( 
            6, selectInput("dpa1inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("dpa1tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.dpa1tog2 % 2 == 1", 
              radioButtons("dpa1col2", "Colour:", 
                           choices = c("Purple Gradient","Blue-Yellow-Red","Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("dpa1ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ) , 
        fluidRow(column(12, uiOutput("dpa1oup2.ui"))), 
        downloadButton("dpa1oup2.pdf", "Download PDF"), 
        downloadButton("dpa1oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("dpa1oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("dpa1oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
  ### Tab1.a2: cellInfo vs cellInfo on dimRed 
  tabPanel( 
    HTML("CellInfo vs CellInfo"), 
    h4("Cell information vs cell information on dimension reduction"), 
    "In this tab, users can visualise two cell informations side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("dpa2drX", "X-axis:", choices = dpconf[dimred == TRUE]$UI, 
                           selected = dpdef$dimred[1]), 
            selectInput("dpa2drY", "Y-axis:", choices = dpconf[dimred == TRUE]$UI, 
                        selected = dpdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("dpa2togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.dpa2togL % 2 == 1", 
          selectInput("dpa2sub1", "Cell information to subset:", 
                      choices = dpconf[grp == TRUE]$UI, 
                      selected = dpdef$grp1), 
          uiOutput("dpa2sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("dpa2tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.dpa2tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("dpa2siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("dpa2psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("dpa2fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("dpa2asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("dpa2txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Cell information 1"), 
        fluidRow( 
          column( 
            6, selectInput("dpa2inp1", "Cell information:", 
                           choices = dpconf$UI[1:6], 
                           selected = dpdef$meta1) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("dpa2tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.dpa2tog1 % 2 == 1", 
              radioButtons("dpa2col1", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("dpa2ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("dpa2lab1", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("dpa2oup1.ui"))), 
        downloadButton("dpa2oup1.pdf", "Download PDF"), 
        downloadButton("dpa2oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("dpa2oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("dpa2oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Cell information 2"), 
        fluidRow( 
          column( 
            6, selectInput("dpa2inp2", "Cell information:", 
                           choices = dpconf$UI[1:6], 
                           selected = dpdef$meta2) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Cell information to colour cells by", 
                     content = c("Select cell information to colour cells", 
                                 "- Categorical covariates have a fixed colour palette", 
                                 paste0("- Continuous covariates are coloured in a ",  
                                        "Blue-Yellow-Red colour scheme, which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("dpa2tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.dpa2tog2 % 2 == 1", 
              radioButtons("dpa2col2", "Colour (Continuous data):", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Blue-Yellow-Red"), 
              radioButtons("dpa2ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Original", inline = TRUE), 
              checkboxInput("dpa2lab2", "Show cell info labels", value = FALSE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("dpa2oup2.ui"))), 
        downloadButton("dpa2oup2.pdf", "Download PDF"), 
        downloadButton("dpa2oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("dpa2oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("dpa2oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
   
  ### Tab1.a3: geneExpr vs geneExpr on dimRed 
  tabPanel( 
    HTML("GeneExpr vs GeneExpr"), 
    h4("Gene expression vs gene expression on dimension reduction"), 
    "In this tab, users can visualise two gene expressions side-by-side ", 
    "on low-dimensional representions.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, h4("Dimension Reduction"), 
        fluidRow( 
          column( 
            12, selectInput("dpa3drX", "X-axis:", choices = dpconf[dimred == TRUE]$UI, 
                           selected = dpdef$dimred[1]), 
            selectInput("dpa3drY", "Y-axis:", choices = dpconf[dimred == TRUE]$UI, 
                        selected = dpdef$dimred[2])) 
        ) 
      ), # End of column (6 space) 
      column( 
        3, actionButton("dpa3togL", "Toggle to subset cells"), 
        conditionalPanel( 
          condition = "input.dpa3togL % 2 == 1", 
          selectInput("dpa3sub1", "Cell information to subset:", 
                      choices = dpconf[grp == TRUE]$UI, 
                      selected = dpdef$grp1), 
          uiOutput("dpa3sub1.ui") 
        ) 
      ), # End of column (6 space) 
      column( 
        6, actionButton("dpa3tog0", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.dpa3tog0 % 2 == 1", 
          fluidRow( 
            column( 
              6, sliderInput("dpa3siz", "Point size:", 
                             min = 0, max = 4, value = .4, step = 0.1), 
              radioButtons("dpa3psz", "Plot size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Large", inline = TRUE), 
              radioButtons("dpa3fsz", "Font size:", 
                           choices = c("Small", "Medium", "Large"), 
                           selected = "Small", inline = TRUE) 
            ), 
            column( 
              6, radioButtons("dpa3asp", "Aspect ratio:", 
                              choices = c("Square", "Fixed", "Free"), 
                              selected = "Square", inline = TRUE), 
              checkboxInput("dpa3txt", "Show axis text", value = FALSE) 
            ) 
          ) 
        ) 
      )  # End of column (6 space) 
    ),   # End of fluidRow (4 space) 
    fluidRow( 
      column( 
        6, style="border-right: 2px solid black", h4("Gene expression 1"), 
        fluidRow( 
          column( 
            6, selectInput("dpa3inp1", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("dpa3tog1", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.dpa3tog1 % 2 == 1", 
              radioButtons("dpa3col1", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("dpa3ord1", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("dpa3oup1.ui"))), 
        downloadButton("dpa3oup1.pdf", "Download PDF"), 
        downloadButton("dpa3oup1.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("dpa3oup1.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("dpa3oup1.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      ), # End of column (6 space) 
      column( 
        6, h4("Gene expression 2"), 
        fluidRow( 
          column( 
            6, selectInput("dpa3inp2", "Gene name:", choices=NULL) %>%  
              helper(type = "inline", size = "m", fade = TRUE, 
                     title = "Gene expression to colour cells by", 
                     content = c("Select gene to colour cells by gene expression", 
                                 paste0("- Gene expression are coloured in a ", 
                                        "White-Red colour scheme which can be ", 
                                        "changed in the plot controls"))) 
          ), 
          column( 
            6, actionButton("dpa3tog2", "Toggle plot controls"), 
            conditionalPanel( 
              condition = "input.dpa3tog2 % 2 == 1", 
              radioButtons("dpa3col2", "Colour:", 
                           choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                       "Yellow-Green-Purple"), 
                           selected = "Purple Gradient"), 
              radioButtons("dpa3ord2", "Plot order:", 
                           choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                           selected = "Max-1st", inline = TRUE) 
            ) 
          ) 
        ), 
        fluidRow(column(12, uiOutput("dpa3oup2.ui"))), 
        downloadButton("dpa3oup2.pdf", "Download PDF"), 
        downloadButton("dpa3oup2.png", "Download PNG"), br(), 
        div(style="display:inline-block", 
            numericInput("dpa3oup2.h", "PDF / PNG height:", width = "138px", 
                         min = 4, max = 20, value = 6, step = 0.5)), 
        div(style="display:inline-block", 
            numericInput("dpa3oup2.w", "PDF / PNG width:", width = "138px", 
                         min = 4, max = 20, value = 8, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  ),     # End of tab (2 space) 
 
 ### Tab1.b2: Gene coexpression plot 
 tabPanel( 
   HTML("Gene coexpression"), 
   h4("Coexpression of two genes on reduced dimensions"), 
   "In this tab, users can visualise the coexpression of two genes ", 
   "on low-dimensional representions.", 
   br(),br(), 
   fluidRow( 
     column( 
       3, h4("Dimension Reduction"), 
       fluidRow( 
         column( 
           12, selectInput("dpb2drX", "X-axis:", choices = dpconf[dimred == TRUE]$UI, 
                           selected = dpdef$dimred[1]), 
           selectInput("dpb2drY", "Y-axis:", choices = dpconf[dimred == TRUE]$UI, 
                       selected = dpdef$dimred[2])) 
       ) 
     ), # End of column (6 space) 
     column( 
       3, actionButton("dpb2togL", "Toggle to subset cells"), 
       conditionalPanel( 
         condition = "input.dpb2togL % 2 == 1", 
         selectInput("dpb2sub1", "Cell information to subset:", 
                     choices = dpconf[grp == TRUE]$UI, 
                    selected = dpdef$grp1), 
         uiOutput("dpb2sub1.ui") 
       ) 
     ), # End of column (6 space) 
     column( 
       6, actionButton("dpb2tog0", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.dpb2tog0 % 2 == 1", 
         fluidRow( 
           column( 
             6, sliderInput("dpb2siz", "Point size:", 
                            min = 0, max = 4, value = 1.5, step = 0.25), 
             radioButtons("dpb2psz", "Plot size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Large", inline = TRUE), 
             radioButtons("dpb2fsz", "Font size:", 
                          choices = c("Small", "Medium", "Large"), 
                          selected = "Small", inline = TRUE) 
           ), 
           column( 
             6, radioButtons("dpb2asp", "Aspect ratio:", 
                             choices = c("Square", "Fixed", "Free"), 
                             selected = "Square", inline = TRUE), 
             checkboxInput("dpb2txt", "Show axis text", value = FALSE) 
           ) 
         ) 
       ) 
     )  # End of column (6 space) 
   ),   # End of fluidRow (4 space) 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", h4("Gene Expression"), 
       selectInput("dpb2inp1", "Gene 1:", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
               title = "Gene expression to colour cells by", 
               content = c("Select gene to colour cells by gene expression", 
                          paste0("- Gene expression are coloured in a ", 
                                 "White-Red colour scheme which can be ", 
                                 "changed in the plot controls"))), 
       selectInput("dpb2inp2", "Gene 2:", choices=NULL) %>% 
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Gene expression to colour cells by", 
                content = c("Select gene to colour cells by gene expression", 
                            paste0("- Gene expression are coloured in a ", 
                                   "Red-Blue colour scheme which can be ", 
                                   "changed in the plot controls"))), 
       actionButton("dpb2tog1", "Toggle plot controls"), 
       conditionalPanel( 
         condition = "input.dpb2tog1 % 2 == 1", 
         radioButtons("dpb2col1", "Colour:", 
                      choices = c("Red (Gene1); Blue (Gene2)", 
                                  "Orange (Gene1); Blue (Gene2)", 
                                  "Red (Gene1); Green (Gene2)", 
                                  "Green (Gene1); Blue (Gene2)"), 
                      selected = "Red (Gene1); Blue (Gene2)"), 
         radioButtons("dpb2ord1", "Plot order:", 
                      choices = c("Max-1st", "Min-1st", "Original", "Random"), 
                      selected = "Max-1st", inline = TRUE) 
       ) 
     ), # End of column (6 space) 
     column( 
       6, style="border-right: 2px solid black", 
       uiOutput("dpb2oup1.ui"), 
       downloadButton("dpb2oup1.pdf", "Download PDF"), 
       downloadButton("dpb2oup1.png", "Download PNG"), br(), 
       div(style="display:inline-block", 
           numericInput("dpb2oup1.h", "PDF / PNG height:", width = "138px", 
                        min = 4, max = 20, value = 8, step = 0.5)), 
       div(style="display:inline-block", 
           numericInput("dpb2oup1.w", "PDF / PNG width:", width = "138px", 
                        min = 4, max = 20, value = 10, step = 0.5)) 
     ), # End of column (6 space) 
     column( 
       3, uiOutput("dpb2oup2.ui"), 
       downloadButton("dpb2oup2.pdf", "Download PDF"), 
       downloadButton("dpb2oup2.png", "Download PNG"), 
       br(), h4("Cell numbers"), 
       dataTableOutput("dpb2.dt") 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
 ### Tab1.c1: violinplot / boxplot 
 tabPanel( 
    HTML("Violinplot / Boxplot"),  
   h4("Cell information / gene expression violin plot / box plot"), 
   "In this tab, users can visualise the gene expression or continuous cell information ",  
   "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).", 
   br(),br(), 
   fluidRow( 
     column( 
       3, style="border-right: 2px solid black", 
       selectInput("dpc1inp1", "Cell information (X-axis):", 
                   choices = dpconf[grp == TRUE]$UI, 
                   selected = dpdef$grp1) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell information to group cells by",  
                content = c("Select categorical cell information to group cells by",  
                            "- Single cells are grouped by this categorical covariate",  
                            "- Plotted as the X-axis of the violin plot / box plot")),  
       selectInput("dpc1inp2", "Cell Info / Gene name (Y-axis):", choices=NULL) %>%  
         helper(type = "inline", size = "m", fade = TRUE, 
                title = "Cell Info / Gene to plot", 
                content = c("Select cell info / gene to plot on Y-axis", 
                            "- Can be continuous cell information (e.g. nUMIs / scores)", 
                            "- Can also be gene expression")), 
       radioButtons("dpc1typ", "Plot type:", 
                    choices = c("violin", "boxplot"), 
                    selected = "violin", inline = TRUE), 
       checkboxInput("dpc1pts", "Show data points", value = FALSE), 
       actionButton("dpc1tog", "Toggle graphics controls"), 
       conditionalPanel( 
         condition = "input.dpc1tog % 2 == 1", 
         sliderInput("dpc1siz", "Data point size:",  
                     min = 0, max = 4, value = .4, step = 0.1),  
         radioButtons("dpc1psz", "Plot size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Large", inline = TRUE), 
         radioButtons("dpc1fsz", "Font size:", 
                      choices = c("Small", "Medium", "Large"), 
                      selected = "Small", inline = TRUE)) 
     ), # End of column (6 space) 
     column(9, uiOutput("dpc1oup.ui"),  
            downloadButton("dpc1oup.pdf", "Download PDF"),  
            downloadButton("dpc1oup.png", "Download PNG"), br(), 
            div(style="display:inline-block", 
                numericInput("dpc1oup.h", "PDF / PNG height:", width = "138px", 
                             min = 4, max = 20, value = 8, step = 0.5)), 
            div(style="display:inline-block", 
                numericInput("dpc1oup.w", "PDF / PNG width:", width = "138px", 
                             min = 4, max = 20, value = 10, step = 0.5)) 
     )  # End of column (6 space) 
   )    # End of fluidRow (4 space) 
 ),     # End of tab (2 space) 
 
### Tab1.c2: Proportion plot 
tabPanel( 
  HTML("Proportion plot"), 
  h4("Proportion / cell numbers across different cell information"), 
  "In this tab, users can visualise the composition of single cells based on one discrete ", 
  "cell information across another discrete cell information. ",  
  "Usage examples include the library or cellcycle composition across clusters.", 
  br(),br(), 
  fluidRow( 
    column( 
      3, style="border-right: 2px solid black", 
      selectInput("dpc2inp1", "Cell information to plot (X-axis):", 
                  choices = dpconf[grp == TRUE]$UI, 
                  selected = dpdef$grp2) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to plot cells by",  
               content = c("Select categorical cell information to plot cells by", 
                           "- Plotted as the X-axis of the proportion plot")), 
      selectInput("dpc2inp2", "Cell information to group / colour by:", 
                  choices = dpconf[grp == TRUE]$UI, 
                  selected = dpdef$grp1) %>%  
        helper(type = "inline", size = "m", fade = TRUE, 
               title = "Cell information to group / colour cells by", 
               content = c("Select categorical cell information to group / colour cells by", 
                           "- Proportion / cell numbers are shown in different colours")), 
      radioButtons("dpc2typ", "Plot value:", 
                   choices = c("Proportion", "CellNumbers"), 
                   selected = "Proportion", inline = TRUE), 
      checkboxInput("dpc2flp", "Flip X/Y", value = FALSE), 
      actionButton("dpc2tog", "Toggle graphics controls"), 
      conditionalPanel( 
        condition = "input.dpc2tog % 2 == 1", 
        radioButtons("dpc2psz", "Plot size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Large", inline = TRUE), 
        radioButtons("dpc2fsz", "Font size:", 
                     choices = c("Small", "Medium", "Large"), 
                     selected = "Small", inline = TRUE)) 
    ), # End of column (6 space) 
    column(9, uiOutput("dpc2oup.ui"),  
           downloadButton("dpc2oup.pdf", "Download PDF"),  
           downloadButton("dpc2oup.png", "Download PNG"), br(), 
           div(style="display:inline-block", 
               numericInput("dpc2oup.h", "PDF / PNG height:", width = "138px", 
                            min = 4, max = 20, value = 8, step = 0.5)), 
           div(style="display:inline-block", 
               numericInput("dpc2oup.w", "PDF / PNG width:", width = "138px", 
                            min = 4, max = 20, value = 10, step = 0.5)) 
    )  # End of column (6 space) 
  )    # End of fluidRow (4 space) 
),     # End of tab (2 space) 
 
  ### Tab1.d1: Multiple gene expr 
  tabPanel( 
    HTML("Bubbleplot / Heatmap"), 
    h4("Gene expression bubbleplot / heatmap"), 
    "In this tab, users can visualise the gene expression patterns of ", 
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(), 
    "The normalised expression are averaged, log-transformed and then plotted.", 
    br(),br(), 
    fluidRow( 
      column( 
        3, style="border-right: 2px solid black", 
        textAreaInput("dpd1inp", HTML("List of gene names <br /> 
                                          (Max 50 genes, separated <br /> 
                                           by , or ; or newline):"), 
                      height = "200px", 
                      value = paste0(dpdef$genes, collapse = ", ")) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "List of genes to plot on bubbleplot / heatmap", 
                 content = c("Input genes to plot", 
                             "- Maximum 50 genes (due to ploting space limitations)", 
                             "- Genes should be separated by comma, semicolon or newline")), 
        selectInput("dpd1grp", "Group by:", 
                    choices = dpconf[grp == TRUE]$UI, 
                    selected = dpconf[grp == TRUE]$UI[1]) %>% 
          helper(type = "inline", size = "m", fade = TRUE, 
                 title = "Cell information to group cells by", 
                 content = c("Select categorical cell information to group cells by", 
                             "- Single cells are grouped by this categorical covariate", 
                             "- Plotted as the X-axis of the bubbleplot / heatmap")), 
        radioButtons("dpd1plt", "Plot type:", 
                     choices = c("Bubbleplot", "Heatmap"), 
                     selected = "Bubbleplot", inline = TRUE), 
        checkboxInput("dpd1scl", "Scale gene expression", value = TRUE), 
        checkboxInput("dpd1row", "Cluster rows (genes)", value = TRUE), 
        checkboxInput("dpd1col", "Cluster columns (samples)", value = FALSE), 
        br(), 
        actionButton("dpd1tog", "Toggle graphics controls"), 
        conditionalPanel( 
          condition = "input.dpd1tog % 2 == 1", 
          radioButtons("dpd1cols", "Colour scheme:", 
                       choices = c("Purple Gradient", "Blue-Yellow-Red", 
                                   "Yellow-Green-Purple"), 
                       selected = "Blue-Yellow-Red"), 
          radioButtons("dpd1psz", "Plot size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Large", inline = TRUE), 
          radioButtons("dpd1fsz", "Font size:", 
                       choices = c("Small", "Medium", "Large"), 
                       selected = "Small", inline = TRUE)) 
      ), # End of column (6 space) 
      column(9, h4(htmlOutput("dpd1oupTxt")), 
             uiOutput("dpd1oup.ui"), 
             downloadButton("dpd1oup.pdf", "Download PDF"), 
             downloadButton("dpd1oup.png", "Download PNG"), br(), 
             div(style="display:inline-block", 
                 numericInput("dpd1oup.h", "PDF / PNG height:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)), 
             div(style="display:inline-block", 
                 numericInput("dpd1oup.w", "PDF / PNG width:", width = "138px", 
                              min = 4, max = 20, value = 10, step = 0.5)) 
      )  # End of column (6 space) 
    )    # End of fluidRow (4 space) 
  )      # End of tab (2 space) 
   ), 

   
br(), 
p("", style = "font-size: 125%;"), 
p(em("This webpage was made by Jane Velghe, using "), a("ShinyCell", 
  href = "https://github.com/SGDDNB/ShinyCell",target="_blank")), 
br(),br(),br(),br(),br() 
))) 
 
 
 
 