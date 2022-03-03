#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# In all models we use total N = 100, set default value to 1 for all parameters in the sliders
# SI - 2 sliders ( I0 (0-10), beta (0-10))
# SIS - 3 sliders (I0 (0-10), beta (0-10),gamma (0-10))
# SIR - 3 sliders (I0 (0-10), beta (0-10),gamma (0-10))
# SIR(Array with age group) - 3 sliders (I0 (0-10), beta (0-10),gamma (0-10))
# SIRS - 4 sliders (I0 (0-10), beta (0-10),gamma (0-10), omega (0-10))
# SEIR - 4 sliders (I0 (0-10), beta (0-10),gamma (0 -10),nui (0-10)) 
# SEIRS - 5 sliders (I0 (0-10), beta (0-10),gamma (0 -10),nui (0-10), omega (0-10))

library(shiny)
library(shinybusy)

# colorBox <- function(color, text=""){
# #     div(
# #         tags$span(text, style = "padding-left: 30px;"),
# #         div(
# #             style = paste0("background-color:", 
# #                            color, "; ",
# #                            "display: inline-block;
# #                         position: absolute;
# #                         height:13px;
# #                         width: 13px;
# #                         left: 5px;
# #                         top: 3px;")),
# #         style = "position: relative;"
# #     )
# # }

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    includeCSS("www/style.css"),
    div(
        # Application title
        column(8,
               h1("Compartmental models"),
        ),
        column(4,
               tags$img(class = "logo",src="MORU_FS_RGB.png")
        ),
    ),
    
        
    tabsetPanel(
        # SI - 2 sliders ( I0 (0-10), beta (0-10))
        tabPanel("SI",
                 column(4,
                 sliderInput("I0_SI", 
                             "I0 : ",
                             min = 0, max = 10, value = 1
                 ),
                 sliderInput("beta_SI", 
                             "Beta(β) : ",
                             min = 0, max = 10, value = 1,step = 0.1
                 ),
                 tags$img(src="SI.png"),
                 ),
                 column(8,
                     plotOutput("SI")
                 )
                 ),
        # SIS - 3 sliders (I0 (0-10), beta (0-10),gamma (0-10))
        tabPanel("SIS",
                 
                 column(4,
                 sliderInput("I0_SIS", 
                             "I0 : ",
                             min = 0, max = 10, value = 1
                 ),
                 sliderInput("beta_SIS", 
                             "Beta(β) : ",
                             min = 0, max = 10, value = 3,step = 0.1
                 ),
                 sliderInput("gamma_SIS", 
                             "Gamma(γ) : ",
                             min = 0, max = 10, value = 1
                 ),
                 tags$img(src="SIS.png"),
                 ),
                 column(8,
                     plotOutput("SIS"),
                 )
                 ),
        # SIR - 3 sliders (I0 (0-10), beta (0-10),gamma (0-10))
        tabPanel("SIR",
                 column(4,
                 sliderInput("I0_SIR", 
                             "I0 : ",
                             min = 0, max = 10, value = 1
                 ),
                 sliderInput("beta_SIR", 
                             "Beta(β) : ",
                             min = 0, max = 10, value = 3,step = 0.1
                 ),
                 sliderInput("gamma_SIR", 
                             "Gamma(γ) : ",
                             min = 0, max = 10, value = 1
                 ),
                 tags$img(src="SIR.png"),
                 ),
                 column(8,
                        plotOutput("SIR"),
                 )
                 ),
        # SIR2(Array with age group) - 3 sliders (I0 (0-10), beta (0-10),gamma (0-10))
        tabPanel("SIR (Array with age group)",
                 column(4,
                        sliderInput("I01_SIR2", 
                                    "I0-1(age 0 - 18) : ",
                                    min = 0.0001, max = 0.0015, value = 0.001,step  = 0.0001
                        ),
                        sliderInput("I02_SIR2", 
                                    "I0-2(age 18 - 60) : ",
                                    min = 0.0001, max = 0.0015, value = 0.001,step  = 0.0001
                        ),
                        sliderInput("I03_SIR2", 
                                    "I0-3(age 60+) : ",
                                    min = 0.0001, max = 0.0015, value = 0.001,step  = 0.0001
                        ),
                        sliderInput("beta1_SIR2", 
                                    "Beta(β)(age 0 - 18) : ",
                                    min = 0, max = 8, value = 3,step = 0.1
                        ),
                        sliderInput("beta2_SIR2", 
                                    "Beta(β)(age 18 - 60) : ",
                                    min = 0, max = 8, value = 8,step = 0.1
                        ),
                        sliderInput("beta3_SIR2", 
                                    "Beta(β)(age 60+) : ",
                                    min = 0, max = 8, value = 2,step = 0.1
                        ),
                        sliderInput("gamma_SIR2", 
                                    "Gamma(γ) : ",
                                    min = 0.0002, max = 0.001, value = 0.0002,step  = 0.0001
                        ),
                 ),
                 column(8,
                        add_busy_bar(color = "#FF0000"),
                        plotOutput("SIR2"),
                 )
        ),
        # SIRS - 4 sliders (I0 (0-10), beta (0-10),gamma (0-10), omega (0-10))
        tabPanel("SIRS",
                 column(4,
                 sliderInput("I0_SIRS", 
                             "I0 : ",
                             min = 0, max = 10, value = 1
                 ),
                 sliderInput("beta_SIRS", 
                             "Beta(β) : ",
                             min = 0, max = 10, value = 6,step = 0.1
                 ),
                 sliderInput("gamma_SIRS", 
                             "Gamma(γ) : ",
                             min = 0, max = 10, value = 2
                 ),
                 sliderInput("omega_SIRS", 
                             "Omega(Ω) : ",
                             min = 0, max = 10, value = 1
                 ),
                 tags$img(src="SIRS.png"),
                 ),
                 column(8,
                        plotOutput("SIRS"),
                 )
                 ),
        # SEIR - 4 sliders (I0 (0-10), beta (0-10),gamma (0 -10),nui (0-10)) 
        tabPanel("SEIR",
                 column(4,
                 sliderInput("I0_SEIR", 
                             "I0 : ",
                             min = 0, max = 10, value = 1
                 ),
                 sliderInput("beta_SEIR", 
                             "Beta(β) : ",
                             min = 0, max = 10, value = 6,step = 0.1
                 ),
                 sliderInput("gamma_SEIR", 
                             "Gamma(γ) : ",
                             min = 0, max = 10, value = 2
                 ),
                 sliderInput("nui_SEIR", 
                             "Nui(ν) : ",
                             min = 0, max = 10, value = 1
                 ),
                 tags$img(src="SEIR.png"),
                 ),
                 column(8,
                        plotOutput("SEIR"),
                 )
                 ),
        # SEIRS - 5 sliders (I0 (0-10), beta (0-10),gamma (0 -10),nui (0-10), omega (0-10))
        tabPanel("SEIRS",
                 column(4,
                 sliderInput("I0_SEIRS", 
                             "I0 : ",
                             min = 0, max = 10, value = 1
                 ),
                 sliderInput("beta_SEIRS", 
                             "Beta(β) : ",
                             min = 0, max = 10, value = 6,step = 0.1
                 ),
                 sliderInput("gamma_SEIRS", 
                             "Gamma(γ) : ",
                             min = 0, max = 10, value = 4
                 ),

                 sliderInput("nui_SEIRS", 
                             "Nui(ν) : ",
                             min = 0, max = 10, value = 2
                 ),
                 sliderInput("omega_SEIRS", 
                             "Omega(Ω) : ",
                             min = 0, max = 10, value = 1
                 ),
                 tags$img(src="SEIRS.png"),
                 ),
                 column(8,
                        plotOutput("SEIRS"),
                 )
                 
        ),
        tabPanel("SEIR with birth&death",
                 column(4,
                        sliderInput("I0_SEIR2", 
                                    "I0 : ",
                                    min = 0, max = 10, value = 1
                        ),
                        sliderInput("beta_SEIR2", 
                                    "Beta(β) : ",
                                    min = 0, max = 10, value = 7,step = 0.1
                        ),
                        sliderInput("gamma_SEIR2", 
                                    "Gamma(γ) : ",
                                    min = 0, max = 10, value = 5
                        ),
                        sliderInput("nui_SEIR2", 
                                    "Nui(ν) : ",
                                    min = 0, max = 10, value = 2
                        ),
                        sliderInput("Birth_SEIR2", 
                                    "Birth rate : ",
                                    min = 0, max = 0.04, value = 0.03,step = 0.005
                        ),
                        sliderInput("Death_SEIR2", 
                                    "Death rate : ",
                                    min = 0, max = 0.04, value = 0.01,step = 0.005
                        ),
                        tags$img(src="SEIR.png"),
                 ),
                 column(8,
                        plotOutput("SEIR2"),
                 )
                 
        ),
        tabPanel("SEIRS with birth&death",
                 column(4,
                        sliderInput("I0_SEIRS2", 
                                    "I0 : ",
                                    min = 0, max = 10, value = 1
                        ),
                        sliderInput("beta_SEIRS2", 
                                    "Beta(β) : ",
                                    min = 0, max = 10, value = 7,step = 0.1
                        ),
                        sliderInput("gamma_SEIRS2", 
                                    "Gamma(γ) : ",
                                    min = 0, max = 10, value = 5
                        ),
                        
                        sliderInput("nui_SEIRS2", 
                                    "Nui(ν) : ",
                                    min = 0, max = 10, value = 2
                        ),
                        sliderInput("omega_SEIRS2", 
                                    "Omega(Ω) : ",
                                    min = 0, max = 10, value = 1
                        ),
                        sliderInput("Birth_SEIRS2", 
                                    "Birth rate : ",
                                    min = 0, max = 0.04, value = 0.03,step = 0.005
                        ),
                        sliderInput("Death_SEIRS2", 
                                    "Death rate : ",
                                    min = 0, max = 0.04, value = 0.005,step = 0.005
                        ),
                        tags$img(src="SEIRS.png"),
                 ),
                 column(8,
                        plotOutput("SEIRS2"),
                 )
                 
        )
    )

))
