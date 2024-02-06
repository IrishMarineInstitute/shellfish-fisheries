### File: ui.R
###
### Template Created: 14/07/2020	08:55:00
### Author: Guillermo Martin
###
####################################################################################################
### Description:
###
### Fluid page for the Inshore Shinny app
###   
###

#Global displaying options
options(spinner.color="green",
        spinner.type=4)

# Header ------------------------------------------------------------------
header<- dashboardHeader(title = "Irish Shellfish Fisheries App Visualization",
                         disable = FALSE,
                         titleWidth = 600, 
                               #dropdownMenuCustom(type = 'message',
                              #                 customSentence = customSentence,
                              #                 messageItem(
                              #                   from = "inshoreteam@marine.ie",#'Feedback and suggestions',
                              #                   message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                              #                   icon = icon("envelope"),
                              #                   href = "mailto:inshoreteam@marine.ie"),
                              #                 icon = icon('comment')),
                         dropdownMenuCustom( type = 'message',
                                               customSentence = customSentence_share,
                                               icon = icon("share-alt"),
                                               messageItem(
                                                 from = 'Twitter',
                                                 message = "",
                                                 icon = icon("twitter")#,
                                                 #href = "https://twitter.com/intent/tweet?url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&text=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                               ),
                                               messageItem(
                                                 from = 'Facebook',
                                                 message = "",
                                                 icon = icon("facebook")#,
                                                 #href = #"https://www.facebook.com/sharer/sharer.php?u=http%3A%2F%2Ftradeintelligence.mbie.govt.nz"
                                               ),
                                               messageItem(
                                                 from = 'LinkedIn',
                                                 message = "",
                                                 icon = icon("linkedin")#,
                                                 #href = "http://www.linkedin.com/shareArticle?mini=true&url=http%3A%2F%2Ftradeintelligence.mbie.govt.nz&title=New%20Zealand%20Trade%20Intelligence%20Dashboard"
                                               ))
                         
                         )

header$children[[2]]$children[[2]] <- header$children[[2]]$children[[1]]
header$children[[2]]$children[[1]] <- tags$a(href='https://www.marine.ie',
                                             tags$img(src='logo/MI.4.png',height='40',width='228.6', align = 'left'),
                                             target = '_blank') #,height='67',width='228.6', align = 'left'



siderbar<- dashboardSidebar(
  introBox(data.step = 1, data.intro = intro$text[1],data.position="right", #  intro tour
           div(class="inlay",style = "height:100%;width:100%;background-color: #ecf0f5;"),
  sidebarMenu(
    menuItem("Sampling programmes",tabName = "Initial", icon = icon("chart-bar"),
             menuSubItem("Overview",tabName = "overview", icon = icon("home")),
             menuItem("SVP and Observer Data",tabName = "SVP_O", icon = icon("chart-bar"),
             menuSubItem("Catch Rate Series",tabName = "Catch_Rate", icon = icon("chart-bar")),
             menuSubItem("Maps trends in Stock status",tabName = "Map", icon = icon("map-marker-alt")),
             menuSubItem("Size Distribution",tabName = "Length_Distribution", icon = icon("chart-area"))
             ),
             menuItem("Data description",tabName = "Details", icon = icon("info-circle"))
             ),
    menuItem("Landings",tabName = "landings", icon = icon("chart-bar")),
    menuItem("Assessment and Advice",tabName = "Assessment", icon = icon("indent-right",lib="glyphicon"),
             ##menuSubItem("Malin Crab Stock Assessment",tabName = "Malin_Assessment", icon = icon("indent-right",lib="glyphicon")),
             menuSubItem("Bivalve Stocks",tabName = "BivalveAssessment", icon = tags$img(src='species/icon/Bivalve.ico',
                                                                                  height='15%',width='15%')),
             menuSubItem("Crustacean Stocks",tabName = "CrustaceanAssessment", icon = tags$img(src='species/icon/Crustacean2.png',
                                                                                     height='15%',width='15%')),
             menuSubItem("Gastropods Stocks",tabName = "GastropodAssessment", icon = tags$img(src='species/icon/Gastropods.ico',
                                                                                     height='15%',width='15%'))
             ),
    menuItem("Further Information",tabName = "Oinfo", icon = icon("info-circle"))
    )
  )
  )

body<-dashboardBody(
  
  tags$head( # must include css
    tags$style(HTML("
        .img-local {
        }
        
        .small-box .img-local {
        position: absolute;
        top: auto;
        bottom: -25px;
        right: 5px;
        z-index: 0;
        font-size: 70px;
        color: rgba(0, 0, 0, 0.15);
        }"
    ))
  ),
  
  useShinyjs(),
  introjsUI(),  
  
  tabItems(
    tabItem(tabName = "overview",
    ## contents for the dashboard tab
    div(id = 'main_wait_message',
        h1('Note, initial load may take up to 10 seconds.',
           style = "color:darkblue" , align = "center" ) ,
        tags$hr()
    ),
    tags$div(h1(paste0("Shellfish Fisheries App")),
             style="text-align: center"),
    tags$div(h3(paste("A brief summary of the Lobster and Crab sampling programmes around Ireland")),#in,maxY,sep=" ")),
             style="text-align: center"),
    br(),
    introBox(data.step = 2, data.intro = intro$text[2], #  intro tour
             div(class = "inlay", style = "height:30px;width:100%;background-color: #ecf0f5;"),
             fluidRow(
               align = "center",
               column(12,
                      box(
                        background = "olive", 
                        width = NULL,
                        #h4("Select year of interest or whole time series:"),
                        br(),
                        column(12,
                               tagList(
                                 tags$style(type = 'text/css', 
                                            '#y_slider .irs-grid-text {font-size: 14px}'), 
                                 div(id = 'y_slider',
                                     sliderInput("Year",
                                                 "Year:",
                                                 min = 2013,
                                                 max = maxY,
                                                 value=c(maxY,maxY),
                                                 round=TRUE,
                                                 sep = "")
                                     )
                                 )
                               ),
                        )
               )
               
             )
             
    ),
    div(
      fluidRow(
        valueBoxOutput("Nvessels_SVP",width = 6),
        valueBoxOutput("NTrips_Obs_Y",width = 6),
        style="margin-top:+2em")
      ),
    div(
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    plotOutput("plotH1",width = 500,height = 500), 
                    plotOutput("plotH2",width = 500,height = 500)),
      style="margin-top:+1em; margin-bottom:+4em")
      ),
    div(
      fluidRow(
        valueBoxOutput("NDaysSVP",width = 6),
        valueBoxOutput("NHauls_Obs_Y",width = 6))
      ),
    div(
      introBox(data.step = 3, data.intro = intro$text[3], #  intro tour
               div(class = "inlay", style = "height:30px;width:100%;background-color: #ecf0f5;"),
               fluidRow(
        valueBoxOutput("CREMeasured",width = 6),
        valueBoxOutput("LBEMeasured",width = 6))
    )
    )
    ),
    tabItem(tabName = "landings",
            fluidRow(
              column(12,
                     box(
                       background = "olive", width = NULL,
                       h4("Apply desired filters:"),
                       br(),
                       column(6,selectInput('SpIDL',
                                            'Species:',
                                            landings$SpeciesName,
                                            selected="Edible crab",
                                            multiple = TRUE),
                              tags$div("Tip: press backspace to remove species from the filter",style = "font-size:13px")),
                       column(3,sliderInput("YearL",
                                            "Year:",
                                            min = 2004,
                                            max = maxY,
                                            value=c(2015,2018),
                                            round=TRUE,
                                            sep = "")),
                     )
              )
            ),
            br(),
            fluidRow(
              column(12, shinycssloaders::withSpinner(plotOutput("landingsPlot", width = 1000, 
                                                                 height=600)),align="center")
            )
    ),
    tabItem(tabName = "Catch_Rate",
            fluidRow(
              column(12,
                     box(
                       background = "olive", width = NULL,
                       h4("Apply desired filters:"),
                       br(),
                       column(3,selectInput('SpID',
                                            'Species:',
                                            catch_plot$CommonName,
                                            choices = c("EUROPEAN LOBSTER",
                                                        "EDIBLE CRAB UNSEXED"))),
                       column(3,checkboxGroupInput('SP', 
                                                   'Sampling Programme:',
                                                   catch_plot$SampleType,
                                                   choices = c("Observer"="Observer",
                                                               "Sentinel Vessel Fleet"="SVP"))),
                       column(3,sliderInput("YearC",
                                            "Year:",
                                            min = 2013,
                                            max = maxY,
                                            value=c(2015,2018),
                                            round=TRUE,
                                            sep = "")),
                       column(3,checkboxGroupInput('CatchID',
                                                   'Catch Type:',
                                                   catch_plot$Catch_type,
                                                   choices = c("LPUE","DPUE",
                                                               "VPUE","OPUE"),
                                                   inline = TRUE)),
                       )
                     )
              ),
            br(),
            fluidRow(
              column(12, shinycssloaders::withSpinner(plotOutput("plot1", width = 1000, 
                                                                 height=500)),
                     tags$div("*LPUE= Landings per unit of Effort;  DPUE= Discards per unit of Effort",style = "font-size:15px",
                              tags$br("*VPUE= V-Notched per unit of Effort;  OPUE= Oversized per unit of Effort",style = "font-size:15px")),
                     align="center")
              )
            ),
    tabItem(tabName = "Map",
            fluidRow(
              column(12,
                     shinycssloaders::withSpinner(leafletOutput("map", height='60vh')),
                     
                     
                     
                     absolutePanel(id = "controls", 
                                   class = "panel panel-default", 
                                   fixed = TRUE,
                                   draggable = TRUE, 
                                   top = 220, left = "auto", 
                                   right = 40, bottom = "auto",
                                   width = 330, height = "auto",
                                   style="background-color: green;
                                          opacity: 0.85;
                                          padding: 20px 20px 20px 20px;
                                          margin: auto;
                                          border-radius: 5pt;
                                          box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                          padding-bottom: 2mm;
                                          padding-top: 1mm;",
                                   selectInput('SpID5',
                                               tags$span(style="color: white;","Species:"), 
                                               choices = c("SELECT SPECIES",
                                                           ICES_LPUE$CommnNm),
                                               selected = "SELECT SPECIES"))),
              column(6,
                     h1("How to use this map?"),
                     tags$ol(
                       tags$li("Select a species within the map. Initial load may take a few seconds"),
                       tags$li("Zoom in and out of the map to focus on an area of interest.")
                     ),
                     h1("Information displayed"),
                     tags$p("Colours displayed in the map indicate the trends in the SVP Landings per Unit of Effort (LPUE) at  
                            ICES rectangle level. Thus, red colours signal negative trends, while green colours are positive.",
                            style="text-align: justify"),
                     tags$p("The Scatterplot on the right, displays the raw LPUE used for this colour assignation, and it is 
                            subsetted to particular areas when the users zoom in/out of the map",
                            style="text-align: justify"),
                     tags$p(HTML("<b> Only ICES rectangles with a sufficient time series of data are displayed. Still, 
                                  certain ICES rectangles on given years contain few data points to estimate reliable trends,
                                 so caution should be taken when extracting conclusions from particular areas.</b>"),
                            style="text-align: justify; color:red")),
              column(6, shinycssloaders::withSpinner(plotlyOutput("scatter_plot", height=300)))
            )
    ),
    tabItem(tabName = "Length_Distribution",
            fluidRow(
              column(12,
                     box(
                       background = "olive", width = NULL,
                       h4("Apply desired filters:"),
                       br(),
                       column(3,selectInput('SpID2',
                                            'Species:',
                                            bio$CommonName,
                                            choices = c("EUROPEAN LOBSTER",
                                                        "EDIBLE CRAB UNSEXED")),
                              checkboxGroupInput('SexID', 
                                                 'Sex:',
                                                 bio$SexID,
                                                 choices = c("Female","Male"),
                                                 inline = TRUE)),
                       column(3,sliderInput("Year2",
                                            "Year:",
                                            min = 2015,
                                            max = maxY,
                                            round=TRUE,
                                            value=c(2016,2018),
                                            sep = ""),
                              sliderInput("Size",
                                          "Size:",
                                          min = 0,
                                          max = 300,
                                          value=c(60,250),
                                          sep = "")),
                       column(3,checkboxGroupInput('DiscardedYN', 
                                                   'Discard or Landed:',
                                                   bio$DiscardedYN,
                                                   choices = c("Discarded",
                                                               "Landed"),
                                                   inline = TRUE)),
                       column(3,checkboxGroupInput('VNotchedYN', 
                                                   'V-Notched:',
                                                   bio$VNotchedYN,
                                                   choices = c("Yes",
                                                               "No",
                                                               "Not Answered"),
                                                   inline = TRUE))
                     )
                     )
              ),
            br(),
            fluidRow(
              column(12, shinycssloaders::withSpinner(plotOutput("plot2", width = 1000, 
                                                                 height=600)),
                     tags$div("*Vertical green dashed line indicating Minimum and Maximum Landing Size",style = "font-size:15px"),
                     align="center")
            )
    ),
    tabItem(tabName = "Details",
            br(),
            HTML("<p>This tab provides a brief description of the different data sources relevant to Shellfish Stocks 
                   around Ireland. <b>Not all data sources are currently included in the app, as work is ongoing</b></p>"),
            br(),
            tags$b("Shellfish Surveys"),
            HTML('<p align= "justify">Every year, the Marine Institute in collaboration with the fishing industry carry a number of scientific surveys 
            around Ireland, mainly focusing on bivalves such as <i>Cerastoderma edule</i> (Cockle), <i>Ensis siliqua</i> 
            (Razor clam) and <i>Ostrea edulis</i> (Native Oyster). The data collected during these surveys 
            is assessed using a geostatistical model which provides important information about the biomass 
            and size profile of the stocks. The outputs from this model, are directly reported to the industry. More information about 
            the different surveys carried out annually can be found in the "Assessment and Advice" tab</p>'),
            br(),
            tags$b("Sentinel Vessel Programme"),
            HTML('<p align="justify">The Sentinel Vessel Programme (SVP) is a joint project implement by BIM and the 
                 Marine Institute on an annual basis since 2010. In terms of data availability, good quality data are 
                 typically available for fish species which are managed via a quota system. However, prior to the SVP, 
                 fishing data in relation to smaller vessels targeting non-quota species was very scant and thus 
                 monitoring fisheries around Ireland using commercial catch rate data was not possible. Vessels are chosen 
                 from different length and gear categories representative of fishing activities by vessels (under 12 m) 
                 around the Irish coast. BIM supply participants with logbooks every January and at the end of each year 
                 completed logbooks are collected by BIM and are forwarded to the Marine Institute who extract and manage 
                 the data. The data recorded in the SVP logbooks includes the catches, landings and discards of several 
                 shellfish species, i.e. <i>Homarus gammarus</i> (Lobster), <i>Cancer pagurus</i> (Brown Crab), 
                 <i>Maja brachydactyla</i> (Spider Crab), <i>Necora puber</i> (Velvet Crab), <i>Buccinum undatum</i> (Whelk),
                 <i>Ensis</i> sp. (Razor clam),<i>Cerastoderma edule</i> (Cockle) and various finfish species. 
                 The fishing location is recorded at either ICES Statistical Rectangle or Inshore Grid Resolution and 
                 additional details such as the type and amount of bait used and vessel operating costs 
                 (i.e. fuel consumption, number of crew, hour’s worked.) are also recorded. Additionally, 
                 although to a lesser extent (every five fishing days), length frequency data for lobsters and crabs may 
                 be included.</p>'),
            br(),
            tags$b("Observer At-Sea Sampling programme"),
            HTML('<p align="justify">As part of the European Union (EU) Data Collection Framework (DCF) (or previous 
                 versions of it), since the early 90s, the Marine Institute has been requested to report catch sampling 
                 information to aid in the assessment of fish stocks. This includes both commercial at sea sampling and 
                 port sampling. Since 2011, Marine Institute staff and contractors seat sea sampling on inshore fishing 
                 vessels to observe and record fishing activity. Approximately 50-80 day trips are completed annually, 
                 although this varies year on year and was lower earlier in the time series and during the Covid lockdown 
                 periods. Effort and catch data per haul is recorded, during an observer at sea sampling trip, on several 
                 shellfish species such as <i>Homarus gammarus</i> (Lobster), <i>Cancer pagurus</i> (Brown Crab), <i>Maja brachydactyla</i> 
                 (Spider Crab), <i>Necora puber</i> (Velvet Crab), <i>Buccinum undatum</i> (Whelk), and the bycatch associated with 
                 these fishing events. Furthermore, all individuals or a sample (depending on catch volume) of the target 
                 species captured are measured to the nearest mm and their sex is determined, providing a significant 
                 amount of valuable biological information for these species. The observer programme provides data at the 
                 level of individual fishing operations (catches per Haul). The sampling levels of 50-80 trips 
                 per year is low relative to the thousands of trips undertaken by the Inshore fishing fleet annually. 
                 Furthermore there is high variance between vessels (related to location of fishing). The low sampling 
                 level and high variance reduces precision and even accuracy in these data sets especially when reported 
                 to local level where the data supports are diluted.</p>'),
            br(),
            tags$b("Skipper Self-Sampling programme"),
            HTML('<p align="justify">Since 2021, a number of commercial inshore vessels around the Irish coast report 
                 daily information  at haul level in relation to catches, landings and discards, for several shellfish 
                 species. Biological data in relation to individual measurements and sex of the catches are also 
                 recorded. This programme is administered fully by the Marine Institute and the information provided 
                 enhances both the resolution (haul), and quantity (number of trips) of the sentinel vessel and observer 
                 programmes, respectively. As this programme is still in its infancy, data from this programme is not yet 
                 included in this Shellfish Fisheries app.</p>'),
            br(),
            tags$b("Landings"),
            HTML('<p align= "justify">Annual landings of crustaceans, bivalves and gastropods are collated for Irish vessels landings into Ireland
                 (with exception of King Scallop (<i>Pecten maximus</i>) and Brown Crab 
                 (<i>Cancer pagurus</i>), in which landings by Irish vessels abroad are also 
                 included). Landings data are reported under two different frameworks depending on the 
                 length of the vessel: Logbooks declarations for vessels >10 m and sales 
                 notes for vessels <10m.</p>'),
            br(),
            tags$b("Port Sampling"),
            HTML('<p align= "justify">Port sampling is undertaken as part of the Data Collection Framework obligation to  
                 provide valuable data relating to the size, weight and gender of landings and the area (at ICES Rectangle 
                 level) where they were caught. Some of the species routinely sampled around Irish harbours include <i>Pecten 
                 maximus</i> (Scallop), <i>Buccinum undatum</i> (whelk), <i>Cancer pagurus</i> (Brown crab) and <i>Homarus gammarus</i> 
                 (European lobster) among others.</p>')
    ),
    tabItem(tabName = "BivalveAssessment",
            fluidRow(
              column(12,
                     box(
                       background = "olive", width = NULL,
                       column(3,
                              selectInput("SpBIDA",
                                          "Species:",
                                          choices = c("SELECT SPECIES",unique(ba_a$Specie)),
                                          selected = "SELECT SPECIES")),
                       column(3,
                              selectInput("SpBArea",
                                          "Area:",
                                          choices = c("SELECT AREA",unique(ba_a$Area)),
                                          selected = "SELECT AREA")),
                       column(3,
                              selectInput("SpBY",
                                          "Year:",
                                          choices = c("SELECT YEAR",ba_a$Year),
                                          selected = "SELECT YEAR")),
                       column(3,
                              imageOutput("bivalve_image", height = "50%")
                       )
                     ))
            ),
            tabsetPanel(
              tabPanel("Management Advice",
                       fluidRow(column(width = 6, 
                                       div(
                                         tags$p(htmlOutput("advice_text")),
                                         style="text-align: justify")
                       ))
              ),
              tabPanel("Stock summary",
                       fluidRow(
                         column(width = 6, 
                                h3("The Fishery"),
                                div(
                                  tags$p(htmlOutput("stock_text")),
                                  style="text-align: justify")
                                ),
                         column(width = 6, 
                                h3("Species Biology"),
                                div(
                                  tags$p(htmlOutput("bio_text")),
                                  style="text-align: justify")
                         ),
                         )
                       
              ),
              tabPanel("Survey Description",
                       fluidRow(
                         column(width = 6, 
                                div(
                                  tags$p(htmlOutput("survey_text")),
                                  style="text-align: justify")
                         ),
                         column(width = 6, 
                                imageOutput("survey.zones")
                         )
                         )
                       ),
              tabPanel("Assesment Outputs",
                       fluidRow(
                         column(width = 6, 
                                div(
                                  tags$p(htmlOutput("Aoutput_text")),
                                  style="text-align: justify"),
                                div(imageOutput("display.size"))
                                ),
                         br(),
                         column(width = 6, 
                                imageOutput("display.assessment")
                                )
                         )
              )
            )
            ),
    tabItem(tabName = "CrustaceanAssessment",
            fluidRow(
              column(12,
                     box(
                       background = "olive", width = NULL,
                       column(3,
                              selectInput("SpCIDA",
                                          "Species:",
                                          choices = c("SELECT SPECIES",unique(ca_a$Specie)),
                                          selected = "SELECT SPECIES")),
                       column(3,
                              selectInput("SpCArea",
                                          "Area:",
                                          choices = c("SELECT AREA",unique(ca_a$Area)),
                                          selected = "SELECT AREA")),
                       column(3,
                              imageOutput("crustacean_image", height = "50%")
                       )
                     )
                     )
              ),
            tabsetPanel(
              tabPanel("Management Advice",
                       htmlOutput("C_ManagementAdvice")),
              tabPanel("Stock summary",
                       fluidRow(
                         column(12,
                                htmlOutput("C_StockSummary")
                                )
                       )
                       ),
              tabPanel("Data Sources",
                       fluidRow(
                         column(12,
                              htmlOutput("C_DataSources")
                              )
                              )
                       ),
              tabPanel("Assessment Outputs",
                       fluidRow(
                         column(12,
                                htmlOutput("C_Assessment")
                         )
                       )
                       )
            )
    ),
    tabItem(tabName = "GastropodAssessment",
            tags$div(h1(paste0("In development")),
                     style="text-align: center")),
    tabItem(tabName = "Oinfo",
            br(),
            tags$p("This tab provides a series of links to other sources of information relevant to Irish Fisheries"),
            br(),
            tags$b("Annual Shellfish Stocks and Fisheries Review: an assessment of selected stocks"),
            br(),
            tags$p(tags$u("2022:"),
                   tags$a(
                     "https://oar.marine.ie/handle/10793/1814",
                     target = "_blank",
                     href = "https://oar.marine.ie/handle/10793/1814#:~:text=The%20intention%20of%20this%20annual,in%20areas%20designated%20under%20European")
                   ),
            tags$p(tags$u("2021:"),
                   tags$a(
                     "https://oar.marine.ie/handle/10793/1744",
                     target = "_blank",
                     href = "https://oar.marine.ie/handle/10793/1744")
            ),
            tags$p(tags$u("2020:"),
                   tags$a(
                     "https://oar.marine.ie/handle/10793/1688",
                     target = "_blank",
                     href = "https://oar.marine.ie/handle/10793/1688")
            ),
            tags$p(tags$u("2019:"),
                   tags$a(
                     "https://oar.marine.ie/handle/10793/1591",
                     target = "_blank",
                     href = "https://oar.marine.ie/handle/10793/1591")
            ),
            br(),
            tags$b("Atlas: Commercial fisheries for shellfish around Ireland"),
            br(),
            tags$a(
              "https://oar.marine.ie/handle/10793/1243",
              target = "_blank",
              href = "https://oar.marine.ie/handle/10793/1243"),
            br(),
            br(),
            tags$b("Natura 2000 Network maps"),
            br(),
            tags$a(
              "https://natura2000.eea.europa.eu/",
              target = "_blank",
              href = "https://natura2000.eea.europa.eu/"),
            br(),
            br(),
            tags$b("Fisheries Natura Plan for Cockle in Dundalk"),
            br(),
            tags$p(tags$u("2021-2025:"),
                   tags$a(
              "http://www.fishingnet.ie/sea-fisheriesinnaturaareas/concludedassessments/dundalkbay-sacspa/",
              target = "_blank",
              href = "http://www.fishingnet.ie/sea-fisheriesinnaturaareas/concludedassessments/dundalkbay-sacspa/")
                   ),
            tags$p(tags$u("2016-2020:"),
                   tags$a(
                     "http://www.fishingnet.ie/sea-fisheriesinnaturaareas/concludedassessments/dundalkbay-sacspa/",
                     target = "_blank",
                     href = "http://www.fishingnet.ie/sea-fisheriesinnaturaareas/concludedassessments/dundalkbay-sacspa/")
            ),
            br(),
            tags$b("Annual Stock Book: Review of Fish Stocks"),
            br(),
            tags$p(tags$u("2022:"),
                   tags$a(
                     "https://oar.marine.ie/handle/10793/1805",
                     target = "_blank",
                     href = "https://oar.marine.ie/handle/10793/1805")
            ),
            tags$p(tags$u("2021:"),
                   tags$a(
                     "https://oar.marine.ie/handle/10793/1726",
                     target = "_blank",
                     href = "https://oar.marine.ie/handle/10793/1726")
            ),
            tags$p(tags$u("2020:"),
                   tags$a(
                     "https://oar.marine.ie/handle/10793/1660",
                     target = "_blank",
                     href = "https://oar.marine.ie/handle/10793/1660")
            ),
            tags$p(tags$u("2019:"),
                   tags$a(
                     "https://oar.marine.ie/handle/10793/1433",
                     target = "_blank",
                     href = "https://oar.marine.ie/handle/10793/1433")
            ),
            br(),
            tags$b("Marine Institute Digital Stock Book (Shiny App)"),
            br(),
            tags$a(
              "https://shiny.marine.ie/stockbook/",
              target = "_blank",
              href = "https://shiny.marine.ie/stockbook/"),
            )
    )
  )
     

ui <- 
  dashboardPage(title="Shellfish Fisheries App", skin = "green",header, siderbar, body)


