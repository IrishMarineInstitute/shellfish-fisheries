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
header<- dashboardHeader(title = HTML("Irish Shellfish Fisheries DataBase Visualization"),
                         disable = FALSE,
                         titleWidth = 600, 
                               dropdownMenuCustom(type = 'message',
                                               customSentence = customSentence,
                                               messageItem(
                                                 from = "inshoreteam@marine.ie",#'Feedback and suggestions',
                                                 message =  "",#paste0("TR_SharedMailbox@mbie.govt.nz" ),
                                                 icon = icon("envelope"),
                                                 href = "mailto:inshoreteam@marine.ie"),
                                               icon = icon('comment')),
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
header$children[[2]]$children[[1]] <- tags$a(href='https://www.marine.ie/Home/home',
                                             tags$img(src='logo/MI.4.png',height='40',width='228.6', align = 'left'),
                                             target = '_blank') #,height='67',width='228.6', align = 'left'



siderbar<- dashboardSidebar(
  introBox(data.step = 1, data.intro = intro$text[1],data.position="right", #  intro tour
           div(class="inlay",style = "height:100%;width:100%;background-color: #ecf0f5;"),
  sidebarMenu(
    menuItem("Sampling programmes",tabName = "Initial", icon = icon("chart-bar"),
             menuSubItem("Overview",tabName = "overview", icon = icon("home")),
             menuSubItem("Catch Rate Series",tabName = "Catch_Rate", icon = icon("chart-bar")),
             menuSubItem("Length Distribution",tabName = "Length_Distribution", icon = icon("chart-area")),
             menuSubItem("Maps",tabName = "Map", icon = icon("map-marker-alt")),
             menuSubItem("Data details",tabName = "Details", icon = icon("info-circle"))),
    menuItem("Landings",tabName = "landings", icon = icon("chart-bar")),
    menuItem("Assessment and Advice",tabName = "Assessment", icon = icon("indent-right",lib="glyphicon"),
             menuSubItem("Malin Crab Stock Assessment",tabName = "Malin_Assessment", icon = icon("indent-right",lib="glyphicon")),
             menuSubItem("Other Stocks",tabName = "Assessment", icon = icon("indent-right",lib="glyphicon")))
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
    tags$div(h1(paste0("Shellfish Fisheries Database App")),
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
                        column(12,sliderInput("Year",
                                              "Year:",
                                              min = 2013,
                                              max = maxY,
                                              value=c(maxY,maxY),
                                              round=TRUE,
                                              sep = "")),
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
                    plotOutput("plotH1",width = 500,height = 250), 
                    plotOutput("plotH2",width = 500,height = 250)),
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
                       column(3,selectInput('SpIDL',
                                            'Species:',
                                            landings$SpeciesName,
                                            selected="Edible crab",
                                            multiple = TRUE)),
                       column(3,sliderInput("YearL",
                                            "Year:",
                                            min = 2004,
                                            max = maxY,
                                            value=c(2015,2018),
                                            round=TRUE,
                                            sep = ""))
                     )
              )
            ),
            br(),
            fluidRow(
              column(12, shinycssloaders::withSpinner(plotOutput("landingsPlot", width = 1000, 
                                                                 height=600)))
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
                              tags$br("*VPUE= V-Notched per unit of Effort;  OPUE= Oversized per unit of Effort",style = "font-size:15px")))
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
                                                        "EDIBLE CRAB UNSEXED"))),
                       column(3,sliderInput("Year2",
                                            "Year:",
                                            min = 2015,
                                            max = maxY,
                                            round=TRUE,
                                            value=c(2016,2018),
                                            sep = "")),
                       column(3,sliderInput("Size",
                                            "Size:",
                                            min = 0,
                                            max = 600,
                                            value=c(60,400),
                                            sep = "")),
                       column(3,checkboxGroupInput('SexID', 
                                                   'Sex:',
                                                   bio$SexID,
                                                   choices = c("Female","Male"),
                                                   inline = TRUE)),
                       column(3,checkboxGroupInput('DiscardedYN', 
                                                   'Discard or Landed:',
                                                   bio$DiscardedYN,
                                                   choices = c("Discarded",
                                                               "Landed"),
                                                   inline = TRUE)),
                     )
                     )
              ),
            br(),
            fluidRow(
              column(12, shinycssloaders::withSpinner(plotOutput("plot2", width = 1000, 
                                                                 height=600)),
                     tags$div("*Vertical green dashed line indicating Minimum and Maximum Landing Size",style = "font-size:15px"))
            )
    ),
    tabItem(tabName = "Details",
            tags$b("Sentinel Vessel Programme"),
            tags$p("Since 2013 a total of approximately 80 Inshore fishing vessels 
            around Ireland have been providing self-sample information about their daily fishing 
            operations. The programme is administered by BIM as a pilot project funded 
            by the Data Collection Framework. Vessels are chosen from different length 
            and gear categories representative of fishing activities by vessels under 12 m 
            around the Irish coast. BIM send hard copies of SVP logbooks to the Marine Institute who extract and 
            manage the data from the SVP books, uploading it to the FEAS Inshore Database. 
            A small number of Skippers in recent years have submitted data via a phone app 
            directly to the Marine Institute (so called eSVP). The data recorded in the SVP logbooks includes the 
            catches, landings and discards of several species, i.e. Homarus gammarus 
            (Lobster), Cancer pagurus (Brown Crab), Maja brachydactyla (Spider Crab), 
            Necora puber (Velvet Crab), Buccinum undatum (Whelk), Ensis sp. (Razor clams), 
            Cerastoderma edule (Cockle) and various finfish species.  The fishing location 
            is recorded at either ICES Statistical Rectangle or Inshore Grid Resolution and 
            additional details such as the type and amount of bait used or vessel operating 
            costs (i.e. fuel consumption, number of crew, hours worked.). Additionally, although 
            to a lesser extent (every five fishing days), length frequency data for lobsters and 
            crabs may be included.",style="text-align: justify"),
            tags$b("Observer programme"),
            tags$p("Each year since 2009, Marine Institute staff and contractors go to sea 
                   on inshore fishing vessels to observe and record fishing activity. 
                   About 50-80 day trips are completed annually, although this varies year 
                   on year and was lower earlier in the time series.  The data recorded in 
                   observer trips includes the catches, landings and discards of several species 
                   such as Homarus gammarus (Lobster), Cancer pagurus (Brown Crab), Maja brachydactyla 
                   (Spider Crab), Necora puber (Velvet Crab), Buccinum undatum (Whelk), and the bycatch 
                   associated with these fishing events. Furthermore, all individuals or a sample 
                   (depending on catch volume) of the target species captured  are measured to the nearest 
                   mm and their sex is determined, providing a significant amount of valuable biological 
                   information for these species. The observer programme provides data at the level of individual 
                   fishing operations in contrast to fishery dependent data collection programmes which report aggregated 
                   data. The sampling levels of 50-80 trips per year is low relative to the thousands of trips undertaken by the 
                   Inshore fishing fleet annually. Furthermore there is high variance between vessels (related to location of fishing). 
                   The low sampling level and high variance reduces precision and even accuracy in these data sets especially when reported 
                   to local level where the data supports are diluted.",style="text-align: justify")
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
                     tags$p(HTML("<b>Some ICES rectangles contain few data points to estimate reliable trends,
                            so caution should be taken when extracting conclusions from particular areas. Additionally, 
                                 some rectangles might be missing the latest years of the time series.</b>"),
                            style="text-align: justify; color:red")),
              column(6, shinycssloaders::withSpinner(plotlyOutput("scatter_plot", height=300)))
              )
            ),
    tabItem(tabName = "Malin_Assessment",
            tags$div(
              tags$image(src = "species/Edible crab.png",height = 300, width = 300,align = 'center'),
              style="text-align: center;color: white; margin-bottom:-4em; margin-top:-4em"
            ),
            fluidRow(
              column(6,
                     tags$div(h3("Introduction"),
                              style="text-align: center"),
                     tags$div(tags$p("Brown Crab (Cancer pagurus) Stock status  in the North West of Ireland was assessed using the Surplus Production Model in Continuous Time (SPiCT;", 
                                     tags$a(href="https://github.com/DTUAqua/spict", "https://github.com/DTUAqua/spict"),
                                     ")"),
                              tags$p("Targeted fisheries for brown crab in Ireland developed during the 1960s. The fishery developed off Malin Head in Donegal and along the Donegal coast and, to a lesser extent, on the south coast during the 1970s. The Malin Head fishery accounted for 25% of national landings during the 1980s. The offshore fishery developed in 1990 and by the mid-1990s had fully explored the distribution of brown crab on the Malin Shelf. This stock, which extends from Donegal to the edge of the continental shelf, is the largest stock fished by Irish vessels. Crab stocks off the southwest and southeast coasts are exploited mainly by Irish vessels <13 m in length inside 12 nm.
                     ICES (WGCRAB) has identified stock units for the purpose of assessment (Figure 1). On the Irish coast these units are identified from tagging data, distribution of fishing activity and larval distribution.
                            "),
                              style="text-align: justify"),
                     tags$div(
                       tags$image(src = "Irish_Stock_limits.png",height = 500, width = 500,align = 'center'),
                       tags$figcaption("Figure 1: Ireland crab stock limits by ICES REectangle"),
                       style="text-align: center; margin-bottom:+2em; margin-top:+2em"),
                     tags$div(h3("Model Caveats"),
                              style="text-align: center"),
                     tags$div(
                       tags$p("The data sources and model settings describe above, 
              do not account for two other potential sources of fishing 
              mortality in the crab stock. These include:",
                              tags$ul(
                                tags$li("Significant volumes of crab are used as whelk bait. If these 
              crab would otherwise have not been landed then it represents an 
              additional mortality directly associated with provision of whelk 
              bait. In particular the whelk bait market involves landing of 
              crab not fit for human consumption and which would otherwise be 
              rejected by the market. In addition there may be direct transfer 
              of crab to whelk bait in vessels that fish both species. This 
              mortality would not accounted for i.e. it is no seen in the 
              landings"),
                                tags$li("Significant volumes of crab have been clawed at sea in 
              recent years. It is unclear if all of this mortality is 
              accounted for and seen in the landings.")
                              ),
                              style="text-align: justify")
                     ),
                     tags$div(
                       tags$image(src = "G.2_KB.png",height = 500, width = 500,align = 'center'),
                       tags$figcaption("Figure 3: Evolution of relative 
                              exploitation and relative stock status from 1990-2020. 
                              F/Fmsy>1 and B/Bmsy<1 indicate overexploitation and loss 
                              in productivity. "),
                       style="text-align: center; margin-bottom:+2em; margin-top:+2em")
                     ),
              column(6,
                     tags$div(h3("Data Sources"),
                              style="text-align: center"),
                     tags$div(tags$p("The data used in the assessment of the Malin Stock 
                            included the time series of landings (including Scottish and Northern Irish) and two independent 
                            biomass indices. All data sources were limited to the 
                            ICES rectangles shown in Figure 1. Landings, as well as 
                            sampling information for vessels under 10m which did not 
                            report the ICES Rectangle were limited to ICES Areas VIa 
                            and VIIb, or Counties Donegal, Mayo and Sligo when the 
                            ICES Area was not available. Even though, the extension 
                            of ICES Area VIa and VIIb (as well as the county limits) 
                            expands beyond the ICES Rectangles shown in Figure 1, 
                            fishing operations for vessels under 10m are known to 
                            occurr within the proposed study area limits."),
                              tags$p("There is no current scientific survey in the area to 
                            estimate brown crab biomass. Commercial catch rate data 
                            can be used as a true index of abundance if the effects 
                            on catch rates of factors (co-variates) other than 
                            changes in crab abundance can be accounted for. 
                            This process is usually referred as catch rate standardization.
                            For this assessment, two commercial catch rates were used, daily 
                            catch rates  collected as part of the SVP programme from 2005-2019
                            (see the Details section of this app for more information), and 
                            haul by haul data from the offshore vivier crab fleet between 1991-2006,
                            Details of the standarization process can be found in the annual"
                                     ,tags$a(href="https://oar.marine.ie/handle/10793/1744","Inshore Stock Book"),
                                     ", as well as in the Marine Institute MyDAS II report (in process of publication)"),
                              style="text-align: justify"),
                     tags$div(h3("Assessment and Stock Status"),
                              style="text-align: center"),
                     tags$p("Several SPiCT scenarios were developed to test consistent trends and convergence. 
                   Across scenarios, comparable diagnosis and outputs were achieved, and model validation 
                   points define by SPiCT developers accepted,thus confirming 
                   model and and data sources suitability.",
                            style="text-align: justify"),
                     tags$p("SPiCT outputs indicates that the stock entered an,
                   overfished status around 2016 (Figure 7). Fishing mortality (F) 
                   is currently higher than optimum fishing mortality rates (Fmsy) 
                   and stock biomass (B) is below the biomass that, on average, 
                   would optimize stock productivity (Bmsy). Stock status derived 
                   from the assessment corresponds closely with industry perception
                   of the stock in recent years derived from questionnaire data 
                   (not shown). The described model has been recently used to 
                   provide advice to DAFM on the relative status of the Malin Crab 
                   stock.",
                            style="text-align: justify"),
                     tags$div(
                       tags$image(src = "Bmsy_G2.png",height = 500, width = 500,align = 'center'),
                       tags$figcaption("Figure 2: Malin Stock relative stock trends"),
                       style="text-align: center; margin-bottom:+2em; margin-top:+2em")
              )
            )
    ),
    tabItem(tabName = "Assessment",
            fluidRow(
              column(6,
                     box(
                       background = "olive", width = NULL,
                       column(6,
                              selectInput("SpIDA",
                                          "Species:",
                                          choices = c("SELECT SPECIES",a_a$Specie),
                                          selected = "SELECT SPECIES")),
                       column(6,
                              selectInput("SpArea",
                                          "Area:",
                                          choices = c("SELECT AREA",a_a$Area),
                                          selected = "SELECT AREA"))
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
                         h3("The Fishery"),
                         column(width = 6, 
                                div(
                                  tags$p(htmlOutput("stock_text")),
                                  style="text-align: justify")
                                ),
                         h3("Species Biology"),
                         column(width = 6, 
                                div(
                                  tags$p(htmlOutput("")),
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
                                imageOutput("survey.zones", height = "50%")
                         )
                         )
                       ),
              tabPanel("Assesment Ouputs",
                       fluidRow(
                         column(width = 6, 
                                div(
                                  tags$p(htmlOutput("Aoutput_text")),
                                  style="text-align: justify"),
                                div(imageOutput("display.size", height = "100%"))
                                ),
                         column(width = 6, 
                                imageOutput("display.assessment", height = "30%")
                         )
                         )
                       #,
                         #column(width = 12, 
                        #        imageOutput("display.size", height = "100%")
                        # )
                        # )
              )
            )
            )
    )
  )
     

ui <- 
  dashboardPage(header, siderbar, body , skin = "green")


