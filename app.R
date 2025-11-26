library(shiny)
library(shinythemes)
library(shinyhelper)
library(shinycssloaders)
library(tidyverse)
library(gghighlight)
library(treemapify)
library(data.table)
library(DT)
library(dplyr)
library(ggplot2)

source("functions.R")

# The colors used in the Plot and in the Tables are from the palette of Ruokavirasto (2022).

################################################################################
# Read data tables:

cleanfinnprioresults <- fread("data/cleanfinnprioresults.csv") # "cleanfinnprioresults.csv" contains FinnPRIO scores./It is used in tabs 1. Plot pests on a graph and 2. Show pests in data table/ The pest's names are used in tab 3. Compare pests by questions
pestquestions <- fread("data/pestquestions_est3.csv")          # "pestquestions_est3.csv" contains FinnPRIO assessments. Each pest is in a column!/ It is used in tab 3. Compare pests by questions
hv <- fread("data/hv.csv")                                     # "hv.csv" contains FinnPRIO hypervolume scores./It is used in tab 4. Rank pests




################################################################################

################################################################################
# Define UI for application that allow to explore FinnPRIO results: ----

ui <- function(request){
  
  navbarPage("FinnPRIO-Explorer",
             theme = shinythemes::shinytheme("sandstone"),
             
             tabPanel("1. Plot pests on a graph",
                      
                      fluidPage(
                        
                        sidebarLayout(
                          
                          # Sidebar with inputs: ----
                          sidebarPanel(
                            
                            h3(strong(style = "font-size:24px;","Selection criteria")),
                            
                            fluidRow(column(6,
                                            tags$h4(strong("FinnPRIO sections to be plotted"), style="color:#7C6A56"),
                                            
                                            # Select variable for Y-axis:----
                                            selectizeInput("yaxis", 
                                                           label = "y-axis",
                                                           choices = c("Entry" = "maahantulo_hallinnan_kanssa_mediaani",
                                                                       "Establishment and spread" = "asettuminen_ja_leviaminen_mediaani",
                                                                       "Invasion" = "invaasion_todennakoisyys_hallinnan_kanssa_mediaani",
                                                                       "Impact" = "vaikutukst_mediaani"),
                                                           selected = "vaikutukst_mediaani"
                                            ),
                                            # Select variable for X-axis:----
                                            selectizeInput("xaxis", 
                                                           label = "x-axis",
                                                           choices = c("Entry" = "maahantulo_hallinnan_kanssa_mediaani",
                                                                       "Establishment and spread" = "asettuminen_ja_leviaminen_mediaani",
                                                                       "Invasion" = "invaasion_todennakoisyys_hallinnan_kanssa_mediaani",
                                                                       "Impact" = "vaikutukst_mediaani"),
                                                           selected = "invaasion_todennakoisyys_hallinnan_kanssa_mediaani"
                                            ),
                                            tags$hr(style="border-color: gray;"),
                                            tags$h4(strong("Quarantine status"), style="color:#7C6A56"),
                                            
                                            # Select pests' status according to the new EU Regulation:----
                                            checkboxGroupInput(inputId = "tuhoojastatus_eu_2016_2031_mukaan",
                                                               label = NULL,
                                                               choices = c(
                                                                 "Priority",
                                                                 "Protected zone",
                                                                 "Emergency measures",
                                                                 "Other quarantine",
                                                                 "RNQP",
                                                                 "Other non-quarantine"
                                                               ),
                                                               selected = c(
                                                                 "Priority",
                                                                 "Protected zone",
                                                                 "Emergency measures",
                                                                 "Other quarantine",
                                                                 "RNQP",
                                                                 "Other non-quarantine"
                                                               ),
                                                               inline = FALSE
                                            ),
                                            
                                            tags$hr(style="border-color: gray;"),
                                            tags$h4(strong("Taxonomic group"), style="color:#7C6A56"),
                                            
                                            #Select pest taxonomic group:----
                                            checkboxGroupInput(inputId = "tuhoojaryhma",
                                                               label = NULL,
                                                               choices = c(
                                                                 "Viruses and viroids",
                                                                 "Bacteria and phytoplasmas", 
                                                                 "Fungi and fungus-like",
                                                                 "Insects",
                                                                 "Mites",
                                                                 "Nematodes",
                                                                 "Snails"
                                                               ),
                                                               selected = c(
                                                                 "Viruses and viroids",
                                                                 "Bacteria and phytoplasmas", 
                                                                 "Fungi and fungus-like",
                                                                 "Insects",
                                                                 "Mites",
                                                                 "Nematodes",
                                                                 "Snails"
                                                               ),
                                                               inline = FALSE
                                            ),
                                            
                            ),
                            
                            column(6, offset = 0,
                                   tags$h4(strong("Presence in Europe"), style="color:#7C6A56"),
                                   
                                   # Select pest presence in Europe: ----
                                   checkboxGroupInput(inputId = "esiintyyko_tuhooja_euroopassa",
                                                      label = NULL, 
                                                      choices = c("Present" = "Yes",
                                                                  "Absent" = "No"),
                                                      selected = c("Yes",
                                                                   "No"),
                                                      inline = FALSE
                                   ),
                                   
                                   tags$hr(style="border-color: gray;"),
                                   tags$h4(strong("Threatened sector"), style="color:#7C6A56"),
                                   
                                   # Select threatened sectors: ----
                                   checkboxGroupInput(inputId = "threatened_sek",
                                                      label = "Trees and shrubs",
                                                      choices = c("Conifers" = "havukasvit_uh",
                                                                  "Broadleaves" = "lehtipuut_ja_pensaat_uh",
                                                                  "Fruits" = "hedelmapuut_uh",
                                                                  "Berries" = "marjakasvit_uh"),
                                                      selected = "havukasvit_uh"),
                                   
                                   checkboxGroupInput(inputId = "threatened_sek2",
                                                      label = "Open-field crops",
                                                      choices = c("Potato" = "peruna_uh",                                                                                   
                                                                  "Sugar beet" = "sokerijuurikas_uh",
                                                                  "Vegetables" = "avomaavihannekset_uh",                                                                        
                                                                  "Other" = "muut_avomaakasvit_uh")),
                                   
                                   
                                   checkboxGroupInput(inputId = "threatened_sek3",
                                                      label = "Greenhouse crops",
                                                      choices = c("Cucumber" = "kasvihuonekurkku_uh",                                                                         
                                                                  "Tomato" = "kasvihuonetomaatti_uh",                                                                       
                                                                  "Pepper" = "kasvihuonepaprika_uh",                                                                        
                                                                  "Lettuce" = "kasvihuonesalaatti_uh",                                                                       
                                                                  "Ornamentals" = "kasvihuonekoristekasvit_uh")),
                                   checkboxGroupInput(inputId = "threatened_sek4",
                                                      label = "Others",
                                                      choices = c("Others" = "muut_uh")),
                                   
                            )),
                            
                            tags$hr(style="border-color: gray;"),
                            tags$h4(strong("Determine the pests shown on the plot based on"), style="color:#7C6A56"),
                            fluidRow(
                              column(3,
                                     tags$br(),
                                     tags$h5(strong("Entry score")),
                                     tags$br(),
                                     tags$h5(strong("Establishment and spread score")),
                                     tags$br(),
                                     tags$h5(strong("Invasion score")),
                                     tags$br(),
                                     tags$h5(strong("Impact score")),
                                     
                              ),
                              
                              column(9,
                                     # Scores selections----
                                     sliderInput(inputId = "entry_score", 
                                                 label = NULL,
                                                 min = 0, max = 1, value = c(0,1), step = 0.05, width = "350px"),
                                     
                                     sliderInput(inputId = "establishment_score", 
                                                 label = NULL,
                                                 min = 0, max = 1, value = c(0,1), step = 0.05, width = "350px"),
                                     
                                     sliderInput(inputId = "invasion_score", 
                                                 label = NULL,
                                                 min = 0, max = 1, value = c(0,1), step = 0.05, width = "350px"),
                                     sliderInput(inputId = "impact_score", 
                                                 label = NULL,
                                                 min = 0, max = 1, value = c(0,1), step = 0.05, width = "350px"),
                              ))
                          ),
                          
                          # Main panel with the results after selections: -----
                          mainPanel(
                            fluidRow(
                              column(9,
                                     tags$br(),
                                     # Display a plot that presents Invasion vs. Impact:
                                     plotOutput(outputId = "pest_plot",
                                                height = "650px", 
                                                brush = brushOpts(id = "plot_brush", resetOnNew = TRUE)
                                     ), 
                                     tags$br(),
                                     column(12,
                                            helpText("To generate a table that contains some (or all) of the pests that are presented on the plot, select an area using the pointer.",
                                                     "Note that this works only for the 'Single plot' option.")
                                     )
                              ),
                              
                              column(3,
                                     wellPanel( 
                                       tags$h4(strong("Show")),
                                       # Pests' names on the plot:
                                       checkboxInput(inputId = "tuhoojanimi",
                                                     label = "Pest names",
                                                     value = FALSE,
                                                     width = "auto"),
                                       
                                       # Error bars on the plot:
                                       help_add(),
                                       tags$h5(strong("Uncertainty")),
                                       checkboxInput(inputId = "err_25_ja_75_prosenttipiste",
                                                     label = "Whiskers for y",
                                                     value = TRUE),
                                       checkboxInput(inputId = "err_25_ja_75_prosenttipiste1",
                                                     label = "Whiskers for x",
                                                     value = TRUE),
                                       
                                       help_threshold(), 
                                       tags$h5(strong("Threshold line")),
                                       fluidRow(
                                         column(1,
                                                tags$h5(strong("y")),
                                                tags$br(),
                                                tags$h5(strong("x"))
                                                
                                         ),
                                         column(10,
                                                # A threshold line for the impact on the plot:
                                                numericInput(inputId ="threshold", 
                                                             label=NULL, 
                                                             value = 0,
                                                             min = 0, max = 1, step = 0.05),
                                                numericInput(inputId ="thresholdX", 
                                                             label=NULL, 
                                                             value = 0,
                                                             min = 0, max = 1, step = 0.05),
                                         ))
                                     ),
                                     
                                     wellPanel(
                                       help_plot_grid(),
                                       #Number of plots:
                                       radioButtons(inputId = "split_plott",
                                                    label = "Number of plots",
                                                    choices = c("Single plot" = 1, 
                                                                "Multiple plots" = 2
                                                    ), 
                                                    selected = 1),
                                       tags$h5(strong("Zoom by")),
                                       fluidRow(
                                         column(1,
                                                tags$h5(strong("y")),
                                                tags$br(),tags$br(),
                                                tags$h5(strong("x"))
                                         ),
                                         column(10,
                                                #Zoom the plot by x and y:
                                                sliderInput(inputId="ylims",
                                                            label=NULL, 
                                                            min = 0, max = 1, value = c(0,1), step = 0.05),
                                                sliderInput(inputId="xlims",
                                                            label=NULL, 
                                                            min = 0, max = 1, value = c(0,1), step = 0.05),
                                         ))
                                     ),
                                     wellPanel(
                                       fluidRow(
                                         column(7,
                                                #Download with options:
                                                downloadButton(outputId = "download", 
                                                               label = "Save Plot")
                                         ),
                                         column(5,offset = 0,
                                                radioButtons(inputId = "extension", 
                                                             label = NULL,
                                                             choices = c("png", "pdf"), inline = FALSE)
                                         )
                                       )))
                            ),
                            
                            tags$hr(style="border-color: gray;"),
                            # helpText("Note that currently the downloaded table contains the scores for all sectors, i.e., Entry, Establishments and spread, Invasion and Impact"),
                            fluidRow(
                              # Table brush selection:
                              
                              helpText(tags$sup(1), "Note that the upper row of the table's header is not visible in the downloaded table."),
                              
                              DT::dataTableOutput(outputId = "tableBrush"
                              )
                            ),
                            tags$br()
                            
                          )
                        )
                      )
             ),
             
             tabPanel("2. Show pests in data table",
                      helpText(tags$sup(1), "Note that the upper row of the table's header is not visible in the downloaded table."),
                      #helpText(strong(style="color: #D2132E;", tags$sup(2), "Link to EPPO GD.")),
                      DT::dataTableOutput(outputId = "table_all")
             ),
             
             tabPanel("3. Compare pests by questions",
                      fluidPage(
                        fluidRow(
                          column(3,
                                 wellPanel(h3(strong(style = "font-size:24px;", "Selection criteria")),
                                           tags$br(),
                                           selectInput(inputId = "Codes",
                                                       label = "Section",
                                                       choices = c("Entry" = "ENT",
                                                                   "Establishment and spread" = "EST",
                                                                   "Impact" = "IMP",
                                                                   "Management" = "MAN"),
                                                       selected = c("Entry" = "ENT")
                                           ),
                                           tags$hr(style="border-color: gray;"),
                                           tags$br(),
                                           selectInput(inputId = "pest1_sel",
                                                       label = "Pest 1",
                                                       #choices=c()
                                                       choices = cleanfinnprioresults$tuhooja
                                           ),
                                           tags$br(),
                                           selectInput(inputId = "pest2_sel",
                                                       label = "Pest 2",
                                                       choices = cleanfinnprioresults$tuhooja,
                                                       selected = "Aculops fuchsiae"
                                           )
                                           
                                 ) ,
                                 tags$br(),
                                 uiOutput(outputId = "helpPW")
                                 
                          ),
                          column(9,
                                 DT::dataTableOutput(outputId = "pest_1_2"),
                                 tags$br()
                          )
                        )
                      )
                      
             ),
             
             tabPanel("4. Rank pests",
                      tags$h4(strong("Ranking of the pests based on stochastic dominance and hypervolume"), style="color:#7C6A56"),
                      radioButtons(inputId = "switch_tree",
                                   label = NULL,
                                   choices = c("Entry" = 1, 
                                               "Establishment and spread" = 2,
                                               "Invasion" = 3,
                                               "Impact" = 4
                                   ), 
                                   selected = 4,
                                   inline = TRUE),
                      
                      shinycssloaders::withSpinner(plotOutput("hv_tree", width = "100%", height = "600px"),
                                                   type = 7, color = "#7C6A56", size = 1),
                      
                      
                      tags$br(),
                      helpText(tags$sup(1), "Note that the upper row of the table's header is not visible in the downloaded table."),
                      DT::dataTableOutput(outputId = "table_hv")
             ),
             
             tabPanel("About the app",
                      fluidRow(column(6,
                                      
                                      wellPanel( 
                                        p(tags$p(
                                          tags$b(style = "font-size:13px;",
                                                 "FinnPRIO-Explorer facilitates examination of assessments made with the FinnPRIO model", 
                                                 tags$a("(Heikkila et al. 2016)",href="https://doi.org/10.1007/s10530-016-1123-4", target="_blank"),". It was developed in 
                                                 the Risk Assessment Unit of the Finnish Food Authority, and it contains all the FinnPRIO assessments done for Finland.")),
                                          tags$br(),
                                          tags$b("FinnPRIO model"),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "FinnPRIO is a model for ranking non-native plant pests based on the risk that they pose to plant health", 
                                                 tags$a("(Heikkila et al. 2016)",href="https://doi.org/10.1007/s10530-016-1123-4", target="_blank"),". 
                                                 It is composed of five sections: likelihood of entry, likelihood of establishment and spread, magnitude of 
                                                 impacts, preventability, and controllability. The score describing the likelihood of invasion is a product 
                                                 of entry and establishment scores. The score describing the manageability of invasion is the minimum of 
                                                 prevantability and controllability scores."),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "FinnPRIO consists of multiple-choice questions with different answer options yielding a different number 
                                                 of points. For each question, the most likely answer option and the plausible minimum and maximum options 
                                                 are selected based on the available scientific evidence. The selected answer options are used to define 
                                                 a PERT probability distribution and the total section scores are obtained with Monte Carlo simulation. 
                                                 The resulting probability distributions of the section scores describe the uncertainty of the assessment."),
                                          
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "In FinnPRIO-Explorer, summary statistics (median and 25th and 75th percentiles) of the score distributions 
                                                 can be explored in the tab 'Plot pests on a graph' while ranking that is based on the whole probability 
                                                 distributions can be studied in the tab 'Rank pests'."),
                                          tags$br(),
                                          tags$b("FinnPRIO assessments for Finland"),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "The FinnPRIO assessments included in the app were done using the FinnPRIO graphical user interface ", 
                                                 tags$a("(Marinova-Todorova et al. 2019)",href="https://doi.org/10.5281/zenodo.2784027", target="_blank"),"."),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;",
                                                 "The probability distributions of the scores were simulated with 5000 iterations. The lambda parameter 
                                                 of the PERT distributions was set to 1, implying a low confidence of the most likely estimate. Equal 
                                                 weight was given to economic (50%) and environmental and social impacts (50%). The likelihood of entry 
                                                 is assessed taking into account the current management measures."),
                                          tags$br(),
                                          tags$b("Ranking FinnPRIO assessments using the hypervolume approach"),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;", 
                                                 "To facilitate comparison of the FinnPRIO score distributions, a hypervolume (HV) approach was used to 
                                                 aggregate distributions into a simple single-dimensional form that reveals the preference order relationship 
                                                 of the distributions (for details, see  ", 
                                                 tags$a("Yemshanov et al. 2012,",href="https://doi.org/10.1111/j.1472-4642.2011.00848.x", target="_blank"),  
                                                 tags$a("2017;",href="https://doi.org/10.1016/j.jenvman.2017.02.021", target="_blank"), "for practical examples, see",
                                                 tags$a("Tuomola et al. 2018;",href="https://doi.org/10.3391/mbi.2018.9.2.05", target="_blank"), 
                                                 tags$a("Marinova-Todorova et al. 2020",href="https://doi.org/10.1111/epp.12667", target="_blank"),")."),
                                          tags$p(),
                                          tags$p(style = "font-size:13px;", 
                                                 "Ranking with the pairwise stochastic dominance rule and the HV indicator calculations were performed using 
                                                 a stand-alone program written in C++ that applies the hypervolume calculation algorithm from", 
                                                 tags$a("While et al. (2012)", href="https://ieeexplore.ieee.org/document/5766730", target="_blank"), ". 
                                                 The program was kindly provided by Denys Yemshanov from Natural Resources Canada. The cumulative distribution 
                                                 functions were calculated from the score distributions at 70 equal intervals and ordered using the first-order 
                                                 stochastic dominance rule.")
                                        )
                                      )),
                               
                               
                               column(5,
                                      tabsetPanel(
                                        
                                        tabPanel("References",
                                                 p(tags$p(
                                                   tags$br(),
                                                   
                                                   tags$p(style = "font-size:13px;","Heikkila J, Tuomola J, Pouta E & Hannunen S (2016) FinnPRIO: a model for ranking invasive 
                                                          plant pests based on risk. Biological Invasions 18, 1827- 1842. ",
                                                          tags$a("doi.org/10.1007/s10530-016-1123-4",href="https://doi.org/10.1007/s10530-016-1123-4", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Marinova-Todorova M, Bjorklund N, Boberg J, Flo D, Tuomola J, Wendell M & Hannunen S (2020), 
                                                          Screening potential pests of Nordic coniferous forests associated with trade in ornamental plants. 
                                                          EPPO Bulletin 50: 249- 267. ",
                                                          tags$a("doi.org/10.1111/epp.12667",href="https://doi.org/10.1111/epp.12667", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Marinova-Todorova M, Tuomola J, Heikkila J & Hannunen S. (2019) A graphical user interface for the FinnPRIO model: 
                                                          A model for ranking plant pests based on risk (Version 1.0). Finnish Food Authority. ",
                                                          tags$a("doi.org/10.5281/zenodo.2784027",href="https://doi.org/10.5281/zenodo.2784027", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Tuomola J, Yemshanov D, Huitu H & Hannunen S (2018) Mapping risks of pest invasions based on the spatio-temporal 
                                                          distribution of hosts. Management of Biological Invasions 9, 115- 126. ",
                                                          tags$a("doi.org/10.3391/mbi.2018.9.2.05",href="https://doi.org/10.3391/mbi.2018.9.2.05", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","While L, Bradstreet L & Barone L (2012) A fast way of calculating exact hypervolumes. IEEE Transactions 
                                                          on Evolutionary Computation 16, 86- 95. doi:",
                                                          tags$a("10.1109/TEVC.2010.2077298",href="https://ieeexplore.ieee.org/document/5766730", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Yemshanov D, Koch FH, Bo L, Fournier R, Cook G & Turgeon JJ (2017) A new hypervolume approach for assessing 
                                                          environmental risks. Journal of Environmental Management 193, 188- 200. ",
                                                          tags$a("doi.org/10.1016/j.jenvman.2017.02.021",href="https://doi.org/10.1016/j.jenvman.2017.02.021", target="_blank")),
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;","Yemshanov D, Koch FH, Lyons B, Ducey M & Koehler K (2012) A dominance-based approach to map risks of 
                                                          ecological invasions in the presence of severe uncertainty. Diversity and Distributions 18, 33- 46.",
                                                          tags$a("doi.org/10.1111/j.1472-4642.2011.00848.x",href="https://doi.org/10.1111/j.1472-4642.2011.00848.x", target="_blank"))
                                                 ))
                                                 
                                        ),
                                        tabPanel("Source code",
                                                 p(tags$p(
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;",
                                                          "The source code is available at ",
                                                          tags$a("Zenodo",href="https://doi.org/10.5281/zenodo.7016771", target="_blank"),
                                                          " under the ",tags$a("GNU General Public License version 3", href="https://opensource.org/licenses/GPL-3.0", target="_blank"), ".")
                                                   
                                                 ))
                                        ),
                                        tabPanel("How to refer?",
                                                 p(tags$p(
                                                   tags$br(),
                                                   tags$p(style = "font-size:13px;",
                                                          "Marinova-Todorova M, Tuomola J and Hannunen S (2022) FinnPRIO-Explorer - 
                                                          A tool for examining assessments made with the FinnPRIO model. Finnish Food Authority, 
                                                          Helsinki, Finland.",
                                                          "Available at ", tags$a("https://finnprio-explorer.rahtiapp.fi/", href="https://finnprio-explorer.rahtiapp.fi/", target="_blank"),", ",tags$a("doi.org/10.5281/zenodo.7016771", href="https://doi.org/10.5281/zenodo.7016771", target="_blank"))
                                                   
                                                 ))
                                        )
                                        
                                      ))
                      )
             ))
  
}

################################################################################

# Define server ------
server <- function(input, output, session) {
  
  observe_helpers()
  
  ###############################################################
  # Filter data:----
  ###############################################################
  
  ## Subset data -> filter data based on the input selections:-----
  selected_status <- reactive({
    
    #req(input$impact_score)
    x1 <- input$threatened_sek
    x2 <- input$threatened_sek2
    x3 <- input$threatened_sek3 
    x4 <- input$threatened_sek4
    thr_sec <- c(x1, x2, x3, x4)
    
    if (is.null(input$tuhoojastatus_eu_2016_2031_mukaan) | is.null(input$tuhoojaryhma) | 
        is.null(input$esiintyyko_tuhooja_euroopassa) | is.null(thr_sec) == TRUE) {
      validate(
        need(input$tuhoojastatus_eu_2016_2031_mukaan, "Select a quarantine status"),
        need(input$tuhoojaryhma, "Select a taxonomic group(s)"),
        need(input$esiintyyko_tuhooja_euroopassa, "Select presence in Europe"),
        need(thr_sec, "Select a threatened sector(s)")
      )
      
    }else  {
      
      cleanfinnprioresults %>%
        filter(
          tuhoojastatus_eu_2016_2031_mukaan  %in% input$tuhoojastatus_eu_2016_2031_mukaan,
          tuhoojaryhma %in% input$tuhoojaryhma,
          esiintyyko_tuhooja_euroopassa %in% input$esiintyyko_tuhooja_euroopassa,
          maahantulo_hallinnan_kanssa_mediaani > input$entry_score[1] & maahantulo_hallinnan_kanssa_mediaani < input$entry_score[2],
          asettuminen_ja_leviaminen_mediaani > input$establishment_score[1] & asettuminen_ja_leviaminen_mediaani < input$establishment_score[2],
          invaasion_todennakoisyys_hallinnan_kanssa_mediaani > input$invasion_score[1] & invaasion_todennakoisyys_hallinnan_kanssa_mediaani < input$invasion_score[2],
          vaikutukst_mediaani > input$impact_score[1] & vaikutukst_mediaani < input$impact_score[2]
        )
    }
  })
  
  
  ## Selection of threatened sector: ---- 
  ## https://dplyr.tidyverse.org/reference/filter_all.html
  selected_status1 <- reactive({ 
    
    filter_at(
      selected_status(), vars(input$threatened_sek, input$threatened_sek2,input$threatened_sek3, input$threatened_sek4), 
      any_vars(. == "Yes")) ### https://stackoverflow.com/questions/53197150/filter-multiple-columns-by-value-using-checkbox-group
    
  })
  
  
  ## Subset data -> filter data for the table generated under the plot:----
  infotable <- reactive({
    req(input$plot_brush)
    
    select(selected_status1(), 
           tuhooja, 
           tuhoojaryhma, 
           esiintyyko_tuhooja_euroopassa,
           maahantulo_hallinnan_kanssa_25_prosenttipiste,
           maahantulo_hallinnan_kanssa_mediaani,
           maahantulo_hallinnan_kanssa_75_prosenttipiste,
           asettuminen_ja_leviaminen_25_prosenttipiste,
           asettuminen_ja_leviaminen_mediaani,
           asettuminen_ja_leviaminen_75_prosenttipiste,
           invaasion_todennakoisyys_hallinnan_kanssa_25_prosenttipiste,
           invaasion_todennakoisyys_hallinnan_kanssa_mediaani, 
           invaasion_todennakoisyys_hallinnan_kanssa_75_prosenttipiste,
           vaikutukst_25_prosenttipiste,
           vaikutukst_mediaani,
           vaikutukst_75_prosenttipiste
           
    )
  })
  
  
  ###############################################################
  # Display data on the tab-panels:----
  ###############################################################
  
  ###############################################################
  
  ## Data used for generating the table from brush in "1. Select pests to plot"-tab: ----
  selected_pests <- reactive({
    
    infotable() %>%
      brushedPoints(input$plot_brush, input$xaxis, input$yaxis)
    
  })
  
  ## Generate table from the plot using brush in "1. Select pests to plot"-tab:----
  output$tableBrush <- DT::renderDataTable({
    
    req(input$xaxis)
    req(input$yaxis)
    
    # Returns rows from a data frame which are selected with the brush used in plotOutput:
    DT::datatable(selected_pests(),
                  colnames = c("Sort" = "tuhooja", 
                               "Pest" = "tuhooja", 
                               "Taxonomic group" = "tuhoojaryhma", 
                               "Presence in Europe" = "esiintyyko_tuhooja_euroopassa",
                               "Entry, min" = "maahantulo_hallinnan_kanssa_25_prosenttipiste",
                               "Entry, median" = "maahantulo_hallinnan_kanssa_mediaani",
                               "Entry, max" = "maahantulo_hallinnan_kanssa_75_prosenttipiste",
                               "Establishment and spread, min" = "asettuminen_ja_leviaminen_25_prosenttipiste",
                               "Establishment and spread, median" = "asettuminen_ja_leviaminen_mediaani",
                               "Establishment and spread, max" = "asettuminen_ja_leviaminen_75_prosenttipiste",
                               "Invasion, min" = "invaasion_todennakoisyys_hallinnan_kanssa_25_prosenttipiste",
                               "Invasion, median" = "invaasion_todennakoisyys_hallinnan_kanssa_mediaani", 
                               "Invasion, max" = "invaasion_todennakoisyys_hallinnan_kanssa_75_prosenttipiste",
                               "Impact, min" = "vaikutukst_25_prosenttipiste",
                               "Impact, median" = "vaikutukst_mediaani",
                               "Impact, max" = "vaikutukst_75_prosenttipiste"
                  ),
                  class = 'cell-border stripe',
                  container = tbl_hdr, # -> Transforms the table header into 2 rows. To make changes go to 'tbl_hdr' in functions.R
                  rownames = TRUE, # It is required for the brush option to have "rownames = TRUE"!
                  extensions = "Buttons",
                  options = list(
                    ## Hide the first column that contains rownames and display columns based on the selections for x and y:
                    columnDefs = list(list(targets = c(0), visible = FALSE), 
                                      if(input$xaxis == "invaasion_todennakoisyys_hallinnan_kanssa_mediaani" | input$yaxis == "invaasion_todennakoisyys_hallinnan_kanssa_mediaani") {
                                        list(targets = c(10,11,12), visible = TRUE)
                                      } else list(targets = c(10,11,12), visible = FALSE),
                                      if(input$xaxis == "maahantulo_hallinnan_kanssa_mediaani" | input$yaxis == "maahantulo_hallinnan_kanssa_mediaani") {
                                        list(targets = c(4,5,6), visible = TRUE)
                                      } else list(targets = c(4,5,6), visible = FALSE),
                                      if(input$xaxis == "asettuminen_ja_leviaminen_mediaani" | input$yaxis == "asettuminen_ja_leviaminen_mediaani") {
                                        list(targets = c(7,8,9), visible = TRUE)
                                      } else list(targets = c(7,8,9), visible = FALSE),
                                      if(input$xaxis == "vaikutukst_mediaani" | input$yaxis == "vaikutukst_mediaani") {
                                        list(targets = c(13,14,15), visible = TRUE)
                                      } else list(targets = c(13,14,15), visible = FALSE)
                    ),
                    
                    ## Add build-in buttons for download
                    dom = "Bfrtip", #createEmptyCells = TRUE,
                    buttons = list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span> Excel <sup>1</sup>', title = NULL, 
                                        exportOptions = list(columns = ":visible")), 
                                   list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV <sup>1</sup>', title = NULL, 
                                        exportOptions = list(columns = ":visible"))
                    ),
                    ## All results appear on same page:
                    paging=FALSE)) %>%
      
      formatStyle("Pest",  color = "black", fontWeight = "bold", fontStyle = "normal") %>%
      formatRound(c("Entry, min", "Entry, median", "Entry, max", "Establishment and spread, min", 
                    "Establishment and spread, median", "Establishment and spread, max", 
                    "Invasion, min", "Invasion, median", "Invasion, max",
                    "Impact, min", "Impact, median", "Impact, max"), 2) %>%
      formatStyle("Entry, median",
                  background = styleColorBar(cleanfinnprioresults$maahantulo_hallinnan_kanssa_mediaani, "#DAE375"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Establishment and spread, median",
                  background = styleColorBar(cleanfinnprioresults$asettuminen_ja_leviaminen_mediaani, "#6D9F80"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Invasion, median",
                  background = styleColorBar(cleanfinnprioresults$invaasion_todennakoisyys_hallinnan_kanssa_mediaani, "#CEB888"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Impact, median",
                  background = styleColorBar(cleanfinnprioresults$vaikutukst_mediaani, "#DE4C9A"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") 
    
  })
  
  ###############################################################
  
  # Assign colors to each quarantine status for the plot:
  cols <- c("Other quarantine"="#D0006F",
            "Priority"="#F7CE3C",
            "Protected zone" ="#6D9F80",
            "Emergency measures"= "#FF6F49", # "#CEB888",
            "RNQP"="#004F71",
            "Other non-quarantine"="#ADD2EE")
  
  
  ## Generate a plot in "1. Select pests to plot"-tab:----
  
  plot_output <- reactive({   
    
    
    p <- ggplot(selected_status1(),
                aes_string(x = input$xaxis, 
                           y = input$yaxis, 
                           color = "tuhoojastatus_eu_2016_2031_mukaan"
                )) + 
      
      geom_point(size = 3) +  
      
      xlim(min = 0, max = 1) + ylim(min = 0, max = 1)  + 
      labs(
        caption = paste("    The dots indicate the simulated median score, and the whiskers show the 25th and the 75th percentiles of the distribution of the scores.
                         \n    Number of pests: ", nrow(selected_status1())),
        color = "" # "Quarantine status:"
      ) +
      
      gghighlight(use_direct_label = FALSE, 
                  keep_scales = TRUE) +
      
      theme_bw() +
      #### Size of the plot scales:
      theme(axis.text = element_text(size=12), 
            #### Size of the plot labels:
            axis.title = element_text(size=14,face="bold")) +  
      #### Size of the plot title:
      theme(plot.title = element_text(size=16,hjust=0.5, vjust=0.25, face="bold")) +
      #### Size of the caption that shows number of pests:
      theme(plot.caption = element_text(size=10, hjust=0, face="bold"), plot.caption.position = "plot") +                      
      theme(axis.title.x=element_text(vjust=-1))+
      theme(axis.title.y=element_text(vjust=1))+
      guides(colour = guide_legend(nrow = 1)) +
      theme(legend.text = element_text(size=10),
            legend.direction = "horizontal",
            legend.position = "bottom") +
      
      #Add to scale_color_manual 'limits = force', if want to update the legend. Note that the order becomes alphabetical!
      scale_color_manual(aesthetics = "color", values = cols,limits = force) +
      
      # Zoom the plot:
      coord_cartesian(xlim = c(input$xlims[1], input$xlims[2]), ylim = c(input$ylims[1], input$ylims[2]), expand = TRUE)
    
    
    #####################  
    # Plot conditions:---
    ##################### 
    
    # Add label to X:
    if(input$xaxis == "invaasion_todennakoisyys_hallinnan_kanssa_mediaani") {
      p <- p+labs(x = "Invasion score") 
    } else if(input$xaxis == "maahantulo_hallinnan_kanssa_mediaani") {
      p <- p+labs(x = "Entry score")
    } else if(input$xaxis == "asettuminen_ja_leviaminen_mediaani") {
      p <- p+labs(x = "Establishment and spread score")
    } else if(input$xaxis == "vaikutukst_mediaani") {
      p <- p+labs(x = "Impact score")
    }
    # Add label to Y:
    if(input$yaxis == "invaasion_todennakoisyys_hallinnan_kanssa_mediaani") {
      p <- p+labs( y = "Invasion score") 
    } else if(input$yaxis == "maahantulo_hallinnan_kanssa_mediaani") {
      p <- p+labs( y = "Entry score")
    } else if(input$yaxis == "asettuminen_ja_leviaminen_mediaani") {
      p <- p+labs( y = "Establishment and spread score")
    } else if(input$yaxis == "vaikutukst_mediaani") {
      p <- p+labs( y = "Impact score")
    }
    
    
    # Switch between single and multiple plots:
    p <- p +
      {switch(input$split_plott,
              "2" = (
                facet_wrap(~ tuhoojastatus_eu_2016_2031_mukaan))
      )
      }
    
    
    # Threshold line (Y): 
    p <- p +  
      {if(input$threshold)
        geom_hline(yintercept = input$threshold, 
                   linetype="dotted",
                   colour = "red", 
                   size = 1, 
                   na.rm = FALSE)} 
    
    # Threshold line (X): 
    p <- p +
      {if(input$thresholdX)
        geom_vline(xintercept = input$thresholdX, 
                   linetype="dotted",
                   colour = "red", 
                   size = 1, 
                   na.rm = FALSE)}
    
    
    # Display pests' names when select checkbox: ----
    p <- p +
      {if(input$tuhoojanimi)    
        geom_text(aes(label = tuhooja), 
                  size = 4, 
                  vjust = -0.05, 
                  hjust = -0.08)} 
    
    
    # Display error bars from 25th and 75th percentile when select checkbox for X: ----
    p <- p +  
      {if(input$err_25_ja_75_prosenttipiste1 && input$xaxis == "invaasion_todennakoisyys_hallinnan_kanssa_mediaani") 
        geom_errorbar(aes(xmax = invaasion_todennakoisyys_hallinnan_kanssa_75_prosenttipiste, 
                          xmin = invaasion_todennakoisyys_hallinnan_kanssa_25_prosenttipiste),  
                      width = 0.01)
        
        else if(input$err_25_ja_75_prosenttipiste1 && input$xaxis == "asettuminen_ja_leviaminen_mediaani") 
          geom_errorbar(aes(xmax = asettuminen_ja_leviaminen_75_prosenttipiste, 
                            xmin = asettuminen_ja_leviaminen_25_prosenttipiste),  
                        width = 0.01)
        
        else if(input$err_25_ja_75_prosenttipiste1 && input$xaxis == "maahantulo_hallinnan_kanssa_mediaani") 
          geom_errorbar(aes(xmax = maahantulo_hallinnan_kanssa_75_prosenttipiste, 
                            xmin = maahantulo_hallinnan_kanssa_25_prosenttipiste),  
                        width = 0.01)
        
        else if(input$err_25_ja_75_prosenttipiste1 && input$xaxis == "vaikutukst_mediaani") 
          geom_errorbar(aes(xmax = vaikutukst_75_prosenttipiste, 
                            xmin = vaikutukst_25_prosenttipiste),  
                        width = 0.01)} 
    
    
    # Display error bars from 25th and 75th percentile when select checkbox for Y: ----
    p <- p + 
      {if(input$err_25_ja_75_prosenttipiste && input$yaxis == "vaikutukst_mediaani") 
        geom_errorbar(aes(ymax = vaikutukst_75_prosenttipiste, 
                          ymin = vaikutukst_25_prosenttipiste),  
                      width = 0.01)
        
        else if(input$err_25_ja_75_prosenttipiste && input$yaxis == "invaasion_todennakoisyys_hallinnan_kanssa_mediaani") 
          geom_errorbar(aes(ymax = invaasion_todennakoisyys_hallinnan_kanssa_75_prosenttipiste, 
                            ymin = invaasion_todennakoisyys_hallinnan_kanssa_25_prosenttipiste),  
                        width = 0.01)
        
        else if(input$err_25_ja_75_prosenttipiste && input$yaxis == "asettuminen_ja_leviaminen_mediaani") 
          geom_errorbar(aes(ymax = asettuminen_ja_leviaminen_75_prosenttipiste, 
                            ymin = asettuminen_ja_leviaminen_25_prosenttipiste),  
                        width = 0.01)
        
        else if(input$err_25_ja_75_prosenttipiste && input$yaxis == "maahantulo_hallinnan_kanssa_mediaani") 
          geom_errorbar(aes(ymax = maahantulo_hallinnan_kanssa_75_prosenttipiste, 
                            ymin = maahantulo_hallinnan_kanssa_25_prosenttipiste),  
                        width = 0.01)}  
    
    p
    
    
  })
  
  
  output$pest_plot <- renderPlot({
    plot_output()
  })
  
  
  ###############################################################
  # Download the plot:----
  
  output$download <- downloadHandler(
    filename = function() {
      paste("plot", input$extension, sep = ".")
    },
    content = function(file){
      ggsave(file, plot_output(), device = input$extension,  width = 15, height = 10)
    }
  )
  
  
  
  
  ###############################################################
  ###############################################################
  
  ## Generate table with all assessed pests in "2. Show pests in data table"-tab:-----
  output$table_all <- DT::renderDataTable({
    
  
    #The format of the following columns is converted from character to factor, so the selectize inputs (list in filter options) to be available:
    cleanfinnprioresults$tuhoojastatus_eu_2016_2031_mukaan <- as.factor(cleanfinnprioresults$tuhoojastatus_eu_2016_2031_mukaan)
    cleanfinnprioresults$tuhoojaryhma <- as.factor(cleanfinnprioresults$tuhoojaryhma)
    cleanfinnprioresults$esiintyyko_tuhooja_euroopassa <- as.factor(cleanfinnprioresults$esiintyyko_tuhooja_euroopassa)
  
    #Creating links to EPPO Global Database:
    eppocode = cleanfinnprioresults$eppo_code
    base_link = "https://gd.eppo.int/taxon/"
    eppoGD = paste0('<a href="', base_link, eppocode,'" target="_blank">', eppocode, '</a>')
    
    #Add new column to the datatable:
    cleanfinnprioresults[,  ':='(eppo = eppoGD)]
    
    
    
    DT::datatable(select(cleanfinnprioresults, 
                         tuhooja, 
                         #eppo_code,
                         eppo,
                         tuhoojaryhma,
                         tuhoojastatus_eu_2016_2031_mukaan,
                         esiintyyko_tuhooja_euroopassa, 
                         maahantulo_hallinnan_kanssa_mediaani,
                         asettuminen_ja_leviaminen_mediaani,
                         invaasion_todennakoisyys_hallinnan_kanssa_mediaani, 
                         vaikutukst_mediaani,
                         maahantulon_hallittavuus_mediaani,
                         leviamisen_hallittavuus_mediaani,
                         hallittavuus_mediaani,
                         assessment_date
    ),
    
    colnames = c("Sort" = "tuhooja", 
                 "Pest" = "tuhooja", 
                 #"EPPO code" = "eppo_code",
                 "EPPO Code" = "eppo",
                 "Taxonomic group" = "tuhoojaryhma", 
                 "Quarantine status" = "tuhoojastatus_eu_2016_2031_mukaan",
                 "Presence in Europe" = "esiintyyko_tuhooja_euroopassa", 
                 "Entry" = "maahantulo_hallinnan_kanssa_mediaani", 
                 "Establishment and spread" = "asettuminen_ja_leviaminen_mediaani",
                 "Invasion" = "invaasion_todennakoisyys_hallinnan_kanssa_mediaani", 
                 "Impact" = "vaikutukst_mediaani",
                 "Preventability" = "maahantulon_hallittavuus_mediaani",
                 "Controllability"= "leviamisen_hallittavuus_mediaani",
                 "Manageability"= "hallittavuus_mediaani",
                 "Assessed, month/year" = "assessment_date"),
    container = tbl_hdr_all, # -> Transforms the table header into 2 rows. To make changes go to 'tbl_hdr_all' in functions.R
    rownames = TRUE,
    extensions = "Buttons",
    filter = list(position = 'top', clear = FALSE),
    options = list(
      #pageLength = 15,
      #columnDefs = list(),
      #fixedHeader = TRUE,
      
      ## Hide the first column that contains rownames
      columnDefs = list(list(targets = c(0), visible = FALSE)),
      
      paging = FALSE,
      autoWidth = TRUE, 
      
      # Display and customize the buttons:
      dom = "Bfrtip", 
      buttons = list(list(extend = "excel", 
                          text = '<span class="glyphicon glyphicon-th"></span> Excel <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible")), 
                     list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible"))
      )
    ),
    class = "cell-border stripe",
    
    #To use the links in the column 'EPPO Codes', 'escape=FALSE' should be enabled:
    escape = FALSE) %>% 
    # escape = c(1,2, 4:13)) %>% 
      
      formatRound(c("Entry","Establishment and spread", "Invasion", 
                    "Impact", "Preventability", "Controllability", "Manageability"), 3) %>%
      formatStyle("Pest",  fontWeight = "bold", fontStyle = "normal") %>%
      
      formatStyle("Entry",
                  background = styleColorBar(cleanfinnprioresults$maahantulo_hallinnan_kanssa_mediaani, "#DAE375"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Establishment and spread",
                  background = styleColorBar(cleanfinnprioresults$asettuminen_ja_leviaminen_mediaani, "#6D9F80"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Invasion",
                  background = styleColorBar(cleanfinnprioresults$invaasion_todennakoisyys_hallinnan_kanssa_mediaani, "#CEB888"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Impact",
                  background = styleColorBar(cleanfinnprioresults$vaikutukst_mediaani, "#DE4C9A"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Manageability",
                  background = styleColorBar(cleanfinnprioresults$vaikutukst_mediaani, "#ADD2EE"),
                  backgroundSize = "98% 88%",
                  backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatDate("Assessed, month/year", method =  "toLocaleDateString", 
                 params = list(
                   'en-US', 
                   list(
                     month = 'numeric',
                     year = 'numeric' 
                     )
                 ))
    
  })
  
  
  
  ###############################################################
  ###############################################################
  
 # observe({
 #   updateSelectInput(session,"pest1_sel",choices=colnames(pestquestions))
 #   #updateSelectInput(session,"sizes",choices=colnames(df_p_samp()))
 # }) 
  
  
  ## Generate table with FinnPRIO assessments allowing comparison of two pests in "3. Compare pests by questions"-tab:----
  output$pest_1_2 <- DT::renderDataTable({
    
    req(input$Codes)
    # Filter the questions' groups based on the info in one column using dropdown menu -> Entry, Establishment and spread,Ipact, Management:
    pestquestion_codes <- reactive({  
      pestquestions %>%
        filter(Codes %in% input$Codes)
    }) 
    
    
    req(input$pest1_sel,input$pest2_sel) 
    dat <- datatable(
      
      select(pestquestion_codes(),
             "Question",
             "Answer for",
             input$pest1_sel,
             input$pest2_sel),
      #class = 'cell-border stripe',
      extensions = "FixedHeader",
      options = list(initComplete = JS(
        
        ###Adds color to the table header:
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#343841', 'color': '#fff'});",
        "}"),
        
        columnDefs = list(list(className = 'dt-right', targets = 1), list(targets = 
                                                                            "_all", width = "70px")),
        
        ## Remove the option to order the columns -> in our case is useful because the content of the table is text or there are empty rows:
        ordering=FALSE, 
        ## All results appear on same page:
        paging=FALSE, 
        ## Removes the search box above the table & the text with number of rows under the table:
        dom = "t"       
      ),
      rownames = FALSE) %>%
      
      
      ### Formating specific cells/words in the table 
      # (https://rstudio.github.io/DT/), 
      # (https://rstudio.github.io/DT/functions.html), (https://rstudio.github.io/DT/010-style.html):
      formatStyle(columns = 1:2, 
                  
                  fontWeight = styleEqual(c("most likely", ""),
                                          c('bold', '')),
                  
                  fontStyle = styleEqual(c("min", "max"),
                                         c('italic', 'italic'))
                  
      ) %>%
      
      formatStyle(input$Codes == "ENT",
                  "Question",
                  target = 'row',
                  backgroundColor = styleEqual(c("Pathway 1","Pathway 2", "Pathway 3", "Pathway 4", "Pathway 5"), 
                                               c("#CEB888", "#CEB888", "#CEB888", "#CEB888", "#CEB888")),
                  fontWeight = styleEqual(c("Pathway 1", "Pathway 2", "Pathway 3", "Pathway 4", "Pathway 5"),
                                          c('bold', 'bold', 'bold', 'bold', 'bold')),
                  fontSize = styleEqual(c("Pathway 1", "Pathway 2", "Pathway 3", "Pathway 4", "Pathway 5"), c(14, 14, 14, 14, 14)))
    
    return(dat)
  })
  
  
  ###############################################################
  
  ## Show explanations for the pathways in Entry questions in "3. Compare pests by questions"-tab:----
  output$helpPW <- renderUI({
    req(input$Codes)
    
    if (input$Codes == "ENT") {
      
      wellPanel(
        tags$h4(strong("Help for ENT questions"), style="color:#7C6A56"),
        
        help_pw(),
        tags$p ("ENT2A & ENT2B"),
        
        help_pw_a_h(),
        tags$p ("Pathways A-H")
        
      )
    }
  })
  
  
  ###############################################################
  ###############################################################
  
  ## Generate a table with ranks and HV in "4. Ranking pests"-tab:-----
  output$table_hv <- DT::renderDataTable({
    
    #The format of the following columns is converted from character to factor, so the selectize inputs (list of filter options) to be available:
    hv$tuhoojastatus_eu_2016_2031_mukaan <- as.factor(hv$tuhoojastatus_eu_2016_2031_mukaan)
    
    DT::datatable(hv, colnames = c("Pest" = "tuhooja",
                                   "Quarantine status" = "tuhoojastatus_eu_2016_2031_mukaan",
                                   "Entry, rank" = "rank_entry",
                                   "Entry, hypervolume" = "hv_entry",
                                   "Establishment and spread, rank" = "rank_establishment",
                                   "Establishment and spread, hypervolume" = "hv_establishment",
                                   "Invasion, rank" = "rank_invasion",
                                   "Invasion, hypervolume" = "hv_invasion",
                                   "Impact, rank" = "rank_impact",
                                   "Impact, hypervolume" = "hv_impact"
    ),
    container = tbl_hdr_map, # -> Transforms the table header into 2 rows. To make changes go to 'tbl_hdr_map' in functions.R
    rownames = TRUE,
    extensions = "Buttons",
    options = list(#pageLength = 15,
      ## Hide the first column that contains rownames
      columnDefs = list(list(targets = c(0), visible = FALSE), list(targets = c(0),className = 'dt-head-center')),
      
      paging=FALSE,
      autoWidth = FALSE, 
      # Display and customize the buttons:
      dom = "Bfrtip", 
      buttons = list(list(extend = "excel", text = '<span class="glyphicon glyphicon-th"></span> Excel <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible")), 
                     list(extend = "csv", text = '<span class="glyphicon glyphicon-download-alt"></span> CSV <sup>1</sup>', title = NULL, 
                          exportOptions = list(columns = ":visible")) #, I('colvis')
      )
    ),
    #class = "cell-border stripe",
    filter = "top")  %>%
      formatStyle("Pest",  fontWeight = "bold", fontStyle = "normal") %>%
      formatRound(c("Entry, hypervolume", "Establishment and spread, hypervolume", "Invasion, hypervolume", "Impact, hypervolume"), 2)
    
  })
  
  
  ###############################################################
  
  ## Generate a treemap for ranks and hypervolume scores:----
  output$hv_tree <- renderPlot ({
    
    Sys.sleep(1.5)
    t <- ggplot(hv, aes(area = hv_impact, fill = rank_impact, 
                        subgroup = rank_impact, 
                        label = paste(tuhooja))) + 
      geom_treemap() +
      geom_treemap_subgroup_border(color = "#DB4258")+
      geom_treemap_subgroup_text(place = "centre", grow = TRUE, alpha = 0.5, color = "#CEB888", min.size = 0)+
      
      geom_treemap_text(colour = "white", place = "middle", reflow = TRUE, fontface = "italic") 
    # theme(legend.position = "none")
    
    t <- t +
      
      # Switch between plots:
      
      {switch(input$switch_tree,
              "1" = aes(area = hv_entry, fill = rank_entry, 
                        subgroup = rank_entry, 
                        label = paste(tuhooja)),
              "2" = aes(area = hv_establishment, fill = rank_establishment, 
                        subgroup = rank_establishment, 
                        label = paste(tuhooja)),
              "3" = aes(area = hv_invasion, fill = rank_invasion, 
                        subgroup = rank_invasion, 
                        label = paste(tuhooja))
      )
      }
    
    t
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
