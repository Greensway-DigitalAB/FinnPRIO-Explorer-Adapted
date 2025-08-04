
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