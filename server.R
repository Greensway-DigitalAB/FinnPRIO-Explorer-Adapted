# SERVER ####
function(input, output, session) {
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