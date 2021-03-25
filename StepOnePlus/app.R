#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required libraries
library(shiny)
library(tidyverse)
library(readxl)
library(DT)

# Functions stored outside of ui / server .. to make code easier to read.


get_results_tb <- function(xls_file) {
    
    results_tb <- read_excel(xls_file, 
                             sheet = "Results", skip = 6)
    
    # Will also need the Sample Setup tab, because that is where the Biogroup Name data are stored.
    sample_setup_tb <- read_excel(xls_file,
                                  sheet = "Sample Setup", skip = 6) %>%
        filter(str_detect(Well, "^[ABCDEFGH]\\d{1,2}$")) %>%
        filter(`Sample Name` != 'NA') %>%
        select(Well, `Biogroup Name`)
    
    # join to get the Biogroup Name data into the results tibble
    results_tb <- left_join(results_tb, sample_setup_tb, by = "Well")
    
    # StepOnePlus Excel files use an unusual character instead of just lower-case 't' in 'Ct'. Fix.
    colnames(results_tb) <- str_replace(colnames(results_tb), intToUtf8(1090), "t")
    
    # Filter for real data lines then select only the columns that might be needed.
    results_tb <- filter(results_tb, str_detect(Well, "^[ABCDEFGH]\\d{1,2}$")) %>%
        select(Well, `Sample Name`, `Target Name`, `Biogroup Name`, Task,
               Ct, `Ct Threshold`)
    
    # Enforce column Ct as a numeric (it could contain "Undetermined", which will be coerced to NA)
    results_tb$Ct <- as.numeric(results_tb$Ct)
    
    # Switch 'NTC' to 'Negative Control' in Task column
    results_tb$Task <- str_replace(results_tb$Task, 'NTC', 'Negative Control')
    
    # Round Ct and `Ct Threshold` to two decimal places
    results_tb <- results_tb %>%
        mutate(Ct = round(Ct, 2)) %>%
        mutate(`Ct Threshold` = round(`Ct Threshold`, 2))
    
    # Add a logical column called Undetected, initialized based on Task and Ct
    # or how about not
    #results_tb <- mutate(results_tb, Undetected = if_else(Task == 'UNKNOWN' & !is.na(Ct), FALSE, TRUE))
    
    return(results_tb)
}

get_amp_tb <- function(xls_file, results_tb) {
    amp_tb <- read_excel(xls_file, 
                         sheet = "Amplification Data", skip = 6)
    
    # Replace the special character capital Greek delta with the string 'delta'
    colnames(amp_tb) <- str_replace(colnames(amp_tb), intToUtf8(916), "delta")
    
    # Get rid of empty rows
    amp_tb <- filter(amp_tb, `Target Name` != 'NA')
    
    # Add sample names
    amp_tb <- select(amp_tb, Well, Cycle, Rn, deltaRn) %>%
        left_join(results_tb, by = "Well")
    
    return(amp_tb)
}

get_melt_tb <- function(xls_file, results_tb) {
    # Get Melt Region Temperature Data
    melt_temp <- read_excel(xls_file,
                            sheet = "Melt Region Temperature Data", skip = 6)
    
    # Rename column for conformity with results (what where they thinking!?)
    melt_temp$Well = melt_temp$`Well Location`
    melt_temp$`Well Location` <- NULL
    
    # Make it tidy
    melt_temp <- melt_temp %>%
        pivot_longer(cols = starts_with("Reading"),
                     names_to = "Reading",
                     values_to = "Temperature")
    
    # Get Melt Region Derivative Data
    melt_deriv <- read_excel(xls_file,
                             sheet = "Melt Region Derivative Data", skip = 6)
    
    # Rename column for conformity with results (what where they thinking!?)
    melt_deriv$Well = melt_deriv$`Well Location`
    melt_deriv$`Well Location` <- NULL
    
    # Make it tidy
    melt_deriv <- melt_deriv %>%
        pivot_longer(cols = starts_with("Reading"),
                     names_to = "Reading",
                     values_to = "Derivative")
    
    
    # Join temperature and derivative..
    ## then select only desired columns..
    ## join to metadata from results_tb..
    ## then finally select only the desired columns.
    melt_tb <- left_join(melt_temp, melt_deriv, 
                         by = c("Well", "Target", "Reporter Dye", "Reading")) %>%
        select(Well, Target, Temperature, Derivative) %>%
        left_join(results_tb, by = "Well") %>%
        select(Well, `Sample Name`, `Target Name`, `Biogroup Name`, Task, Temperature, Derivative)
    
    return(melt_tb)
}

add_tech_reps <- function(in_tibble, varSamples) {
    new_tibble <- tibble()
    all_letters <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n",
                     "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
    for (varSample in varSamples) {
        x <- filter(in_tibble, `Sample Name` == varSample)
        rnums <- as.numeric(rownames(x))
        lets <- all_letters[rnums]
        
        # testing
        #print(rnums)
        #print(lets)
        
        x <- mutate(x, `Technical Replicate` = lets)
        new_tibble <- bind_rows(new_tibble, x)
        
        # testing
        # print("working on varSample")
        # print(varSample)
        # print("x was found to be")
        # print(x)
        
    }
    return(new_tibble)
}



# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("Step One Plus qRT-PCR data analysis"),

    # Sidebar contains excel file input and lots of conditional inputs
    sidebarLayout(
        sidebarPanel(
            p(a(href = "README.html", "Help")),
            p(a(href = "data/MJA_March15_2021_data.xls", "Download example Excel file")),
            hr(),
            fileInput("excel_file", "Load StepOnePlus Excel File",
                      accept = ".xls"),
            
            
            radioButtons("plot_type", "Choose plot type",
                         choices = c("deltaRN by Cycle",
                                     "Ct values",
                                     "Melt Curve",
                                     "Analysis")),
            uiOutput("undetect_ui"),
            
            h4(textOutput("adj_aes_text")),
            uiOutput("adjust_Ct_plots_ui"),
            uiOutput("adjust_line_plots_ui"),
            
            h4(textOutput("adj_analysis_text")),
            uiOutput("analysis_ui"),
            
            h4(textOutput("download_text")),
            uiOutput("download_ui"),
            uiOutput("download_code_ui")
        ),

        mainPanel(
            plotOutput("plot"),
            conditionalPanel("input.plot_type != 'Analysis'",
                             div(DTOutput("well_select"),
                                 style = "font-size: 75%")),
            conditionalPanel("input.plot_type == 'Analysis'", 
                            div(tableOutput("analysis_table"),
                                 style = "font-size: 75%"))
        )
    )
)

# Define server logic 
server <- function(input, output) {
    
    results_tb <- reactive({
        req(input$excel_file)
        ext <- tools::file_ext(input$excel_file$datapath)
        validate(need(ext == "xls", "Please upload a xls file"))
        get_results_tb(input$excel_file$datapath)
    })
    
    amp_tb <- reactive ({
        get_amp_tb(input$excel_file$datapath, results_tb())
    })
    
    melt_tb <- reactive({
        get_melt_tb(input$excel_file$datapath, results_tb())
    })
    
    f_results_tb <- reactive({
        x <- results_tb()
        
        # Make logical column 'Undetected' and initialize it 
        #  based only on Task and a real Ct
        x <- mutate(x, Undetected = if_else(Task == 'UNKNOWN' & !is.na(Ct), FALSE, TRUE))
        
        # Update 'Undetected' based on input$undetect
        x[x$Well %in% input$undetect, "Undetected"] <- TRUE
        
        # Filter based on rows selected from datatable
        x <- x[input$well_select_rows_selected, ]
        
        # testing
        # print(x)
        
        # return it
        x
    })
    
    f_amp_tb <- reactive ({
        # determine currently selected wells
        ok_Wells <- f_results_tb()$Well
       
        # filter amplification data to retain only currently selected wells
        x <- amp_tb() %>%
            filter(Well %in% ok_Wells)
        
        # join the Undetected column
        y <- select(f_results_tb(), Well, Undetected)
        x <- left_join(x, y, by = "Well")
        
        # testing
        #print(x)
        
        # return it
        x
    })
    
    f_melt_tb <- reactive ({
        ok_Wells <- f_results_tb()$Well
        x <- melt_tb() %>%
            filter(Well %in% ok_Wells)
        
        y <- select(f_results_tb(), Well, Undetected)
        x <- left_join(x, y, by = "Well")
        
        # testing
        # print(x)
        
        # return it
        x
        
    })
    
    und_wells <- reactive({
        ok_lines <- results_tb() %>%
            filter(Task != 'Negative Control') %>%
            filter(!is.na(Ct))
        as.vector(ok_lines$Well)
    })
    
    get_analysis_table <- reactive({
        req(input$refTarget)
        req(input$varTarget)
        req(input$refBiogroup)
        req(input$E_refTarget)
        req(input$E_varTarget)
        
        Ct <- results_tb() %>%
            filter(Task != 'Negative Control')
        
        # Deal with any wells the user has manually marked as undetected
        # Make logical column 'Undetected' and initialize it 
        #  based only on Task and a real Ct
        Ct <- mutate(Ct, Undetected = if_else(Task == 'UNKNOWN' & !is.na(Ct), FALSE, TRUE))
    
        # Update 'Undetected' based on input$undetect
        Ct[Ct$Well %in% input$undetect, "Undetected"] <- TRUE

        # Filter for just the two targets of interest, and rename certain columns.
        Ct_refTarget <- Ct %>%
            filter(`Target Name` == input$refTarget) %>%
            mutate(refTargetWell = Well, refTargetCt = Ct, refTargetName = `Target Name`,
                   refUndetected = Undetected, Undetected = NULL,
                   Well = NULL, Ct = NULL, `Target Name` = NULL,
                   Task = NULL, `Ct Threshold` = NULL)
        Ct_varTarget <- Ct %>%
            filter(`Target Name` == input$varTarget) %>%
            mutate(varTargetWell = Well, varTargetCt = Ct, varTargetName = `Target Name`,
                   varUndetected = Undetected, Undetected = NULL,
                   Well = NULL, Ct = NULL, `Target Name` = NULL,
                   Task = NULL, `Ct Threshold` = NULL)
        
        # Add technical replicate integers for each sample
        varSamples <- unique(Ct_varTarget$`Sample Name`)
        
        Ct_refTarget <- add_tech_reps(Ct_refTarget, varSamples)
        Ct_varTarget <- add_tech_reps(Ct_varTarget, varSamples)
        
        # Now a left join can be performed.
        # This will have the effect of leaving behind any mis-matched replicates
        Ct_analysis <- left_join(Ct_varTarget, Ct_refTarget)
        
        # Note the columns for the refBiogroup
        Ct_analysis <- mutate(Ct_analysis, 
                              isRefBiogroup = Ct_analysis$`Biogroup Name` == input$refBiogroup)
        
        # Note the efficiencies
        
        Ct_analysis <- Ct_analysis %>%
            mutate(ERefTarget = input$E_refTarget,
                   EVarTarget = input$E_varTarget)
        
        # Calculate the comparator value for each, which is the median Ct in the refBiogroup
        Ct_refTargetBiogroup <- Ct_analysis %>%
            filter(`Biogroup Name` == input$refBiogroup)
        Ct_refTargetComparator <- median(Ct_refTargetBiogroup$refTargetCt)  
        
        Ct_varTargetBiogroup <- Ct_analysis %>%
            filter(`Biogroup Name` == input$refBiogroup)
        Ct_varTargetComparator <- median(Ct_varTargetBiogroup$varTargetCt)  
        
        # Calculate relative expression values
        Ct_analysis <- mutate(Ct_analysis, `Relative Expression` = (
            (EVarTarget ^ (Ct_varTargetComparator - varTargetCt))
            / (ERefTarget ^ (Ct_refTargetComparator - refTargetCt))
        ))
        
        # varUndetected == TRUE & refUndetected == FALSE should become a 0 relative expression. This is a 
        #  non-detect event for the variable target, but the reference is detected.
        Ct_analysis <- mutate(Ct_analysis, `Relative Expression` = 
                   if_else(varUndetected == TRUE & refUndetected == FALSE, 0, `Relative Expression`))
        
        # refUndetected == TRUE must become an NA. If the reference target is undetected, or marked as undetected by
        #  the user, you can't report a relative expression value.
        Ct_analysis <- mutate(Ct_analysis, `Relative Expression` = 
                                  ifelse(refUndetected == TRUE, NA, `Relative Expression`))
        
        # log2 conversion, with pseudo-values for rel. expression 0 cases.
        Ct_analysis <- mutate(Ct_analysis, `log2 Pseudoplotted` = 
                                  ifelse(`Relative Expression` == 0, TRUE, FALSE))
        
        # minimum real log2 transformed value
        
        min_real_log2 <- log2(min(Ct_analysis$`Relative Expression`[Ct_analysis$`Relative Expression` > 0],
                                  na.rm = TRUE))
        
        pseudo_log2 <- min_real_log2 - 4 ## about 6%
        
        Ct_analysis <- mutate(Ct_analysis, `log2 Relative Expression` = 
                                  ifelse(`Relative Expression` == 0, pseudo_log2, 
                                         log2(`Relative Expression`)))

        Ct_analysis
        
    })
    
    output$well_select <- renderDT (
        {
            req(input$excel_file)
            req(input$plot_type != "Analysis")
            results_tb()
        },
        #extensions = c('Select', 'Buttons'),
        options = list(
            paging = FALSE
            #select = list(style = 'os', items = 'row'),
            #dom = 'Blfrtip',
            #rowId = 0,
            #buttons = c('selectAll', 'selectNone', 'selectRows')
        ),
        caption = h4("Select Wells to Plot"),
        selection = list(mode = 'multiple', selected = c(1:nrow(results_tb()))),
        rownames = FALSE
        #server = FALSE
    )
    
    output$plot <- renderPlot({
        req(input$excel_file)
        req(input$well_select_rows_selected)
        if(input$plot_type == "Ct values") {
            ggplot(f_results_tb()) +
                geom_jitter(aes(x = `Biogroup Name`, y = Ct, 
                                color = !!as.name(input$sel_Ct_col), 
                                shape = !!as.name(input$sel_Ct_shape)),
                            height = 0, width = 0.15, size = 4) +
                ggtitle("Ct values")
        } else if (input$plot_type == "deltaRN by Cycle") {
            ggplot(f_amp_tb()) +
                geom_line(aes(x = Cycle, y = log10(deltaRn), 
                              group = Well, color = !!as.name(input$sel_col),
                              linetype = !!as.name(input$sel_lt))) +
                geom_point(aes(x = Ct, y = log10(`Ct Threshold`))) +
                ggtitle("deltaRn by Cycle")
        } else if (input$plot_type == "Melt Curve") {
            ggplot(f_melt_tb()) +
                geom_line(aes(x = Temperature, y = Derivative,
                              group = Well, color = !!as.name(input$sel_col),
                              linetype = !!as.name(input$sel_lt))) +
                ggtitle("Melt Derivative by Temperature")
        } else if (input$plot_type == "Analysis") {
            req(input$an_plot_type)
            if(input$an_plot_type == "raw") {
                ggplot(get_analysis_table()) +
                    geom_jitter(aes(x = `Biogroup Name`, y = `Relative Expression`,
                                    shape = `Technical Replicate`),
                                height = 0, width = 0.15, size = 4) +
                    ggtitle(paste0("Accumulation of ", input$varTarget, 
                                   " (Ref. Target: ", input$refTarget,
                                   "; Ref. Biogroup: ", input$refBiogroup, ")"))
            } else if (input$an_plot_type == "log") {
                ggplot(get_analysis_table()) +
                    geom_jitter(aes(x = `Biogroup Name`, y = `log2 Relative Expression`,
                                    shape = `Technical Replicate`,
                                    color = `log2 Pseudoplotted`),
                                height = 0, width = 0.15, size = 4) +
                    ggtitle(paste0("Accumulation of ", input$varTarget, 
                                   " (Ref. Target: ", input$refTarget,
                                   "; Ref. Biogroup: ", input$refBiogroup, ")"))
            }
        }
    })
    
    output$download_tidy <- downloadHandler(
        filename = function() {
            if(input$plot_type == "Ct values") {
                "Ct_tidy.csv"
            } else if (input$plot_type == "deltaRN by Cycle") {
                "deltaRn_tidy.csv"
            } else if (input$plot_type == "Melt Curve") {
                "MeltCurve_tidy.csv"
            } else if (input$plot_type == "Analysis") {
                "Analysis_tidy.csv"
            }
        },
        content = function(file) {
            if (input$plot_type == "Ct values") {
                write_csv(f_results_tb(), file)
            } else if (input$plot_type == "deltaRN by Cycle") {
                write_csv(f_amp_tb(), file)
            } else if (input$plot_type == "Melt Curve") {
                write_csv(f_melt_tb(), file)
            } else if (input$plot_type == "Analysis") {
                write_csv(get_analysis_table(), file)
            }
        }
    )
    
    output$download_ui <- renderUI({
        req(input$excel_file)
        downloadButton("download_tidy", label = "Download data (.csv)")
    })
    
    output$adjust_line_plots_ui <- renderUI({
        req(input$excel_file)
        if(input$plot_type == "deltaRN by Cycle" | 
           input$plot_type == "Melt Curve") {
            tagList(
                radioButtons("sel_lt", "Linetype", choices = c(
                    "Task" = "Task",
                    "Target Name" = "Target Name",
                    "Biogroup Name" = "Biogroup Name",
                    "Undetected" = "Undetected"
                ), selected = "Target Name"),
                radioButtons("sel_col", "Color", choices = c(
                    "Task" = "Task",
                    "Target Name" = "Target Name",
                    "Biogroup Name" = "Biogroup Name",
                    "Undetected" = "Undetected"
                ), selected = "Biogroup Name")
            )
        }
        
    })
    
    output$adjust_Ct_plots_ui <- renderUI({
        req(input$excel_file)
        if(input$plot_type == "Ct values") {
            tagList(
                radioButtons("sel_Ct_shape", "Shape", choices = c(
                    "Task" = "Task",
                    "Target Name" = "Target Name",
                    "Undetected" = "Undetected"
                ), selected = "Target Name"),
                radioButtons("sel_Ct_col", "Color", choices = c(
                    "Task" = "Task",
                    "Target Name" = "Target Name",
                    "Undetected" = "Undetected"
                ), selected = "Task")
            )
        }
        
    })
    
    output$download_code <- downloadHandler(
        filename = function() {
            if(input$plot_type == "Ct values") {
                "Ct_plot.R"
            } else if (input$plot_type == "deltaRN by Cycle") {
                "deltaRn_plot.R"
            } else if (input$plot_type == "Melt Curve") {
                "MeltCurve_plot.R"
            } else if (input$plot_type == "Analysis") {
                "Analysis_plot.R"
            }
        },
        content = function(file) {
            if (input$plot_type == "Ct values") {
                write_lines(c(
                    "if(!require(tidyverse)){",
                    "  install.packages(\"tidyverse\")",
                    "}",
                    "library(tidyverse)",
                    "Ct_data <- read_csv(\"Ct_tidy.csv\")",
                    "ggplot(Ct_data) +",
                    "       geom_jitter(aes(x = `Biogroup Name`, y = Ct,",
                    paste0("                  color = \`", 
                           input$sel_Ct_col, "\`,"),
                    paste0("                  shape = \`",
                           input$sel_Ct_shape, "\`),"),
                    "                   height = 0, width = 0.15, size = 4) +",
                    "         ggtitle(\"Ct values\")"
                ), file)
            } else if (input$plot_type == "deltaRN by Cycle") {
                write_lines(c(
                    "if(!require(tidyverse)){",
                    "  install.packages(\"tidyverse\")",
                    "}",
                    "library(tidyverse)",
                    "deltaRn_data <- read_csv(\"deltaRn_tidy.csv\")",
                    "ggplot(deltaRn_data) +",
                    "  geom_line(aes(x = Cycle, y = log10(deltaRn),",
                    paste0("                group = Well, color = \`",
                           input$sel_col, "\`,"),
                    paste0("                linetype = \`",
                           input$sel_lt, "\`)) +"),
                    "   geom_point(aes(x = Ct, y = log10(`Ct Threshold`))) +",
                    "  ggtitle(\"Melt Derivative by Temperature\")"
                ), file)
            } else if (input$plot_type == "Melt Curve") {
                write_lines(c(
                    "if(!require(tidyverse)){",
                    "  install.packages(\"tidyverse\")",
                    "}",
                    "library(tidyverse)",
                    "MeltCurve_data <- read_csv(\"MeltCurve_tidy.csv\")",
                    "ggplot(MeltCurve_data) +",
                    "  geom_line(aes(x = Temperature, y = Derivative,",
                    paste0("                group = Well, color = \`", 
                           input$sel_col, "\`,"),
                    paste0("                linetype = \`",
                           input$sel_lt, "\`)) +"),
                    "  ggtitle(\"Melt Derivative by Temperature\")"
                ), file)
            } else if (input$plot_type == "Analysis" & input$an_plot_type == "raw") {
                write_lines(c(
                    "if(!require(tidyverse)){",
                    "  install.packages(\"tidyverse\")",
                    "}",
                    "library(tidyverse)",
                    "Analysis_data <- read_csv(\"Analysis_tidy.csv\")",
                    "ggplot(Analysis_data) +",
                    "  geom_jitter(aes(x = `Biogroup Name`, y = `Relative Expression`,",
                    "                  shape = `Technical Replicate`),",
                    "              height = 0, width = 0.15, size = 4) +",
                    paste0("  ggtitle(paste0(\"Accumulation of \", \"", input$varTarget, 
                           "\", \" (Ref. target: \", \"", input$refTarget, "\", \"; Ref. Biogroup: \", \"", 
                           input$refBiogroup, ")\" ))"
                    )
                    
                ), file)
            } else if (input$plot_type == "Analysis" & input$an_plot_type == "log") {
                write_lines(c(
                    "if(!require(tidyverse)){",
                    "  install.packages(\"tidyverse\")",
                    "}",
                    "library(tidyverse)",
                    "Analysis_data <- read_csv(\"Analysis_tidy.csv\")",
                    "ggplot(Analysis_data) +",
                    "  geom_jitter(aes(x = `Biogroup Name`, y = `log2 Relative Expression`,",
                    "                  shape = `Technical Replicate`, color = `log2 Pseudoplotted`),",
                    "              height = 0, width = 0.15, size = 4) +",
                    paste0("  ggtitle(paste0(\"Accumulation of \", \"", input$varTarget, 
                           "\", \" (Ref. target: \", \"", input$refTarget, "\", \"; Ref. Biogroup: \", \"", 
                           input$refBiogroup, ")\" ))"
                    )
                    
                ), file)
            }
        }
    )
    
    output$download_code_ui <- renderUI({
        req(input$excel_file)
        downloadButton("download_code", label = "Download code (.R)")
    })
    
    output$analysis_ui <- renderUI({
        req(input$excel_file)
        if(input$plot_type == "Analysis") {
            tagList(
                selectInput("refTarget", "Reference Target",
                             choices = unique(results_tb()$`Target Name`)),
                selectInput("varTarget", "Variable Target",
                             choices = unique(results_tb()$`Target Name`)),
                selectInput("refBiogroup", "Reference Biogroup",
                            choices = unique(results_tb()$`Biogroup Name`)),
                numericInput("E_refTarget", "Efficiency - Reference Target",
                             value = 2),
                numericInput("E_varTarget", "Efficiency - Variable Target",
                             value = 2),
                radioButtons("an_plot_type", "Select analysis plot type",
                             choices = list("Relative Expression" = "raw",
                                         "log2 Relative Expression" = "log"))
            )
        }
    })
    
    output$undetect_ui <- renderUI({
        req(input$excel_file)
        undetect_choices <- und_wells()
        tagList(
            selectInput('undetect', 'Choose wells to mark as undetected for analysis',
                        undetect_choices, multiple = TRUE, selectize = TRUE)
        )
    })
    
    output$adj_aes_text <- renderText({
        req(input$excel_file)
        if(input$plot_type == "deltaRN by Cycle" | input$plot_type == "Melt Curve" |
           input$plot_type == "Ct values") {
            "Adjust aesthetic mappings"
        }
    })
    
    output$adj_analysis_text <- renderText({
        req(input$excel_file)
        if(input$plot_type == "Analysis") {
            "Adjust analysis settings"
        }
    })
    
    output$download_text <- renderText({
        req(input$excel_file)
        "Download code and data to reproduce current plot"
    })
    
    output$analysis_table <- renderTable({
        req(input$excel_file)
        if(input$plot_type == "Analysis") {
            get_analysis_table()
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
