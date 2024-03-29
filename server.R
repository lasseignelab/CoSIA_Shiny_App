library(dplyr)
library(magrittr)
library(plotly)
library(miceadds)
library(shiny)
library(shinyalert)
library(tidyverse)
library(org.Hs.eg.db)
library(org.Mm.eg.db)
library(org.Rn.eg.db)
library(org.Dm.eg.db)
library(org.Dr.eg.db)
library(org.Ce.eg.db)

server <- function(input,output,session){
  miceadds::source.all("cosia_scripts", grepstring = ".r", print.source = FALSE)
  global_cosia <- CoSIAn(
    gene_set = "",
    i_species="",
    o_species="",
    input_id="",
    output_ids="",
    mapping_tool="",
    ortholog_database = "",
    map_tissues = "",
    map_species = "",
    metric_type=""
  )
  # Popup on landing page----
  histdata <- rnorm(500)
  CoSIAlogo <- "CoSIA_logo.png"
  observeEvent(once = TRUE,ignoreNULL = FALSE, ignoreInit = FALSE, eventExpr = histdata, { 
    # event will be called when histdata changes, which only happens once, when it is initially calculated
    showModal(
      modalDialog(
        title = "Welcome to CoSIA",
        div(
          div(
            tags$figure(
              align = "center",
              tags$img( class="pop-logo",
                        src = CoSIAlogo
              )
            )
          ),
          div(
            p(
              style="font-size:15px",
              strong("CoSIA"), "is a package that provides researchers with an 
          alternative methodology for comparing across species and tissues 
          using normal wild-type RNA-Seq Gene Expression data from Bgee." 
            )
          )
        ),
        br(),
        p( 
          style="font-size:20px",
          strong("PLEASE READ THE BRIEF CLARIFICATIONS ON THE LANDING PAGE 
          BEFORE MOVING TO THE OTHER TABS!"
           )
        ),
        
        p(
          style="font-size:20px",
          "Quick Notes: To use CoSIA fully and smoothly, input gene symbols 
          need species-specific formatting. You must provide and convert 
         inputs on the first tab prior to using any of the plotting tabs.  
         Ensembl IDs are required as an output type for any of the plotting 
      tabs."
        )
      ))
  })
  # Instruction Popups----
  observeEvent(input$conversion_id_instructions , {
    shinyalert::shinyalert(  title="Conversion Instructions",
                             text='
    <p style="text-align:left">
      Paste IDs in this format:<br>
      ENSG00000172315<br>
      ENSG00000087460<br>
      ENSG00000049759<br>
      ENSG00000088256<br>
      ENSG00000102144<br>
      ENSG00000072501<br>
      ENSG00000148459<br>
      ENSG00000198931<br>
      ENSG00000081479<br>
      ENSG00000157483<br>
      ENSG00000113924<br>
      ENSG00000171863<br>
      ENSG00000025796<br>
      ENSG00000011201<br>
      <br>
      Or upload a csv file with 1 column.<br>
    </p>',
                             html=TRUE)
  })
  
  observeEvent(input$conversion_output_instructions , {
    shinyalert::shinyalert(  title="Conversion Instructions",
                             text='
    <p style="text-align:left">
    Select any number of species and ID types to convert to. You must select as least 1 for each.<br>
      <br>
    Genes must be converted to Ensembl to use any further CoSIA modules  
      </p>',
                             html=TRUE)
  })
  observeEvent(input$plot_instructions ,{
    shinyalert::shinyalert(  title="Plot Expression Instructions",
                             text='
    <p style="text-align:left">
    This section of CoSIA plots the expression of genes chosen in the previous step. 
    Conversion must be done before plotting expression. Genes must be converted to Ensembl format.
    They will autopopulate the text field once the conversion step has been run.
    
                      </p>',
                             html=TRUE)
  })

  # Actions----  
  observeEvent(input$gene_file,{
    x <- input$gene_file
    x <- read.csv(x$datapath)
    x <- x[,1]
    x <- paste0(x,"\n")
    x[length(x)] <- substr(x[length(x)],1,nchar(x[length(x)])-1)
    x=paste(x,collapse="")
    updateTextAreaInput("gene_ids", value=x, session=session)
    
  })
  
  valid_ids <- reactiveValues(checked = character(), failed = character(), 
                              errorM = character(), warningM = character(),
                              printM = character())
  
  observeEvent(input$check_inputs,{
    gene_inputs <- unlist(strsplit(input$gene_ids, split = "\n"))
    gene_input_species <- input$gene_input_species
    gene_input_id_type <- input$gene_input_id_type
    
    valid_input <- TRUE
    if(is.null(gene_inputs)|
       gene_input_species == "Select..."|
       gene_input_id_type == "Select..."){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Please fill out all fields", type="error")
    }
    print(valid_input)
    if(valid_input){
      tryCatch({
        print(gene_inputs)
        gene_inputs_tws <- map(
          gene_inputs, 
          ~ trimws(
            .x,
            which = c("both"),
            whitespace = "[ \t]"
          )
        )
        print(gene_inputs_tws)
        checked <- check_ids(unlist(gene_inputs_tws), 
                             gene_input_id_type, 
                             gene_input_species)
        valid_ids$checked <- checked |> dplyr::filter(found == TRUE) |> 
          dplyr::select(1)
        if(nrow(valid_ids$checked) != 0){
          shinyalert::shinyalert("Success", "Valid IDs are available for conversion on the next tab!", type="success")
        }
        
        valid_ids$failed <- checked |> dplyr::filter(found == FALSE) |> 
          dplyr::select(1)
        if(nrow(valid_ids$failed) != 0){
          message("\nSome IDs were not found in the species database (org.db).\nPlease check these IDs for mispellings, missing or transposed values, or the correct species.\nFor more information about possible sources of error, please see instructions on 'Inputs' page.")
        }
        
      }, error = function(e) {
        # Handle the error condition
        valid_ids$errorM <- as.character(e)
      }, warning = function(w) {
        # Handle the warning condition
        valid_ids$warningM <- as.character(w)
      }, message = function(m){
        valid_ids$printM <- as.character(m)
      })
    }
    
  })
  
 
  output$errors <- renderText({
    valid_ids$errorM
  })
  output$warnings <- renderText({
    valid_ids$warningM
  })
  
  output$printed <- renderText({
    valid_ids$printM
  })
  output$failed_input_check <- renderPrint({
    valid_ids$failed
  })
  
  observeEvent(input$reset_inputs,{
    valid_ids$checked <- character()
    valid_ids$failed <- character()
    valid_ids$warningM <- character()
    valid_ids$errorM <- character()
    valid_ids$printM <- character()
  })
  
  observeEvent(input$conversion_go,{
    id_vec <- valid_ids$checked$inputs
    gene_ids <- unlist(strsplit(id_vec,split = "\n"))
    gene_input_species <- input$gene_input_species
    gene_input_id_type <- input$gene_input_id_type
    conversion_output_species <- input$conversion_output_species
    conversion_output_id_types <- input$conversion_output_id_types
    conversion_tool <- input$conversion_tool
    conversion_ortholog_database <- input$conversion_ortholog_database
    
    valid_input <- TRUE
    if(is.null(conversion_output_species)|is.null(conversion_output_id_types)){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Fill out All Fields", type="error")
    }
    print(valid_input)
    if(valid_input){
      try({
        withProgress(message="Fetching IDs", value=0,{
          
          global_cosia@gene_set <<-  gene_ids
          global_cosia@i_species <<- gene_input_species
          global_cosia@o_species <<- conversion_output_species
          global_cosia@input_id <<- gene_input_id_type
          global_cosia@output_ids <<- conversion_output_id_types
          global_cosia@mapping_tool <<- conversion_tool
          global_cosia@ortholog_database <<-  conversion_ortholog_database
          
          global_cosia <<- getConversion(global_cosia)
          incProgress(2/2, detail="Rendering Table")
          conversion_df <- reactive(global_cosia@converted_id)
          output$conversion_table <- renderDataTable({global_cosia@converted_id})
          for_input <- global_cosia@converted_id
          for_input <- data.frame(for_input[,grepl("ensembl_id",names(for_input))])
          if(dim(for_input)[2]!=1){
            for_input <- c(for_input,sep="/")
          }
          for_input <- do.call(paste,for_input)
          for_input <- c(for_input,"")
          updateSelectizeInput(session=session,label="gene", inputId = "plot_gene", choices=(for_input))
          
          tib <- getTissues(conversion_output_species)
          vec <- pull(tib,Common_Anatomical_Entity_Name)
          updateCheckboxGroupInput("cv_tissue",session=session, choices = vec, inline=FALSE, label = paste("tissues for ", paste(conversion_output_species, collapse=", ")))
          updateCheckboxGroupInput("ds_tissue",session=session, choices = vec, inline=FALSE, label = paste("tissues for ", paste(conversion_output_species, collapse=", ")))
          reactive(con)
          output$conversion_download <- downloadHandler(
            filename = function() {"conversion_download.csv"},
          content = function(file) {
            write.csv(conversion_df(),file)
          })
          })
      })
      if(global_cosia@converted_id[1,1]==0){
        shinyalert::shinyalert("Error", "No converted IDs were returned. Make sure the input IDs match the chosen ID type", type="warning")
      }
      }
  })
  
  observeEvent(input$plot_species,{
    tib <- getTissues(input$plot_species)
    vec <- pull(tib,Common_Anatomical_Entity_Name)
    updateCheckboxGroupInput("plot_tissue",session=session, choices = vec, inline=FALSE, label = paste("tissues for ", paste(input$plot_species, collapse=", ")))
    
  })
  
  observeEvent(input$plot_go,{
    valid_input <- TRUE
    if(is.null(input$plot_tissue)|is.null(input$plot_species)|is.null(input$plot_gene)){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Fill out All Fields", type="error")
      
    }
    if(valid_input){
      try({
        withProgress(message="Plotting Expression", value=0,{
          global_cosia@map_species <<- input$plot_species
          global_cosia@map_tissues <<- input$plot_tissue
          incProgress(1/2, detail="Fetching Expression Data")
          global_cosia <<- getGEx(global_cosia)
          genes <- input$plot_gene
          #genes <- unlist(strsplit(gene,split = "/"))
          tissues <- input$plot_tissue
          species <- input$plot_species
          print(tissues)
          incProgress(2/2, detail="Plotting")
          if(input$plot_by =="Species"){
            gene_tissue <- NULL
            for(gene in genes){
              for(tissue in tissues){
                gene_tissue <- c(gene_tissue,paste(unlist(strsplit(gene,split = "/"))[1], tissue, sep="_"))
              }
            }
            output$plots <- renderUI({
              plot_output_list <- lapply(gene_tissue,function(i){
                plotname <- paste("plot",i,sep="_")
                plotlyOutput(plotname)
              })
              do.call(tagList, plot_output_list)
            })
            
            for(gene in genes){
              for(tissue in tissues){
                local({
                  my_gene <- gene
                  my_tissue <- tissue
                  plotname <- paste("plot",unlist(strsplit(my_gene,split = "/"))[1],my_tissue, sep="_")
                  output[[plotname]] <- renderPlotly({
                    plotSpeciesGEx(global_cosia, my_tissue,unlist(strsplit(my_gene,split = "/"))[1])
                  })
                })
              }
            }
            
          }else{
            gene_species <- NULL
            for(gene in genes){
              for(spec in species){
                gene_species <- c(gene_species,paste(unlist(strsplit(gene,split = "/"))[1], spec, sep="_"))
              }
            }
            output$plots <- renderUI({
              plot_output_list <- lapply(gene_species,function(i){
                plotname <- paste("plot",i,sep="_")
                plotlyOutput(plotname)
              })
              do.call(tagList, plot_output_list)
            })
            
            for(gene in genes){
              for(spec in species){
                local({
                  my_gene <- gene
                  my_spec <- spec
                  plotname <- paste("plot",unlist(strsplit(my_gene,split = "/"))[1],my_spec, sep="_")
                  output[[plotname]] <- renderPlotly({
                    plotTissueGEx(global_cosia, my_spec,unlist(strsplit(my_gene,split = "/"))[1])
                  })
                })
              }
            }
          }
        })
      })
    }
    
    print("done")
  })
  
  observeEvent(input$ds_go,{
    print("trying to run ds")
    valid_input <- TRUE
    if(global_cosia@converted_id[1,1]==0){
      print("no converted id")
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Run Conversion First", type="error")
    }
    else if(is.null(input$ds_tissue)){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Select Tissues First", type="error")
    }
    else if(!any(grepl("ensembl",colnames(global_cosia@converted_id)))){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Convert IDs to Ensembl", type="error")
    }
    else if((nrow(global_cosia@converted_id)==1) & (input$ds_metric== "DS_Tissue")){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "DS Tissue requires more than 1 gene", type="error")
    }
    else if((length(input$ds_tissue)==1) & (input$ds_metric== "DS_Gene")){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "DS Gene requires more than 1 tissue", type="error")
    }
    if(valid_input){try({
      print("running ds")
      withProgress(message="Plotting Metric", value=0,{
        global_cosia@metric_type<<-input$ds_metric
        global_cosia@map_tissues <<- input$ds_tissue
        global_cosia@map_species <<- global_cosia@o_species
        print("test")
        incProgress(1/2, detail="Fetching Metric Data")
        global_cosia <- getGExMetrics(global_cosia)
        print(head(global_cosia@metric))
        incProgress(2/2, detail="Plotting")
        output$ds_plot <- renderPlot({
          plotDSGEx(global_cosia)
          
        })
        print("done")
      })
    })
    }
  })
  
  observeEvent(input$cv_go,{
    valid_input <- TRUE
    if(global_cosia@converted_id[1,1]==0){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Run Conversion First", type="error")
    }
    else if(is.null(input$cv_tissue)){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Select Tissues First", type="error")
    }
    else if(!any(grepl("ensembl",colnames(global_cosia@converted_id)))){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Convert IDs to Ensembl", type="error")
    }
    if(valid_input){
      try({
        withProgress(message="Plotting Metric", value=0,{
          global_cosia@metric_type<<-input$cv_metric
          global_cosia@map_tissues <<- input$cv_tissue
          global_cosia@map_species <<- global_cosia@o_species
          print("test")
          incProgress(1/2, detail="Fetching Metric Data")
          global_cosia <- getGExMetrics(global_cosia)
          print(head(global_cosia@metric))
          incProgress(2/2, detail="Plotting")
          output$cv_plot <- renderPlot({
            plotCVGEx(global_cosia)
          })
          print("done")
        })
      })
    }
  })
  
}