#Setup----
library(shiny)
library(shinyalert)
library(plotly)
library(tidyverse)
library(miceadds)
library(shinysky)
options(shiny.maxRequestSize = 40*1024^2)
tissues_file <<- read_csv("data/tissues_file")
#tissues_by_species <- data.frame(read.csv("tissues_by_species.csv"))
species_list <- c("h_sapiens",
                  "c_elegans",
                  "d_melanogaster",
                  "m_musculus",
                  "d_rerio",
                  "r_norvegicus")
id_type_list <- c("Ensembl_id",
                  "Entrez_id",
                  "Symbol")
tool_list <- c("annotationDBI",
               "biomaRt")
homolog_database_list <- c("HomoloGene",
                           "NCBIOrthoAnnotationPipe")

#Start UI----
ui <- fluidPage(
  
  sidebarPanel(
    #Gene Input----
    wellPanel(HTML("<h4>Gene Input</h4>"),
              actionButton("conversion_instructions", "Instructions"),
              textAreaInput("gene_ids","Paste IDs"),
              fileInput("gene_file", "Upload CSV File",
                        multiple = FALSE,
                        accept = ".csv"),
              selectInput("gene_input_species", "Input Species",
                          choices=species_list),
              selectInput("gene_input_id_type", "Input ID type",
                          choices=id_type_list)
    ),
    #ID Conversion----
    wellPanel(HTML("<h4>ID Conversion</h4>"),
              checkboxGroupInput("conversion_output_species", "Output Species",
                                 choices=species_list),
              checkboxGroupInput("conversion_output_id_types", "Output ID types",
                                 choices=id_type_list),
              selectInput("conversion_tool", "Tool",
                          choices=tool_list),
              selectInput("conversion_ortholog_database","Otholog Database",
                          choices=homolog_database_list),
              actionButton("conversion_go","Convert IDs")
    ),
    #Plot Expression----
    wellPanel(HTML("<h4>Plot Expression</h4>"),
              actionButton("plot_instructions","Instructions"),
              select2Input("plot_gene", "gene",choices=NULL),
              checkboxGroupInput("plot_species", "species",choices=species_list),
              checkboxGroupInput("plot_tissue","tissues",choices="Select Species First"),
              checkboxInput("plot_by", label=HTML("<b>Plot by Tissue</b>")),
              actionButton("plot_go", "Plot")
    ),#----
    wellPanel(HTML("<h4>Plot DS</h4>"),
              checkboxGroupInput("ds_tissue","tissues",choices="run conversion first"),
              selectInput("ds_metric", "metric", choices=c("DS_Gene","DS_Tissue")),
              actionButton("ds_go", "Plot")
              
    ),
    wellPanel(HTML("<h4>Plot CV</h4>"),
              checkboxGroupInput("cv_tissue", "tissues", choices="run conversion first"),
              selectInput("cv_metric", "metric", choices= c("CV_Tissue", "CV_Species")),
              actionButton("cv_go", "Plot")
    ),
    #Main Panel----
    mainPanel(
      dataTableOutput("conversion_table"),
      uiOutput("plots"),
      plotOutput("metric_plot")
    )#----
  )
)
server <- function(input,output,session){
  source.all("cosia_scripts", grepstring = ".r", print.source = FALSE)
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
  observeEvent(input$conversion_instructions , {
    shinyalert::shinyalert(  title="Conversion Instructions",
                             text='
    <p style="text-align:left">
    1) Paste IDs in this format:<br>
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
      Or upload a csv file with 1 column.<br>
      <br>
      2) Select the species and ID type of your genes.<br>
      <br>
      3) Select any number of species and ID types to convert to. You must select as least 1 for each.<br>
      <br>
      4) Select between annotationDBI and biomaRt. We recommend annotationDBI because it is faster. <br>
      <br>
      5) Select ortholog database. If input species is the same as output species, this step does not matter.
                      </p>',
                             html=TRUE)
  })
  observeEvent(input$plot_instructions ,{
    shinyalert::shinyalert(  title="Conversion Instructions",
                             text='
    <p style="text-align:left">
    This section of CoSIA plots the expression of genes chosen in the previous step. 
    Conversion must be done before plotting expression. 
    
                      </p>',
                             html=TRUE)
  })
  observeEvent(input$gene_file,{
    x <- input$gene_file
    x <- read.csv(x$datapath)
    x <- x[,1]
    x <- paste0(x,"\n")
    x[length(x)] <- substr(x[length(x)],1,nchar(x[length(x)])-1)
    x=paste(x,collapse="")
    updateTextAreaInput("gene_ids", value=x, session=session)
    
  })
  
  observeEvent(input$conversion_go,{
    gene_ids <- unlist(strsplit(input$gene_ids,split = "\n"))
    gene_input_species <- input$gene_input_species
    gene_input_id_type <- input$gene_input_id_type
    conversion_output_species <- input$conversion_output_species
    conversion_output_id_types <- input$conversion_output_id_types
    conversion_tool <- input$conversion_tool
    conversion_ortholog_database <- input$conversion_ortholog_database
    
    valid_input <- TRUE
    if(is.null(gene_ids)|is.null(gene_input_species)|is.null(conversion_output_species)|is.null(gene_input_id_type)|is.null(conversion_output_id_types)){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "Fill out All Fields", type="error")
    }
    print(valid_input)
    if(valid_input){
      try({
        global_cosia@gene_set <<-  gene_ids
        global_cosia@i_species <<- gene_input_species
        global_cosia@o_species <<- conversion_output_species
        global_cosia@input_id <<- gene_input_id_type
        global_cosia@output_ids <<- conversion_output_id_types
        global_cosia@mapping_tool <<- conversion_tool
        global_cosia@ortholog_database <<-  conversion_ortholog_database
        
        global_cosia <<- getConversion(global_cosia)
        output$conversion_table <- renderDataTable({global_cosia@converted_id})
        for_input <- global_cosia@converted_id
        for_input <- data.frame(for_input[,grepl("ensembl_id",names(for_input))])
        if(dim(for_input)[2]!=1){
          for_input <- c(for_input,sep="/")
        }
        for_input <- do.call(paste,for_input)
        for_input <- c(for_input,"")
        updateSelect2Input(session=session,label="gene", inputId = "plot_gene", choices=(for_input))
        
        tib <- getTissues(conversion_output_species)
        vec <- pull(tib,Common_Anatomical_Entity_Name)
        updateCheckboxGroupInput("cv_tissue",session=session, choices = vec, inline=FALSE, label = paste("tissues for ", paste(conversion_output_species, collapse=", ")))
        updateCheckboxGroupInput("ds_tissue",session=session, choices = vec, inline=FALSE, label = paste("tissues for ", paste(conversion_output_species, collapse=", ")))
      })
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
          if(input$plot_by %% 2==0){
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
    else if((nrow(global_cosia@converted_id)==1) & (input$ds_metric== "ds_tissue")){
      valid_input <- FALSE
      shinyalert::shinyalert("Error", "DS Tissue requires more than 1 gene", type="error")
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
        output$metric_plot <- renderPlot({
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
          output$metric_plot <- renderPlot({
            plotCVGEx(global_cosia)
          })
          print("done")
        })
      })
    }
  })
  
}
shinyApp(ui=ui,server=server)

