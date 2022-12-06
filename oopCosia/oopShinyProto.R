#Setup----
library(shiny)
library(CoSIA)
library(shinyalert)
library(plotly)
library(tidyverse)
library(shinysky)
options(shiny.maxRequestSize = 30*1024^2)
tissues_by_species <- data.frame(read.csv("tissues_by_species.csv"))
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
homolog_database_list <- c("None",
                           "HomoloGene",
                           "NCBIOrthoAnnotationPipe")

#Start UI----
ui <- fluidPage(
  sidebarPanel(
    #Gene Input----
    wellPanel(HTML("<h4>Gene Input</h4>"),
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
              actionButton("conversion_go","Convert IDs"),
              actionButton("skip_go","Skip Conversion")
    ),
    #Plot Expression----
    wellPanel(HTML("<h4>Plot Expression</h4>"),
              select2Input("plot_gene", "gene",choices=NULL),
              checkboxGroupInput("plot_species", "species",choices=species_list),
              checkboxGroupInput("plot_tissue","tissues",choices="Select Species First"),
              checkboxInput("plot_by", label=HTML("<b>Plot by Tissue</b>")),
              actionButton("plot_go", "Plot")
    ),#----
    wellPanel(HTML("<h4>Plot DS</h4>"),
              checkboxGroupInput("ds_species", "species",choices=species_list),
              checkboxGroupInput("ds_tissue","tissues",choices="Select Species First"),
              selectInput("ds_metric", "metric", choices=c("DS_Gene","DS_Tissue", "DS_Tissue_all", "DS_Gene_all")),
              actionButton("ds_go", "Plot")
              
    ),
    wellPanel(HTML("<h4>Plot CV</h4>"),
              
    )        
  ),
  #Main Panel----
  mainPanel(
    dataTableOutput("conversion_table"),
    uiOutput("plots"),
    plotOutput("ds_plot")
  )#----
)
server <- function(input,output,session){
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
    
    global_cosia@gene_set <<-  gene_ids
    global_cosia@i_species <<- gene_input_species
    global_cosia@o_species <<- conversion_output_species
    global_cosia@input_id <<- gene_input_id_type
    global_cosia@output_ids <<- conversion_output_id_types
    global_cosia@mapping_tool <<- conversion_tool
    global_cosia@ortholog_database <<-  conversion_ortholog_database
    
    global_cosia <<- getConversions(global_cosia)
    output$conversion_table <- renderDataTable({global_cosia@converted_id})
    for_input <- global_cosia@converted_id
    for_input <- for_input[,grepl("ensembl_id",names(for_input))]
    for_input <- c(for_input,sep="/")
    for_input <- do.call(paste,for_input)
    updateSelect2Input(session=session,label="gene", inputId = "plot_gene", choices=(for_input))
    
  })
  
  observeEvent(input$skip_go,{
    updateSelect2Input(session=session,label="gene", inputId = "plot_gene", choices=(unlist(strsplit(input$gene_ids,split = "\n"))))
  })
  
  observeEvent(input$plot_species,{
    tib <- getTissues(input$plot_species)
    vec <- pull(tib,Common_Anatomical_Entity_Name)
    updateCheckboxGroupInput("plot_tissue",session=session, choices = vec, inline=FALSE, label = paste("tissues for ", paste(input$plot_species, collapse=", ")))
    
  })
  
  observeEvent(input$plot_go,{
    
    global_cosia@map_species <<- input$plot_species
    global_cosia@map_tissues <<- input$plot_tissue
    global_cosia <<- getGEx(global_cosia)
    genes <- input$plot_gene
    #genes <- unlist(strsplit(gene,split = "/"))
    tissues <- input$plot_tissue
    species <- input$plot_species
    print(tissues)
    
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
    print("done")
  })
  
  observeEvent(input$ds_species,{
    tib <- getTissues(input$ds_species)
    vec <- pull(tib,Common_Anatomical_Entity_Name)
    updateCheckboxGroupInput("ds_tissue",session=session, choices = vec, inline=FALSE, label = paste("tissues for ", paste(input$ds_species, collapse=", ")))
    
  })
  
  observeEvent(input$ds_go,{
    global_cosia@metric_type<<-input$ds_metric
    global_cosia@map_tissues <- input$ds_tissue
    global_cosia@map_species <- input$ds_species
    global_cosia <- getGExMetrics(global_cosia)
    output$ds_plot <- renderPlot({
      plotDSGEx(global_cosia)
    })
    
  })
}
shinyApp(ui=ui,server=server)

