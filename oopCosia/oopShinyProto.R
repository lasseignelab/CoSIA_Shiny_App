#Setup----
library(shiny)
library(CoSIA)
library(shinyalert)
library(plotly)
library(tidyverse)
library(shinysky)
options(shiny.maxRequestSize = 30*1024^2)
tissues_by_species <- data.frame(read.csv("tissues_by_species.csv"))
species_list <- c("c_elegans",
                  "d_melanogaster",
                  "m_musculus",
                  "d_rerio",
                  "h_sapiens",
                  "r_norvegicus")
id_type_list <- c("Entrez_id",
                  "Ensembl_id",
                  "Ensembl_id_version",
                  "Gene_name",
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
    )#----
  ),
  #Main Panel----
  mainPanel(
    dataTableOutput("conversion_table"),
    plotlyOutput("plot")
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
    gene <- input$plot_gene
    gene <- unlist(strsplit(gene,split = "/"))
    tissue <- input$plot_tissue[1]

    print(paste("global_cosia@gene_set",global_cosia@gene_set))
    print(paste("global_cosia@i_species",global_cosia@i_species))
    print(paste("global_cosia@o_species",global_cosia@o_species))
    print(paste("global_cosia@input_id",global_cosia@input_id))
    print(paste("global_cosia@output_ids",global_cosia@output_ids))
    print(paste("global_cosia@mapping_tool",global_cosia@mapping_tool))
    print(paste("global_cosia@ortholog_database",global_cosia@ortholog_database))
    print(paste("global_cosia@map_species",global_cosia@map_species))
    print(paste("global_cosia@map_tissues",global_cosia@map_tissues))
    
    print(gene[1])
    
    output$plot <- renderPlotly({plotSpeciesGEx(global_cosia,tissue, gene[1])})
    
  })
}
shinyApp(ui=ui,server=server)

