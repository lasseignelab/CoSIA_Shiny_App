library(shiny)
library(CoSIA)
library(shinyalert)
library(plotly)
tissues_by_species <- data.frame(read.csv("tissues_by_species.csv"))
species_list <- c("H. sapiens",
                  "C. elegans",
                  "D. melanogaster",
                  "M. musculus",
                  "D. rerio",
                  "R. norvegicus")
id_type_list <- c("Entrez ID",
                  "Ensembl",
                  "Ensembl With Version",
                  "Gene Name",
                  "Symbol")
tool_list <- c("biomaRt",
               "annotationDBI")

homolog_database_list <- c("None",
                           "HomoloGene",
                           "NCBIOrthoAnnotationPipe")
ui <- fluidPage(
  sidebarPanel(
    textAreaInput("conversion_input_ids","Paste IDs"),
    #actionButton("conversion_example","See ID Input Example"),
    selectInput("conversion_input_species", "Input Species",
                choices=species_list),
    selectInput("conversion_output_species", "Output Species",
                choices=species_list),
    selectInput("conversion_input_id_type", "Input ID type",
                choices=id_type_list),
    checkboxGroupInput("conversion_output_id_types", "Output ID types",
                       choices=id_type_list),
    selectInput("conversion_tool", "Tool",
                choices=tool_list),
    selectInput("conversion_ortholog_database","Otholog Database",
                choices=homolog_database_list),
    actionButton("conversionGo","Submit")
  ),
  mainPanel(
    dataTableOutput("conversion_table"),
    downloadButton("conversion_download", "Download File Once Data Table has Loaded")
  )
)
server <- function(input,output,session){
  observeEvent(input$conversionGo,{
    try({
      input_ids <- unlist(strsplit(input$conversion_input_ids,split = "\n"))
      
      input_species <- switch(input$conversion_input_species,
                              "C. elegans"="c_elegans",
                              "D. melanogaster"="d_melanogaster",
                              "M. musculus"="m_musculus",
                              "D. rerio"="d_rerio",
                              "H. sapiens"="h_sapiens",
                              "R. norvegicus"="r_norvegicus")
      output_species <- switch(input$conversion_output_species,
                               "C. elegans"="c_elegans",
                               "D. melanogaster"="d_melanogaster",
                               "M. musculus"="m_musculus",
                               "D. rerio"="d_rerio",
                               "H. sapiens"="h_sapiens",
                               "R. norvegicus"="r_norvegicus")
      input_id_type <- switch(input$conversion_input_id_type,
                              "Entrez ID"="Entrez_id",
                              "Ensembl"="Ensembl_id",
                              "Ensembl With Version"="Ensembl_id_version",
                              "Gene Name"="Gene_name",
                              "Symbol"="Symbol")
      output_id_types <- input$conversion_output_id_types
      for(x in seq(length(output_id_types)))
      {
        output_id_types[x] <- switch(output_id_types[x],
                                     "Entrez ID"="Entrez_id",
                                     "Ensembl"="Ensembl_id",
                                     "Ensembl With Version"="Ensembl_id_version",
                                     "Gene Name"="Gene_name",
                                     "Symbol"="Symbol")
      }
      
      tool <- input$conversion_tool
      ortholog_database <- input$conversion_ortholog_database
      
      print(input_ids)
      print(input_species)
      print(output_species)
      print(input_id_type)
      print(output_id_types)
      print(tool)
      print(ortholog_database)
      conversion_object <- CoSIA::CoSIAn(
        gene_set=input_ids,
        i_species=input_species,
        input_id = input_id_type,
        o_ids = output_id_types,
        o_species=output_species,
        mapping_tool =tool ,
        ortholog_database=ortholog_database,
        map_tissues="",
        map_species="",
        metric_type="")
    })
    conversion_object <- getConversions(conversion_object)
    output$conversion_table <- renderDataTable({conversion_object@converted_id})
  })
  
}
shinyApp(ui=ui,server=server)
