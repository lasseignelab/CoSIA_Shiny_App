
tissues_file <<- read_csv("data/tissues_file")
#tissues_by_species <- data.frame(read.csv("tissues_by_species.csv"))
species_list <- c("Select...",
                  "h_sapiens",
                  "c_elegans",
                  "d_melanogaster",
                  "m_musculus",
                  "d_rerio",
                  "r_norvegicus"
)
id_type_list <- c("Select...",
                  "Ensembl_id",
                  "Entrez_id",
                  "Symbol"
                  )
tool_list <- c("annotationDBI",
               "biomaRt")

homolog_database_list <- c("HomoloGene",
                           "NCBIOrthoAnnotationPipe")
# Images
CoSIAlogo <- "CoSIA_logo.png"
Lablogo <- "lablogo.png"
CoSIAworkflow <- "CoSIA_Workflow.png"

# Main Tab
CoSIA_about <- tabPanel(title = "About CoSIA",
                        titlePanel("Cross-Species Invesigation & Analysis"),
                        tags$figure(
                          align = "left",
                          tags$img(
                            src = CoSIAlogo,
                            height = 450,
                            width = 400,
                          ),
                          tags$figcaption(
                            "CoSIA logo; Created in BioRender"
                          )
                        ),
                        hr(),
                        br(),
                        h2("Description:"),
                        br(),
                        p(strong("CoSIA"), "is a package that provides 
                        researchers with an alternative methodology for 
                        comparing across species and tissues using normal 
                        wild-type RNA-Seq Gene Expression data from Bgee. 
                        Using RNA-Seq Gene Expression data, CoSIA provides 
                        multiple visualization tools to explore the 
                        transcriptome diversity and variation across genes, 
                        tissues, and species.CoSIA uses Coefficient of Variation
                        and Shannon Entropy and Specificity to calculate 
                        transcriptome diversity and variation. CoSIA also 
                        provides additional conversion tools and utilities to 
                        provide a streamlined methodology for cross-species 
                        comparison across the tissues and genes of five commonly
                        used biomedical research species", 
                        em("(Mus musculus, Rattus norvegicus, Danio rerio, 
                        Drosophila melanogaster, and Caenorhabditis elegans)"), 
                        "in addition to Homo sapiens."
                        ),
                        hr(),
                        br(),
                        h2("Using CoSIA"),
                        br(),
                        tags$figure(
                          align = "center",
                          tags$img(
                            src = CoSIAworkflow,
                            height = 450,
                            width = 1000,
                          ),
                          tags$figcaption(
                            "CoSIA Workflow; Created in BioRender"
                          )
                        ),
                        br(),
                        h4("Converting Gene IDs"),
                        p("To get started, navigate to the", 
                        em("Inputs & Conversions"), 
                        "tab. This is where you will provide inputs to be 
                        converted between different gene identifiers in the 
                        same species, as well as orthologs in different 
                        species."
                        ),
                        br(),
                        p(strong("NOTE:"), "When starting with gene symbols, 
                        you", em("must"),"use species-specific formatting rules. 
                        Example: Your gene input is insulin-like growth factor 
                        1."
                        ),
                        tags$ul(
                          tags$li("The HGNC format for human is",
                                  strong("IGF1")),
                          tags$li("The MGI format for mouse and rat is",
                                  strong("Igf1")),
                          tags$li("The Flybase format for fly is",
                                  strong("daf-2")),
                          tags$li("The Wormbase format for nematode is", 
                                  strong("dilp1"))
                          
                        ),
                        p("Information for formatting input symbols for human, 
                        mouse & rat, fly, nematode, and zebrafish can be found 
                        using HGNC, MGI, Flybase, Wormbase, or ZFIN, 
                        respectively or all at NCBI."
                        ),
                        br(),
                        p("If you would like to use a csv file as input, it is 
                          advised that you use a small subset of genes. The
                          gene identifiers", strong("must be in the first 
                          column"), ". Pasted gene entries need to be separated 
                          by a new line."
                        ),
                        br(),
                        p("To use any of the downstream plotting tabs, 
                          you will need to select", em(strong("Ensembl_id")),
                          "as an", strong("Output ID type."), "Any species that you would like
                          to include in downstream plots need to be selected
                          as", strong("Output Species"), "during ID Conversion"
                        ),
                        br(),
                        h4("Plotting Gene Expression & Metrics")
)

# Conversion Tab----
convert_sidebar <- sidebarPanel(
  width = 3,

  wellPanel(HTML("<h4>Gene Input</h4>"),
            actionButton("conversion_instructions", "Instructions"),
            textAreaInput("gene_ids","Paste IDs"),
            fileInput("gene_file", "Upload CSV File",
                      multiple = FALSE,
                      accept = ".csv"),
            selectInput("gene_input_species", "Input Species",
                        choices=species_list, selected = "Select..."),
            selectInput("gene_input_id_type", "Input ID type",
                        choices=id_type_list, selected = "Select...")
            ),
  wellPanel(HTML("<h4>ID Conversion</h4>"),
            checkboxGroupInput("conversion_output_species", "Output Species",
                               choices=species_list[-1]),
            checkboxGroupInput("conversion_output_id_types", "Output ID types",
                               choices=id_type_list[-1]),
            selectInput("conversion_tool", "Tool",
                        choices=tool_list),
            selectInput("conversion_ortholog_database","Otholog Database",
                        choices=homolog_database_list),
            actionButton("conversion_go","Convert IDs")
            )
  )

convert_main <- mainPanel(
  dataTableOutput("conversion_table"), #DT:: causes app to break
)

CoSIA_convert <- tabPanel(
  "Inputs & Conversions",
  titlePanel("Gene ID and Ortholog Conversion"),
  sidebarLayout(
    convert_sidebar, convert_main
  )
)

# Expression Tab
PlotExp_sidebar <- sidebarPanel(
  width = 3,
  #Plotting Expression----
  wellPanel(HTML("<h4>Plot Expression</h4>"),
            actionButton("plot_instructions","Instructions"),
            select2Input("plot_gene", "gene",choices=NULL),
            checkboxGroupInput("plot_species", 
                               "species",choices=species_list[-1]),
            checkboxGroupInput("plot_tissue","tissues",
                               choices="Select Species First"),
            checkboxInput("plot_by", label=HTML("<b>Plot by Tissue</b>")),
            actionButton("plot_go", "Plot")
            )
  )

PlotExp_main <- mainPanel(
  uiOutput("plots")
  
)
CoSIA_PlotExp <- tabPanel(
  "Plot Expression",
  titlePanel("Gene Expression Across Tissues or Species"),
  sidebarLayout(
    PlotExp_sidebar, PlotExp_main
  )
)

# Diversity Specificity Tab
PlotDS_sidebar <- sidebarPanel(
  width = 3,
  #Plotting Diversity & Specificity----
  wellPanel(HTML("<h4>Plot DS</h4>"),
            checkboxGroupInput("ds_tissue","tissues",
                               choices="run conversion first"),
            selectInput("ds_metric", "metric", 
                        choices=c("DS_Gene","DS_Tissue")),
            actionButton("ds_go", "Plot")
  )
)
PlotDS_main <- mainPanel(
  plotOutput("ds_plot")
  
)
CoSIA_PlotDS <- tabPanel(
  "Plot Diversity & Specificity",
  titlePanel("Diversity & Specificity Across Tissues or Genes"),
  sidebarLayout(
    PlotDS_sidebar, PlotDS_main
  )
)

# Coefficient of Variation
PlotCV_sidebar <- sidebarPanel(
  width = 3,
  #Plotting Coefficient of Variation----
  wellPanel(HTML("<h4>Plot CV</h4>"),
            checkboxGroupInput("cv_tissue", "tissues", 
                               choices="run conversion first"),
            selectInput("cv_metric", "metric", 
                        choices= c("CV_Tissue", "CV_Species")),
            actionButton("cv_go", "Plot")
  )
)
PlotCV_main <- mainPanel(
  plotOutput("cv_plot")
  
)
CoSIA_PlotCV <- tabPanel(
  "Plot Coefficient of Variation",
  titlePanel("Gene Expression Variation Across Tissues Between Species"),
  sidebarLayout(
    PlotCV_sidebar, PlotCV_main
  )
)

ui <- navbarPage(
                 title = "CoSIA: Cross-Species Investigation & Analysis",
                 CoSIA_about,
                 CoSIA_convert,
                 CoSIA_PlotExp,
                 CoSIA_PlotDS,
                 CoSIA_PlotCV
                 )