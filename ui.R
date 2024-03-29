tissues_file <<- readr::read_csv("data/tissues_file")
# tissues_by_species <- data.frame(read.csv("tissues_by_species.csv"))
species_list <- c(
  "Select...",
  "h_sapiens",
  "c_elegans",
  "d_melanogaster",
  "m_musculus",
  "d_rerio",
  "r_norvegicus"
)
id_type_list <- c(
  "Select...",
  "Ensembl_id",
  "Entrez_id",
  "Symbol"
)
tool_list <- c(
  "annotationDBI",
  "biomaRt"
)

homolog_database_list <- c(
  "HomoloGene",
  "NCBIOrthoAnnotationPipe"
)
# Images
CoSIAlogo <- "CoSIA_logo.png"
Lablogo <- "lablogo.png"
CoSIAworkflow <- "CoSIA_Workflow.png"

# Main Tab ----
CoSIA_about <- tabPanel(
  title = "About CoSIA",
  titlePanel("Cross-Species Investigation & Analysis"),
  div(
    claas = "logo-desc",
    div(
      class = "logo-div",
      tags$figure(
        align = "left",
        tags$img(
          class = "logo",
          src = CoSIAlogo
        ),
        tags$figcaption(
          "CoSIA logo"
        )
      )
    ),
    div(
      class = "desc",
      hr(),
      br(),
      br(),
      br(),
      p(
        class = "my-p",
        strong(
          "CoSIA"
        ), 
        "is a",
        tags$a(href="https://github.com/lasseignelab/CoSIA","package"),
        "created by the",
        tags$a(href="https://www.lasseigne.org/","Lasseigne Lab"),
        "that provides researchers with an 
        alternative methodology for comparing across species and tissues using 
        normal wild-type RNA-Seq Gene Expression data from Bgee.
        Using RNA-Seq Gene Expression data, CoSIA provides multiple 
        visualization tools to explore the transcriptome diversity and 
        variation across genes, tissues, and species. CoSIA uses Coefficient of 
        Variation, Shannon Entropy, and Specificity to calculate
        transcriptome diversity and variation. CoSIA also provides additional 
        conversion tools and utilities to provide a streamlined methodology for 
        cross-species comparison across the tissues and genes of five commonly
        used biomedical research species",
        em(
          "(Mus musculus, Rattus norvegicus, Danio rerio, Drosophila melanogaster, 
        and Caenorhabditis elegans)"
        ),
        "in addition to", em("Homo sapiens"),"."
      )
    )
  ),
  br(),
  br(),
  br(),
  hr(),
  div(
    class="use-cosia",
    tags$h2(
      "Using CoSIA"
    ),
    br(),
    tags$figure(
      align = "center",
      tags$img(
        class="cosia-workflow",
        src = CoSIAworkflow
      ),
      tags$figcaption(
        "CoSIA Workflow"
      )
    )
  ),
  br(),
  div(
    class="conversion-tips",
    h4(
      "Converting Gene IDs"
    ),
    p(
      class = "my-p",
      "To get started, navigate to the",
      em("Inputs & Conversions"),
      "tab. This is where you will provide inputs to be converted between 
      different gene identifiers in the same species, as well 
      as orthologs in different species."
    ),
    p(
      class = "my-p",
      strong("NOTE:"), 
      "When starting with gene symbols, you", em("must"), 
      "use species-specific formatting rules.", 
      br(), 
      "Example: Your gene input is insulin-like growth factor 1 receptor .",
    ),
    div(
      class="my-list",
      tags$ul(
        tags$li(
          "The HGNC format for human is",
          strong("IGF1R")
        ),
        tags$li(
          "The MGI format for mouse and rat is",
          strong("Igf1r")
        ),
        tags$li(
          "The Flybase format for fly is",
          strong("InR")
        ),
        tags$li(
          "The Wormbase format for nematode is",
          strong("daf-2")
        )
      )
    ),
    div(
      p(
        class = "my-p",
        "Information for formatting input symbols for human,
         mouse & rat, and zebrafish can be found using",
        a(href = "https://www.genenames.org/about/guidelines/","HGNC"),
        a(href = "https://www.informatics.jax.org/", "MGI"), "or",
        a(href = "http://zfin.org/action/marker/search", "ZFIN"),
        "respectively or at",
        a(href = "https://www.ncbi.nlm.nih.gov/gene", "NCBI"),
        ". Fly and nematode information can be found using",
        a(href = "https://flybase.org/", "Flybase"), "and",
        a(href = "https://wormbase.org/species/c_elegans#014--10","Wormbase"),
        ", respectively."
      )
    ),
    div(
      p(
        class = "my-p",
        "If you would like to use a csv file as input, it is advised that you  
        use a subset smaller than 10,000 genes. The gene identifiers", strong("must be in  
        the first column"), ". Pasted gene entries need to be separated by a new
        line."
      ),
      p(
        class = "my-p",
        "To use any of the downstream plotting tabs, you will need to select", 
        code("Ensembl_id"), "as an", strong("Output ID type."), 
        "Any species that you would like to include in downstream plots need to  
        be selected as",strong("Output Species"), "during ID Conversion"
      ),
      p(
        class = "my-p",
        "The default tools are", strong("annotationDBI"), "and", strong("Homologene"),
        "because of their speed.", strong("biomaRt"), "and", strong("NCBIOrthologDatabase"),
        "are also available via dropdown menus."
      )
    )
  ),
  hr(),
  br(),
  div(
    class="plotting-tips",
    div(
      h4("Plotting Gene Expression & Metrics"),
      p(
        class = "my-p",
        "After ID conversion on the", em("Inputs & Conversions"), 
        "tab, your converted gene IDs for each species are stored and can be
        used in three different comparative gene expression visualizations 
        found on the remaining tabs."
      )
    ),
    div(
      class="my-list",
      tags$ul(
        tags$li(
          "Plot Expression visualizes gene expression across tissues or species 
        for a single gene"
        ),
        tags$li(
          "Plot Diversity & Specificity visualizes diversity and specificity 
          across your genes in your selected tissues with", code("DS_Gene"),
          "or across your selected tissues in your geneset with", 
          code("DS_Tissue")
        ),
        tags$li(
          "Plot Coefficient of Variation visualizes variability in expression 
          across tissues between species"
        )
      ),
      br(),
      p(
        strong("NOTE:"), code("DS_Tissue"), "requires more than one input gene and",
        code("DS_Gene"), "requires more than one input tissue."
      )
    ),
    hr(),
    br(),
    div(
      class = "branding",
      p(
        "All figures created with BioRender.com"
      ),
      p(
        "Copyright 2023 by",
        tags$a(href="https://www.lasseigne.org/","Lasseigne Lab")
      )
    )
  )
)
# Input Tab----
input_sidebar <- sidebarPanel(
  width = 3,
  wellPanel(
    HTML("<h4>Gene Input</h4>"),
    textAreaInput("gene_ids", "Paste IDs"),
    actionButton("conversion_id_instructions", "Formatting IDs Info"),
    fileInput("gene_file", "Upload CSV File",
              multiple = FALSE,
              accept = ".csv"
    ),
    selectInput("gene_input_species", "Input Species",
                choices = species_list, selected = "Select..."
    ),
    selectInput("gene_input_id_type", "Input ID type",
                choices = id_type_list, selected = "Select..."
    ),
    column(
      width = 6,
      fluidRow(
        actionButton("check_inputs", "Check Inputs")
      )
    ),
    column(
      width = 6,
      fluidRow(
        actionButton("reset_inputs", "Reset Outputs")
      )
    )
  )
)

input_main <- mainPanel(
  div(
    div(
      class="conversion-tips",
      verbatimTextOutput("input_warnings"),
      h4("Formatting Check"),
      p(
        class="my-p-input_1",
      "IDs that are not formatted for the correct species will cause the app to 
      error and print below. (see below for formatting tips)"),
      verbatimTextOutput("input_errors"),
      h4("Problematic Inputs"),
      p(
        class="my-p-input_1",
        "IDs that were not found in your selected species and ID type will print below."),
      verbatimTextOutput("input_printed"),
      verbatimTextOutput("failed_input_check"),
      br(),
      br(),
      hr()
    ),
    div(
      class="conversion-tips",
      h4(
        "Input Handling in CoSIA Shiny"
      ),
      p(
        class="my-p-input_2",
        "In this tab, inputs (in the textbox on the left) will be checked and 
      corrected for whitespace. Inputs are checked for the correct formatting
      based on input ID type and species. CoSIA shiny checks for correct Ensembl ID
        prefixes by species, for numerical input for Entrez IDs, and for basic
        symbol formatting by species. Check out the example below for symbol 
        by species formatting.",
        strong("NOTE:"), 
        "ID checks are based on pattern matching. This means if you input human 
        Ensembl IDs but select Symbol as your ID type, 
        there will be no error message. This is because Symbol checks for human
        requires inputs to have capitalized text and allows for numbers and 
        hypens (which also matches ENSG00000123456).",
        em("Still, the IDs will return as problematic inputs.")
      ),
      br(),
      p(
        class="my-p-input_2",
        "When starting with gene symbols, you", em("must"), 
        "use species-specific formatting rules.", 
        "Example: Your gene input is insulin-like growth factor 1 receptor.",
      ),
      div(
        class="my-list-inputs",
        tags$ul(
          tags$li(
            "The HGNC format for human is",
            strong("IGF1R")
          ),
          tags$li(
            "The MGI format for mouse and rat is",
            strong("Igf1r")
          ),
          tags$li(
            "The Flybase format for fly is",
            strong("InR")
          ),
          tags$li(
            "The Wormbase format for nematode is",
            strong("daf-2")
          )
        )
      ),
      div(
        p(
          class="my-p-input_2",
          "Information for formatting input symbols for human,
         mouse & rat, and zebrafish can be found using",
          a(href = "https://www.genenames.org/about/guidelines/","HGNC"),
          a(href = "https://www.informatics.jax.org/", "MGI"), "or",
          a(href = "http://zfin.org/action/marker/search", "ZFIN"),
          "respectively or at",
          a(href = "https://www.ncbi.nlm.nih.gov/gene", "NCBI"),
          ". Fly and nematode information can be found using",
          a(href = "https://flybase.org/", "Flybase"), "and",
          a(href = "https://wormbase.org/species/c_elegans#014--10","Wormbase"),
          ", respectively."
        )
      )
    )
  )
)

CoSIA_input <- tabPanel(
  "Inputs",
  titlePanel("Gene ID Inputs"),
  sidebarLayout(
    input_sidebar, input_main
  )
)

# Conversion Tab----
convert_sidebar <- sidebarPanel(
  width = 3,
  wellPanel(
    HTML("<h4>ID Conversion</h4>"),
    actionButton("conversion_output_instructions", "Formatting Output"),
    checkboxGroupInput("conversion_output_species", "Output Species",
                       choices = species_list[-1]
    ),
    checkboxGroupInput("conversion_output_id_types", "Output ID types",
                       choices = id_type_list[-1]
    ),
    selectInput("conversion_tool", "Tool",
                choices = tool_list
    ),
    selectInput("conversion_ortholog_database", "Otholog Database",
                choices = homolog_database_list
    ),
    actionButton("conversion_go", "Convert IDs")
  )
)

convert_main <- mainPanel(
  div(
    p(
      class="my-p",
      strong("NOTE:"),
      "If you are unsuccessful in obtaining conversions for c_elegans and/or
      d_melanogaster as outputs, please refresh and attempt with biomaRt as 
      the tool used for conversion."
    )
  ),
  verbatimTextOutput("convert_errors"),
  dataTableOutput("conversion_table"),# DT:: causes app to break
  h5("Once IDs are converted, use the button below to download CSV formatted Output:"),
  verbatimTextOutput("convert_printed"),
  verbatimTextOutput("convert_warnings"),

  downloadButton("conversion_download", "Download Converted IDs")
  
)

CoSIA_convert <- tabPanel(
  "Conversions",
  titlePanel("Gene ID and Ortholog Conversion"),
  sidebarLayout(
    convert_sidebar, convert_main
  )
)

# Expression Tab----
PlotExp_sidebar <- sidebarPanel(
  width = 3,
  # Plotting Expression
  wellPanel(
    HTML("<h4>Plot Expression</h4>"),
    actionButton("plot_instructions", "Instructions"),
    selectizeInput("plot_gene", "gene", choices = NULL, multiple=TRUE),
    selectInput("plot_by", label = "Plot by", choices=c("Species","Tissue")),
    checkboxGroupInput("plot_species",
                       "species",
                       choices = species_list[-1]
    ),
    checkboxGroupInput("plot_tissue", "tissues",
                       choices = "Select Species First"
    ),
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
  # Plotting Diversity & Specificity----
  wellPanel(
    HTML("<h4>Plot DS</h4>"),
    checkboxGroupInput("ds_tissue", "tissues",
                       choices = "run conversion first"
    ),
    selectInput("ds_metric", "metric",
                choices = c("DS_Gene", "DS_Tissue")
    ),
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
  # Plotting Coefficient of Variation----
  wellPanel(
    HTML("<h4>Plot CV</h4>"),
    checkboxGroupInput("cv_tissue", "tissues",
                       choices = "run conversion first"
    ),
    selectInput("cv_metric", "metric",
                choices = c("CV_Tissue", "CV_Species")
    ),
    actionButton("cv_go", "Plot")
  )
)
PlotCV_main <- mainPanel(
  plotOutput("cv_plot")
)
CoSIA_PlotCV <- tabPanel(
  "Plot Coefficient of Variation",
  titlePanel("Gene Expression Variation Across Tissues or Species"),
  sidebarLayout(
    PlotCV_sidebar, PlotCV_main
  )
)


ui <- navbarPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(type = "text/javascript", src = "code.js")
  ),
  title = "CoSIA: Cross-Species Investigation & Analysis",
  position = "fixed-top",
  CoSIA_about,
  CoSIA_input,
  CoSIA_convert,
  CoSIA_PlotExp,
  CoSIA_PlotDS,
  CoSIA_PlotCV
)
