library(shiny)
library(shinyalert)
library(plotly)
library(miceadds)
library(shinythemes)
source.all("cosia_scripts")
tissues_by_species <- read.csv("tissues_by_species.csv")
#I MODIFIED GETTISSUEEXPRESSION TO FIX THE FOLLOWING
#zebrafish_specific <- read.csv("Filtered_Dr_BgeeDB.RData")
# mouse_specific <- read.csv("Filtered_Mm_BgeeDB.RData")
# rat_specific <- read.csv("Filtered_Rn_BgeeDB.RData")

#filter_zebrafish <- zebrafish_specific
# filter_mouse <- mouse_specific
# filter_rat <- rat_specific

#bgee_species <- filter_zebrafish
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
ui <- navbarPage(theme=shinytheme("sandstone"),HTML("<img src='lablogo.png' width='30px'>"),
                 tabPanel("Home",
                          sidebarPanel(width=3,
                                       withTags({
                                         div(
                                           h4(a(href="https://www.lasseigne.org/","Lasseigne Lab"),
                                              p(),
                                              a(href="https://www.bioconductor.org/","Bioconductor"),
                                              p(),
                                              a(href="https://bgee.org/","Bgee"),
                                              p()
                                              
                                            
                                           )
                                         )
                                       })
                          ),
                          mainPanel(
                            HTML("
                            
                            <h1>CoSIA ShinyApp</h1>
                            <!--<h2><b>C</b>r<b>o</b>ss <b>S</b>pecies<b> I</b>nvestigation and <b>A</b >nalysis</h2>-->
                            <h2>Cross Species Investigation and Analysis</h2>
                            <p>Because of the ethical issues surrounding modifying human genomes, other organisms must be used as a model. Organismal models are used to
                            study many biological processes including rare disease mechanisms, cellular pathways, and phenotypic expression. Because of the characteristics
                            the organisms share with humans, these models help researches understand these processes at a higher level, allowing for further scientific
                            discovery in the field of genetics.</p>
                            <p>An early step in developing a disease model is selecting the best model organism in which to implement the target genetic change. The R
                            package CoSIA offers insight into this decision by providing tools to compare and visualize curated wild-type RNA-Seq gene expression data
                            across species and tissues using variation and diversity metrics. The package provides additional tools for streamlined ortholog and id mapping.
                            This Shiny App is a way to access the tools provided by CoSIA without installation or knowledge of R packages. Several modules are included in
                            the package, and are detailed below.</p>
                            <h2>CoSIA Modules</h2>
                            <img src='Modules_1-4 2.jpeg' alt='Figure 1' width='800px'>
                            <h3>Gene Identifier Conversion(Modules 1&2)</h3>
                            <p>The Gene Identifier tab combines Modules 1 & 2 of CoSIA into one process. If the user wants to change the gene ID type, they can use
                            either biomaRt or AnnotationDbi to convert among any of the following gene ID types:</p>
                            <ul>
                              <li>Entrez ID</li>
                              <li>Ensembl</li>
                              <li>Ensembl With Version</li>
                              <li>Gene Name</li>
                              <li>Symbol</li>
                            </ul>
                            <p>If the user wants to convert between different species, they can use either Homologene or the NCBI Eukaryotic Genome Annotation Pipeline
                            to convert between the following:</p>
                            <ul>
                              <li><i>Homo sapiens</i></li>
                              <li><i>Caenorhabditis elegans</i></li>
                              <li><i>Drosophila melanogaster </i></li>
                              <li><i>Danio rerio</i></li>
                              <li><i>Mus musculus</i></li>
                              <li><i>Rattus norvegicus</i></li>
                            </ul>
                            
                            <h3>Cross Tissue RNA Seq-Expression(Module 3)</h3>
                            <p>The user can create a plot of a gene's expression across multiple tissues within the same species. These expression values are
                            pulled from the database Bgee, which is also used in Module 4. Once the gene's species is input into this ShinyApp, the app provides
                            a list of available tissues to be analyzed. The following species are options for this module:</p>         
                            <ul>
                              <li><i>Homo sapiens</i></li>
                              <li><i>Danio rerio</i></li>
                              <li><i>Mus musculus</i></li>
                              <li><i>Rattus norvegicus</i></li>
                            </ul>
                            <h3>Cross Species RNA Seq Expression(Module 4)</h3>
                            <p>Instead of visualizing a gene for one species and multiple tissues, Module 4 visualizes a gene for one tissue and multiple
                            species. After the user selects all the species to be analyzed, the ShinyApp provides a list of available tissues that exist
                            in each of the selected species. The available species are the same as the Cross Tissue Expression Module.</p>
                            
                                         ")
                            
                          )
                          
                 ),
                 tabPanel("Gene Identifier Conversion",
                          sidebarPanel(
                            textAreaInput("conversion_input","Paste IDs"),
                            actionButton("conversion_example","See ID Input Example"),
                            selectInput("conversion_input_species", "Input Species",
                                        choices=species_list),
                            selectInput("conversion_output_species", "Output Species",
                                        choices=species_list),
                            selectInput("conversion_input_id", "Input ID type",
                                        choices=id_type_list),
                            checkboxGroupInput("conversion_output_ids", "Output ID types",
                                               choices=id_type_list),
                            selectInput("conversion_tool", "Tool",
                                        choices=tool_list),
                            selectInput("conversion_ortholog_database","Otholog Database",
                                        choices=homolog_database_list),
                            actionButton("conversionGo","Submit")
                          ),
                          mainPanel(
                            dataTableOutput("conversion_table"),
                            downloadButton("conversion_download", "Download File Once Data Table has Loaded"),
                            HTML("
                                 <h2>Instructions</h2>
                                 <ol>
                                    <li>Paste genes into the text box, 1 gene per line. Make sure all genes follow the nomenclature guidelines for its gene ID type. See below:
                                        <ul>
                                            <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1761442/'>Entrez ID</a></li<>
                                            <li><a href='https://www.ensembl.org/Help/Faq?id=488;redirect=no'>Ensembl</a></li<>
                                            <li><a href='http://www.ensembl.org/info/genome/stable_ids/index.html'>Ensembl With Version</a></li<>
                                            <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7494048/'>Gene Name</a></li<>
                                            <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7494048/'>Symbol</a></li<>
                                        </ul></li>
                                    <li>Select the species the genes came from.</li>
                                    <li>Select the species for the genes to be converted to. If you do not want to convert between species, choose the same species as Step 2.</li>
                                    <li>Select the gene ID type for the genes you input.</li>
                                    <li>Select any number of gene ID types for the genes to be converted into.</li>
                                    <li>Select the tool that will carry out the conversion.</li>
                                    <li>If you are converting between species, select an Ortholog Database. Otherwise, select None.</li>
                                    <li>Submit.</li>
                                 </ol>
                                 ")
                          )
                 ),
                 
                 tabPanel("Cross-Tissue RNA Seq-Expression",
                          sidebarPanel(
                            textInput("tissue_gene", "Ensembl Gene ID"),
                            selectInput("tissue_species", "Species",choices=c(colnames(tissues_by_species))),
                            actionButton("tissue_select_all", "Select All Tissues"),
                            checkboxGroupInput("tissue_tissues", "Tissues", choices=""),
                            actionButton("tissueGo","Submit")
                          ),
                          mainPanel(
                            HTML("
                                  <h2>Instructions</h2>
                                  <ol>
                                      <li>Paste a single gene in <a href=''>Ensemble</a> format.</li>
                                      <li>Select the species that the gene came from.</li>
                                      <li>Select any number of tissues to be plotted.</li>
                                      <li>Submit.</li>

                                  </ol>
                                  
                                 "),
                            plotlyOutput("tissue_plot")
                          )
                 ),
                 tabPanel("Cross-Species RNA Seq-Expression",
                          sidebarPanel(
                            textInput("species_input_gene", "Ensembl Id Gene"),
                            selectInput("species_input_species", choices=c("Human", "Zebrafish", "Mouse", "Rat"), label="Input ID Species"),
                            checkboxGroupInput("species_output_species", "Output Species",
                                               choices=colnames(tissues_by_species)),
                            selectInput("species_tissue","Select Tissue Type After Selecting Species", choices=""),
                            actionButton("speciesGo","Submit")
                          ),
                            mainPanel(
                              HTML("
                                <h2>Instructions</h2>
                                <ol>
                                  <li>Paste a single gene in <a href=''>Ensemble</a> format.</li>
                                  <li>Select the species that the gene came from.</li>
                                  <li>Select any number of species to be plotted.</li>
                                  <li>Select a tissue to be plotted.</li>
                                  <li>Submit.</li>
                                </ol>
                                "),
                              plotlyOutput("species_plot")
                            )
                          
                 ),
                 tabPanel("Contact Us",
                          HTML("nsdevoss@uab.edu")
                 ),
                 tabPanel("Documentation",
                          # markdown("
                          # <img src='vignette/empty_get_conversion.png' alt='Figure 1' width='800px'>
                          # <img src='vignette/filled_get_conversion.png' alt='Figure 1' width='800px'>
                          # <img src='vignette/results_get_conversion.png' alt='Figure 1' width='800px'>
                          # <img src='vignette/filled_get_tissue.png' alt='Figure 1' width='800px'>
                          # <img src='vignette/results_get_tissue.png' alt='Figure 1' width='800px'>
                          # ")
                          markdown("
                                   The following is a demonstration of how to use this website.
                                   <h1>Gene Identifier Conversion</h1>
                                   I have a patient with a suspected mutation in the gene ENSG00000104313 and want to see if the zebrafish is a viable model organism.
                                   I go to the Gene Identifier Conversion tab.
                                   <img src='vignette/empty_get_conversion.png' alt='Figure 1' width='800px'>
                                   
                                   ")
                 )
                 
)
server <- function(input,output,session){
  observeEvent(input$conversion_example , {
    shinyalert(  title="Paste Your IDs in this format:",
                 text='
    <p style="text-align:left">
      gene_ID_01<br>
      gene_ID_02<br>
      gene_ID_03<br>
      gene_ID_04<br>
      gene_ID_05<br>
      gene_ID_06<br>
      gene_ID_07<br>
      gene_ID_08<br>
      gene_ID_09<br>
      gene_ID_10<br>
      gene_ID_11<br>
      gene_ID_12<br>
      gene_ID_13<br>
      gene_ID_14</p>',
                 html=TRUE)
  })#conversion id example pop-up
  
  #if submit button for conversion tab is pressed
  observeEvent(input$conversionGo,{
    print(paste("The input is", input))
    conversion_dataframe <- NULL
    try({
      
      objectValidity=TRUE
      
      #check that symbol and biomaRt don't conflict
      if(
        input$conversion_tool == "biomaRt" 
        && 
        ((!input$conversion_input_species %in% c("H. sapiens","M. musculus")
          &&input$conversion_input_id=="Symbol"
        )
        ||
        (
          "Symbol" %in% input$output_ids
          &&!input$conversion_output_species %in% c("H. sapiens","M. musculus")
        ))
      ){
        objectValidity=FALSE
        shinyalert(title="Error",
                   text="BiomaRt only supports Symbol ID for H. sapiens and M. musculus.
                 Select annotationDBI or another ID type",
                   type="error")
      }
      
      #check if ortholog database is selected for converting between species
      if(
        input$conversion_input_species!=input$conversion_output_species
        &&
        input$conversion_ortholog_database==homolog_database_list[1]
      ){
        objectValidity=FALSE
        shinyalert(title="Error",
                   text="An Ortholog Database is needed for converting between species",
                   type="error")
      }
      
      #make sure ids were input
      if(input$conversion_input==""){
        objectValidity=FALSE
        shinyalert(title="Error",
                   text="No IDs provided",
                   type="error")
      }
      
      #make sure output_ids were selected
      if(is.null(input$conversion_output_ids)){
        objectValidity=FALSE
        shinyalert(title="Error",
                   text="No Output ID Types provided",
                   type="error")
      }
      
      #check input_ids are the right type
      input_input <- unlist(strsplit(input$conversion_input,split = "\n"))
      if(input$conversion_input_id=="Entrez ID"){
        if(sum(grepl("\\D",input_input))>0){#sees if there are any letters
          objectValidity=FALSE
          shinyalert(title="Error",
                     text="1 or more IDs are not Entrez ID Format
                   Entrez IDs are only numbers",
                     type="error")
        }
      }
      
      
      #if it passes all the above checks
      if(objectValidity){
        #tidy up user inputs to be used in creating CosiaAnnotate Object
        withProgress(message = 'Retrieving your data', value = 0, {
          incProgress(1/4, detail="Tidying Query")
          input_species <- switch(input$conversion_input_species,
                                  "C. elegans"="c_elegans",
                                  "D. melanogaster"="d_melanogaster",
                                  "M. musculus"="mus_musculus",
                                  "D. rerio"="danio_rerio",
                                  "H. sapiens"="homo_sapiens",
                                  "R. norvegicus"="r_norvegicus")
          output_species <- switch(input$conversion_output_species,
                                   "C. elegans"="c_elegans",
                                   "D. melanogaster"="d_melanogaster",
                                   "M. musculus"="mus_musculus",
                                   "D. rerio"="danio_rerio",
                                   "H. sapiens"="homo_sapiens",
                                   "R. norvegicus"="r_norvegicus")
          input_id <- switch(input$conversion_input_id,
                             "Entrez ID"="Entrez.id",
                             "Ensembl"="Ensembl.id",
                             "Ensembl With Version"="Ensembl.id.version",
                             "Gene Name"="Gene.name",
                             "Symbol"="Symbol"
          )
          output_ids <- input$conversion_output_ids
          for(x in seq(length(output_ids)))
          {
            output_ids[x] <- switch(output_ids[x],
                                    "Entrez ID"="Entrez.id",
                                    "Ensembl"="Ensembl.id",
                                    "Ensembl With Version"="Ensembl.id.version",
                                    "Gene Name"="Gene.name",
                                    "Symbol"="Symbol")
          }
          if(input$conversion_tool=="biomaRt"){
            if(input_id=="Symbol"){
              input_id <- switch(input_species,
                                 "homo_sapiens"="HGNC.Symbol",
                                 "mus_musculus"="MGI.Symbol")
            }
            for(x in seq(length(output_ids)))
            {
              if(output_ids[x]=="Symbol"){
                output_ids[x] <- switch(output_species,
                                        "homo_sapiens"="HGNC.Symbol",
                                        "mus_musculus"="MGI.Symbol")
              }
            }
          }
          
          tool <- input$conversion_tool
          ortholog_database <- input$conversion_ortholog_database
          
          print(input_input)
          print(input_species)
          print(output_species)
          print(input_id)
          print(output_ids)
          print(ortholog_database)
          #create object from userinputs
          cosia_annotate <- new("CosiaAnnotate",
                                input=input_input,
                                input_species=input_species,
                                output_species=output_species,
                                input_id=input_id,
                                output_ids=output_ids,
                                tool=tool,
                                ortholog_database=ortholog_database)
          print(cosia_annotate@input)
          print(cosia_annotate@input_species)
          print(cosia_annotate@output_species)
          print(cosia_annotate@input_id)
          print(cosia_annotate@output_ids)
          print(cosia_annotate@ortholog_database)
          incProgress(2/4, detail="Retrieving Data")
          
          #do conversion
          conversion_dataframe <- getConversion(cosia_annotate)
          incProgress(3/4, detail="Preparing Download")
          
          output$conversion_table <- renderDataTable({conversion_dataframe}) 
          #download file if user wants
          output$conversion_download <- downloadHandler(filename = function() {
            paste0("conversion_download", ".csv")
          },
          content = function(file) {
            write.csv(conversion_dataframe,file)
          })
          incProgress(4/4, detail="Done")
          
          
        })
      }
    })
    if(is.null(conversion_dataframe)&&objectValidity){
      shinyalert(title="Warning",
                 text=
                   "CoSIA was unable to perform your request. Check the troubleshooting section for help.",
                 type="warning")
    }
    
  })
  #-------
  observeEvent(input$species_output_species,{
    output_id_species <- input$species_output_species
    if(length(output_id_species)>1){
      tissuechoice <- Reduce(intersect,tissues_by_species[,output_id_species])
    }else{
      tissuechoice <- tissues_by_species[,output_id_species]
    }
    updateSelectInput("species_tissue",session=session, choices = tissuechoice)
  })#updates choices when species_output_species changes
  #-------
  #if submit button for Species Expression tab is pressed
  observeEvent(input$speciesGo,{
    input_gene <- input$species_input_gene
    print(input_gene)
    input_id_species <- input$species_input_species
    print(input_id_species)
    output_id_species <- input$species_output_species
    print(output_id_species)
    tissue <- input$species_tissue
    print(tissue)
    input_id_species <- switch(input_id_species,
                               "Human"="homo_sapiens",
                               "Zebrafish"="danio_rerio",
                               "Mouse"="mus_musculus",
                               "Rat"="r_norvegicus"
    )
    print(input_id_species)
    for(x in seq(length(output_id_species)))
    {
      output_id_species[x] <- switch(output_id_species[x],
                                     "Human.Tissues"="homo_sapiens",
                                     "Zebrafish.Tissues"="danio_rerio",
                                     "Mouse.Tissues"="mus_musculus",
                                     "Rat.Tissues"="r_norvegicus")
    }
    print(output_id_species)
    ready_genes <- NULL
    ready_species <- NULL
    for(x in seq(length(output_id_species))){
      inter_species <- output_id_species[x]
      ready_species <- c(ready_species,inter_species)
      convert_ids <- new("CosiaAnnotate",
                         input=input_gene,
                         input_species=input_id_species,
                         input_id="Ensembl_id",
                         output_ids="Ensembl_id",
                         output_species=inter_species,
                         tool="biomaRt",
                         ortholog_database="HomoloGene")
      converted_ids <- getConversion(convert_ids)
      column_name <- paste(inter_species,"_Ensembl_ID",sep="")
      try({
        ready_genes <- c(ready_genes,converted_ids[1,column_name])
      })
      try({
        ready_genes <- c(ready_genes,converted_ids[1,"ensembl_gene_id"])
      })
    }
    plot_species <- new("CosiaExpressSpecies",
                        list_of_ensembl_ids=ready_genes,
                        list_of_respective_species=ready_species,
                        single_tissue=tissue)
    figure_species <- getSpeciesExpression(plot_species)
    output$species_plot <- renderPlotly({figure_species})
    
    
    
  })
  #-------
  observeEvent(input$tissue_species,{
    
    tissuechoice <- tissues_by_species[,input$tissue_species]
    tissuechoice <- tissuechoice[nzchar(tissuechoice)]
    updateCheckboxGroupInput("tissue_tissues",session=session, choices = tissuechoice, inline=FALSE)
    
  })#updates choices when tissue_species changes
  #if submit button for tissue expression is pressed
  observeEvent(input$tissue_select_all,{
    tissuechoice <- tissues_by_species[,input$tissue_species]
    tissuechoice <- tissuechoice[nzchar(tissuechoice)]
    if(input$tissue_select_all%%2==0){
      updateCheckboxGroupInput(session, "tissue_tissues", choices = tissuechoice)
    }
    if(input$tissue_select_all%%2==1){
      updateCheckboxGroupInput(session, "tissue_tissues", choices = tissuechoice, selected = tissuechoice)
    }
  })
  #-------
  
  observeEvent(input$tissueGo,{
    try({
      
      gene_id <- input$tissue_gene
      species <- switch(input$tissue_species,
                        "Human.Tissues"="Homo_sapiens",
                        "Mouse.Tissues"="Mus_musculus",
                        "Zebrafish.Tissues"="Danio_rerio",
                        "Rat.Tissues"="R_norvegicus")
      tissues <- input$tissue_tissues
      # print(gene_id)
      # print(species)
      # print(tissues)
      cosia_tissue <- new("CosiaExpressTissue",
                          single_gene=gene_id,
                          gene_species=species,
                          tissues=tissues)
      single_gene <- cosia_tissue@single_gene
      final_figure <- getTissueExpression(cosia_tissue)
      output$tissue_plot <- renderPlotly({final_figure})
    })
  })
  
}
shinyApp(ui=ui,server=server)
