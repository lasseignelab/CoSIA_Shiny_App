load("/Users/Nathan2/Desktop/lasseigneLab/CoSIA_Shiny_App/oopCosia/EH_Data.RData")
library(CoSIA)
Kidney_Genes<-CoSIA::CoSIAn(
  gene_set = c("ENSG00000008710","ENSG00000118762","ENSG00000152217"),
  i_species = "h_sapiens",
  input_id = "Ensembl_id",
  o_species = "m_musculus",
  o_ids ="Ensembl_id", 
  mapping_tool = "annotationDBI",
  ortholog_database = "HomoloGene",
  map_tissues = c("adult mammalian kidney","liver","heart"), 
  map_species = c("m_musculus","h_sapiens"),
  metric_type = "CV")
Kidney_gene_conversion<-CoSIA::getConversions(Kidney_Genes)
Kidney_gene_gex<- getGEx(Kidney_gene_conversion)
plotSpeciesGEx(Kidney_gene_gex,single_tissue="adult mammalian kidney",single_gene="ENSG00000008710")
