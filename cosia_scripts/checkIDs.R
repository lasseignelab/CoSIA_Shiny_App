
check_ids <- function(ids, type, species){
  species_check <- character(0)
  switch(as.character(type),
         Ensembl_id = {
           # switch species----
           switch(as.character(species),
                  h_sapiens = {
                    patt <- "^ENSG"
                    matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                    if (!all(matches)) {
                      not_matches <- which(!matches)
                      stop("\nError, check formatting: The following elements are not formatted as human Ensembl IDs (org.Hs.eg.db)\n",
                           not_matches)
                    }
                    species_check <- AnnotationDbi::keys(org.Hs.eg.db, 
                                                         keytype = "ENSEMBL")
                  },
                  m_musculus = {
                    patt <- "^ENSMUSG"
                    matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                    if (!all(matches)) {
                      not_matches <- which(!matches)
                      stop("\nError, check formatting: The following elements are not formatted as mouse Ensembl IDs (org.Mm.eg.db)\n",
                           not_matches)
                    }
                    species_check <- AnnotationDbi::keys(org.Mm.eg.db, 
                                                         keytype = "ENSEMBL")
                  },
                  r_norvegicus = {
                    patt <- "^ENSRNOG"
                    matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                    if (!all(matches)) {
                      not_matches <- which(!matches)
                      stop("\nError, check formatting: The following elements are not formatted as rat Ensembl IDs (org.Rn.eg.db)\n",
                           not_matches)
                    }
                    species_check <- AnnotationDbi::keys(org.Rn.eg.db, 
                                                         keytype = "ENSEMBL")
                  },
                  c_elegans = {
                    patt <- "^WBGene"
                    matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                    if (!all(matches)) {
                      not_matches <- which(!matches)
                      stop("\nError, check formatting: The following elements are not formatted as nematode Ensembl IDs (org.Ce.eg.db)\n",
                           not_matches)
                    }
                    species_check <- AnnotationDbi::keys(org.Ce.eg.db, 
                                                         keytype = "ENSEMBL")
                  },
                  d_melanogaster = {
                    patt <- "^FBgn"
                    matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                    if (!all(matches)) {
                      not_matches <- which(!matches)
                      stop("\nError, check formatting: The following elements are not formatted as fly Ensembl IDs (org.Dm.eg.db)\n",
                           not_matches)
                    }
                    species_check <- AnnotationDbi::keys(org.Dm.eg.db, 
                                                         keytype = "ENSEMBL")
                  },
                  d_rerio = {
                    patt <- "^ENSDARG"
                    matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                    if (!all(matches)) {
                      not_matches <- which(!matches)
                      stop("\nError, check formatting: The following elements are not formatted as zebrafish Ensembl IDs (org.Dr.eg.db)\n",
                           not_matches)
                    }
                    species_check <- AnnotationDbi::keys(org.Dr.eg.db, 
                                                         keytype = "ENSEMBL")
                  })
           # return single df that has IDs and if it is found or not
          ## ids in spec_Ens
         },
         Entrez_id = {
           switch(as.character(species),
             h_sapiens = {
               patt <- "^[0-9]+$"
               matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
               if (!all(matches)) {
                 notnum <- which(!matches)
                 stop("\nError, check formatting: The following elements are not formatted as human Entrez IDs (org.Hs.eg.db)\n",
                      notnum)
               }
               species_check <- AnnotationDbi::keys(org.Hs.eg.db, keytype = "ENTREZID")
             },
             m_musculus = {
               patt <- "^[0-9]+$"
               matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
               if (!all(matches)) {
                 notnum <- which(!matches)
                 stop("\nError, check formatting: The following elements are not formatted as mouse Entrez IDs (org.Mm.eg.db)\n",
                      notnum)
               }
               species_check <- AnnotationDbi::keys(org.Mm.eg.db, keytype = "ENTREZID")
             },
             r_norvegicus = {
               patt <- "^[0-9]+$"
               matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
               if (!all(matches)) {
                 notnum <- which(!matches)
                 stop("\nError, check formatting:  The following elements are not formatted as rat Entrez IDs (org.Rn.eg.db)\n",
                      notnum)
               }
               species_check <- AnnotationDbi::keys(org.Rn.eg.db, keytype = "ENTREZID")
             },
             c_elegans = {
               patt <- "^[0-9]+$"
               matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
               if (!all(matches)) {
                 notnum <- which(!matches)
                 stop("\nError, check formatting:  The following elements are not formatted as nematode Entrez IDs (org.Ce.eg.db)\n",
                      notnum)
               }
               species_check <- AnnotationDbi::keys(org.Ce.eg.db, keytype = "ENTREZID")
             },
             d_melanogaster = {
               patt <- "^[0-9]+$"
               matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
               if (!all(matches)) {
                 notnum <- which(!matches)
                 stop("\nError, check formatting: The following elements are not formatted as fly Entrez IDs (org.Dm.eg.db)\n",
                      notnum)
               }
               species_check <- AnnotationDbi::keys(org.Dm.eg.db, keytype = "ENTREZID")
             },
             d_rerio = {
               patt <- "^[0-9]+$"
               matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
               if (!all(matches)) {
                 notnum <- which(!matches)
                 stop("\nError, check formatting:  The following elements are not formatted as zebrafish Entrez IDs (org.Dr.eg.db)\n",
                      notnum)
               }
               species_check <- AnnotationDbi::keys(org.Dr.eg.db, keytype = "ENTREZID")
             })

         },
         Symbol = {
           switch(as.character(species),
                   h_sapiens = {
                     patt <- "^[A-Z0-9-]*$"
                     matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                     if (!all(matches)) {
                       not_matches <- which(!matches)
                       stop("\nError, check formatting: The following elements are not formatted as human symbols (org.Hs.eg.db)\n",
                            not_matches)
                     }
                     species_check <- AnnotationDbi::keys(org.Hs.eg.db, keytype = "SYMBOL")
                   },
                   m_musculus = {
                     patt <- "^[A-Z][a-z0-9-]*$"
                     matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                     if (!all(matches)) {
                       not_matches <- which(!matches)
                       stop("\nError, check formatting: The following elements are not formatted as mouse symbols (org.Mm.eg.db)\n",
                            not_matches)
                     }
                     species_check <- AnnotationDbi::keys(org.Mm.eg.db, keytype = "SYMBOL")
                   },
                   r_norvegicus = {
                     patt <- "^[A-Z][a-z0-9-]*$"
                     matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                     if (!all(matches)) {
                       not_matches <- which(!matches)
                       stop("\nError, check formatting: The following elements are not formatted as rat symbols (org.Rn.eg.db)\n",
                            not_matches)
                     }
                     species_check <- AnnotationDbi::keys(org.Rn.eg.db, keytype = "SYMBOL")
                   },
                   c_elegans = {
                     patt <- "^[a-z0-9-]*$"
                     matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                     if (!all(matches)) {
                       not_matches <- which(!matches)
                       stop("\nError, check formatting: The following elements are not formatted as nematode symbols (org.Ce.eg.db)\n",
                            not_matches)
                     }
                     species_check <- AnnotationDbi::keys(org.Ce.eg.db, keytype = "SYMBOL")
                   },
                   d_melanogaster = {
                     patt <- "^[A-Za-z0-9-]*$"
                     matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                     if (!all(matches)) {
                       not_matches <- which(!matches)
                       stop("\nError, check formatting: The following elements are not formatted as fly symbols (org.Dm.eg.db)\n",
                            not_matches)
                     }
                     species_check <- AnnotationDbi::keys(org.Dm.eg.db, keytype = "SYMBOL")
                   },
                   d_rerio = {
                     patt <- "^[a-z0-9-]*$"
                     matches <- purrr::map_lgl(ids, ~ grepl(patt, .x))
                     if (!all(matches)) {
                       not_matches <- which(!matches)
                       stop("\nError, check formatting: The following elements are not formatted as zebrafish symbols (org.Dr.eg.db)\n",
                            not_matches)
                     }
                     species_check <- AnnotationDbi::keys(org.Dr.eg.db, keytype = "SYMBOL")
                   })
         })
  
  checked_ids <- data.frame(inputs = ids, 
                            found = ids %in% species_check)
  return(checked_ids)
}

