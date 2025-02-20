### Generació d'informe


rmarkdown::render("codi/1_Lectura_MHEART.Rmd", 
                  output_file = here::here("outputs",paste0("Informe_estadistic",Sys.Date())))



rmarkdown::render("codi/2_Analisi.2.Rmd", 
                  output_file = here::here("outputs",paste0("Informe_estadistic_analisi",Sys.Date())), output_format = "html_document")



rmarkdown::render("codi/2_Analisi.2.Rmd", 
                  output_file = here::here("outputs",paste0("Informe_estadistic_analisi",Sys.Date())),   output_format = "pdf_document")


rmarkdown::render("codi/2_Analisi.2.Rmd", 
                  output_file = here::here("outputs",paste0("Informe_estadistic_analisi",Sys.Date())),   output_format = "word_document")


##  script que mogui el report a la caperta corresponent ###



rmarkdown::render("codi/2_Analisi.2.Rmd", 
                  output_file =  paste0("Informe_estadistic_analisi",Sys.Date()),   output_format = "pdf_document")

source("codi/render_pdf.R")

