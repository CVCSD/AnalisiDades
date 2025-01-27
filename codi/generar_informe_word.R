
# ' Parametre path fitxer a generar informe
# ' path_informe=here::here("outputs","informe7.docx")

# ' Parametre path destí on salvar l'informe docx
# ' path_informe=here::here("outputs","informe7.docx")

generar_informe_word<-function(path_arxiu="2_Analisi.Rmd",path_informe=here::here("outputs","informe7.docx")) {
  
  # path_arxiu=here::here("codi","2_Analisi.Rmd")
  # path_informe=here::here("outputs","informepp.docx")
  
  # Función para capturar objectes  tipus "ggplot2","kableExtra", "table1", "descrTable" 
  # del entorno global y almacenarlos en una lista
  
  # Captura de titul de l'infore Rmarkdown de l'arxiu en questió YAml
  titul_heading1<-rmarkdown::yaml_front_matter(path_arxiu)$title
  author_heading1<-rmarkdown::yaml_front_matter(path_arxiu)$author
  subtitul_heading1<-rmarkdown::yaml_front_matter(path_arxiu)$subtitle
 
  # Captura diferents tipus d'objectes i posar-los en llista
  
  # Exemple plots
  plots_en_llista <- capturar_objectes_entorn_en_lista(c("gg","ggplot"))
  
  # Taules tipo kable kableExtra
  kables_en_llista <-capturar_objectes_entorn_en_lista(tipo=c("kableExtra"))
  
  # Taules tipo table1
  table1_en_llista <-capturar_objectes_entorn_en_lista(tipo=c("table1"))
  
  # Taules tipo comparegroups 
  Compare_grups_en_llista <-capturar_objectes_entorn_en_lista(tipo=c("descrTable","createTable"))
  
  # S'hauria de posar tots els objectes en una llista 
  # Obrir document 
  # Colocar logo en capçalera i text
  # Colocar tots els objectes en el word
  
  
  # funció que afegeix llistat de plots en un document
  add_plots_to_doc <- function(doc=report_doc, plots_list) {
    purrr::walk(plots_list, ~ officer::body_add_gg(doc, .x))
    return(doc)}
  
  # funció que afegeix llistat de tables1 en un document doc
  add_tables1_to_doc <- function(doc=report_doc, tables1_list=table1_en_llista){
    flextable_list<-tables1_list %>% 
      purrr::map(~ table1::t1flex(.x,tablefn = c("qflextable", "flextable", "regulartable"))) 
    
    if (!dir.exists(here::here("temp"))) {dir.create(here::here("temp"))} # c
    
    llistat_noms<-c()
    for (i in seq_along(flextable_list)) {
      # i<-1
      nom_fitxer<-here::here("temp",paste0("temp",i,".docx"))
      officer::read_docx() %>% flextable::body_add_flextable(flextable_list[[i]]) %>% # officer::body_add_par("") %>%
        print(nom_fitxer)
      
      llistat_noms<-c(llistat_noms,nom_fitxer)}
    
    # Afegir-ho al doc creat
    purrr::walk(llistat_noms, ~ officer::body_add_docx(doc, .x, pos = "after"))
    
    # doc %>% officer::body_add_docx("temp1.docx") %>% officer::body_add_docx("temp2.docx") %>% 
    #   print("pelludo_tables1.docx")
    
    # llistat_noms %>% purrr::walk( ~ file.remove(.x))
    # sub("\\.docx$", ".Rmd", llistat_noms) %>% purrr::walk( ~ file.remove(.x))
    
    return(doc)}
  
  
  # funció que afegeix llistat de taules tipus Comparegrups "DescrTable" en un document
  add_compares_to_doc <- function(doc=report_doc, compare_list=Compare_grups_en_llista) {
    
    if (!dir.exists(here::here("temp"))) {dir.create(here::here("temp"))} # c
    
    llistat_noms<-c()
    
    for (i in seq_along(Compare_grups_en_llista)) {
      # i<-1
      nom_fitxer<-here::here("temp",paste0("tempo",i,".docx"))
      Compare_grups_en_llista[[i]] %>% compareGroups::export2word(nom_fitxer)
      here::here("temp",paste0("tempo",i,".Rmd")) %>% file.remove()  # Eliminar pq no peti
      
      llistat_noms<-c(llistat_noms,nom_fitxer)}
    
    purrr::walk(llistat_noms, ~ officer::body_add_docx(doc, .x))
    
    # # Eliminar temporals o carpeta temporal
    # llistat_noms %>% purrr::walk( ~ file.remove(.x))
    # sub("\\.docx$", ".Rmd", llistat_noms) %>% purrr::walk( ~ file.remove(.x))
    
    return(doc)
  }
  
  # encabezado <- "Encabezado de página"
  # Agregar el encabezado al documento (en todas las páginas)
  # doc <- doc %>%
  #   officer::header_add(even_page_header = encabezado, odd_page_header = encabezado)
  
  # Capssalera 
  report_doc<-
    officer::read_docx() %>% 
    officer::body_add_img(src = here::here("codi/logos_css/logo_santpau.png"),height = 1, width = 2) %>% 
    officer::body_add_par(titul_heading1, style = "heading 1") %>% 
    officer::body_add_par(subtitul_heading1, style = "heading 1") %>% 
    officer::body_add_par(author_heading1, style = "Normal") 
    # officer::body_add_caption(author_heading1) %>% 
    # officer::body_add(officer::block_caption("CAPTION"))
  
  
  
  # Incloure plots + table1 + compare_grups
  report_doc %>% 
    add_plots_to_doc(plots_en_llista) %>% 
    # officer::body_add_break() %>% 
    add_tables1_to_doc(table1_en_llista) %>% 
    officer::body_add_break() %>% 
    add_compares_to_doc(Compare_grups_en_llista) %>% 
    print(path_informe)


}

# Donar una classe d'objecte capturarlos de l'entorn

capturar_objectes_entorn_en_lista <- function(tipo = c("gg","ggplot")) {
  
  objetos_globales <- ls(envir = .GlobalEnv)  # Obtener los nombres de los objetos en el entorno global
  
  # Filtrar y capturar plots ggplot2 en una lista
  lista_plots <- 
    lapply(objetos_globales, function(objeto) {
      objeto_actual <- get(objeto)
      class_object <- class(objeto_actual)
      if ((class_object %in% tipo ) %>% any()) {return(objeto_actual)}
    })
  
  # Eliminar elementos nulos (objetos que no son plots ggplot2)
  lista_plots <- lista_plots[!sapply(lista_plots, is.null)]
  
  return(lista_plots)  # Devolver la lista con plots ggplot2
}

