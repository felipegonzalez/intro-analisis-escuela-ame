
## Sección 1

formatear_tabla <- function(x_tbl, scroll = FALSE){
    tabla <- knitr::kable(x_tbl) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 15, fixed_thead = TRUE) 
    if(scroll) tabla <- tabla %>% scroll_box(width = "780px") 
    tabla 
}

cuantil <- function(x, probs = c(0,0.25, 0.5, 0.75,1), ...){
    x_quo <- enquo(x)
    valores <- quantile(x, probs = probs, names = FALSE, ...)
    cuantil_nom <- probs
    tibble(cuantil = cuantil_nom, valor = valores) 
}

grafica_cuantiles <- function(datos, grupo, valor){
    if(!(".sample" %in% names(datos))){
        datos$.sample <- 1
    }
    
    cuantiles_tbl <- datos %>% group_by({{ grupo }}, .sample) %>% 
        summarise(
            num = n(),
            cuantiles = list(cuantil({{ valor }}, c(0.1, 0.25, 0.5, 0.75, 0.9)))) %>% 
        unnest 
    
    grafica <- ggplot(cuantiles_tbl  %>% spread(cuantil, valor), 
        aes(x = {{ grupo }}, y = `0.5`)) +
        geom_linerange(aes(ymin= `0.1`, ymax = `0.9`), colour = "gray40") +
        geom_linerange(aes(ymin= `0.25`, ymax = `0.75`), size = 2, colour = "gray") 
        geom_point(colour = "salmon", size = 2) 
    grafica
}