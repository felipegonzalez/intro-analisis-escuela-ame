
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