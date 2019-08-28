library(formattable)
library(knitr)
## Sección 1

formatear_tabla <- function(x_tbl, scroll = FALSE){
    tabla <- kable(x_tbl) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                      full_width = FALSE, font_size = 15, fixed_thead = TRUE) 
    
    if(scroll) tabla <- tabla %>% scroll_box(width = "780px") 
    tabla 
}
