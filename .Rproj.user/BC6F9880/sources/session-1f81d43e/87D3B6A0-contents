if(!exists("showme")) showme = T
library(targets)
library(bookdown) 
render_book( "presentation", gitbook(split_by = "section", self_contained = FALSE, config = list(sharing = NULL, toc = list(collapse = "section"))) )
##render_book( "apmma-imputed-with-mice-analysis-plan.Rmd", html_document2(toc = TRUE, number_sections = T))
##render_book( "apmma-imputed-with-mice-analysis-plan.Rmd", word_document2(toc = TRUE, number_sections = T))
#file.rename("_main.html", "apmma-imputed-with-mice-analysis-plan.html")

if(showme){
browseURL("presentation/_book/index.html")
}
showme = F
