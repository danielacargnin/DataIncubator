
#################
# Web Scrapping #
#################

library(rvest)

ptm <- proc.time()

# URLs

url <- "http://allrecipes.com/recipes/main.aspx?Page="

ingredients_base  <- data.frame()
category_base <- data.frame()
nutrition_base <- data.frame()
review_base <-data.frame()

recipe_base <- matrix(NA, ncol=6, nrow=1)

id <-0

for (i in 0:300) {
  
  recipe <- html((paste(url, i, sep="")))
  
  recipe_list <- html_attr(html_nodes(recipe, "#recipes~ .grid-view .title"), "href")
  
  for (j in 1:20) {
    id <- id + 1
    recipe_name <-unlist(strsplit(recipe_list[j], "/"))[3]
    url_recipe <- html(paste("http://allrecipes.com/Recipe/", recipe_name, "/Detail.aspx?", sep=""))  
    
    # Ready in Time
    
    prep_time <- html_text(html_nodes(url_recipe, ".time , #lblTimeValuePlus , #pnlReadyInTime .emp-orange"))[1]
    
    # Recipe directions
    
      directions <- paste(html_text(html_nodes(url_recipe, ".break")), collapse=" $$$ ")
    
    # Recipe stars
    
    stars <- round(as.numeric(html_attr(html_node(html_nodes(url_recipe, ".rating-stars-img"), "meta"), "content")[1]),2)
  
    qty_reviews <- as.numeric(html_text(html_nodes(url_recipe, "#btnScrollToReview span")))

    recipe_base <-matrix(c(id, recipe_name, prep_time, stars, qty_reviews, directions ), nrow=1)

    write.table(recipe_base, append=TRUE, file = "recipe_base_final.csv", col.names = FALSE, sep =";")
    

    ## Recipe's Categories
    
    category <- html_text(html_nodes(url_recipe, "#breadcrumbs span"))
    category_base <- data.frame(id, category)
    
    write.table(category_base, append=TRUE, file = "category_base_final.csv", sep =";", col.names = FALSE)
    
    
    ## Ingredients
    
    ingredients <- html_text(html_nodes(url_recipe, "#lblIngName"))
    qtygrams <- round(as.numeric(html_attr(html_nodes(url_recipe, ".ingredient-wrap #liIngredient"), "data-grams")),1)
    ingredientid <- as.numeric(html_attr(html_nodes(url_recipe, ".ingredient-wrap #liIngredient"), "data-ingredientid"))
  
    if (identical(ingredients, character(0))== FALSE) {
    ingredients_base <-data.frame(id, ingredientid, ingredients, qtygrams)
    write.table(ingredients_base, append=TRUE, file = "ingredients_base_final.csv", sep =";", col.names = FALSE)
    }
    
    
    
    # Informação Nutricional
    
    nutrients <- html_text(html_nodes(url_recipe, ".categories"))
    units <- html_text(html_nodes(url_recipe, ".units"))
    percentage <- html_text(html_nodes(url_recipe, ".percentages"))
    
    if (identical(nutrients, character(0))== FALSE) {
    nutrition_base <- data.frame(id, nutrients, units, percentage)
    write.table(nutrition_base, append=TRUE, file = "nutrition_base_final.csv", sep =";", col.names = FALSE)
    }
    
    ## Reviews

        max_rev <- 10
      
      for (k in 1:max_rev) {
        url_review <- paste("http://allrecipes.com/Recipe/", recipe_name, "/reviews.aspx?Page=", sep="")    
        reviews <- html((paste(url_review, k, sep="")))

        # Stars
        review_stars <- html_attr(html_node(html_nodes(reviews, ".rating-stars-img")
                                 , "meta"), "content")
        if (identical(review_stars, character(0))== FALSE) {        
        review_date <- gsub("\r\n                            ", "", 
                 html_text(html_nodes(reviews, ".date")))
    
       # Comentários
 
        comments <- gsub("   ","", chartr("\n\r", "   ",html_text(html_nodes(reviews, ".listItemReviewFull"))))
      
          review_base <-data.frame(id, review_stars, review_date, comments)
        write.table(review_base, append=TRUE, file = "review_base_final.csv", sep =";", col.names = FALSE)
        }        
      }
    
  }
}

proc.time() - ptm

  

