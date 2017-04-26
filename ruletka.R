library(tidyverse)
library(rvest)


options(stringsAsFactors = FALSE, encoding = "UTF-8")


a <- read_html("http://ruletka.se/2017/04/20/kak-ne-otravitsya-v-stokgolmskom-restorane/") %>% 
	html_nodes("a")


df <- cbind(
	as.data.frame(sapply(c("id", "class", "href", "title"), function(x) a %>% html_attr(x))),
	as.data.frame(sapply(c("class", "src"), function(x) a %>% html_node("img") %>% html_attr(x))) %>% 
		rename(img.class = class)
) %>% 
	filter(!is.na(src))
