library(tidyverse)
library(plotly)


df <- jsonlite::fromJSON("https://relativio-jccm.rhcloud.com/api/data")


tokens_with_multiple_links <- df %>% 
	# valid links & tokens
	filter(
		grepl("^http://ruletka.se/", link),
		grepl("^_relativio_", token)
	) %>% 
	# remove common part
	mutate(
		link = stringr::str_match(link, "^http://ruletka.se/(.*)")[, 2],
		token = stringr::str_match(token, "^_relativio_(.*)")[, 2]
	) %>% 
	# groups links by token
	group_by(token) %>% 
	summarise(
		links = paste(unique(link), collapse = ", "),
		n = n_distinct(link)
	) %>% 
	# remove tokens with one link
	filter(n > 1)


# string with concatenated links -> cross-reference matrix
str2xref <- function(s) expand.grid(
		A = strsplit(s, ", ")[[1]],
		B = strsplit(s, ", ")[[1]],
		stringsAsFactors = FALSE
	) %>% 
		filter(A != B) %>% 
		mutate(n = 1)


xref <- lapply(tokens_with_multiple_links$links, str2xref) %>% 
	Reduce(function(x, y) full_join(x, y, by = c("A", "B")), .) %>%
	# http://stackoverflow.com/a/33315448/17216
	do({
		.$total <- rowSums(select(., -A, -B), na.rm = TRUE)
		.
	}) %>% 
	select(A, B, n = total)


# sanity check
intersect(
	df %>% 
		filter(
			grepl("^http://ruletka.se/", link),
			grepl("^_relativio_", token)
		) %>% 
		mutate(
			link = stringr::str_match(link, "^http://ruletka.se/(.*)")[, 2],
			token = stringr::str_match(token, "^_relativio_(.*)")[, 2]
		) %>% 
		filter(link == "ads/trebuyutsya-stroiteli/") %>% 
		.$token,
	df %>% 
		filter(
			grepl("^http://ruletka.se/", link),
			grepl("^_relativio_", token)
		) %>% 
		mutate(
			link = stringr::str_match(link, "^http://ruletka.se/(.*)")[, 2],
			token = stringr::str_match(token, "^_relativio_(.*)")[, 2]
		) %>% 
		filter(link == "ads/predlagayu-interesnuyu-rabotu-pochti-v-kazhdom-shvedskom-gorode/") %>% 
		.$token
)


# old stuff ----
df %>% 
	mutate(
		timestamp = as.POSIXct(
			substr(timestamp, 1, 19),
			format = "%Y-%m-%dT%H:%M:%S"
		)
	) %>% 
	plot_ly(width = 800) %>% 
	add_histogram(
		color = I("black"),
		x = ~timestamp
	) %>% 
	layout(
		title = sprintf(
			"Всего %s посещений",
			format(nrow(df), big.mark = ",")
		),
		xaxis = list(
			dtick = 86400000,
			title = ""
		)
	)


df %>% 
	filter(grepl("^http://ruletka.se/", link)) %>% 
	count(link) %>% 
	plot_ly() %>% 
	add_bars(
		hoverinfo = "text",
		marker = list(
			color = "black",
			line = list(color = "white", width = .5)
		),
		text = ~stringr::str_match(link, "^http://ruletka.se/(.*)")[, 2],
		x = ~n,
		y = 1
	) %>% 
	layout(
		bargap = 0,
		barmode = "stack",
		hovermode = "closest",
		title = sprintf(
			"%.1f%% ссылок с одним посещением",
			df %>% 
				filter(grepl("^http://ruletka.se/", link)) %>% 
				count(link) %>% 
				count(n) %>% 
				summarise(nn[n == 1] / sum(nn)) %>% 
				.[[1]] * 100
		),
		xaxis = list(title = "Посещений")
	)


df %>% 
	filter(grepl("^_relativio_", token)) %>% 
	count(token) %>% 
	plot_ly() %>% 
	add_bars(
		hoverinfo = "text",
		marker = list(
			color = "black",
			line = list(color = "white", width = .5)
		),
		text = ~stringr::str_match(token, "^_relativio_(.*)")[, 2],
		x = ~n,
		y = 1
	) %>% 
	layout(
		bargap = 0,
		barmode = "stack",
		hovermode = "closest",
		title = sprintf(
			"%.1f%% токенов с одним посещением",
			df %>% 
				filter(grepl("^_relativio_", token)) %>% 
				count(token) %>% 
				count(n) %>% 
				summarise(nn[n == 1] / sum(nn)) %>% 
				.[[1]] * 100
		),
		xaxis = list(
			dtick = 1,
			title = "Посещений"
		)
	)
