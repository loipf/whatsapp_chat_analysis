############################################
### whatsapp chat analysis

### also check:https://github.com/SinghalHarsh/WhatsApp-Chat-Analysis
### https://medium.com/analytics-vidhya/whatsapp-chat-analyze-visualize-68e4d30be729
### https://cran.r-project.org/web/packages/rwhatsapp/vignettes/Text_Analysis_using_WhatsApp_data.html
### https://github.com/programmingboy/Whasapp-Data-Analysis/blob/master/Code%20Whatsapp%20Analysis.R
### https://towardsdatascience.com/the-romantic-side-of-data-science-analyzing-a-relationship-through-a-year-worth-of-text-messages-be7e32d81fa9

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))   ### only in RStudio

install.packages("pacman")

pacman::p_load(dplyr, ggplot2, rwhatsapp, lubridate, tidyr, stringr,
               ggimage, tidytext, stopwords, gtools, ComplexHeatmap, gridExtra, tools)


CHAT_HISTORY_FILE = "WhatsApp Chat mit Markus.txt"

OUTPUT_FILE = "whatsapp_chat_summary.pdf"


############################################
### global variables

### libre office color palette
COLOR_PALETTE = c('#004586','#ff420e','#ffd320','#579d1c','#7e0021','#83caff','#314004','#aecf00','#4b1f6f','#ff950e','#c5000b','#0084d1')
options(ggplot2.discrete.colour= COLOR_PALETTE, ggplot2.discrete.fill = COLOR_PALETTE)
theme_set(theme_classic())

### RColorBrewer::brewer.pal(n = 9, name = "Blues")
# HEATMAP_COLORS = c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#08519C","#08306B")  ### Blues
HEATMAP_COLORS = c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D") ### Reds

### table theme
PLOT_TABLE_THEME <- ttheme_default(base_size = 10)

WEEKDAY_ORDER = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")

# LANGUAGE_SETTING = 'en'
LANGUAGE_SETTING = 'de'

PHRASE_MEDIA_OMITTED = switch(LANGUAGE_SETTING,
                              "en" = "<Media omitted>",
                              "de" = "<Medien ausgeschlossen>")

PHRASE_MESSAGE_DELETED = switch(LANGUAGE_SETTING,
                                       "en" = c("This message was deleted.", "You deleted this message."),
                                       "de" = c("Diese Nachricht wurde gelöscht.", "Du hast diese Nachricht gelöscht.") )

PHRASE_CHANGED_SUBJECT = switch(LANGUAGE_SETTING,
                                    "en" = "Changed the subject",
                                    "de" = "")

############################################
### read in and preprocessing

chat <- rwa_read(CHAT_HISTORY_FILE) %>% 
  filter(!is.na(author)) %>% # remove messages without author
  filter(text != PHRASE_CHANGED_SUBJECT) %>%
  mutate(day = date(time))


chat$text_without_emoji = sapply(1:nrow(chat), function(curr_row) {
  gsub(paste0("(",paste0(chat$emoji[curr_row][[1]], collapse="|"),")"), "",chat$text[curr_row])
})

chat$words_in_text = sapply(chat$text_without_emoji, function(curr_text) {
  length(strsplit(curr_text, "\\s")[[1]])
}, simplify = T, USE.NAMES = F)



pdf(OUTPUT_FILE, width=8, height=4.5)

############################################
### overview table

conversation_start = min(as.Date(chat$time))
conversation_end = max(as.Date(chat$time))
conversation_duration_days = as.numeric(difftime(conversation_end, conversation_start, units="days"))

conversation_days = unique(as.Date(chat$time))
date_range <- seq(conversation_start, conversation_end, by = 1) 
conversation_missing_days = length(date_range[!date_range %in% conversation_days])

total_authors = length(unique(chat$author))
total_num_messages = table(chat$author)
total_send_media = chat %>% filter(text == PHRASE_MEDIA_OMITTED ) %>% select(author) %>% table()

total_deleted_messages = chat %>% filter(text %in% PHRASE_MESSAGE_DELETED ) %>% select(author) %>% table()
mean_messages_per_day = round(total_num_messages/conversation_duration_days,2)

max_message_per_day_df = chat %>% group_by(author) %>% count(day) %>% slice(which.max(n)) %>% ungroup() %>% as.data.frame()
max_message_per_day = setNames(max_message_per_day_df$n,  max_message_per_day_df$author)

words_per_message_df = chat %>% group_by(author) %>% dplyr::summarize(mean_words_per_message = mean(words_in_text, na.rm=TRUE)) %>% as.data.frame()
words_per_message = round(setNames(words_per_message_df$mean_words_per_message,  words_per_message_df$author),2)


### print tables in document
plot_table_1 = data.frame("values"=c(as.character(conversation_start), as.character(conversation_end), conversation_duration_days,conversation_missing_days,total_authors),
                          row.names = c("conversation start", "conversation_end", "duration days", "days without message","total authors") )

plot_table_2 = data.frame(rbind(total_num_messages,total_deleted_messages,mean_messages_per_day,max_message_per_day,words_per_message, total_send_media), check.names = F)
rownames(plot_table_2) = gsub("_"," ", rownames(plot_table_2))

plot_table_obj_1 = tableGrob(plot_table_1, theme = PLOT_TABLE_THEME, cols=NULL)
plot_table_obj_2 = tableGrob(plot_table_2, theme = PLOT_TABLE_THEME)
grid.arrange(plot_table_obj_1,plot_table_obj_2,
             top = textGrob(tools::file_path_sans_ext(basename(CHAT_HISTORY_FILE)),gp=gpar(fontsize=20,font=3)) )


############################################
### plots

chat %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("days") +
  ggtitle("messages per day")


chat %>%
  mutate(month = paste0(year(day),"-",month(day))) %>%
  count(month) %>% mutate(month = as.Date(paste0(a$month,"-01"))) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("months") +
  ggtitle("message timeline")


chat %>%
  mutate(year = year(time)) %>%
  count(year) %>% slice(gtools::mixedorder(year)) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("years") +
  ggtitle("messages per year")


chat %>%
  mutate(weekday = factor(wday(time, label=T), levels=WEEKDAY_ORDER)) %>%
  count(weekday) %>% #slice(gtools::mixedorder(year)) %>%
  ggplot(aes(x = weekday, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("weekdays") +
  ggtitle("messages per day of week")


chat %>%
  mutate(hour = hour(time)) %>%
  count(hour) %>% #slice(gtools::mixedorder(year)) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("time [hour]") +
  ggtitle("messages per hour of day")


chat %>%
  mutate(hour = hour(time)) %>%
  group_by(author) %>%
  count(hour) %>% #slice(gtools::mixedorder(year)) %>%
  ggplot(aes(x = hour, y = n, fill=author, colour=author)) +
  geom_bar(stat = "identity", position="dodge") +
  # geom_line() +
  ylab("# messages") + xlab("time [hour]") +
  ggtitle("messages per hour of day")




# colors = {'Late Night': "#003367",
#   'Early Morning':"#F3CD05",
#   'Morning': "#F49F05",
#   'Noon':"#F18904",
#   'Eve': "#36688D",
#   'Night':"#28497a"}


heatmap_df_hour_weekday = chat %>%
  mutate(hour = hour(time), weekday = factor(wday(time, label=T), levels=WEEKDAY_ORDER)) %>%
  count(hour, weekday) %>% spread(hour, n) %>% as.data.frame()
rownames(heatmap_df_hour_weekday) = heatmap_df_hour_weekday$weekday
heatmap_df_hour_weekday$weekday = NULL

Heatmap(heatmap_df_hour_weekday, cluster_rows = F, cluster_columns = F,
        column_title = "number of messages per day and hour",
        rect_gp = gpar(col = "white", lwd = 0.5),
        na_col = "white",
        heatmap_legend_param = list(title="# messages"),
        col = HEATMAP_COLORS )



### first message of the day
chat %>%
  mutate(date = date(time), hour = hour(time)) %>%
  filter(hour > 4) %>%
  distinct(date, .keep_all= TRUE) %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_col() +
  ylab("# messages") + xlab("") +
  coord_flip() +
  ggtitle("first message of the day (after 4am)")


### first message time
chat %>%
  mutate(date = date(time), hour = hour(time)) %>%
  group_by(author) %>%
  distinct(date, .keep_all= TRUE) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n, colour=author)) +
  geom_line() +
  ylab("# messages") + xlab("time [hours]") +
  ggtitle("hour of first message of the day")


# ### emoji usage per time
# chat %>%
#   mutate(date = date(time), hour = hour(time), emoji_length = length(emoji_name)) %>%
#   group_by(author)
#   
# a



### percentage smileys per time



# # pdf("emoji_usage.pdf")
# # cairo_pdf("emoji_usage.pdf")
# chat %>%
#   unnest(emoji) %>%
#   count(author, emoji, sort = TRUE) %>%
#   group_by(author) %>%
#   top_n(n = 6, n) %>%
#   ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
#   geom_col(show.legend = FALSE) +
#   ylab("") +
#   xlab("") +
#   coord_flip() +
#   facet_wrap(~author, ncol = 2, scales = "free_y")  +
#   ggtitle("Most often used emojis")
# # dev.off()


emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s.*", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/", 
                            tolower(hex_runes1), ".png"))

chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 15, n) %>%
  left_join(emoji_data, by = "emoji") %>% 
  # ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
  ggplot(aes(x = reorder_within(emoji, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  xlab("") +
  ylab("# emoji") +
  coord_flip() +
  geom_image(aes(y = n + 20, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("most often used emojis") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


chat %>%
  unnest(emoji) %>%
  count(author, emoji, sort = TRUE) %>%
  group_by(author) %>%
  # top_n(n = 25, n) %>%
  top_n(n = 15, n) %>%
  mutate(n_perc = (n/sum(n))*100) %>%
  left_join(emoji_data, by = "emoji") %>% 
  ggplot(aes(x = reorder_within(emoji, n_perc, author), y = n_perc, fill = author)) +
  geom_col(show.legend = FALSE) +
  xlab("") +
  ylab("# emoji [%]") +
  coord_flip() +
  geom_image(aes(y = n_perc + 1, image = emoji_url)) +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  ggtitle("most often used emojis [in %]") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())




############################################
### text analysis


to_remove <- c(stopwords(language = "de"),
               "media",
               "omitted",
               "medien",
               "ausgeschlossen")

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  count(author, word, sort = TRUE) %>%
  group_by(author) %>%
  top_n(n = 20, n) %>%
  ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("# words used") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("most often used words")

chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  select(word, author) %>%
  filter(!word %in% to_remove) %>%
  # mutate(word = gsub("^gag", "9gag", word)) %>%
  count(author, word, sort = TRUE) %>%
  bind_tf_idf(term = word, document = author, n = n) %>%
  group_by(author) %>%
  top_n(n = 15, tf_idf) %>%
  ggplot(aes(x = reorder_within(word, tf_idf, author), y = n, fill = author)) +
  geom_col(show.legend = FALSE) +
  ylab("# occurence of words") +
  xlab("") +
  coord_flip() +
  facet_wrap(~author, ncol = 2, scales = "free_y") +
  scale_x_reordered() +
  ggtitle("most important words by author [sorted by tf-idf]")



chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% to_remove) %>%
  group_by(author) %>%
  summarise(lex_diversity = n_distinct(word)) %>%
  arrange(desc(lex_diversity)) %>%
  ggplot(aes(x = reorder(author, lex_diversity),
             y = lex_diversity)) +
  geom_col(show.legend = FALSE) +
  # scale_y_continuous(expand = (mult = c(0, 0, 0, 2000))) +
  # geom_text(aes(label = scales::comma(lex_diversity)), hjust = -0.1) +
  ylab("# unique words") +
  xlab("") +
  ggtitle("lexical diversity") +
  coord_flip()



dev.off()



