############################################
### whatsapp chat analysis

### also check:https://github.com/SinghalHarsh/WhatsApp-Chat-Analysis
### https://medium.com/analytics-vidhya/whatsapp-chat-analyze-visualize-68e4d30be729
### https://cran.r-project.org/web/packages/rwhatsapp/vignettes/Text_Analysis_using_WhatsApp_data.html
### https://github.com/programmingboy/Whasapp-Data-Analysis/blob/master/Code%20Whatsapp%20Analysis.R
### https://towardsdatascience.com/the-romantic-side-of-data-science-analyzing-a-relationship-through-a-year-worth-of-text-messages-be7e32d81fa9

# setwd(dirname(rstudioapi::getSourceEditorContext()$path))   ### only in RStudio

# install.packages("pacman")
pacman::p_load(dplyr, ggplot2, ggforce, rwhatsapp, lubridate, tidyr, stringr, cowplot,
               ggimage, tidytext, stopwords, gtools, ComplexHeatmap, gridExtra, circlize,
               wordcloud,tm, stringi, RColorBrewer, syuzhet, parallel)


# CHAT_HISTORY_FILE = "WhatsApp Chat mit Markus.txt"

OUTPUT_FILE = "whatsapp_chat_summary.pdf"

NUM_THREADS = detectCores()-1


############################################
### global variables

### libre office color palette
COLOR_PALETTE = c('#004586','#ff420e','#ffd320','#579d1c','#7e0021','#83caff','#314004','#aecf00','#4b1f6f','#ff950e','#c5000b','#0084d1')
options(ggplot2.discrete.colour= COLOR_PALETTE, ggplot2.discrete.fill = COLOR_PALETTE)
theme_set(theme_classic())

### RColorBrewer::brewer.pal(n = 9, name = "Blues")
# HEATMAP_COLORS = c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6","#4292C6","#2171B5","#08519C","#08306B")  ### Blues
# HEATMAP_COLORS = c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D") ### Reds

HEATMAP_COLORS = c("#FFF5F0", "#A70000")

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

PHRASE_OTHER_WHATSAPP = switch(LANGUAGE_SETTING,
                                    "en" = c("Changed the subject"), 
                                    "de" = c("", "Verpasster Sprachanruf") )


manual_addition_stopwords_bavaria = c("scho","schon","ned","dass","werd","nix","fur","a","wia","is","moi","oda",
                                      "no","ma","no","de","i","ja","jaa","jaaa","oba","san","hast","na","naa", "sei","do","wos",
                                      "vo","o","ko","koa","vui","hod","mi","eiso","ab","hod","hosd","mei","hoid","neda",
                                      "nein","ne","n","d","mal")
manual_addition_stopwords = c("https")

PHRASE_TO_REMOVE = c(stopwords::stopwords(language = LANGUAGE_SETTING), manual_addition_stopwords_bavaria, manual_addition_stopwords,
                     PHRASE_MEDIA_OMITTED,PHRASE_MESSAGE_DELETED,PHRASE_OTHER_WHATSAPP)


Heatmap_default = function(df, ...){
  Heatmap(df, cluster_rows = F, cluster_columns = F,
          rect_gp = gpar(col = "white", lwd = 0.2),
          na_col = "white",
          # row_names_gp = gpar(fontsize = 8),
          column_names_centered = T, 
          ...)
}

############################################
### read in and preprocessing

chat <- rwa_read(CHAT_HISTORY_FILE) %>% 
  filter(!is.na(author)) %>% # remove messages without author
  filter(!text %in% PHRASE_OTHER_WHATSAPP) %>%
  mutate(day = date(time),
         weekday = factor(wday(time, label=T), levels=WEEKDAY_ORDER),
         month = as.Date(paste0(year(day),"-",month(day),"-01")),
         hour = hour(time) )


chat$text_without_emoji = sapply(1:nrow(chat), function(curr_row) {
  gsub(paste0("(",paste0(chat$emoji[curr_row][[1]], collapse="|"),")"), "",chat$text[curr_row])
})

chat$words_in_text = sapply(chat$text_without_emoji, function(curr_text) {
  length(strsplit(curr_text, "\\s")[[1]])
}, simplify = T, USE.NAMES = F)
chat$emoji_in_text = sapply(chat$emoji, length, simplify = T, USE.NAMES = F)


pdf(OUTPUT_FILE, width=8, height=5)


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

total_send_emoji_df = chat %>% group_by(author) %>% mutate("emoji_length" = sapply(emoji, length)) %>%
  dplyr::summarize(emoji_sum = sum(emoji_length, na.rm=TRUE))  %>% as.data.frame()
total_send_emoji = setNames(total_send_emoji_df$emoji_sum,  total_send_emoji_df$author)


### print tables in document
plot_table_1 = data.frame("values"=c(as.character(conversation_start), as.character(conversation_end), conversation_duration_days,conversation_missing_days,total_authors),
                          row.names = c("conversation start", "conversation end", "duration days", "days without message","total authors") )

plot_table_2 = data.frame(rbind(total_num_messages,total_deleted_messages,mean_messages_per_day,max_message_per_day,words_per_message, total_send_media, total_send_emoji), check.names = F)
rownames(plot_table_2) = gsub("_"," ", rownames(plot_table_2))

plot_table_obj_1 = tableGrob(plot_table_1, theme = PLOT_TABLE_THEME, cols=NULL)
plot_table_obj_2 = tableGrob(plot_table_2, theme = PLOT_TABLE_THEME)
grid.arrange(plot_table_obj_1,plot_table_obj_2,
             top = textGrob(tools::file_path_sans_ext(basename(CHAT_HISTORY_FILE)),gp=gpar(fontsize=20,font=3)) )



### large author number handling
is_large_author_number = ifelse(total_authors>4, T, F)
if(is_large_author_number) {  warning("can be problematic and take a long time for more than 12 group members ") }

if(total_authors == 2) {
  PLOT_PAGE_ROWS = 1
  PLOT_PAGE_COLS = 2
} else {
  if(total_authors>2 & total_authors<=4) {
    PLOT_PAGE_ROWS = 2
    PLOT_PAGE_COLS = 2
} else {
  PLOT_PAGE_ROWS = 3
  PLOT_PAGE_COLS = 2
} }


############################################
### message amount plots

chat %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_bar(stat = "identity") +
  ylab("# messages") + xlab("") +
  coord_flip() +
  ggtitle("number of messages")


chat %>%
  count(day) %>%
  ggplot(aes(x = day, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("days") +
  ggtitle("messages per day")


chat %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("months") +
  ggtitle("message timeline")


chat %>%
  mutate(year = year(time)) %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("years") +
  ggtitle("messages per year")


chat %>%
  count(weekday) %>%
  ggplot(aes(x = weekday, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("weekdays") +
  ggtitle("messages per day of week")

### maybe replace with boxplot
# chat %>%
#   mutate(weekday = factor(wday(time, label=T), levels=WEEKDAY_ORDER)) %>%
#   group_by(author) %>%
#   count(weekday) %>%
#   ggplot(aes(x = weekday, y = n, fill=author, colour=author)) +
#   geom_bar(stat = "identity", position="dodge") +
#   # geom_line() +
#   ylab("# messages") + xlab("time [hour]") +
#   ggtitle("messages per hour of day")


chat %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n)) +
  geom_col() +
  ylab("# messages") + xlab("time [hour]") +
  ggtitle("messages per hour of day")


chat %>%
  group_by(author) %>%
  count(hour) %>%
  ggplot(aes(x = hour, y = n, fill=author, colour=author)) +
  geom_bar(stat = "identity", position="dodge") +
  # geom_line() +
  ylab("# messages") + xlab("time [hour]") +
  ggtitle("messages per hour of day")


chat %>% 
  group_by(author, hour) %>% 
  dplyr::summarize(mean_words_per_message = mean(words_in_text, na.rm=TRUE)) %>% 
  ggplot(aes(x = hour, y = mean_words_per_message, fill=author, colour=author)) +
  geom_bar(stat = "identity", position="dodge") +
  ylab("mean # of words") + xlab("time [hour]") +
  ggtitle("words per message over the day")


chat %>% 
  group_by(author, hour) %>% 
  dplyr::summarize(mean_emoji_per_message = mean(emoji_in_text, na.rm=TRUE)) %>% 
  ggplot(aes(x = hour, y = mean_emoji_per_message, fill=author, colour=author)) +
  geom_bar(stat = "identity", position="dodge") +
  ylab("mean # of emoji") + xlab("time [hour]") +
  ggtitle("emoji per message over the day")



heatmap_df_hour_weekday = chat %>%
  count(hour, weekday) %>% spread(hour, n) %>% as.data.frame()
rownames(heatmap_df_hour_weekday) = heatmap_df_hour_weekday$weekday
heatmap_df_hour_weekday$weekday = NULL

Heatmap_default(heatmap_df_hour_weekday,
                column_title = "number of messages per day and hour",
                heatmap_legend_param = list(title="# messages"),
                column_names_rot=0,
                col = HEATMAP_COLORS)


if(total_authors == 2) {
  ### max for heatmap legend
  heatmap_df_hour_weekday_author_max =  chat  %>% group_by(author) %>%
    count(hour, weekday) %>% slice_max(n, with_ties=F) %>% ungroup()
  heatmap_df_hour_weekday_author_max = max(heatmap_df_hour_weekday_author_max$n)

  heatmap_df_hour_weekday_list = lapply(1:2, function(author_num){
    author_name = unique(chat$author)[author_num]
    heatmap_df_hour_weekday_author =  subset(chat, author==author_name) %>%
      count(hour, weekday) %>% spread(hour, n) %>% as.data.frame()
    rownames(heatmap_df_hour_weekday_author) = heatmap_df_hour_weekday_author$weekday
    heatmap_df_hour_weekday_author$weekday = NULL

    Heatmap_default(heatmap_df_hour_weekday_author,
                    column_title = author_name,
                    heatmap_legend_param = list(title="# messages"),
                    column_names_gp = gpar(fontsize = 8),
                    column_names_rot = 45,
                    col = colorRamp2(c(0, heatmap_df_hour_weekday_author_max), HEATMAP_COLORS ),
                    show_heatmap_legend = ifelse(author_num==1, T, F) )

  })
  heatmap_hour_weekday_list = heatmap_df_hour_weekday_list[[1]] + heatmap_df_hour_weekday_list[[2]]
  draw(heatmap_hour_weekday_list, column_title = "number of messages per day and hour", ht_gap = unit(1, "cm"))

}



############################################
### first message of the day

chat_first_message = chat %>%
  filter(hour > 5) %>%
  distinct(day, .keep_all= TRUE)

chat_first_message %>%
  count(author) %>%
  ggplot(aes(x = reorder(author, n), y = n)) +
  geom_col() +
  ylab("# messages") + xlab("") +
  coord_flip() +
  ggtitle("first message of the day (after 5am)")

### first message of the day over time
chat_first_message %>%
  group_by(author) %>% count(month) %>%
  ggplot(aes(x = month, y = n, colour=author)) +
  geom_line() +
  ylab("# messages") + xlab("months") +
  ggtitle("first message of the day over time (after 5am)")


### first message time
chat %>%
  group_by(author) %>%
  distinct(day, .keep_all= TRUE) %>%
  mutate(hour_quarter = format(floor_date(time, "30 minutes"), format="2020-01-01 %H:%M:%S")  ) %>%
  mutate(hour_quarter = as.POSIXct(hms::parse_hm(gsub("2020-01-01 ","", hour_quarter))) ) %>%
  count(hour_quarter)  %>%
  ggplot(aes(x = hour_quarter, y = n, colour=author)) +
  geom_line() +
  scale_x_datetime(date_labels = "%H:%M") +
  ylab("# messages") + xlab("time [30min steps]") +
  ggtitle("hour of first message of the day")



### TODO how do first message of the day look like - wordcloud


############################################
### reply analysis - only 2 person

### get only non-consecutive messages
non_consecutive_entries = sapply(1:(nrow(chat)-1), function(curr_row_num) {
  chat[curr_row_num,"author"] != chat[curr_row_num+1,"author"]
} )

chat_reply = chat[which(non_consecutive_entries),]
chat_reply$time_diff = c(0,diff(chat_reply$time)/60) # min

### keep only between 0 and 3 hours
chat_reply = subset(chat_reply, time_diff >=0 & time_diff <=180)

chat_reply %>%
  ggplot(aes(x = time_diff,colour=author)) +
  geom_density() +
  ylab("messages proportion") + xlab("delay [min]") +
  ggtitle("reply delay distribution")


### create author specific heatmaps
if(total_authors == 2) {
  reply_bins = c(-1, 2, 5, 10, 30, 60, 120, 181)
  reply_labels = c('0-2','2-5','5-10','10-30','30-60','60-120','120-180')

  heatmap_df_chat_reply = chat_reply %>% group_by(author) %>%
    mutate(time_bin = cut(time_diff, breaks=reply_bins, labels = reply_labels))  %>%
    count(hour, time_bin)
  reply_heatmap_legend_max_messages = max(heatmap_df_chat_reply$n, na.rm = T)

  reply_heatmap_list = lapply(1:2, function(author_num){
    author_name = unique(chat$author)[author_num]
    heatmap_df_chat_reply_author = subset(heatmap_df_chat_reply, author==author_name) %>%
      spread(hour, n)  %>% as.data.frame()
    rownames(heatmap_df_chat_reply_author) = heatmap_df_chat_reply_author$time_bin
    heatmap_df_chat_reply_author = heatmap_df_chat_reply_author[,-c(1,2)] # remove author + time_bin

    Heatmap_default(heatmap_df_chat_reply_author,
                    column_title = author_name,
                    row_order = rev(reply_labels),
                    column_names_gp = gpar(fontsize = 8),
                    column_names_rot = 45,
                    heatmap_legend_param = list(title="# messages"),
                    col = colorRamp2(c(0, reply_heatmap_legend_max_messages), HEATMAP_COLORS ),
                    show_heatmap_legend = ifelse(author_num==1, T, F) )

  })
  reply_heatmap_list = reply_heatmap_list[[1]] + reply_heatmap_list[[2]]
  draw(reply_heatmap_list, row_title = "delay [min]", column_title = "reply delay over the day", ht_gap = unit(1, "cm"))

}



############################################
### emoji analysis

### todo: percentage smiley usage per time

emoji_data <- rwhatsapp::emojis %>% # data built into package
  mutate(hex_runes1 = gsub("\\s.*", "", hex_runes)) %>% # ignore combined emojis
  mutate(emoji_url = paste0("https://abs.twimg.com/emoji/v2/72x72/",
                            tolower(hex_runes1), ".png"))

# chat %>%
#   unnest(emoji) %>%
#   count(author, emoji, sort = TRUE) %>%
#   group_by(author) %>%
#   top_n(n = 15, n) %>%
#   left_join(emoji_data, by = "emoji") %>%
#   # ggplot(aes(x = reorder(emoji, n), y = n, fill = author)) +
#   ggplot(aes(x = reorder_within(emoji, n, author), y = n, fill = author)) +
#   geom_col(show.legend = FALSE) +
#   xlab("") +
#   ylab("# emoji") +
#   coord_flip() +
#   geom_image(aes(y = n + 20, image = emoji_url)) +
#   facet_wrap(~author, ncol = 2, scales = "free_y") +
#   ggtitle("most often used emojis") +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())


p_list_emoji = lapply(1:floor(total_authors/(PLOT_PAGE_ROWS*PLOT_PAGE_COLS)), function(plot_page) {
  chat %>%
    unnest(emoji) %>%
    count(author, emoji, sort = TRUE) %>%
    group_by(author) %>%
    # top_n(n = 25, n) %>%
    slice_max(n, n = ifelse(is_large_author_number, 5, 15), with_ties=F) %>%
    mutate(n_perc = (n/sum(n))*100) %>%
    left_join(emoji_data, by = "emoji") %>%
    ggplot(aes(x = reorder_within(emoji, n_perc, author), y = n_perc, fill = author)) +
    geom_col(show.legend = FALSE) +
    xlab("") +
    ylab("# emoji [%]") +
    coord_flip() +
    geom_image(aes(y = n_perc + 1, image = emoji_url)) +
    facet_wrap_paginate(~author, ncol = PLOT_PAGE_COLS, nrow = PLOT_PAGE_ROWS,scales = "free_y",  page = plot_page) +
    ggtitle("most often used emojis [in %]") +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
})
print(p_list_emoji)



###TODO emoji percentage per messages over the day





############################################
### text analysis


p_list_most_often_words = lapply(1:floor(total_authors/(PLOT_PAGE_ROWS*PLOT_PAGE_COLS)), function(plot_page) {
  chat %>% filter(!text %in% PHRASE_TO_REMOVE) %>%
    unnest_tokens(input = text,
                  output = word) %>%
    filter(!word %in% PHRASE_TO_REMOVE) %>%
    count(author, word, sort = TRUE) %>%
    group_by(author) %>%
    slice_max(n, n = ifelse(is_large_author_number, 10, 20), with_ties=F) %>%
    ggplot(aes(x = reorder_within(word, n, author), y = n, fill = author)) +
    geom_col(show.legend = FALSE) +
    ylab("# words used") +
    xlab("") +
    coord_flip() +
    facet_wrap_paginate(~author, ncol = PLOT_PAGE_COLS, nrow = PLOT_PAGE_ROWS, scales = "free_y",  page = plot_page) +
    scale_x_reordered() +
    ggtitle("most often used words")
})
print(p_list_most_often_words)


p_list_tfidf_words = lapply(1:floor(total_authors/(PLOT_PAGE_ROWS*PLOT_PAGE_COLS)), function(plot_page) {
  chat %>% filter(!text %in% PHRASE_TO_REMOVE) %>%
    unnest_tokens(input = text,
                  output = word) %>%
    select(word, author) %>%
    filter(!word %in% PHRASE_TO_REMOVE) %>%
    count(author, word, sort = TRUE) %>%
    bind_tf_idf(term = word, document = author, n = n) %>%
    group_by(author) %>%
    slice_max(n, n = ifelse(is_large_author_number, 10, 20), with_ties=F) %>%
    ggplot(aes(x = reorder_within(word, tf_idf, author), y = n, fill = author)) +
    geom_col(show.legend = FALSE) +
    ylab("# occurence of words") +
    xlab("") +
    coord_flip() +
    facet_wrap_paginate(~author, ncol = PLOT_PAGE_COLS, nrow = PLOT_PAGE_ROWS, scales = "free_y",  page = plot_page) +
    scale_x_reordered() +
    ggtitle("most important words by author [sorted by tf-idf]")
})
print(p_list_tfidf_words)


chat %>%
  unnest_tokens(input = text,
                output = word) %>%
  filter(!word %in% PHRASE_TO_REMOVE) %>%
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



############################################
### sentiment analysis over time 


if(total_authors==2) {
  
  cl <- makeCluster(NUM_THREADS) # or detect_cores() - 1
  clusterExport(cl = cl, c("get_sentiment", "get_sent_values", "get_nrc_sentiment", "get_nrc_values", "parLapply"))
  
  syuzhet_language_setting = switch(LANGUAGE_SETTING,
                                    "en" = "english",
                                    "de" = "german")
  
  ### overall sentiment
  plot_sentiment_list = lapply(1:2, function(author_num){
        author_name = unique(chat$author)[author_num]
        chat_author = subset(chat, author==author_name)#[1:100,]
        chat_author = chat_author[!chat_author$text %in% PHRASE_TO_REMOVE,]
  
        chat_split_sentences = get_sentences(chat_author$text, as_vector = F)
        chat_split_sentences_time = rep(chat_author$time, sapply(chat_split_sentences, length))
        chat_split_sentences = get_sentences(chat_author$text, as_vector = T)
        
        chat_author_sentiment = get_nrc_sentiment(chat_split_sentences, cl=cl, language =syuzhet_language_setting, lowercase = TRUE)
        # chat_author_sentiment = get_nrc_sentiment(chat_split_sentences, language =syuzhet_language_setting, lowercase = TRUE)
        chat_author_sentiment$neutral = chat_author_sentiment$negative ==0 & chat_author_sentiment$positive == 0
        chat_author_sentiment$time = chat_split_sentences_time
  
        ### emotion overview      
        overall_emotion_df = data.frame("emotion_perc"=prop.table(colSums(chat_author_sentiment[,1:8])))
        overall_emotion_df$emotion = rownames(overall_emotion_df)
        overall_emotion_df$emotion_perc_label = paste0(round(overall_emotion_df$emotion_perc*100,1),"%")
        
        p1 = ggplot(overall_emotion_df, aes(x = "", y = emotion_perc, fill = emotion)) +
          geom_col() +
          geom_text(aes(label = emotion_perc_label),
                    position = position_stack(vjust = 0.5)) +
          coord_polar(theta = "y") + theme_void() +
          ggtitle(author_name) + theme(plot.title = element_text(hjust = 0.5))
        
        ### sentiment overview      
        overall_sentiment_df = data.frame("sentiment_perc"=prop.table(colSums(chat_author_sentiment[,9:11])))
        overall_sentiment_df$sentiment = rownames(overall_sentiment_df)
        overall_sentiment_df$sentiment_perc_label = paste0(round(overall_sentiment_df$sentiment_perc*100,1),"%")
        
        p2 = ggplot(overall_sentiment_df, aes(x = "", y = sentiment_perc, fill = sentiment)) +
          geom_col() + 
          geom_text(aes(label = sentiment_perc_label),
                    position = position_stack(vjust = 0.5)) +
          scale_fill_manual(values = c(COLOR_PALETTE[2],COLOR_PALETTE[8], "grey"), labels=c("negative","positive","neutral")) + 
          coord_polar(theta = "y") + theme_void() + 
          ggtitle(author_name) + theme(plot.title = element_text(hjust = 0.5))
        
        
        ### emotion overview first message of day
        chat_author_emotion_first_message = chat_author_sentiment  %>% mutate(day = date(time)) %>%
          distinct(day, .keep_all= TRUE)
        overall_emotion_first_df = data.frame("emotion_perc"=prop.table(colSums(chat_author_emotion_first_message[,1:8])))
        overall_emotion_first_df$emotion = rownames(overall_emotion_first_df)
        overall_emotion_first_df$emotion_perc_label = paste0(round(overall_emotion_first_df$emotion_perc*100,1),"%")
        
        p3 = ggplot(overall_emotion_first_df, aes(x = "", y = emotion_perc, fill = emotion)) +
          geom_col() +
          geom_text(aes(label = emotion_perc_label),
                    position = position_stack(vjust = 0.5)) +
          coord_polar(theta = "y") + theme_void() +
          ggtitle(author_name) + theme(plot.title = element_text(hjust = 0.5))
        
        
        ### sentiment overview over time  
        chat_author_time_sentiment = chat_author_sentiment %>% mutate(hour = hour(time)) %>%
          # group_by(hour) %>% select(negative, positive, neutral, hour) %>% ungroup() %>% as.data.frame() #%>% group_map(prop.table(colSums(.x[,c("negative","positive")])))
          group_by(hour) %>% select(negative, positive, hour) %>% ungroup() %>% as.data.frame() #%>% group_map(prop.table(colSums(.x[,c("negative","positive")])))
        
        time_sentiment_list = sapply(0:23, function(curr_time) {
          curr_time_df = subset(chat_author_time_sentiment, hour == curr_time)
          # prop.table(colSums(curr_time_df[,c("negative","positive")]))
          colSums(curr_time_df[,c("negative","positive")])
          # colSums(curr_time_df[,c("negative","positive", "neutral")])
        }, simplify = F, USE.NAMES = T)
        time_sentiment_df = data.frame(do.call("rbind", time_sentiment_list), "hour" = 0:23)
        time_sentiment_df = time_sentiment_df %>% gather("sentiment", "value", -hour)
        
        p4 = ggplot(time_sentiment_df, aes(x = hour, y = value, fill = sentiment)) +
          geom_col() +
          scale_fill_manual(values = c(COLOR_PALETTE[2],COLOR_PALETTE[8], "grey"), labels=c("negative","positive","neutral")) + 
          ylab("# messages") + xlab("time [hour]") +
          ggtitle(author_name) + theme(plot.title = element_text(hjust = 0.5))
        
          p1 = p1 + theme(legend.position="none")
          p2 = p2 + theme(legend.position="none")
          p3 = p3 + theme(legend.position="none")
          p4 = p4 + theme(legend.position="none")
              
        return(list(p1,p2,p3,p4))
      })
  
  
  ### add legend at the bottom from tmp data frame
  emotions_list = c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust")
  emotion_legend_tmp_data = data.frame("x_values" = c(1:length(emotions_list)), y_values=c(1:length(emotions_list)), emotion = emotions_list)
  emotion_legend_tmp_plot = ggplot(data=emotion_legend_tmp_data, aes(x=x_values, y=y_values, fill=emotion)) + geom_col() + theme_classic()+
    scale_fill_manual("emotion", breaks = emotions_list,
                        values = COLOR_PALETTE[1:8] )
  emotion_legend_tmp <- get_legend(emotion_legend_tmp_plot + guides(color = guide_legend(nrow = 1)) +
                             theme(legend.position = "bottom") )
  
  sentiment_legend_tmp_data = data.frame("x_values" = c(1:3), y_values=c(1:3), sentiment = c("negative", "positive","neutral"))
  sentiment_legend_tmp_plot = ggplot(data=sentiment_legend_tmp_data, aes(x=x_values, y=y_values, fill=sentiment)) + geom_col() + theme_classic()+
    scale_fill_manual("sentiment", breaks = c("negative", "positive","neutral"),
                        values = c(COLOR_PALETTE[2],COLOR_PALETTE[8], "grey") )
  sentiment_legend_tmp <- get_legend(sentiment_legend_tmp_plot + guides(color = guide_legend(nrow = 1)) +
                                     theme(legend.position = "bottom") )
  
  p_grid_emotion_overview = plot_grid(plot_sentiment_list[[1]][[1]], plot_sentiment_list[[2]][[1]], labels=NULL, ncol=2)
  p_grid_emotion_overview_title <- ggdraw() + draw_label("emotion in messages overview", fontface='bold')
  print(plot_grid(p_grid_emotion_overview_title, p_grid_emotion_overview, emotion_legend_tmp,  ncol=1, rel_heights=c(0.1, 0.8, 0.1)))
  
  p_grid_sentiment_overview = plot_grid(plot_sentiment_list[[1]][[2]], plot_sentiment_list[[2]][[2]], labels=NULL, ncol=2)
  p_grid_sentiment_overview_title <- ggdraw() + draw_label("sentiment in messages overview", fontface='bold')
  print(plot_grid(p_grid_sentiment_overview_title, p_grid_sentiment_overview, sentiment_legend_tmp,  ncol=1, rel_heights=c(0.1, 0.8, 0.1)))
  
  p_grid_emotion_first = plot_grid(plot_sentiment_list[[1]][[3]], plot_sentiment_list[[2]][[3]], labels=NULL, ncol=2)
  p_grid_emotion_first_title <- ggdraw() + draw_label("emotion in first message of the day", fontface='bold')
  print(plot_grid(p_grid_emotion_first_title, p_grid_emotion_first, emotion_legend_tmp,  ncol=1, rel_heights=c(0.1, 0.8, 0.1)))
  
  p_grid_sentiment_time = plot_grid(plot_sentiment_list[[1]][[4]], plot_sentiment_list[[2]][[4]], labels=NULL, ncol=2)
  p_grid_sentiment_time_title <- ggdraw() + draw_label("sentiment in messages over the day", fontface='bold')
  print(plot_grid(p_grid_sentiment_time_title, p_grid_sentiment_time, sentiment_legend_tmp,  ncol=1, rel_heights=c(0.1, 0.8, 0.1)))
}


# # method="nrc"
# # method="syuzhet"
# 
# get_sentiment(chat$text[1], language = , method="syuzhet", lowercase = TRUE)
# get_sentiment(chat$text[1], language = syuzhet_language_setting, method="nrc", lowercase = TRUE)
# get_sentiment(chat$text[1:20], language = syuzhet_language_setting, method="nrc", lowercase = TRUE)
# get_sentiment(chat$text[1:20], language = syuzhet_language_setting, method="nrc", lowercase = TRUE)
# 
# get_nrc_sentiment(chat$text[1:5], language =syuzhet_language_setting, lowercase = TRUE)




############################################
### word clouds (first message, overall)


plot_wordcloud = function(chat_text) {
  # chat_text = chat$text_without_emoji
  chat_text = chat_text[!chat_text %in% PHRASE_TO_REMOVE]
  
  corpus_all <- Corpus(VectorSource(chat_text))
  corpus_all <- corpus_all %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  corpus_all <- tm_map(corpus_all, content_transformer(tolower))
  corpus_all <- tm_map(corpus_all, content_transformer(function(x) stri_trans_general(x, id = "Latin-ASCII") )) ### remove special character
  corpus_all <- tm_map(corpus_all, removeWords, PHRASE_TO_REMOVE)
  
  ### maybe text stemming - SnowballC
  # http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
  
  ### maybe switch to more efficient textTinyR 
  ### https://www.dustinstoltz.com/blog/2020/12/1/creating-document-term-matrix-comparison-in-r
  
  dtm <- TermDocumentMatrix(corpus_all)
  terms_passing_threshold <- findFreqTerms(dtm, lowfreq = 3) 
  dtm = dtm[terms_passing_threshold,]
  
  dtm_matrix <- as.matrix(dtm)
  dtm_words <- sort(rowSums(dtm_matrix),decreasing=TRUE)
  dtm_df <- data.frame(word = names(dtm_words),freq=dtm_words)

  set.seed(123)
  wordcloud(words = dtm_df$word, freq = dtm_df$freq, min.freq = 5,
            max.words=100, random.order=FALSE, rot.per=0.35,
            colors=brewer.pal(8, "Dark2"), scale=c(4, .4), use.r.layout=F)
  
}


if(total_authors==2) {
  par(mfrow=c(1,2), mar=rep(0, 4), oma=rep(0, 4) )
  author_name1 = unique(chat$author)[1]
  plot_wordcloud(subset(chat, author==author_name1)$text_without_emoji)
  text(x=0.2, y=1, author_name1)
  author_name2 = unique(chat$author)[2]
  plot_wordcloud(subset(chat, author==author_name2)$text_without_emoji)
  text(x=0.2, y=1, author_name2)
}





dev.off()












