
# tb <- jsonlite::read_json('data/trivia_avina.json') 
# tb <- tb[-1]
# 
# data_trivia <- map_df(seq_along(tb), function(i) {
#   dt_g <- data.frame(choices = unlist(tb[[i]]$choices), choices_id = unlist(tb[[i]]$answer))
#   dt_g$question_id <- tb[[i]]$id 
#   dt_g$id_text <- tb[[i]]$text 
#   dt_g
# })
# write_csv(data_trivia, 'data/dic_trivia.csv')
