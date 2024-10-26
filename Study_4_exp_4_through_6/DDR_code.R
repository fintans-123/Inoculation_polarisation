#NLP code
library(fastrtext)
library(quanteda)
library(caret)

#load model
model_eng <- load_model("cc.en.300.bin")

affect_pol <- c("people", "shit", "fuck", "let", "tell", "fucking", "damn", "makes", "wrong", "crazy", "stupid", "kill", "lie", "truth", "annoying", "literally", "dumb", "death", "bs", "telling", "bullshit", "lies", "totally", "absolutely", "worse", "killed", "fear", "killing", "caught", "gun", "attack", "lying", "completely", "except", "idiot", "blame", "shooting", "fault", "ridiculous", "islam", "horror", "kills", "devil", "awful", "insane", "beneran", "muslim", "guns", "liars", "murder", "crime", "rape", "strike", "destroy", "terror", "enemy", "idiots", "criminal", "dumbass", "unreal", "violence", "enemies", "unbelievable", "diaries", "claims", "rumor", "assholes", "shocking", "charged", "attacks", "broadcast", "muslims", "threat", "revealed", "charges", "exposed", "accused", "weapon", "terrorist", "attacked", "weapons", "fraud", "theft", "corruption", "christians", "expose", "blaming", "crimes", "hoax", "bombing", "scam", "terrorists", "threats", "moron", "islamic", "scheme", "bullets", "suspense", "terrorism", "jews", "cheaters", "hypocrites", "hindu", "threatening", "rifle", "criminals", "exposing", "isis", "jihad", "genocide", "morons", "dumbasses", "scams", "buddhist", "firearms", "hindus", "sharia", "extremists", "extremist", "extremism", "scammers")

#mean vec for affective polarization
aff_pol_mean <- fastrtext::get_word_vectors(model = model_eng,
                                            words = affect_pol) %>% 
  as_tibble() %>% 
  summarise(across(.cols = everything(), .fns = mean))


#create vector for each text

vec_it <- function(x){
  fastrtext::get_word_vectors(model = model_eng,
                              words = as.character(x)) |> 
    as_tibble()
}


#tokenize and vectorize
behav_vect <- dt_analysis |>
  select(prolific_id, group, text) |> 
  distinct() |> 
  group_by(prolific_id) |> 
  tidytext::unnest_tokens(word, text, token = "words") |> 
  summarise(vec_it(word))

#summarize
behav_vect_summed <- behav_vect |> 
  group_by(prolific_id) |> 
  summarise(across(.cols = everything(), .fns = mean))

#create similarity score
sim_aff <- word2vec::word2vec_similarity(x = as.matrix(aff_pol_mean),
                                         y = as.matrix(behav_vect_summed[,2:301]),
                                         type = "cosine") %>% 
  t()
