#######################
#### Load packages ####
#######################
install.packages("syuzhet")
library(syuzhet)
library(vader)
library(stringr)
library(dplyr)

path_to_load<-"../data/Subtitles/"
path_to_save<-"../data/scene_valence/"
file_names=list.files(path = path_to_load,recursive = TRUE)
i<-1

###########################
##### Build function ######
###########################
replace_curly <- function(x, ...){
  replaces <- c('\x91', '\x92', '\x93', '\x94')
  Encoding(replaces) <- "latin1"
  for (i in 1:4) {
    x <- gsub(replaces[i], c("'", "'", "\"", "\"")[i], x, fixed = TRUE)
  }
  x
}

get_scene <- function(text_of_file, n_words, n_chunks, fix_curly_quotes = TRUE, as_vector = TRUE){
  if (!is.character(text_of_file)) stop("Data must be a character vector.")
  if (isTRUE(fix_curly_quotes)) text_of_file <- replace_curly(text_of_file)
  splits <- textshape::split_portion(text_of_file, n.words = n_words, 
                                     n.chunks = n_chunks, as.string = TRUE, as.table = FALSE)
  if (isTRUE(as_vector)) splits <- unlist(splits)
  splits
}


get_vader_values <- function(char_v){
  result <- get_vader(char_v)["compound"]
}

get_sent <- function(char_v, path_to_tagger = NULL, cl=NULL, language = "english", lexicon = NULL, regex = "[^A-Za-z']+", lowercase = TRUE){
  if(lowercase == TRUE){
    char_v <- tolower(char_v)
    # remove links
    char_v = str_remove_all(char_v, "https\\S*")
    char_v = str_remove_all(char_v, "http\\S*")
    char_v = str_remove_all(char_v, "t.co*")
    # remove mentions
    char_v = str_remove_all(char_v, "@\\S*")
    # remove annoying html stuff
    char_v = str_remove_all(char_v, "amp")
    char_v = str_remove_all(char_v, "&S*♪-")
    char_v = str_replace_all(char_v, "&#x27;|&quot;|&#x2F;", "'")
    char_v = str_replace_all(char_v, "<a(.*?)>", " ")
    char_v = str_replace_all(char_v, "&gt;|&lt;|&amp;", " ")
    char_v = str_replace_all(char_v, "&#[:digit:]+;", " ")
    char_v = str_remove_all(char_v, "<[^>]*>")
    # remove numbers
    char_v = str_remove_all(char_v, "[:digit:]")
    # remove excess whitespace
    char_v = str_squish(char_v)
    char_v = str_trim(char_v)
  }
  
  language <- tolower(language)
  if(!is.character(char_v)) stop("Data must be a character vector.")
  if(!is.null(cl) && !inherits(cl, 'cluster')) stop("Invalid Cluster")
  if(language %in% tolower(c("Arabic", "Bengali", "Chinese_simplified", "Chinese_traditional", "Greek", "Gujarati", "Hebrew", "Hindi", "Japanese", "Marathi", "Persian", "Russian", "Tamil", "Telugu", "Thai", "Ukranian", "Urdu", "Yiddish"))) stop ("Sorry, your language choice is not yet supported.")
  if(is.null(cl)){
    result <- unlist(lapply(char_v, get_vader_values))
  }
  else {
    result <- unlist(parallel::parLapply(cl=cl, char_v, get_vader_values))
  }
}

###########################
##### Text analysis #######
###########################
for (file in file_names) {
  print(i)
  i <- (i+ 1)
  load_file_path<-paste(path_to_load,file,sep="")
  save_file_path<-paste(path_to_save,file,sep="")
  text<- get_text_as_string(load_file_path)
  scene<- get_scene(text, n_words = 1000, n_chunks = 50)
  sentiment <- as.numeric(get_sent(scene))
  write.table(sentiment, save_file_path, row.names = FALSE, col.names=FALSE, sep = "\t")
}

# Try to combine all txt files
install.packages("data.table")
library(data.table)

filenames <-dir("../data/scene_valence/")

dt_long <- rbindlist(setNames(lapply(paste("../data/scene_valence/",filenames, sep=""), fread), paste("Subtitles/",filenames, sep="")), idcol = "Subtitles")

## Reshape to wide format
dt_wide <- dcast(dt_long, rowid(Subtitles) ~ Subtitles, value.var = "V1")


# Calculate scene volatility

dt_wide <- dt_wide[, lapply(.SD, function(x) (x-shift(x))[-1]), .SDcols = 2:526]
dt_wide <- dt_wide[, lapply(.SD, function(x) (x-mean(x))), .SDcols = 1:525]
dt_wide <- dt_wide[, lapply(.SD, function(x) (sqrt(sum(x^2)/50))), .SDcols = 1:525]

dt_long <- melt(dt_wide, variable.name = "Subtitles")
setnames(dt_long, "value", "scene_volatility")

# Export data
write.csv(dt_long,"../data/scene_volatility.csv", row.names = FALSE)







