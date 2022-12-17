#this script doing the emotional analysis  for select scripts.by using
#i) the select movies scripts are found in '../data/Subtitles/'
#-------------------------
#the progression:
#1) The emotional arc of each movie script was calculated by exploiting the default labelled lexicon developed at the Nebraska Literary Laboratory. To that end, each script was partitioned into sentences and for each sentence the valence was calculated by assigning every word its sentimental value δ ∈ ( -1,1), where -1 referred to emotionally negative terms; and 1 referred to emotionally positive terms according to the lexico.
#2)I applies smoothing method called Discrete Cosine Transform (DCT)to smooth the trajectory.
#3) the resulting trajectory was uniformly sub-sampled to have 100 points so that each script sentiment arc could be represented using the scrpts timing from 0% (beginning of the movie) to 100% (end of the movie).
#4) save the every selected movie's 100-dimensional vectors  to '../data/normed_sentiment/'
#------------------------
install.packages("syuzhet")
library(syuzhet)
library(vader)
library(stringr)
path_to_load<-"../data/Subtitles/"
path_to_save<-"../data/normed_sentiment/"
file_names=list.files(path = path_to_load,recursive = TRUE)
i<-1

###########################
##### Build function ######
###########################

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
##### Extract emotion #####
###########################
for (file in file_names) {
  print(i)
  i <- (i+ 1)
  load_file_path<-paste(path_to_load,file,sep="")
  save_file_path<-paste(path_to_save,file,sep="")
  text<- get_text_as_string(load_file_path)
  sentence<- get_sentences(text)
  sentiment <- as.numeric(get_sent(sentence))
  normed_sentiment <- get_dct_transform(sentiment, scale_range = T, low_pass_size = 5)
  write.table(normed_sentiment,save_file_path, row.names = FALSE, col.names=FALSE, sep = "\t")
}

# For k-means cluster analysis, please run the Jupiter Notebook
