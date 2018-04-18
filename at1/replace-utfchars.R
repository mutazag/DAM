# clean file 


list.files(pattern = ".txt")
filenames = list.files(pattern = ".txt")
string <- readLines(file("./Doc34.txt") )

pp <- "[\x96|\x85]"
utflist <- c(utf8::as_utf8("\x96"),
             utf8::as_utf8("\x85"),
             utf8::as_utf8("\x86")) 
ppUtf <- paste0("[",paste(utflist, collapse = "|"),"]")

utfS <- enc2utf8(string)
gsub(pattern = pp, replacement = "<<MAG>>", utfS)
gsub(pattern = utf8::as_utf8("\x96"), replacement = "<<UTF>>", utfS)
gsub(pattern = utf8::as_utf8("\x85"), replacement = "<<UTF>>", utfS)
gsub(pattern = pp, replacement = "<<MAG>>", string)


# ## another approach to ensure utf8 docs
# install.packages('enc2utf8')
# library(enc2utf8)
# news.p <- tm_map(news, function(x) iconv(enc2utf8(x), sub = "byte")) 

     