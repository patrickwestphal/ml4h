# install.packages('tmp')
library('tm')
library('ggplot2')

#' function to load the e-mail body
#' 
#' @param path: the file path to the e-mail file to load
read.email.body <- function(path) {
    #                         v-- open for reading in text mode
    conn <- file(path, open='rt', encoding='latin1')
    email.lines <- readLines(conn)
    close(conn)
    
    first.linebreak <- which(email.lines == '')[1]
    # all lines after the first linebreak --> email body
    body.lines <- email.lines[seq(first.linebreak, length(email.lines), 1)]
    
    return(paste(body.lines, collapse='\n'))
}

#' counts the occurences of a given word in a file given by path
#' 
#' @param file.path: a string containing the path to the file to investigate
#' @param word: a string containing the word to count
count.word <- function(file.path, word) {
    content <- read.email.body(file.path)
    #          .- a collection of documents containing (natural language) text
    #          |      .-A vector source interprets each element of the vector x 
    #          |      | as a document. 
    corpus <- Corpus(VectorSource(content))
    control <- list(stopwords=T, removePunctuation=T, removeNumbers=T)
    
    #                                            Docs
    # Terms                                       1
    #   access                                    1
    #   affordable                                1
    #   aligndcenterbfont                         1
    #   aligndcenterbrp                           1
    #   aligndleftbrbbrbrbrbrp                    1
    #   aligndleftbrbbrbrfontif                   1
    #   aligndleftfont                            1
    #   aligndleftp                               1
    #   aligndleftpfontuibbrfontubui              1
    #   aligndmiddle                              1
    #   andor                                     1
    #   best                                      3
    #   black                                     1
    #   body                                      1
    #   bold                                      3
    #   bordercolord                              3
    #   borderd                                   3
    #   brbrbrfontpcentercentertrtbodytablecenter 1
    #   buying                                    1
    #   calypso                                   4
    #   ...
    tdm <- TermDocumentMatrix(corpus, control)
    # > typeof(word.freqs)
    # [1] "double"
    # > names(word.freqs)
    # [1] "access"                                    "affordable"
    # [3] "aligndcenterbfont"                         "aligndcenterbrp"
    # [5] "aligndleftbrbbrbrbrbrp"                    "aligndleftbrbbrbrfontif"
    # [7] "aligndleftfont"                            "aligndleftp"
    # [9] "aligndleftpfontuibbrfontubui"              "aligndmiddle"
    # ...
    word.freqs <- rowSums(as.matrix(tdm))
    # position 77 (but there might be multiple)
    word.cols <- which(names(word.freqs) == word)
    # > word.count
    # html 
    #    2 
    word.count <- word.freqs[word.cols]
    
    return(ifelse(length(word.count)>0, word.count, 0))
}


spam.path <- file.path('ch3', 'data', 'spam')
spam.docs <- dir(spam.path)

spam.file.blacklist <- c('cmds', '0000.7b1b73cf36cf9dbc3d64e3f2ee2b91f1')
for (filename in spam.file.blacklist) {
    # all docs but the one named <filemane>
    spam.docs <- spam.docs[which(spam.docs != filename)]
}

# counts of the word 'html' for each doc in spam.docs
# (names(spam.html.counts) == spam.docs)
spam.html.counts <- sapply(
    spam.docs,
    function(path) count.word(file.path(spam.path, path), 'html'))
# counts of the word 'table' for each doc in spam.docs
# (names(spam.table.counts) == spam.docs)
spam.table.counts <- sapply(
    spam.docs,
    function(path) count.word(file.path(spam.path, path), 'table'))

# > str(spam.stats)
# chr [1:1000, 1:3] "2" "0" "0" "0" "0" "0" "0" "2" "0" "2" "2" "0" "0" "0" ...
# - attr(*, "dimnames")=List of 2
# ..$ : chr [1:1000] "00001.7848dde101aa985090474a91ec93fcf0" ...
# ..$ : chr [1:3] "spam.html.counts" "spam.table.counts" ""
#
# > spam.stats[[1,1]]
# [1] "2"
# > spam.stats[[1,2]]
# [1] "4"
# > spam.stats[[1,3]]
# [1] "SPAM"
spam.stats <- cbind(spam.html.counts, spam.table.counts, 'SPAM')

easyham.path <- file.path('ch3', 'data', 'easy_ham')
easyham.docs <- dir(easyham.path)

easyham.file.blacklist <- c('cmds')
for (filename in easyham.file.blacklist) {
    easyham.docs <- easyham.docs[which(easyham.docs != filename)]
}

easyham.html.counts <- sapply(
    easyham.docs,
    function(path) count.word(file.path(easyham.path, path), 'html'))
easyham.table.counts <- sapply(
    easyham.docs,
    function(path) count.word(file.path(easyham.path, path), 'table'))
# > str(easyham.stats)
# chr [1:5051, 1:3] "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" "0" ...
# - attr(*, "dimnames")=List of 2
# ..$ : chr [1:5051] "00001.7c53336b37003a9286aba55d2945844c" ...
# ..$ : chr [1:3] "easyham.html.counts" "easyham.table.counts" ""
easyham.stats <- cbind(easyham.html.counts, easyham.table.counts, 'EASYHAM')

# > str(email.stats.df)
# 'data.frame':    6051 obs. of  3 variables:
#  $ spam.html.counts : chr  "2" "0" "0" "0" ...
#  $ spam.table.counts: chr  "4" "0" "0" "0" ...
#  $ V3               : chr  "SPAM" "SPAM" "SPAM" "SPAM" ...
#
# 6051 = 1000 + 5051
email.stats.df <- data.frame(
    rbind(spam.stats, easyham.stats),
    stringsAsFactors=F)
names(email.stats.df) <- c('html', 'table', 'type')
email.stats.df$html <- as.numeric(email.stats.df$html)
email.stats.df$table <- as.numeric(email.stats.df$table)
email.stats.df$type <- as.factor(email.stats.df$type)

stats.plot <- ggplot(email.stats.df, aes(x=html, y=table)) +
    geom_point(aes(shape=type)) +
    scale_shape_manual(values=c('SPAM'=1, 'EASYHAM'=3), name='Email Type') +
    xlab('frequency of HTML') +
    ylab('frequency of table') + 
    theme_bw()

print(stats.plot)