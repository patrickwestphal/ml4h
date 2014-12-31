load_ufo_dataset <- function() {
    file_name <- 'ufo_awesome.tsv'
    file_path <- file.path(getwd(), 'ch1', 'data', file_name)
    url_str <- 'https://raw.githubusercontent.com/johnmyleswhite/ML_for_Hackers/master/01-Introduction/data/ufo/ufo_awesome.tsv'
    
    if (!file.exists(file_path)) {
        download.file(url_str, destfile=file_path, method='curl', quiet=T)
    }
    read.delim(file_path, stringsAsFactors=F, header=F, na.strings='')
}