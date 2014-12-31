library(ggplot2)

load_ufo_dataset <- function() {
    ###########################################################################
    # constants:
    file_name <- 'ufo_awesome.tsv'
    file_path <- file.path(getwd(), 'ch1', 'data', file_name)
    url_str <- 'https://raw.githubusercontent.com/johnmyleswhite/ML_for_Hackers/master/01-Introduction/data/ufo/ufo_awesome.tsv'
    headers <- c('DateOccurred', 'DateReported', 'Location',
                 'ShortDescription', 'Duration', 'LongDescription')
    us_states <- c('ak', 'al', 'ar', 'az', 'ca', 'co', 'ct', 'de', 'fl', 'ga',
                   'hi', 'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md',
                   'me', 'mi', 'mn', 'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh',
                   'nj', 'nm', 'nv', 'ny', 'oh', 'ok', 'or', 'pa', 'ri', 'sc',
                   'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa', 'wi', 'wv', 'wy')
    
    ###########################################################################
    # read file:
    message('Reading file...')
    if (!file.exists(file_path)) {
        download.file(url_str, destfile=file_path, method='curl', quiet=T)
    }
    ufo_data <- read.delim(file_path, stringsAsFactors=F, header=F,
                           na.strings='', colClasses='character')

    names(ufo_data) <- headers
    message('-Done-')
    
    ###########################################################################
    # data cleansing:
    # 1) First and second column should contain an eight chararcter string
    #    representing a date
    message('Cleaning data...')
    good_idxs <- which(nchar(ufo_data[, 1]) == 8 & nchar(ufo_data[, 2]) == 8)
    ufo_data <- ufo_data[good_idxs, ]
    message('-Done-')
    
    ###########################################################################
    # datatype conversions:
    message('Doing data conversions...')
    ## DateOccurred, e.g. '19950420', '19950115', '19940801'
    ufo_data[, 1] <- as.Date(ufo_data[, 1], '%Y%m%d')
    
    ## DateReported, e.g. '19951009', '19950510', '19951024'
    ufo_data[, 2] <- as.Date(ufo_data[, 2], '%Y%m%d')
    message('-Done-')
    
    ###########################################################################
    # data extraction:
    message('Doing data extractions...')
    ## split location column (e.g. 'Iowa City, IA', 'Milwaukee, WI', ...) into
    ## separate columns for the city and state
    city__state <- lapply(ufo_data[, 3], get_location)
    city__state_mtrx <- do.call(rbind, city__state)
    
    ufo_data <- transform(
            ufo_data[c(1, 2, 4, 5, 6)],
            USCity=city__state_mtrx[, 1],
            USState=tolower(city__state_mtrx[, 2]))
    
    ## would also work, but needs col names update:
    # ufo_data <- cbind(ufo_data[c(1, 2)], city__state_mtrx, ufo_data[c(4, 5, 6)])
    message('-Done-')
    
    ###########################################################################
    # data filtering: only consider data from US states
    message('Filtering data...')
    us_states_idxs <- ufo_data[, 7] %in% us_states
    ufo_data <- ufo_data[us_states_idxs, ]
    message('-Done-')

    ufo_data
}

#' @param location_str A string containing a city and a state, e.g. Brunswick County, ND
#' @return A vector containing the city and state as separate items
get_location <- function(location_str) {
    # split on ',' / return (NA, NA) in case no ',' is present
    split_location <- tryCatch(
            strsplit(location_str, ',')[[1]],
            error=function(e) {
                return(c(NA, NA))
            }
        )
    
    # remove leading whitespace
    clean_location <- gsub('^ ', '', split_location)
    
    # only return results that have two parts, (NA, NA) otherwise
    if (length(clean_location)>2) {
        return(c(NA, NA))
    } else {
        return(clean_location)
    }
}

#' @param ufo_data A data frame created by the load_ufo_dataset function
#' @param output_file_path A string containing the file path where to store the histogram image
#' @param only_last_decades Boolean indicating whether to consider the whole data, or only sightings after 1990-01-01
print_histogram <- function(ufo_data, output_file_path=NULL, only_last_decades=F) {
    if (only_last_decades) {
        ld_idxs <- ufo_data[, 1] > as.Date("1990-01-01")
        ufo_data <- ufo_data[ld_idxs, ]
    }
    
    #       basic layer w/ DateOccurred as X vals   histogram layer
    hist <- ggplot(ufo_data, aes(x=DateOccurred)) + geom_histogram()
    
    if (!only_last_decades) {
        # scale the X axis labels to occur every 50 years
        hist <- hist + scale_x_date(breaks='100 years')
    }
    
    if (!is.null(output_file_path)) {
        ggsave(plot=hist, filename=output_file_path, height=6, width=8)
    }
    print(hist)
}