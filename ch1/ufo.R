library(ggplot2)
library(plyr)
library(scales)

us_states <- c('ak', 'al', 'ar', 'az', 'ca', 'co', 'ct', 'de', 'fl', 'ga',
               'hi', 'ia', 'id', 'il', 'in', 'ks', 'ky', 'la', 'ma', 'md',
               'me', 'mi', 'mn', 'mo', 'ms', 'mt', 'nc', 'nd', 'ne', 'nh',
               'nj', 'nm', 'nv', 'ny', 'oh', 'ok', 'or', 'pa', 'ri', 'sc',
               'sd', 'tn', 'tx', 'ut', 'va', 'vt', 'wa', 'wi', 'wv', 'wy')

load_ufo_dataset <- function() {
    ###########################################################################
    # constants:
    file_name <- 'ufo_awesome.tsv'
    file_path <- file.path(getwd(), 'ch1', 'data', file_name)
    url_str <- 'https://raw.githubusercontent.com/johnmyleswhite/ML_for_Hackers/master/01-Introduction/data/ufo/ufo_awesome.tsv'
    headers <- c('DateOccurred', 'DateReported', 'Location',
                 'ShortDescription', 'Duration', 'LongDescription')
    
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

last_decades <- function(ufo_data) {
    ld_idxs <- ufo_data[, 1] > as.Date("1990-01-01")
    ufo_data[ld_idxs, ]
}

#' @param ufo_data A data frame created by the load_ufo_dataset function
#' @param output_file_path A string containing the file path where to store the histogram image
#' @param only_last_decades Boolean indicating whether to consider the whole data, or only sightings after 1990-01-01
print_histogram <- function(ufo_data, output_file_path=NULL, only_last_decades=F) {
    if (only_last_decades) {
        ufo_data <- last_decades(ufo_data)
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


print_season_histogram <- function(ufo_data, output_file_path=NULL, only_last_decades=F) {
    if (only_last_decades) {
        ufo_data <- last_decades(ufo_data)
    }
    
    ###########################################################################
    # prepare data
    
    message('Creating year-month column...')
    # add year-month column to data frame
    ufo_data$YearMonth <- strftime(ufo_data[, 1], '%Y-%m')
    message('-Done-')
    
    message('Creating sighting counts...')
    # '.': Quote variables to create a list of unevaluated expressions for later evaluation.'
    # ddply: Split data frame, apply function, and return results in a data frame.
    #                       <data>    <where/how to split>  <fn>
    sighting_cnts <- ddply(ufo_data, .(USState, YearMonth), nrow)

    # minor cleaning since there may be a row like this:
    # Browse[2]> tail(sighting_cnts, 1)
    #      USState YearMonth V1
    # 8343    <NA>      <NA>  1
    good_idxs <- !is.na(sighting_cnts$USState) & !is.na(sighting_cnts$YearMonth)
    sighting_cnts <- sighting_cnts[good_idxs, ]
    message('-Done-')
    
    # PROBLEM: sighting_cnts does not contain months where no sightings were
    # reported!
    # STRATEGY:
    # 1) create a dataframe for *all* states and dates without gaps
    # 2) merge the created dataframe with the sighting_cnts dataframe
    # 3) add zero counts where no data is given
    
    ## 1)
    message('Creating gapless states-dates dataframe...')
    date_range <- seq.Date(
            from=as.Date(min(ufo_data$DateOccurred, na.rm=T)),
            to=as.Date(max(ufo_data$DateOccurred, na.rm=T)),
            by='month'
        )
    date_strings <- strftime(date_range, '%Y-%m')
    
    # list w/ one entry for every state, which looks sth like this:
    # [[6]]
    #        s    date_strings
    #   [1,] "co" "1990-01"
    #   [2,] "co" "1990-02"
    #   [3,] "co" "1990-03"
    #   [4,] "co" "1990-04"
    #   [5,] "co" "1990-05"
    #   [6,] "co" "1990-06"
    #   ...
    states__dates <- lapply(us_states, function(s) cbind(s, date_strings))
    
    #                           | 2-col matrix looking like |
    #                           |      s    date_strings    |
    #                           | [1,] "ak" "1990-01"       |
    #                           | [2,] "ak" "1990-02"       |
    #                           | [3,] "ak" "1990-03"       |
    #                           | [4,] "ak" "1990-04"       |
    #                           | ...                       |
    states__dates <- data.frame(do.call(rbind, states__dates), stringsAsFactors=F)
    message('-Done-')
    
    ## 2)
    message('Merging gapless states-dates dataframe with input data')
    # Browse[1]> str(states__dates)
    # 'data.frame':    12400 obs. of  2 variables:
    #  $ s           : chr  "ak" "ak" "ak" "ak" ...
    #  $ date_strings: chr  "1990-01" "1990-02" "1990-03" "1990-04" ...
    #
    # Browse[1]> str(sighting_cnts)
    # 'data.frame':    8343 obs. of  3 variables:
    #  $ USState  : Factor w/ 3289 levels ""," eastern cape) (south africa)",..: 66 66 66 66 66 66 66 66 66 66 ...
    #  $ YearMonth: chr  "1990-01" "1990-03" "1990-05" "1993-11" ...
    #  $ V1       : int  1 1 1 1 1 1 1 1 1 1 ...
    all_sightings <- merge(
            states__dates, sighting_cnts,
            by.x=c('s', 'date_strings'), by.y=c('USState', 'YearMonth'),
            all=T
        )
    names(all_sightings) <- c('State', 'YearMonth', 'Sightings')
    message('-Done-')

    ## 3) replace NA's with 0
    message('Further data cleaning...')
    all_sightings$Sightings[is.na(all_sightings$Sightings)] <- 0
    
    # replace the YearMonth strings with actual dates from date_range (which
    # contains one date per month, e.g. "1990-01-03" "1990-02-03" "1990-03-03"
    # ...)
    #                                  | repeat all the date_range      |
    #                                  | dates for each us state        |
    all_sightings$YearMonth <- as.Date(rep(date_range, length(us_states)))
    
    # convert (upper case) state abbreviations to factors
    all_sightings$State <- as.factor(toupper(all_sightings$State))
    message('-Done-')
    
    ###########################################################################
    # plot
    message('Creating plot...')
    # base plot object
    state_plot <- ggplot(all_sightings, aes(x=YearMonth, y=Sightings))
    # add layer with line showing the number of sightings
    state_plot <- state_plot + geom_line(aes(colour='darkblue'))
    # create a facet plot, i.e. one panel for each state, arranged in 10 rows
    # and 5 columns
    state_plot <- state_plot + facet_wrap(~State, nrow=10, ncol=5)
    # change theme to black/white (i.e. no gray background)
    state_plot <- state_plot + theme_bw()
    # specify that the string 'darkblue' corresponds to the web-safe color
    # 'darkblue' (ggplot2 requires explicit definitions of details such as
    # color)
    state_plot <- state_plot + scale_color_manual(
                                    values=c('darkblue'='darkblue'),
                                    guide='none')
    # scale the major breaks and adjust the labels
    state_plot <- state_plot + scale_x_date(breaks='5 years', labels=date_format('%Y'))
    # add x and y axis labels
    state_plot <- state_plot + xlab('Time') + ylab('Number of Sightings')
    # add a plot title
    state_plot <- state_plot + labs(title=paste('Numnberof UFO sightings by',
                                                 'month-mear and U.S. state'))
    
    if (!is.null(output_file_path)) {
        ggsave(plot=state_plot, filename=output_file_path, width=14, height=8.5)
    }
    
    print(state_plot)
    message('-Done-')
}