# library(ggplot2)

load_whg_data <- function() {
    ###########################################################################
    # constants
    file_name <- '01_heights_weights_genders.csv'
    file_path <- file.path(getwd(), 'ch2', 'data', file_name)
    url_str <- 'https://raw.githubusercontent.com/johnmyleswhite/ML_for_Hackers/master/05-Regression/data/01_heights_weights_genders.csv'

    ###########################################################################
    # read file
    if (!file.exists(file_path)) {
        message('Downloading file...')
        download.file(url_str, destfile=file_path, method='curl', quiet=F)
        message('-Done-')
    }

    message('Reading file...')
    # whg --> weight/height/gender
    whg_data <- read.csv(file_path)
    message('-Done-')

    whg_data
}

#' @param whg_data The weight/height/gender data frame created by the load_whg_data function
#' @param col The column to use as source; one of 'Height', 'Weight' or 'Gender'
#' @param bin_width The bin width parameter adjustable in the geom_histogram() function
plot_hist <- function(whg_data, col='Height', bin_width=1) {
    plot <- ggplot(whg_data, aes_string(x=col)) + geom_histogram(binwidth=bin_width)
    print(plot)
}

#' @param whg_data The weight/height/gender data frame created by the load_whg_data function
#' @param col The column to use as source; 'Height' or 'Weight'
#' @param bin_width The bin width parameter adjustable in the geom_histogram() function
plot_hist_by_gender <- function(whg_data, col='Height', bin_width=1) {
    plot <- ggplot(whg_data, aes_string(x=col)) +
                geom_histogram(binwidth=bin_width) +
                facet_grid(Gender ~ .)
    print(plot)
}

#' @param whg_data The weight/height/gender data frame created by the load_whg_data function
#' @param col The column to use as source; one of 'Height', 'Weight' or 'Gender'
plot_density <- function(whg_data, col='Height') {
    plot <- ggplot(whg_data, aes_string(x=col)) + geom_density()
    print(plot)
}

#' @param whg_data The weight/height/gender data frame created by the load_whg_data function
#' @param col The column to use as source; 'Height' or 'Weight'
plot_density_by_gender <- function(whg_data, col='Height') {
    plot <- ggplot(whg_data, aes_string(x=col))+
                geom_density() +
                facet_grid(Gender ~ .)
    print(plot)
}

#' @param whg_data The weight/height/gender data frame created by the load_whg_data function
plot_height_weight <- function(wh_data, num_samples=0) {
    if (num_samples == 0) {
        num_samples <- nrow(whd_data)
    }
    plot <- ggplot(whg_data[0:num_samples, ], aes(x=Height, y=Weight, color=Gender)) +
                geom_point() + geom_smooth() #+ facet_grid(Gender ~ .)

    print(plot)
}

plot_bell_curve <- function(mean=0, variance=1) {
    df <- data.frame(X=rnorm(10000, mean, variance))
    plot <- ggplot(df, aes(x=X)) + geom_density()

    print(plot)
}

plot_gamma_curve <- function(shape=1, rate=0.001) {
    df <- data.frame(X=rgamma(100000, shape, rate), shape, rate)
    ggplot(df, aes(x=X)) + geom_density()

    print(plot)
}

plot_exponential_curve <- function(rate=1) {
    df <- data.frame(X=rexp(n=100000, rate))
    plot <- ggplot(df, aes(x=X)) + geom_density()
    
    print(plot)
}