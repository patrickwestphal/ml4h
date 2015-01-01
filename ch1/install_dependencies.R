requirements <- c('ggplot2', 'devtools', 'plyr', 'scales')
installed_pckgs <- .packages(all=T)

for (package in requirements) {
    print(package)
    if (!(package %in% installed_pckgs)) install.packages(package)
}