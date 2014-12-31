requirements <- c('ggplot2')
installed_pckgs <- .packages(all=T)

for (package in requirements) {
    print(package)
    if (!(package %in% installed_pckgs)) install.packages(package)
}