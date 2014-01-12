.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.BiocCheckFromCommandLine <- function()
{
    option_list <- list(
#        make_option(c("-n", "--add_numbers"), action="store_true", default=FALSE,
#        help="Print line number at the beginning of each line [default]")
        )
    parser <- OptionParser(usage = "%prog [options] package", option_list=option_list)
    arguments <- parse_args(parser, positional_arguments = 1)
    opt <- arguments$options
    file <- arguments$args

    opt$Called_from_command_line <- TRUE
    BiocCheck(file, opt)
}

BiocCheck <- function(package, ...)
{
    dots = list(...)
    if (length(package)==0)
        .stop("Supply a package directory or source tarball.")
    package_dir <- .get_package_dir(package)

    # checks
    handleMessage("Checking vignette directories...")
    checkVignetteDir(package_dir)
    handleMessage("Checking version number...")
    checkVersionNumber(package_dir)
}

.get_package_dir <- function(pkgname)
{
    if (!file.exists(pkgname))
    {
        stop(.printf("'%s' does not exist!", pkgname))
    }
    if (file.info(pkgname)$isdir)
        return(pkgname)

    if(!grepl("\\.tar\\.gz$", pkgname))
    {
        stop(.printf("'%s' is not a directory or package source tarball.",
            pkgname))
    }

    t = tempdir()
    res = untar(pkgname, exdir=t)
    

}