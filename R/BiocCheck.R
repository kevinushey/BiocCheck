
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

    package_dir <- .get_package_dir(package)

    # now start checking:

    checkVignetteDir(package_dir)

    print("errors:")
    print(check_errors$name)
    .printf("Number of errors: %s", num_errors$get())
    print("warnings:")
    print(check_warnings$name)
    .printf("Number of warnings: %s", num_warnings$get())
    .printf("Number of notes: %s", num_notes$get())

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

    tarname <- basename(pkgname)
    exact_pkgname <- strsplit(pkgname, "_", TRUE)[[1]][1]

    t = tempdir()
    ret = file.path(t, exact_pkgname)
    if (file.exists(ret))
        unlink(ret, true)

    untar(pkgname, exdir=t)
    ret
}