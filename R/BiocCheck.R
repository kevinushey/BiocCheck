.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.BiocCheckFromCommandLine <- function()
{
    option_list <- list(
        make_option("--no-check-vignettes", action="store_false",
            help="disable vignette checks"),
        make_option("--new-package", action="store_false",
            help="enable checks specific to new packages")
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
    d <- list(...)
    if (length(d))
        dots <- list(...)[[1]]
    else
        dots <- list()

    if (length(package)==0)
        .stop("Supply a package directory or source tarball.")
    package_dir <- .get_package_dir(package)
    package_name <- .get_package_name(package)

    handleMessage("Installing package...")
    installAndLoad(package)


    ## checks
    if (is.null(dots[["no-check-vignettes"]]))
    {
        handleMessage("Checking vignette directories...")
        checkVignetteDir(package_dir)
    }
    handleMessage("Checking version number...")
    if (!is.null(dots[["new-package"]]))
    {
        handleMessage("Checking new package version number...")
        checkNewPackageVersionNumber(package_dir)
    }
    checkVersionNumber(package_dir, !is.null(dots[["new-package"]]))
    handleMessage("Checking biocViews...")
    checkBiocViews(package_dir)
    handleMessage("Checking build system compatibility...")
    checkBBScompatibility(package_dir)
    handleMessage("Checking unit tests...")
    checkUnitTests(package_dir)
    handleMessage("Checking native routine registration...")
    checkRegistrationOfEntryPoints(package_name)
    handleMessage("Checking for namespace import suggestions...")
    checkImportSuggestions(package_name)
    ## Summary
    .msg("Summary:")
    .msg("Number of notes: %s", num_notes$get())
    .msg("Number of warnings: %s", num_warnings$get())

}

.get_package_name <- function(input)
{
    strsplit(basename(input), "_")[[1]][1]
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

    expectedPackageName <- strsplit(basename(pkgname), "_")[[1]][1]
    t = tempdir()
    untar(pkgname, exdir=t)
    file.path(t, expectedPackageName)
}