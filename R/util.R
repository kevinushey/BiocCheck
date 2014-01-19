.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.msg <- function(...) message(noquote(sprintf(...)))
.stop <- function(...) stop(noquote(sprintf(...)), call.=FALSE)


handleMessage <- function(msg)
{
    .msg("* %s", msg)
}

handleError <- function(msg)
{
    .errors$add(msg)
    .msg("* REQUIRED: %s", msg)
    #.stop(msg)
}

handleWarning <- function(msg)
{
    .warnings$add(msg)
    .msg("* RECOMMENDED: %s", msg)
}

handleNote <- function(msg)
{
    .notes$add(msg)
    .msg(sprintf("* NOTE: %s", msg))
}

installAndLoad <- function(pkg)
{
    libdir <- file.path(tempdir(), "lib")
    dir.create(libdir, showWarnings=FALSE)
    stderr <- file.path(tempdir(), "install.stderr")
    res <- system2(file.path(Sys.getenv("R_HOME"), "bin", "R"),
        sprintf("CMD INSTALL --no-test-load --library=%s %s", libdir, pkg),
        stdout=NULL, stderr=stderr)
    if (res != 0) 
    {
        cat(paste(readLines(stderr), collapse="\n"))
        handleError(sprintf("Failed to install %s!", pkg))

    }
    pkgname <- strsplit(basename(pkg), "_")[[1]][1]
    args <- list(package=pkgname, lib.loc=libdir)
    suppressPackageStartupMessages(do.call(library, args))
}

cleanupDependency <- function(input)
{
    output <- gsub("\\s", "", input)
    output <- gsub("\\([^)]*\\)", "", output)
    strsplit(output, ",")[[1]]
}

getAllDependencies <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    fields <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
    out <- c()
    for (field in fields)
    {   
        if (field %in% colnames(dcf))
            out <- append(out, cleanupDependency(dcf[, field]))
    }
    out
}

parseFile <- function(infile, pkgdir)
{
    if (grepl("\\.Rnw$|\\.Rmd|\\.Rrst", infile, TRUE))
    {
        dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
        if ("VignetteBuilder" %in% colnames(dcf))
        {
            outfile <- file.path(tempdir(), "parseFile.tmp")
            purl(infile, outfile, documentation=0L)
        } else {
            oldwd <- getwd()
            on.exit(setwd(oldwd))
            setwd(tempdir())
            outfile <- file.path(tempdir(), sub("\\.Rnw$", ".R", TRUE), basename(infile))
            Stangle(infile)
        }
    } else if (grepl("\\.Rd", infile, TRUE)) 
    {
        rd <- parse_Rd(infile)
        outfile <- file.path(tempdir(), "parseFile.tmp")
        code <- Rd2ex(rd)
        cat(code, file=outfile)
    } else if (grepl("\\.R", infile, TRUE)) {
        outfile <- infile
    }
    p <- parse(outfile)
    getParseData(p)
}