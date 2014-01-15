setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)


getVigSources <- function(dir)
{
    dir(dir,
        pattern="\\.Rmd$|\\.Rnw$|\\.Rrst$",
        ignore.case=TRUE)
}

checkVignetteDir <- function(pkgdir)
{
    vigdir <- file.path(pkgdir, "vignettes")
    instdocdir <- file.path(pkgdir, "inst", "doc")
    if (!file.exists(vigdir))
        handleError("No 'vignettes' directory!")
    vigdircontents <- getVigSources(vigdir)
    if (length(vigdircontents)==0)
    {
        handleError("No vignette sources in vignettes/ directory.")
    }
    instdocdircontents <- getVigSources(instdocdir)
    if (length(instdocdircontents) > 0)
    {
        handleWarning("Vignette sources exist in inst/doc/; they belong in vignettes/.")
    }


}


checkVersionNumber <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    version <- dcf[, "Version"]
    regex <- "^[0-9]+[-\\.]([0-9]+)[-\\.][0-9]+$"
    if(!grepl(regex, version))
        handleError("Invalid package Version")
    tryCatch(pv <- package_version(version),
        error=function(e) handleError("Invalid package version"))
    y <- pv$minor
    mod <- y %% 2
    biocY <- packageVersion("BiocInstaller")$minor
    bioc.mod <- biocY %% 2
    isDevel <- (bioc.mod == 1)
    if (mod != bioc.mod)
    {
        shouldBe <- ifelse(isDevel, "odd", "even")
        vers <- ifelse(isDevel, "devel", "release")
        handleWarning(sprintf("y of x.y.z version should be %s in %s",
                shouldBe, vers))
    }
}

checkBiocViews <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    if (!"biocViews" %in% colnames(dcf))
    {
        handleWarning("No biocViews found!")
        return()
    } else {
        biocViews <- dcf[, "biocViews"]
        views <- strsplit(gsub("\\s", "", biocViews), ",")[[1]]
        library(biocViews)
        data(biocViewsVocab)
        if (!all(views %in% nodes(biocViewsVocab)))
        {
            badViews <- paste(views[!(views %in% nodes(biocViewsVocab))],
                collapse=", ")
            handleWarning(paste("Some biocViews are invalid:",
                badViews))
        }
    }

    getParent <- function(view)
    {
        topLevel <- c("Software", "ExperimentData", "AnnotationData")
        for (level in topLevel) {
            if (view %in% names(acc(biocViewsVocab, level)[[level]]))
                return(level)
        }
    }
    parents <- c()
    for (view in views)
    {
        parents <- c(parents, getParent(view))
    }
    if (length(unique(parents)) > 1)
    {
        handleWarning(paste("Including biocViews from more than one category",
            "(Software, ExperimentData, AnnotationData)"))
    }
}

checkBBScompatibility <- function(pkgdir)
{
    dcf <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
    segs <- strsplit(pkgdir, .Platform$file.sep)[[1]]
    pkgNameFromDir <- segs[length(segs)]
    if (dcf[, "Package"] != pkgNameFromDir)
        handleError(sprintf(
            "Package dir %s does not match Package: field %s!"),
            pkgNameFromDir, dcf[, "Package"])

    if (!"Version" %in% colnames(dcf))
        handleError("Version field not found in DESCRIPTION!")

    maintainer <- NULL
    if ("Authors@R" %in% colnames(dcf))
    {
        env <- new.env(parent=emptyenv())
        env[["c"]] = c
        env[["person"]] <- person
        pp <- parse(text=dcf[,"Authors@R"]) 
        tryCatch(people <- eval(pp, env),
            error=function(e)
                handleError("Failed to evaluate Authors@R field!"))
        if (!"person" %in% class(people))
            handleError("Authors@R does not evaluate to 'person' object!")
        for (person in people)
        {
            if ("cre" %in% person$role)
            {
                email <- person$email
                if (is.null(email))
                    return(NULL)
                given <- paste(person$given, collapse=" ")
                if (is.null(given))
                    given <- ""
                family <- paste(person$family, collapse=" ")
                if (is.null(family))
                    family <- ""
                if (given == "" && family == "")
                    return(NULL)
                res <- sprintf("%s %s <%s>", given, family, email)
                res <- sub("^ +", "", res)
                maintainer <- res
                break
            }
        }
        if (is.null(maintainer))
            handleError("No author with maintainer (cre) role.")
    } else if ("Maintainer" %in% colnames(dcf)) {
        maintainer <- dcf[,"Maintainer"]
    } else {
        .handleError("No Maintainer or Authors@R field in DESCRIPTION file!")
    }
    # now need to make sure that regexes work, a la python/BBS 
    regex = '(.*\\S)\\s*<(.*)>\\s*'
    match <- regexec(regex, maintainer)[[1]]
    match.length <- attr(match, "match.length")
    if (!  (all(match)  > 0) && (all(match.length) > 0) )
    {
        handleError("Couldn't get email address from Maintainer field.")
    }
}