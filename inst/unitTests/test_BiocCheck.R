UNIT_TEST_PKG <- "unitTestTempDir"
UNIT_TEST_TEMPDIR <- file.path(tempdir(), UNIT_TEST_PKG)

message("You may see some warnings here -- they don't indicate unit test problems.")


create_test_package <- function(pkgpath, description=list())
{
    canned <- list(Author="Test Author", 
        Maintainer="Test Maintainer <test@test.com>", "Authors@R"=NULL)
    for (name in names(description))
    {
        canned[[name]] <- description[[name]]
    }
    path <- file.path(tempdir(), pkgpath)
    suppressMessages(create(path, canned))
    cat("#", file=file.path(path, "NAMESPACE"))
    path
}

zeroCounters <- function()
{
    BiocCheck:::num_notes$zero()
    BiocCheck:::num_warnings$zero()
    BiocCheck:::num_errors$zero()
}

stillZero <- function()
{
    BiocCheck:::num_notes$get() == 0 &&
    BiocCheck:::num_warnings$get() == 0 &&
    BiocCheck:::num_errors$get() == 0
}

.setUp <- function()
{
    dir.create(UNIT_TEST_TEMPDIR)
    zeroCounters()
}

.tearDown <- function()
{
    unlink(UNIT_TEST_TEMPDIR, TRUE)
}

setVersion <- function(version)
{
    cat(paste("Version:", version),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
}


test_vignettes0 <- function()
{
    checkException(checkVignetteDir(UNIT_TEST_TEMPDIR)) ## no vignettes dir

    dir.create(file.path(UNIT_TEST_TEMPDIR, "vignettes"))
    checkException(checkVignetteDir(UNIT_TEST_TEMPDIR)) ## empty vignettes dir
    zeroCounters()
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "vignettes", "test.Rnw"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR) ## vig dir w/source file

    checkTrue(BiocCheck:::num_errors$get() == 0 
        && BiocCheck:::num_warnings$get() == 0 
        && BiocCheck:::num_notes$get() == 0)
    zeroCounters()
    instdoc <- file.path(UNIT_TEST_TEMPDIR, "inst", "doc")
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.rnw"))
    zeroCounters()
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::num_errors$get() == 0 
        && BiocCheck:::num_warnings$get() == 1 
        && BiocCheck:::num_notes$get() == 0)
    zeroCounters()
    unlink(instdoc, TRUE)
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.Rmd"))
    checkException(checkVignetteDir(UNIT_TEST_TEMPDIR),
        "Rmd file not seen as valid vignette source")
}

test_checkVersionNumber <- function()
{
    setVersion("lkjgfhfdlkgjhdflkgj")
    checkException(checkVersionNumber(UNIT_TEST_TEMPDIR))
    setVersion("1.2.3.4")
    checkException(checkVersionNumber(UNIT_TEST_TEMPDIR))
    isDevel <- ((packageVersion("BiocInstaller")$minor %% 2) == 1) 
    zeroCounters()
    if (isDevel)
    {
        setVersion("1.2.3")
    } else {
        setVersion("1.3.3")
    }
    BiocCheck:::checkVersionNumber(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::num_warnings$get() ==1)

}

test_checkBiocViews <- function()
{
    cat("Foo: bar", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::num_warnings$get() == 1,
        "missing biocViews doesn't produce warning")
    zeroCounters()
    cat("biocViews: foo, Cancer, bar,\n    baz", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::num_warnings$get() == 1,
        "invalid biocViews don't produce warning")
    cat("biocViews: GO, CellBasedAssays", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    zeroCounters()
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::num_warnings$get() == 0,
        "valid biocViews produce warning")
    zeroCounters()
    cat("biocViews: aCGH, ChipName", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBiocViews(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::num_warnings$get() == 1,
        "biocViews from multiple categories don't produce warning")
}

test_checkBBScompatibility <- function()
{
    cat("Package: Foo", file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    checkException(checkBBScompatibility(UNIT_TEST_TEMPDIR),
        "Package name which doesn't match dir name does not cause exception!")
    cat(sprintf("Package: ", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    checkException(checkBBScompatibility(UNIT_TEST_TEMPDIR),
        "Missing Version doesn't throw exception!")
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: syntax error", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    checkException(checkBBScompatibility(UNIT_TEST_TEMPDIR),
        "Syntax error in Authors@R doesn't throw exception!")
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: 1 + 1", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    checkException(checkBBScompatibility(UNIT_TEST_TEMPDIR),
        "Wrong class in Authors@R doesn't throw exception!")
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: c(person('Bioconductor', 'Package Maintainer', email='maintainer@bioconductor.org', role=c('aut')))", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    checkException(checkBBScompatibility(UNIT_TEST_TEMPDIR),
        "Missing cre role in Authors@R doesn't throw exception!")
    cat(sprintf("Package: %s\nVersion: 0.99.0", UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    checkException(checkBBScompatibility(UNIT_TEST_TEMPDIR),
        "Missing Maintainer and Authors@R doesn't throw exception!")
    cat(sprintf("Package: %s\nVersion: 0.99.0\nMaintainer: Joe Blow",
        UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    checkException(checkBBScompatibility(UNIT_TEST_TEMPDIR),
        "Missing email in Maintainer doesn't throw exception!")
    zeroCounters()
    cat(sprintf("Package: %s\nVersion: 0.99.0\nMaintainer: Joe Blow <joe@blow.com>",
        UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
    zeroCounters()
    cat(sprintf("Package: %s\nVersion: 0.99.0\nAuthors@R: c(person('Bioconductor', \n  'Package Maintainer', email='maintainer@bioconductor.org', role=c('aut', 'cre')))",
        UNIT_TEST_PKG),
        file=file.path(UNIT_TEST_TEMPDIR, "DESCRIPTION"))
    BiocCheck:::checkBBScompatibility(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())

}

test_checkUnitTests <- function()
{
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(BiocCheck:::num_notes$get() == 1)
    dir.create(file.path(UNIT_TEST_TEMPDIR, "tests"))
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "tests",
        "foo.R"))
    zeroCounters()
    BiocCheck:::checkUnitTests(UNIT_TEST_TEMPDIR)
    checkTrue(stillZero())
}

test_installAndLoad <- function()
{
    BiocCheck:::installAndLoad(create_test_package('testpkg'))
    checkTrue("package:testpkg" %in% search(),
        "testpkg is not installed!")
}