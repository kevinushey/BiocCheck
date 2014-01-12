UNIT_TEST_TEMPDIR <- file.path(tempdir(), "unitTestTempDir")

zeroCounters <- function()
{
    BiocCheck:::num_notes$zero()
    BiocCheck:::num_warnings$zero()
    BiocCheck:::num_errors$zero()
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

