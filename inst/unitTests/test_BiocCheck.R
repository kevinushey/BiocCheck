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



test_vignettes0 <- function()
{
    checkException(checkVignetteDir(UNIT_TEST_TEMPDIR))
    dir.create(file.path(UNIT_TEST_TEMPDIR, "vignettes"))
    checkException(checkVignetteDir(UNIT_TEST_TEMPDIR))
    zeroCounters()
    cat("nothing", file=file.path(UNIT_TEST_TEMPDIR, "vignettes", "test.Rnw"))
    BiocCheck:::checkVignetteDir(UNIT_TEST_TEMPDIR)

    checkTrue(BiocCheck:::num_errors$get() == 0 
        && BiocCheck:::num_warnings$get() == 0 
        && BiocCheck:::num_notes$get() == 0)
    zeroCounters()
    instdoc <- file.path(UNIT_TEST_TEMPDIR, "inst", "doc")
    dir.create(instdoc, recursive=TRUE)
    cat("nothing", file=file.path(instdoc, "test.Rnw"))
    checkException(checkVignetteDir(UNIT_TEST_TEMPDIR))
    unlink(file.path(instdoc, "test.Rnw"))
    cat("nothing", file=file.path(instdoc, "test.rnw"))
    checkException(checkVignetteDir(UNIT_TEST_TEMPDIR))
}

