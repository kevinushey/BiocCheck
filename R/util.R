.printf <- function(...) cat(noquote(sprintf(...)), "\n")

.msg <- function(...) message(noquote(sprintf(...)))
.stop <- function(...) stop(noquote(sprintf(...)), call.=FALSE)


handleMessage <- function(msg)
{
    .msg("* %s", msg)
}

handleError <- function(msg)
{
    num_errors$bump()
    .stop(msg)
}

handleWarning <- function(msg)
{
    num_warnings$bump()
    .msg("* WARNING: %s", msg)
}

handleNote <- function(msg)
{
    num_notes$bump()
    .msg(sprintf("* NOTE: %s", msg))
}

