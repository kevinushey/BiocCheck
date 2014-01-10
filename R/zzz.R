setRefClass("MsgClass",
    fields=list(
        name="character"
    )
)

setRefClass("NumClass",
    fields=list(
        num="integer"
    ),
    methods=list(
        bump = function(){
            if (length(.self$num) == 0)
                .self$num <- 0
            .self$num <- .self$num + 1
            return(.self$num)
            },
        get = function() .self$num,
        zero = function() .self$num <- 0
        )
)


check_errors <- new("MsgClass", name=character(0))
check_warnings <- new("MsgClass", name=character(0))
check_notes <- new("MsgClass", name=character(0))

num_errors <- new ("NumClass", num=integer(0))
num_warnings <- new ("NumClass", num=integer(0))
num_notes <- new ("NumClass", num=integer(0))
