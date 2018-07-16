## Aryeh Nigri
## Moishe Fischbein

library(R6)

Symbol <- R6Class("Symbol",
  public = list(

    type = "character",
    kind = "character",   ##   STATIC, FIELD, ARG, VAR, NONE
    index = "integer",

    initialize = function(type, kind, index) {
        self$type <- type
        self$kind <- kind
        self$index <- index
    },

    getType = function() {
        return(self$type)
    },

    getKind = function() {
        return(self$kind)
    },

    getIndex = function() {
        return(self$index)
    }

  )
)