presentationCounter <- 0

savePresentedObject <- function(slimeConnection, value) {
  if(!exists("idToObject", envir=slimeConnection)) {
    assign("idToObject", new.env(), envir=slimeConnection)
  }
  presentationCounter <<- presentationCounter + 1
  assign(as.character(presentationCounter), value, envir=slimeConnection$idToObject)
  presentationCounter
}

presentReplResult <- function(slimeConnection, value) {
  id <- savePresentedObject(slimeConnection, value)
  sendToEmacs(slimeConnection,
              list(quote(`:presentation-start`), id, quote(`:repl-result`)))
  sendReplResult(slimeConnection, value)
  sendToEmacs(slimeConnection,
              list(quote(`:presentation-end`), id, quote(`:repl-result`)))
  sendToEmacs(slimeConnection,
              list(quote(`:write-string`), "\n", quote(`:repl-result`)))
}

sendReplResultFunction <- presentReplResult

`cl:nth-value` <- function(slimeConnection, sldbState, n, values) {
  values[[n+1]]
}

`swank:lookup-presented-object` <- function(slimeConnection, sldbState, id) {
  if(exists(as.character(id), envir=slimeConnection$idToObject)) {
    value <- get(as.character(id), envir=slimeConnection$idToObject)
    list(value, TRUE)
  } else {
    list(FALSE, FALSE)
  }
}

`swank:lookup-presented-object-or-lose` <- function(slimeConnection, sldbState, id) {
  stuff <- `swank:lookup-presented-object`(slimeConnection, sldbState, id)
  if(stuff[[2]]) {
    stuff[[1]]
  } else {
    stop(sprintf("attempt to access unrecorded object (id %d)", id))
  }
}
    
`swank:clear-repl-results` <- function(slimeConnection, sldbState) {
  if(!exists("idToObject", envir=slimeConnection)) {
    assign("idToObject", new.env(), envir=slimeConnection)
  }
  rm(list=ls(slimeConnection$idToObject), envir=slimeConnection$idToObject)
  TRUE
}
