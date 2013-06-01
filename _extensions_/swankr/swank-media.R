makeMediaReplResult <- function(value) {
  UseMethod("makeMediaReplResult")
}

makeMediaReplResult.default <- function(value) {
  makeReplResult(value)
}

makeMediaReplResult.trellis <- function(value) {
  filename <- tempfile("swank-media-")
  png(filename, width=320, height=200)
  print(value)
  dev.off()
  list(quote(`:write-image`), list(list(quote(`:type`), quote(png),
                                        quote(`:file`), filename)),
       deparse(value$call, nlines=1))
}
makeMediaReplResult.ggplot <- function(value) {
  filename <- tempfile("swank-media-")
  png(filename, width=320, height=200)
  print(value)
  dev.off()
  list(quote(`:write-image`), list(list(quote(`:type`), quote(png),
                                        quote(`:file`), filename)),
       deparse(value$call, nlines=1))
}

makeMediaReplResult.numeric <- function(value) {
  string <- paste(deparse(value), sep="", collapse="\n")
  list(quote(`:write-string`), string, quote(`:repl-result`))
}
makeMediaReplResult.complex <- function(value) {
  string <- paste(deparse(value), sep="", collapse="\n")
  list(quote(`:write-string`), string, quote(`:repl-result`))
}

makeMediaReplResult.array <- function(value) {
  makeMediaReplResult.default(value)
}
makeMediaReplResult.matrix <- function(value) {
  makeMediaReplResult.default(value)
}

makeMediaReplResult.help_files_with_topic <- function(value) {
  output <- capture.output(tools:::Rd2txt(utils:::.getHelpFile(value),
                                          options=list(underline_titles=FALSE)))
  string <- paste(output, collapse="\n")
  list(quote(`:popup-buffer`), sprintf("*slime-help(%s)*", attr(value, "topic")),
       string, quote(`ess-help-mode`))
}

makeReplResultFunction <- makeMediaReplResult
