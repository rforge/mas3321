#' Launches an editor containing the commands in a demo
#' @param demo name of demo.
#' @importFrom utils file.edit
#' @export
demoCommands=function(demo){
  if(!.Platform$OS.type=="unix"){
    options(editor = "internal")
  }
  filename = paste(deparse(substitute(demo)),".R",sep="")
  pathname = system.file("demo",filename, package="nclbayes")
  file.edit(pathname,title = filename)
}
