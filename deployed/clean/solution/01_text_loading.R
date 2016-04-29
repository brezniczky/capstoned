# intent:
# to be able to load the corpora

data.path  = "../../cleaned_input/"

read.files = function(path, indexes = -1) {
  # Args:
  # Indexes of the files to be read up from the data path
  files = list()

  file.names = dir(path)
  file.paths = file.path(path, file.names)
  sizes = file.size(file.paths)

  if (indexes == -1) {
    indexes = 1:length(file.names)
  }

  for(i in indexes) {
    text = readLines(file.paths[i], -1)
    files[[i]] = text
  }

  return(files)
}
