# Function to load and return specified data from a specified file. 

loaddata <- function(filename="All_list_Cleaned.Rda", return_variables) {
  # The function loads the specified file and asserts that the requested objects are present.
  # Arguments:
  #     filename: string                    - name of data file
  #     return_variables: list of string    - names of requested return variables
  # Returns:
  #     list of objects. The objects are the loaded data structures, their names are preserved and match the requested names 
  loaded <- load(filename)
  for (var in return_variables) {
    if (!(hasName(mget(loaded), var))) {
      print(paste("Requested object", var, "not present in loaded file", filename))
      print("Giving up.")
      quit(status=1)
    }
  }
  return(mget(loaded))
}
