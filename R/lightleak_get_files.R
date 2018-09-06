get_files<-  function(dir,opensavexlsx=F){
  require(dplyr);
  if (!opensavexlsx == T) {
    xprt::open_save_xl_wb(dir)
  }
  lapply(list.files(dir,recursive = T,pattern = 'xlsx',full.names = T),mungeLL)
  # needs to add a two part upload to SQL db
  # 1. add meta data to table and get index for key
  # 2. add key to data df and upload that to othertable
}
#getLightLeakFiles
