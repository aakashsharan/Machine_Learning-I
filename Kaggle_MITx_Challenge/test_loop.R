imputeFeatures <- function() {
  features <- c('ab', 'cd', 'ef')
  count = 0
  for (m_cols in features){
    if(m_cols == 'cd'){
      features[2] = ''
    }
    else{
      if(count != 0){
        features[1] = '1'
      }
    }
    count = count + 1
  }
  return(features)
}
  

