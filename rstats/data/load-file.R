# タブ区切りのテキストファイルをデータフレームに読み込む関数
# Parameter:
#   path: ファイル名またはファイルパス
load.file <- function(path) {
  # ファイルを読み込んでデータフレームに格納
  data <- read.table(    
    path,                # 読み込むファイル
    header=T,            # 1行目は列名を指定
    fileEncoding="UTF-8" # 文字コードをUTF-8に指定
  )
  # データフレームを戻り値として返す
  return(data)
}
