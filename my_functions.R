pacman::p_load(
  "shiny", "stringr", "stringi", "data.table",
  "rclipboard", "knitr", "rmarkdown", "purrr", "tidytable",
  "tinytex", "DT", "shinydashboard", "journalabbr"
)
# texfile = "rmd/weakconsistency.tex"
# clsfile = "rmd/fuzzy-sets-and-systems.csl"
# clsfile = "rmd/chinese-gb7714-2005-author-date.csl"
# # clsfile = "rmd/chinese-gb7714-2005-numeric.csl"
# bibfile = "rmd/weakconsistency.bib"


fixed_clsfile = "./default.csl"
fixed_qmdfile = "./qmd_default.qmd"
fixed_bibfile = "./MEMIO_default.bib"
output_qmd = "qmd_finally.qmd"
output_tex = gsub("\\.qmd$", ".tex", output_qmd)#这个是根据 output_qmd 自动产生的


clear_file <- function(pattern = "(.*\\.R$)|(.*\\.Rproj$)") {
  tryCatch(
    {
      now_dir <- list.files()
      old_dir <- list.files(pattern = pattern)
      delete_dir <- setdiff(now_dir, old_dir)
      files_to_remove <- delete_dir[!file_test("-d", delete_dir)] # 过滤非目录文件
      file.remove(files_to_remove) # 一次性删除文件
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      warning("An error occurred: ", e$message)
      clear_file()
      stop(safeError(e))
    }
  )
}
############################################################################################
## 1, 通过调用 qmd, 生成 tex 文件  
## eg: output_qmd --> qmd_finally.tex, 然后对这个生成的 tex 进行数据清洗操作
############################################################################################
read_tex <- function(texfile){
  #### 1.Read the original Tex file and extract the key---- ready to form the second half of the .Rmd
  ###############################################
  document = readLines(texfile, encoding = "UTF-8")
  document <- str_squish(document)
  document <- gsub("^%.*", "", document, perl = TRUE) # Delete everything starting with %.
  document <- gsub("([^\\\\])(%.*)", "\\1", document, perl = TRUE) # Delete everything after % that doesn't start with \\
  document <- document[which(document != "")]
  return(document)
}
extract_key <- function(texfile) {
  #### 1.Read the original Tex file and extract the key---- ready to form the second half of the .Rmd
  document <- read_tex(texfile)
  # extract key
  tex_key <- unlist(str_extract_all(document, "(?<=\\\\cite[pt]?\\{).*?(?=\\})"))
  # There are multiple keys in a \cite{***,***,***}, Split with ',' and remove any extra Spaces,
  tex_key <- unique(str_squish(unlist(str_split(tex_key, ","))))
  return(tex_key)
}
copy_file = function(input, out){
  # 先读取后，写入实现拷贝文件的操作
  doc <- readLines(input, encoding = "UTF-8")
  writeLines(doc, out)
}
generate_qmd = function(ckey, output, qmd_template=NULL){
  ckeytext = paste(sprintf("[@%s]", ckey), collapse = "\n\n")
  if(is.null(qmd_template)){
    qmd_template <- system.file("template", "qmd_default.qmd", package = "journalabbr", mustWork = TRUE)
    
  }
  # 两个部分进行拼接
  head = readLines(qmd_template, encoding = "UTF-8")
  body = ckeytext 
  writeLines(c(head, "\n\n", body, "\n\n"), output)
}







#### 开始对 qmd_finally.tex 进行操作 ##
# 0. 先加载qmd_finally.tex 文件,然后进行一定的清洗,得到原始的doc
# 1. 通过对 doc 进行提取, extract_body, 找到 body 部分,  
#   然后对 body 部分进行清洗, 提取出一个 dt_body 数据框, 只含有两列: ckey 和 refvalue
# 2. 通过对 doc 进行提取, extract_ref, 找到 ref 部分, 
#   然后对 ref 部分进行清洗, 提取出一个 dt_ref 数据框, 只含有两列: ckey 和 bibitem
# 3. 进行联合,得到一个 dt
# 4. 根据 dt 的 refvalue 列可以判断出引用的风格是数字还是作者年 
# 4. 根据 dt 可以生成新的参考文献格式.

# 定义函数
extract_body <- function(doc) {
  # 找到 \maketitle 行的索引
  from <- grep("\\\\maketitle", doc) + 1
  
  # 找到所有 \citeproc{ 的行的索引
  indices <- grep("\\\\citeproc\\{", doc)
  
  # 获取最后一行的索引
  to <- if (length(indices) > 0) indices[length(indices)] else NA
  
  # 检查有效性
  if (is.na(to) || from > to) {
    warning("No valid body found between \\maketitle and the last \\citeproc{.")
    return(NULL)
  }
  
  # 提取文档主体
  docbody <- doc[from:to]
  return(docbody)
}
extract_ref <- function(doc) {
  # 找到 \begin{CSLReferences} 和 \end{CSLReferences} 的行索引
  begin_index <- grep("\\\\begin\\{CSLReferences\\}", doc)
  end_index <- grep("\\\\end\\{CSLReferences\\}", doc)
  
  # 检查是否找到索引
  if (length(begin_index) == 1 && length(end_index) == 1) {
    # 提取这两行之间的内容
    references_lines <- doc[(begin_index + 1):(end_index - 1)]
    
    # 返回提取的内容
    return(references_lines)
  } else {
    return(NULL)  # 如果没有找到，则返回 NULL
  }
}

#######
extract_citeproc <- function(text) {
  # 正则匹配模式，用于匹配 \citeproc{ref-***}{***} 结构
  pattern1 <- "(?<=\\\\citeproc\\{ref-)(.*?)(\\}\\{)(.*?)(?=\\})"
  temp <- str_extract(text, pattern1) # 获取 ***}{*** 的结构
  newtext <- str_split(temp, "\\}\\{", n = 2, simplify=T)  #然后按照 }{ 进行拆分,得到 key和 refvalue
  dt = as.data.table(newtext)
  colnames(dt) <- c("ckey", "refvalue")
  #去重
  dt = unique(dt)
  return(dt)
}

#######
ref2list = function(pattern, doc){
  # 向量doc, 根据搜索\\bibitem 进行划分, 划分为 list
  ##  \bibitem[\citeproctext]{ref-saaty2013modern}
  ## pattern = "\\\\bibitem\\[\\\\citeproctext"
  from <- grep(pattern, doc)
  to <- c(from[-1] - 1, length(doc))
  if (length(from) == 0L) {
    stop("There are no available references, please check the doc file.")
  }
  itemslist <- mapply(function(x, y) {
    return(doc[x:y])
  }, x = from, y = to, SIMPLIFY = FALSE)
  return(itemslist)
}
remove_brackets <- function(input_string) {
  # 使用 gsub 去掉 {[} 和 {]} 的括号
  cleaned_string <- gsub("\\{\\[\\}", "[", input_string, perl = TRUE)
  cleaned_string <- gsub("\\{\\]\\}", "]", cleaned_string, perl = TRUE)
  return(cleaned_string)
}
getfields <- function(item) {
  # 检查 item 是否至少有两个元素
  if (length(item) < 2) {
    stop("Item must have at least two elements")
  }
  
  # 提取 ckey
  ckey <- str_extract(item[1], "(?<=\\{ref-).*?(?=\\}$)")
  if (length(ckey) == 0) {
    ckey <- NA  # 如果没匹配到 ckey，设置为 NA
  }
  
  #  提取bibitem
  bibitem = paste(item[2:length(item)], collapse = " ")
  bibitem = remove_brackets(bibitem)
  return(list(ckey=ckey, bibitem=bibitem) )
}
extra_bibitem <- function(doc){
  # 1. 把文档划分为 list, 一个 list 代表一个参考文献
  itemslist = ref2list(pattern = "\\\\bibitem\\[\\\\citeproctext", doc) 
  # 2. 对每个 list 进行信息提取
  itemslist = map(itemslist, getfields)
  # 3. 转为 data.table
  refdt = data.table(rbindlist(itemslist))
  return(refdt)
}

check_ref_type <- function(text) {
  # text 是一个字符向量
  
  # 定义正则表达式模式，检测是否为纯数字或数字范围（例如 12, 12-15）
  numeric_pattern <- "^\\d+(-\\d+)?$"
  
  # 检查每个元素是否符合数字风格
  is_numeric <- grepl(numeric_pattern, text)
  
  # 计算数字风格占比
  numeric_ratio <- sum(is_numeric) / length(text)
  
  # 判断占比是否大于等于 90%
  if (numeric_ratio >= 0.9) {
    warning("参考文献推算为： 数字风格")
    return(1)
  } else {
    warning("参考文献推算为： 作者-年风格")
    return(2)
  }
}

# 
# # 1. 读取 tex 文件
# doc = read_tex(output_tex)
# 
# # 2. 通过对 doc 进行提取, 找到 body 部分,并进行清洗, 提取出一个 dt_body 数据框, 只含有两列: ckey 和 refvalue
# docbody = extract_body(doc) 
# dt_body = extract_citeproc(docbody)
# 
# # 3.  通过对 doc 进行提取, 找到 ref 部分,并进行清洗, 提取出一个 dt_ref 数据框, 只含有两列: ckey 和 bibitem
# docref = extract_ref(doc) 
# dt_ref = extra_bibitem(docref)
# # 4. 合并
# dt = merge(dt_body, dt_ref, by.x = "ckey", by.y = "ckey", all=TRUE)
# # 检查,如果有 NA 值或者 NULL 则发出警告, 并删除 NA 值或 NULL 值所在的行
# if (any(is.na(dt))) {
#   warning("数据框中存在 NA 值，正在删除含有 NA 的行。")
#   # 删除含有 NA 值的行
#   dt <- na.omit(dt)  
# }
# 
# check_ref_type(dt$refvalue)
# 


# abbrevr::AbbrevTitle("Journal of Evolutionary Biology")
# abbrevr::AbbrevTitle("European Journal of Operational Research")
# abbrevr::AbbrevTitle("The European Journal of Operational Research")
# 


# pattern <- "\\\\citeproc\\{(ref-[^{}]+)\\}\\{(.*?)\\}"
# matches <- str_match(doc, pattern)
# # 查看提取结果
# if (!is.na(matches[1, 1])) {
#   key <- matches[1, 2]      # 提取 key (ref-XXX 部分)
#   cationkey <- matches[1, 3] # 提取 cationkey (XX 页码部分)
#   cat("Key:", key, "\n")
#   cat("Cationkey:", cationkey, "\n")
# } else {
#   cat("No match found\n")
# }





















# 
# 
# 
# document = read_tex(texfile)
# 
# 
# tidy_tex <- function(tex) {
#   ## Extract the display information of the page -- it may be a number, an author, or an author-year
#   from1 <- grep("\\\\begin\\{document\\}", tex)
#   from2 <- grep("\\\\maketitle", tex)
#   if (is_empty(from1) && is_empty(from2)) {
#     clear_file()
#     stop("There is no '\\begin{document}' in the .tex file")
#   } else if (!is_empty(from1) && is_empty(from2)) {
#     ind_from <- from1
#   } else if (is_empty(from1) && !is_empty(from2)) {
#     ind_from <- from2
#   } else if (from1 <= from2) {
#     ind_from <- from2
#   } else {
#     clear_file()
#     stop("Index error, there is no '\\begin{document}' in the .tex file")
#   }
#   to1 <- grep("\\\\hypertarget\\{refs\\}", tex, ignore.case = TRUE)
#   to2 <- grep("\\\\begin\\{cslreferences\\}", tex, ignore.case = TRUE)
#   if (is_empty(to1) && is_empty(to2)) {
#     clear_file()
#     stop("there is no '\\begin{cslreferences}' in the .tex file.")
#   } else if (!is_empty(to1) && is_empty(to2)) {
#     ind_to <- to1
#   } else if (is_empty(to1) && !is_empty(to2)) {
#     ind_to <- to2
#   } else if (to1 <= to2) {
#     ind_to <- to1
#   } else {
#     clear_file()
#     stop("Index error, there is no '\\begin{cslreferences}' in the .tex file")
#   }
#   
#   tex_show <- tex[(ind_from + 1):(ind_to - 1)] # Extract display page
#   
#   #### Delete empty lines before the tex_show document
#   for (i in seq_len(100)) {
#     value <- which(tex_show[1] == "")
#     if (!is_empty(value) && value == 1) {
#       tex_show <- tex_show[2:length(tex_show)]
#     } else {
#       break
#     }
#   }
#   #### Delete empty lines after the tex_show document
#   for (i in seq_len(100)) {
#     max_len <- length(tex_show)
#     value <- which(tex_show[max_len] == "")
#     if (!is_empty(value) && value == 1) {
#       tex_show <- tex_show[1:(max_len - 1)]
#     } else {
#       break
#     }
#   }
#   ######### Merge tex_show, because some two lines represent one field
#   tex_show[tex_show == ""] <- "\n@@@\n\n\n" # Special value
#   tex_show <- paste(tex_show, collapse = " ", sep = "")
#   tex_show <- unlist(str_split(tex_show, "\n@@@\n\n\n"))
#   tex_show <- str_trim(tex_show, side = "both")
#   return(tex_show)
# }
# 
# style_fun <- function(texfile) {
#   # texfile='./rmd_finally5.tex'
#   tex <- readLines(texfile, encoding = "UTF-8")
#   style_raw <- paste0(tex, collapse = "\n") # rmd raw style
#   
#   # clear_file()
#   tex_show <- tidy_tex(tex)
#   ################################################################
#   ##### style \bbitem[author-year/number]{CEKY}{reference format} was extracted from tex_show ######
#   ################################################################
#   # 1. extract CEKY and author-year/number
#   extra_text <- str_extract_all(tex_show, "(\\\\protect)(.*?)(\\\\hyperlink)(\\{ref-)([0-9A-Za-z\\-]*?)(\\}.*)")
#   ### 1.1 extract CEKY
#   key_show <- str_replace_all(extra_text, "(\\\\protect)(.*?)(\\\\hyperlink)(\\{ref-)([0-9A-Za-z\\-]*?)(\\}.*)", "\\5")
#   ### 1.2 extract author-year/number
#   word_show <- gsub("(\\\\protect)(.*?)(\\\\hyperlink)(\\{ref-)([0-9A-Za-z\\-]*?)(\\}.*)", "\\6", extra_text, perl = T)
#   word_show2 <- gsub("(\\{\\[\\})(.*?)(\\{\\]\\})", "\\2", word_show, perl = T) # Special case handling
#   word_show3 <- gsub("(^\\}\\{)(.*?)(\\})(.*)", "\\2", word_show2, perl = T) #  Normal handling
#   
#   stopifnot(length(key_show) == length(word_show))
#   show_df <- data.frame("key" = key_show, "word" = word_show3)
#   
#   # 2. Extract reference format from tex
#   ### 2.1 Extract part of references
#   ind_begin <- grep("\\\\begin\\{cslreferences\\}", tex, ignore.case = TRUE)
#   ind_end <- grep("\\\\end\\{cslreferences\\}", tex, ignore.case = TRUE)
#   
#   if (is_empty(ind_begin)) {
#     clear_file()
#     stop("There is no '\\begin{cslreferences}' in tex")
#   }
#   if (is_empty(ind_end)) {
#     clear_file()
#     stop("There is no '\\end{cslreferences}' in tex")
#   }
#   
#   tex_sub <- tex[(ind_begin + 1):(ind_end - 1)] # Extract part of references
#   
#   #### 2.2 Processing tex_sub, so that it becomes \bbitem[author-year/number]{CEKY}{reference format}
#   #### 2.2.1  Delete the line of'\\CSLLeftMargin'
#   ind <- grepl("^\\\\CSLLeftMargin", tex_sub, perl = T, ignore.case = T)
#   tex_sub_new <- str_squish(tex_sub[!ind])
#   ##### 2.2.2 '\leavevmode\hypertarget{ref-****}{}%' field in tex_sub_new becomes the '\bbitem[author-year/number]{CEKY}{' field
#   pat <- sprintf("(\\\\leavevmode)(.*?)(\\\\hypertarget)(\\{ref-)(%s)(\\})(.*%%$)", show_df$key)
#   ####  If show_df$word are all numbers, then '\bibitem{***}{' field
#   ####  otherwise, then '\bibitem[***]{***}{' field
#   is_num <- if (sum(is.na(as.numeric(unlist(show_df$word)))) > 0L) {
#     0L
#   } else {
#     1L
#   }
#   
#   if (is_num) {
#     s0 <- sprintf("\\\\bibitem{%s}{", show_df$key) # \\bibitem{***}{ field
#   } else {
#     s0 <- sprintf("\\\\bibitem[%s]{%s}{", show_df$word, show_df$key) # \\bibitem[***]{***}{
#   }
#   
#   for (i in seq_along(pat)) {
#     tex_sub_new <- gsub(pat[i], s0[i], tex_sub_new, perl = T, ignore.case = T)
#   }
#   #### 2.2.3  Replace the character '\\CSLRightInline{***' with '',  and keep the ***
#   tex_sub_new <- gsub("^\\\\CSLRightInline\\{", "", tex_sub_new, ignore.case = T)
#   
#   ##### 2.2.4 If there is a '{[}***{]}' pattern in tex_sub_new, and keep the ***
#   tex_sub_new <- gsub("(\\{\\[\\})([A-Za-z0-9]*?)(\\{\\]\\})", "[\\2]", tex_sub_new, perl = T) # Special case handling
#   
#   #### style 1 ----  Fixed width to make a reference in multiple lines
#   style_1 <- c("\\section*{References}", "\\begin{thebibliography}{99}", tex_sub_new, "\\end{thebibliography}")
#   style_1 <- paste(style_1, collapse = "\n") #
#   #### style 2 ---- Make a reference on one line
#   tex_sub_new[tex_sub_new == ""] <- "\n\n"
#   style_2 <- c("\\section*{References}\n\n", "\\begin{thebibliography}{99}\n\n", tex_sub_new, "\n\n\\end{thebibliography}")
#   style_2 <- paste(style_2, collapse = " ")
#   style_2 <- str_replace_all(style_2, pattern = " {2,}", replacement = " ") # Handle excess space  or [:blank:]
#   style_2 <- str_replace_all(style_2, pattern = "\n\n {1,}", replacement = "\n\n") # Handles space after a newline
#   return(list(style_1, style_2, is_num))
# }
# 
# 
# tex_diff_bib <- function(texfile, dt) {
#   ###############################################
#   #### 1.Read the original Tex file and extract the key---- ready to form the second half of the .Rmd
#   texkey <- extract_key(texfile = texfile)
#   texkey_dt <- as.data.table(data.frame("CKEY" = texkey, "tex_id" = seq_along(texkey)))
#   
#   #### 2. merge
#   new_dt <- merge.data.table(texkey_dt, dt, by = "CKEY", all = TRUE, suffixes = c("_tex", "_bib"), sort = FALSE)
#   
#   ### 3. diff
#   tex_diff <- !is.na(new_dt$tex_id) & is.na(new_dt$fz_id) # tex has it, bib hasn't  it
#   if (sum(tex_diff) >= 1) {
#     s <- paste0(new_dt$CKEY[tex_diff], collapse = "\n")
#     clear_file()
#     stop(paste0("Error, the following key is used in the Tex file, but not in the bib file!!\n", s, "\nPlease check the key and run again\n"))
#   } else {
#     rmd_yaml <- "---\ntitle: \"how to use cite\"\nauthor: \"zsc\"\ndate: \"`r Sys.Date()`\"\noutput:\n  pdf_document: \n    keep_tex: true\n    latex_engine: xelatex\n    extra_dependencies: [\"ctex\",\"caption\"]\nlink-citations: yes \ncsl: navigation_default.csl\nbibliography: MEMIO_default.bib\n---\n"
#     rmd_key <- paste0("\n[@", texkey, "]\n") %>% paste(., collapse = "")
#     rmd_key %>%
#       paste0(rmd_yaml, .) %>%
#       writeLines(., "rmd_finally.Rmd")
#     return(new_dt)
#   }
# }
# 
# 
# 
# clear_file()