library(shiny)
library(stringr)
require(rclipboard)
library(knitr)
library(rmarkdown)
library(purrr)
library(dplyr)
library(tinytex)
#library(lubridate)
#options(shiny.fullstacktrace = TRUE)
options(shiny.sanitize.errors = FALSE)
rm(list = ls())
clear_file = function(mypattern = '(.*\\.R$)|(.*\\.Rproj$)' ){
  tryCatch(
    {
      now_dir= list.files()
      old_dir = dir(pattern= mypattern)
      
      delete_dir = setdiff(now_dir,old_dir)
      for(i in delete_dir){
        if(!file_test("-d", i)){#不是目录则删除
          file.remove(i)
        }
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      print("出错啦")
      stop(safeError(e))
    }
  )
}

clear_file()



# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel( title = h2("自用参考文献样式调整", align = "left"), windowTitle = '自用参考文献样式调整' ),
  rclipboardSetup(), # 剪切板设置,必须在开头声明,后面才能用,这是一段js的调用
  tabsetPanel( 
    tabPanel("输入",
             wellPanel(
               fileInput("file1_tex", "Choose tex File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.tex')),
               fileInput("file2_csl", "Choose csl File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.csl')),
               fileInput("file3_bib", "Choose bib File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.bbl')),
               actionButton("goButton", "Submit")
             )),
    tabPanel("使用说明与警告显示",
             helpText("1, 上传文件时采用 UTF-8 编码.", br(), 
                      "2, 上传文件到生成参考文献样式,需要花一定时间,还请耐心等待.",br(), 
                      "3, 最终输出的参考文献结果还需要仔细检查,符合期刊要求.")
             #,HTML("<p><font color='red'>\n警告显示如下:\n</font></p>")  # 方法一:  直接使用HTML标签
             ,p("警告显示如下:", style="font-weight:bold;color:red;")
             ,verbatimTextOutput("out_warning")),
    tabPanel("style 1", 
             helpText("注意:最终的结果可能还需要细调"),
             uiOutput("out_style1_clip"), 
             verbatimTextOutput("out_style1")),
    tabPanel("style 2", 
             helpText("注意:最终的结果可能还需要细调"),
             uiOutput("out_style2_clip"), 
             verbatimTextOutput("out_style2")),
    tabPanel("style 0", 
             helpText("注意:最终的结果可能还需要细调"),
             uiOutput("out_style0_clip"), 
             verbatimTextOutput("out_style0")),
    tabPanel("引用bib和key", 
             fluidRow(
               column(12,
                      p("输出引用的bib项和key值")
               )
             ),
             radioButtons("inSelect", "bib文件和key的输出顺序,会同步变化",
                          c("按tex文中引用顺序", "按key字母升序" , "按key字母降序","按文献类型字母升序排序","按文献类型字母降序排序","按文献类型排序")),
             fluidRow(
               column(9, p('引用bib'), wellPanel(
                 uiOutput("bib01yinyongclip"), 
                 verbatimTextOutput("bib01yinyong"), 
               )),
               column(3, p('引用key'), wellPanel(
                 uiOutput("key01yinyongclip"), 
                 verbatimTextOutput("key01yinyong")
               ))
             )
    ),
    tabPanel("未引用bib和key", 
             helpText("输出未引用的bib项和key值"),
             fluidRow(
               column(9, p('未引用bib'), wellPanel(
                 uiOutput("bib02noyinyongclip"), 
                 verbatimTextOutput("bib02noyinyong")
               )),
               column(3, p('未引用key'), wellPanel(
                 uiOutput("key02noyinyongclip"), 
                 verbatimTextOutput("key02noyinyong")
               ))
             )
    ),
    tabPanel("运行环境与目录", 
             verbatimTextOutput("out_runenvir")
    )
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  ###0. 处理 bib 文件, csl, tex 文件
  randomVals <- eventReactive(input$goButton, {
    clear_file()
    tryCatch(
      {
        file_csl = readLines(input$file2_csl$datapath,encoding = "UTF-8")
        writeLines(file_csl,'./navigation_default.csl')
        
        
        file_bib = readLines(input$file3_bib$datapath,encoding = "UTF-8")
        file_bib = c('\n',file_bib,'\n')# 对第一个参考文献添加换行,与最后一个参考文件添加换行
        writeLines(file_bib,'./MEMIO_default.bib')
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    ###############################################
    #### 1 , 开始读取tex文件, 准备形成 rmd 的后半部分
    document = readLines(input$file1_tex$datapath, encoding = "UTF-8")
    document = str_replace_all(document,'^[:blank:]*?%.*','')# 删除以% 开头的所有内容
    document = str_replace_all(document,'([^\\\\])(%.*)','\\1') # 删除前面不是以\\开始的% 以后的所有内容
    document = document[which(document != '')] %>% paste(collapse = ' ')
    tex_key = str_extract_all(document,'(?<=\\\\cite[pt]?\\{).*?(?=\\})')[[1]]
    # 如果存在逗号分隔的需要考虑 并 删除两边的空格,后 删除重复标签
    tex_key = unlist(str_split(tex_key,','))  %>% str_trim(., side = "both") %>% unique()
    
    ################################################
    ###### 2. 提取 bib文件,把key和key所对应的类型 组合成数据框
    ## 读取一个file_bib文件,解析出 key值和 type 数据框,
    file_bib = readLines("MEMIO_default.bib",encoding = "UTF-8") 
    file_bib_type_and_key = str_extract(file_bib,'(^[ ]{0,}?@.*\\{)(.*?,)') %>% na.omit() %>% as.vector() %>%str_trim(., side = "both") %>%  unique()# 提取command命令,这里包含文献的类型和keyword
    file_bib_command = str_extract(file_bib_type_and_key,'(?<=\\{).*?(?=,)') %>% str_trim(., side = "both") # key键命令
    file_bib_type = str_extract(file_bib_type_and_key,'(?<=@).*?(?=\\{)') %>% str_trim(., side = "both") #key键对应的参考文献类型
    if(length(file_bib_type) == length(file_bib_command)){
      bib_df = data.frame('type' = file_bib_type, 'key' = file_bib_command)
    }else{
      stop("提取的key键与key键对应的参考文献类型长度不一致")
    }
    
    ### 2.0 对key和参考文献的类型进行拼接对应. 当bib中含有tex所有参考文献时
    if( all(tex_key %in% c(bib_df$key,""))  ){
      ##### 2.1 找出bib中没有被tex引用的参考文献
      set_bib_diff = setdiff(bib_df$key,tex_key)# bib中存在,但tex没有引用 
      ## 3. 生成 rmd 文件的参考文献样式
      rmd_yaml = "---\ntitle: \"how to use cite\"\nauthor: \"zsc\"\ndate: \"`r Sys.Date()`\"\noutput:\n  pdf_document: \n    keep_tex: true\n    latex_engine: xelatex\n    extra_dependencies: [\"ctex\",\"caption\"]\nlink-citations: yes \ncsl: navigation_default.csl\nbibliography: MEMIO_default.bib\n---\n"
      rmd_key = paste0('\n[@',tex_key,']\n') %>%  paste(.,collapse = "")
      rmd_finally = rmd_key %>%  paste0(rmd_yaml, . )
      
      writeLines(rmd_finally,"rmd_finally.Rmd")
      
      file_bib = readLines("MEMIO_default.bib")
      file_bib_paste = paste0(file_bib,collapse = "\n")
      return(list(tex_key, bib_df,set_bib_diff,file_bib_paste)) # 对参考文献bib进行处理.方便返回list
      # tex_key 为 key值在tex 中出现的顺序
      # bib_df 为 bib数据库中key值与类型的对应 
      # set_bib_diff 为bib 中有,但tex没有现的数据
      # file_bib_paste 为bib数据库--只不过合并了
    }else{
      ##### 2.3 对比 tex 文件提取的 command 命令, tex 中有,但bib没有的数据 
      set_tex_diff = setdiff(tex_key, bib_df$key)
      mm = paste(set_tex_diff, collapse =  "\n") # tex中引用,但bib中不存在,则报错
      print(mm)
      stop(paste0("\n",mm,'\n出错,这些参考文献在tex文件中引用了,但是在数据库中不存在该文献!!') )
    }
    
  })
  
  
  tex_cite_style = reactive({
    tempvalue = randomVals()
    ## 读取新的tex文件,并且提取tex 中具有参考文献样式的字段
    # 并通过pandoc 把Rmd文件转变为latex文件
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "正在计算中,请耐心等待!!!", value = 0)
    rmarkdown::render("rmd_finally.Rmd", output_format = latex_document())
    tex =  readLines("rmd_finally.tex", encoding = "UTF-8")
    clear_file()
    
    begin_weizhi = which(str_extract(tex,"\\\\hypertarget" ) == '\\hypertarget')[1] + 1
    end_weizhi = length(tex)
    tex_sub = tex[begin_weizhi:end_weizhi] # 提取tex 的一个子页面
    end_weizhi = length(tex_sub)
    
    if(str_detect(tex_sub[end_weizhi],'document')){
      # 检查最后一行是否包含字符串 'document', 如果包括,则删除该行
      tex_sub = tex_sub[1:(length(tex_sub) - 1)]
    }
    style_0 = paste0(tex_sub,collapse = "\n")     # 传递一个子tex页面,返回处理好的引用样式
    
    
    ## 6. 处理tex_sub的参考文献字段,使其变为bibitem样式
    ##### 6.1 把tex_sub中的 '\leavevmode\hypertarget{ref-****}{}%'  字段变成'\bibitem{***}' 字段
    tex_sub_new = str_replace(tex_sub,'(^\\\\leavevmode\\\\hypertarget\\{ref-)(.*?)(\\})(\\{\\}%$)',replacement= "\\\\bibitem{\\2\\3")
    ##### 6.2 把tex_sub_new 中的 {[}**{]} 给替换为空
    tex_sub_new = str_replace(tex_sub_new,"(^\\{\\[\\}[0-9]{1,2}\\{\\]\\})( )",'') %>% str_trim(.,side = "both") # 特殊情况处理
    ##### 6.3 把环境 \\begin{cslreferences} 改为\begin{thebibliography}环境
    tex_sub_new = str_replace_all(tex_sub_new,'(?<=\\{)cslreferences(?=\\})',"thebibliography")
    tex_sub_new = str_replace(tex_sub_new, '\\\\begin\\{thebibliography\\}','\\\\section*{References}\n\\\\begin{thebibliography}{99}\n\n')
    #### 样式1 ----  每一个参考文献在不同行
    style_1 = paste(tex_sub_new,collapse = "\n") #
    
    #### 样式2 ---- 每一个参考文献在同一行
    tex_sub_new[tex_sub_new == ""] = "\n\n"
    tex_sub_new_2 = paste(tex_sub_new, collapse = " ")
    tex_sub_new_2 = str_replace_all(tex_sub_new_2,pattern = " {2,}",replacement = " ") # 处理多余的空格
    style_2 =  str_replace_all(tex_sub_new_2,pattern = "\n\n ",replacement = "\n\n") # 处理换行后的空格
    
    
    
    return(list(style_1,style_2,style_0))
  })
  
  
  
  #######################################################
  ######################### 显示运行环境函数 ###############
  
  
  ###############################################
  ############### 输出警告函数,----- 即 tex中有引用,但是bib数据库中不存在该参考文献  ###############
  output$out_warning <- renderText({
    d1 = randomVals() # 返回一个list
    if(length(d1)>1){
      return('无')
    }
  })
  
  ##########################################
  ##### 输出样式1################
  output$out_style1 <- renderText({
    tex_cite_style()[[1]]
  })
  output$out_style1_clip <- renderUI({
    rclipButton("out_style1_clip", "style1 Copy", tex_cite_style()[[1]], icon("clipboard"))
  })
  ##########################################
  ##### 输出样式2################
  output$out_style2 <- renderText({
    tex_cite_style()[[2]]
  })
  output$out_style2_clip <- renderUI({
    rclipButton("out_style2_clip", "style2 Copy", tex_cite_style()[[2]], icon("clipboard"))
  })
  
  ##########################################
  ##### 输出样式0################
  output$out_style0 <- renderText({
    tex_cite_style()[[3]]
  })
  output$out_style0_clip <- renderUI({
    rclipButton("out_style0_clip", "style0 Copy", tex_cite_style()[[3]], icon("clipboard"))
  })
  
  
  ##########################################
  ##### 输出引用的bib和key################
  out_yinyong = reactive({
    data_list = randomVals()
    tex_key  =  data_list[[1]]     # tex_key 为 key值在tex 中出现的顺序
    bib_df = data_list[[2]]     # bib_df 为 bib数据库中key值与类型的对应 
    set_bib_diff = data_list[[3]]     # set_bib_diff 为bib 中有,但tex没有出现的数据
    file_bib_paste = data_list[[4]]       # file_bib_paste 为bib数据库--只不过合并了
    if(input$inSelect == '按tex文中引用顺序'){
      tex_key = tex_key
    }else if(input$inSelect == '按key字母升序'){
      tex_key = sort(tex_key)
    }else if(input$inSelect=='按key字母降序'){
      tex_key = sort(tex_key,decreasing = T)
    }else if(input$inSelect == '按文献类型字母升序排序'){
      temp_df = as.data.frame(tex_key)
      bib_df2_temp = inner_join(bib_df,temp_df,by=c('key' = 'tex_key'))
      bib_df2_temp = arrange(bib_df2_temp,type, key) 
      tex_key = bib_df2_temp$key
    }else if(input$inSelect == '按文献类型字母降序排序'){
      temp_df = as.data.frame(tex_key)
      bib_df2_temp = inner_join(bib_df,temp_df,by=c('key' = 'tex_key'))
      bib_df2_temp = arrange(bib_df2_temp,desc(type),key) 
      tex_key = bib_df2_temp$key
    }else{
      ################### 按照文献类型排序
      temp_df = as.data.frame(tex_key)
      bib_df2_temp = inner_join(bib_df,temp_df,by=c('key' = 'tex_key'))
      bib_df2_temp = arrange(bib_df2_temp,type) 
      tex_key = bib_df2_temp$key
    }
    
    if(is_empty(tex_key)||is.na(tex_key)){
      s_temp = 'bib数据库中的文献,一篇文章都没有被引用'
      return(list(s_temp,s_temp))
    }else{
      cite_bib = c()
      k = 0
      for(i in tex_key){
        k = k+1
        s1_keyword = paste0("\\n@.*?",i,"[\\s\\S]*?(\\n\\}(\\n)*?(?=@)|\\n\\})")
        one_bib = str_extract(file_bib_paste,s1_keyword)
        cite_bib =paste0(cite_bib,one_bib) 
        #  cat(one_bib,file = 'yinyong.bib',append = T) # 带引用的参考文献
      }
      
      cite_key = paste(tex_key,collapse = '\n')
      cite_key = paste0('总计: ', k ,' 篇参考文献被引用\n\n',cite_key)
      return( list(cite_bib,cite_key) )
    }
  })
  out_no_yinyong = reactive({
    data_list = randomVals()
    tex_key  =  data_list[[1]]     # tex_key 为 key值在tex 中出现的顺序
    bib_df = data_list[[2]]     # bib_df 为 bib数据库中key值与类型的对应 
    set_bib_diff = data_list[[3]]     # set_bib_diff 为bib 中有,但tex没有出现的数据
    file_bib_paste = data_list[[4]]       # file_bib_paste 为bib数据库--只不过合并了
    
    if(is_empty(set_bib_diff) || is.na(set_bib_diff)){
      s_temp ='刚好没有引用的key,\n(即bib数据库中的文献全部被引用)'
      return(list(s_temp,s_temp))
    }else{
      nocite_bib = c()
      k = 0
      for(i in set_bib_diff){
        k = k+1
        s1_keyword = paste0("\\n@.*?",i,"[\\s\\S]*?(\\n\\}(\\n)*?(?=@)|\\n\\})")
        one_bib = str_extract(file_bib_paste,s1_keyword)
        nocite_bib =paste0(nocite_bib,one_bib) 
        #cat(one_bib,file = 'noyinyong.bib',append = T) # 带引用的参考文献
      }
      nocite_key = paste(set_bib_diff,collapse = '\n')
      nocite_key = paste0('总计: ', k ,' 篇参考文献没被引用\n\n',nocite_key)
      return(list(nocite_bib,nocite_key))
    }
  })
  
  output$bib01yinyongclip <- renderUI({
    rclipButton("bib01yinyongclip", " bib cite Copy", out_yinyong()[[1]], icon("clipboard"))
  })
  output$bib01yinyong <- renderText({
    out_yinyong()[[1]]
  })
  output$key01yinyongclip <- renderUI({
    rclipButton("key01yinyongclip", "key cite Copy", out_yinyong()[[2]], icon("clipboard"))
  })
  output$key01yinyong <- renderText({
    out_yinyong()[[2]]
  })
  ############################################
  
  ############################################
  ###### 输出没有引用的bib和key##############
  output$bib02noyinyongclip <- renderUI({
    rclipButton("bib02noyinyongclip", "bib no cite Copy", out_no_yinyong()[[1]], icon("clipboard"))
  })
  output$bib02noyinyong <- renderText({
    out_no_yinyong()[[1]]
  })
  output$key02noyinyongclip <- renderUI({
    rclipButton("key02noyinyongclip", "key no cite Copy", out_no_yinyong()[[2]], icon("clipboard"))
  })
  output$key02noyinyong <- renderText({
    out_no_yinyong()[[2]]
  })
  ############################################
  
  ############################################
  ###### 输出运行环境 ####################
  output$out_runenvir <- renderPrint({
    print(list(sessionInfo(),dir()))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
