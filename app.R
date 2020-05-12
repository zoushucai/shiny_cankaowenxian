library(shiny)
library(stringr)
library(rclipboard)
library(knitr)
library(rmarkdown)
library(purrr)
library(dplyr)
library(tinytex)

#options(shiny.fullstacktrace = TRUE)
options(shiny.sanitize.errors = FALSE)
rm(list = ls())
now_dir= list.files()

old_dir = dir(pattern='(.*\\.R$)|(.*\\.Rproj$)|(^navigation_default_orgin.csl$)|(^MEMIO_default_orgin.bib)|(^MEMIO.tex$)|(^navigation_default.csl$)|(^MEMIO_default.bib$)|(^unicode-math.sty$)')

delete_dir = setdiff(now_dir,old_dir)
for(i in delete_dir){
  if(!file_test("-d", i)){#不是目录则删除
    file.remove(i)
  }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel( title = h2("自用参考文献样式调整", align = "left"), windowTitle = '自用参考文献样式调整' ),
    rclipboardSetup(),# 剪切板设置,必须在开头声明,后面才能用,这是一段js的调用
    tabsetPanel( 
        tabPanel("输入",
                 wellPanel(
                   fileInput("file1_tex", "Choose tex File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.tex')),
                   fileInput("file2_csl", "Choose csl File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.csl')),
                   fileInput("file3_bib", "Choose bib File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.bbl')),
                   #radioButtons("Select_Latex_Engine", "选择latex引擎",c('xelatex','lualatex')),
                   actionButton("goButton", "Submit")
                 )),
        tabPanel("使用说明与警告显示",
                 helpText("1, 上传文件时采用 UTF-8 编码.", br(), 
                          "2, 上传文件到生成参考文献样式,需要花一定时间,还请耐心等待.",br(), 
                          "3, 最终输出的参考文献结果还需要仔细检查,符合期刊要求."),
                 HTML("<p><font color='red'>\n警告显示如下:\n</font></p>"),
                 verbatimTextOutput("b0")),
        tabPanel("style 1", 
                 helpText("注意:最终的结果可能还需要细调"),
                 uiOutput("style1clip"), 
                 verbatimTextOutput("style1")),
        tabPanel("style 2", 
                 helpText("注意:最终的结果可能还需要细调"),
                 uiOutput("style2clip"), 
                 verbatimTextOutput("style2")),
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
       
       tabPanel("清空缓存", 
                p("是否要清空缓存"),
                actionButton("goButton3", "Submit"),
                verbatimTextOutput("clearvar"),
                p("是否显示当前目录的所有文件及目录"),
                actionButton("goButton4", "Submit"),
                verbatimTextOutput("filels")
                ,textInput('caption', '输入想要读取的文件', "app.R")
                ,actionButton("goreadfile", "Submit2")
                ,verbatimTextOutput("goreadfiletext")
                ),
       tabPanel('运行环境',
                uiOutput("runenvirclip"), 
                verbatimTextOutput("runenvir")
                ,actionButton("goButton5", "不要轻易点此按钮")
                ,verbatimTextOutput("mmmxxx")
                )
        )
)
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    ###0. 处理 bib 文件与 csl 文件
  randomVals <- eventReactive(input$goButton, {
    tryCatch(
      {
        file_bib = readLines(input$file3_bib$datapath,encoding = "UTF-8")
        file_bib = c('\n',file_bib,'\n')# 对第一个参考文献添加换行,与最后一个参考文件添加换行
        writeLines(file_bib,'./MEMIO_default.bib')
        file_csl = readLines(input$file2_csl$datapath,encoding = "UTF-8")
        writeLines(file_csl,'./navigation_default.csl')
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ##############################################
    ### 0. 形式 rmd 的 yaml 部分##################
    ######0.1 添加模板################
    # s00 = "\\documentclass[12pt,a4paper,oneside]{article}\n\\usepackage{ctex}\n\\usepackage{lmodern}\n\\usepackage{lineno, hyperref}\n\\hypersetup{colorlinks=true, citecolor=blue, anchorcolor=blue}\n\\usepackage{amsmath}\n\\usepackage{amssymb}\n\\usepackage{amsthm}\n\n\\begin{document}\n$body$\n\\end{document}"
    # writeLines(s00,'./template.tex')
    # s0 = "---\ntitle: \"how to use Rmarkdown\"\nauthor: \"Juyuan\"\ndate: \"`r Sys.Date()`\"\noutput:\n  pdf_document: \n    keep_tex: true\n    template: template.tex\n    latex_engine: xelatex\nlink-citations: yes \n"
    ####################
    ### 0.1 使用xelatex, 没有添加模板
    s0 = "---\ntitle: \"how to use Rmarkdown\"\nauthor: \"Juyuan\"\ndate: \"`r Sys.Date()`\"\noutput:\n  pdf_document: \n    keep_tex: true\n    latex_engine: xelatex\nlink-citations: yes \n"
    s1 = "csl: navigation_default_orgin.csl\nbibliography: MEMIO_default_orgin.bib\n---\n"
    s1 = str_replace(s1,'MEMIO_default_orgin.bib','MEMIO_default.bib') %>%  str_replace(.,'navigation_default_orgin.csl','navigation_default.csl')
    file_rmd_sec_yaml = paste(s0,s1,sep = "") # 形成Rmd 的 yaml 部分
    ###############################################
    #### 1 , 开始读取tex文件, 准备形成 rmd 的后半部分
    document = readLines(input$file1_tex$datapath,encoding = "UTF-8")  %>% paste(collapse = ' ')
    command_order = str_extract_all(document,'(?<=\\\\cite[p]?\\{).*?(?=\\})')[[1]]
    command_order = unlist(str_split(command_order,',')) # 如果存在逗号分隔的需要考虑
    command_order = str_trim(command_order, side = "both") # 删除两边的空格
    command_order = unique(command_order) # 删除重复标签
    
    ##### 2, 处理上传的 bib 文件, 提取其 command 命令
    file_bib_type_and_key = str_extract(file_bib,'(^[ ]{0,}?@.*\\{)(.*?,)') %>% na.omit() %>% as.vector() %>%str_trim(., side = "both") %>%  unique()# 提取command命令,这里包含文献的类型和keyword
    file_bib_command = str_extract(file_bib_type_and_key,'(?<=\\{).*?(?=,)') %>% str_trim(., side = "both") 
    file_bib_type = str_extract(file_bib_type_and_key,'(?<=@).*?(?=\\{)') %>% str_trim(., side = "both") 
    
    ### 2.0 对key和参考文献的类型进行拼接对应.
    if(all(command_order %in% file_bib_command) && length(file_bib_type) == length(file_bib_command)){
      bib_type_key_order = data.frame(file_bib_type,file_bib_command)
      temp_df = as.data.frame(command_order)
      bib_type_key = inner_join(as.data.frame(command_order),bib_type_key_order, by=c('command_order'='file_bib_command'))
    }else{
      stop('出错,tex文件中引用了数据库中不存在的文献!!')
    }
    ##### 2.1 对比 tex 文件提取的 command 命令,找到 tex 中没有的bib 
    set_command_diff = setdiff(command_order,file_bib_command)# tex中引用,但bib中不存在
    ##### 2.2 找出bib中没有被tex引用的参考文献
    set_bib_diff = setdiff(file_bib_command,command_order)# bib中存在,但tex没有引用
    
    ##### 2.3 对参考文献bib进行处理.
    # command_order   # 这些都传递给list 函数的输出, 在函数bibout中进行处理
    file_bib_paste = paste(file_bib,collapse = "\n")
    

    
    ## 3. 生成 rmd 文件的参考文献样式
    rmd_order_label = paste0('\n[@',command_order,']\n')
    
    file_finally_default =  paste(file_rmd_sec_yaml,paste(rmd_order_label,collapse = ""),sep = '')
    
    writeLines(file_finally_default,"./file_finally_default.Rmd")
    return(list(rmd_order_label,set_command_diff,command_order,file_bib_paste,bib_type_key,set_bib_diff))
    })

  out_yinyong = reactive({
    data_list = randomVals()
    command_order  =  data_list[[3]]
    file_bib_paste = data_list[[4]]
    bib_type_key = data_list[[5]]
    ## 再次判断这个两个向量是否相同
    if (!identical(bib_type_key$command_order,command_order)){
      stop('出错,tex文件中引用了数据库中不存在的文献!!!')
    }
    if(input$inSelect == '按tex文中引用顺序'){
      command_order = command_order
    }else if(input$inSelect == '按key字母升序'){
      command_order = sort(command_order)
    }else if(input$inSelect=='按key字母降序'){
      command_order = sort(command_order,decreasing = T)
    }else if(input$inSelect == '按文献类型字母升序排序'){
      temp_df2 = arrange(bib_type_key,file_bib_type,command_order) 
      command_order = temp_df2$command_order
    }else if(input$inSelect == '按文献类型字母降序排序'){
      temp_df2 = arrange(bib_type_key,file_bib_type,desc(command_order) )
      command_order = temp_df2$command_order
    }else{
      temp_df2 = arrange(bib_type_key,file_bib_type)
      command_order = temp_df2$command_order
    }

    if(is_empty(command_order)||is.na(command_order)){
      s_temp = 'bib数据库中的文献,一篇文章都没有被引用'
      return(list(s_temp,s_temp))
    }else{
      yinyong_bib = c()
      k = 0
      for(i in command_order){
        k = k+1
        s1_keyword = paste0("\\n@.*?",i,"[\\s\\S]*?(\\n\\}(\\n)*?(?=@)|\\n\\})")
        one_bib = str_extract(file_bib_paste,s1_keyword)
        yinyong_bib =paste0(yinyong_bib,one_bib) 
      #  cat(one_bib,file = 'yinyong.bib',append = T) # 带引用的参考文献
      }
      
      key_yinyong_order = paste(command_order,collapse = '\n')
      key_yinyong_order = paste0('总计: ', k ,' 篇参考文献被引用\n\n',key_yinyong_order)
      return( list(yinyong_bib,key_yinyong_order) )
    }
  })
  out_no_yinyong = reactive({
    data_list = randomVals()
    set_bib_diff =  data_list[[6]]
    file_bib_paste = data_list[[4]]
    
    if(is_empty(set_bib_diff) || is.na(set_bib_diff)){
      s_temp ='刚好没有引用的key,\n(即bib数据库中的文献全部被引用)'
      return(list(s_temp,s_temp))
    }else{
      noyinyong_bib = c()
      k = 0
      for(i in set_bib_diff){
        k = k+1
        s1_keyword = paste0("\\n@.*?",i,"[\\s\\S]*?(\\n\\}(\\n)*?(?=@)|\\n\\})")
        one_bib = str_extract(file_bib_paste,s1_keyword)
        noyinyong_bib =paste0(noyinyong_bib,one_bib) 
        #cat(one_bib,file = 'noyinyong.bib',append = T) # 带引用的参考文献
      }
      key_noyinyong_order = paste(set_bib_diff,collapse = '\n')
      key_noyinyong_order = paste0('总计: ', k ,' 篇参考文献没被引用\n\n',key_noyinyong_order)
      return(list(noyinyong_bib,key_noyinyong_order))
    }
  })
  
  yangshi = reactive({
    data_list = randomVals()
    rmd_order_label = data_list[[1]]
    set_command_diff =  data_list[[2]]
    jinggao = c()
    ############################################################
    ## 上述以及形成的 rmd 完整的文件, 文件名为file_finally_default.Rmd
    ###########################################################
    ## 4. 产生 通过 rmd 文件 生成 tex 文件---再到 pdf 文件 
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    
    progress$set(message = "正在计算中,请耐心等待!!!", value = 0)
   # render("./file_finally_default.Rmd", output_format = "pdf_document") #render("file_finally.Rmd", output_format = "all")
    rmarkdown::render("./file_finally_default.Rmd", output_format = latex_document())
    
    ## 5. 读取新的tex文件,并且提取tex 中具有参考文献样式的字段
    file_finally_default_tex = readLines("./file_finally_default.tex")
    begin_weizhi = which(str_extract(file_finally_default_tex,"\\\\hypertarget" ) == '\\hypertarget')[1] + 1
    end_weizhi = length(file_finally_default_tex)
    file_finally_default_tex2 = file_finally_default_tex[begin_weizhi:end_weizhi]
    end_weizhi = length(file_finally_default_tex2)
    if(str_detect(file_finally_default_tex2[end_weizhi],'document')){
      file_finally_default_tex2 = file_finally_default_tex2[1:(length(file_finally_default_tex2) - 1)]
    }

    ## 6. 处理提取后的参考文献字段,使其变为bibitem样式
    file_finally_tex_cite = str_replace(file_finally_default_tex2,'(^\\\\leavevmode\\\\hypertarget\\{ref-)(.*?)(\\})(\\{\\}%$)',replacement= "\\\\bibitem{\\2\\3")
    ## 6.1 样式 1
    file_finally_tex_cite22 = str_replace(file_finally_tex_cite,'(^[0-9]{1,2}\\.)( )', '')
    file_finally_tex_cite23 = str_replace(file_finally_tex_cite22,"(^\\{\\[\\}[0-9]{1,2}\\{\\]\\})( )",'')# 特殊情况处理
    yangshi1_list = paste(file_finally_tex_cite23,collapse = '\n')
    yangshi1_list = paste0('\\section*{References}\n\\begin{thebibliography}{99}\n',yangshi1_list,'\n\n\\end{thebibliography}')
    
    ## 6.2. 把不同的行的参考文献进行合并, 合并为一行,保证一行对应一个参考文献 --样式 2
    weizhi = which(nchar(file_finally_tex_cite) ==0)
    weizhi = weizhi[weizhi!=1]
    weizhi_end = weizhi - 1 
    weizhi_begin = weizhi +1
    weizhi_begin = c(1,weizhi_begin[-length(weizhi_begin)])
    
    weizhi_m = matrix(c(weizhi_begin,weizhi_end),ncol = 2)
    weizhi_m = weizhi_m[-nrow(weizhi_m),]
    
    temp = c()
    for( i in 1:nrow(weizhi_m)){
      temp[i] = paste(file_finally_tex_cite[weizhi_m[i,1]:weizhi_m[i,2] ], collapse = " ")
    }
    
    temp = temp[nchar(temp) >=5] # 取字符大于等于 5 以上的长度
    if(length(temp) != length(rmd_order_label) ){
      jinggao = append(jinggao,"keyvalue长度不相等, 请仔细检查,有问题!!!")
    }
    file_finally_tex_cite4 = str_replace(temp,'(^\\\\bibitem\\{.*\\} )([0-9]{1,2}\\.)( )', '\\1')
    file_finally_tex_cite41 = str_replace(file_finally_tex_cite4,"(^\\\\bibitem\\{.*?\\} )(\\{\\[\\}[0-9]{1,2}\\{\\]\\})( )",'\\1')
    yangshi2_list = paste(file_finally_tex_cite41,collapse = '\n\n') # %>% writeLines(.,con = 'file_finally_tex_cite2.txt')
    yangshi2_list = paste0('\\section*{References}\n\\begin{thebibliography}{99}\n',yangshi2_list,'\n\n\\end{thebibliography}')
    yangshi_list = list(yangshi1_list,yangshi2_list, jinggao)  # 以 list 形式输出
    return(yangshi_list)
    })
   #######################################################
  ######################### 清楚文件函数 ###############
  clear_file_now = eventReactive(input$goButton3,{
    now_dir= list.files()
    delete_file = list.files(path = ".", pattern = '(.*\\.Rmd$)|(.*\\.tex$)|(.*\\.bib$)|(.*\\.csl$)|(.*\\.md$)|(.*\\.docx$)(.*\\.pdf$)(.*\\.html$)')
    for(i in delete_file){
      if(!file_test("-d", i)){#不是目录则删除
        file.remove(i)
      }
    }
    if (file.exists("file_finally_default.pdf")){
      file.remove("file_finally_default.pdf")
    }
    if(file.exists("template.tex")){
      file.remove("template.tex")
    }
  
    d_file = list.files()
    exist_file = setdiff(d_file, c("app.R","packrat","rsconnect","shiny_cankaowenxian.Rproj",'unicode-math.sty'))
    if(length(exist_file) == 0){
      return(paste("是的,已经清空缓存"))
    }else {
      return(paste0("没有清空缓存,还有以下文件没有删除\n\n",paste(exist_file,collapse = '\n')))
    }
  })
  #######################################################
  ######################### 列举当前文件函数 ###############
  list_file_now = eventReactive(input$goButton4,{
    now_dir= list.files()
    paste(now_dir,collapse = '\n')
  })
  #######################################################
  ######################### 显示运行环境函数 ###############
  read_sessionInfo = reactive({
    sink(file = 'aa.txt')
    print(sessionInfo())
    sink()
    session_txt = readLines("aa.txt",encoding = "UTF-8") %>%  paste(.,collapse ='\n' )
    file.remove('aa.txt')
    return(session_txt)
  })
  read_file = eventReactive(input$goreadfile,{
    temp_txt = readLines(input$caption,encoding = "UTF-8") %>%  paste(.,collapse ='\n' )
    return(temp_txt)
  })
  ##  更新tex函数
   update_sty = eventReactive(input$goButton5,{
     mm = tlmgr_search('unicode-math.sty')  
     tt = tlmgr_install('unicode-math') 
     return(paste(mm,tt,collapse = '\n',sep = '\n'))
   })
  #######################################################
  ############### 输出警告函数,----- 即 tex中有引用,但是bib数据库中不存在该参考文献  ###############
   output$b0 <- renderText({
    d1 = randomVals()[[2]]
    if(length(d1)>=1){
      s3 = "以下keyvalue在 bib 文件中找不到!!!\n\n"
      return(paste(s3, paste(d1, collapse = '\n\n'), sep  = ''))
    }else{
      return('无')
    }
  })
   
   ##########################################
   ##### 输出样式1################
   output$style1 <- renderText({
     yangshi()[[1]]
   })
   output$style1clip <- renderUI({
     rclipButton("style1clip", "style1 Copy", yangshi()[[1]], icon("clipboard"))
   })
   ##########################################
   ##### 输出样式2################
   output$style2 <- renderText({
     yangshi()[[2]]
   })
   output$style2clip <- renderUI({
     rclipButton("clipbtn2", "style2 Copy", yangshi()[[2]], icon("clipboard"))
   })
   
   ##########################################
   ##### 清楚文件################
   output$clearvar <- renderText({
     clear_file_now()
   })
   ##########################################
   ##### 显示文件################
   output$filels <- renderText({
     list_file_now()
   })
   ##########################################
   ##### 输出引用的bib和key################
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
   output$runenvirclip <- renderUI({
     rclipButton("runenvirclip", "environment Copy", read_sessionInfo(), icon("clipboard"))
   })
   output$runenvir <- renderPrint({
     print(sessionInfo())
   })
   ############################################
   output$goreadfiletext <- renderText({
     read_file()
   })
   output$mmmxxx <-renderText({
     update_sty()
   })
}

# Run the application
shinyApp(ui = ui, server = server)
