library(shiny)
library(stringr)
library(rclipboard)
library(knitr)
library(rmarkdown)
library(purrr)
library(dplyr)
library(tinytex)
#options(shiny.fullstacktrace = TRUE)
rm(list = ls())
now_dir= list.files()

old_dir = dir(pattern='(.*\\.R$)|(.*\\.Rproj$)|(^navigation_default_orgin.csl$)|(^MEMIO_default_orgin.bib)|(^MEMIO.tex$)|(^navigation_default.csl$)|(^MEMIO_default.bib$)')

delete_dir = setdiff(now_dir,old_dir)
for(i in delete_dir){
  if(!file_test("-d", i)){#不是目录则删除
    file.remove(i)
  }
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("参考文献样式调整"),
    rclipboardSetup(),
    tabsetPanel( 
        tabPanel("输入",
                 wellPanel(
                   fileInput("file1_tex", "Choose tex File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.tex')),
                   fileInput("file2_csl", "Choose csl File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.csl')),
                   fileInput("file3_bib", "Choose bib File(文件编码:UTF-8)", accept = c("text/csv","text/comma-separated-values,text/plain",'.bbl')),
                   actionButton("goButton", "Submit")
                 )),
        tabPanel("警告",
                 verbatimTextOutput("b0")),
        tabPanel("输出 样式 1", 
                 helpText("注意:最终的结果可能还需要细调"),
                 uiOutput("clip1"), 
                 verbatimTextOutput("b1")),
        tabPanel("输出样式 2", 
                 helpText("注意:最终的结果可能还需要细调"),
                 uiOutput("clip2"), 
                 verbatimTextOutput("b2")),
       tabPanel("使用说明", 
                helpText("1, 上传文件时采用 UTF-8 编码.", br(), 
                         "2, 上传文件到生成参考文献样式,需要花一定时间,还请耐心等待.",br(), 
                         "3, 最终输出的参考文献结果还需要仔细检查,符合期刊要求.")),
       tabPanel("清空缓存or显示文件", 
                helpText("注意:是否清空缓存,"),
                actionButton("goButton3", "Submit"),
                verbatimTextOutput("b3"),
                helpText("注意:这里是显示当前目录的所有文件和目录"),
                actionButton("goButton4", "Submit"),
                verbatimTextOutput("b4"))
        )
)
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    ###0. 处理 bib 文件与 csl 文件
  randomVals <- eventReactive(input$goButton, {
    tryCatch(
      {
        file_bib = readLines(input$file3_bib$datapath,encoding = "UTF-8")
        writeLines(file_bib,'./MEMIO_default.bib')
        file_csl = readLines(input$file2_csl$datapath,encoding = "UTF-8")
        writeLines(file_csl,'./navigation_default.csl')
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    ### 0. 形式 rmd 的 yaml 部分
    s0 = "---\ntitle: \"how to use Rmarkdown\"\nauthor: \"Juyuan\"\ndate: \"`r Sys.Date()`\"\noutput:\n  pdf_document: \n    keep_tex: true\n    latex_engine: xelatex\nlink-citations: yes \n"
    s1 = "csl: navigation_default_orgin.csl\nbibliography: MEMIO_default_orgin.bib\n---\n"
    s1 = str_replace(s1,'MEMIO_default_orgin.bib','MEMIO_default.bib') %>%  str_replace(.,'navigation_default_orgin.csl','navigation_default.csl')
    file_rmd_sec_yaml = paste(s0,s1,sep = "") # 形成Rmd 的 yaml 部分
    
    #### 1 , 开始读取tex文件, 准备形成 rmd 的后半部分
    document = readLines(input$file1_tex$datapath,encoding = "UTF-8")  %>% paste(collapse = ' ')
    command_order = str_extract_all(document,'(?<=\\\\cite[p]?\\{).*?(?=\\})')[[1]]
    command_order = unlist(str_split(command_order,',')) # 如果存在逗号分隔的需要考虑
    command_order = str_trim(command_order, side = "both") # 删除两边的空格
    command_order = unique(command_order) # 删除重复标签
    
    ##### 2, 处理上传的 bib 文件, 提取其 command 命令
    file_bib_command = str_extract(file_bib,'(^[ ]{0,}?@.*\\{)(.*?,)') %>% na.omit() %>% as.vector()
    file_bib_command = str_extract(file_bib_command,'(?<=\\{).*?(?=,)') %>% str_trim(., side = "both") %>% unique()
    ##### 2.1 对比 tex 文件提取的 command 命令,找到 tex 中没有的bib 
    set_command_diff = setdiff(command_order,file_bib_command)
    
    ## 3. 生成 rmd 文件的参考文献样式
    rmd_order_label = paste0('\n[@',command_order,']\n')
    
    file_finally_default =  paste(file_rmd_sec_yaml,paste(rmd_order_label,collapse = ""),sep = '')
    
    writeLines(file_finally_default,"./file_finally_default.Rmd")
    return(list(rmd_order_label,set_command_diff))
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
    render("./file_finally_default.Rmd", output_format = "pdf_document") #render("file_finally.Rmd", output_format = "all")
    
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
    
    yangshi_list = list(yangshi1_list,yangshi2_list, jinggao)  # 以 list 形式输出

    })
   
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
  
    d_file = list.files()
    exist_file = setdiff(d_file, c("app.R","packrat","rsconnect","shiny_cankaowenxian.Rproj"))
    if(length(exist_file) == 0){
      paste("是的,已经清空缓存")
    }else {
      paste0("没有清空缓存,还有以下文件没有删除\n\n",paste(exist_file,collapse = '\n'))
    }
  })
  list_file_now = eventReactive(input$goButton4,{
    now_dir= list.files()
    paste(now_dir,collapse = '\n')
  })
   ## 输出文件
  output$b0 <- renderText({
    d1 = randomVals()[[2]]
    if(length(d1)>=1){
      s3 = "以下keyvalue在 bib 文件中找不到!!!\n\n"
      return(paste(s3, paste(d1, collapse = '\n\n'), sep  = ''))
    }else{
      return('无')
    }
  })
   output$b1 <- renderText({
     yangshi()[[1]]
   })
   output$b2 <- renderText({
     yangshi()[[2]]
   })
   output$clip1 <- renderUI({
       rclipButton("clipbtn1", "rclipButton Copy", yangshi()[[1]], icon("clipboard"))
   })
   output$clip2 <- renderUI({
     rclipButton("clipbtn2", "rclipButton Copy", yangshi()[[2]], icon("clipboard"))
   })
   output$b3 <- renderText({
     clear_file_now()
   })
   output$b4 <- renderText({
     list_file_now()
   })
}

# Run the application
shinyApp(ui = ui, server = server)
