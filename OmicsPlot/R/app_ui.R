#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  fluidPage(
    #  list(tags$head(HTML('<link rel="icon", href="logo.png",
    #                        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title="", windowTitle="OmicsPlot"
        )
    ),
    navbarPage(
      title = "OmicsPlot",#网页logo
      inverse = T,
      theme = shinytheme("cerulean"),
      header = NULL,
      tabPanel("主页", icon = icon("home"),
               fluidRow(
                 column(6,
                        panel_div(
                          class_type = "info",
                          panel_title = "关于该软件",
                          content = "这是一个多组学分析可视化的软件，
                        目前支持GWAS、RNA-seq分析及可视化，后续我们会陆续加入其他功能，
                        欢迎使用！"
                        )),
                 column(6,
                        panel_div(
                          class_type = "info",
                          panel_title = "安装软件",
                          content = HTML(
                            "本软件有网页版和本地版，网页版不能提供长久服务，建议您安装本地版，有以下两种方式：",
                            "<br>",
                            "1）如果您习惯于使用R环境，请下载本软件对应的R包<a href='http://8.130.92.72:9000/app' target='_top'>点击此处</a>",
                            "<br>",
                            "2）如果您想免于R的安装过程，请下载本软件对应的docker镜像<a href='http://8.130.92.72:9000/app' target='_top'>点击此处</a>"
                          )
                        ))
               ),
               br(),
               fluidRow(
                 column(6,
                        panel_div(
                          class_type = "info",
                          panel_title = "使用说明",
                          content = HTML(
                            "使用过程中输入文件需要严格按照示例文件的格式"
                          )
                        )),
                 column(6,
                        panel_div(
                          class_type = "info",
                          panel_title = "联系作者",
                          content = HTML(
                            "如果使用过程中遇到问题或者有更好的建议，欢迎联系",
                            "<br>",
                            "邮箱：","<a href='mailto:zhaojh96@outlook.com?Subject=Shiny%20Help' target='_top'>赵俊恒</a>")
                        ))
               )
      ),
      navbarMenu("GWAS",
                 tabPanel("基因型分析",
                          h4(strong("基因型系统发育树")),
                          sidebarLayout(
                            sidebarPanel(
                              width =4,
                              fileInput("uploadhmp",
                                        "上传hmp文件",
                                        accept = c(".txt", ".hmp"),
                                        buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = NULL),
                              selectInput("Tree_type", "选择系统发育树的形状",
                                          choices = c("rectangular", "dendrogram","slanted","ellipse","roundrect","fan","circular","inward_circular","radial","equal_angle","daylight","ape")),
                              numericInput("tree_width", "选择下载图片宽度", value = 10),
                              numericInput("tree_height", "选择下载图片高度", value = 10),
                              radioButtons("tree_farmat", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("structural_analyse", '开始', icon = icon('play')),
                              downloadButton("download_tree", "下载")),
                            mainPanel(
                              helpText("您需要上传Hapmap格式文件，表头包括：rs、alleles、chrom、pos、strand、assembly、center、prot、assayLSID、panelLSID、QCcode、品种#1、品种#2等。这个过程将持续一段时间，它取决于你数据的大小"),
                              plotOutput("PhyloTree_plot")
                            )
                          ),
                          h4(strong("基因型PCA分析-2D")),
                          sidebarLayout(
                            sidebarPanel(
                              width = 4,
                              numericInput("X_2D", "选择X轴", value = '1', min = '1'),
                              numericInput("y_2D", "选择Y轴", value = '2', min = '1'),
                              numericInput("PCA_plot_2D_width", "选择下载图片宽度", value = 10),
                              numericInput("PCA_plot_2D_height", "选择下载图片高度", value = 10),
                              radioButtons("PCA_plot_2D_farmat", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("PCA_2D", '开始', icon = icon('play')),
                              downloadButton("download_PCA_plot_2D", "下载")
                            ),
                            mainPanel(
                              plotOutput("PCA_plot_2D")
                            )
                          ),
                          h4(strong("基因型PCA分析-3D")),
                          sidebarLayout(
                            sidebarPanel(
                              width = 4,
                              numericInput("X_3D", "选择X轴", value = '1', min = '1'),
                              numericInput("y_3D", "选择Y轴", value = '2', min = '1'),
                              numericInput("Z_3D", "选择Z轴", value = '3', min = '1'),
                              numericInput("PCA_plot_3D_width", "选择下载图片宽度", value = 10),
                              numericInput("PCA_plot_3D_height", "选择下载图片高度", value = 10),
                              radioButtons("PCA_plot_3D_farmat", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("PCA_3D", '开始', icon = icon('play')),
                              downloadButton("download_PCA_plot_3D", "下载")),
                            mainPanel(
                              plotOutput("PCA_plot_3D")
                            ))),
                 tabPanel("表型数据分析",
                          h4(strong("计算BLUP值")),
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("uploadpheno", "上传表型数据",
                                        accept = c(".txt"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE),
                              radioButtons("blup_table_format", "选择下载表格格式", choices = c("csv","tsv", "txt")),
                              actionButton("Calblup", "计算", icon = icon('edit')),
                              downloadButton("download_blup", "下载")),
                            mainPanel(
                              helpText("表型文件表头：Line、Rep、Year、Loc、Phe。其中Line列为不同品种，Rep列为重复，Year列为采集数据的年份，Loc列为采集数据的地点，Phe列为你的表型数据。"),
                              dataTableOutput("Bluptable")
                            )),
                          h4(strong("绘制直方图")),
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("Histplot_width", "下载图片宽度", value = 10),
                              numericInput("Histplot_height", "下载图片高度", value = 10),
                              radioButtons("Histplot_faormat", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              downloadButton("download_histplot", "下载")
                            ),
                            mainPanel(
                              plotOutput("Histplot_mean")
                            )
                          )
                 ),
                 tabPanel("GWAS分析",
                          h4(strong("GWAS分析")),
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("uploadpheno_GWAS", "上传处理后的表型数据",
                                        accept = c(".txt"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE),
                              fileInput("uploadgeno", "上传处理后的基因型文件",
                                        accept = c(".txt"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE),
                              selectInput("GWAS_model", "选择GWAS模型", choices = c("BLINK", "CMLM", "GLM", "MLM", "MMLM", "SUPER", "FarmCPU", "EMMAxP3D")),
                              numericInput("PCA_num", "PCA数", value = '3'),
                              radioButtons("GWAS_table_format", "Select the format for downloading the form", choices = c("csv","tsv", "txt")),
                              actionButton("RunGWAS", "开始", icon = icon('random')),
                              downloadButton("download_GWAS", "下载")
                            ),
                            mainPanel(
                              helpText("表型文件表头：ID、Phe。其中ID列为品种名，Phe列为表型数据（列名可更改）"),
                              dataTableOutput("GWASresult")
                            )
                          ),
                          h4(strong("QQ图")),
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("QQ_width", "下载图片宽度", value = 10),
                              numericInput("QQ_height", "下载图片高度", value = 10),
                              radioButtons("QQ_farmat", "下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("Plot_QQ", "开始", icon = icon('play')),
                              downloadButton("download_QQ", "下载")
                            ),
                            mainPanel(
                              plotOutput("QQ_plot")
                            )
                          ),
                          h4(strong("曼哈顿图")),
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("Manhattan_y", "选择阈值-log10（P）", value = '3'),
                              numericInput("Manhattan_width", "选择下载图片宽度", value = 10),
                              numericInput("Manhattan_height", "选择下载图片高度", value = 10),
                              radioButtons("Manhattan_farmat", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("Plot_Manhattan", "开始", icon = icon('play')),
                              downloadButton("download_Manhattan", "下载")
                            ),
                            mainPanel(
                              plotlyOutput("Manhattan_plot", width = "100%", height = "400px")
                            )
                          ),
                          h4(strong("LD热图")),
                          sidebarLayout(
                            sidebarPanel(
                              textInput("LD_Chromosome", "选择染色体", value = "1A"),
                              numericInput("LD_left", "选择起始位置", value = '1000000'),
                              numericInput("LD_right", "选择终止位置", value = '5000000'),
                              numericInput("LD_width", "选择下载图片宽度", value = 10),
                              numericInput("LD_height", "选择下载图片高度", value = 10),
                              radioButtons("LD_farmat", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("Plot_LD", "开始", icon = icon('play')),
                              downloadButton("download_LD", "下载")
                            ),
                            mainPanel(
                              helpText("需要注意的是，此过程需要重新上传HapmapDiploid格式的基因型文件（不需要上传表型文件）"),
                              plotOutput("LD_plot")
                            )
                          ),
                          h4(strong("提取SNP对应的基因")),
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("uploadGXF", "上传GXF文件",
                                        accept = c(".txt", ".gff", ".gtf", ".gff", ".gff3", ".GFF", ".GTF", ".GFF", ".GFF3"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE),
                              numericInput("up_down_stream", "选择上下游范围（bp）", value = '100000'),
                              numericInput("Significant_SNP", "GWAS结果设置p值筛选SNP", value = '0.01'),
                              radioButtons("SNP_Gene_format", "选择下载表格格式", choices = c("csv","tsv", "txt")),
                              actionButton("Extract", "提取", icon = icon('play')),
                              downloadButton("download_SNP_Gene", "下载")
                            ),
                            mainPanel(
                              dataTableOutput("SNP_Gene")
                            )
                          )
                 )
      ),
      navbarMenu("RNA-seq",
                 tabPanel("表达量分析",
                          h4(strong("相关性分析")),
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("Uploadtraits", "上传表型文件",
                                        accept = c(".txt"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE),
                              selectInput("Cor_method", "选择分析方法", choices = c("pearson", "kendall", "spearman")),
                              radioButtons("Cor_format", "选择下载图片格式", choices = c("csv","tsv", "txt")),
                              numericInput("Cor_heatmap_width", "选择下载图片宽度", value = 10),
                              numericInput("Cor_heatmap_height", "选择下载图片高度", value = 10),
                              radioButtons("Cor_heatmap_format", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("Cal_corr", "开始", icon = icon('play')),
                              downloadButton("download_Cor", "下载表格"),
                              downloadButton("download_Cor_heatmap", "下载图片")
                            ),
                            mainPanel(
                              helpText("表头需要：ID、trait1、trait2...（trait可改为其他名字）"),
                              dataTableOutput("Cor_tab"),
                              plotOutput("Cor_heatmap")
                            )
                          ),
                          h4(strong("聚类分析")),
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("Hcluster_width", "选择下载图片宽度", value = 10),
                              numericInput("Hcluster_height", "选择下载图片高度", value = 10),
                              radioButtons("Hcluster_format", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("Hcluster_plot", "开始", icon = icon('play')),
                              downloadButton("download_Hcluster", "下载图片")
                            ),
                            mainPanel(
                              plotOutput("Hcluster")
                            )
                          ),
                          h4(strong("火山图")),
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("UploadVolcano", "上传差异分析文件",
                                        accept = c(".txt"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE),
                              numericInput("Volcano_x_left", "选择x轴范围（原点左侧）", value = -10),
                              numericInput("Volcano_x_right", "选择x轴范围（原点右侧）", value = 10),
                              numericInput("Volcano_p", "选择p值", value = 0.05),
                              numericInput("Volcano_FC", "选择变异倍数", value = 2),
                              numericInput("Volcano_width", "选择下载图片宽度", value = 10),
                              numericInput("Volcano_height", "选择下载图片高度", value = 10),
                              radioButtons("Volcano_format", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("Volcano_plot", "开始", icon = icon('play')),
                              downloadButton("download_Volcano", "下载图片")
                            ),
                            mainPanel(
                              helpText("表头需要：GeneID、log2FoldChange、padj"),
                              plotOutput("Volcano")
                            )
                          ),
                          h4(strong("热图")),
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("Uploadheatmap", "上传基因表达量文件",
                                        accept = c(".txt"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE),
                              radioButtons("heatmap_scale", "选择标准化对象", choices = c("row", "column", "none")),
                              #radioButtons("heatmap_cluster_row", "是否对行聚类",choices = c("TRUE","FALSE")),
                              #radioButtons("heatmap_cluster_col", "是否对列聚类",choices = c("TRUE", "FALSE")),
                              numericInput("heatmap_width", "选择下载图片宽度", value = 10),
                              numericInput("heatmap_height", "选择下载图片高度", value = 10),
                              radioButtons("heatmap_format", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("heatmap_plot", "开始", icon = icon('play')),
                              downloadButton("download_heatmap", "下载图片")
                            ),
                            mainPanel(
                              helpText("表头需要：ID、trait1、trait2...（trait可改为其他名字）"),
                              plotOutput("heatmap")
                            )
                          )),
                 tabPanel("功能富集分析",
                          h4(strong("GO富集分析")),
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("GeneInfo", "上传注释文件",
                                        accept = c(".txt"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE),
                              fileInput("GeneList", "上传待分析基因集",
                                        accept = c(".txt"), buttonLabel = "Browse...",
                                        placeholder = "No file selected",
                                        capture = TRUE) ,
                              numericInput("GO_pvalue", "选择p值", value = '1'),
                              #numericInput("GO_qvalue", "select the Q-value", value = '1'),
                              radioButtons("GO_table_format", "选择下载格式", choices = c("csv","tsv", "txt")),
                              actionButton("Run_GO", "开始", icon = icon('play')),
                              downloadButton("download_GO_table", "下载")
                            ),
                            mainPanel(
                              helpText("注释文件需要有：GID、GO、KEGG、Pathway列，GO、KEGG、Pathway一般每行有多个数据，中间关键用,隔开。基因集需要有GID列，可以仅次一列"),
                              dataTableOutput("GO_result")
                            )
                          ),
                          h4(strong("KEGG富集分析")),
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("KEGG_pvalue", "选择p值", value = '1'),
                              #numericInput("KEGG_qvalue", "select the Q-value", value = '1'),
                              radioButtons("KEGG_table_format", "选择下载表格格式", choices = c("csv","tsv", "txt")),
                              actionButton("Run_KEGG", "开始", icon = icon('play')),
                              downloadButton("download_KEGG_table", "下载")
                            ),
                            mainPanel(
                              dataTableOutput("KEGG_result")
                            )
                          ),
                          h4(strong("GO富集可视化")),
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("BP_num", "展示BP数量", value = '10'),
                              numericInput("CC_num", "展示CC数量", value = '10'),
                              numericInput("MF_num", "展示MF数量", value = '10'),
                              numericInput("GO_width", "选择下载图片宽度", value = 10),
                              numericInput("GO_height", "选择下载图片高度", value = 10),
                              radioButtons("GO_farmat", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("Plot_GO", "开始", icon = icon('play')),
                              downloadButton("download_GO", "下载")
                            ),
                            mainPanel(
                              plotlyOutput("GO_plot", width = "100%", height = "400px")
                            )
                          ),
                          h4(strong("KEGG富集可视化")),
                          sidebarLayout(
                            sidebarPanel(
                              numericInput("KEGG_num", "展示通路数量", value = '10'),
                              numericInput("KEGG_width", "选择下载图片宽度", value = 10),
                              numericInput("KEGG_height", "选择下载图片高度", value = 10),
                              radioButtons("KEGG_farmat", "选择下载图片格式", choices = c("jpeg", "pdf", "png", "svg", "tiff")),
                              actionButton("Plot_KEGG", "开始", icon = icon('play')),
                              downloadButton("download_KEGG", "下载")
                            ),
                            mainPanel(
                              plotlyOutput("KEGG_plot", width = "100%", height = "400px")
                            )
                          )
                 )
      ),
      tabPanel("Others",
               h4(strong("待更新")))
    ))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "OmicsPlot"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
