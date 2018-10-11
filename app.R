source("./helper.R", local = TRUE)

ui <- fluidPage(useShinyjs(), # for reactive functions such as observe input.
                
                titlePanel("DOM Prototype - Interactive Marketing Channel Analysis Dashboard"),
                
                sidebarLayout(
                        sidebarPanel(h2("Choose from the settings below: "), 
                                     
                                     #input file to display
                                     fileInput("df_file", h5("Import .csv below: "),
                                               accept = c('text/csv','text/comma-separated-values,text/plain','.csv')
                                     ),
                                     
                                     helpText("NOTE: csv file MUST match the format of the example data file. No data input
                                              will result in the example data being used. To see the format, select the
                                              example data tab. You can also download the example code to help."),
                                     
                                     #reset option
                                     actionButton("start_over", "Reset Settings"),
                                     
                                     helpText("To use the example data again, please refresh the page."),
                                     
                                     #weeks (Updates in server)
                                     sliderInput(
                                             "week", h5("Week(s) to display"),
                                             min = 2, max = 5, value = c(2,5)
                                     ),
                                     
                                     #conversion selection
                                     selectInput("conversion", h5("Statistic of Interest (Conversion): "),
                                                         choices = name_dict[4:11], selected = name_dict[10]),
                                     
                                     helpText("Any option below will work if you are not interested in rates. Each
                                              variable page has one graph focused only on the conversion variable and another 
                                              focusing on the rate, which in this case you can ignore."),
                                     
                                     #rate stat selection
                                     selectInput("rate", h5("Rate statistic: "),
                                                             choices = name_dict[4:11], selected = name_dict[6]),
                                     
                                     #mediums
                                     checkboxGroupInput("medium", h5("Channel to explore: "), 
                                                        choices = c("Direct", "Email", "Natural Search", 
                                                                    "Paid Search", "Total"),
                                                        selected = c("Direct", "Email", "Natural Search", 
                                                                     "Paid Search", "Total")),
                                     #row for ylim selection
                                     splitLayout(
                                             
                                             numericInput("ylim", "Set max Y Value: ",
                                                  value = NA),
                                             
                                             numericInput("ylim2", "Set min Y Value",
                                                  value = NA)),
                                     
                                     #row for xlim selection
                                     splitLayout(
                                             
                                             numericInput("xlim", "Set max X Value: ",
                                                          value = NA),
                                             
                                             numericInput("xlim2", "Set min X Value",
                                                          value = NA)),
                                     
                                     #best fit line config
                                     
                                     selectInput("bf", "Best Fit Line: ",
                                                 choices = c(None = "n", Curve = "c", Line = "l")),
                                     
                                     #color of best fit line
                                     selectInput("bfCol", "Color: ",
                                                 choices = c(Default = "#706b6b", Purple = "#7851a9", Red = "#e60000", Green = "#00e600",
                                                             Blue = "#349aff", Black = "#000000", Orange = "#ffa500")),
                                     
                                     helpText("Best fit line will not affect the certain graphs." ),
                                     
                                     helpText("Choose points/lines/both on graphs where applicable"),
                                     
                                     #data presentation config
                                     splitLayout(
                                             
                                             checkboxInput("line", "Use Lines ", TRUE),
                                             checkboxInput("points", "Use Points ", TRUE)
                                             
                                     ),
                                     
                                     textOutput('input_list')
                                     
                                     ),
                        
                        mainPanel(
                                
                                #general navigation - all variables
                                tabsetPanel(id = "navbar",
                                            
                                            #title text
                                            tabPanel(
                                                    title = "Welcome", value = "welcome_tab",
                                                    
                                                    h1("Introduction to DOM"),
                                                    
                                                    p("DOM is a prototype dashboard aiming to save 
                                                      you some time by automating the routine marketing channel performance check."), 
                                                    
                                                    p("Using ", em("Shiny, "), "an ", em("r "), "package for developing web apps, 
                                                      DOM breaks down the performance of your various channels at a glance and 
                                                      puts metric up against metric in a simple x ~ y format, returning handy graphs and 
                                                      numbers to present to your directors and all at a fraction of the work."),
                                                    
                                                    h3("Quick Start Guide"),
                                                    
                                                    p("1) Get your web analytics data in a csv file format & upload it to the dashboard 
                                                      using the file input system on the left - it", strong(" MUST ") ,"match the format of the example data 
                                                      which can be viewed in the 'Example Data' tab above. Read more below under 'Data'."),
                                                    
                                                    p("2) In the sidebar, toggle all the settings that suit your needs. You can zoom into the data
                                                      using the x and y axes limits."),
                                                    
                                                    p("You will be returned information focusing on the statistic of interest that you choose.
                                                        For example, choosing orders (as your conversion stat) will return information regarding
                                                      orders themselves and how they measure against all other statistics e.g. time, views etc."),
                                                    
                                                    p("The 'rate' variable will be used against the 'conversion' variable
                                                      to help get some context to your data i.e. if your 'conversion' variable
                                                      is 'orders' and 'rate' variable is 'visits', you will get orders/visits or 
                                                      orders per visit." ),
                                                    
                                                    p("3) Check all the x values of interest in the check boxes below to plot 
                                                      against your conversion and rate statistics chosen in step 2. The tab will appear as an option above."), 
                                                    
                                                    p("4) And that's it! Copy and paste the graphs to your deck, 
                                                      click points on any graph to get the numbers and use the 'Explore Data' 
                                                      tab to get percentage changes."),
                                                    
                                                    h2("Data"),
                                                    
                                                    h4("Data Specification"),
                                                    
                                                    p("The data is formatted in a way that is common and simple to execute with a bit of SQL. The data is grouped by week
                                                      and then by channel. It lists counts in various website metrics such as visits. The current version of DOM does 
                                                      not support dimensional data (e.g. Device) apart from week and channel."),
                                                    
                                                    p("All data is already in a usable and relevant data structure (i.e. before uploaded, data should be in the correct formats and
                                                      classified as the correct data types)."),
                                                    
                                                    h4("Demo Data"),
                                                    
                                                    p("There is already some example data that was randomly generated during development handy and already uploaded
                                                      that you can play around with in DOM - just immediately go to any other tab. You can use the data to play with as well 
                                                      as use as a template to design your own data around. 
                                                      It can be found raw in the 'Example Data' tab for viewing or download. You can also click the button below 
                                                      to generate custom data made to demonstrate DOM's functionality."),
                                                    
                                                    p("Otherwise, feel free to use your own data in DOM and see how it works in that way. This current version of DOM does not offer 
                                                      custom variable selection or week range as they are hard coded into the application."), p(),
                                                    
                                                    #download eg data
                                                    downloadButton("makeData", "Click Here To Generate Data"), br(),
                                                    
                                                    h5("In the generated data, there is a big dip in traffic between weeks 4 and 5 but that's not the only issue. \nCan you find out what it is?"),
                                                    
                                                    #trivia
                                                    actionButton("showAns", "Show Answer"),
                                                    
                                                    hidden(
                                                            div(id = "hideAns",
                                                                br(),
                                                                h5("There is also a huge, consistent rise in errors by those coming through the Email channel!")
                                                            )
                                                    ),
                                                    
                                                    br(),
                                                    
                                                    p(h5("To see your data change over time, the 'Week' tab will do the job."),
                                                      h5("To see your chosen statistics as a result of others, choose from the selection below and then open the tab that appears.")),
                                                    
                                                    #choose which variables to compare to
                                                    fluidRow(
                                                            column(3,
                                                                   checkboxInput("week_check", "Week", TRUE),
                                                                   checkboxInput("visitors_check", "Visitors", FALSE),
                                                                   checkboxInput("visits_check", "Visits", FALSE),
                                                                   checkboxInput("eg_check", "Example Data", TRUE)
                                                            ),
                                                            column(4, offset = 1,
                                                                   checkboxInput("pageviews_check", "Page Views", FALSE),
                                                                   checkboxInput("basketstarts_check", "Basket Starts", FALSE),
                                                                   checkboxInput("orders_check", "Orders", FALSE),
                                                                   checkboxInput("explore", "Explore Data", TRUE)
                                                            ),
                                                            column(4,
                                                                   checkboxInput("errors_check", "Errors", FALSE),
                                                                   checkboxInput("bounces_check", "Bounces", FALSE),
                                                                   checkboxInput("revenue_check", "Revenue", FALSE)
                                                            )
                                                            
                                                            
                                                    ),
                                                    
                                                    h2("Have fun using DOM! Please get in touch with any questions."),
                                                    br(), h2("A.")
                                                    ),
                                            
                                            #each variable gets its own tab
                                            tabPanel(
                                                    title = "Week", value = "week_tab",
                                                    textInput("week_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("overWeeks", click = "overWeeksClick"),
                                                    dataTableOutput("weekNums"),
                                                    textInput("week_Title2", h5("Plot Title: ")),
                                                    plotOutput("byChannel_overWeeks", click = "weekHovClick"),
                                                    dataTableOutput("weekHovNums"),
                                                    textInput("week_Title3", h5("Plot Title: ")),
                                                    plotOutput("overWeeks_segChannel", click = "weekSegClick"),
                                                    dataTableOutput("weekSegNums")
                                            ),
                                            tabPanel(
                                                    title = "Visitors", value = "visitors_tab",
                                                    textInput("visit_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("visitor1", click = "visitor1Click"),
                                                    dataTableOutput("visitor1Nums"),
                                                    textInput("visit_Title2", h5("Plot Title: ")),
                                                    plotOutput("visitor2", click = "visitor2Click"),
                                                    dataTableOutput("visitor2Nums")
                                            ),
                                            tabPanel(
                                                    title = "Visits", value = "visits_tab",
                                                    textInput("visitor_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("visit1", click = "visit1Click"),
                                                    dataTableOutput("visit1Nums"),
                                                    textInput("visitor_Title2", h5("Plot Title: ")),
                                                    plotOutput("visit2", click = "visit2Click"),
                                                    dataTableOutput("visit2Nums")
                                            ),
                                            tabPanel(
                                                    title = "Page Views", value = "pageviews_tab",
                                                    textInput("pv_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("pv1", click = "pv1Click"),
                                                    dataTableOutput("pv1Nums"),
                                                    textInput("pv_Title2", h5("Plot Title: ")),
                                                    plotOutput("pv2", click = "pv2Click"),
                                                    dataTableOutput("pv2Nums")
                                            ),
                                            tabPanel(
                                                    title = "Basket Starts", value = "basketstarts_tab",
                                                    textInput("bs_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("bs1", click = "bs1Click"),
                                                    dataTableOutput("bs1Nums"),
                                                    textInput("bs_Title2", h5("Plot Title: ")),
                                                    plotOutput("bs2", click = "bs2Click"),
                                                    dataTableOutput("bs2Nums")
                                            ),
                                            tabPanel(
                                                    title = "Errors", value = "errors_tab",
                                                    textInput("error_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("error1", click = "error1Click"),
                                                    dataTableOutput("error1Nums"),
                                                    textInput("error_Title2", h5("Plot Title: ")),
                                                    plotOutput("error2", click = "error2Click"),
                                                    dataTableOutput("error2Nums")
                                            ),
                                            tabPanel(
                                                    title = "Bounces", value = "bounces_tab",
                                                    textInput("bounce_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("bounce1", click = "bounce1Click"),
                                                    dataTableOutput("bounce1Nums"),
                                                    textInput("bounce_Title2", h5("Plot Title: ")),
                                                    plotOutput("bounce2", click = "bounce2Click"),
                                                    dataTableOutput("bounce2Nums")
                                            ),
                                            tabPanel(
                                                    title = "Orders", value = "orders_tab",
                                                    textInput("order_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("order1", click = "order1Click"),
                                                    dataTableOutput("order1Nums"),
                                                    textInput("order_Title2", h5("Plot Title: ")),
                                                    plotOutput("order2", click = "order2Click"),
                                                    dataTableOutput("order2Nums")
                                            ),
                                            tabPanel(
                                                    title = "Revenue", value = "revenue_tab",
                                                    textInput("rev_Title1", h5("Enter your plot title here or keep blank to use the default: ")),
                                                    plotOutput("rev1", click = "rev1Click"),
                                                    dataTableOutput("rev1Nums"),
                                                    textInput("rev_Title2", h5("Plot Title: ")),
                                                    plotOutput("rev2", click = "rev2Click"),
                                                    dataTableOutput("rev2Nums")
                                            ),
                                            
                                            #view and download example data
                                            tabPanel(
                                                    title = "Example Data", value = "eg_tab",
                                                    dataTableOutput("eg_data"),
                                                    downloadButton("downloadData", "Download the data here")
                                            ),
                                            
                                            #explore % values for the current crop of data
                                            tabPanel(
                                                    title = "Explore Data", value = "explore",
                                                    tabsetPanel(
                                                            tabPanel("Percentages",
                                                                     htmlOutput("conv_title"),
                                                                     dataTableOutput("explore_conv"),
                                                                     htmlOutput("rate_title"),
                                                                     dataTableOutput("explore_rate"),
                                                                     htmlOutput("per_title"),
                                                                     dataTableOutput("explore_per")
                                                                     ),
                                                            tabPanel("Values",
                                                                     htmlOutput("conv_title_nums"),
                                                                     dataTableOutput("explore_conv_nums"),
                                                                     htmlOutput("rate_title_nums"),
                                                                     dataTableOutput("explore_rate_nums"),
                                                                     htmlOutput("per_title_nums"),
                                                                     dataTableOutput("explore_per_nums"))
                                                    )
                                            )
                                                    )
                                
                                
                                
                                                    )
                                            )
)

server <- function(input, output, session) {
        
        #FILE INPUT FUNCTIONALITY
        
        #Getting user inputted data
        myData <- reactive({
                
                #recieve file
                inFile <- input$df_file
                
                #return default data if no data is inputted
                if(is.null(inFile)) return(data)
                
                #get data and clean the data
                df <- read.csv(inFile$datapath, header = T)
                df$week <- as.factor(df$week)
                df$week_start_date <- as.POSIXct(df$week_start_date)
                
                #drop first row if it is the row numbers index
                if(identical(df[,1], 1:nrow(df))){
                        
                        df <- df[-1]
                        
                }
                
                return(df)
                
        })
        
        #EXAMPLE PAGE
        
        #Data table of the default data
        output$eg_data <- renderDataTable({
                
                #show the example table in the tab
                datatable(data, options = list(lengthMenu = c(5,10,20), pageLength = 20))
                
        })
        
        output$downloadData <- downloadHandler(
                
                #write the filename
                filename = "example_data.csv",
                
                #write the file for DL
                content = function(file) {
                        write.csv(data, file, row.names = FALSE)
                })
        
        #EXPLORE DATA
        
        ##% titles
        output$conv_title <- renderUI({
                
                #Clean up the conversion titles
                x <- Hmisc::capitalize(strsplit(input$conversion, "_")[[1]])
                
                #Depending on conversion name length (e.g. Page Views vs. Bounces), return output name
                h3(ifelse(length(x) == 2, paste(x[1], x[2], "Change Over the Weeks"),
                       paste(x[1], "Change Over the Weeks")))
                
                
        })
        
        output$rate_title <- renderUI({
                
                #Clean up the conversion titles
                x <- Hmisc::capitalize(strsplit(input$rate, "_")[[1]])
                
                #Depending on conversion name length (e.g. Page Views vs. Bounces), return output name
                h3(ifelse(length(x) == 2, paste(x[1], x[2], "Change Over the Weeks"),
                       paste(x[1], "Change Over the Weeks")))
                
                
        })
        
        output$per_title <- renderUI({
                
                #rate name depending on the rate
                if(input$rate == "revenue"){
                        
                        per <- paste(name_dict[input$conversion], "per Unit of", name_dict[input$rate])
                        
                } else {
                        
                        per <- paste(name_dict[input$conversion], "per", 
                                     substring(name_dict[input$rate],1, nchar(name_dict[input$rate]) - 1))
                }
                
                h3(paste(per, "Change Over The Weeks"))
                
        })
        
        #% change WoW tables
        output$explore_conv <- renderDataTable({
                
                #retrieve week on week table (see helper for function details)
                wow_df(df = myData(), conv = input$conversion, rate = input$rate,
                       mediums = input$medium, weekRange = input$week , name_dict = name_dict)[[1]][,-2]
                
        }, options = list(dom = 't'))
        
        output$explore_rate <- renderDataTable({
                
                #retrieve week on week table (see helper for function details)
                wow_df(df = myData(), conv = input$conversion, rate = input$rate,
                       mediums = input$medium, weekRange = input$week , name_dict = name_dict)[[2]][,-2]
                
        }, options = list(dom = 't'))
        
        output$explore_per <- renderDataTable({
                
                #retrieve week on week table (see helper for function details)
                wow_df(df = myData(), conv = input$conversion, rate = input$rate,
                       mediums = input$medium, weekRange = input$week , name_dict = name_dict)[[3]][,-2]
                
        }, options = list(dom = 't'))
        
        ##Value titles
        output$conv_title_nums <- renderUI({
                
                #Clean up the conversion titles
                x <- Hmisc::capitalize(strsplit(input$conversion, "_")[[1]])
                
                #Depending on conversion name length (e.g. Page Views vs. Bounces), return output name
                h3(ifelse(length(x) == 2, paste(x[1], x[2], "Change Over the Weeks"),
                          paste(x[1], "Change Over the Weeks")))
                
                
        })
        
        output$rate_title_nums <- renderUI({
                
                #Clean up the conversion titles
                x <- Hmisc::capitalize(strsplit(input$rate, "_")[[1]])
                
                #Depending on conversion name length (e.g. Page Views vs. Bounces), return output name
                h3(ifelse(length(x) == 2, paste(x[1], x[2], "Change Over the Weeks"),
                          paste(x[1], "Change Over the Weeks")))
                
                
        })
        
        output$per_title_nums <- renderUI({
                
                #rate name depending on the rate
                if(input$rate == "revenue"){
                        
                        per <- paste(name_dict[input$conversion], "per Unit of", name_dict[input$rate])
                        
                } else {
                        
                        per <- paste(name_dict[input$conversion], "per", 
                                     substring(name_dict[input$rate],1, nchar(name_dict[input$rate]) - 1))
                }
                
                h3(paste(per, "Change Over The Weeks"))
                
        })
        
        #Value change WoW tables
        output$explore_conv_nums <- renderDataTable({
                
                #retrieve week on week table (see helper for function details)
                wow_df(df = myData(), conv = input$conversion, rate = input$rate,
                       mediums = input$medium, weekRange = input$week , name_dict = name_dict, values = TRUE)[[1]][,-2]
                
        }, options = list(dom = 't'))
        
        output$explore_rate_nums <- renderDataTable({
                
                #retrieve week on week table (see helper for function details)
                wow_df(df = myData(), conv = input$conversion, rate = input$rate,
                       mediums = input$medium, weekRange = input$week , name_dict = name_dict, values = TRUE)[[2]][,-2]
        }, options = list(dom = 't'))
        
        output$explore_per_nums <- renderDataTable({
                
                #retrieve week on week table (see helper for function details)
                wow_df(df = myData(), conv = input$conversion, rate = input$rate,
                       mediums = input$medium, weekRange = input$week , name_dict = name_dict, values = TRUE)[[3]][,-2]
                
        }, options = list(dom = 't'))
        
        #MAKE DATA
        
        genData <- reactive({
                
                #set base value
                weekly <- 30000
                
                #traffic rates
                traffic_rates <- c(Direct = 0.5, Email = 0.1, N_Search = 0.2, P_Search = 0.2)
                
                #rates for each of the variables (e.g. page view, bounces etc.)
                v_r <- list(Direct = 0.95, Email = 0.8, N_Search = 0.97, P_Search = 0.99)
                pv_r <- list(Direct = 1.3, Email = 1, N_Search = 1.1, P_Search = 1.1)
                bs_r <- list(Direct = 0.02, Email = 0.01, N_Search = 0.005, P_Search = 0.02)
                e_r <- list(Direct = 0.001, Email = 0.003, N_Search = 0.001, P_Search = 0.002)
                b_r <- list(Direct = 0.4, Email = 0.5, N_Search = 0.7, P_Search = 0.7)
                o_r <- list(Direct = 0.99, Email = 0.96, N_Search = 0.9, P_Search = 0.9)
                a_v <- list(Direct = 6, Email = 7, N_Search = 6, P_Search = 5)
                
                #put all the rates into a list
                rates <- list(vis = v_r, pv = pv_r,  bs = bs_r,  err = e_r,  b = b_r, o = o_r, av = a_v)
                
                #initialise dataframe
                dummy_data <- data.frame(week = integer(), week_start_date = factor(), channel = character(), 
                                         visitors = numeric(), visits = numeric(), page_views = numeric(),
                                         basket_starts = numeric(), errors = numeric(), bounces = numeric(),
                                         orders = numeric(), revenue = numeric())
                
                #get unique dates
                dates <- unique(data$week_start_date)
                
                for(i in 2:7){ #2:5 due to default week range
                        
                        #initialise empty df
                        temp <- data.frame(week = integer(), week_start_date = factor(), channel = character(),
                                           visitors = numeric(), visits = numeric(), page_views = numeric(),
                                           basket_starts = numeric(), errors = numeric(), bounces = numeric(),
                                           orders = numeric(), revenue = numeric())
                        
                        #increment traffic by the rates
                        for(n in names(traffic_rates)){
                                
                                #get the new number
                                v <- ceiling(weekly * traffic_rates[n])
                                
                                #new df with updated values using the rates
                                temp2 <- data.frame(week = i, week_start_date = dates[i - 1], channel = n,
                                                    visits = ceiling(v),
                                                    visitors = ceiling(v * rates[["vis"]][[n]]),
                                                    page_views = ceiling(v * rates[["pv"]][[n]]), 
                                                    basket_starts = ceiling(v * rates[["bs"]][[n]]), 
                                                    errors = ceiling((v * rates[["bs"]][[n]]) * rates[["err"]][[n]]), 
                                                    bounces = ceiling(v * rates[["b"]][[n]]),
                                                    orders = ceiling((ceiling(v * rates[["bs"]][[n]]) - 
                                                                              ceiling((v * rates[["bs"]][[n]]) * 
                                                                                              rates[["err"]][[n]])) * rates[["o"]][[n]]), 
                                                    revenue = ceiling((ceiling(v * rates[["bs"]][[n]]) - 
                                                                               ceiling((v * rates[["bs"]][[n]]) * 
                                                                                               rates[["err"]][[n]])) * rates[["o"]][[n]]) *
                                                            rates[["av"]][[n]]
                                )
                                
                                #aggregate with existing df
                                temp <- rbind(temp, temp2)
                                
                        }
                        
                        #add traffic dip
                        if(i != 4){
                                
                                #normal increment (random number)
                                weekly <- weekly * rnorm(1, 1.05, 0.05)
                                
                                #get rot - random decrease rate
                                rot <- rnorm(1, 0.06, 0.02)
                                
                                #change the rates
                                traffic_rates[1] <- traffic_rates[1] - rot
                                traffic_rates[2:4] <- traffic_rates[2:4] + ((1 - sum(traffic_rates)) / 3)
                                
                        } else{
                                #increment traffic
                                weekly <- weekly * 0.75  
                                
                                #get rot
                                rot <- rnorm(1, 0.06, 0.02)
                                
                                #change rates
                                traffic_rates[1] <- traffic_rates[1] - rot
                                traffic_rates[2:4] <- traffic_rates[2:4] + ((1 - sum(traffic_rates)) / 3)
                                
                        }
                        
                        # change the rates - for the different variables (not error rate)
                        for(r in c(1:3, 5:7)){
                                
                                for(m in 1:4){
                                        
                                        #randomly change the rates of each channel within the variables
                                        rates[[r]][[m]] <- rates[[r]][[m]] + abs(rnorm(1, 0.005, 0.004))
                                        
                                }
                                
                        }
                        
                        #edit the error rate seperately
                        for(m in c(1,3,4)){
                                
                                #fixed error rate
                                rates[["err"]][[m]] <- rates[["err"]][[m]] * 0.7
                                
                        }
                        
                        #implement answer to the mock problem
                        rates[["err"]][["Email"]] <- rates[["err"]][["Email"]] + 0.1
                        
                        #new temp df for the totals
                        temp3 <- data.frame(week = i, week_start_date = dates[i - 1], channel = "Total",
                                            visitors = sum(temp$visitors), visits = sum(temp$visits),
                                            page_views = sum(temp$page_views), basket_starts = sum(temp$basket_starts),
                                            errors = sum(temp$errors), bounces = sum(temp$bounces),
                                            orders = sum(temp$orders), revenue = sum(temp$revenue))
                        
                        #aggregate with the existing df
                        temp <- rbind(temp, temp3)
                        
                        #roll back up to top level
                        dummy_data <- rbind(dummy_data, temp)
                        
                }
                #ensure data type and get rid of rownames
                rownames(dummy_data) <- c() ; dummy_data$channel <- as.character(dummy_data$channel)
                
                #rename channels
                for(n in c("P_Search", "N_Search")){
                        
                        if(n == "P_Search"){
                                
                                dummy_data$channel[dummy_data$channel == n] <- "Paid Search"
                                
                        } else{
                                
                                dummy_data$channel[dummy_data$channel == n] <- "Natural Search"
                                
                        }
                        
                }
                
                #ensure data types
                dummy_data$channel <- as.factor(dummy_data$channel)
                
                return(dummy_data)
                
        })
        
        output$makeData <- downloadHandler(
                
                #download generated data
                filename = "gen_data.csv",
                content = function(file) {
                        write.csv(genData(), file, row.names = FALSE)
                }
                
        )
        
        #NOT ALL ANNOTATED DUE TO REPITITION
        #Pattern - Plot & retrieve using helper.R function
        
        #vs. WEEK
        
        #get graphs using input
        week_graphs <- reactive({
                
                plot_weeks(df = myData(), conv = input$conversion, rate = input$rate, 
                           mediums = input$medium, pal = col_pal, weekRange = input$week, 
                           name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$week_Title1,
                           bestfit = input$bf, points = input$points, line = input$line, bfCol = input$bfCol)
                
        })
        
        #gen 1st plot for weeks tab
        output$overWeeks <- renderPlot({
                
                #see helper.R to see function
                week_graphs()[[1]]
        })
        
        #gen numbers for weeks tab - 1st graph
        output$weekNums <- renderDataTable({
                
                #see helper.R for details
                get_week_data(df = myData(), weekRange = input$week, click_input = input$overWeeksClick, 
                              conv = input$conversion, rate = input$rate, name_dict = name_dict, mediums = input$medium,
                              outPut = 1)
                
                
        }, options = list(dom = 't'))
        
        #gen 2nd plot for weeks tab
        output$byChannel_overWeeks <- renderPlot({
                
                #see helper.R to see function
                week_graphs()[[2]]
                
        })
        
        #get the data for the plot - rate
        output$weekSegNums <- renderDataTable({
                
                #see helper.R for details
                get_week_data(df = myData(), weekRange = input$week, click_input = input$weekSegClick, 
                              conv = input$conversion, rate = input$rate, name_dict = name_dict, mediums = input$medium,
                              outPut = 1)
                
        }, options = list(dom = 't'))
        
        #gen 3rd plot (2nd on page)
        output$overWeeks_segChannel <- renderPlot({
                
                #see helper.R to see function
                week_graphs()[[3]]
                
        })
        
        #get the data for the numbers when clicking on the week graph for the rates
        output$weekHovNums <- renderDataTable({
                
                #see helper.R for details
                get_week_data(df = myData(), weekRange = input$week, click_input = input$weekHovClick, 
                              conv = input$conversion, rate = input$rate, name_dict = name_dict, mediums = input$medium,
                              outPut = 2)
                
        }, options = list(dom = 't'))
        
        #vs. VISITORS
        
        #get plots - see helper.R
        
        visitor_graphs <- reactive({
                
                plot_others(col = "visitors", df = myData(), conv = input$conversion, rate = input$rate, 
                            mediums = input$medium, pal = col_pal, weekRange = input$week, 
                            name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$visitor_Title1,
                            xlim1 = input$xlim, xlim2 = input$xlim2, bestfit = input$bf, line = input$line,
                            points = input$points, bfCol = input$bfCol)
                
        })
        
        #first visitors tab graph
        output$visitor1 <- renderPlot({
                
                #subset the first graph
                visitor_graphs()[[1]]
        })
        
        #second visitors tab graph
        output$visitor2 <- renderPlot({
                
                #subset the first graph
                visitor_graphs()[[2]]
        })
        
        #Output click value related data
        output$visitor1Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$visitor1Click, outPut = 1, col = "visitors")
                
        }, options = list(dom = 't'))
        
        output$visitor2Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input = input$visitor2Click, outPut = 2, col = "visitors")
                
        }, options = list(dom = 't'))
        
        #vs. VISITS
        
        #generate graphs for visit
        visit_graphs <- reactive({
                
                plot_others("visits", df = myData(), conv = input$conversion, rate = input$rate, 
                            mediums = input$medium, pal = col_pal, weekRange = input$week, 
                            name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$visitor_Title1,
                            xlim1 = input$xlim, xlim2 = input$xlim2, bestfit = input$bf)
                
        })
        
        output$visit1 <- renderPlot({
                
                visit_graphs()[[1]]
        })
        
        output$visit2 <- renderPlot({
                
                visit_graphs()[[2]]
        })
        
        output$visit1Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$visit1Click, outPut = 1, col = "visits")
                
        }, options = list(dom = 't'))
        
        output$visit2Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$visit2Click, outPut = 2, col = "visits")
                
        }, options = list(dom = 't'))
        
        #vs. PAGE VIEWS
        
        #gen graphs
        pv_graphs <- reactive({
                
                plot_others("page_views", df = myData(), conv = input$conversion, rate = input$rate, 
                            mediums = input$medium, pal = col_pal, weekRange = input$week, 
                            name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$pv_Title1,
                            xlim1 = input$xlim, xlim2 = input$xlim2, bestfit = input$bf)
                
        })
        
        output$pv1 <- renderPlot({
                
                pv_graphs()[[1]]
        })
        
        output$pv2 <- renderPlot({
                
                pv_graphs()[[2]]
        })
        
        output$pv1Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$pv1Click, outPut = 1, col = "page_views")
        }, options = list(dom = 't'))
        
        output$pv2Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$pv2Click, outPut = 2, col = "page_views")
                
        }, options = list(dom = 't'))
        
        # vs. BASKET STARTS
        
        bs_graphs <- reactive({
                
                plot_others("basket_starts", df = myData(), conv = input$conversion, rate = input$rate, 
                            mediums = input$medium, pal = col_pal, weekRange = input$week, 
                            name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$bs_Title1,
                            xlim1 = input$xlim, xlim2 = input$xlim2, bestfit = input$bf)
                
        })
        
        output$bs1 <- renderPlot({
                
                bs_graphs()[[1]]
        })
        
        output$bs2 <- renderPlot({
                
                bs_graphs()[[2]]
        })
        
        output$bs1Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$bs1Click, outPut = 1, col = "basket_starts")
                
        }, options = list(dom = 't'))
        
        output$bs2Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$bs2Click, outPut = 2, col = "basket_starts")
                
        }, options = list(dom = 't'))
        
        # vs. ERRORS
        
        error_graphs <- reactive({
                
                plot_others("errors", df = myData(), conv = input$conversion, rate = input$rate, 
                            mediums = input$medium, pal = col_pal, weekRange = input$week, 
                            name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$error_Title1,
                            xlim1 = input$xlim, xlim2 = input$xlim2, bestfit = input$bf)
                
        })
        
        output$error1 <- renderPlot({
                
                error_graphs()[[1]]
        })
        
        output$error2 <- renderPlot({
                
                error_graphs()[[2]]
        })
        
        output$error1Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$error1Click, outPut = 1, col = "errors")
                
                
        }, options = list(dom = 't'))
        
        output$error2Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$error2Click, outPut = 2, col = "errors")
                
        }, options = list(dom = 't'))
        
        # vs. BOUNCES
        
        bounce_graphs <- reactive({
                
                plot_others("bounces", df = myData(), conv = input$conversion, rate = input$rate, 
                            mediums = input$medium, pal = col_pal, weekRange = input$week, 
                            name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$bounce_Title1,
                            xlim1 = input$xlim, xlim2 = input$xlim2, bestfit = input$bf)
                
        })
        
        output$bounce1 <- renderPlot({
                
                bounce_graphs()[[1]]
        })
        
        output$bounce2 <- renderPlot({
                
                bounce_graphs()[[2]]
        })
        
        output$bounce1Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$bounce1Click, outPut = 1, col = "bounces")
                
        }, options = list(dom = 't'))
        
        output$bounce2Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$bounce2Click, outPut = 2, col = "bounces")
                
        }, options = list(dom = 't'))
        
        #vs. ORDERS
        
        order_graphs <- reactive({
                
                plot_others("orders", df = myData(), conv = input$conversion, rate = input$rate, 
                            mediums = input$medium, pal = col_pal, weekRange = input$week, 
                            name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$order_Title1,
                            xlim1 = input$xlim, xlim2 = input$xlim2, bestfit = input$bf)
                
        })
        
        output$order1 <- renderPlot({
                
                order_graphs()[[1]]
        })
        
        output$order2 <- renderPlot({
                
                order_graphs()[[2]]
        })
        
        output$order1Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$order1Click, outPut = 1, col = "orders")
                
        }, options = list(dom = 't'))
        
        output$order2Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$order2Click, outPut = 2, col = "orders")
                
        }, options = list(dom = 't'))
        
        #vs. REVENUE
        
        rev_graphs <- reactive({
                
                plot_others("revenue", df = myData(), conv = input$conversion, rate = input$rate, 
                            mediums = input$medium, pal = col_pal, weekRange = input$week, 
                            name_dict = name_dict, ylim1 = input$ylim, ylim2 = input$ylim2, pTitle = input$rev_Title1,
                            xlim1 = input$xlim, xlim2 = input$xlim2, bestfit = input$bf)
                
        })
        
        output$rev1 <- renderPlot({
                
                rev_graphs()[[1]]
        })
        
        output$rev2 <- renderPlot({
                
                rev_graphs()[[2]]
        })
        
        output$rev1Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$rev1Click, outPut = 1, col = "revenue")
        }, options = list(dom = 't'))
        
        output$rev2Nums <- renderDataTable({
                
                others_get_data(df = myData(), conv = input$conversion, rate = input$rate,
                                mediums = input$medium, weekRange = input$week, name_dict = name_dict,
                                click_input= input$rev2Click, outPut = 2, col = "revenue")
        }, options = list(dom = 't'))
        
        #CLOSING SESSION WITH EXIT
        
        session$onSessionEnded(stopApp)
        
        #Q&A
        
        observeEvent(input$showAns, {
                
                #show answer to question on title page
                show("hideAns")
                
        })
        
        #HIDING TABS WITH SELECTION
        
        #Hide extra tabs in the beginning to unclutter the tab bar
        
        observe({
                toggle(condition = input$eg_check, selector = "#navbar li a[data-value=eg_tab]")
        })
        observe({
                toggle(condition = input$week_check, selector = "#navbar li a[data-value=week_tab]")
        })
        observe({
                toggle(condition = input$visitors_check, selector = "#navbar li a[data-value=visitors_tab]")
        })
        observe({
                toggle(condition = input$visits_check, selector = "#navbar li a[data-value=visits_tab]")
        })
        observe({
                toggle(condition = input$pageviews_check, selector = "#navbar li a[data-value=pageviews_tab]")
        })
        observe({
                toggle(condition = input$basketstarts_check, selector = "#navbar li a[data-value=basketstarts_tab]")
        })
        observe({
                toggle(condition = input$errors_check, selector = "#navbar li a[data-value=errors_tab]")
        })
        observe({
                toggle(condition = input$bounces_check, selector = "#navbar li a[data-value=bounces_tab]")
        })
        observe({
                toggle(condition = input$orders_check, selector = "#navbar li a[data-value=orders_tab]")
        })
        observe({
                toggle(condition = input$revenue_check, selector = "#navbar li a[data-value=revenue_tab]")
        })
        observe({
                toggle(condition = input$explore, selector = "#navbar li a[data-value=explore]")
        })
        
        #RESET
        
        observeEvent(input$start_over, {
                
                reset("df_file")
                reset("week")
                reset("conversion")
                reset("rate")
                reset("ylim")
                reset("ylim2")
                reset("medium")
                
        })
        
}

shinyApp(ui, server)
