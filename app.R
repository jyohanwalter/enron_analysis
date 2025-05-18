#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)
library(stringr)
library(wordcloud2)


# --- Load Dataset ---
load("E:/DSTI/R/Project/Submission/Enron_cleaned.RData")

custom_theme <- theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14)
  )

# set globally so all ggplots inherit this theme
theme_set(custom_theme)

# --- UI ---
ui <- fluidPage(
  titlePanel("Analysis of Enron Dataset"),
  HTML('<h4>This analysis uses the data beween 1999-01-01 and 2002-12-31.</h4>'),
  
  tabsetPanel(
    # Tab 1: Active Employees
    tabPanel("Active Employees",
             # Full-width activityTimePlot at the top
             fluidRow(
               column(12,
                      HTML('<h4 style="text-align:center;">Email Activity Over Time (Sender vs Recipient)</h4>'),
                      plotOutput("activityTimePlot")
               )
             ),
             br(),
             HTML('<p style="font-size:14px;">  This timeseries graph presents the daily number of emails sent and received throughout the entire Enron company from 1999 to 2002. We get this spike in communication during important business times. Recipient counts tend to be larger than sender counts, representing the fact that many emails are sent to more than one recipient. Significant slowdown of activity begins with Enron\'s financial problems in late 2001. </p>'),
             br(),
             hr(),
             fluidRow(
               column(12,
                      HTML('<h4 style="text-align:center;">Top Employee Visualization</h4>')
               )
             ),
             # Sidebar layout below the full-width plot
             sidebarLayout(
               sidebarPanel(
                 sliderInput("top_n", "Number of Top Employees", min = 5, max = 25, value = 10),
                 selectInput("activity_type", "Activity Type",
                             choices = c("Overall", "Sender", "Recipient"), selected = "Overall")
               ),
               mainPanel(
                 plotOutput("activityPlot", height = "600px")
               )
             ),
             br(),
             HTML('<p style="font-size:14px;"> Histogram of the most active employees as senders or recipients is shown above. Some of the executives are members of both top senders and top recipients. Jeff Dasovich (Employee) shows exceptional activity volume in both sending and receiving. <i>no.address@enron.com</i> appears as a top recipient despite being invalid. </p>'),
             br(),
             hr()
    ),
    
    # Tab 2: Status and Timeline
    tabPanel("Status Analysis & Msg Timeline",
             fluidRow(
               column(12,
                      HTML('<h4 style="text-align:center;">Count of Employees by Status</h4>'),
                      plotOutput("statusPlot"),
                      br(),
                      HTML('<p style="font-size:14px;"> Employees divided by organizational status/ title. Most user accounts are for Employees VPs and Directors. "N/A" category offers incomplete HR records </p>'),
                      hr(),
                      HTML('<p style="font-size:16px;"><strong>Note:</strong> Employee status = N/A and NULL are ignored for the analysis done below.</p>'),
                      br(),
                      HTML('<h4 style="text-align:center;">Email Activity Over Time by Individual Status</h4>'),
                      fluidRow(
                        column(3,
                               dateRangeInput("date_range", "Select Date Range:",
                                              start = as.Date("2001-01-01"),
                                              end = as.Date("2002-01-01"),
                                              min = min(as.Date(message_new$date), na.rm = TRUE),
                                              max = max(as.Date(message_new$date), na.rm = TRUE)
                               )
                        )
                      ),
                      uiOutput("statusGridUI"),
                      htmlOutput("eventLegend"),
                      br(),
                      br(),
                      HTML('<p style="font-size:14px;"> Smoothed timeseries of email overall by employee status are displayed in the above nine plots. There are distinctions between staff and executives communications (the CEO, President). Red color dotted lines indicate special events that occured in the company. </p>'),
                      br(),
                      hr(),
                      HTML('<h4 style="text-align:center;">Emails Sent by Status and Communication Type</h4>'),
                      plotOutput("stackedBarPlot"),
                      HTML('<p style="font-size:16px;"><strong>Note:</strong> "communication_type" identifies if an email was sent within the same organization ("internal") or to a different domain ("outside").</p>'),
                      hr(),
                      br(),
                      HTML('<p style="font-size:14px;"> Stacked bar chart that displays distribution of communication within Enron and communication with external domains.</br>Enron traffic: Most was within Enron (inside the Enron Network). The first anti-pattern Statistically speaking, trumping-insiders. Some (Trading, for example) have different internal/ external breakdowns.</p>'),
               )
             )
    ),
    
    # Tab3: Basic analysis of Message content
    tabPanel("Message Content Analysis",
       # Word Cloud - Full Width
       
       # Email Sentiment Plot - Full Width
       fluidRow(
         column(12,
                HTML('<h4 style="text-align:center;">Email Sentiment Over Time</h4>'),
                plotOutput("sentimentPlot", height = "600px")
         )
       ),
       br(),
       HTML('<p style="font-size:14px;">Net sentiment (number of positve words - number of negative words) in email body over time. Most other times the tone has been upbeat. Observable dips in conjunction with reported crisis events early 2002 recovery efforts seen before bankruptcy. </p>'),
       br(),
       hr(),
       
       # Row: Positive Table, Negative Table, and Pie Chart
       fluidRow(
         column(
           width = 4,
           div(style = "text-align: center;",
               h4("Top Positive Words"),
               div(style = "display: inline-block;", tableOutput("positiveTable"))
           )
         ),
         column(
           width = 4,
           div(style = "text-align: center;",
               h4("Top Negative Words"),
               div(style = "display: inline-block;", tableOutput("negativeTable"))
           )
         ),
         column(
           width = 4,
           div(style = "text-align: center;",
               h4("Sentiment Pie Chart"),
               plotOutput("sentimentPie", height = "400px"))
         )
       ),
       br(),
       HTML('<p style="font-size:14px;"> Most common positive/ negative words and overall sentiment distribution displayed as tables. Words that move in the positive direction are generally business like (“well”, “good”). Unwelcome words can also be associated with issues (“issue”, "strictly"). Sentiment was, on the whole, astonishingly positive even during the collapse.</p>'),
       br()
    ),
    
    
    # Tab4: Basic analysis of Message content
    tabPanel("Word Frequency Display",
             # Word Cloud - Full Width
             HTML('<h4 style="text-align:center;">Word Cloud of Email Content</h4>'),
             wordcloud2Output("wordCloud", height = "600px"),
             HTML('<p style="font-size:14px;">  Most frequent words contained in the emails (stop words are eliminated for simplicity) are displayed above. Dominant terms like "meeting", "review", and "market" reveal the formal, business-oriented nature of internal correspondence. Frequent appearance of "contract", "agreement" and "confidential" suggests heightened attention to legal matters during this period. The prominence of "2001" and "2002" visually underscores how communication intensity tracked with the crisis timeline. </p>'),
             br(),
             hr()
    ),
    
    # Tab5: Conclusion
    tabPanel("Conclusion of the Analysis",
             # Heading
             HTML('<h4 style="text-align:center;"><strong>Conclusion</strong></h4>'),
             br(),
             # Paragraph text
             HTML('<p style="font-size:14px;"> 
     This effort gave a detailed study of the Enron email dataset, examining relationships between employees during dark days of the company (1999–2002). Using an interactive Shiny application we made it possible to explore various aspects of the data, such as the most active employees, the role, and behavior of individuals by their organizational status, and the temporal dynamics of email activity with relation to significant news events such as the SEC investigation, and Enron’s bankruptcy event. </br></br>
     Our findings represented major shifts in communication dynamics during times of organisational tension, demonstrating the utility of network analysis, temporal shifts, and content review in understanding internal dynamics. The initial email content analysis also highlighted some tone changes and internal sentiments over time.</br></br>
     The R Markdown file provided the data for the methodology, procesing, and visualizations, concluding the supported results with statistical outputs and commentary. In the end this piece of work has, to my mind, shown how a technical proficiency in R, Shiny and complex relational data is also very much a manifestation of a company-investigated-and-collapse when considered in light of the wider structural, social and ethical phenomena that we can see at work in digital communication traces. 
     </p>'),
             
             # Add Enron logo image centered below
             HTML('
          <div style="text-align:center; padding-top:20px;">
            <img src="enron_logo.png" height="100px" alt="Enron logo not found." onerror="this.onerror=null;this.outerHTML=\'<p style=&quot;color:red;&quot;>Enron logo not found.</p>\';">
          </div>
        '),
             br(),
             hr()
    )
  )
)


# --- Server ---
server <- function(input, output) {
  
  # Tab 1: Activity Plot
  output$activityTimePlot <- renderPlot({
    # Prepare sender and recipient data frames
    sender_df <- activity_sender_time %>%
      group_by(date) %>%
      summarise(total = sum(n, na.rm = TRUE)) %>%
      mutate(type = "Sender")
    
    recipient_df <- activity_recipient_time %>%
      group_by(date) %>%
      summarise(total = sum(n, na.rm = TRUE)) %>%
      mutate(type = "Recipient")
    
    # Combine them
    combined_df <- bind_rows(sender_df, recipient_df)
    
    # Stacked bar chart with custom colors
    ggplot(combined_df, aes(x = date, y = total, fill = type)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Sender" = "#FFE94D", "Recipient" = "#FF991C")) +
      labs(x = "Date", y = "Email Count", fill = "Type")
  })
  
  output$activityPlot <- renderPlot({
    activity_counts <- switch(input$activity_type,
                              "Sender" = activity_sender,
                              "Recipient" = activity_recipient,
                              "Overall" = activity_overall
    )
    
    # Create a data frame column for fill mapping
    activity_counts <- activity_counts %>%
      mutate(activity_type = input$activity_type)
    
    # Define colors for types
    fill_colors <- c("Sender" = "#FFE94D",
                     "Recipient" = "#FF991C",
                     "Overall" = "#009E73")
    
    activity_counts %>%
      arrange(desc(n)) %>%
      slice_head(n = input$top_n) %>%
      ggplot(aes(x = reorder(email, n), y = n, fill = activity_type)) +  # map fill to activity_type
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = fill_colors) +
      labs(
        title = paste("Top", input$top_n, input$activity_type, "Employees"),
        x = "Email", y = "Message Count",
        fill = NULL  # Optional: remove legend title
      )
  })
  
  # Tab 2: Static Plots
  output$statusPlot <- renderPlot({
    employeelist_new %>%
      count(status) %>%
      ggplot(aes(x = reorder(status, -n), y = n, fill = status)) +
      geom_col() +
      labs(title = "", x = "Status", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c(
        "Employee" = "#A3A601",
        "N/A" = "#00B0F6",
        "Vice President" = "#FF62BD",
        "Director" = "#D89000",
        "Manager" = "#00BF7C",
        "Trader" = "#E86BF3",
        "CEO" = "#F9766E",
        "President" = "#9490FF",
        "Managing Director" = "#00BFC4",
        "In House Lawyer" = "#39B700"
      ))
  })
  
  # Dynamic Grid UI
  output$statusGridUI <- renderUI({
    top_statuses <- employeelist_new %>%
      filter(!is.na(status), status != "N/A") %>%
      count(status, sort = TRUE) %>%
      slice_head(n = 9) %>%
      pull(status)
    
    plot_rows <- lapply(seq(1, 9, 3), function(i) {
      fluidRow(
        lapply(i:(i+2), function(j) {
          if (j <= length(top_statuses)) {
            column(4, plotOutput(outputId = paste0("statusPlot_", j)))
          } else {
            column(4)
          }
        })
      )
    })
    
    do.call(tagList, plot_rows)
  })
  
  # Dynamic Plot Rendering
  observe({
    # Get the selected date range from the UI input
    date_range_selected <- input$date_range
    
    top_statuses <- employeelist_new %>%
      filter(!is.na(status), status != "N/A") %>%
      count(status, sort = TRUE) %>%
      slice_head(n = 9) %>%
      pull(status)
    
    status_colors <- c(
      "Employee" = "#A3A601",
      "Vice President" = "#FF62BD",
      "Director" = "#D89000",
      "Manager" = "#00BF7C",
      "Trader" = "#E86BF3",
      "CEO" = "#F9766E",
      "President" = "#9490FF",
      "Managing Director" = "#00BFC4",
      "In House Lawyer" = "#39B700"
    )
    
    default_color <- "#999999"
    
    # Filter enron_events to only include events within the selected date range
    filtered_enron_events <- enron_events %>%
      filter(Date >= date_range_selected[1] & Date <= date_range_selected[2])
    # Add an index to the filtered events
    filtered_enron_events$Index <- seq_len(nrow(filtered_enron_events))
    
    for (i in seq_along(top_statuses)) {
      local({
        my_i <- i
        my_status <- top_statuses[my_i]
        my_color <- status_colors[my_status] %||% default_color
        
        output[[paste0("statusPlot_", my_i)]] <- renderPlot({
          plot_data <- message_new %>%
            left_join(employeelist_new, by = c("sender" = "email")) %>%
            filter(status == my_status) %>%
            mutate(date = as.Date(date)) %>%
            filter(!is.na(date)) %>%
            filter(date >= date_range_selected[1], date <= date_range_selected[2]) %>%
            count(date)
          
          base_plot <- ggplot(plot_data, aes(x = date, y = n)) +
            geom_smooth(method = "loess", span = 0.3, se = FALSE, color = my_color, size = 1) +
            labs(title = paste("Status:", my_status), x = "Date", y = "Emails") +
            theme_minimal()
          
          # Add vertical lines and index labels
          for (idx in seq_len(nrow(filtered_enron_events))) {
            event_date <- filtered_enron_events$Date[idx]
            event_label <- as.character(filtered_enron_events$Index[idx])
            
            base_plot <- base_plot +
              geom_vline(xintercept = event_date, color = "red", linetype = "dashed", linewidth = 1) +
              annotate("text", x = event_date, 
                       y = max(plot_data$n, na.rm = TRUE) * 0.9,  # <- Controlled vertical position
                       label = event_label, vjust = -0.5, color = "red", angle = 90, hjust = 0.5)
          }
          base_plot
        })
      })
    }
  })
  
  output$eventLegend <- renderUI({
    date_range_selected <- input$date_range
    
    filtered_enron_events <- enron_events %>%
      filter(Date >= date_range_selected[1], Date <= date_range_selected[2]) %>%
      mutate(Index = seq_len(n()))
    
    if (nrow(filtered_enron_events) == 0) return(NULL)
    
    legend_items <- paste0(
      "<strong>[", filtered_enron_events$Index, "]</strong> ", 
      filtered_enron_events$Event, 
      " (", format(filtered_enron_events$Date, "%Y-%m-%d"), ")"
    )
    
    HTML(paste("<div style='font-size:14px;'>", 
               "<h5 style='margin-top:1em;'>Event Legend:</h5>", 
               paste(legend_items, collapse = "<br>"),
               "</div>"))
  })
  
  output$stackedBarPlot <- renderPlot({
    email_edges %>%
      left_join(employeelist_new, by = c("sender" = "email")) %>%
      filter(status != "N/A") %>%
      count(status, communication_type) %>%
      ggplot(aes(x = reorder(status, -n), y = n, fill = communication_type)) +
      geom_col() +
      labs(
        title = "",
        x = "Employee Status", 
        y = "Number of Emails"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),  # <- h4 style
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_fill_manual(values = c("internal" = "#FFCC17", "outside" = "#63BDBD"))
  })
  
  # Tab3: Basic analysis of Message content
  output$sentimentPlot <- renderPlot({
    if (nrow(sentiment_time_series) == 0) return(NULL)
    ggplot(sentiment_time_series, aes(x = date, y = net_sentiment)) +
      geom_line(color = "#009E73") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(title = "", x = "Date", y = "Net Sentiment")
  })
  
  output$wordCloud <- renderWordcloud2({
    word_freq <- message_reference %>%
      filter(!is.na(message_text)) %>%
      unnest_tokens(word, message_text) %>%
      anti_join(stop_words, by = "word") %>%
      count(word, sort = TRUE)
    
    wordcloud2(data = word_freq, size = 0.7)
    
  })
  
  # Top Sentiment Words Table
  # Precompute sentiment counts
  bing_sentiments_unique <- get_sentiments("bing") %>%
    distinct(word, .keep_all = TRUE)
  
  # Precompute sentiment counts
  sentiment_word_counts <- message_reference %>%
    filter(!is.na(message_text)) %>%
    unnest_tokens(word, message_text) %>%
    inner_join(bing_sentiments_unique, by = "word") %>%
    count(word, sentiment, sort = TRUE)
  
  
  # Positive Table
  output$positiveTable <- renderTable({
    sentiment_word_counts %>%
      filter(sentiment == "positive") %>%
      slice_max(n, n = 10) %>%
      select(Word = word, Count = n)
  })
  
  # Negative Table
  output$negativeTable <- renderTable({
    sentiment_word_counts %>%
      filter(sentiment == "negative") %>%
      slice_max(n, n = 10) %>%
      select(Word = word, Count = n)
  })
  
  # Sentiment Pie Chart
  output$sentimentPie <- renderPlot({
    sentiment_counts <- sentiment_word_counts %>%
      count(sentiment, wt = n)
    
    ggplot(sentiment_counts, aes(x = "", y = n, fill = sentiment)) +
      geom_col(width = 1) +
      coord_polar("y") +
      theme_void() +
      scale_fill_manual(values = c("positive" = "#8CFF8C", "negative" = "#FF8C8C")) +
      theme(
        legend.text = element_text(size = 14),   # Adjust size as needed
        legend.title = element_blank()           # Optional: remove legend title
      )
  })
  
}

# --- Run App ---
shinyApp(ui = ui, server = server)