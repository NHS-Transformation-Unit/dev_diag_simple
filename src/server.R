library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

shinyServer(function(input, output) {
  
  simulate_pathway <- reactive({
    
    set.seed(input$seed)
    
    n_days <- input$n_days
    planned_cap <- input$planned_capacity
    emergency_cap <- input$emergency_capacity
    min_wait <- input$min_wait

# Convert parameters ------------------------------------------------------

    mu <- input$init_mean
    sigma <- input$init_sd
    sdlog <- sqrt(log(1 + (sigma^2 / mu^2)))
    meanlog <- log(mu) - 0.5 * sdlog^2
    
    id_counter <- 1


# Simulate Starting Waiting List ------------------------------------------

    planned <- data.frame(
      wait = rlnorm(input$init_pop, meanlog = meanlog, sdlog = sdlog),
      id = id_counter:(id_counter + input$init_pop - 1),
      day_referred = 0,
      type = "planned"
    )
    
    

# Create starting dataframes ----------------------------------------------

    initial_planned <- planned
    id_counter <- id_counter + input$init_pop
    
    emergency <- data.frame(wait = numeric(0),
                            id = numeric(0),
                            day_referred = numeric(0),
                            type = character(0))
    
    summary_stats <- data.frame(
      day = 1:n_days,
      start_waiting = integer(n_days),
      new_planned = integer(n_days),
      new_emergency = integer(n_days),
      seen_planned = integer(n_days),
      seen_emergency = integer(n_days),
      end_waiting = integer(n_days),
      q25 = numeric(n_days),
      median = numeric(n_days),
      q75 = numeric(n_days)
    )
    

# Loop for all simulation days --------------------------------------------

    
    for (day in 1:n_days) {
      
      # Simulate New Referrals and Emergencies
      new_referrals <- rpois(1, input$lambda)
      new_emergencies <- rpois(1, input$emergency_lambda)
      
      summary_stats$new_planned[day] <- new_referrals
      summary_stats$new_emergency[day] <- new_emergencies
      
      if (new_referrals > 0) {
        new_planned <- data.frame(
          wait = rep(0, new_referrals),
          id = id_counter:(id_counter + new_referrals - 1),
          day_referred = day,
          type = "planned"
        )
        id_counter <- id_counter + new_referrals
        planned <- bind_rows(planned, new_planned)
      }
      
      if (new_emergencies > 0) {
        new_em <- data.frame(
          wait = rep(0, new_emergencies),
          id = id_counter:(id_counter + new_emergencies - 1),
          day_referred = day,
          type = "emergency"
        )
        id_counter <- id_counter + new_emergencies
        emergency <- bind_rows(emergency, new_em)
      }
      
      # Increment waiting time
      planned$wait <- planned$wait + 1
      emergency$wait <- emergency$wait + 1
      
      # See Emergency Patients First
      to_diag_em <- emergency |>
        arrange(desc(wait)) |>
        slice_head(n = emergency_cap + planned_cap)
      
      use_em_cap <- min(nrow(to_diag_em), emergency_cap)
      overflow <- max(0, nrow(to_diag_em) - emergency_cap)
      use_plan_cap <- min(overflow, planned_cap)
      
      actual_em_diag <- to_diag_em |> slice_head(n = use_em_cap + use_plan_cap)
      summary_stats$seen_emergency[day] <- nrow(actual_em_diag)
      emergency <- anti_join(emergency, actual_em_diag, by = "id")
      
      # See Planned Patients
      remaining_planned_cap <- planned_cap - use_plan_cap
      
      eligible_planned <- planned |> filter(wait >= min_wait)
      to_diag_planned <- eligible_planned |>
        arrange(desc(wait)) |>
        slice_head(n = remaining_planned_cap)
      
      summary_stats$seen_planned[day] <- nrow(to_diag_planned)
      planned <- anti_join(planned, to_diag_planned, by = "id")
      
      # Daily Summary Calculations for Outputs
      if (day == 1) {
        summary_stats$start_waiting[day] <- input$init_pop
      } else {
        summary_stats$start_waiting[day] <- summary_stats$end_waiting[day - 1]
      }
      
      summary_stats$end_waiting[day] <-
        summary_stats$start_waiting[day] +
        summary_stats$new_planned[day] +
        summary_stats$new_emergency[day] -
        summary_stats$seen_planned[day] -
        summary_stats$seen_emergency[day]
      
      all_waiting <- bind_rows(planned, emergency)
      if (nrow(all_waiting) > 0) {
        summary_stats$q25[day] <- quantile(all_waiting$wait, 0.25)
        summary_stats$median[day] <- median(all_waiting$wait)
        summary_stats$q75[day] <- quantile(all_waiting$wait, 0.75)
      }
    }
    
    list(
      summary = summary_stats,
      initial_planned = initial_planned
    )
  })
  
  output$start_hist <- renderPlot({
    sim <- simulate_pathway()
    planned <- sim$initial_planned
    
    ggplot(planned, aes(x = wait)) +
      geom_histogram(bins = 30, fill = "#6CC24A", color = "white") +
      labs(
        title = "Starting Waiting Times (Planned Patients)",
        x = "Days waited at start",
        y = "Number of patients") +
      theme(axis.text = element_text(size = 11),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 16, color = "#407EC9"),
            plot.subtitle = element_text(size = 12),
            panel.background = element_rect(fill = "#ffffff"),
            panel.grid.major.y = element_line(color = "#cecece", linewidth = 0.1),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(color = "#cecece", linewidth = 0.1),
            axis.line = element_line(color = "#000000"),
            legend.position = "right",
            legend.text = element_text(size = 12)
      )
  })
  
  output$waitlist_plot <- renderPlot({
    stats <- simulate_pathway()$summary
    
    ggplot(stats, aes(x = day, y = end_waiting)) +
      geom_line(color = "#8A1538") +
      labs(title = "Waiting List Size Across Simulation",
           x = "Day",
           y = "Patients Waiting") + 
      theme(axis.text = element_text(size = 11),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 16, color = "#407EC9"),
            plot.subtitle = element_text(size = 12),
            panel.background = element_rect(fill = "#ffffff"),
            panel.grid.major.y = element_line(color = "#cecece", linewidth = 0.1),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(color = "#cecece", linewidth = 0.1),
            axis.line = element_line(color = "#000000"),
            legend.position = "right",
            legend.text = element_text(size = 12)
      )
  })
    

  
  output$wait_summary_plot <- renderPlot({
    stats <- simulate_pathway()$summary
    ggplot(stats, aes(x = day)) +
      geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#F68D2E", alpha = 0.4) +
      geom_line(aes(y = median), color = "#F68D2E", size = 1) +
      labs(title = "Summary of Waiting Times",
           subtitle = "Median and Interquartile Range for those still on waiting list",
           x = "Day",
           y = "Days Waited") +
      theme(axis.text = element_text(size = 11),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 16, color = "#407EC9"),
            plot.subtitle = element_text(size = 12),
            panel.background = element_rect(fill = "#ffffff"),
            panel.grid.major.y = element_line(color = "#cecece", linewidth = 0.1),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(color = "#cecece", linewidth = 0.1),
            axis.line = element_line(color = "#000000"),
            legend.position = "right",
            legend.text = element_text(size = 12)
      )
  })
  
  output$daily_table <- DT::renderDataTable({
    simulate_pathway()$summary |>
      mutate(
        q25 = round(q25, 1),
        median = round(median, 1),
        q75 = round(q75, 1)
      ) |>
      select(day, start_waiting, new_planned, new_emergency,
             seen_planned, seen_emergency, end_waiting,
             q25, median, q75) |>
      rename("Day" = 1,
             "Starting Waiting List" = 2,
             "New Referrals - Planned" = 3,
             "New Referrals - Emergency" = 4,
             "Planned Patients Seen" = 5,
             "Emergency Patients Seen" = 6,
             "End of Day Waiting List" = 7,
             "Lower Quartile Waiting Time (days)" = 8,
             "Median Waiting Time (days)" = 9,
             "Upper Quartile Waiting Time (days)" = 10,)
  }, options = list(pageLength = 25), rownames = FALSE)
  
  output$download_table <- downloadHandler(
    filename = function() {
      paste0("diagnostic_simulation_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(
        simulate_pathway()$summary |>
          mutate(
            q25 = round(q25, 1),
            median = round(median, 1),
            q75 = round(q75, 1)
          ) |>
          select(day, start_waiting, new_planned, new_emergency,
                 seen_planned, seen_emergency, end_waiting,
                 q25, median, q75),
        file,
        row.names = FALSE
      )
    }
  )
})
