### SHINY APP

#load libraries
library(tidyverse)
library(shiny)
library(nwslR)

# theme set before visualizations
theme_set(theme_bw() +
            theme(axis.text = element_text(size = 12, face = "bold"),
                  axis.title = element_text(size = 16),
                  panel.border = element_blank(),
                  panel.grid.major = element_line(color = "grey")))

# create a ranking function
percentile <- function(x){
  a = (rank(x, ties.method = "average") / length(x)) * 100
  a
}


# join nwsl tables together
adv_stats <- adv_team_stats %>% 
  filter(season == "2019") %>% 
  select(team_id, season, game_id) %>%
  left_join(adv_player_stats, by = c("team_id", "game_id"))

# do some data cleaning to better work with the dataset
season_stats <- adv_stats %>%
  mutate(player = paste(first_name, last_name, sep = " ")) %>%
  relocate(player, .after = season) %>% 
  select(-first_name, -last_name) %>%
  unnest(season) %>%
  filter(!(position %in% c("Goalkeeper")))

# create some summary statistics for investigation and visualization
nwsl_stats <-  season_stats %>%
  group_by(player) %>%
  summarize(across(c("mins_played", "touches", "accurate_pass","goal_assist", "turnover", "duel_lost", "won_tackle", "duel_won", "total_shots", "shots_on_goal", "goals"), ~sum(.x))) %>% 
  filter(mins_played >= 760) %>%
  mutate(goals_MP = round(goals / mins_played, 2), 
         accPass_touch = round(accurate_pass / touches, 2),
         wonTackle_MP = round(won_tackle / mins_played, 2),
         duel_ratio = duel_won / duel_lost,
         duelWon_MP = round(duel_won/ mins_played, 2),
         asst_pass = goal_assist / accurate_pass,
         turnover_MP = round(turnover / mins_played, 2),
         shots_touch= round(total_shots / touches , 2),
         goals_touch = goals / touches,
         goals_target = round(goals / shots_on_goal, 2),
         across(c("goals_MP":"goals_target"), list(pctl = percentile))
  ) %>% 
  ungroup()

# create a pivot longer helper function
pivot_summarized_values <- function(.data, ...){
  .data %>%
    select(...) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "value"
    )
}

# ui
ui <- fluidPage(
  
  sidebarPanel(
    width = 3,
    
    selectInput(inputId = "player",
                label = "player",
                choices = unique(nwsl_stats$player),
                selected = "Christine Sinclair")
    
  ),
  
  mainPanel(
    plotOutput(outputId = "plt_score"),
    plotOutput(outputId = "plt_def")
  )
)


server <- function(input, output){
  
  
  player_stats <- reactive({
    
    nwsl_stats %>%
      filter(player == input$player)
    
  })
  
  
  scoring_stats <- reactive({
    
    player_stats() %>%
      rename(
        goals_MP_value = goals_MP,
        accPass_touch_value = accPass_touch,
        shots_touch_value = shots_touch,
        goals_target_value = goals_target
      ) %>%
      pivot_summarized_values(
        goals_MP_value,
        accPass_touch_value, 
        shots_touch_value,
        goals_target_value,
        goals_MP_pctl,
        accPass_touch_pctl,
        shots_touch_pctl,
        goals_target_pctl
      ) %>%
      mutate(
        stat = gsub("(.*_.*)_.*$", "\\1", variable),
        type = gsub(".*_.*_(.*)$", "\\1", variable)
      ) %>% 
      pivot_wider(
        id_cols = stat,
        names_from = type,
        values_from = value
      ) %>%
      unnest() %>%
      mutate(
        stat = str_replace_all(stat, "[_]", "/"),
        stat = factor(stat, rev(unique(stat))),
        color = case_when(pctl > 75 ~ "Good",
                          pctl < 45 ~ "Bad",
                          TRUE ~ "Average"),
        color = factor(color, levels = c("Bad","Average","Good"))
      )
  })
  
  
  defense_stats <- reactive({
    
    player_stats() %>%
      rename(
        wonTackle_MP_value = wonTackle_MP,
        duelWon_MP_value = duelWon_MP,
        turnover_MP_value = turnover_MP
      ) %>%
      pivot_summarized_values(
        wonTackle_MP_value,
        duelWon_MP_value,
        turnover_MP_value,
        wonTackle_MP_pctl,
        duelWon_MP_pctl,
        turnover_MP_pctl
      ) %>%
      mutate(
        stat = gsub("(.*_.*)_.*$", "\\1", variable),
        type = gsub(".*_.*_(.*)$", "\\1", variable)
      ) %>% 
      pivot_wider(
        id_cols = stat,
        names_from = type,
        values_from = value
      ) %>%
      unnest() %>%
      mutate(
        stat = str_replace_all(stat, "[_]", "/"),
        stat = factor(stat, rev(unique(stat))),
        color = case_when(pctl > 75 ~ "Good",
                          pctl < 45 ~ "Bad",
                          TRUE ~ "Average"),
        color = factor(color, levels = c("Bad","Average","Good"))
      )
  })
  
  output$plt_score <- renderPlot({
    
    scoring_stats() %>%
      mutate(
        stat_pos = as.factor(stat)
      ) %>% 
      ggplot() + 
      theme_void() +
      theme(legend.position = "none") +
      geom_text(
        aes(x = .1, y = stat_pos,label = stat),
        fill = "black",
        label.size = 0,
        label.padding = unit(0.1, "lines"),
        hjust = 0,
        size = 8
      ) +
      geom_text(
        aes(x = 1.9, y = stat_pos,label = value),
        fill = "black",
        label.size = 0,
        label.padding = unit(0.1, "lines"),
        hjust = 1,
        size = 8
      ) + 
      
      ## add table lines - vertical
      annotate("segment", 
               x = c( 0, 1, 2, 2.25, 4.25), 
               xend = c( 0, 1, 2, 2.25, 4.25),
               y = .5, yend = 4.5, size = 0.3) +
      
      ## add table lines - horizontal
      annotate("segment",
               y = c(.5, 1.5, 2.5, 3.5, 4.5),
               yend = c(.5, 1.5, 2.5, 3.5, 4.5),
               x = c(0), 
               xend = c(2),
               size = 1) +
      annotate("segment",
               y = c( 4.5),
               yend = c( 4.5),
               x = c(0), 
               xend = c(2),
               size = 2) +
      annotate("segment",
               y = c(.5, 1.5, 2.5, 3.5, 4.5),
               yend = c(.5, 1.5, 2.5, 3.5, 4.5),
               x = c(2.25), 
               xend = c(4.25),
               size = 1) +
      annotate("segment",
               y = c(4.5),
               yend = c(4.5),
               x = c(2.25), 
               xend = c(4.25),
               size = 2) +
      
      ## plot vertical lines
      annotate("segment", 
               x = c( 2.75, 3.75), 
               xend = c( 2.75, 3.75),
               y = .5, yend = 4.5, size = 0.2, color = "grey") +
      annotate("segment", 
               x = c( 3.25), 
               xend = c(3.25 ),
               y = .5, yend = 4.5, size = 2, color = "grey") +
      annotate("text", 
               x = c( 3.25), 
               y = 4.7, 
               label = "50%",
               size = 5, color = "grey") +
      
      ## plot percentiles
      geom_point(
        aes(x = (pctl / 50) + 2.25, y = stat_pos,
            fill = color,
            size = 10),
        color = "black",
        shape = 21
      ) +
      
      scale_fill_manual(
        breaks = c("Bad","Average","Good"),
        values = c("red","white","blue")
      ) +
      
      geom_text(
        data = data.frame(
          lab = c("Shooting Statistics","Shooting Percentile"),
          x = c(0,2.25),
          y = c(5,5)
        ),
        aes(x = x, y = y, label = lab),
        hjust = 0,
        size = 10
      ) 
    
  })
  
  output$plt_def <- renderPlot({
    
    defense_stats() %>%
      mutate(
        stat_pos = as.factor(stat)
      ) %>% 
      ggplot() + 
      theme_void() +
      theme(legend.position = "none") +
      geom_text(
        aes(x =.1, y = stat_pos, label = stat),
        fill = "black",
        label.size = 0,
        label.padding = unit(0.1, "lines"),
        hjust = 0,
        size = 8
      ) +
      geom_text(
        aes(x = 1.9, y = stat_pos, label = value),
        fill = "black",
        label.size = 0,
        label.padding = unit(0.1, "lines"),
        hjust = 1,
        size = 8
      ) + 
      
      ## add table lines - vertical
      annotate("segment", 
               x = c( 0, 1, 2, 2.25, 4.25), 
               xend = c( 0, 1, 2, 2.25, 4.25),
               y = .5, yend = 3.5, size = 0.3) +
      
      ## add table lines - horizontal
      annotate("segment",
               y = c(.5, 1.5, 2.5, 3.5),
               yend = c(.5, 1.5, 2.5, 3.5),
               x = c(0), 
               xend = c(2),
               size = 1) +
      annotate("segment",
               y = c( 3.5),
               yend = c( 3.5),
               x = c(0), 
               xend = c(2),
               size = 2) +
      annotate("segment",
               y = c(.5, 1.5, 2.5, 3.5 ),
               yend = c(.5, 1.5, 2.5, 3.5 ),
               x = c(2.25), 
               xend = c(4.25),
               size = 1) +
      annotate("segment",
               y = c(3.5),
               yend = c(3.5),
               x = c(2.25), 
               xend = c(4.25),
               size = 2) +
      
      ## plot vertical lines
      annotate("segment", 
               x = c( 2.75, 3.75), 
               xend = c( 2.75, 3.75),
               y = .5, yend = 3.5, size = 0.2, color = "grey") +
      annotate("segment", 
               x = c( 3.25), 
               xend = c(3.25 ),
               y = .5, yend = 3.5, size = 2, color = "grey") +
      annotate("text", 
               x = c( 3.25), 
               y = 3.7, 
               label = "50%",
               size = 5, color = "grey") +
      
      ## plot percentiles
      geom_point(
        aes(x = (pctl / 50) + 2.25, y = stat_pos, fill = color,
            size = 10),
        color = "black",
        shape = 21
      ) +
      
      scale_fill_manual(
        breaks = c("Bad","Average","Good"),
        values = c("red","white","blue")
      ) +
      
      geom_text(
        data = data.frame(
          lab = c("Defensive Statistics","Defensive Percentile"),
          x = c(0,2.25),
          y = c(4,4)
        ),
        aes(x = x, y = y, label = lab),
        hjust = 0,
        size = 10
      ) 
    
  })
}


shinyApp(ui, server)
