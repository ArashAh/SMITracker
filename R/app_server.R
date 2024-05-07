#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd


app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 300*1024^2)

  #sourceCpp('R/MSDFunction.cpp')

  rv <- reactiveValues()


##### tab1 #####

  roots = c(wd = '../')
  shinyDirChoose(input, 'folder', roots = roots, filetypes = c('','json'))

  folder.path <- reactive({req(input$folder)
    as.character(parseDirPath(roots, input$folder))})


  output$folderpath <- renderText(list.files(path = folder.path())[1])
  #
  #output$folderpath <- renderText(getwd())

  observeEvent(input$load, {

    rv$collect.data <- readJsonFiles(paste((folder.path()),"/",sep = ""), nEnd = nchar(input$dataset.name.s))

    rv$collect.data <- TransformData(InputData = rv$collect.data,
                                     nameList = c(input$pos1, input$pos2, input$pos3,
                                                  input$pos4, input$pos5, input$pos6, input$pos7),
                                     separator = input$sep,
                                     numerName = c("frame.interval", input$checkGroup1))

    rv$collect.data <- rv$collect.data %>% mutate(analysis.id = input$analysis.id)

    show.data1 <<- rv$collect.data

    output$summary1.1 <- renderText(rv$collect.data %>%
                                      select(data.set.name) %>% unique() %>% nrow()%>% as.numeric())

    output$summary1.2 <- renderText(rv$collect.data %>%
                                      group_by(data.set.name) %>%
                                      summarise(frames = max(frame.number)- min(frame.number)) %>%
                                      summarise(sum(frames)) %>% as.numeric())

    output$summary1.3 <- renderText(rv$collect.data %>%
                                      group_by(data.set.name) %>%
                                      summarise(observations = n()) %>%
                                      summarise(sum(observations))%>% as.numeric())

    output$summary1.4 <- renderDataTable(rv$collect.data %>%
                                           group_by(data.set.name) %>%
                                           summarise(frames = max(frame.number)- min(frame.number), observations = n()))

  })

  shinyDirChoose(input,'folder2', roots=roots, filetypes=c('','json'))

  folder.path2 <- reactive({req(input$folder2)
    as.character(parseDirPath(roots, input$folder2))})


    output$folder.path.output <-
      output$folder.path.output2 <-
      output$folder.path.output3 <-
      output$folder.path.output4 <-
      output$folder.path.output5 <-renderText({
      folder.path2()

    }
    )
  observeEvent(input$save,{


    saveRDS(rv$collect.data, paste(folder.path2(),"/", "1_data_transformed_", rv$collect.data$analysis.id %>% unique(), "_",
                                   as.character(Sys.Date()), ".rds", sep = ""))

    # write.table(bind_rows(rv$collect.data %>%
    #               group_by(data.set.name) %>%
    #               summarise(frames = max(frame.number)- min(frame.number), observations = n()),
    #               rv$collect.data %>%
    #                 group_by(data.set.name) %>%
    #                 summarise(frames = max(frame.number)- min(frame.number), observations = n()) %>%
    #                 summarise(frames = sum(frames), observations = sum(observations)) %>%
    #               mutate(data.set.name = "Total")),
    #             paste("1_Summary_", rv$collect.data$analysis.id %>% unique(), "_",
    #                   as.character(Sys.Date()), ".txt", sep =""),
    #             sep = "\t", col.names = TRUE)



  })


  ##### tab2 ######

  my.data <- reactive({
    req(input$file1)
    readRDS(input$file1$datapath)})


  list.of.data.sets <-reactive(my.data() %>%
                                 select(data.set.name)%>% unique())
  my.data.filtered <- reactive(my.data() %>%
                                 filter(data.set.name == as.character(list.of.data.sets()[input$num,])))


  output$full.name <- renderText({ req(input$start)
    list.of.data.sets()[input$num,]})


  observeEvent(input$select, {
    rv$out.data1 <- brushedPoints(my.data.filtered(),
                                  input$plot_brush,
                                  allRows = TRUE)
    rv$collect.out.data <- bind_rows(rv$collect.out.data, rv$out.data1%>%
                                       mutate(sub.data.set.name = data.set.name , signal.type = "DNA trace") %>%
                                       rename("spatial.filter" = "selected_"))

    # saveRDS(rv$collect.out.data, paste("2_spatial_filtered_", rv$collect.out.data$analysis.id %>% unique(),
    #                                 "_", as.character(Sys.Date()), ".rds", sep = ""))

    rv$prog.rep <- rv$collect.out.data$data.set.name %>% unique()
    print(nrow(rv$collect.out.data))

  })

  observeEvent(c(input$num, input$start, input$reset),{
    rv$j = 0
    rv$i = 0
  })


  observeEvent(input$split, {
    rv$j = rv$j + 1
    print(rv$j)
    rv$out.data2 <- brushedPoints(my.data.filtered(),
                                  input$plot_brush,
                                  allRows = TRUE)
    rv$collect.out.data <- bind_rows(rv$collect.out.data, rv$out.data2 %>%
                                       mutate(sub.data.set.name = paste(data.set.name,"sp",rv$j,sep = ""),
                                              signal.type = "DNA trace") %>%
                                       rename("spatial.filter" = "selected_"))


    # saveRDS(rv$collect.out.data, paste("2_spatial_filtered_", rv$collect.out.data$analysis.id %>% unique(),
    #                                 "_", as.character(Sys.Date()), ".rds", sep = ""))

    rv$prog.rep <- rv$collect.out.data$data.set.name %>% unique()


    print(nrow(rv$collect.out.data))
  })



  observeEvent(input$stuck, {
    rv$i = rv$i + 1
    print(rv$i)
    rv$out.data3 <- brushedPoints(my.data.filtered(),
                                  input$plot_brush,
                                  allRows = TRUE)
    rv$collect.out.data <- bind_rows(rv$collect.out.data, rv$out.data3 %>%
                                       mutate(sub.data.set.name = paste(data.set.name,"st", rv$i,sep = ""),
                                              signal.type = "stuck") %>%
                                       rename("spatial.filter" = "selected_"))


    # saveRDS(rv$collect.out.data, paste("2_spatial_filtered_", rv$protein.name,
    #                                    "_", as.character(Sys.Date()), ".rds", sep = ""))

    rv$prog.rep <- rv$collect.out.data$data.set.name %>% unique()


    print(nrow(rv$collect.out.data))
  })

  observeEvent(input$next.data.set, {
    # Update the numericInput value to +1
    updateNumericInput(session, "num", value = input$num + 1)
  })

  observeEvent(input$reset, {

    rv$collect.out.data <- rv$collect.out.data %>%
      filter(data.set.name != as.character(list.of.data.sets()[input$num,]))

    output$plot2 <- renderPlot({
      my.data.filtered() %>%
        ggplot(aes(x=X, y=Y)) + geom_point(size = 0.1)+
        xlim(c(min(my.data.filtered()$X) -50,
               max(my.data.filtered()$X) +1050)) +
        ylim(c(min(my.data.filtered()$Y) -50,
               max(my.data.filtered()$Y) +50))+
        ggtitle("Raw data visualization")
    })

    # saveRDS(rv$collect.out.data, paste("2_spatial_filtered_", rv$collect.out.data$analysis.id %>% unique(),
    #                                 "_", as.character(Sys.Date()), ".rds", sep = ""))

    rv$prog.rep <- rv$collect.out.data$data.set.name %>% unique()
    print(nrow(rv$collect.out.data))

  })

  output$progress <-renderDataTable({as.data.frame(rv$prog.rep)})


  observeEvent(c(input$num, input$start,  input$select, input$split, input$reset, input$stuck), {
    if (as.character(list.of.data.sets()[input$num,]) %in% rv$prog.rep)
    {
      output$plot2 <- renderPlot({
        rv$collect.out.data %>%
          filter(data.set.name == as.character(list.of.data.sets()[input$num,]),
                 spatial.filter) %>%
          ggplot() + geom_point(aes(x=X, y=Y, color =signal.type), size = 0.1)+
          xlim(c(min(my.data.filtered()$X) -50,
                 max(my.data.filtered()$X) +200)) +
          ylim(c(min(my.data.filtered()$Y) -50,
                 max(my.data.filtered()$Y) +50))+
          ggtitle("Spatially filtered data sets")
      })
    }

    else
    {

      output$plot1 <- renderPlot({
        my.data.filtered() %>%
          ggplot(aes(x=X, y=Y)) + geom_point(size = 0.1, alpha = 0.2) +
          xlim(c(min(my.data.filtered()$X) -50,
                 max(my.data.filtered()$X) +1050)) +
          ylim(c(min(my.data.filtered()$Y) -50,
                 max(my.data.filtered()$Y) +50))+ ggtitle("Raw data visualization")

      })
    }
  })


  observeEvent(input$intensity, {
    output$intensity.plot <- renderPlot({
      my.data() %>% ggplot()+
        geom_density(aes(x = intensity), color = "black")+
        geom_density(data = rv$collect.out.data %>%
                       filter(spatial.filter), aes(x = intensity, color = signal.type)) +
        scale_x_log10(breaks= c(10,100,1000,10000), limits = c(1, 100000)) +
        ggtitle("Distribution of intensity of the localized data")})


  })



  observeEvent(input$done, {

    rv$collect.out.data.to.save <- rv$collect.out.data %>%
      mutate(intensity.filter = ifelse(intensity < input$min.intensity |
                                         intensity > input$max.intensity , FALSE, TRUE )) %>%
      filter(spatial.filter, intensity.filter) %>%
      select(-spatial.filter, - intensity.filter)

    show.data2 <<-rv$collect.out.data.to.save


    saveRDS(rv$collect.out.data.to.save, paste(folder.path2(),"/","2_spatial_filtered_", rv$collect.out.data$analysis.id %>% unique(),
                                               "_", as.character(Sys.Date()), ".rds", sep = ""))

    # output$summary3 <- renderDataTable ({
    # rv$collect.out.data %>%
    # group_by(data.set.name, spatial.filter, intensity.filter) %>%
    # summarise(frames = length(unique(frame.number)), observations = n()) %>%
    # ungroup() %>%
    # group_by(spatial.filter, intensity.filter) %>%
    # summarise(data.sets = n(), frames = sum(frames), observations = sum(observations))
    # })
    #
    # write.table(rv$collect.out.data %>%
    #               group_by(data.set.name, spatial.filter, intensity.filter) %>%
    #               summarise(frames = length(unique(frame.number)), observations = n()) %>%
    #               ungroup() %>%
    #               group_by(spatial.filter, intensity.filter) %>%
    #               summarise(data.sets = n(), frames = sum(frames), observations = sum(observations)),
    #               paste("2_Summary_", rv$collect.out.data$analysis.id %>% unique(), "_", as.character(Sys.Date()), ".txt", sep =""),
    #               sep = "\t", col.names = TRUE)

  })

  ##### tab3 #####


  filtered.data <- reactive({
    req(input$file2)
    readRDS(input$file2$datapath)})




  observeEvent(c(input$detect, input$reanalyse), {
    rv$collect.detected.data.d <- detectTrajectories(filteredDataSet = filtered.data() %>%
                                                       filter(signal.type != "stuck") ,
                                                     dxMax = input$max.dx,
                                                     dyMax = input$max.dy)

    # rv$prog.reporter <- rv$collect.detected.data.dd[[1]]
    # rv$collect.detected.data.d <- rv$collect.detected.data.dd[2]
    #
    # output$summary4 <- renderDataTable ({
    #   rv$collect.detected.data %>%
    #     group_by(data.set.name, duration.filter, displacement.filter, large.displacement.filter ) %>%
    #     summarise(frames = length(unique(frame.number)), observations = n()) %>%
    #     ungroup() %>%
    #     group_by(duration.filter, displacement.filter, large.displacement.filter) %>%
    #     summarise(data.sets = n(), frames = sum(frames), observations = sum(observations))
    # })

    rv$collect.detected.data.s <- detectStuck(filteredDataSet = filtered.data() %>%
                                                filter(signal.type == "stuck") ,
                                              dxMax = 2*input$max.dx.2,
                                              dyMax = 2*input$max.dx.2)

    # rv$prog.reporter <- rv$collect.detected.data.ss[[1]]
    # rv$collect.detected.data.s <- rv$collect.detected.data.ss[2]



    rv$collect.detected.data <- bind_rows(rv$collect.detected.data.d, rv$collect.detected.data.s) %>%
      arrange(data.set.name) %>% filter(!is.na(sub.data.set.name))


    rv$collect.detected.data %<>% AddDurationFilter(minFrameDuration = input$min.frame.duration)

    rv$collect.detected.data %<>% AddVisualInspection(TrajsToInspect = input$trajs.to.inspect)


    show.data3 <<- rv$collect.detected.data


    output$plot31 <- renderPlot({ rv$collect.detected.data %>%
        group_by(sub.data.set.name, trajectory.id) %>%
        mutate(x.displacement.per.frame = lead(X, n =1) - X,
               y.displacement.per.frame = lead(Y, n =1) - Y) %>% ungroup() %>%
        filter(!is.na(x.displacement.per.frame)) %>%
        select(x.displacement.per.frame, y.displacement.per.frame, signal.type) %>%
        pivot_longer(cols =  1:2, names_to = "direction.of.displacement", values_to = "displacement") %>%
        ggplot()+
        geom_histogram(aes(x= displacement, fill= signal.type))+
        facet_wrap(~direction.of.displacement)+
        xlab("Displacement per frame (nm)")+ ylab("Counts (frames)") +
        ggtitle("Distribution of displacement per frames")


    })




    output$plot32 <- renderPlot({ rv$collect.detected.data %>%
        group_by(sub.data.set.name, trajectory.id, visual.inspection) %>%
        summarise(x.range = max(X)- min(X)) %>%
        ggplot() + geom_histogram(aes(x= x.range, fill= visual.inspection))+
        xlab("Range per trajectory (nm)")+ ylab("Counts (frames)") +
        ggtitle("Distribution of range of the detected movements along DNA")
    })

    rv$limits <-  rv$collect.detected.data %>% filter(signal.type != "stuck") %>%
      group_by(sub.data.set.name, trajectory.id) %>%
      mutate(x.step = lead(X, n =1) - X, y.step = lead(Y, n =1) - Y) %>%
      filter(!is.na(x.step), !is.na(y.step)) %>%
      mutate(x.limit.check = ifelse(abs(x.step) > 0.8 * input$max.dx, TRUE, FALSE)) %>%
      mutate(y.limit.check = ifelse(abs(y.step) > 0.8 * input$max.dy, TRUE, FALSE)) %>%
      ungroup() %>%
      summarise(xlimit = sum(x.limit.check)/n(), ylimit = sum(y.limit.check)/n())
  })

  # output$prog.reporter <-  renderText({rv$prog.reporter})

  output$eval <-  renderText({ req(input$detect)
    c (if (rv$limits$xlimit > 0.01) {
      print("Increase max displacement along DNA by ~20 % and re-analyse")},
      if(rv$limits$ylimit > 0.01) {
        print("Increase max displacement accros DNA by ~20 % and re-analyse")},
      if(rv$limits$xlimit < 0.01 & rv$limits$ylimit < 0.01  ) {
        print("Max displacements approved!")}
    )
  })

  list.of.data.sets2 <-reactive(filtered.data() %>% select(data.set.name)%>% unique())

  output$full.name2 <- renderText({req(input$show)
    list.of.data.sets2()[input$num2,]})

  observeEvent(input$show, {

    output$plot33 <- renderPlot({
      ggplot()+
        geom_point(data = filtered.data() %>%
                     filter(data.set.name == as.character(list.of.data.sets2()[input$num2,])),
                   aes(x = X, y= Y), alpha = 0.2, color = "black") +
        geom_point(data = rv$collect.detected.data %>%
                     filter(data.set.name == as.character(list.of.data.sets2()[input$num2,]), duration.filter),
                   aes(x = X, y= Y, color = signal.type), alpha = 1)+
        xlim(c(min(rv$collect.detected.data %>%
                     filter(data.set.name == as.character(list.of.data.sets2()[input$num2,])) %>% "$" (X)) -1000,
               max(rv$collect.detected.data %>%
                     filter(data.set.name == as.character(list.of.data.sets2()[input$num2,])) %>% "$" (X)) +1000)) +
        ylim(c(min(rv$collect.detected.data %>%
                     filter(data.set.name == as.character(list.of.data.sets2()[input$num2,])) %>% "$" (Y)) -1000,
               max(rv$collect.detected.data %>%
                     filter(data.set.name == as.character(list.of.data.sets2()[input$num2,])) %>% "$" (Y)) +1000))
    })
  }
  )




  observeEvent(input$save2, {

    print("Hi")
    #saveRDS(rv$collect.detected.data, "3_trajectories_detected.rds")

    saveRDS(rv$collect.detected.data, paste(folder.path2(),"/","3_trajectories_detected_",
                                            rv$collect.detected.data$analysis.id %>% unique(),
                                            "_", as.character(Sys.Date()), ".rds", sep = ""))

    # write.table(rv$collect.detected.data %>%
    #               group_by(data.set.name, duration.filter, visual.inspection ) %>%
    #               summarise(frames = length(unique(frame.number)), observations = n()) %>%
    #               ungroup() %>%
    #               group_by(duration.filter, visual.inspection) %>%
    #               summarise(data.sets = n(), frames = sum(frames), observations = sum(observations)),
    #             paste("3_Summary_", rv$collect.detected.data$analysis.id %>% unique(),
    #                   "_", as.character(Sys.Date()), ".txt", sep =""),
    #             sep = "\t", col.names = TRUE)
  })


  ##### tab4 #####


  detected.data <- reactive({
    req(input$file3)
    readRDS(input$file3$datapath)})

  output$data.set.name <- renderPrint({detected.data() %>%
      select(data.set.name) %>% unique()})

  observeEvent(input$do.it, {
    rv$transformed.data <- detected.data() %>% ungroup() %>% MakeTrajectoryUniqeID()

    show.data4 <<- rv$transformed.data

    trajectory.address <- rv$transformed.data %>%
      MakeTrajectoryAddress()

    write.table(trajectory.address,
                paste(folder.path2(),"/", rv$transformed.data$analysis.id %>% unique(),
                      "_trajectory_address_",
                      as.character(Sys.Date()), ".txt", sep =""),
                sep = "\t", col.names = TRUE)

    to.cut.data.sets <- trajectory.address$file.name

    write.table(to.cut.data.sets,
                paste(folder.path2(),"/",
                      rv$transformed.data$analysis.id %>% unique(),"_file_name_",
                      as.character(Sys.Date()), ".txt", sep =""),
                quote = FALSE, row.names = FALSE)
  })


  list.of.data.sets4 <-reactive(rv$transformed.data %>% select(data.set.name)%>% unique())
  list.of.trajs4 <- reactive(rv$transformed.data %>%
                               filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
                                      visual.inspection, duration.filter) %>%
                               select(trajectory.unique.id) %>% unique())



  observeEvent(c(input$do.it, input$num41),{

    max.traj <- reactive({req(input$do.it)
      rv$transformed.data %>%
        filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
               visual.inspection, duration.filter) %>%
        select(trajectory.unique.id) %>%
        unique() %>% nrow()})

    updateNumericInput(session, "num42", value = 1, max = max.traj())
  })

  observeEvent(input$do.it, {

    output$full.name41 <- renderText({as.character(list.of.data.sets4()[input$num41,])})

    output$full.name42 <- renderText({ as.character(list.of.trajs4()[input$num42,])})


    output$plot41 <- renderPlot({
      ggplot()+
        geom_point(data = rv$transformed.data %>%
                     filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
                            visual.inspection, duration.filter),
                   aes(x = X, y= Y, color = signal.type), alpha = 0.2) +
        geom_point(data = rv$transformed.data %>%
                     filter(trajectory.unique.id == as.character(list.of.trajs4()[input$num42,]),
                            visual.inspection, duration.filter),
                   aes(x = X, y= Y), color ="black") +
        xlim(c(min(rv$transformed.data %>%
                     filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
                            visual.inspection, duration.filter) %>% "$" (X)) -1000,
               max(rv$transformed.data %>%
                     filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
                            visual.inspection, duration.filter) %>% "$" (X)) +1000)) +
        ylim(c(min(rv$transformed.data %>%
                     filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
                            visual.inspection, duration.filter) %>% "$" (Y)) -1000,
               max(rv$transformed.data %>%
                     filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
                            visual.inspection, duration.filter) %>% "$" (Y)) +1000)) +
        ggtitle("Collection of trajectoires for visual inspection")

    })

    output$plot42 <- renderPlot({

      rv$transformed.data %>%
        group_by(data.set.name) %>%
        mutate(intensity.alarm = as.factor(ifelse(intensity > mean(intensity)*5, "High intensity", "Intensity"))) %>%
        ungroup() %>%
        filter(trajectory.unique.id == as.character(list.of.trajs4()[input$num42,]),
               visual.inspection, duration.filter) %>%
        ggplot()+
        geom_point(aes(x = X, y= frame.number, size = intensity, color = intensity.alarm )) +
        geom_path(aes(x = X, y= frame.number), color = "black") +
        xlim(c(min(rv$transformed.data %>%
                     filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
                            visual.inspection, duration.filter) %>% "$" (X)) -200,
               max(rv$transformed.data %>%
                     filter(data.set.name == as.character(list.of.data.sets4()[input$num41,]),
                            visual.inspection, duration.filter) %>% "$" (X)) +200)) +
        scale_color_manual(values = c("High intensity" = "red", "Intensity" = "black"))+
        ggtitle("X cooredinate of the selected trajectory
                    ")

    })

  })

  observeEvent(input$remove4, {


    rv$transformed.data <- rv$transformed.data %>%
      mutate(visual.inspection = ifelse(trajectory.unique.id == as.character(list.of.trajs4()[input$num42,]),
                                        FALSE, visual.inspection))

  })

  observeEvent(input$save4, {

    saveRDS(rv$transformed.data, paste(folder.path2(),"/","4_visually_inspected_",
                                       rv$transformed.data$analysis.id %>% unique(),
                                       "_", as.character(Sys.Date()), ".rds", sep = ""))
  })


  ##### tab5 #####

  raw.data <- reactive({
    req(input$file51)
    readRDS(input$file51$datapath)})

  detected.inspected.data <- reactive({
    req(input$file52)
    readRDS(input$file52$datapath)})

  observeEvent(c(input$noise.filters, input$stuck.gauge,
                 input$flank.gauge, input$intensity.gauge,
                 input$checkGroup), {
                   rv$filtered.added.data <- detected.inspected.data() %>%
                     group_by(data.set.name) %>%
                     mutate(remove.out = ifelse(sum(visual.inspection)==0, FALSE, TRUE)) %>%
                     filter(remove.out) %>%
                     AddRotatedCoord() %>%
                     AddStuckFilter(gauge = 10- input$stuck.gauge) %>%
                     AddFlankFilter(gauge = input$flank.gauge) %>%
                     AddIntensityFilter(gauge = input$intensity.gauge) %>%
                     ArrangeFilters()  %>% filter(signal.type != "stuck")

                   show.data3 <<- rv$filtered.added.data
                 })

  list.of.data.sets5 <-reactive(rv$filtered.added.data %>% ungroup() %>%
                                  select(data.set.name)%>% unique())
  list.of.trajs5 <- reactive(rv$filtered.added.data %>%
                               filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                      detected.data %in% input$checkGroup) %>%
                               select(trajectory.unique.id) %>% unique())

  observeEvent(c(input$noise.filters, input$stuck.gauge,
                 input$flank.gauge, input$intensity.gauge,
                 input$checkGroup), {


                   output$plot51 <- renderPlot({ req(input$noise.filters)
                     ggplot()+
                       geom_point(data = raw.data ()%>%
                                    filter(data.set.name == as.character(list.of.data.sets5()[input$num51,])),
                                  aes(x = X, y= Y),color = "gray", alpha = 0.5) +
                       geom_point(data = rv$filtered.added.data %>%
                                    filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                           detected.data %in% input$checkGroup),
                                  aes(x = X, y= Y, color = detected.data))+
                       xlim(c(min(rv$filtered.added.data %>%
                                    filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                           duration.filter) %>% "$" (X)) -1000,
                              max(rv$filtered.added.data %>%
                                    filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                           duration.filter) %>% "$" (X)) +1000)) +
                       ylim(c(min(rv$filtered.added.data %>%
                                    filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                           duration.filter) %>% "$" (Y)) -1000,
                              max(rv$filtered.added.data %>%
                                    filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                           duration.filter) %>% "$" (Y)) +1000))+
                       scale_color_manual(values = c("Noise-excluded data" = "red", "Short-lived noise" = "blue",
                                                     "High/low intensty noise" = "dark blue", "Surface-bound emitters" = "green",
                                                     "Non-linear movements" = "yellow"))+
                       ggtitle("Original cooredinates")+ xlab("X") + ylab("Y")

                   })

                 })

  observeEvent(c(input$do.it5,input$noise.filters), {
    output$plot52 <- renderPlot({req(input$noise.filters)
      ggplot()+
        geom_point(data = rv$filtered.added.data %>%
                     filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                            detected.data== "Noise-excluded data"),
                   aes(x = r.X, y= r.Y ),color = "red", alpha = 0.5)+
        facet_wrap(~sub.data.set.name, ncol =1)+
        geom_point(data = rv$filtered.added.data %>%
                     filter(trajectory.unique.id == as.character(list.of.trajs5()[input$num52,])),
                   aes(x = r.X, y= r.Y), color ="black") +
        xlim(c(min(rv$filtered.added.data %>%
                     filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                            duration.filter) %>% "$" (r.X)) -1000,
               max(rv$filtered.added.data %>%
                     filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                            duration.filter) %>% "$" (r.X)) +1000)) +
        ylim(c(min(rv$filtered.added.data %>%
                     filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                            duration.filter) %>% "$" (r.Y)) -1000,
               max(rv$filtered.added.data %>%
                     filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                            duration.filter) %>% "$" (r.Y)) +1000))+
        # scale_color_manual(values = c("Noise-excluded data" = "red", "Short lived noise" = "blue",
        #                               "High intensty noise" = "dark blue", "Stuck to surface" = "green",
        #                               "Non-linear scanning" = "yellow"))+
        ggtitle("Rotated cooredinates-Noise excluded data")+ xlab("Rotated X") + ylab("Rotated Y")

    })
  })

  observeEvent(input$do.it5,{
    output$plot53 <- renderPlot({

      # rv$filtered.added.data %>%
      #   filter(trajectory.unique.id == as.character(list.of.trajs5()[input$num52,])) %>%
      #   ggplot()+
      #   geom_point(aes(x = r.X, y= frame.number, size = intensity)) +
      #   ggtitle(paste ("Trajectory id: ", as.character(list.of.trajs5()[input$num52,]), sep = " ")) +
      #   geom_path(aes(x = r.X, y= frame.number))  +
      #   xlim(c(min(rv$filtered.added.data %>%
      #                filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
      #                       duration.filter) %>% "$" (r.X)) -200,
      #          max(rv$filtered.added.data %>%
      #                filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
      #                      duration.filter) %>% "$" (r.X)) +200))

      rv$filtered.added.data %>%
        filter(trajectory.unique.id == as.character(list.of.trajs5()[input$num52,])) %>%
        ggplot()+
        geom_point(aes(x = r.X, y= frame.number, size = intensity)) +
        ggtitle(paste ("Rotated X---Trajectory id: ", as.character(list.of.trajs5()[input$num52,]), sep = " ")) +
        geom_path(aes(x = r.X, y= frame.number))  +
        scale_x_continuous(limits =  c(min(rv$filtered.added.data %>%
                                             filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                                    duration.filter) %>% "$" (r.X)) -200,
                                       max(rv$filtered.added.data %>%
                                             filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                                    duration.filter) %>% "$" (r.X)) +200))

    })


    output$plot54 <- renderPlot({

      # rv$filtered.added.data %>%
      #   filter(trajectory.unique.id == as.character(list.of.trajs5()[input$num52,])) %>%
      #   ggplot()+
      #   geom_point(aes(x = r.Y, y= frame.number , size = intensity)) +
      #   ggtitle(paste ("Trajectory id: ", as.character(list.of.trajs5()[input$num52,]), sep = " ")) +
      #   geom_path(aes(x = r.Y, y= frame.number))  +
      #   xlim(c(min(rv$filtered.added.data %>%
      #                filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
      #                       visual.inspection, duration.filter) %>% "$" (r.Y)) -200,
      #          max(rv$filtered.added.data %>%
      #                filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
      #                       visual.inspection, duration.filter) %>% "$" (r.Y)) +200))

      rv$filtered.added.data %>%
        filter(trajectory.unique.id == as.character(list.of.trajs5()[input$num52,])) %>%
        ggplot()+
        geom_point(aes(x = r.Y, y= frame.number , size = intensity)) +
        ggtitle(paste ("Rotated Y---Trajectory id: ", as.character(list.of.trajs5()[input$num52,]), sep = " ")) +
        geom_path(aes(x = r.Y, y= frame.number))  +
        scale_x_continuous(limits = c(min(rv$filtered.added.data %>%
                                            filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                                   visual.inspection, duration.filter) %>% "$" (r.Y)) -200,
                                      max(rv$filtered.added.data %>%
                                            filter(data.set.name == as.character(list.of.data.sets5()[input$num51,]),
                                                   visual.inspection, duration.filter) %>% "$" (r.Y)) +200))

    })

  })

  observeEvent(input$save5, {

    saveRDS(rv$filtered.added.data, paste(folder.path2(),"/","5_noise_excluded_",
                                          rv$filtered.added.data$analysis.id %>% unique(),
                                          "_", as.character(Sys.Date()), ".rds", sep = ""))
  })


  ##### tab6 #####
  analysed.data <- reactive({
    req(input$file6)
    readRDS(input$file6$datapath)})

  #data.collect <- NULL

  observeEvent(input$add6, {

    rv$data.collect <- bind_rows(rv$data.collect, analysed.data() %>%
                                   mutate(trajectory.unique.id = paste(trajectory.unique.id,
                                                                       analysis.id, sep = "-"),
                                          NA.condition= "expr.cond.NA")) %>% distinct()

    show.data6 <<-  rv$data.collect

    rv$protein.list <- rv$data.collect %>% "$"(protein) %>% unique()

    # output$datasets <- renderText(rv$data.collect %>% filter(protein %in% input$inCheckboxGroup,
    #                                                          detected.data == "Noise excluded") %>%
    #                                 select(data.set.name) %>% unique() %>% nrow()%>% as.numeric())

    output$datasets <- renderDataTable(rv$data.collect %>% filter(protein %in% input$inCheckboxGroup,
                                                                  detected.data == "Noise-excluded data") %>%
                                         group_by(protein, across(all_of(input$in.select)))%>%
                                         summarise(data.sets = length(unique(data.set.name)),
                                                   trajectories = length(unique(trajectory.unique.id)),
                                                   frames = n()))

    #
    # output$trajectories <- renderText(rv$data.collect %>%filter(protein %in% input$inCheckboxGroup,
    #                                                             detected.data == "Noise-excluded data") %>%
    #                                   select(trajectory.unique.id) %>%
    #                                     unique() %>% nrow()%>% as.numeric())
    # output$frames <- renderText(rv$data.collect %>% filter(protein %in% input$inCheckboxGroup,
    #                                                        detected.data == "Noise-excluded data") %>%
    #                                     group_by(trajectory.unique.id) %>%
    #                                     summarise(frames = length(frame.number)) %>%
    #                                     summarise(sum(frames)) %>% as.numeric())


    updateCheckboxGroupInput(session, "inCheckboxGroup",
                             label = "Proteins added",
                             choices = rv$protein.list, inline = TRUE, selected = rv$protein.list)
    updateCheckboxGroupInput(session, "inCheckboxGroup0",
                             label = "Proteins added",
                             choices = rv$protein.list, inline = TRUE, selected = rv$protein.list)
    updateCheckboxGroupInput(session, "inCheckboxGroup1",
                             label = "Proteins added",
                             choices = rv$protein.list, inline = TRUE, selected = rv$protein.list)
    updateCheckboxGroupInput(session, "inCheckboxGroup2",
                             label = "Proteins added",
                             choices = rv$protein.list, inline = TRUE, selected = rv$protein.list)
    updateCheckboxGroupInput(session, "inCheckboxGroup3",
                             label = "Proteins added",
                             choices = rv$protein.list, inline = TRUE, selected = rv$protein.list)
    updateCheckboxGroupInput(session, "inCheckboxGroup4",
                             label = "Proteins added",
                             choices = rv$protein.list, inline = TRUE, selected = rv$protein.list)
    updateCheckboxGroupInput(session, "inCheckboxGroup5",
                             label = "Proteins added",
                             choices = rv$protein.list, inline = TRUE, selected = rv$protein.list)




    rv$conditions.list <- rv$data.collect[, grepl("expr.cond",names(rv$data.collect))] %>% names()
    rv$conditions.list <- c("NA.condition", rv$conditions.list)

    updateSelectInput(session, "in.select",
                      label = "Exprimental conditions",
                      choices = rv$conditions.list)
    updateSelectInput(session, "in.select0",
                      label = "Exprimental conditions",
                      choices = rv$conditions.list)
    updateSelectInput(session, "in.select1",
                      label = "Exprimental conditions",
                      choices = rv$conditions.list)
    updateSelectInput(session, "in.select2",
                      label = "Exprimental conditions",
                      choices = rv$conditions.list)
    updateSelectInput(session, "in.select3",
                      label = "Exprimental conditions",
                      choices = rv$conditions.list)
    updateSelectInput(session, "in.select4",
                      label = "Exprimental conditions",
                      choices = rv$conditions.list)
    updateSelectInput(session, "in.select5",
                      label = "Exprimental conditions",
                      choices = rv$conditions.list)


  })

  observeEvent(input$show60,{

    plot60.1 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup0,
                                 detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(step.per.f=abs(lead(r.X, n=1)- r.X)/frame.interval) %>%
        group_by(protein, across(all_of(input$in.select0))) %>%
        summarise(ave.displacement.per.ms = mean(step.per.f, na.rm= TRUE)) %>%
        ggplot()+
        geom_point(aes(x = protein, y= ave.displacement.per.ms,
                       color=as.factor(eval(as.name(input$in.select0)))), size=5) +
        ggtitle("Average scanning speed") +
        xlab("Protein") +
        ylab("Scanning speed (nm/ms)") +
        scale_color_discrete(name = "Experimental condition")
    })

    output$plot60.1 <- renderPlot({
      plot60.1()
    })


    observeEvent( input$save60.2,{

      write.csv(ggplot_build(plot60.1())$plot$data,
                paste0(folder.path2(),"/","plot.1.1.data.csv"))

    })

    plot60.2 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup0,
                                 detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(step.per.f=abs(lead(r.X, n=1)- r.X)/frame.interval) %>%
        group_by(protein, trajectory.unique.id, across(all_of(input$in.select0))) %>%
        summarise(displacement.per.ms = mean(step.per.f, na.rm =T)) %>%
        ggplot()+
        geom_histogram(aes(x = displacement.per.ms, fill = as.factor(eval(as.name(input$in.select0))))) +
        facet_wrap(~protein, scales = "free")+
        ggtitle("Distribution of average scanning speed of trajectories") +
        xlab("Scanning speed (nm/ms)") +
        ylab("Counts (trajectories)") +
        scale_fill_discrete(name = "Experimental condition")
    })

    output$plot60.2 <- renderPlot({
      plot60.2()
    })

    observeEvent(input$save60.2,{

      write.csv(ggplot_build(plot60.2())$plot$data,
                paste0(folder.path2(),"/","plot.1.2.data.csv"))

    })

    plot60.3 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup0,
                                 detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(displacement.per.ms=(lead(r.X, n=1)- r.X)/frame.interval) %>%
        ggplot()+
        geom_histogram(aes(x = displacement.per.ms, fill =as.factor(eval(as.name(input$in.select0))))) +
        facet_wrap(~protein, scales = "free")+
        ggtitle("Distribution of instantaneous scanning speed") +
        xlab("Scanning speed (nm/ms)") +
        ylab("Counts (frames)") +
        scale_fill_discrete(name = "Experimental condition")

    })

    output$plot60.3 <- renderPlot({
      plot60.3()
    })

    observeEvent(input$save60.3,{

      write.csv(ggplot_build(plot60.3())$plot$data,
                paste0(folder.path2(),"/","plot.1.3.data.csv"))

    })

  })



  observeEvent(input$show61,{

    plot61.1 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup1,
                                 detected.data == "Noise-excluded data") %>%
        group_by(protein, trajectory.unique.id, across(all_of(input$in.select1))) %>%
        summarise(binding.lifetime =
                    mean((max(frame.number) - min(frame.number))* unique(frame.interval)/1000)) %>%
        group_by(protein, across(all_of(input$in.select1))) %>%
        summarise(ave.binding.lifetime = mean(binding.lifetime)) %>%
        ggplot()+
        geom_point(aes(x =protein,  y= ave.binding.lifetime,
                       color = as.factor(eval(as.name(input$in.select1)))), size =5) +
        ggtitle("Average binding life time") +
        xlab("Protein") +
        ylab("Binding lifetime (s)") +
        scale_color_discrete(name = "Experimental condition")

    })

    output$plot61.1 <- renderPlot({
      plot61.1()
    })

    observeEvent(input$save61.1,{

      write.csv(ggplot_build(plot61.1())$plot$data,
                paste0(folder.path2(),"/","plot.2.1.data.csv"))

    })



    plot61.2 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup1,
                                 detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(step=lead(r.X, n=1)- r.X) %>%
        group_by(protein, trajectory.unique.id, across(all_of(input$in.select1))) %>%
        summarise(binding.lifetime =
                    mean((max(frame.number) - min(frame.number))* unique(frame.interval)/1000)) %>%
        ggplot()+
        geom_histogram(aes(x = binding.lifetime, fill = as.factor(eval(as.name(input$in.select1))))) +
        facet_wrap(~protein, scales = "free")+
        ggtitle("Distribution of binding life time of trajectories") +
        xlab("Binding life time (s)") +
        ylab("Counts (trajectories)") +
        scale_fill_discrete(name = "Experimental condition")
    })


    output$plot61.2 <- renderPlot({
      plot61.2()
    })

    observeEvent(input$save61.2,{

      write.csv(ggplot_build(plot61.2())$plot$data,
                paste0(folder.path2(),"/","plot.2.2.data.csv"))

    })


  })



  observeEvent(input$show62,{

    plot62.1 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup2,
                                 detected.data == "Noise-excluded data") %>%
        group_by(protein, trajectory.unique.id, across(all_of(input$in.select2))) %>%
        summarise(scanning.coverage = mean((max(r.X)- min(r.X)))) %>%
        group_by(protein, across(all_of(input$in.select2))) %>%
        summarise(ave.scanning.coverage = mean(scanning.coverage)) %>%
        ggplot()+
        geom_point(aes(x = protein, y = ave.scanning.coverage,
                       color = as.factor(eval(as.name(input$in.select2)))), size = 5) +
        ggtitle("Average scanning coverage") +
        xlab("Protein") +
        ylab("Scanning coverage (nm)") +
        scale_color_discrete(name = "Experimental condition")
    })

    output$plot62.1 <- renderPlot({
      plot62.1()
    })

    observeEvent(input$save62.1,{

      write.csv(ggplot_build(plot62.1())$plot$data,
                paste0(folder.path2(),"/","plot.3.1.data.csv"))

    })


    plot62.2 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup2,
                                 detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(step=lead(r.X, n=1)- r.X) %>%
        group_by(protein, trajectory.unique.id, across(all_of(input$in.select2))) %>%
        summarise(scanning.coverage = mean((max(r.X)- min(r.X))/0.34)) %>%
        ggplot()+
        geom_histogram(aes(x = scanning.coverage, fill = as.factor(eval(as.name(input$in.select2))))) +
        facet_wrap(~protein, scales = "free")+
        ggtitle("Distribution of scanning coverage") +
        xlab("Scanning  coverage (nm)") +
        ylab("Counts (trajectories)") +
        scale_fill_discrete(name = "Experimental condition")
    })
    output$plot62.2 <- renderPlot({
      plot62.2()
    })

    observeEvent(input$save62.2,{

      write.csv(ggplot_build(plot62.2())$plot$data,
                paste0(folder.path2(),"/","plot.3.2.data.csv"))

    })

  })



  observeEvent(input$show63,{

    plot63.1 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup3,
                                 detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(step=lead(r.X, n=1)- r.X) %>%
        group_by(protein, trajectory.unique.id, across(all_of(input$in.select3))) %>%
        summarise(bases.checked = max(cumsum(abs(step)), na.rm = T)) %>%
        group_by(protein, across(all_of(input$in.select3))) %>%
        summarise(ave.bases.checked = mean(bases.checked)) %>%
        ggplot()+
        geom_point(aes(x = protein , y= ave.bases.checked,
                       color = as.factor(eval(as.name(input$in.select3)))), size = 5)+
        ggtitle("Average accumulative scanning length") +
        xlab("Protein") +
        ylab("Accumulative scanning length (nm)") +
        scale_color_discrete(name = "Experimental condition")
    })

    output$plot63.1 <- renderPlot({
      plot63.1()
    })

    observeEvent(input$save63.1,{

      write.csv(ggplot_build(plot63.1())$plot$data,
                paste0(folder.path2(),"/","plot.4.1.data.csv"))

    })



    plot63.2 <- reactive({
      rv$data.collect %>% filter(protein %in% input$inCheckboxGroup3,
                                 detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(step=lead(r.X, n=1)- r.X) %>%
        group_by(protein, trajectory.unique.id, across(all_of(input$in.select3))) %>%
        summarise(bases.checked = max(cumsum(abs(step)), na.rm = T)/0.34) %>%
        ggplot()+
        geom_histogram(aes(x = bases.checked, fill = as.factor(eval(as.name(input$in.select3))))) +
        facet_wrap(~protein, scales = "free") +
        ggtitle("Distribution of accumulative scanning length") +
        xlab("accumulative scanning length (nm)") +
        ylab("Counts (trajectories)") +
        scale_fill_discrete(name = "Experimental condition")
    })

    output$plot63.2 <- renderPlot({
      plot63.2()
    })

    observeEvent(input$save63.2,{

      write.csv(ggplot_build(plot63.2())$plot$data,
                paste0(folder.path2(),"/","plot.4.2.data.csv"))

    })
  })




  observeEvent(input$show64,{
    plot64.1 <- reactive({

      rv$data.collect %>%
        filter(protein %in% input$inCheckboxGroup4,
               detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(duration = (max(frame.number)-min(frame.number))*frame.interval,
               speed = (lead(r.X, n=1)- r.X)/frame.interval,
               walked.length = mean(abs(speed), na.rm = T) *duration,
               covered.length = max(r.X)-min(r.X),
               walking.speed = mean(abs(speed), na.rm = T),
               covering.speed = (max(r.X)-min(r.X))/duration,
               redudancy = walked.length/covered.length) %>%
        group_by(protein, across(all_of(input$in.select4))) %>%
        summarise(ave.redundancy = mean(redudancy, na.rm = T),
                  ave.efficiency = mean(covering.speed, na.rm = T)) %>%
        ggplot()+
        geom_point(aes(x = ave.efficiency,
                       y = ave.redundancy,
                       color =protein ,
                       shape = as.factor(eval(as.name(input$in.select4)))), size = 5)+
        ggtitle("Average redundancy-efficiency")+
        ylab("Redundancy") +
        xlab("Efficiency (nm/ms)")+
        scale_color_discrete(name = "Protein")+
        scale_shape_discrete(name = "Experimental condition")

    })

    output$plot64.1 <- renderPlot({
      plot64.1()
    })

    observeEvent(input$save64.1,{

      write.csv(ggplot_build(plot64.1())$plot$data,
                paste0(folder.path2(),"/","plot.5.1.data.csv"))

    })

    plot64.2 <- reactive({

      rv$data.collect %>%
        filter(protein %in% input$inCheckboxGroup4,
               detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(duration = (max(frame.number)-min(frame.number))*frame.interval,
               speed = (lead(r.X, n=1)- r.X)/frame.interval,
               walked.length = mean(abs(speed), na.rm = T) *duration,
               covered.length = max(r.X)-min(r.X),
               walking.speed = mean(abs(speed), na.rm = T),
               covering.speed = (max(r.X)-min(r.X))/duration,
               redundancy = walked.length/covered.length) %>%
        ggplot()+
        geom_point(aes(x = covering.speed,
                       y = redundancy,
                       color = as.factor(eval(as.name(input$in.select4)))), size = 0.1)+
        ggtitle("Distribution of redundancy-efficiency for trajectories")+
        ylab("Redundancy") +
        xlab("Efficiency (nm/ms)")+
        scale_color_discrete(name = "Experimental condition")

    })
    output$plot64.2 <- renderPlot({
      plot64.2()
    })

    observeEvent(input$save64.2,{

      write.csv(ggplot_build(plot64.2())$plot$data,
                paste0(folder.path2(),"/","plot.5.2.data.csv"))

    })

    plot64.3 <- reactive({

      rv$data.collect %>%
        filter(protein %in% input$inCheckboxGroup4,
               detected.data == "Noise-excluded data") %>%
        group_by(trajectory.unique.id) %>%
        mutate(duration = (max(frame.number)-min(frame.number))*frame.interval,
               speed = (lead(r.X, n=1)- r.X)/frame.interval,
               walked.length = mean(abs(speed), na.rm = T) *duration,
               covered.length = max(r.X)-min(r.X),
               walking.speed = mean(abs(speed), na.rm = T),
               covering.speed = (max(r.X)-min(r.X))/duration,
               redundancy = walked.length/covered.length) %>%
        ggplot()+
        geom_point(aes(x = covering.speed,
                       y = redundancy,
                       color = protein), size = 0.1)+
        ggtitle("Distribution of redundancy-efficiency for trajectories")+
        ylab("Redundancy") +
        xlab("Efficiency (nm/ms)")+
        scale_color_discrete(name = "Protein")

    })
    output$plot64.3 <- renderPlot({
      plot64.3()
    })

    observeEvent(input$save64.3,{

      write.csv(ggplot_build(plot64.3())$plot$data,
                paste0(folder.path2(),"/","plot.5.3.data.csv"))

    })


  })



  observeEvent(input$show65,{

    plot65.1 <- reactive({
      rv$data.collect %>%
        filter(protein %in% input$inCheckboxGroup5,
               detected.data == "Noise-excluded data") %>%
        group_by(protein, trajectory.unique.id) %>%
        mutate(time=frame.number-min(frame.number)) %>%
        arrange(time) %>%
        mutate(local.msd.05 = localMSDcomplete(r.X, 5)/ (2000*frame.interval),
               local.msd.07 = localMSDcomplete(r.X, 7)/ (2000*frame.interval),
               local.msd.10 = localMSDcomplete(r.X, 10)/ (2000*frame.interval),
               local.msd.15 = localMSDcomplete(r.X, 15)/ (2000*frame.interval)) %>%
        filter(local.msd.05!=0) %>%
        group_by(protein, across(all_of(input$in.select5))) %>%
        summarise(ave.diffusion.rate = mean(local.msd.05)) %>%
        ggplot() + geom_point(aes(x= protein, y = ave.diffusion.rate,
                                  color = as.factor(eval(as.name(input$in.select5)))),size = 5)+
        ggtitle("Average diffusion rate")+
        xlab("Protein")+
        ylab(expression(Average~diffusion~rate~(mu*m^2/s)))+
        scale_color_discrete(name = "Experimental condition")

    })

    output$plot65.1 <- renderPlot({
      plot65.1()
    })

    observeEvent(input$save65.1,{

      write.csv(ggplot_build(plot65.1())$plot$data,
                paste0(folder.path2(),"/","plot.6.1.data.csv"))

    })


    plot65.2 <- reactive({
      rv$data.collect %>%
        filter(protein %in% input$inCheckboxGroup5,
               detected.data == "Noise-excluded data") %>%
        group_by(protein, trajectory.unique.id) %>%
        mutate(time=frame.number-min(frame.number)) %>%
        arrange(time) %>%
        mutate(local.msd.05 = localMSDcomplete(r.X, 5)/ (2000*frame.interval),
               local.msd.07 = localMSDcomplete(r.X, 7)/ (2000*frame.interval),
               local.msd.10 = localMSDcomplete(r.X, 10)/ (2000*frame.interval),
               local.msd.15 = localMSDcomplete(r.X, 15)/ (2000*frame.interval)) %>%
        filter(local.msd.05!=0) %>%
        group_by(protein, across(all_of(input$in.select5))) %>%
        ggplot(aes(x=local.msd.05)) +
        facet_wrap(~protein, ncol = 3) +
        geom_histogram(bins=65, position = "stack",
                       aes(y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..],
                           fill = as.factor(eval(as.name(input$in.select5))))) +
        scale_x_log10(breaks= c(0.01,0.1,1,10))+
        scale_y_continuous(breaks = c(0.025,0.05,0.075))+
        annotation_logticks(side= "b",
                            short = unit(0.3,"mm"),
                            mid = unit(0.6,"mm"),
                            long = unit(1,"mm")) +
        ggtitle("Distribution of instantaneous diffusion rate") +
        xlab(expression(Instantaneous~diffusion~rate~(mu*m^2/s)))+
        ylab("Fractions")+
        scale_fill_discrete(name = "Experimental condition")

    })
    output$plot65.2 <- renderPlot({
      plot65.2()
    })

    observeEvent(input$save65.2,{

      write.csv(ggplot_build(plot65.2())$plot$data,
                paste0(folder.path2(),"/","plot.6.2.data.csv"))

    })


    plot65.3 <- reactive({
      rv$data.collect %>%
        filter(protein %in% input$inCheckboxGroup5,
               detected.data == "Noise-excluded data") %>%
        group_by(protein, trajectory.unique.id) %>%
        mutate(time=frame.number-min(frame.number)) %>%
        arrange(time) %>%
        mutate(local.msd.05 = localMSDcomplete(r.X, 5)/ (2000*frame.interval),
               local.msd.07 = localMSDcomplete(r.X, 7)/ (2000*frame.interval),
               local.msd.10 = localMSDcomplete(r.X, 10)/ (2000*frame.interval),
               local.msd.15 = localMSDcomplete(r.X, 15)/ (2000*frame.interval)) %>%
        filter(local.msd.05!=0) %>%
        group_by(protein, across(all_of(input$in.select5))) %>%
        ggplot(aes(x=local.msd.05, color = protein)) +
        geom_density(size=3) +
        scale_x_log10(breaks= c(0.01,0.1,1,10))+
        annotation_logticks(side= "b",
                            short = unit(0.3,"mm"),
                            mid = unit(0.6,"mm"),
                            long = unit(1,"mm")) +
        ggtitle("Distribution of instantaneous diffusion rate")+
        xlab(expression(Instantaneous~diffusion~rate~(mu*m^2/s)))+
        ylab("Density")+
        scale_fill_discrete(name = "Protein")

    })
    output$plot65.3 <- renderPlot({
      plot65.3()
    })

    observeEvent(input$save65.3,{

      write.csv(ggplot_build(plot65.3())$plot$data,
                paste0(folder.path2(),"/","plot.6.3.data.csv"))

    })

  })



}
