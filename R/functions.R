
# FUNCTIONS FOR GENERATING AND PLOTTING RESULTS ---------------------------


#' Table of characteristics by sex
#'
#' @param sex_ Numeric; 1 for male, 2 for female. Defaults to 1.
#'
#' @return A list containing:
#' - `table`: a gt table of participant characteristics by BMI and diabetes status.
#' - `ns`: counts of participants and events by diabetes and BMI categories.
#' @export
#'
#' @examples
toc <- function(sex_ = 1) {

    # Step 1: Filter and recode data
    data_tbl1 <- data %>%
        mutate(sex = haven::zap_labels(sex)) %>%           # Remove labels from sex variable
        filter(sex == sex_) %>%                            # Filter by requested sex
        select(DMall, alder, bmi, bmikat, dm2, educa, income,
               v37, ascvd) %>%
        filter(bmikat != 1) %>%                            # Remove BMI category "1" (if needed)
        mutate(
            DMall = case_when(DMall == 0 ~ "No diabetes", DMall == 1 ~ "Diabetes"),
            DMall = factor(DMall, levels = c("No diabetes", "Diabetes")),
            bmikat = case_when(bmikat == 0 ~ "Normal weight", bmikat == 2 ~ "Overweight", bmikat == 3 ~ "Obesity"),
            bmikat = factor(bmikat, levels = c("Normal weight", "Overweight", "Obesity")),
            dm2 = case_when(dm2 == 0 ~ "0", dm2 == 1 ~ "Type 2 Diabetes Mellitus"),
            income = case_when(
                income == 0 ~ "<GBP18,000",
                income == 1 ~ "GBP18,000-30,999",
                income == 2 ~ "GBP31,000-51,999",
                income == 3 ~ "GBP52,000-100,000",
                income == 4 ~ ">GBP100,000"
            ),
            income = factor(income, levels = c("<GBP18,000", "GBP18,000-30,999", "GBP31,000-51,999", "GBP52,000-100,000", ">GBP100,000")),
            v37 = case_when(v37 == 0 ~ "0", v37 == 1 ~ "Current smokers, %")
        )

    # Step 2: Count participants and events
    n <- data_tbl1 %>% group_by(DMall, bmikat) %>% count()
    n_event <- filter(data_tbl1, ascvd == 1) %>% group_by(DMall, bmikat) %>% count() %>% rename(ascvd = n)
    ns <- left_join(n, n_event)

    # Step 3: Create stratified summary table
    table <- data_tbl1 %>%
        select(-ascvd) %>%
        tbl_strata(
            strata = c("DMall"),
            .tbl_fun = ~ .x %>%
                tbl_summary(
                    by = bmikat,
                    missing = "ifany",
                    missing_text = "Missing",
                    digits = list(educa = 0),
                    type = list(educa ~ "continuous"),
                    statistic = list(
                        all_continuous() ~ "{median} ({p25}, {p75})",
                        all_categorical() ~ "{n} ({p}%)"
                    ),
                    label = list(
                        alder = "Age",
                        bmi = "Body Mass Index (kg/m<sup>2</sup>)",
                        dm2 = "Type 2 Diabetes Mellitus",
                        educa = "Education (years in school)",
                        income = "Household income before tax",
                        v37 ~ "Current smokers, %"
                    )
                )
        ) %>%
        modify_header(label ~ "**Characteristic**") %>%
        remove_row_type(variables = "dm2", type = "level", level_value = "0") %>%
        modify_table_body(~ .x |> filter(!(variable %in% "dm2" & row_type %in% "label"))) %>%
        remove_row_type(variables = "v37", type = "level", level_value = "0") %>%
        modify_table_body(~ .x |> filter(!(variable %in% "v37" & row_type %in% "label"))) %>%
        modify_table_body(fun = ~ .x %>% arrange(variable)) %>%
        modify_column_indent(columns = label) %>%
        modify_footnote(everything() ~ NA_character_) %>%
        as_gt() %>%
        tab_source_note("Continuous variables are shown as median (interquartile range), categorical variables as number (%)")

    return(list(table, ns))
}


#' Distribution plot of BMI by diabetes status, Figure 1
#'
#' @param sex_ Numeric; 1 for male, 2 for female.
#' @param dm Character; variable for diabetes status, default "dm2".
#'
#' @return A list containing:
#' - `plot`: ggplot density plot of BMI by diabetes status
#' - `dist_ns`: counts of participants by diabetes and sex
#' @export
#'
#' @examples
distribution <- function(sex_ = 1, dm = "dm2") {
    data <- data %>% mutate(sex = haven::zap_labels(sex))

    dist_ns <- data %>% group_by(DMall, sex) %>% count()

    plot <- ggplot() +
        geom_vline(xintercept = c(18.5, 25, 30), color = "grey") +
        stat_density(data = filter(data, get(dm) == 0, sex == sex_), aes(x = bmi, color = "#66C2A5"), geom = "line") +
        stat_density(data = filter(data, get(dm) == 1, sex == sex_), aes(x = bmi, color = "#FC8D62"), geom = "line") +
        scale_color_brewer(palette = "Set2", labels = c("No diabetes", "Diabetes")) +
        scale_x_continuous(limits = c(15, 50), breaks = c(18.5, 25, 30, 40, 50)) +
        scale_y_continuous(limits = c(0, 0.13), breaks = c(0.05, 0.10)) +
        labs(color = "Diabetes status", x = expression(paste("Body Mass Index (kg/", m^2, ")")), y = "Density") +
        theme_classic() +
        theme(
            legend.position = c(0.85, 0.85),
            legend.title = element_blank(),
            legend.background = element_rect(size = 0.5, linetype = "solid", color = "black")
        )

    return(list(plot, dist_ns))
}


#' Cumulative incidence with competing risk, Figure 2
#'
#' @param futime Follow-up time variable
#' @param endpoint Event indicator variable
#' @param by_cat Grouping variable (bmikat, DMall, risk_var)
#' @param sex_ Numeric; 1 male, 2 female
#'
#' @return List containing:
#' - `plot`: cumulative incidence plot
#' - `plotdata_df`: data used for plotting
#' - `survfit_sum2`: summary table from survfit
#' @export
#'
#' @examples
cum_inc_cr <- function(futime = "fu_ascvd", endpoint = "ascvd", by_cat = "risk_var", sex_ = 2) {
    # Step 1: Set labels for plot legend
    if(by_cat == "bmikat") labels <- c("Normal weight", "Overweight", "Obesity")
    if(by_cat == "DMall") labels <- c("No diabetes", "Diabetes")
    if(by_cat == "risk_var") labels <- c(
        "Normal weight, no diabetes", "Overweight, no diabetes", "Obesity, no diabetes",
        "Normal weight, diabetes", "Overweight, diabetes", "Obesity, diabetes"
    )
    if(by_cat == "risk_vardm2") labels <- labels

    # Step 2: Prepare dataset
    cumincdata <- data %>% filter(alder >= 40, bmikat != 1, sex == sex_) %>%
        mutate(
            ep = case_when(
                get(endpoint) == 1 ~ 1,
                dod == 1 ~ 2,
                status == 2 ~ 3,
                TRUE ~ 0
            ),
            fu_ep = case_when(
                get(endpoint) == 1 ~ fu_ascvd,
                dod == 1 ~ fu_dod,
                dod == 0 & status == 2 ~ fu_dod,
                TRUE ~ fu_dod
            )
        )

    # Step 3: Create summary table from survfit
    survfit_sum <- summary(survfit(Surv(alder, alder + fu_ep, as_factor(ep)) ~ get(by_cat), data = cumincdata))$table %>% as.data.frame()
    colnames(survfit_sum) <- c("n", "n_event", "rmean", "sermean")
    survfit_sum2 <- survfit_sum %>%
        mutate(group = rownames(survfit_sum), group = str_replace(group, "get\\(by_cat\\)=", "riskvar"))
    rownames(survfit_sum2) <- NULL

    # Step 4: Plot cumulative incidence
    plotdata <- survfit(Surv(alder, alder + fu_ep, as_factor(ep)) ~ get(by_cat), data = cumincdata) %>% ggcuminc()
    plotdata_df <- ggplot_build(plotdata)$data[[1]] %>% select(colour, x, y, group)

    plot <- ggplot(plotdata_df, aes(x = x, y = y, color = as.factor(group))) +
        geom_step() +
        scale_x_continuous(limits = c(40, 100)) +
        scale_y_continuous(limits = c(0, 0.875), labels = scales::number_format(scale = 100)) +
        scale_color_brewer(palette = "Set2", labels = labels) +
        labs(x = "Age (years)", y = "Cumulative Incidence (%)") +
        theme_minimal() +
        theme(
            axis.line = element_line(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = "right",
            legend.background = element_rect(size = 0.5, linetype = "solid", color = "black")
        ) +
        guides(color = guide_legend(ncol = 1, reverse = TRUE))

    return(list(plot, plotdata_df, survfit_sum2))
}


#' Forestplot of Cox regression results, Figure 3
#'
#' @param fu Follow-up time variable
#' @param sex_ Numeric; 1 male, 2 female
#' @param endpoint Event variable
#' @param multifac_adj Logical; whether to adjust for multiple covariates
#' @param risk_var Variable for risk categories
#'
#' @return List containing:
#' - `plot`: forestplot ggplot
#' - `p_table`: p-values for interaction tests
#' - `results_data`: detailed Cox regression results
#' @export
#'
#' @examples
forestplot_lej <- function(fu = "fu_ascvd", sex_ = 2, endpoint = "ascvd", multifac_adj = FALSE, risk_var = "risk_var") {
    if(endpoint == "ascvd" & sex_ == 1) {x_label = "Hazard ratio (95% confidence interval)\nfor ASCVD in men"}
    if(endpoint == "ascvd" & sex_ == 2) {x_label = "Hazard ratio (95% confidence interval)\nfor ASCVD in women"}

    if(multifac_adj == FALSE) {formula = paste0("Surv(alder, (alder+ get(fu)), get(endpoint)) ~", risk_var, " + educa + income + v37")}
    if(multifac_adj == TRUE)  {formula = paste0("Surv(alder, (alder+ get(fu)), get(endpoint)) ~", risk_var, " + educa + income + v37 + ldl + trig + systolic")}

    if(multifac_adj == FALSE) {formula_bmikatnum = "Surv(alder, (alder+ get(fu)), get(endpoint)) ~ bmikat_cox+ educa + income + v37"}
    if(multifac_adj == TRUE)  {formula_bmikatnum = "Surv(alder, (alder+ get(fu)), get(endpoint)) ~ bmikat_cox+ educa + income + v37 + ldl + trig + systolic"}

    if(multifac_adj == FALSE) {formula_bmikatnumint = "Surv(alder, (alder+ get(fu)), get(endpoint)) ~ bmikat_cox*DMall+ educa + income + v37"}
    if(multifac_adj == TRUE)  {formula_bmikatnumint = "Surv(alder, (alder+ get(fu)), get(endpoint)) ~ bmikat_cox*DMall+ educa + income + v37 + ldl + trig + systolic"}

    if(multifac_adj == FALSE) {formula_bmicont= "Surv(alder, (alder+ get(fu)), get(endpoint)) ~ bmi+ educa + income + v37"}
    if(multifac_adj == TRUE)  {formula_bmicont = "Surv(alder, (alder+ get(fu)), get(endpoint)) ~ bmi+ educa + income + v37 + ldl + trig + systolic"}

    if(multifac_adj == FALSE) {formula_bmicontint= "Surv(alder, (alder+ get(fu)), get(endpoint)) ~ bmi*DMall+ educa + income + v37"}
    if(multifac_adj == TRUE)  {formula_bmicontint = "Surv(alder, (alder+ get(fu)), get(endpoint)) ~ bmi*DMall+ educa + income + v37 + ldl + trig + systolic"}



    cox_data <- data %>%
        mutate(sex = haven::zap_labels(sex)) %>%
        filter(get(fu)>0,
               sex == sex_,
               bmikat !=1) %>%
        mutate(bmikat_cox = case_when(
            bmikat == 0 ~ 0,
            bmikat == 2 ~ 1,
            bmikat == 3 ~2
        ))

    nodm_bmikat <- coxph(as.formula(formula_bmikatnum), filter(cox_data, DMall == 0))

    dm_bmikat <- coxph(as.formula(formula_bmikatnum), filter(cox_data, DMall == 1))

    interaction_bmikat <- coxph(as.formula(formula_bmikatnumint), cox_data)

    nodm_bmicont <- coxph(as.formula(formula_bmicont), filter(cox_data, DMall == 0))

    dm_bmicont<- coxph(as.formula(formula_bmicont), filter(cox_data, DMall == 1))

    interaction_bmicont <- coxph(as.formula(formula_bmicontint), cox_data)

    p_table <- tibble(
        p_bmikat_nodm = summary(nodm_bmikat)$coefficients[1,5],
        p_bmikat_dm = summary(dm_bmikat)$coefficients[1,5],
        p_int_bmikat_dm = filter(tidy(interaction_bmikat), term == "bmikat_cox:DMall1")$p.value,
        p_bmicont_nodm = summary(nodm_bmicont)$coefficients[1,5],
        p_bmicont_dm = summary(dm_bmicont)$coefficients[1,5],
        p_int_bmicont = filter(tidy(interaction_bmicont), term == "bmi:DMall1")$p.value,
        est_int_bmikat = filter(tidy(interaction_bmikat), term == "bmikat_cox:DMall1")$estimate,
        est_int_bmicont = filter(tidy(interaction_bmicont), term == "bmi:DMall1")$estimate) %>%
        pivot_longer(cols = everything(), names_to = "origin", values_to = "p")


    # model check -------------------------------------------------------------

    # #this is the curve mentioned in methods
    # fit <- survfit(Surv(alder, (alder+get(fu)), get(endpoint)) ~ risk_var , cox_data)
    #
    # survminer::ggsurvplot(fit, fun = "cloglog", data = cox_data, xlim = c(40,100))
    #

    #   -----------------------------------------------------------------------



    cox_output <- coxph(as.formula(formula), cox_data) %>%
        tidy(conf.int = TRUE, exponentiate = TRUE) %>%
        mutate(hrci = sprintf("%.2f (%.2f-%.2f)", estimate, conf.low, conf.high)) %>%
        bind_rows(tibble(
            term = "risk_var1",
            estimate = 1,
            hrci = "1.00 (1.00-1.00)")) %>%
        arrange(term) %>%
        filter(str_starts(term, "risk_var"))

    if(multifac_adj == FALSE) {
        formula_n = map_int(1:6, ~ nrow(filter(cox_data, risk_var == .x, !is.na(income), !is.na(educa), !is.na(v37))))
        formula_n_event = map_int(1:6, ~ nrow(filter(cox_data,
                                                     risk_var == .x, !is.na(income), !is.na(educa), !is.na(v37),
                                                     .data[[endpoint]] == 1)))}
    #add bmi cat, dm cat, number of participants and events
    left_table <- tibble(term =cox_output$term,
                         bmikat = c("Normal weight", "Overweight", "Obesity", "Normal weight", "Overweight", "Obesity"),
                         diab = c("No", "No", "No", "Yes", "Yes", "Yes"),
                         n = formula_n,
                         n_event =formula_n_event )%>%
        mutate( n = format(n, big.mark = " "),
                n_event = format(n_event, big.mark = " "))

    ggdata <- left_join(left_table, cox_output)

    results_data <- ggdata %>%
        full_join(tibble(term = "int",
                         p_intcat = filter(tidy(interaction_bmikat), term == "bmikat_cox:DMall1")$p.value,
                         est_intcat = filter(tidy(interaction_bmikat), term == "bmikat_cox:DMall1")$estimate))


    plot <-

        ggplot(ggdata, aes(y = as.numeric(fct_rev(term)), x = estimate))+
        geom_point()+
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2)+
        geom_segment(x = 1, xend = 1, y = 0, yend = 6.5, linetype = "dashed")+
        scale_x_continuous(limits =c(-8, 7.5))+
        # coord_cartesian(xlim = c(-8, 7.5))+
        scale_y_continuous(limits = c(-3, 8))+
        geom_text(aes(x = -8, label = bmikat), hjust = 0)+
        geom_text(aes(x = -5.5, label = diab), hjust = 0)+
        geom_text(aes(x = -3.7, label = n), hjust = 0)+
        geom_text(aes(x = -1.2, label = n_event), hjust = 0)+
        geom_text(aes(x = 4.5, label = hrci), hjust = 0)+

        annotate("text", x = -8, y = 7, label = "BMI category", fontface = "bold", hjust = 0)+
        annotate("text", x = -5.5, y = 7, label = "Diabetes", fontface = "bold", hjust = 0)+
        annotate("text", x = -3.7, y = 7, label = "N, participants", fontface = "bold", hjust = 0)+
        annotate("text", x = -1.2, y = 7, label = "N, events", fontface = "bold", hjust = 0)+
        annotate("text", x = 4.5, y = 7, label = "HR (95% CI)", fontface = "bold", hjust = 0)+
        annotate(
            "text",
            x = -8,
            y = -1,
            label = ifelse(
                filter(tidy(interaction_bmikat), term == "bmikat_cox:DMall1")$p.value < 0.01,
                "P for interaction < 0.01",
                paste0(
                    "P for interaction = ",
                    format(
                        round(filter(tidy(interaction_bmikat), term == "bmikat_cox:DMall1")$p.value, 2),
                        nsmall = 2,
                        trim = TRUE
                    )
                )
            ),
            hjust = 0,
            fontface = "italic"
        )+

        geom_segment(y = 0, yend = 0, x = -0.2, xend = 4.5)+
        annotate("text", x = 1.8, y = -1.2, label = x_label, hjust = 0.5)+
        annotate("text", x = c(1, 2, 3, 4), y = -0.4, label = c(1,2,3,4))+
        geom_point(aes(x = 1, y = 0), shape = 124)+
        geom_point(aes(x = 2, y = 0), shape = 124)+
        geom_point(aes(x = 3, y = 0), shape = 124)+
        geom_point(aes(x = 4, y = 0), shape = 124)+
        # geom_point(aes(x = 7, y = 0), shape = 124)+
        # geom_point(aes(x = 9, y = 0), shape = 124)+
        theme_void()

    ggplot_build(plot)$layout$panel_params[[1]]$x.range


    return(list(plot, p_table, results_data))
}


#' Prepare restricted cubic spline data, preparation for next function
#'
#' @param dm_type Diabetes variable
#' @param dm Numeric; 0 = no diabetes, 1 = diabetes
#' @param sex_ Numeric; 1 male, 2 female
#' @param futime Follow-up time
#' @param endpoint Event variable
#' @param env Environment for storing intermediate results
#'
#' @return Data frame suitable for spline plotting
#' @export
prep_rcs <- function(dm_type = "dm2", dm = 0, sex_ = 2, futime = "fu_ascvd", endpoint = "ascvd", env = parent.frame()) {
    if(dm == 0) {suffix = "_dm0"}
    if(dm == 1) {suffix = "_dm1"}

    rcs_data <- data %>%
        filter(get(dm_type) == dm, sex == sex_) %>%
        select(alder, all_of(futime), all_of(endpoint), bmi, all_of(dm_type), educa, income, v37)

    dd <<- datadist(rcs_data)

    env$ndt <- rcs_data

    env$dd <- datadist(env$ndt)

    options(datadist = "dd")

    env$dd$limits["Adjust to", "bmi"] <-18.5

    env$cox_spline_rms <- cph(Surv(alder, (alder + get(futime)), get(endpoint)) ~ rcs(bmi,  3) + educa + income + v37, rcs_data)

    predict_data <- Predict(env$cox_spline_rms, bmi = seq(18.5,  max(rcs_data$bmi, na.rm = TRUE), length.out = 100), ref.zero = TRUE)

    plot_data_rms <- data.frame(bmi = predict_data$bmi, hr = exp(predict_data$yhat), ci_lower = exp(predict_data$lower), ci_upper = exp(predict_data$upper))  %>%
        mutate(dmgroup = factor(dm) )

    # options(datadist = NULL)
    #
    # rm("dd", envir = globalenv())

    return(plot_data_rms)
}


#' Create restricted cubic spline plots, Figure 4
#'
#' @param dm Diabetes variable
#' @param sex_ Numeric; 1 male, 2 female
#' @param futime Follow-up time
#' @param endpoint Event variable
#'
#' @return List containing:
#' - `plot`: spline plot
#' - `ggdata`: data used for plotting
#' @export
spline_function <- function(dm = "DMall", sex_ = 1, futime = "fu_ascvd", endpoint = "ascvd", env = parent.frame()) {
    if(endpoint == "ascvd"){ylab = " atherosclerotic cardiovascular disease"}

    ggdata <- bind_rows(prep_rcs(dm_type = dm, dm=0, sex_ = sex_, futime = futime, endpoint = endpoint, env = parent.frame()), prep_rcs(dm_type = dm, dm=1, sex_ = sex_, futime = futime, endpoint = endpoint, env = parent.frame()))

    plot <- ggplot(ggdata)+
        # geom_density(data = filter(data, fu_ascvd>0, DMall == 0, sex == 1), aes(y = 0.5+..density..*10, x = bmi), color = "transparent", fill = "#66C2A5", geom = "line", alpha = 0.2)+
        # geom_density(data = filter(data, fu_ascvd>0, DMall == 1, sex == 1), aes(y = 0.5+..density..*10, x = bmi), color = "transparent", fill = "#FC8D62", geom = "line", alpha = 0.2)+
        geom_hline(aes(yintercept = 1))+
        geom_line(aes(x = bmi, y = hr, color = dmgroup), lineend = "round",  linewidth = 0.9 )+
        geom_ribbon(aes(x = bmi, ymin = ci_lower, ymax = ci_upper, fill = dmgroup),  alpha = 0.2, linetype = "dashed", size = 0.9, show.legend = FALSE)+
        # geom_line(aes(x = bmi_dm1, y = hr_dm1, color = "Diabetes"), lineend = "round", color = "#FC8D62", size = 0.9)+
        # geom_ribbon(aes(x = bmi_dm1, ymin = ci_lower_dm1, ymax = ci_upper_dm1), fill =  "#FC8D62",alpha = 0.2, linetype = "dashed", size = 0.9)+
        coord_cartesian(xlim = c(18.5, 50), ylim = c(0.5, 3), expand = FALSE)+
        xlab(expression(paste("Body Mass Index (kg/", m^2, ")")))+
        ylab(paste0("Hazard ratio (95% CI) for \n", ylab))+
        scale_color_manual(values = c("0" = "#66C2A5", "1" ="#FC8D62"),
                           labels = c("No diabetes", "Diabetes"))+
        scale_fill_manual(values = c("0" = "#66C2A5", "1" ="#FC8D62"))+
        # labs(color = "Legend")+
        # scale_x_continuous(limits = c(18.5, 50))+
        # scale_y_continuous(limits = c(0.4, 2))+
        theme_classic()+
        theme(
            legend.title = element_blank(),
            legend.position = "right",
            legend.background = element_rect(
                size = 0.5,
                linetype = "solid",
                color = "black"))

    return(list(plot, ggdata))
}


#' Poisson regression for incidence barplot, Figure 5
#'
#' @param dm Diabetes variable
#' @param futime Follow-up time
#' @param endpoint Event variable
#' @param sex_ Numeric; 1 male, 2 female
#'
#' @return List containing:
#' - `plot`: barplot of incidence per 1000 person-years
#' - `table`: gt table with incidence and counts
#' - `results_table`: detailed predicted incidence table
#' @export
poisson_barplot <- function(dm = "DMall", futime = "fu_ascvd", endpoint = "ascvd", sex_ = 2) {

    if(endpoint == "ascvd") {ylab = "Incidence of ASCVD event \nper 1000 person-years"}
    if(endpoint == "ascvd") {tabletitle = "Incidence of ASCVD event per 1000 person-years (95% confidence interval)"}

    model_data <- data %>%
        filter(!is.na(bmikat), bmikat != 1,
               sex == sex_) %>%
        mutate(py = get(futime))  # person-years

    n <- model_data %>%
        group_by(bmikat, !!sym(dm)) %>%
        summarise(
            n = n(),
            events = sum(.data[[endpoint]], na.rm = TRUE),
            .groups = "drop"
        ) %>%
        mutate(
            bmidm = paste0(as.character(bmikat), as.character(!!sym(dm)))
        )

    poisson_model <- glm(
        formula = as.formula(paste0(endpoint, " ~ bmikat *factor(", dm, ") + alder")),
        family = quasipoisson(link = "log"),
        data = model_data,
        offset = log(py)
    )

    # Step 2: Create new data for predictions
    predict_data <- expand.grid(
        bmikat = c(0, 2, 3),        # Normal, Overweight, Obesity
        value = factor(c(0, 1)),            # No diabetes, diabetes
        alder = 60,           # Centered at age 60
        py = 1                      # Setting to 1 so predicted rate equals rate per person-year
    )

    predict_data <- setNames(predict_data, c("bmikat", dm, "alder", "py"))
    # Step 3: Predict incidence (per person-year), then scale
    pred <- predict(poisson_model, newdata = predict_data, type = "response", se.fit = TRUE)

    predict_data <- predict_data %>%
        mutate(
            pred_rate = pred$fit,
            se_rate = pred$se.fit,
            incidence1000 = pred_rate * 1000,
            ci_low = (pred_rate - 1.96 * se_rate) * 1000,
            ci_high = (pred_rate + 1.96 * se_rate) * 1000
        )

    # Step 4: Add labels for plotting
    predict_data <- predict_data %>%
        mutate(
            bmi_lab = case_when(
                bmikat == 0 ~ "Normal weight",
                bmikat == 2 ~ "Overweight",
                bmikat == 3 ~ "Obesity"
            ),
            bmi_lab = factor(bmi_lab, levels = c("Normal weight", "Overweight", "Obesity")),
            dm_lab = case_when(
                get(dm) == 0 ~ "No diabetes",
                get(dm) == 1 ~ "Diabetes"
            ),
            bmidm = paste0(as.character(bmikat), as.character(DMall))
        )

    # Optional: Plot (similar style to before)
    plot <- ggplot(predict_data, aes(x = bmi_lab, y = incidence1000, fill = factor(!!sym(dm)))) +
        geom_col(position = position_dodge(width = 0.9), color = "black") +
        geom_errorbar(aes(ymin = ci_low, ymax = ci_high), stat = "identity", position = position_dodge(width = 0.9), width = 0.4)+
        labs(y = ylab) +
        scale_y_continuous(limits = c(0, 45.5))+
        theme_minimal()+
        scale_fill_manual(values = c("0" = "#66C2A5", "1" = "#FC8D62"),
                          labels = c("No diabetes", "Diabetes")) +
        theme(axis.title.x = element_blank(),
              legend.title = element_blank(),
              legend.position = "right",
              legend.background = element_rect(
                  size = 0.5,
                  linetype = "solid",
                  color = "black"))


    table_pre <- left_join(predict_data, n) %>%
        mutate(in_ci = paste0(
            format(round(incidence1000, 1), nsmall = 1, trim = TRUE),
            " (",
            format(round(ci_low, 1), nsmall = 1, trim = TRUE),
            "-",
            format(round(ci_high, 1), nsmall = 1, trim = TRUE),
            ")"
        ),
        n_nevent = paste0(format(n, big.mark = ",", trim = TRUE), "/",format(events, big.mark = ",", trim = TRUE)))

    results_table <- table_pre %>%
        select(bmikat, DMall, bmidm, incidence1000, ci_low, ci_high, in_ci, n ,events)

    table <- table_pre %>%
        select(bmi_lab, dm_lab, in_ci, n_nevent) %>%
        pivot_longer(cols = c(in_ci, n_nevent), names_to = "type", values_to = "value") %>%
        pivot_wider(names_from = dm_lab, values_from= value) %>%
        arrange(type) %>%
        mutate(type_titles = case_when(
            type == "in_ci" ~ tabletitle,
            type == "n_nevent" ~ "Number of individuals/events"
        )) %>%
        # filter(type == "in_ci") %>%
        select(-type) %>%
        gt(groupname_col = "type_titles") %>%
        tab_style(style = cell_fill(color ="lightblue"),
                  locations = cells_row_groups(groups = 1)) %>%
        tab_style(style = cell_fill(color ="lightblue"),
                  locations = cells_row_groups(groups = 2)) %>%
        tab_style(style = cell_text(align = "center"),
                  locations = cells_row_groups()) %>%

        cols_label(bmi_lab = "BMI category") %>%
        tab_style(style = cell_text(weight = "bold"),
                  locations = cells_column_labels())

    return(list(plot, table, results_table))
}

#' Find the highest level of education from a string of education choices
#'
#' @param ed_str Character string of education levels separated by "|"
#'
#' @return The highest education level as a character, or NA if missing
#' @export
#'
#' @examples
#' get_highest_edu("highschool|bachelor|master")
#' get_highest_edu(NA)
#' get_highest_edu("")
get_highest_edu <- function(ed_str) {

    # Step 1: Handle missing or blank values
    if (is.na(ed_str) || ed_str == "") return(NA_character_)

    # Step 2: Split the string into separate education choices
    # Example: "highschool|bachelor|master" -> c("highschool", "bachelor", "master")
    choices <- str_split(ed_str, "\\|")[[1]]

    # Step 3: Match the choices against a predefined vector of education levels
    # edu_levels should be defined elsewhere, e.g., c("none", "primary", "highschool", "bachelor", "master", "phd")
    matches <- match(choices, edu_levels)

    # Step 4: Drop NA matches and select the highest-ranked level
    best_i <- max(matches, na.rm = TRUE)

    # Step 5: If no valid match is found, return NA
    if (is.infinite(best_i)) return(NA_character_)

    # Step 6: Return the corresponding education level from edu_levels
    edu_levels[best_i]
}
