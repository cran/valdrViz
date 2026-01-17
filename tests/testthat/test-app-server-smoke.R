test_that("app_server can initialise in a Shiny session", {
    # Mock module servers + init to keep this test as a true "boot loader smoke test"
    testthat::local_mocked_bindings(
        init_main_inputs = function(...) NULL,
        mod_metric_explorer_server = function(...) NULL,
        mod_radar_server = function(...) NULL,
        mod_longit_athlete_comparison_server = function(...) NULL,
        mod_quadrant_server = function(...) NULL,
        mod_player_report_server = function(...) NULL,
        .package = "valdrViz"
    )

    # Provide non-NULL data so the observe() sets dat_raw()
    dummy_data <- data.frame(x = 1)

    shiny::testServer(
        app = function(input, output, session) {
            app_server(input, output, session, data = dummy_data)
        },
        expr = {
            expect_true(TRUE) # If we got here, the server initialised
        }
    )
})
