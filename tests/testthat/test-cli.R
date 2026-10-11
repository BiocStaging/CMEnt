.loadStandaloneCMEntCLI <- function() {
    script <- test_path("..", "..", "run_cment.R")
    skip_if_not(file.exists(script), "Standalone CLI is excluded from the R source package.")
    expressions <- parse(script)
    cli_env <- new.env(parent = baseenv())
    # Load definitions without running the final commandArgs() invocation.
    for (expression in expressions[-length(expressions)]) {
        eval(expression, envir = cli_env)
    }
    cli_env
}

test_that("standalone CLI restores command help and dispatch", {
    skip_if_not_installed("optparse")
    cli <- .loadStandaloneCMEntCLI()

    expect_output(cli$.runCMEntCLI("--help"), "CMEnt command line interface")
    for (command in c("buildDMRs", "findDMPsArray", "findDMPsBSSeq", "launchCMEntViewer")) {
        help <- capture.output(cli$.runCMEntCLI(c("help", command)))
        expect_true(any(grepl(paste0(command, " [options]"), help, fixed = TRUE)))
    }
    expect_equal(cli$.resolveCMEntCLIInvocation(c("--beta", "beta.tsv"))$command, "buildDMRs")
    expect_equal(cli$.resolveCMEntCLIInvocation("viewer")$command, "launchCMEntViewer")
    expect_error(cli$.runCMEntCLI("unknown"), "Unknown CMEnt command 'unknown'", fixed = TRUE)
    expect_error(cli$.runCMEntCLI("buildDMRs"), "Missing required option(s): --beta", fixed = TRUE)

    captured <- NULL
    cli$launchCMEntViewerCLI <- function(args) captured <<- args
    cli$.runCMEntCLI(c(
        "launchCMEntViewer", "--output_prefix", "results/test",
        "--launch_browser", "FALSE", "--host", "0.0.0.0", "--port", "3838"
    ))
    expect_equal(captured$output_prefix, "results/test")
    expect_false(captured$launch_browser)
    expect_equal(captured$host, "0.0.0.0")
    expect_equal(captured$port, 3838L)
})

test_that("standalone buildDMRs CLI resolves package helpers and preserves options", {
    skip_if_not_installed("optparse")
    cli <- .loadStandaloneCMEntCLI()
    samplesheet <- tempfile(fileext = ".tsv")
    withr::defer(unlink(samplesheet))
    write.table(
        data.frame(Sample_Name = c("S1", "S2"), Group = c("control", "case")),
        samplesheet, sep = "\t", quote = FALSE, row.names = FALSE
    )

    captured <- NULL
    local_mocked_bindings(
        buildDMRs = function(...) captured <<- list(...),
        .package = "CMEnt"
    )
    withr::local_options(CMEnt.verbose = 0)
    expect_no_error(cli$.runCMEntCLI(c(
        "buildDMRs", "--beta", "beta.tsv", "--seeds_file", "seeds.tsv",
        "--samplesheet", samplesheet, "--sample_group_col", "Group",
        "--sample_group_control", "control", "--sample_group_case", "case",
        "--verbose", "0", "--njobs", "4", "--entanglement", "weak"
    )))
    expect_equal(rownames(captured$pheno), c("S1", "S2"))
    expect_equal(captured$pheno$Group, c("control", "case"))
    expect_equal(captured$pheno$casecontrol, c(0, 1))
    expect_equal(captured$beta, "beta.tsv")
    expect_equal(captured$seeds, "seeds.tsv")
    expect_equal(captured$njobs, 4L)
    expect_equal(captured$entanglement, "weak")
})
