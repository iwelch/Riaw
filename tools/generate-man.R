#!/usr/bin/env Rscript
## tools/generate-man.R  --  generate man/*.Rd from roxygen-style comments
## No roxygen2 dependency; pure base R.
##
## Usage:  Rscript tools/generate-man.R          (from package root)
##         make man                              (via Makefile)

## ---- helpers ---------------------------------------------------------------

escape_rd <- function(x) gsub("%", "\\\\%", x)

is_operator_name <- function(nm) grepl("^%.*%$", nm)

## Parse formals from a (possibly multi-line) function definition.
## Returns argument string or NULL.
parse_formals <- function(code_text) {
    ## code_text: single string (lines collapsed with " ")
    m <- regmatches(code_text, regexpr("function\\s*\\(", code_text))
    if (length(m) == 0L || nchar(m) == 0L) return(NULL)
    start <- regexpr("function\\s*\\(", code_text)
    paren_start <- start + nchar(m) - 1L
    chars <- strsplit(substr(code_text, paren_start, nchar(code_text)), "")[[1L]]
    depth <- 0L
    for (i in seq_along(chars)) {
        if (chars[i] == "(") depth <- depth + 1L
        else if (chars[i] == ")") depth <- depth - 1L
        if (depth == 0L) {
            raw <- substr(code_text, paren_start + 1L, paren_start + i - 2L)
            ## Clean up whitespace from multi-line
            raw <- gsub("\\s+", " ", trimws(raw))
            return(raw)
        }
    }
    NULL
}

## Extract function name from the first code line
parse_funcname <- function(line) {
    m <- regmatches(line, regexec("^\\s*iaw\\$([\\.a-zA-Z0-9_]+)\\s*<-", line))[[1L]]
    if (length(m) >= 2L) return(m[2L])
    m <- regmatches(line, regexec("^\\s*`(%[^%]+%)`\\s*<-", line))[[1L]]
    if (length(m) >= 2L) return(m[2L])
    m <- regmatches(line, regexec("^\\s*([\\.a-zA-Z][a-zA-Z0-9_.]+)\\s*<-\\s*function", line))[[1L]]
    if (length(m) >= 2L) return(m[2L])
    NULL
}


## ---- Pass 1: Parse roxygen blocks ------------------------------------------

parse_file <- function(filepath) {
    lines <- readLines(filepath, warn = FALSE)
    n <- length(lines)
    blocks <- list()

    i <- 1L
    while (i <= n) {
        if (!grepl("^#'", lines[i])) { i <- i + 1L; next }

        ## Collect contiguous #' lines
        rox_lines <- character(0)
        while (i <= n && grepl("^#'", lines[i])) {
            rox_lines <- c(rox_lines, sub("^#'\\s?", "", lines[i]))
            i <- i + 1L
        }

        ## Skip blank lines between block and code
        code_i <- i
        while (code_i <= n && grepl("^\\s*$", lines[code_i])) code_i <- code_i + 1L

        ## Grab enough code lines to capture multi-line function signatures
        ## Read up to 5 lines of code context
        code_lines <- character(0)
        for (cl in code_i:min(code_i + 4L, n)) {
            code_lines <- c(code_lines, lines[cl])
        }
        code_first <- if (code_i <= n) lines[code_i] else ""
        code_text <- paste(code_lines, collapse = " ")

        block <- parse_roxygen_block(rox_lines, code_first, code_text)
        block$file <- basename(filepath)
        if (!is.null(block$name) || !is.null(block$rdname))
            blocks <- c(blocks, list(block))
    }
    blocks
}

parse_roxygen_block <- function(rox_lines, code_first, code_text) {
    block <- list(
        name = NULL, rdname = NULL, title = NULL, desc = NULL,
        params = list(), return_ = NULL, details = NULL,
        seealso = NULL, family = NULL, keywords = NULL,
        examples = NULL, export = FALSE,
        funcname = NULL, formals = NULL, is_alias = FALSE
    )

    block$funcname <- parse_funcname(code_first)

    ## Detect function definition (might be multi-line)
    if (grepl("function\\s*\\(", code_text))
        block$formals <- parse_formals(code_text)

    ## Detect alias: iaw$x <- iaw$y (no function keyword)
    if (grepl("^\\s*iaw\\$[a-zA-Z0-9_.]+\\s*<-\\s*iaw\\$", code_first) &&
        !grepl("function", code_first))
        block$is_alias <- TRUE

    ## Detect closure factory: make_xxx <- function()
    ## The funcname (make_xxx) is internal; clear it so we use @name instead
    if (!is.null(block$funcname) && grepl("^make_", block$funcname)) {
        block$funcname <- NULL
        block$formals <- NULL  # factory formals are not user-facing
    }

    ## Detect non-function assignment: iaw$x <- FALSE / iaw$x <- NULL / etc.
    ## These are not real function definitions — clear funcname
    if (!is.null(block$funcname) && is.null(block$formals) && !block$is_alias) {
        if (!grepl("function", code_text) &&
            !grepl("<-\\s*iaw\\$", code_first) &&
            !grepl("<-\\s*make_", code_first))
            block$funcname <- NULL
    }

    ## ---- Segment the roxygen lines into tagged sections ---------------------
    single_line_tags <- c("name", "rdname", "family", "keywords", "export", "importFrom")

    sections <- list()
    n <- length(rox_lines)
    j <- 1L

    while (j <= n) {
        ln <- rox_lines[j]
        m <- regmatches(ln, regexec("^@(\\w+)\\b\\s*(.*)", ln))[[1L]]

        if (length(m) >= 3L) {
            tag <- m[2L]
            content <- m[3L]

            if (tag %in% single_line_tags) {
                sections <- c(sections, list(list(tag = tag, lines = content)))
                j <- j + 1L
            } else if (tag == "examples") {
                ex_lines <- if (nchar(trimws(content)) > 0L) content else character(0)
                j <- j + 1L
                while (j <= n) {
                    if (grepl("^@(\\w+)", rox_lines[j]) &&
                        !grepl("^@examples", rox_lines[j])) break
                    ex_lines <- c(ex_lines, rox_lines[j])
                    j <- j + 1L
                }
                sections <- c(sections, list(list(tag = "examples", lines = ex_lines)))
            } else {
                ## Multi-line tag
                ml_lines <- if (nchar(trimws(content)) > 0L) content else character(0)
                j <- j + 1L
                while (j <= n) {
                    if (grepl("^@\\w+", rox_lines[j])) break
                    ml_lines <- c(ml_lines, rox_lines[j])
                    j <- j + 1L
                }
                sections <- c(sections, list(list(tag = tag, lines = ml_lines)))
            }
        } else {
            ## Free text
            text_lines <- ln
            j <- j + 1L
            while (j <= n && !grepl("^@\\w+", rox_lines[j])) {
                text_lines <- c(text_lines, rox_lines[j])
                j <- j + 1L
            }
            sections <- c(sections, list(list(tag = "", lines = text_lines)))
        }
    }

    ## ---- Extract fields from sections --------------------------------------

    for (sec in sections) {
        tag <- sec$tag
        lns <- sec$lines

        if (tag == "") {
            nonempty <- lns[nchar(trimws(lns)) > 0L]
            if (length(nonempty) > 0L && is.null(block$title)) {
                block$title <- trimws(nonempty[1L])
                if (length(nonempty) > 1L) {
                    desc_new <- paste(trimws(nonempty[-1L]), collapse = "\n")
                    block$desc <- if (is.null(block$desc)) desc_new
                                  else paste(block$desc, desc_new, sep = "\n")
                }
            } else if (length(nonempty) > 0L) {
                desc_new <- paste(trimws(nonempty), collapse = "\n")
                block$desc <- if (is.null(block$desc)) desc_new
                              else paste(block$desc, desc_new, sep = "\n")
            }
        } else if (tag == "name") {
            block$name <- trimws(lns)
        } else if (tag == "rdname") {
            block$rdname <- trimws(lns)
        } else if (tag == "family") {
            block$family <- c(block$family, trimws(lns))
        } else if (tag == "keywords") {
            block$keywords <- c(block$keywords, trimws(lns))
        } else if (tag == "export") {
            block$export <- TRUE
        } else if (tag == "param") {
            txt <- paste(trimws(lns), collapse = " ")
            m2 <- regmatches(txt, regexec("^(\\S+)\\s+(.*)", txt))[[1L]]
            if (length(m2) >= 3L)
                block$params <- c(block$params, list(list(name = m2[2L], desc = m2[3L])))
        } else if (tag == "return") {
            block$return_ <- paste(trimws(lns), collapse = " ")
        } else if (tag == "details") {
            det <- paste(trimws(lns), collapse = "\n")
            block$details <- if (is.null(block$details)) det
                             else paste(block$details, det, sep = "\n\n")
        } else if (tag == "seealso") {
            block$seealso <- paste(trimws(lns), collapse = " ")
        } else if (tag == "examples") {
            block$examples <- paste(lns, collapse = "\n")
        }
    }

    block
}


## ---- Pass 2: Group blocks by name/rdname -----------------------------------

group_blocks <- function(all_blocks) {
    pages <- list()

    for (b in all_blocks) {
        if (!is.null(b$name)) {
            nm <- b$name
            if (is.null(pages[[nm]]))
                pages[[nm]] <- list(primary = b, secondary = list())
            else
                pages[[nm]]$secondary <- c(pages[[nm]]$secondary, list(b))
        }
    }

    for (b in all_blocks) {
        if (!is.null(b$rdname) && is.null(b$name)) {
            rd <- b$rdname
            if (!is.null(pages[[rd]]))
                pages[[rd]]$secondary <- c(pages[[rd]]$secondary, list(b))
            else
                pages[[rd]] <- list(primary = b, secondary = list())
        }
    }

    pages
}


## ---- Pass 3: Emit .Rd files ------------------------------------------------

build_family_index <- function(pages) {
    idx <- list()
    for (nm in names(pages)) {
        fam <- pages[[nm]]$primary$family
        for (f in fam) idx[[f]] <- c(idx[[f]], nm)
    }
    idx
}

make_usage <- function(funcname, formals_str, is_operator = FALSE) {
    if (is_operator) {
        ## Use actual parameter names if available, else default to e1/e2
        if (!is.null(formals_str) && nchar(trimws(formals_str)) > 0L) {
            parts <- trimws(strsplit(formals_str, ",")[[1]])
            ## Strip defaults (e.g., "x = 1" -> "x")
            parts <- sub("\\s*=.*", "", parts)
            if (length(parts) >= 2)
                return(paste0(parts[1], " ", escape_rd(funcname), " ", parts[2]))
        }
        return(paste0("e1 ", escape_rd(funcname), " e2"))
    }
    args <- if (!is.null(formals_str) && nchar(trimws(formals_str)) > 0L)
                escape_rd(formals_str) else ""
    ## S3 methods: print.olm -> \method{print}{olm}(...)
    if (grepl("^(print|format|summary|plot|coef|residuals|predict|vcov)\\.", funcname)) {
        parts <- regmatches(funcname, regexpr("^[^.]+", funcname))
        class_part <- sub(paste0("^", parts, "\\."), "", funcname)
        return(paste0("\\method{", parts, "}{", class_part, "}(", args, ")"))
    }
    paste0(escape_rd(funcname), "(", args, ")")
}

emit_rd <- function(page_name, page, family_index, outdir) {
    primary <- page$primary
    secondaries <- page$secondary

    ## Collect aliases
    aliases <- page_name
    if (!is.null(primary$funcname) && !(primary$funcname %in% aliases))
        aliases <- c(aliases, primary$funcname)
    for (s in secondaries)
        if (!is.null(s$funcname) && !(s$funcname %in% aliases))
            aliases <- c(aliases, s$funcname)

    title <- if (!is.null(primary$title)) primary$title else page_name
    desc <- if (!is.null(primary$desc)) primary$desc else title

    ## Params — dedup by name
    all_params <- primary$params
    for (s in secondaries) {
        for (p in s$params) {
            existing <- vapply(all_params, function(x) x$name, "")
            if (!(p$name %in% existing))
                all_params <- c(all_params, list(p))
        }
    }

    ## Return
    return_text <- primary$return_
    if (is.null(return_text))
        for (s in secondaries)
            if (!is.null(s$return_)) { return_text <- s$return_; break }

    ## Details
    details_parts <- character(0)
    if (!is.null(primary$details)) details_parts <- primary$details
    for (s in secondaries)
        if (!is.null(s$details)) details_parts <- c(details_parts, s$details)
    details_text <- if (length(details_parts)) paste(details_parts, collapse = "\n\n") else NULL

    ## Seealso — escape % in user-authored text
    seealso_text <- primary$seealso
    if (is.null(seealso_text))
        for (s in secondaries)
            if (!is.null(s$seealso)) { seealso_text <- s$seealso; break }
    if (!is.null(seealso_text)) seealso_text <- escape_rd(seealso_text)

    ## Family cross-refs
    family_refs <- character(0)
    for (fam in primary$family) {
        siblings <- setdiff(family_index[[fam]], page_name)
        if (length(siblings) > 0L) {
            links <- paste0("\\code{\\link{", escape_rd(siblings), "}}", collapse = ", ")
            family_refs <- c(family_refs, paste0("\nOther ", fam, ": ", links))
        }
    }
    seealso_full <- c(if (!is.null(seealso_text)) seealso_text, family_refs)
    seealso_combined <- if (length(seealso_full)) paste(seealso_full, collapse = "\n") else NULL

    ## Examples
    examples_text <- primary$examples
    if (is.null(examples_text))
        for (s in secondaries)
            if (!is.null(s$examples)) { examples_text <- s$examples; break }

    keywords <- primary$keywords

    ## ---- Build usage lines ----
    usage_lines <- character(0)
    is_op <- is_operator_name(page_name)

    ## Primary usage
    if (!is.null(primary$funcname) && !isTRUE(primary$is_alias) &&
        !is.null(primary$formals)) {
        ## Has a real function definition
        usage_lines <- c(usage_lines, make_usage(primary$funcname, primary$formals, is_op))
    } else if (is_op) {
        usage_lines <- c(usage_lines, make_usage(page_name, NULL, TRUE))
    } else if (!is.null(primary$funcname) && !isTRUE(primary$is_alias)) {
        ## funcname but no formals (closure factory) — derive from @param
        if (length(all_params) > 0L) {
            args <- paste(vapply(all_params, function(p) p$name, ""), collapse = ", ")
            usage_lines <- c(usage_lines, paste0(escape_rd(page_name), "(", args, ")"))
        } else {
            usage_lines <- c(usage_lines, paste0(escape_rd(page_name), "()"))
        }
    } else if (is.null(primary$funcname)) {
        ## No code line matched (e.g., bare variable or `base.R`)
        ## Try to build usage from @param tags
        if (length(primary$params) > 0L) {
            args <- paste(vapply(primary$params, function(p) p$name, ""), collapse = ", ")
            usage_lines <- c(usage_lines, paste0(escape_rd(page_name), "(", args, ")"))
        } else {
            usage_lines <- c(usage_lines, paste0(escape_rd(page_name), "()"))
        }
    }

    ## Secondary usages (functions with their own signature)
    for (s in secondaries) {
        if (!is.null(s$funcname) && !isTRUE(s$is_alias) && !is.null(s$formals))
            usage_lines <- c(usage_lines, make_usage(s$funcname, s$formals, FALSE))
    }

    ## ---- Write .Rd ----
    rd <- character(0)
    a <- function(...) rd <<- c(rd, paste0(...))

    a("% Generated by tools/generate-man.R -- do not edit by hand")
    a("\\name{", escape_rd(page_name), "}")
    for (al in aliases) a("\\alias{", escape_rd(al), "}")
    a("\\title{", title, "}")

    a("\\description{")
    a(desc)
    a("}")

    if (length(usage_lines)) {
        a("\\usage{")
        for (u in usage_lines) a(u)
        a("}")
    }

    if (length(all_params)) {
        a("\\arguments{")
        for (p in all_params)
            a("  \\item{", p$name, "}{", p$desc, "}")
        a("}")
    }

    if (!is.null(return_text)) {
        a("\\value{")
        a(return_text)
        a("}")
    }

    if (!is.null(details_text)) {
        a("\\details{")
        a(details_text)
        a("}")
    }

    if (!is.null(seealso_combined)) {
        a("\\seealso{")
        a(seealso_combined)
        a("}")
    }

    if (!is.null(examples_text) && nchar(trimws(examples_text)) > 0L) {
        a("\\examples{")
        ex <- escape_rd(examples_text)
        if (!grepl("\\\\dontrun\\{", ex)) {
            a("\\dontrun{")
            a(ex)
            a("}")
        } else {
            a(ex)
        }
        a("}")
    }

    if (!is.null(keywords))
        for (kw in keywords) a("\\keyword{", kw, "}")

    ## Sanitise page_name for filesystem
    safe_name <- gsub("%", "pct", page_name)
    outfile <- file.path(outdir, paste0(safe_name, ".Rd"))
    writeLines(rd, outfile)
    outfile
}


## ---- Main ------------------------------------------------------------------

main <- function() {
    pkg_root <- getwd()
    outdir <- file.path(pkg_root, "man")

    if (dir.exists(outdir)) {
        old <- list.files(outdir, pattern = "\\.Rd$", full.names = TRUE)
        for (f in old) {
            first <- readLines(f, n = 1L, warn = FALSE)
            if (grepl("Generated by tools/generate-man.R", first))
                file.remove(f)
        }
    } else {
        dir.create(outdir, recursive = TRUE)
    }

    src_files <- list.files(pkg_root, pattern = "\\.R$", full.names = TRUE)
    src_files <- src_files[!grepl("/Rprofile\\.R$", src_files)]

    message("Parsing ", length(src_files), " source files...")
    all_blocks <- list()
    for (f in src_files) {
        blocks <- parse_file(f)
        all_blocks <- c(all_blocks, blocks)
    }
    message("Found ", length(all_blocks), " roxygen blocks")

    pages <- group_blocks(all_blocks)
    message("Grouped into ", length(pages), " man pages")

    family_index <- build_family_index(pages)

    written <- character(0)
    for (nm in names(pages)) {
        f <- emit_rd(nm, pages[[nm]], family_index, outdir)
        written <- c(written, f)
    }

    message("Wrote ", length(written), " .Rd files to man/")
    invisible(written)
}

main()
